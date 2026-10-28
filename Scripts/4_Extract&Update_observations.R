# ============================================================
# Monitoring data incremental updater - transactional update workflow
# 
# Skips fetching if baseline already covers model timeframe
# where timeframe is determined by latest forcing data availability
# 
# Fetches  monitoring data for sites attached to: 
# - Discharge consents
# - Xobs sites list
# via the Water Quality Explorer API
#
# ============================================================

# ============================================================
# Config & packages
# ============================================================

if (!require(pacman)) install.packages("pacman")
pacman::p_load(
  tidyverse,
  here,
  httr,
  jsonlite,
  lubridate,
  future.apply,
  furrr,
  glue,
  data.table,
  readr
)

options(future.globals.maxSize = 5 * 1024^3)
plan(sequential)

# ============================================================
# Paths & constants
# ============================================================

cache_dir         <- here("PointSourceData", "Discharges", "api_cache")
eff_update_tmp    <- file.path(cache_dir, "update_temp_effluent")
xobs_update_tmp   <- file.path(cache_dir, "update_temp_xobs")

per_call_limit        <- 250
sleep_between_calls   <- 0.05
workers_pct           <- 0.8

determinands_all <- c(
  "0180","0192","0348","0135","0111","0116","0117","0118",
  "0076","3458","2551","2348","3723"
)

base_sampling_point_url <- "https://environment.data.gov.uk/water-quality/sampling-point"
api_accept_header <- "application/ld+json"
api_version <- "1"

# ============================================================
# Inputs check
# ============================================================

required_inputs <- c(
  here("PointSourceData", "Discharges", "data_out", "effluent_monitoring_sites.csv"),
  list.files(cache_dir, pattern = "^HYPE_EFFLUENT_MONITORING_DATA_.*\\.rds$", full.names = TRUE),
  list.files(cache_dir, pattern = "^HYPE_XOBS_DATA_.*\\.rds$", full.names = TRUE),
  file.path(cache_dir, "invalid_eff_sites.csv"),
  file.path(cache_dir, "closed_sampling_points.csv"),
  here("PointSourceData", "Discharges", "data_in", "WISKI_to_WIMS_LUT.txt"),
  here("ForcingData", "_forcing_metadata.rds"),
  here("XobsData", "Xobs_sites_SUBID_lut.csv")
)

missing_inputs <- required_inputs[!file.exists(required_inputs)]
if (length(missing_inputs) > 0) {
  stop(
    "❌ The following required input files are missing:\n",
    paste(missing_inputs, collapse = "\n")
  )
}
message("✅ All required input files present.")
flush.console()

# record baseline file paths

eff_base_file <-  list.files(cache_dir, pattern = "^HYPE_EFFLUENT_MONITORING_DATA_.*\\.rds$", full.names = TRUE)
xob_base_file <-  list.files(cache_dir, pattern = "^HYPE_XOBS_DATA_.*\\.rds$", full.names = TRUE)

# ============================================================
# Helper functions (top-level for futures)
# ============================================================

safe_api_GET <- function(url, query = list(), max_attempts = 6, base_wait = 0.5) {
  for (attempt in seq_len(max_attempts)) {
    res <- tryCatch(
      GET(
        url,
        query = query,
        add_headers(Accept = api_accept_header, `API-Version` = api_version)
      ),
      error = function(e) NULL
    )
    if (!is.null(res)) {
      status <- status_code(res)
      if (status >= 200 && status < 300) return(res)
      # do not retry non-rate-limit client errors
      if (!(status == 429 || status >= 500)) return(res)
    }
    Sys.sleep(base_wait * 2^(attempt - 1) * runif(1, 0.5, 1.5))
  }
  NULL
}

safe_site_filename <- function(site) {
  paste0("site_", gsub("[^A-Za-z0-9_\\-]", "_", site), ".rds")
}

read_optional_csv1col <- function(path) {
  if (!file.exists(path)) return(tibble(notation = character()))
  # accept either 1-col CSV or normal headered
  df <- suppressWarnings(read_csv(path, show_col_types = FALSE))
  if (ncol(df) == 1) df <- df %>% rename(notation = 1)
  df %>% mutate(notation = as.character(notation)) %>% distinct(notation)
}

write_atomic_csv <- function(df, path) {
  tmp <- paste0(path, ".tmp_", as.integer(Sys.time()))
  write_csv(df, tmp)
  file.rename(tmp, path)
}

append_unique_notations <- function(existing_df, new_notations) {
  new_df <- tibble(notation = as.character(new_notations)) %>% filter(!is.na(notation))
  bind_rows(existing_df, new_df) %>% distinct(notation) %>% arrange(notation)
}

# Clean and reshape member rows 
flatten_and_clean <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(tibble())
  df <- as_tibble(df)
  
  # convert list cols to strings
  list_cols <- names(df)[sapply(df, is.list)]
  if (length(list_cols) > 0) {
    for (col in list_cols) {
      df[[col]] <- vapply(df[[col]], function(x) {
        if (is.null(x) || length(x) == 0) return(NA_character_)
        if (is.data.frame(x)) {
          paste(apply(x, 1, function(r) paste(r, collapse = "|")), collapse = ";")
        } else {
          paste(as.character(x), collapse = ";")
        }
      }, character(1))
    }
  }
  
  out <- df %>%
    transmute(
      SMPT_USER_REFERENCE    = .data$hasSamplingPoint.notation,
      SMPT_LONG_NAME         = .data$hasSamplingPoint.prefLabel,
      MEAS_SAMPLE_ID         = .data$hasSample.isResultOf.id,
      MEAS_DETERMINAND_CODE  = .data$observedProperty.notation,
      DETE_DESC              = .data$observedProperty.prefLabel,
      MEAS_RESULT            = .data$hasSimpleResult,
      DETE_UNIT              = .data$hasResult.hasUnit.notation,
      UNIT_SHORT_DESC        = .data$hasResult.hasUnit.altLabel,
      SAMP_SAMPLE_DATE       = .data$phenomenonTime
    )
  
  # robust parsing
  out %>%
    mutate(
      MEAS_SAMPLE_ID = suppressWarnings(as.numeric(sub(".*/", "", as.character(MEAS_SAMPLE_ID)))),
      SAMP_DT = suppressWarnings(ymd_hms(SAMP_SAMPLE_DATE, quiet = TRUE, tz = "UTC")),
      SAMP_SAMPLE_DATE = as.Date(SAMP_DT),
      MEAS_SIGN = str_extract(as.character(MEAS_RESULT), "^[<>]"),
      MEAS_RESULT = suppressWarnings(as.numeric(str_extract(as.character(MEAS_RESULT), "[0-9.]+")))
    ) %>%
    select(-SAMP_DT)
}

# Single determinand fetch with pagination
fetch_determinand_for_site <- function(site, det, dateFrom, dateTo) {
  base_url <- paste0(base_sampling_point_url, "/", URLencode(site), "/observation")
  skip <- 0
  all_chunks <- list()
  
  repeat {
    query <- list(
      skip = skip,
      limit = per_call_limit,
      determinand = det,
      dateFrom = format(dateFrom, "%Y-%m-%d"),
      dateTo = format(dateTo, "%Y-%m-%d"),
      complianceOnly = "false"
    )
    
    res <- tryCatch(safe_api_GET(base_url, query), error = function(e) NULL)
    if (is.null(res)) return(tibble())
    
    txt <- content(res, "text", encoding = "UTF-8")
    j <- tryCatch(fromJSON(txt, simplifyVector = TRUE), error = function(e) list())
    
    if (!("member" %in% names(j)) || length(j$member) == 0) break
    
    member_n <- length(j$member)
    df <- flatten_and_clean(as_tibble(jsonlite::flatten(j$member)))
    if (nrow(df) > 0) all_chunks[[length(all_chunks) + 1]] <- df
    
    #stop condition based on raw member length 
    if (member_n < per_call_limit) break
    
    skip <- skip + per_call_limit
    Sys.sleep(sleep_between_calls)
  }
  
  if (length(all_chunks) == 0) return(tibble())
  bind_rows(all_chunks)
}

# For a given site, fetch all determinands based on det_meta_df
# det_meta_df columns required: det, latest_date
fetch_updates_for_site <- function(site, det_meta_df, dateTo = Sys.Date()) {
  site_res <- list()
  
  for (i in seq_len(nrow(det_meta_df))) {
    det <- det_meta_df$det[i]
    dateFrom <- det_meta_df$latest_date[i]
    
    # Start at 2000 by default (new sites)  or 1 day after last baseline date
    if (is.na(dateFrom)) dateFrom <- as.Date("2000-01-01") else dateFrom <- as.Date(dateFrom) + 1
    
    if (dateFrom > dateTo) next
    
    new_df <- fetch_determinand_for_site(site, det, dateFrom, dateTo)
    if (nrow(new_df) > 0) site_res[[length(site_res) + 1]] <- new_df
  }
  
  if (length(site_res) == 0) return(tibble())
  bind_rows(site_res) %>% distinct()
}

# Quick API confirmation for a site:
# - tries a tiny call against first determinand with short date window
# - returns TRUE if response is 2xx and parses or 2xx with empty member
is_site_valid_api <- function(site, det = determinands_all[1]) {
  url <- paste0(base_sampling_point_url, "/", URLencode(site), "/observation")
  query <- list(
    skip = 0,
    limit = 1,
    determinand = det,
    dateFrom = "2000-01-01",
    dateTo = format(Sys.Date(), "%Y-%m-%d"),
    complianceOnly = "false"
  )
  res <- safe_api_GET(url, query)
  if (is.null(res)) return(FALSE)
  status <- status_code(res)
  if (!(status >= 200 && status < 300)) return(FALSE)
  
  txt <- content(res, "text", encoding = "UTF-8")
  j <- tryCatch(fromJSON(txt, simplifyVector = TRUE), error = function(e) NULL)
  !is.null(j) && ("member" %in% names(j))
}

# Build det metadata from baseline for incremental update
build_site_det_lookup <- function(baseline_df) {
  meta <- baseline_df %>%
    rename(notation = SMPT_USER_REFERENCE, det = MEAS_DETERMINAND_CODE) %>%
    group_by(notation, det) %>%
    summarise(latest_date = max(SAMP_SAMPLE_DATE, na.rm = TRUE), .groups = "drop")
  
  split(meta %>% select(det, latest_date), meta$notation)
}

# Create det metadata for a brand new site 
build_new_site_det_meta <- function() {
  tibble(det = determinands_all, latest_date = as.Date(NA))
}

# Build compact per-site job objects for parallel workers
# - avoids exporting the full site_det_lookup object to every worker
# - each worker receives only the site id plus that site's determinand metadata
build_site_jobs <- function(sites_to_fetch, site_det_lookup) {
  jobs <- lapply(sites_to_fetch, function(site) {
    list(
      site = site,
      det_meta = site_det_lookup[[site]]
    )
  })
  names(jobs) <- sites_to_fetch
  jobs
}

# Top-level worker for futures
# - defined at top level so it does not capture large parent environments
# - writes one per-site RDS output and returns the completed site id
fetch_site_worker <- function(job, update_temp_dir, date_to = Sys.Date()) {
  site <- job$site
  det_meta <- job$det_meta
  out_file <- file.path(update_temp_dir, paste0("data_", safe_site_filename(site)))
  
  if (is.null(det_meta) || nrow(det_meta) == 0) {
    saveRDS(tibble(), out_file)
    return(site)
  }
  
  res <- fetch_updates_for_site(site, det_meta, date_to)
  saveRDS(res, out_file)
  site
}

# Transactional, resumable multi-site fetch
# - writes each site result as RDS into update_temp_dir
# - uses manifest to know what’s done
fetch_all_sites_transactional <- function(
    baseline_df,
    site_det_lookup,
    update_temp_dir,
    workers_pct = 0.8,
    max_retries = 10
) {
  dir.create(update_temp_dir, showWarnings = FALSE, recursive = TRUE)
  manifest_path <- file.path(update_temp_dir, "manifest_done.csv")
  
  # read existing manifest
  done <- if (file.exists(manifest_path)) {
    suppressWarnings(read_csv(manifest_path, show_col_types = FALSE)) %>%
      mutate(site = as.character(site)) %>%
      distinct(site)
  } else {
    tibble(site = character())
  }
  
  # per-site output naming convention retained for resumable transactional fetches
  per_site_file <- function(site) file.path(update_temp_dir, paste0("data_", safe_site_filename(site)))
  
  # ALSO consider sites done if per-site file exists
  existing_files <- list.files(update_temp_dir, pattern = "^data_site_.*\\.rds$", full.names = FALSE)
  existing_sites_safe <- gsub("^data_(site_.*)\\.rds$", "\\1", existing_files) # safe_site_filename output
  
  # recover completed sites from existing per-site files if manifest is missing / incomplete
  # - because the safe filename transform is not reliably invertible, we only auto-recover
  #   where the original site id is unchanged by safe_site_filename()
  recoverable_sites <- names(site_det_lookup)[safe_site_filename(names(site_det_lookup)) %in% existing_sites_safe]
  
  if (length(recoverable_sites) > 0) {
    done <- bind_rows(done, tibble(site = recoverable_sites)) %>% distinct(site)
  }
  
  all_sites <- names(site_det_lookup)
  
  for (attempt in seq_len(max_retries)) {
    sites_to_fetch <- setdiff(all_sites, done$site)
    
    message("Attempt ", attempt, ": fetching ", length(sites_to_fetch), " sites")
    flush.console()
    
    if (length(sites_to_fetch) == 0) break
    
    workers <- max(1L, floor(future::availableCores() * workers_pct))
    
    # build compact per-site jobs before entering futures
    # - this keeps worker exports small and prevents shipping the full lookup
    site_jobs <- build_site_jobs(sites_to_fetch, site_det_lookup)
    
    # use multisession only during fetch
    future::plan(future::multisession, workers = workers)
    options(future.stdout = TRUE)
    
    # fetch in parallel
    fetched_sites <- future.apply::future_lapply(
      site_jobs,
      FUN = fetch_site_worker,
      update_temp_dir = update_temp_dir,
      date_to = Sys.Date(),
      future.seed = TRUE
    )
    
    future::plan(future::sequential)
    
    fetched_sites <- unlist(fetched_sites, use.names = FALSE)
    if (length(fetched_sites) > 0) {
      done <- bind_rows(done, tibble(site = fetched_sites)) %>% distinct(site)
      write_atomic_csv(done, manifest_path)
    }
    
    # if still not done, retry
    if (nrow(done) == length(all_sites)) break
    Sys.sleep(1)
  }
  
  if (nrow(done) != length(all_sites)) {
    stop("❌ Not all sites completed after ", max_retries, " attempts.")
  }
  
  # Merge baseline + all per-site files
  update_files <- list.files(update_temp_dir, pattern = "^data_.*\\.rds$", full.names = TRUE)
  updates <- bind_rows(lapply(update_files, readRDS))
  
  bind_rows(baseline_df, updates) %>%
    distinct(
      SMPT_USER_REFERENCE,
      MEAS_SAMPLE_ID,
      MEAS_DETERMINAND_CODE,
      .keep_all = TRUE
    )
}

# Unified update runner for (effluent | xobs)
run_update_pipeline <- function(
    dataset_name,
    baseline_pattern,
    update_temp_dir,
    invalid_sites_path,
    site_list_df,
    site_col = "notation",
    forcing_year,
    record_invalid_sites = TRUE   
) {
  message("\n==============================")
  message("▶️  Updating: ", dataset_name)
  message("==============================")
  flush.console()
  
  baseline_file <- list.files(cache_dir, pattern = baseline_pattern, full.names = TRUE)
  if (length(baseline_file) < 1) stop("❌ No baseline file found for ", dataset_name)
  
  baseline <- readRDS(baseline_file[1])
  
  baseline_max_year <- baseline %>%
    mutate(yr = year(as.Date(SAMP_SAMPLE_DATE))) %>%
    summarise(maxyr = suppressWarnings(max(yr, na.rm = TRUE))) %>%
    pull(maxyr)
  
  if (is.infinite(baseline_max_year) || is.na(baseline_max_year)) baseline_max_year <- 0L
  
  skip_fetch <- baseline_max_year >= forcing_year
  if (skip_fetch) {
    message("ℹ️ ", dataset_name, " baseline max year (", baseline_max_year,
            ") already covers forcing year (", forcing_year, "). Skipping fetch.")
    flush.console()
    baseline_updated <- baseline
  } else {
    message("📌 ", dataset_name, " baseline max year: ", baseline_max_year,
            " | forcing year: ", forcing_year, " → fetching updates.")
    flush.console()
    
    invalid_sites <- read_optional_csv1col(invalid_sites_path)
    
    site_det_lookup <- build_site_det_lookup(baseline)
    
    # Exclude closed sampling points from ALL fetching
    # - prevents querying closed sites even if already in baseline
    closed_path <- file.path(cache_dir, "closed_sampling_points.csv")
    closed_sites <- read_optional_csv1col(closed_path)
    
    if (nrow(closed_sites) > 0) {
      n_before <- length(site_det_lookup)
      
      site_det_lookup <- site_det_lookup[
        !names(site_det_lookup) %in% closed_sites$notation
      ]
      
      message(
        "🚫 Removed ",
        n_before - length(site_det_lookup),
        " closed sites from update list."
      )
      flush.console()
    }
    
    # onboard unknown sites (present in site list but not baseline lookup)
    sites_in <- site_list_df %>%
      mutate(site = as.character(.data[[site_col]])) %>%
      filter(!is.na(site)) %>%
      distinct(site)
    
    unknown_sites <- sites_in %>%
      filter(!site %in% names(site_det_lookup)) %>%
      filter(!site %in% invalid_sites$notation)
    
    message("🔎 Unknown sites needing API confirmation: ", nrow(unknown_sites))
    flush.console()
    
    # Confirm unknown sites and add to lookup for full backfill
    if (nrow(unknown_sites) > 0) {
      closed_path <- file.path(cache_dir, "closed_sampling_points.csv")
      closed_sites <- read_optional_csv1col(closed_path)
      
      valid_new <- character()
      invalid_new <- character()
      
      for (s in unknown_sites$site) {
        if (s %in% closed_sites$notation) {
          invalid_new <- c(invalid_new, s)
          next
        }
        ok <- is_site_valid_api(s)
        if (ok) valid_new <- c(valid_new, s) else invalid_new <- c(invalid_new, s)
      }
      
      if (length(valid_new) > 0) {
        message("✅ Confirmed new sites (will backfill): ", length(valid_new))
        for (s in valid_new) site_det_lookup[[s]] <- build_new_site_det_meta()
      }
      
      if (length(invalid_new) > 0) {
        if (record_invalid_sites) {
          message("🚫 Invalid/closed/failed sites recorded: ", length(invalid_new))
          invalid_sites2 <- append_unique_notations(invalid_sites, invalid_new)
          write_atomic_csv(invalid_sites2, invalid_sites_path)
        }
      }
      flush.console()
    }
    
    # Transactional resumable update
    baseline_updated <- fetch_all_sites_transactional(
      baseline_df = baseline,
      site_det_lookup = site_det_lookup,
      update_temp_dir = update_temp_dir,
      workers_pct = workers_pct,
      max_retries = 10
    )
  }
  
  # Save outputs
  timestamp <- format(Sys.Date(), "%Y-%m-%d")
  new_file <- file.path(cache_dir, paste0(dataset_name, "_2000-", timestamp, ".rds"))
  
  saveRDS(baseline_updated, new_file)
  if (!skip_fetch && file.exists(baseline_file[1])) file.remove(baseline_file[1])
  
  log_path <- file.path(cache_dir, paste0(tolower(dataset_name), "_data_log.csv"))
  write_csv(
    baseline_updated %>%
      group_by(SMPT_USER_REFERENCE, MEAS_DETERMINAND_CODE) %>%
      summarise(
        n_obs = n(),
        earliest_date = min(SAMP_SAMPLE_DATE, na.rm = TRUE),
        latest_date = max(SAMP_SAMPLE_DATE, na.rm = TRUE),
        .groups = "drop"
      ),
    log_path
  )
  
  # Cleanup temp dir only if successful
  unlink(update_temp_dir, recursive = TRUE, force = TRUE)
  
  list(
    baseline_updated = baseline_updated,
    new_file = new_file,
    log_file = log_path,
    skip_fetch = skip_fetch
  )
}

# ============================================================
# Read forcing metadata
# ============================================================

forcing_metadata <- readRDS(here("ForcingData", "_forcing_metadata.rds"))
forcing_year <- forcing_metadata$latest_forcing_year

# ============================================================
# Load site lists
# ============================================================

#sites lists must contain column "notation"
eff_sites_in <- read_csv(
  here("PointSourceData", "Discharges", "data_out", "effluent_monitoring_sites.csv"),
  show_col_types = FALSE
)

if (!("notation" %in% names(eff_sites_in))) {
  stop("❌ effluent_monitoring_sites.csv must contain a 'notation' column.")
}

xobs_lut <- read_csv(
  here("XobsData", "Xobs_sites_SUBID_lut.csv"),
  show_col_types = FALSE
)


if (!"notation" %in% names(xobs_lut)) {
  stop("❌ Xobs_sites_SUBID_lut.csv must contain column 'notation'")
}

# ============================================================
# Update effluent dataset
# ============================================================

eff <- run_update_pipeline(
  dataset_name = "HYPE_EFFLUENT_MONITORING_DATA",
  baseline_pattern = "^HYPE_EFFLUENT_MONITORING_DATA_.*\\.rds$",
  update_temp_dir = eff_update_tmp,
  invalid_sites_path = file.path(cache_dir, "invalid_eff_sites.csv"),
  site_list_df = eff_sites_in,
  site_col = "notation",
  forcing_year = forcing_year
)

baseline_updated <- eff$baseline_updated
eff_new_file <- eff$new_file
eff_log_file <- eff$log_file

# ============================================================
# Determine sites valid for quantitative analysis (effluent)
# ============================================================

message("\n🕵️ Identifying site-dets suitable for quantitative analysis")
flush.console()

quant_PS_list <- baseline_updated %>%
  filter(SMPT_USER_REFERENCE %in% eff_sites_in$notation) %>%
  mutate(samp_yr = year(as.Date(SAMP_SAMPLE_DATE))) %>%
  group_by(SMPT_USER_REFERENCE, MEAS_DETERMINAND_CODE) %>%
  summarise(
    n_yrs = n_distinct(samp_yr),
    n_samps = n(),
    sufficient = if_else(n_yrs > 1 & n_samps > 11, "Y", "N"),
    .groups = "drop"
  ) %>%
  filter(sufficient == "Y") %>%
  ungroup()

# Load WIMS-WISKI lookup and append Flow_m3s eligibility
wims_wiski_lut <- read_tsv(
  here("PointSourceData", "Discharges", "data_in", "WISKI_to_WIMS_LUT.txt"),
  show_col_types = FALSE
) %>%
  filter(SMPT_USER_REFERENCE %in% baseline_updated$SMPT_USER_REFERENCE) %>%
  mutate(
    MEAS_DETERMINAND_CODE = "Flow_m3s",
    sufficient = "Y"
  )

quant_PS_list <- quant_PS_list %>%
  bind_rows(wims_wiski_lut) %>%
  mutate(
    MEAS_DETERMINAND_CODE = case_when(
      MEAS_DETERMINAND_CODE == "Flow_m3s" & is.na(WISKI_STATION) ~ NA_character_,
      TRUE ~ MEAS_DETERMINAND_CODE
    )
  ) %>%
  group_by(SMPT_USER_REFERENCE) %>%
  mutate(
    quant_for = toString(sort(unique(na.omit(MEAS_DETERMINAND_CODE))))
  ) %>%
  ungroup()

quant_PS_file <- here("PointSourceData", "Discharges", "data_out", "sites_for_quant_analysis.csv")
write_csv(quant_PS_list, quant_PS_file)

# ============================================================
# Fetch and update Xobs data 
# ============================================================

xobs <- run_update_pipeline(
  dataset_name = "HYPE_XOBS_DATA",
  baseline_pattern = "^HYPE_XOBS_DATA_.*\\.rds$",
  update_temp_dir = xobs_update_tmp,
  invalid_sites_path = file.path(cache_dir, "closed_sampling_points.csv"),
  site_list_df = xobs_lut,
  site_col = "notation",
  forcing_year = forcing_year,
  record_invalid_sites = FALSE
)

xobs_new_file <- xobs$new_file
xobs_log_file <- xobs$log_file

# ============================================================
# Declare outputs
# ============================================================

declared_outputs <- c(
  eff_new_file,
  eff_log_file,
  quant_PS_file,
  xobs_new_file,
  xobs_log_file
)

declared_outputs

# ============================================================
# Finally, remove old baseline files 
# ============================================================

unlink(eff_base_file, force = TRUE)
unlink(xob_base_file, force = TRUE)

message("\n✅ Effluent + Xobs update complete")
flush.console()

