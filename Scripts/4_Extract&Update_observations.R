# ============================================================
# Effluent incremental updater - transactional update workflow
# 
# Skips fetching if baseline already covers model timeframe
# where timeframe is determined by latest forcing data availability
# 
# Fetches effluent monitoring data for sites attached to Discharge consents
# via the Water Quality Explorer API
#
# Written and tested by Tom Newton, Jan 2026
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
  data.table
)

# Ensure clean future state
plan(sequential)
options(future.globals.maxSize = 5 * 1024^3)

# ============================================================
# Paths & constants
# ============================================================

cache_dir       <- here("PointSourceData", "Discharges", "api_cache")
update_temp_dir <- file.path(cache_dir, "update_temp")

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
  here("PointSourceData", "Discharges", "data_in", "WISKI_to_WIMS_LUT.txt"),
  here("ForcingData", "_forcing_metadata.rds")
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

# ============================================================
# Helper functions (must be top-level for future_lapply)
# ============================================================

safe_api_GET <- function(url, query = list(), max_attempts = 6, base_wait = 0.5) {
  for (attempt in seq_len(max_attempts)) {
    res <- tryCatch(
      GET(url, query = query,
          add_headers(Accept = api_accept_header, `API-Version` = api_version)),
      error = function(e) NULL
    )
    if (!is.null(res)) {
      status <- status_code(res)
      if (status >= 200 && status < 300) return(res)
      if (!(status == 429 || status >= 500)) return(res)
    }
    Sys.sleep(base_wait * 2^(attempt - 1) * runif(1, 0.5, 1.5))
  }
  NULL
}

safe_site_filename <- function(site) {
  paste0("site_", gsub("[^A-Za-z0-9_\\-]", "_", site), ".rds")
}

flatten_and_clean <- function(df) {
  if (nrow(df) == 0) return(tibble())
  df <- as_tibble(df)
  list_cols <- names(df)[sapply(df, is.list)]
  if (length(list_cols) > 0) {
    for (col in list_cols) {
      df[[col]] <- sapply(df[[col]], function(x) {
        if (is.null(x) || length(x) == 0) return(NA_character_)
        if (is.data.frame(x)) paste(apply(x,1,function(r) paste(r,collapse="|")), collapse=";")
        else paste(as.character(x), collapse=";")
      })
    }
  }
  df %>%
    select(
      SMPT_USER_REFERENCE = hasSamplingPoint.notation,
      SMPT_LONG_NAME      = hasSamplingPoint.prefLabel,
      MEAS_SAMPLE_ID      = hasSample.isResultOf.id,
      MEAS_DETERMINAND_CODE = observedProperty.notation,
      DETE_DESC           = observedProperty.prefLabel,
      MEAS_RESULT         = hasSimpleResult,
      DETE_UNIT           = hasResult.hasUnit.notation,
      UNIT_SHORT_DESC     = hasResult.hasUnit.altLabel,
      SAMP_SAMPLE_DATE    = phenomenonTime
    ) %>%
    mutate(
      MEAS_SAMPLE_ID = as.numeric(sub(".*/","",MEAS_SAMPLE_ID)),
      SAMP_DT = ymd_hms(SAMP_SAMPLE_DATE, quiet = TRUE),
      SAMP_SAMPLE_DATE = as.Date(SAMP_DT),
      MEAS_SIGN = str_extract(MEAS_RESULT, "^[<>]"),
      MEAS_RESULT = as.numeric(str_extract(MEAS_RESULT, "[0-9.]+"))
    ) %>%
    select(-SAMP_DT)
}

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
    j <- fromJSON(txt, simplifyVector = TRUE)
    if (!"member" %in% names(j) || length(j$member) == 0) break
    df <- flatten_and_clean(as_tibble(jsonlite::flatten(j$member)))
    all_chunks[[length(all_chunks)+1]] <- df
    if (nrow(df) < per_call_limit) break
    skip <- skip + per_call_limit
    Sys.sleep(sleep_between_calls)
  }
  if (length(all_chunks)==0) return(tibble())
  bind_rows(all_chunks)
}

fetch_updates_for_site <- function(site, det_meta_df, dateTo = Sys.Date()) {
  site_res <- list()
  for (i in seq_len(nrow(det_meta_df))) {
    det <- det_meta_df$det[i]
    dateFrom <- det_meta_df$latest_date[i]
    if (is.na(dateFrom)) dateFrom <- as.Date("2000-01-01")
    new_df <- fetch_determinand_for_site(site, det, dateFrom, dateTo)
    if (nrow(new_df) > 0) site_res[[length(site_res)+1]] <- new_df
  }
  if (length(site_res)==0) return(tibble())
  bind_rows(site_res) %>% distinct()
}

# ============================================================
# Read baseline year
# ============================================================

baseline_file <- list.files(
  cache_dir,
  pattern = "^HYPE_EFFLUENT_MONITORING_DATA_.*\\.rds$",
  full.names = TRUE
)
baseline_file_name <- basename(baseline_file[1])
baseline_date <- str_extract(baseline_file_name, "\\d{4}-\\d{2}-\\d{2}")
baseline_year <- as.integer(substr(baseline_date, 1, 4))

# ============================================================
# Read forcing metadata
# ============================================================

forcing_metadata_file <- here("ForcingData", "_forcing_metadata.rds")
forcing_metadata <- readRDS(forcing_metadata_file)
forcing_year <- forcing_metadata$latest_forcing_year

# ============================================================
# Determine whether to skip fetching
# ============================================================

skip_fetch <- baseline_year >= forcing_year

if (skip_fetch) {
  message("ℹ️ Effluent data (", baseline_year, 
          ") already up to date for forcing data (", forcing_year, ").")
  message("➡️ Skipping data fetch. Using existing baseline only.")
  flush.console()
  baseline_updated <- readRDS(baseline_file[1])
} else {
  
  # ============================================================
  # Load effluent site list
  # ============================================================
  eff_sites_in <- read_csv(
    here("PointSourceData", "Discharges", "data_out", "effluent_monitoring_sites.csv"),
    show_col_types = FALSE
  )
  
  # ============================================================
  # Load invalid sites list (if exists)
  # ============================================================
  invalid_sites_path <- file.path(cache_dir, "invalid_eff_sites.csv")
  invalid_sites <- if (file.exists(invalid_sites_path)) {
    read_csv(invalid_sites_path, show_col_types = FALSE) %>%
      rename(notation = 1) %>%
      mutate(notation = as.character(notation))
  } else {
    tibble(notation = character())
  }
  
  # ============================================================
  # Prepare directories
  # ============================================================
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(update_temp_dir, showWarnings = FALSE, recursive = TRUE)
  
  # ============================================================
  # Read full baseline & build site-det lookup
  # ============================================================
  baseline <- readRDS(baseline_file[1])
  meta <- baseline %>%
    rename(notation = SMPT_USER_REFERENCE, det = MEAS_DETERMINAND_CODE) %>%
    group_by(notation, det) %>%
    summarise(latest_date = max(SAMP_SAMPLE_DATE, na.rm = TRUE), .groups="drop")
  site_det_lookup <- split(meta %>% select(det, latest_date), meta$notation)
  
  unknown_sites <- eff_sites_in %>%
    filter(!notation %in% names(site_det_lookup)) %>%
    filter(!notation %in% invalid_sites$notation)
  
  message("Potential new sites requiring API confirmation: ", nrow(unknown_sites))
  flush.console()
  
  # ============================================================
  # Fetch updates in parallel safely
  # ============================================================
  fetch_site_data <- function(max_retries = 10) {
    
    per_site_file <- function(site) {
      file.path(update_temp_dir, paste0("data_", safe_site_filename(site)))
    }
    
    for (attempt in seq_len(max_retries)) {
      
      sites_to_fetch <- names(site_det_lookup)[
        !file.exists(vapply(names(site_det_lookup), per_site_file, character(1)))
      ]
      
      message("Attempt ", attempt, ": fetching ", length(sites_to_fetch), " sites")
      flush.console()
      
      result <- tryCatch({
        
        workers <- max(1L, floor(future::availableCores() * workers_pct))
        future::plan(future::multisession, workers = workers)
        
        ## prevent huge accidental exports
        options(
          future.stdout = TRUE,
          future.globals.maxSize = 1e9
        )
        
        future.apply::future_lapply(
          sites_to_fetch,
          function(site) {
            
            det_meta <- site_det_lookup[[site]]
            
            if (is.null(det_meta)) {
              saveRDS(tibble(), per_site_file(site))
              return(NULL)
            }
            
            res <- fetch_updates_for_site(site, det_meta, Sys.Date())
            saveRDS(res, per_site_file(site))
            NULL
          },
          future.seed = TRUE
        )
        
        future::plan(future::sequential)
        
        bind_rows(
          baseline,
          bind_rows(lapply(
            list.files(update_temp_dir, pattern = "^data_.*\\.rds$", full.names = TRUE),
            readRDS
          ))
        ) %>%
          distinct(
            SMPT_USER_REFERENCE,
            MEAS_SAMPLE_ID,
            MEAS_DETERMINAND_CODE,
            .keep_all = TRUE
          )
        
      }, error = function(e) {
        message("⚠️ Attempt failed: ", conditionMessage(e))
        NULL
      })
      
      if (!is.null(result)) return(result)
      
      Sys.sleep(2)
    }
    
    stop("❌ All sites could not be fetched after ", max_retries, " attempts.")
  }
  
  
  baseline_updated <- fetch_site_data()
  
} # end skip_fetch branch

# ============================================================
# Save outputs
# ============================================================

timestamp <- format(Sys.Date(), "%Y-%m-%d")
new_file <- file.path(
  cache_dir,
  paste0("HYPE_EFFLUENT_MONITORING_DATA_2000-", timestamp, ".rds")
)

saveRDS(baseline_updated, new_file)
if (!skip_fetch && file.exists(baseline_file[1])) file.remove(baseline_file[1])

write_csv(
  baseline_updated %>%
    group_by(SMPT_USER_REFERENCE, MEAS_DETERMINAND_CODE) %>%
    summarise(
      n_obs = n(),
      earliest_date = min(SAMP_SAMPLE_DATE, na.rm = TRUE),
      latest_date = max(SAMP_SAMPLE_DATE, na.rm = TRUE),
      .groups = "drop"
    ),
  file.path(cache_dir, "effluent_data_log.csv")
)

unlink(update_temp_dir, recursive = TRUE, force = TRUE)

# ============================================================
# Determine sites valid for quantitative analysis
# ============================================================
message("🕵️ Identifying site-dets suitable for quantitative analysis")
flush.console()

# check which monitoring points and det combination meet the minimum 
# requirement of 12 samples over 2 years for quantitative assessment 

quant_PS_list <- baseline_updated %>% 
  #filter for effluent monitoring sites
  filter(SMPT_USER_REFERENCE %in% eff_sites_in$notation) %>%
  mutate(samp_yr = strftime(SAMP_SAMPLE_DATE, "%Y")) %>%
  group_by(SMPT_USER_REFERENCE, MEAS_DETERMINAND_CODE) %>% 
  summarise(
    n_yrs = n_distinct(samp_yr),
    n_samps = n(),
    sufficient = case_when(
      n_yrs > 1 & n_samps > 11 ~ "Y",
      TRUE ~ "N"
    ),
    .groups = "drop"
  ) %>% 
  filter(sufficient == "Y") %>%
  ungroup()

#load WIMS-WISKI lookup

wims_wiski_lut <- read_tsv(
  here(
    "PointSourceData", "Discharges", "data_in", "WISKI_to_WIMS_LUT.txt"
    )
  ) %>%
  #filter for wims sites in effluent data
  filter(SMPT_USER_REFERENCE %in% baseline_updated$SMPT_USER_REFERENCE) %>%
  mutate(MEAS_DETERMINAND_CODE = "Flow_m3s", 
         sufficient = "Y") 

quant_PS_list <- quant_PS_list %>%
  bind_rows(wims_wiski_lut) %>%
  # flag sites that aren't quantitative for flow by setting the flow det to NA
  mutate(MEAS_DETERMINAND_CODE = case_when(MEAS_DETERMINAND_CODE == "Flow_m3s" 
                                           & is.na(WISKI_STATION) ~ NA_character_, 
                                           .default = MEAS_DETERMINAND_CODE)) %>%
  group_by(SMPT_USER_REFERENCE) %>% 
  mutate(
    quant_for = toString(sort(unique(na.omit(MEAS_DETERMINAND_CODE))))
  ) %>%
  ungroup()


quant_PS_file <- here(
  "PointSourceData","Discharges","data_out", "sites_for_quant_analysis.csv"
)
write_csv(quant_PS_list, quant_PS_file)
  

# ============================================================
# Declare outputs
# ============================================================

message("✅ Effluent update complete")
flush.console()

declared_outputs <- c(
  new_file,
  file.path(cache_dir, "effluent_data_log.csv"),
  quant_PS_file
)

declared_outputs

