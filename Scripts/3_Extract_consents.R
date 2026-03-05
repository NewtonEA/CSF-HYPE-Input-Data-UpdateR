# ============================================================
#
### Script written and tested by Tom Newton, Dec 2025
### NOTE: 
# Access ODBC drivers must be installed for the Access connection to work. 
#
# ============================================================

# ============================================================
# Config
# ============================================================


# load required packages
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  tidyverse,
  sf,
  odbc,
  DBI,
  here, 
  lubridate,
  rnrfa
)

# required input files
required_inputs <- c(
    here("PointSourceData", "Discharges", "data_in", "excluded_consent_types.csv"),
  here("GeoData", "Eco_HYPE_SUBBASINS.gpkg")
)

missing_inputs <- required_inputs[!file.exists(required_inputs)]
if (length(missing_inputs) > 0) {
  stop(
    "❌ Script 3 cannot run. Missing required input files:\n",
    paste(missing_inputs, collapse = "\n")
  )
} else {
  message("✅ Script 3: All required input files are present.")
}

# settings & paths
use_odbc <- TRUE   # set TRUE to use DBI::dbConnect(odbc::odbc(), ...) instead of RODBC (old method)
subdir <- here::here("PointSourceData", "Discharges")
data_in_dir <- file.path(subdir, "data_in")
data_out_dir <- file.path(subdir, "data_out")

dir.create(data_in_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(data_out_dir, showWarnings = FALSE, recursive = TRUE)

# function: safe pmin that returns NA if all inputs are NA (instead of Inf)
safe_pmin <- function(..., na.rm = TRUE) {
  vals <- list(...)
  mat <- do.call(cbind, vals)
  # if matrix has zero columns, return NA vector of length 1
  if (ncol(mat) == 0) return(NA_real_)
  res <- do.call(pmin, c(as.data.frame(mat), list(na.rm = na.rm)))
  # replace Inf (happens if all inputs were NA and na.rm = TRUE) with NA
  res[is.infinite(res)] <- NA_real_
  return(res)
}

# function: parse NGRs
safe_osg_parse <- function(ngr_vec) {
  # prefer rnrfa::osg_parse if available
  if ("osg_parse" %in% ls("package:rnrfa")) {
    coords <- rnrfa::osg_parse(ngr_vec)
    return(coords)
  }
  # fallback: attempt a very small parser for 10-digit NGRs like "ST 12345 67890"
  # returns a list(X, Y) of numerics or NA
  parse_one <- function(x) {
    if (is.na(x) || x == "" || grepl("^\\s*null\\s*$", x, ignore.case = TRUE)) return(c(NA_real_, NA_real_))
    # remove spaces
    s <- gsub("\\s+", "", x)
    # if already numeric comma-separated "E,N"
    if (grepl("^[0-9]+,[0-9]+$", s)) {
      parts <- as.numeric(strsplit(s, ",")[[1]])
      return(parts)
    }
    # otherwise give NA
    return(c(NA_real_, NA_real_))
  }
  parsed <- lapply(ngr_vec, parse_one)
  X <- vapply(parsed, `[[`, numeric(1), 1)
  Y <- vapply(parsed, `[[`, numeric(1), 2)
  list(X, Y)
}

# load excluded industry types 
excluded_types_file <- file.path(data_in_dir, "excluded_consent_types.csv")
if (!file.exists(excluded_types_file)) stop(" ❌ Missing excluded_consent_types.csv in data_in folder.")
excluded_types <- readr::read_csv(excluded_types_file, show_col_types = FALSE) %>%
  mutate(Industry_Type = stringr::str_replace_all(Industry_Type, "[^[:alnum:]]", " "))

# subbasin polygons
hype_file <- here::here("GeoData", "Eco_HYPE_SUBBASINS.gpkg")
if (!file.exists(hype_file)) {
  stop("❌ HYPE subbasins file not found: ", hype_file)
}
HYPE_subs <- sf::st_read(hype_file, quiet = TRUE)
message("🌊 Loaded HYPE sub-basin shapefile: ", format(nrow(HYPE_subs), big.mark = ","), " basins.")


env_types <- c("Freshwater river", "Pond/Lake/Reservoir")

# ============================================================
# Download and unzip latest Access database (overwrite existing)
# ============================================================

zip_url <- "https://environment.data.gov.uk/api/file/download?fileDataSetId=a54fdea1-7769-4b22-a518-10d51fed6f33&fileName=Consented%20Discharges%20to%20Controlled%20Waters%20with%20Conditions.zip"

zip_dest <- file.path(data_in_dir, "Consented_Discharges_latest.zip")

download_ok <- TRUE

message("⬇️ Attempting to download latest Consented Discharges dataset...")
flush.console()

tryCatch({
  
  # remove any previous zip
  if (file.exists(zip_dest)) {
    file.remove(zip_dest)
  }
  
  # download zip
  utils::download.file(
    url      = zip_url,
    destfile = zip_dest,
    mode     = "wb",
    quiet    = TRUE
  )
  
  # unzip (overwrite existing files)
  message("📦 Unzipping Access database (overwriting existing files)...")
  utils::unzip(
    zipfile   = zip_dest,
    exdir     = data_in_dir,
    overwrite = TRUE
  )
  
  # clean up zip
  file.remove(zip_dest)
  
  message("✅ Latest Access database downloaded and extracted successfully.")
  flush.console()
  
}, error = function(e) {
  
  download_ok <<- FALSE
  
  message("\n❌ FAILED to automatically download or unzip the Access database.")
  message("   Reason: ", conditionMessage(e), "\n")
  
  message("👉 Please manually download the latest dataset from:")
  message("   https://www.data.gov.uk/dataset/55b8eaa8-60df-48a8-929a-060891b7a109/consented-discharges-to-controlled-waters-with-conditions1")
  message("\n👉 Then unzip the archive and place the '.accdb' file into:")
  message("   ", data_in_dir, "\n")
  
  message("⚠️ Once the file is in place, type 'y' and press Enter to continue.")
  message("   Any other input will abort the script.\n")
  flush.console()
})


# Pause execution if auto-download failed
if (!download_ok) {
  
  user_input <- tolower(trimws(readline("Continue after manual download? (y/n): ")))
  
  if (user_input != "y") {
    stop("❌ Script halted. Access database not available.")
  }
  
  message("🔄 Continuing script using manually supplied Access database...\n")
  flush.console()
}

# Check if the Access database exists
access_file <- file.path(data_in_dir, "Consented Discharges to Controlled Waters with Conditions.accdb")
if (file.exists(access_file)) {
  file_info <- file.info(access_file)
  file_date <- format(file_info$mtime, "%d %B %Y")
  
  message("✅ Access database found:")
  message("   File: ", basename(access_file))
  message("   Location: ", access_file)
  message("   Last modified on: ", file_date, "\n")
  message("   If this is not the latest version, please download the most recent dataset from:")
  message("   https://www.data.gov.uk/dataset/55b8eaa8-60df-48a8-929a-060891b7a109/consented-discharges-to-controlled-waters-with-conditions1")
  message("   Then unzip the downloaded archive and place the '.accdb' file in the following folder:")
  message("   ", data_in_dir, "\n")
  flush.console()

  Sys.sleep(2) # allows messages to be read
  
} else {
  stop(paste0(
    "\n❌ Access database not found at:\n   ", access_file, "\n\n",
    "Please download the latest version of 'Consented Discharges to Controlled Waters with Conditions'\n",
    "from the following URL:\n",
    "   https://www.data.gov.uk/dataset/55b8eaa8-60df-48a8-929a-060891b7a109/consented-discharges-to-controlled-waters-with-conditions1\n\n",
    "After downloading, unzip the archive and place the '.accdb' file into:\n   ", data_in_dir, "\n"
  ))
}

# ============================================================
#      Connect to Access DB and fetch tables 
# ============================================================
message("Connecting to Access database...") ; flush.console()

if (use_odbc) {
  # DBI/odbc approach (requires Access ODBC driver) 
  con <- tryCatch({
    DBI::dbConnect(
      odbc::odbc(),
      .connection_string = paste0(
        "Driver={Microsoft Access Driver (*.mdb, *.accdb)};",
        "DBQ=", access_file
      )
    )
  }, error = function(e) stop("❌ ODBC DBI connection failed: ", e$message))
  
  # Fetch tables as tibbles
  all_consents <- DBI::dbReadTable(con, "consents_all") %>% as_tibble()
  effluents <- DBI::dbReadTable(con, "Effluents") %>% as_tibble()
  dets <- DBI::dbReadTable(con, "Determinands") %>% as_tibble()
  
  DBI::dbDisconnect(con)
  
} else {
  #Legacy RODBC approach 
  pacman::p_load(RODBC)
  con <- tryCatch({
    RODBC::odbcConnectAccess2007(access_file)
  }, error = function(e) stop("❌ RODBC connection failed: ", e$message))
  
  all_consents <- RODBC::sqlFetch(con, 'consents_all') %>% as_tibble()
  effluents <- RODBC::sqlFetch(con, 'Effluents') %>% as_tibble()
  dets <- RODBC::sqlFetch(con, 'Determinands') %>% as_tibble()
  
  RODBC::odbcClose(con)
};message("✅ Successfully fetched tables from Access DB.") ; flush.console()

# ============================================================
#       Clean determinand table 
# ============================================================

# Ensure DETE_CODE is a clean string without leading zeros or spaces
# filters for required determinands and converts to numeric 
dets <- dets %>%
  mutate(
    DETE_CODE = str_trim(as.character(DETE_CODE)),   # remove stray spaces
    DETE_CODE = str_replace_all(DETE_CODE, "^0+", "") # remove leading zeros
  ) %>%
  filter(
    METHOD == "ABSOLUTE",
    DETE_CODE %in% c("111", "116", "117", "118", "135", "180", "192", "348")
  ) %>%
  mutate(
    across(starts_with("VAL_"), ~ suppressWarnings(as.numeric(.x)))  # quietly coerce to numeric
  )

# ============================================================
#      process consents table 
# ============================================================

# clean/align fields and convert dates
all_consents <- all_consents %>%
  mutate(
    DSI_TYPE_DESCRIPTION = stringr::str_replace_all(DSI_TYPE_DESCRIPTION, "[^[:alnum:]]", " "),
    REVOCATION_DATE = as.Date(REVOCATION_DATE),
    REVOCATION_DATE = replace_na(REVOCATION_DATE, as.Date("3000-01-01")),
    EFFECTIVE_DATE = as.Date(EFFECTIVE_DATE),
    ISSUED_DATE = as.Date(ISSUED_DATE)
  ) %>%
  filter(
    REVOCATION_DATE > as.Date("1999-12-31"),
    EFFECTIVE_DATE < REVOCATION_DATE,
    REC_ENV_CODE_DESCRIPTION %in% env_types,
    !DSI_TYPE_DESCRIPTION %in% excluded_types$Industry_Type,
    !(DSI_TYPE_DESCRIPTION %in% c("null", NA))
  )

# ============================================================
# collect mean / mean-approximate values consistently
# ============================================================

# fix potential spelling mistakes in CODE_* labels
dets <- dets %>%
  mutate(across(starts_with("CODE"), ~ ifelse(. == "MINIIMUM VALUE", "MINIMUM VALUE", .)))

# get direct mean values first
mean_dets <- dets %>%
  filter(CODE_1 == "MEAN VALUE" | CODE_2 == "MEAN VALUE" | CODE_3 == "MEAN VALUE") %>%
  mutate(RES = pmin(VAL_1, VAL_2, VAL_3, na.rm = TRUE)) %>%
  select(EA_REGION:MONTH_TO, DETE_CODE:RES)

# 95%ile to mean (/2)
p95_dets <- dets %>%
  anti_join(mean_dets, by = c("EA_REGION", "DETE_CODE")) %>%
  filter(CODE_1 == "95 PERCENTILE" | CODE_2 == "95 PERCENTILE" | CODE_3 == "95 PERCENTILE") %>%
  mutate(RES = pmin(VAL_1, VAL_2, VAL_3, na.rm = TRUE) / 2) %>%
  select(EA_REGION:MONTH_TO, DETE_CODE:RES)

# Max to mean (/3)
max_dets <- dets %>%
  anti_join(mean_dets, by = c("EA_REGION", "DETE_CODE")) %>%
  anti_join(p95_dets, by = c("EA_REGION", "DETE_CODE")) %>%
  filter(CODE_1 == "MAXIMUM VALUE" | CODE_2 == "MAXIMUM VALUE" | CODE_3 == "MAXIMUM VALUE") %>%
  mutate(RES = pmin(VAL_1, VAL_2, VAL_3, na.rm = TRUE) / 3) %>%
  select(EA_REGION:MONTH_TO, DETE_CODE:RES)

# Min to mean (*3)
min_dets <- dets %>%
  anti_join(mean_dets, by = c("EA_REGION", "DETE_CODE")) %>%
  anti_join(p95_dets, by = c("EA_REGION", "DETE_CODE")) %>%
  anti_join(max_dets, by = c("EA_REGION", "DETE_CODE")) %>%
  filter(CODE_1 == "MINIMUM VALUE" | CODE_2 == "MINIMUM VALUE" | CODE_3 == "MINIMUM VALUE") %>%
  mutate(RES = pmin(VAL_1, VAL_2, VAL_3, na.rm = TRUE) * 3) %>%
  select(EA_REGION:MONTH_TO, DETE_CODE:RES)

dets_combined <- bind_rows(max_dets, mean_dets, p95_dets, min_dets) %>%
  mutate(
    DETE = case_when(
      DETE == "Ammoniacal Nitrogen as N" ~ "NH3-N_mgl",
      DETE == "Solids, Suspended at 105 C" ~ "SS_mgl",
      DETE == "Nitrate as N" ~ "Nitrate_mgl",
      DETE == "Phosphorus, Total as P" ~ "TP_mgl",
      DETE == "Nitrogen, Total Oxidised as N" ~ "TN_mgl",
      DETE == "Nitrite as N" ~ "Nitrite_mgl",
      DETE == "Orthophosphate, reactive as P" ~ "OP_mgl",
      DETE == "Phosphate :- {TIP}" ~ "TIP_mgl",
      TRUE ~ DETE
    )
  ) %>%
  unite("DETE", c("DETE_CODE", "DETE"), remove = FALSE) %>%
  select(-c(DETE_CODE, UNITS, METHOD)) %>%
  pivot_wider(names_from = DETE, values_from = RES, values_fn = mean)

rm(max_dets, mean_dets, p95_dets, min_dets, dets)

# ============================================================
#       Standardize effluent column names      
# ============================================================
# 2025 fix - the field names of the effluent table had changed - 
# this fixes that but also ensures if they happen to revert to the old format
# the code will still work

# Trim, normalise spaces, and replace dots
effluents <- effluents %>%
  rename_with(~ str_replace_all(.x, "\\.+", " ")) %>%  # dots ==> spaces
  rename_with(~ str_replace_all(.x, "\\s+", " ")) %>%  # multiple spaces ==> single
  rename_with(~ str_trim(.x))                          # remove leading/trailing spaces

# Safe renaming 
rename_first_exists <- function(df, new_name, old_names) {
  existing <- old_names[old_names %in% names(df)]
  if (length(existing) > 0) {
    df <- df %>% rename(!!new_name := all_of(existing[1]))
  }
  df
}

# Apply renaming to canonical names
effluents <- effluents %>%
  rename_first_exists("DWF (m3_d)",       c("DWF (m3_d)", "DWF m3_d", "DWF..m3_d.")) %>%
  rename_first_exists("MAX_DAILY (m3_d)", c("MAX_DAILY (m3_d)", "MAX_DAILY m3_d", "MAX_DAILY..m3_d.")) %>%
  rename_first_exists("MEAN (m3_d)",      c("MEAN (m3_d)", "MEAN m3_d", "MEAN..m3_d.")) %>%
  rename_first_exists("MAX_RATE (l_s)",   c("MAX_RATE (l_s)", "MAX_RATE l_s", "MAX_RATE..l_s."))

# Final trim to ensure no stray spaces remain
colnames(effluents) <- str_trim(colnames(effluents))

# Check
expected_cols <- c("MAX_DAILY (m3_d)", "MAX_RATE (l_s)", "DWF (m3_d)", "MEAN (m3_d)")
missing_cols <- setdiff(expected_cols, colnames(effluents))
if (length(missing_cols) > 0) {
  warning("⚠️ Missing expected columns in effluents table after renaming: ",
          paste(missing_cols, collapse = ", "))
} else {
  message("✅ Effluent column names standardised successfully.")
}

# ============================================================
#       Join and process      
# ============================================================

# Function for grouped mean
mean_or_na <- function(x) {
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

# Columns to explicitly select
core_cols <- c(
  "COMPANY_NAME", "DISCHARGE_SITE_NAME", "DISCHARGE_SITE_TYPE_CODE",
  "DSI_TYPE_DESCRIPTION", "ADD_OF_DISCHARGE_SITE_LINE_1", "ADD_OF_DISCHARGE_SITE_LINE_2",
  "ADD_OF_DISCHARGE_SITE_LINE_3", "ADD_OF_DISCHARGE_SITE_LINE_4", "ADD_OF_DISCHARGE_SITE_PCODE",
  "DISTRICT_COUNCIL", "DISCHARGE_NGR", "CATC_NAME",
  "CATCHMENT_CODE", "EA_REGION", "SOURCE",
  "PERMIT_NUMBER", "PERMIT_VERSION", "RECEIVING_WATER",
  "RECEIVING_ENVIRON_TYPE_CODE", "REC_ENV_CODE_DESCRIPTION", "ISSUED_DATE",
  "EFFECTIVE_DATE", "REVOCATION_DATE", "STATUS_OF_PERMIT",
  "STATUS_DESCRIPTION", "OUTLET_NUMBER", "OUTLET_TYPE_CODE",
  "OUTLET_TYPE_DESCRIPTION", "OUTLET_GRID_REF", "EFFLUENT_NUMBER",
  "EFFLUENT_TYPE", "EFF_TYPE_DESCRIPTION", "EFFLUENT_GRID_REF",
  "SPT_DESC", "EFF_SAMPLE_POINT", "EFF_TMEN_CODE",
  "TMEN_DESC"
)

consents_processed <- all_consents %>%
  # Join with effluents and dets
  inner_join(
    effluents,
    by = c(
      "EA_REGION",
      "SOURCE" = "REGION",
      "PERMIT_NUMBER" = "PERMIT_REF",
      "PERMIT_VERSION" = "VERSION",
      "OUTLET_NUMBER",
      "EFFLUENT_NUMBER"
    ),
    relationship = "many-to-many"
  ) %>%
  left_join(
    dets_combined,
    by = c(
      "EA_REGION",
      "SOURCE" = "REGION",
      "PERMIT_NUMBER" = "PERMIT_REF",
      "PERMIT_VERSION" = "VERSION",
      "OUTLET_NUMBER",
      "EFFLUENT_NUMBER"
    ),
    relationship = "many-to-many"
  ) %>%
  
  # Convert flows to mean approximations in m³/s
  mutate(
    `MAX_DAILY (m3_d)` = as.numeric(`MAX_DAILY (m3_d)`) / 86400 * 0.3333333,
    `MAX_RATE (l_s)`   = as.numeric(`MAX_RATE (l_s)`) / 3 * 0.001,
    `DWF (m3_d)`       = as.numeric(`DWF (m3_d)`) / 86400 * 1.25,
    `MEAN (m3_d)`      = as.numeric(`MEAN (m3_d)`) / 86400,
    
    Flow_m3s = if_else(
      !is.na(`MEAN (m3_d)`),                   # If a mean is consented
      `MEAN (m3_d)`,                           # Use it
      pmin(`MAX_DAILY (m3_d)`, `MAX_RATE (l_s)`, `DWF (m3_d)`, na.rm = TRUE)  # Otherwise use min approximation
    )
  ) %>%
  select(all_of(core_cols), matches("(mgl|m3s)$"), Flow_m3s) %>%
  # Group and summarise consented values
  group_by(EA_REGION, PERMIT_NUMBER, PERMIT_VERSION, OUTLET_NUMBER, EFFLUENT_NUMBER, EFFECTIVE_DATE, REVOCATION_DATE) %>%
  mutate(across(matches("(mgl|m3s)$"), mean_or_na)) %>%
  ungroup() %>%
  distinct(EA_REGION, PERMIT_NUMBER, PERMIT_VERSION, OUTLET_NUMBER, EFFLUENT_NUMBER, EFFECTIVE_DATE, REVOCATION_DATE, .keep_all = TRUE)

message("✅ Joined consents, effluents, and determinands successfully.")
flush.console()
rm(all_consents, dets_combined, effluents)

# Parse NGR -> XY      
OUTLET_XY <- safe_osg_parse(consents_processed$OUTLET_GRID_REF)
consents_processed <- consents_processed %>%
  mutate(OUTLET_X = OUTLET_XY[[1]],
         OUTLET_Y = OUTLET_XY[[2]])

# join SUBID 

# Ensure CRS compatibility and join
if (all(is.na(consents_processed$OUTLET_X)) || all(is.na(consents_processed$OUTLET_Y))) {
  warning("⚠️  No numeric OUTLET_X/Y parsed; skipping spatial join. Check NGR parsing.")
  consents_processed$SUBID <- NA_character_
} else {
  message("🗺️  Joining consents to HYPE sub-basins...")
  
  # perform spatial join safely
  consents_processed <- consents_processed %>%
    filter(!is.na(OUTLET_X) & !is.na(OUTLET_Y)) %>%
    st_as_sf(coords = c("OUTLET_X", "OUTLET_Y"), crs = st_crs(HYPE_subs)) %>%
    st_join(HYPE_subs["SUBID"], join = st_nearest_feature) %>%
    mutate(geom = as.character(geometry)) %>%
    st_drop_geometry() %>%
    rename(geometry = geom)
  
  # Success message and simple stats
  n_total <- nrow(consents_processed)
  n_matched <- sum(!is.na(consents_processed$SUBID))
  pct_matched <- round(100 * n_matched / n_total, 1)
  
  message("✅ Successfully joined consents to HYPE sub-basins.")
  flush.console()
  message("   ==> Total consents processed: ", format(n_total, big.mark = ","))
  flush.console()
  message("   ==> Consents matched to a SUBID: ", format(n_matched, big.mark = ","), 
          " (", pct_matched, "%)")
  flush.console()
  
  # Log unmatched consents to file
  if (n_matched < n_total) {
    n_unmatched <- n_total - n_matched
    
    message("⚠️  ", format(n_unmatched, big.mark = ","), 
            " consents could not be matched to a HYPE sub-basin (SUBID = NA).")
    flush.console()
    
    unmatched_consents <- consents_processed %>%
      filter(is.na(SUBID)) %>%
      mutate(log_message = "No matching HYPE sub-basin (SUBID); consent logged for review")
    
    write_csv(
      unmatched_consents,
      here(
        "PointSourceData", "Discharges", "data_out", "unmatched_consents_SUBID.csv"
      )
    )
    
    message(paste0("📝 Unmatched consents written to: \n ", 
                   here(
                     "PointSourceData", "Discharges", "data_out", "unmatched_consents_SUBID.csv"
                     )
                   )
    )
    flush.console()
  }
}

# ============================================================
#       Import Welsh consents from NRW datamap WFS      
# ============================================================

message("🌍 Reading Welsh consents from NRW DataMap WFS...")
flush.console()

nrw_url <- "https://datamap.gov.wales/capabilities/layer/3544/?ows_service=wfs"

Welsh_consents <- tryCatch({
  dat <- st_read(nrw_url, quiet = TRUE)
  
  # If successful, provide feedback to the user
  if (nrow(dat) > 0) {
    message("✅ Successfully read Welsh consents from NRW WFS.")
    flush.console()
    message("   ==> Records loaded: ", format(nrow(dat), big.mark = ","))
    flush.console()
    # If there’s a date column, give the range
    date_cols <- grep("DATE|Date|date", names(dat), value = TRUE)
    if (length(date_cols) > 0) {
      # Try to summarize the first date-like column
      date_vals <- suppressWarnings(as.Date(dat[[date_cols[1]]]))
      date_vals <- date_vals[!is.na(date_vals)]
      if (length(date_vals) > 0) {
        message("   ==> Date range: ", min(date_vals), " to ", max(date_vals))
        flush.console()
        }
    }
  } else {
    message("⚠️ Welsh consents WFS read completed but returned zero records.")
    flush.console()
    }
  
  dat
}, error = function(e) {
  stop("❌ Failed to read NRW datamap WFS: ", e$message, "\n",
       "Check the WFS URL is online or your internet connection:\n", nrw_url)
})


# transform & clean NRW table

Welsh_consents <- Welsh_consents %>%
  mutate(
    DISCHARGE_SITE_TYPE_CODE = str_sub(site_description, 1, 2),
    DSI_TYPE_DESCRIPTION = str_sub(site_description, 5, -1),
    CATC_NAME = "WALES",
    CATCHMENT_CODE = "NA",
    EA_REGION = "NRW",
    PERMIT_VERSION = 1,
    RECEIVING_WATER = receiving_waters,
    RECEIVING_ENVIRON_TYPE_CODE = as.integer(str_sub(receiving_environment, 1, 2)),
    REC_ENV_CODE_DESCRIPTION = str_sub(receiving_environment, 5, -1),
    REVOCATION_DATE = as.Date("3000-01-01"),
    EFFLUENT_TYPE = str_sub(release_type, 1, 2),
    EFF_TYPE_DESCRIPTION = str_sub(release_type, 5, -1),
    EFFLUENT_NUMBER = "1",
    OUTLET_TYPE_CODE = case_when(
      discharge_type == "Sewage - non Water Undertaker" ~ "U",
      discharge_type == "Sewage - Water Undertaker" ~ "S",
      discharge_type == "Sewage & Trade" ~ "Y",
      discharge_type == "Trade" ~ "T",
      discharge_type == "Miscellaneous" ~ "D",
      discharge_type == "Agriculture" ~ "A",
      TRUE ~ NA_character_
    ),
    OUTLET_GRID_REF = stringr::str_remove_all(outlet_ngr, " "),
    SPT_DESC = "NA",
    EFF_TMEN_CODE = "NA",
    TMEN_DESC = "NA",
    MONTH_TO = as.integer(factor(season_start_month, levels = month.name)),
    MONTH_FROM = as.integer(factor(season_end_month, levels = month.name))
  ) %>%
  # select determinands of interest
  filter(
    determinand %in% c(
      "Flow : Daily total", "Flow : Dry Weather :- {DWF}", "Solids, Suspended at 105 C",
      "Ammoniacal Nitrogen as N", "Flow, maximum", "Phosphorus, Total as P",
      "Nitrogen, Total as N", "Phosphate :- {TIP}", "Nitrate as N", "Ammonia un-ionised as N"
    )
  ) %>%
  select(
    COMPANY_NAME = operator, DISCHARGE_SITE_NAME = site_name,
    DISCHARGE_SITE_TYPE_CODE, DSI_TYPE_DESCRIPTION,
    ADD_OF_DISCHARGE_SITE_LINE_1 = site_address_1, ADD_OF_DISCHARGE_SITE_LINE_2 = site_address_2,
    ADD_OF_DISCHARGE_SITE_LINE_3 = site_address_3, ADD_OF_DISCHARGE_SITE_LINE_4 = site_town_or_city,
    ADD_OF_DISCHARGE_SITE_PCODE = site_postcode, DISTRICT_COUNCIL = local_authority, DISCHARGE_NGR = site_ngr,
    CATC_NAME, CATCHMENT_CODE, EA_REGION, SOURCE = online_public_reg_link, PERMIT_NUMBER = permit_number,
    PERMIT_VERSION, RECEIVING_WATER, RECEIVING_ENVIRON_TYPE_CODE, REC_ENV_CODE_DESCRIPTION,
    ISSUED_DATE = issue_date, EFFECTIVE_DATE = effective_date, REVOCATION_DATE,
    STATUS_OF_PERMIT = permit_status, STATUS_DESCRIPTION = permit_type,
    OUTLET_NUMBER = outlet_name, OUTLET_TYPE_CODE, OUTLET_TYPE_DESCRITION = discharge_type,
    OUTLET_GRID_REF, EFFLUENT_NUMBER, EFFLUENT_TYPE, EFF_TYPE_DESCRIPTION, EFFLUENT_GRID_REF = outlet_ngr,
    SPT_DESC, EFF_SAMPLE_POINT = sample_point_ngr, EFF_TMEN_CODE, TMEN_DESC,
    determinand_code:definition_3, limit_1:limit_3, season_start_month, season_end_month
  ) %>%
  mutate(
    limit_1 = as.numeric(limit_1),
    limit_2 = as.numeric(limit_2),
    limit_3 = as.numeric(limit_3),
    ISSUED_DATE = as.Date(ISSUED_DATE),
    EFFECTIVE_DATE = as.Date(EFFECTIVE_DATE),
    DSI_TYPE_DESCRIPTION = str_replace_all(DSI_TYPE_DESCRIPTION, "[^[:alnum:]]", " ")
  ) %>%
  st_join(HYPE_subs["SUBID"]) %>%
  st_set_geometry(NULL) %>%
  filter(!is.na(SUBID)) %>%
  group_by(PERMIT_NUMBER, PERMIT_VERSION, OUTLET_NUMBER, EFFECTIVE_DATE, EFFLUENT_NUMBER, determinand_code) %>%
  mutate(
    limit_1 = mean(limit_1, na.rm = TRUE),
    limit_2 = mean(limit_2, na.rm = TRUE),
    limit_3 = mean(limit_3, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  distinct()

#    Welsh dets consolidation 
mean_dets_w <- Welsh_consents %>%
  filter(definition_1 == "Mean" | definition_2 == "Mean" | definition_3 == "Mean") %>%
  mutate(RES = pmin(limit_1, limit_2, limit_3, na.rm = TRUE)) %>%
  select(EA_REGION:determinand, determinand_code:RES)

p95_dets_w <- Welsh_consents %>%
  anti_join(mean_dets_w, by = c("EA_REGION", "determinand_code")) %>%
  filter(definition_1 == "95%" | definition_2 == "95%" | definition_3 == "95%") %>%
  mutate(RES = pmin(limit_1, limit_2, limit_3, na.rm = TRUE) / 2) %>%
  select(EA_REGION:determinand, determinand_code:RES)

max_dets_w <- Welsh_consents %>%
  anti_join(mean_dets_w, by = c("EA_REGION", "determinand_code")) %>%
  anti_join(p95_dets_w, by = c("EA_REGION", "determinand_code")) %>%
  filter(definition_1 == "Max" | definition_2 == "Max" | definition_3 == "Max") %>%
  mutate(RES = pmin(limit_1, limit_2, limit_3, na.rm = TRUE) / 3) %>%
  select(EA_REGION:determinand, determinand_code:RES)

dets_w <- bind_rows(max_dets_w, mean_dets_w, p95_dets_w)

# convert flows and rename determinands
flow_dets <- c("Flow : Daily total", "Flow, maximum", "Flow : Dry Weather :- {DWF}")

dets_w <- dets_w %>%
  mutate(
    RES = case_when(
      determinand == flow_dets[1] ~ RES * 0.00000482253,  # daily total to m3/s
      determinand == flow_dets[2] ~ RES * 0.0004166667,   # max flow (l/s?) convert to m3/s approximation (you had this)
      determinand == flow_dets[3] ~ RES * 0.00001446759,  # DWF convert to m3/s approximation used earlier
      TRUE ~ RES
    ),
    determinand = case_when(
      determinand == "Ammoniacal Nitrogen as N" ~ "NH3-N_mgl",
      determinand == "Solids, Suspended at 105 C" ~ "SS_mgl",
      determinand == "Nitrate as N" ~ "Nitrate_mgl",
      determinand == "Phosphorus, Total as P" ~ "TP_mgl",
      determinand == "Nitrogen, Total as N" ~ "TN_mgl",
      determinand == "Nitrite as N" ~ "Nitrite_mgl",
      determinand == "Flow : Daily total" ~ "Flow",
      determinand == "Flow, maximum" ~ "Flow",
      determinand == "Flow : Dry Weather :- {DWF}" ~ "Flow",
      TRUE ~ determinand
    )
  ) %>%
  unite("DETE", c("determinand_code", "determinand"), remove = TRUE) %>%
  pivot_wider(names_from = DETE, values_from = RES, values_fn = mean) %>%
  mutate(Flow_m3s = do.call(safe_pmin, c(across(ends_with("_Flow")), na.rm = TRUE))) %>%
  select(-ends_with("_Flow"), -starts_with("limit"), -starts_with("definition"))

# join dets back to Welsh_consents and tidy
Welsh_consents <- Welsh_consents %>%
  ungroup() %>%
  select(COMPANY_NAME:TMEN_DESC) %>%
  distinct() %>%
  left_join(dets_w, by = intersect(names(.), names(dets_w))) %>%
  rename_with(~ gsub("^0+", "", .)) %>%
  group_by(PERMIT_NUMBER, PERMIT_VERSION, OUTLET_NUMBER, EFFECTIVE_DATE) %>%
  mutate(across(ends_with(c("mgl", "m3s")), ~ mean(.x, na.rm = TRUE)),
         across(ends_with(c("mgl", "m3s")), ~ replace_na(.x, NA_real_))) %>%
  filter(!DSI_TYPE_DESCRIPTION %in% excluded_types$Industry_Type) %>%
  distinct()

#function to harmonise column types across multiple data frames
harmonize_column_types <- function(dfs, type = "character") {
  # Get all column names
  all_cols <- unique(unlist(lapply(dfs, colnames)))
  
  dfs_fixed <- lapply(dfs, function(df) {
    for (col in all_cols) {
      if (!col %in% colnames(df)) next
      # Coerce to desired type if not already
      if (type == "character") {
        df[[col]] <- as.character(df[[col]])
      } else if (type == "numeric") {
        df[[col]] <- as.numeric(df[[col]])
      } else if (type == "integer") {
        df[[col]] <- as.integer(df[[col]])
      }
    }
    df
  })
  
  dfs_fixed
}

# Apply to consents & combine
dfs_list <- list(consents_processed, Welsh_consents)
dfs_list <- harmonize_column_types(dfs_list, type = "character")

Final_consents_to_use <- bind_rows(dfs_list) %>%
  distinct(); message("✅ English and Welsh consents combined successfully ")

out_rds <- file.path(
  data_out_dir,"consent_discharges_used_HYPE_with_consented_means.rds"
  )

readr::write_rds(Final_consents_to_use, out_rds); message("✅ Wrote consent RDS to: ", out_rds)
flush.console()

# create look-up for effluent monitoring sites
wims_file <- file.path(
  data_out_dir,"effluent_monitoring_sites.csv"
  )

Final_consents_to_use %>%
  filter(!is.na(EFF_SAMPLE_POINT)) %>%
  distinct(EFF_SAMPLE_POINT, EA_REGION) %>%
  mutate(notation = paste0(EA_REGION, "-", EFF_SAMPLE_POINT)) %>%
  readr::write_csv(wims_file); message(" ✅ Wrote Wffluent monitoring sites CSV to: ", wims_file)

# Done
message(" ✅  All consents successfully extracted and prepared " )
flush.console()


declared_outputs <- c(
  here("PointSourceData", "Discharges", "data_out", "consent_discharges_used_HYPE_with_consented_means.rds"),
  here("PointSourceData", "Discharges", "data_out", "effluent_monitoring_sites.csv")
)

declared_outputs

