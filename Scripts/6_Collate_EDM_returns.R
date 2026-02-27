# ============================================================
# EDM returns collatoR - Auto-download latest annual returns
# Written & tested: Tom Newton, updated January 2026
# Purpose: find new EDM Excel (or ZIP) files, read all sheets,
#          flexibly match required columns, combine with existing
#          collated EDM returns, and save for downstream use
# ============================================================
message("❯❯❯❯ Collating EDM returns...")
flush.console()

# ============================================================
# Load required packages
# ============================================================
if (!require(pacman)) install.packages("pacman")
pacman::p_load(here, 
               tidyverse,
               readxl,
               tictoc,
               httr,
               xml2, 
               rstudioapi)

required_inputs <- c(
  here("PointSourceData", "Discharges", "data_in"),   # base input dir
  here("PointSourceData", "Discharges", "data_out")   # base output dir
)

missing_inputs <- required_inputs[!dir.exists(required_inputs)]

if (length(missing_inputs) > 0) {
  stop(
    "Missing required directories for Script 6:\n",
    paste(" -", missing_inputs, collapse = "\n")
  )
}

# ============================================================
# Config
# ============================================================
subdir <- here("PointSourceData", "Discharges")
new_returns_dir <- file.path(subdir, "data_in", "EDM_returns")
existing_dir   <- file.path(subdir, "data_out", "EDM_returns_collated")

# required inputs
required_inputs <- c(
  here("PointSourceData", "Discharges", "data_in"),   # base input dir
  here("PointSourceData", "Discharges", "data_out")   # base output dir
)

missing_inputs <- required_inputs[!dir.exists(required_inputs)]

if (length(missing_inputs) > 0) {
  stop(
    "Missing required directories for Script 6:\n",
    paste(" -", missing_inputs, collapse = "\n")
  )
}

# ============================================================
# Function to extract 4-digit year from filename
# ============================================================
extract_year <- function(filename) {
  yr <- sub(".*?(\\d{4}).*", "\\1", filename)
  if (nzchar(yr) && grepl("^\\d{4}$", yr)) as.numeric(yr) else NA_real_
}

# ============================================================
# Load existing collated file
# ============================================================
existing_file <- list.files(existing_dir, pattern = "\\.rds$|EDM_returns_collated_", full.names = TRUE, recursive = FALSE)

existing_data <- NULL
if (length(existing_file) == 0) {
  message("⚠️ No previously collated EDM file found (this is OK for a first run).")
  flush.console()
} else if (length(existing_file) > 1) {
  stop("❗ Multiple existing collated EDM files found in existing_returns_collated/. Keep only one consolidated file.")
} else {
  existing_data <- read_rds(existing_file)
  message("✅ Previously collated EDM file loaded")
  flush.console()
}

# Current latest return year
current_latest_yr <- if (!is.null(existing_data)) max(existing_data$Return_yr, na.rm = TRUE) else 0

# ============================================================
# Locate new EDM Excel files, or download newer ZIPs
# ============================================================
if (!dir.exists(new_returns_dir)) dir.create(new_returns_dir, recursive = TRUE)

core_pattern <- "EDM.*Storm Overflow Annual Return - all water and sewerage companies"

new_data_files <- list.files(
  path = new_returns_dir,
  pattern = core_pattern,
  full.names = TRUE,
  ignore.case = TRUE,
  recursive = TRUE
)

# Attempt auto-download if folder empty (deafault)
    # but allow returns to be manually added if auto-download fails 
if (length(new_data_files) == 0 || current_latest_yr > 0) {
  
  message("⚠️ Checking for newer EDM Annual Return ZIPs online...")
  flush.console()
  
  # Get dataset via GetRecordById endpoint
  dataset_id <- "21e15f12-0df8-4bfc-b763-45226c16a8ac"
  dataset_url <- paste0("https://environment.data.gov.uk/discover/ea/csw?request=GetRecordById&id=", dataset_id)
  
  xml_doc <- tryCatch({
    res <- httr::GET(dataset_url)
    xml2::read_xml(httr::content(res, "text", encoding = "UTF-8"))
  }, error = function(e) stop("❌ Failed to fetch EDM dataset page: ", e$message))
  
  urls <- xml2::xml_find_all(xml_doc, ".//gmd:linkage/gmd:URL") %>% xml_text()
  
  # Keep only annual ZIPs (ignore long-term trends)
  annual_zip_urls <- urls[grepl("EDM_\\d{4}_Storm_Overflow_Annual_Return\\.zip$", urls)]
  
  if (length(annual_zip_urls) == 0 && length(list.files(new_returns_dir)) == 0) {
    stop(
      "❌ No EDM annual ZIPs found online AND no local EDM files in '", 
      new_returns_dir, 
      "'.\nPlease manually download the latest EDM return ZIP(s) from the Environment Agency dataset page and place them in this directory."
    )
  }
  
  # Keep only files newer than existing collated data
  get_year <- function(x) as.numeric(sub(".*EDM_(\\d{4})_.*", "\\1", x))
  years <- sapply(annual_zip_urls, get_year)
  
  new_urls <- annual_zip_urls[years > current_latest_yr]
  
  if (length(new_urls) == 0) {
    message("ℹ️ No new return data available for collation - continuing with existing data")
    new_data_files <- character(0)
  } else {
    for (zip_url in new_urls) {
      parsed <- httr::parse_url(zip_url)
      file_name <- parsed$query$fileName
      dest_zip <- file.path(new_returns_dir, file_name)
      
      message("⬇️ Downloading EDM ZIP: ", file_name)
      flush.console()
      tryCatch({
        utils::download.file(zip_url, destfile = dest_zip, mode = "wb")
        message("✅ Downloaded: ", dest_zip)
        flush.console()
      }, error = function(e) stop("❌ Failed to download EDM ZIP: ", e$message))
      
      tryCatch({
        utils::unzip(dest_zip, exdir = new_returns_dir)
        message("🗜️ Extracted ZIP contents")
        flush.console()
      }, error = function(e) stop("❌ Failed to unzip EDM ZIP: ", e$message))
    }
    
    # Re-search for Excel files after download
    new_data_files <- list.files(
      path = new_returns_dir,
      pattern = core_pattern,
      full.names = TRUE,
      ignore.case = TRUE,
      recursive = TRUE
    )
    
    if (length(new_data_files) == 0) {
      stop("❌ After download/extract, no EDM Excel files were found in '", new_returns_dir, "'.")
    } else {
      message(paste0("✅ Excel file(s) ready for processing: ", paste(basename(new_data_files), collapse = ", ")))
      flush.console()
    }
  }
}

# ============================================================
# If no new files, skip collation
# ============================================================
if (length(new_data_files) == 0) {
  message("⚠️ No new returns to add; using existing data only.")
  flush.console()
  updated_data <- existing_data
} else {
  
  # =========================
  # Column matching
  # =========================
  required_keywords <- c(
    "WATER COMPANY NAME",
    "SITE NAME",
    "EA PERMIT REFERENCE",
    "TOTAL DURATION",
    "COUNTED SPILLS",
    "EDM OPERATION"
  )
  
  renamed_columns <- c(
    "Water_Company_Name",
    "Site_Name",
    "PERMIT_NUMBER",
    "Duration",
    "Spills",
    "PerReport"
  )
  
  check_and_select_columns <- function(df, file, sheet) {
    raw_names <- colnames(df)
    clean_names <- toupper(gsub("[\r\n]", " ", raw_names)) %>% trimws()
    clean_names_alpha <- gsub("[^A-Z0-9 ]", "", clean_names)
    
    matched_cols <- character(0)
    for (kw in required_keywords) {
      kw_alpha <- gsub("[^A-Z0-9 ]", "", toupper(kw))
      idx <- grep(kw_alpha, clean_names_alpha, useBytes = TRUE)
      if (length(idx) == 0) stop(paste0("❌ Missing column containing keyword: '", kw, "' in file: ", basename(file), " sheet: ", sheet))
      matched_cols <- c(matched_cols, raw_names[idx[1]])
    }
    
    df %>% select(all_of(matched_cols)) %>% setNames(renamed_columns)
  }
  
  # =========================
  # Read & clean all sheets
  # =========================
  all_sheets_data <- list()
  for (file in new_data_files) {
    fn <- basename(file)
    reporting_year <- extract_year(fn)
    sheets <- excel_sheets(file)
    
    for (sheet in sheets) {
      sheet_data <- read_excel(file, sheet = sheet, skip = 1, col_types = "text", col_names = TRUE)
      if (nrow(sheet_data) == 0) next
      cleaned <- check_and_select_columns(sheet_data, file, sheet)
      cleaned$Return_yr <- reporting_year
      cleaned$._source_file <- fn
      cleaned$._source_sheet <- sheet
      all_sheets_data[[paste0(fn, "_", sheet)]] <- cleaned
      message(paste0("📄 Read & cleaned sheet: '", sheet, "' from ", fn))
      flush.console()
    }
  }
  
  if (length(all_sheets_data) == 0) stop("❌ No valid sheets were read from new EDM files.")
  
  # =========================
  # Combine new data & append to existing
  # =========================
  new_data <- bind_rows(all_sheets_data) %>%
    mutate(
      Spills = as.numeric(Spills),
      Duration = as.numeric(Duration),
      PerReport = as.numeric(PerReport),
      PerReport = ifelse(!is.na(PerReport) & PerReport < 1, PerReport * 100, PerReport)
    )
  
  if (!is.null(existing_data)) {
    updated_data <- bind_rows(existing_data, new_data) %>% distinct()
    years_existing <- paste(unique(existing_data$Return_yr), collapse = ", ")
    years_new <- paste(unique(new_data$Return_yr), collapse = ", ")
    message(paste0("⚙️ Appended new data (existing years: ", years_existing, "; new years added: ", years_new, ")"))
    flush.console()
  } else {
    updated_data <- new_data
    years <- paste(unique(new_data$Return_yr), collapse = ", ")
    message(paste0("⚙️ Using only new data (years: ", years, ")."))
    flush.console()
  }
}

# ============================================================
# Save collated EDM returns
# ============================================================
if (!is.null(updated_data)) {
  yr_min <- min(updated_data$Return_yr, na.rm = TRUE)
  yr_max <- max(updated_data$Return_yr, na.rm = TRUE)
  out_file <- file.path(existing_dir,"EDM_returns_collated.rds")
  write_rds(updated_data, out_file)
  message(paste0("💾 Collated EDM returns written: ", out_file, " (rows: ", nrow(updated_data), ")"))
  flush.console()
}

# ============================================================
# Cleanup ready for next update
# ============================================================

unlink(new_returns_dir, recursive = TRUE, force = TRUE)
dir.create(new_returns_dir, recursive = TRUE)

message("✅ EDM collation complete!")
flush.console()

# ============================================================
# Declare outputs
# ============================================================

declared_outputs <- c(
  here(
    "PointSourceData",
    "Discharges",
    "data_out",
    "EDM_returns_collated",
    "EDM_returns_collated.rds"
  )
)

declared_outputs

