# ============================================================
# HYPE forcing data (Pobs, Tobs) updater
# 
# This script automates the downloading and processing of HadUK 
# 1km gridded rainfall and temperature data (requires a CEDA account)
# in the CSF-HYPE forcing data input files (Pobs.txt, Tobs.txt)
#
# Rainfall and Temperature averaging within sub-basins 
# is based on original method by Kay Shelton (JBA)
#
# Written by Tom Newton 
# Tested December 2025
#
# ============================================================

# ============================================================
# Config
# ============================================================

# load libraries
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  tidyverse,
  here,
  data.table, 
  httr, 
  jsonlite, 
  terra,
  sf,
  lubridate,
  exactextractr, 
  ncdf4
)

# required input files & directories 
required_inputs <- c(
  here("ForcingData", "Rainfall", "Pobs.txt"),
  here("ForcingData", "Temperature", "Tobs.txt"),
  here("GeoData", "Eco_HYPE_SUBBASINS.gpkg")
)

missing_inputs <- required_inputs[!file.exists(required_inputs)]

if (length(missing_inputs) > 0) {
  stop(
    "Missing required input files:\n",
    paste(missing_inputs, collapse = "\n")
  )
}

# set paths
forcing_dir <- here("ForcingData")
temp_dir <- file.path(forcing_dir, "Temperature")
rain_dir <- file.path(forcing_dir, "Rainfall")
data_dir <- file.path(forcing_dir, "HadUKRaw")


# load data

Pobs_old <- fread(file.path(rain_dir, "Pobs.txt"))
Tobs_old <- fread(file.path(temp_dir, "Tobs.txt"))
subs <- st_read(here("GeoData", "Eco_HYPE_SUBBASINS.gpkg"))

#determine period to update from 

update_from <- year(max(Pobs_old$DATE)+1)


# ============================================================
# HadUK-Grid 1km Daily (rainfall + temperature) downloadeR 
# Uses CEDA access token + curl for authenticated download
# ============================================================

# ============================================================
# User input: CEDA Access Token
# You must generate manually at:
# https://services.ceda.ac.uk/ceda_services/token/
# and paste it into the console when prompted below
# ============================================================

ceda_token <- readline(prompt = "Enter your CEDA Access Token: ")

if (nchar(ceda_token) < 20) stop("Error: token looks too short — please check, have you copied it correctly?.")


# Windows-compatible null device (required for curl requests)
null_device <- if (.Platform$OS.type == "windows") "NUL" else "/dev/null"

#set variables to download (others could be added as required)
variables <- c("rainfall", "tasmax", "tasmin")

# set years to download (daily files are for calendar month, but function takes care of that)
# uses Sys.date as to date, ensuring the latest available data is downloaded

update_to <- as.integer(format(Sys.Date(), "%Y"))
  
years <- update_from:update_to

# Ensure output dirs exist
invisible(
  lapply(variables, function(v) {
    dir.create(file.path(data_dir, paste0("Daily_", v)), recursive = TRUE, showWarnings = FALSE)
  })
)

# Get listing and resolved folder URL for a variable
get_file_listing <- function(variable) {
  latest_url <- paste0(
    "https://data.ceda.ac.uk/badc/ukmo-hadobs/data/insitu/MOHC/HadOBS/HadUK-Grid/",
    "v1.3.2.ceda/1km/", variable, "/day/v20260512/"
  )
  
  resolved_url <- system2(
    "curl",
    c("-Ls", "-o", null_device, "-w", "%{url_effective}", latest_url),
    stdout = TRUE
  )
  # resolved_url should be something like:
  # https://data.ceda.ac.uk/badc/.../v1.3.1.ceda/1km/rainfall/day/v20250415
  
  json_url <- paste0(resolved_url, "/?json")
  listing <- jsonlite::fromJSON(json_url)
  all_files <- listing$items$name
  
  pattern <- paste0("^", variable, "_hadukgrid_uk_1km_day_(", paste(years, collapse = "|"), ")")
  files_needed <- all_files[grepl(pattern, all_files)]
  
  list(files = files_needed, resolved_url = resolved_url)
}

# Download using the resolved_url (but switch domain to dap.ceda.ac.uk)
download_haduk_file <- function(f, variable, resolved_url) {
  
  # switch domain to dap (where files are served)
  download_folder <- sub("^https://data.ceda.ac.uk", "https://dap.ceda.ac.uk", resolved_url)
  
  # final download URL
  url <- paste0(download_folder, "/", f)
  
  outdir <- file.path(data_dir, paste0("Daily_", variable))
  outfile <- file.path(outdir, f)
  
  # Skip good files
  if (file.exists(outfile) && file.size(outfile) > 10e6) {
    cat("✔ Already downloaded:", f, "\n")
    return(TRUE)
  }
  
  cat("⬇ Downloading:", variable, ":", f, "\n")
  
  # Use curl with Authorization header
  # also record HTTP status code to a temp file so we can show it if failure occurs
  http_code_file <- tempfile("httpcode")
  # quiet, clean curl call:
  cmd <- sprintf(
    'curl -s -S -L -H "Authorization: Bearer %s" "%s" -o "%s"',
    ceda_token, url, outfile
  )

  # Run the command, capture the printed HTTP code (written to http_code_file)
  system(cmd, ignore.stdout = TRUE, ignore.stderr = FALSE)
  
  # read http code if present
  http_code <- tryCatch(readLines(http_code_file, warn = FALSE)[1], error = function(e) NA)
  unlink(http_code_file)
  
  # Validate file
  if (!file.exists(outfile) || file.size(outfile) < 10e6) {
    cat("✖ Failed or incomplete:", f, "\n")
    if (!is.na(http_code)) cat("  HTTP status code:", http_code, "\n")
    
    # show the start of the downloaded file (HTML error message often small)
    if (file.exists(outfile)) {
      cat("  First 300 chars of server response:\n")
      txt <- tryCatch(readBin(outfile, "raw", n = 400), error = function(e) NULL)
      if (!is.null(txt)) {
        # print as characters (replace non-printable)
        preview <- rawToChar(txt)
        preview <- gsub("\n", " ", preview)
        preview <- gsub("\\s+", " ", preview)
        cat(substr(preview, 1, 300), "\n")
      }
    }
    return(FALSE)
  }
  
  cat("✔ Downloaded:", f, "(", round(file.size(outfile) / 1e6, 1), "MB)\n")
  TRUE
}

# Run all variables
any_files_downloaded <- FALSE

for (v in variables) {
  
  cat("\n=====================================================\n")
  cat("Processing variable:", v, "\n")
  cat("=====================================================\n")
  
  listing <- get_file_listing(v)
  cat("Found", length(listing$files), v, "files.\n")
  
  # Only process if files were found
  if (length(listing$files) > 0) {
    results <- sapply(
      listing$files,
      download_haduk_file,
      variable = v,
      resolved_url = listing$resolved_url
    )
    
    cat("\nDownload summary for", v, ":\n")
    cat(sum(results), "successful\n")
    cat(sum(!results), "failed\n\n")
    
    # Track if any files were successfully downloaded
    if (sum(results) > 0) {
      any_files_downloaded <- TRUE
    }
  } else {
    cat("\nNo files to download for", v, "\n\n")
  }
}

# Exit early if no new files were downloaded
if (!any_files_downloaded) {
  cat("\n✔ No new forcing data to process. Existing files are up to date.\n")
  
  declared_outputs <- c(
    here("ForcingData", "Rainfall", "Pobs.txt"),
    here("ForcingData", "Temperature", "Tobs.txt")
  )
  
  declared_outputs  # Return existing file paths
  
} else {
  

# ============================================================
#  Update Pobs - daily average of all HadUK grid cells that
# intersect each sub-basin, weighted by fractional cell coverage
# ============================================================


# Find all rainfall NetCDF files downloaded
rain_files <- list.files(file.path(data_dir, "Daily_rainfall"), pattern = "\\.nc$", full.names = TRUE)

all_pobs_list <- list()  # store each file's results

# Ensure subs is in raster CRS
r0 <- rast(rain_files[1])
subs <- st_transform(subs, crs = crs(r0)) ; rm(r0)

for (rain_nc in rain_files) {
  
  cat("Processing:", rain_nc, "\n")
  
  # Read raster
  r <- rast(rain_nc)

  # Get fill value
  nc <- nc_open(rain_nc)
  fill_value <- ncatt_get(nc, varid = "rainfall", "_FillValue")$value
  nc_close(nc)
  
  # Replace fill values with NA
  r[r == fill_value] <- NA
  
  # Extract polygon means
  rain_by_sub <- exact_extract(
    as(r, "Raster"),   # convert to Raster* for exactextractr
    subs,
    fun = "mean"
  )
  
  dates <- terra::time(r)
  
  # Convert to long table
  rain_dt <- as.data.table(rain_by_sub)
  rain_dt[, SUBID := subs$SUBID]
  
  rain_long <- melt(
    rain_dt,
    id.vars = "SUBID",
    variable.name = "layer",
    value.name = "rain_mm"
  )
  
  rain_long[, layer_index := seq_len(.N), by = SUBID]
  rain_long[, DATE := dates[layer_index]]
  rain_long[, layer_index := NULL]
  
  # Cast to wide HYPE Pobs format
  pobs_dt <- dcast(
    rain_long,
    DATE ~ SUBID,
    value.var = "rain_mm"
  )
  
  all_pobs_list[[rain_nc]] <- pobs_dt
}

# Combine all months/days into one table
new_pobs <- rbindlist(all_pobs_list)
setorder(new_pobs, DATE)

# Convert POSIXct → Date → IDate (to match pobs_old)
new_pobs[, DATE := as.IDate(DATE)]

# Round all columns except DATE to 2 decimal places
num_cols <- setdiff(names(new_pobs), "DATE")
new_pobs[, (num_cols) := lapply(.SD, round, 2), .SDcols = num_cols]

# Append to old Pobs and remove potential duplicate dates
Pobs_updated <- rbind(Pobs_old, new_pobs)
Pobs_updated <- unique(Pobs_updated, by = "DATE")  # keep first occurrence

# Write back to file
fwrite(Pobs_updated, file.path(rain_dir, "Pobs.txt"))

cat("✔ Pobs.txt updated with new rainfall data.\n")


# ============================================================
#  Update Tobs - daily mean temperature per sub-basin
#  Mean = (tasmin + tasmax) / 2
# ============================================================

# Find tasmin / tasmax NetCDF files
tasmin_files <- list.files(
  file.path(data_dir, "Daily_tasmin"),
  pattern = "\\.nc$",
  full.names = TRUE
)

tasmax_files <- list.files(
  file.path(data_dir, "Daily_tasmax"),
  pattern = "\\.nc$",
  full.names = TRUE
)

# Ensure files match (same months)
get_daterange <- function(x) {
  sub(".*_(\\d{8}-\\d{8})\\.nc$", "\\1", basename(x))
}

tasmin_dates <- get_daterange(tasmin_files)
tasmax_dates <- get_daterange(tasmax_files)

stopifnot(identical(tasmin_dates, tasmax_dates))

all_tobs_list <- list()

for (i in seq_along(tasmin_files)) {
  
  cat("Processing temperature:", basename(tasmin_files[i]), "\n")
  
  # Read rasters
  r_min <- rast(tasmin_files[i])
  r_max <- rast(tasmax_files[i])
  
  # Read fill values
  nc_min <- nc_open(tasmin_files[i])
  fill_min <- ncatt_get(nc_min, varid = "tasmin", "_FillValue")$value
  nc_close(nc_min)
  
  nc_max <- nc_open(tasmax_files[i])
  fill_max <- ncatt_get(nc_max, varid = "tasmax", "_FillValue")$value
  nc_close(nc_max)
  
  # Replace fill values with NA
  r_min[r_min == fill_min] <- NA
  r_max[r_max == fill_max] <- NA
  
  # Extract polygon means
  tasmin_by_sub <- exact_extract(
    as(r_min, "Raster"),
    subs,
    fun = "mean"
  )
  
  tasmax_by_sub <- exact_extract(
    as(r_max, "Raster"),
    subs,
    fun = "mean"
  )
  
  dates <- terra::time(r_min)
  
  # Convert to data.tables
  tmin_dt <- as.data.table(tasmin_by_sub)
  tmax_dt <- as.data.table(tasmax_by_sub)
  
  tmin_dt[, SUBID := subs$SUBID]
  tmax_dt[, SUBID := subs$SUBID]
  
  # Long format
  tmin_long <- melt(tmin_dt, id.vars = "SUBID", variable.name = "layer", value.name = "tmin")
  tmax_long <- melt(tmax_dt, id.vars = "SUBID", variable.name = "layer", value.name = "tmax")
  
  # Assign dates
  tmin_long[, layer_index := seq_len(.N), by = SUBID]
  tmin_long[, DATE := dates[layer_index]]
  tmin_long[, layer_index := NULL]
  
  tmax_long[, layer_index := seq_len(.N), by = SUBID]
  tmax_long[, DATE := dates[layer_index]]
  tmax_long[, layer_index := NULL]
  
  # Merge min/max and compute mean
  tmean_long <- merge(
    tmin_long,
    tmax_long,
    by = c("SUBID", "DATE")
  )
  
  tmean_long[, temp := (tmin + tmax) / 2]
  
  # Cast to wide HYPE Tobs format
  tobs_dt <- dcast(
    tmean_long,
    DATE ~ SUBID,
    value.var = "temp"
  )
  
  all_tobs_list[[i]] <- tobs_dt
}

# Combine all temperature data
new_tobs <- rbindlist(all_tobs_list)
setorder(new_tobs, DATE)

# Convert DATE to IDate
new_tobs[, DATE := as.IDate(DATE)]

# Round to 2 decimal places
num_cols <- setdiff(names(new_tobs), "DATE")
new_tobs[, (num_cols) := lapply(.SD, round, 2), .SDcols = num_cols]

# Append to old Tobs and remove duplicates
Tobs_updated <- rbind(Tobs_old, new_tobs)
Tobs_updated <- unique(Tobs_updated, by = "DATE")

# Write back to file
fwrite(Tobs_updated, file.path(temp_dir, "Tobs.txt"))

cat("✔ Tobs.txt updated with new temperature data.\n")



# ============================================================
# Write forcing metadata 
# ============================================================

latest_forcing_date <- max(Tobs_updated$DATE)

forcing_metadata <- list(
  latest_forcing_date = latest_forcing_date,  
  latest_forcing_year = as.integer(format(latest_forcing_date, "%Y")),
  generated_at = Sys.time(),
  script = "1_Update_Forcing_data.R"
)

metadata_file <- here("ForcingData", "_forcing_metadata.rds")

saveRDS(forcing_metadata, metadata_file)

message("📌 Forcing metadata written to: ", metadata_file)
message("📅 Latest forcing year: ", forcing_metadata$latest_forcing_year)
flush.console()

# ============================================================
# Cleanup 
# ============================================================
#remove raw data
unlink(data_dir, recursive = TRUE, force = TRUE)

# Done
cat("✔ Forcing data update complete! \n")

declared_outputs <- c(
  here("ForcingData", "Rainfall", "Pobs.txt"),
  here("ForcingData", "Temperature", "Tobs.txt"),
  here("ForcingData", "_forcing_metadata.rds")
)

declared_outputs
}
