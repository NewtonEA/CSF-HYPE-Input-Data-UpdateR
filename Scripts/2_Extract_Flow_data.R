# ============================================================
# Download Flow data for HYPE
#
#Script written  & tested by Tom Newton, Dec 2025 
# adapted from Sarah Warren's 'Download latest NRFA data.R'
# ============================================================

# ============================================================
# Config
# ============================================================

#load required packages
if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse,
               rnrfa,
               here,
               zoo)

# required input files 
required_inputs <- c(
  here("QObs", "data_in", "NRFA_IDs_SUBIDs_lookup.csv")
)

missing_inputs <- required_inputs[!file.exists(required_inputs)]
if (length(missing_inputs) > 0) {
  stop(
    "❌ Script 2 cannot run. Missing required input files:\n",
    paste(missing_inputs, collapse = "\n")
  )
} else {
  message("✅ Script 2: All required input files are present.")
}

# Import lookup file to find required gauging stations for which data are required

required_gauges <- read_csv(here("QObs", "data_in", "NRFA_IDs_SUBIDs_lookup.csv"))

# Issue with 26802 - NRFA ID is now 26016
required_gauges$STATION_NUMBER[which(required_gauges$STATION_NUMBER == "26802")] <- "26016"
# Issue with 27858 - NRFA ID appears to be 27099 although the website doesn't 
# mention that it's changed recently... same name, location and area
required_gauges$STATION_NUMBER[which(required_gauges$STATION_NUMBER == "27858")] <- "27099"

# ============================================================
# Extract all required GDF data from the NRFA
# ============================================================
rg <- required_gauges$STATION_NUMBER[!is.na(required_gauges$STATION_NUMBER)]


GDF_data <- map(rg, function(id) {
  message("⬇️ Fetching flow data for NRFA station: ", id)
  Sys.sleep(0.5) # avoid API rate limits
  tryCatch({
    gdf(id, metadata = TRUE)
  }, error = function(e) {
    message("⚠️ Failed for ", id, " — ", e$message)
    NULL
  })
})

# Remove NULL results - does nothing if fetching doesn't fail for any stations
GDF_data <- compact(GDF_data)
names(GDF_data) <- rg

# in case data fetching fails for any stations, check which 
# these will need iveseiagting or logging - have the station IDs changed? 
failed_ids <- rg[map_lgl(GDF_data, is.null)]
filter(required_gauges, STATION_NUMBER %in% failed_ids)

failed_ids_file <- here("QObs","NRFA_failed_ids.csv")

if(length(failed_ids) > 0) {
  write_csv(tibble(Failed_Stations = failed_ids), failed_ids_file)
  optional_outputs <- failed_ids_file # otional in the sense it won't be created if no IDs fail
} else {
  optional_outputs <- character(0) # empty character in this case, requiredf for logging 
}


## note as of Dec 2025 - station 26002 Hempholme Lock, River Hull no longer exists  - 
## not sure why it has been removed from the archive, assumed for a data quality reason so no longer used

# Set up output folder
out_folder <- here("QObs", "data_in")
if(!dir.exists(out_folder)){
  dir.create(out_folder)
}

# Create a sub-folder for the flow values
flow_folder <- file.path(out_folder, "Flow_data")
if(!dir.exists(flow_folder)){
  dir.create(flow_folder)
}

# Export metadata as one csv for all sites 
GDF_metadata <- map_dfr(GDF_data, function(site) {
  as_tibble(site$meta)
}, .id = "Station_ID"); write_csv(GDF_metadata, file.path(out_folder, "NRFA_GDF_metadata.csv"))


# Export flow data for each gauging station

walk2(GDF_data, names(GDF_data), function(site, siteName) {
  # Extract the data component 
  siteData <-  site$data %||% site$gdf
  
  # Ensure siteData is in data.frame/tibble format
  siteDF <- if (inherits(siteData, "zoo")) {
    data.frame(Date = index(siteData), coredata(siteData))
  } else if ("Date" %in% names(siteData)) {
    as_tibble(siteData)
  } else {
    as_tibble(siteData) %>% mutate(Date = row_number())
  }
  out_file <- file.path(flow_folder, paste0("NRFA_GDF_", siteName, ".csv"))
  write_csv(siteDF, out_file)
  
  message("✅ Exported: ", out_file)
flush.console()
  })

# Done
message("✅ Flow data extraction complete! ")
flush.console()

declared_outputs <- c(
  here("QObs","data_in","NRFA_GDF_metadata.csv"),
  list.files(
    here("QObs", "data_in", "Flow_data"),
    pattern = "^NRFA_GDF_.*\\.csv$",
    full.names = TRUE
  ),
  optional_outputs
)

declared_outputs