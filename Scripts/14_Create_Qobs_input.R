# ============================================================
# Create Qobs.txt input file for CSF-HYPE
# 
#Reads individual NRFA CSVs: NRFA_GDF_<station>.csv
# ============================================================

message("🛠️ Creating Qobs file...")
flush.console()

# ============================================================
# Config
# ============================================================
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(data.table,
               here)


# ============================================================
# Required inputs
# ============================================================

required_inputs <- c(
  here("QObs", "data_in", "NRFA_IDs_SUBIDs_lookup.csv"), 
  here("QObs", "data_in", "Flow_data"), 
  here("ForcingData", "_forcing_metadata.rds")
)

missing_inputs <- required_inputs[!file.exists(required_inputs)]
if (length(missing_inputs) > 0) {
  stop(
    "❌ Qobs script cannot run. Missing required input files:\n",
    paste(missing_inputs, collapse = "\n")
  )
} else {
  message("✅ Script 14: All required input files are present.")
  flush.console()
}

# ============================================================
# Load inputs
# ============================================================

site_lut     <- fread(here("QObs", "data_in", "NRFA_IDs_SUBIDs_lookup.csv"))
forcing_meta <- readRDS(here("ForcingData", "_forcing_metadata.rds"))
flow_files <- list.files(
  here("QObs", "data_in", "Flow_data"),
  pattern = "^NRFA_GDF_\\d+\\.csv$",
  full.names = TRUE
  )

# Load and combine all NRFA CSVs

read_one <- function(fp) {
  st <- sub("^NRFA_GDF_(\\d+)\\.csv$", "\\1", basename(fp))
  dt <- fread(fp)
  
  # Normalise columns: Date + gdf

  if (!all(c("Date", "gdf") %in% names(dt))) {
    stop("❌ Unexpected columns in ", basename(fp),
         ". Expected at least: Date, gdf. Found: ", paste(names(dt), collapse = ", "))
  }
  
  dt[, `:=`(
    STATION_NUMBER = as.integer(st),
    date = as.IDate(Date),
    q = as.numeric(gdf)
  )]
  
  dt[, .(STATION_NUMBER, date, q)]
}

qob_dt <- rbindlist(lapply(flow_files, read_one), use.names = TRUE)

# ============================================================
# Prepare data: dates, join SUBID, daily mean
# ============================================================

# Join SUBID
setkey(site_lut, STATION_NUMBER)
setkey(qob_dt, STATION_NUMBER)
qob_dt <- site_lut[qob_dt, on = .(STATION_NUMBER)]

# Collapse to daily mean per SUBID-date 
qob_daily <- qob_dt[
  ,
  .(q = mean(q, na.rm = TRUE)),
  by = .(SUBID, date)
]

# ============================================================
# Drop gauges that are too short (min 5 years of non-missing days)
# ============================================================

min_years <- 5L
min_days  <- 365L * min_years

# Count non-missing daily values per SUBID in the model period
subid_counts <- qob_daily[
  q > -Inf & !is.na(q),        # qob_daily already has q values; this is just "non-missing"
  .(n_nonmissing = sum(!is.na(q))),
  by = SUBID
]

# SUBIDs that pass
subids_keep <- subid_counts[n_nonmissing >= min_days, SUBID]

# report what got dropped
subids_drop <- setdiff(unique(qob_daily$SUBID), subids_keep)
message("ℹ️ SUBIDs kept: ", length(subids_keep), " / ", length(unique(qob_daily$SUBID)))
message("ℹ️ SUBIDs dropped (<", min_days, " non-missing days): ", length(subids_drop))
flush.console()

# Filter daily data
qob_daily <- qob_daily[SUBID %in% subids_keep]

# ============================================================
# Make full daily grid: all SUBIDs x all dates
# ============================================================

start_date <- as.IDate("2000-01-01")
end_date   <- as.IDate(forcing_meta$latest_forcing_date)

subids <- sort(unique(qob_daily$SUBID))
dates  <- seq(start_date, end_date, by = "day")
full_grid <- CJ(SUBID = subids, date = dates, unique = TRUE)

setkey(qob_daily, SUBID, date)
setkey(full_grid, SUBID, date)

qob_full <- qob_daily[full_grid]  # keep all rows in full_grid

# Fill missing with -9999
qob_full[is.na(q), q := -9999]

# ============================================================
# Reshape wide: one column per SUBID
# ============================================================

qob_wide <- dcast(
  qob_full,
  date ~ SUBID,
  value.var = "q"
)

# Date format yyyymmdd
qob_wide[, date := format(date, "%Y%m%d")]

# Header row
header <- paste(c("date", names(qob_wide)[-1]), collapse = "\t")

# ============================================================
# Write Qobs.txt
# ============================================================

outfile <- file.path(here("QObs", "data_out", "Qobs.txt"))

comment_line <- paste0(
  "!! Qobs generated from NRFA daily mean flow on ",
  format(Sys.Date(), "%Y-%m-%d")
)

con <- file(outfile, open = "wt", encoding = "UTF-8")
writeLines(comment_line, con)
writeLines(header, con)
close(con)

fwrite(
  qob_wide,
  file = outfile,
  sep = "\t",
  quote = FALSE,
  col.names = FALSE,
  append = TRUE,
  na = "-9999"
)

message("✅ Qobs.txt written to:  ", outfile)
flush.console()
