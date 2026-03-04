# ============================================================
# Create Xobs.txt input file for CSF-HYPE (data.table version)
# ============================================================

message("🛠️ Creating Xobs file...")
flush.console()

# ============================================================
# Config
# ============================================================
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(data.table,
               here)

cache_dir <- here::here("PointSourceData", "Discharges", "api_cache")
xob_dir   <- here::here("XobsData")

# ============================================================
# Required inputs
# ============================================================
xob_file <- list.files(
  cache_dir,
  pattern = "^HYPE_XOBS_DATA_.*\\.rds$",
  full.names = TRUE)

required_inputs <- c(
  xob_file,
  file.path(xob_dir, "Xobs_sites_SUBID_lut.csv"),
  here::here("ForcingData", "_forcing_metadata.rds")
)

missing_inputs <- required_inputs[!file.exists(required_inputs)]
if (length(missing_inputs) > 0) {
  stop(
    "❌ Script 15 cannot run. Missing required input files:\n",
    paste(missing_inputs, collapse = "\n")
  )
} else {
  message("✅ Script 15: All required input files are present.")
  flush.console()
}

# ============================================================
# Load inputs 
# ============================================================

xob_dt <- setDT(readRDS(xob_file))

site_lut <- fread(file.path(xob_dir, "Xobs_sites_SUBID_lut.csv"))
forcing_meta <- readRDS(here::here("ForcingData", "_forcing_metadata.rds"))

# ============================================================
# Prepare data: censor handling, join SUBID, daily mean
# ============================================================

# Ensure date is Date/IDate
# (Assumes SAMP_SAMPLE_DATE is Date already -  if it is character, as.IDate will parse)
xob_dt[, date := as.IDate(SAMP_SAMPLE_DATE)]

# Censor rule: if < MRV then 50% of value else leave as is
xob_dt[MEAS_SIGN == "<", MEAS_RESULT := MEAS_RESULT * 0.5]

# Join SUBID (notation in lut matches SMPT_USER_REFERENCE)
setkey(site_lut, notation)
setkey(xob_dt, SMPT_USER_REFERENCE)
xob_dt <- site_lut[xob_dt, on = .(notation = SMPT_USER_REFERENCE)]

# Daily mean per SUBID-date-determinand
xob_dt <- xob_dt[
  ,
  .(MEAS_RESULT = mean(MEAS_RESULT, na.rm = TRUE)),
  by = .(SUBID, date, MEAS_DETERMINAND_CODE)
]

# ============================================================
# Pivot determinands wide (det codes become columns)
# ============================================================
xob_dt <- dcast(
  xob_daily,
  SUBID + date ~ MEAS_DETERMINAND_CODE,
  value.var = "MEAS_RESULT"
)

# ============================================================
# Apply HYPE substance mapping + unit conversions
# ============================================================
# Notes:
# - reSS: mg/L already (no conversion)
# - reT1: convert cfu/100ml -> cfu/ml => divide by 100
# - reT2: temperature already
# - reSP/reTP/reIN: mg/L -> ug/L => * 1000


xob_dt[
  ,
  `:=`(
    reSS = get("0135"),
    reT1 = get("2348") / 100,
    reT2 = get("0076"),
    
    reSP = get("0180") * 1000,
    reTP = fifelse(!is.na(get("0348")), get("0348"), get("0192")) * 1000,
    
    # IN: prefer 0111+0116; else 0111+0117+0118 when 0116 is NA
    reIN = fifelse(
      !is.na(get("0116")),
      (get("0111") + get("0116")) * 1000,
      (get("0111") + get("0117") + get("0118")) * 1000
    )
  )
]

# Keep only required output columns
xob_dt <- xob_dt[, .(SUBID, date, reIN, reSP, reTP, reSS, reT1, reT2)]

# Replace NA in substances with -9999
substance_cols <- c("reIN","reSP","reTP","reSS","reT1","reT2")
for (cc in substance_cols) set(xob_dt, which(is.na(xob_dt[[cc]])), cc, -9999)

# ============================================================
# Make full daily grid: all SUBIDs x all dates
# ============================================================
start_date <- as.IDate("2000-01-01")
end_date   <- as.IDate(forcing_meta$latest_forcing_date)

subids <- sort(unique(xob_dt$SUBID))
dates  <- seq(start_date, end_date, by = "day")

full_grid <- CJ(SUBID = subids, date = dates, unique = TRUE)

setkey(xob_dt, SUBID, date)
setkey(full_grid, SUBID, date)

# Left join xob_subs onto full grid
xob_full <- xob_dt[full_grid]  # i.e. full_grid as i, keep all rows

# Fill missing (days with no obs) with -9999
for (cc in substance_cols) {
  set(xob_full, which(is.na(xob_full[[cc]])), cc, -9999)
}

# ============================================================
# Reshape to Xobs wide: one column per (substance, SUBID)
# ============================================================

substance_order <- c("reIN","reSP","reTP","reSS","reT1","reT2")

xob_full <- melt(
  xob_full,
  id.vars = c("date", "SUBID"),
  measure.vars = substance_cols,
  variable.name = "var",
  value.name = "value"
)

# enforce substance order
xob_full[, var := factor(var, levels = substance_order)]

xob_full[, colkey := paste0(SUBID, "__", var)]

xob_full <- dcast(
  xob_full,
  date ~ colkey,
  value.var = "value"
)

# Header rows for HYPE
data_cols <- setdiff(names(xob_full), "date")
var_row   <- c("date", sub("^.*__", "", data_cols))
subid_row <- c("0",    sub("__.*$", "", data_cols))

# Date format yyyymmdd
xob_full[, date := format(date, "%Y%m%d")]

# ============================================================
# Write Xobs.txt in HYPE format
# ============================================================
outfile <- here::here("XobsData", "Xobs.txt")

comment_line <- paste0(
  "!! Xobs generated from monitoring data on ",
  format(Sys.Date(), "%Y-%m-%d")
)

# Write comment + 2 header lines
con <- file(outfile, open = "wt", encoding = "UTF-8")
writeLines(comment_line, con)
writeLines(paste(var_row, collapse = "\t"), con)
writeLines(paste(subid_row, collapse = "\t"), con)
close(con)

# Append data 
fwrite(
  xob_full,
  file = outfile,
  sep = "\t",
  quote = FALSE,
  col.names = FALSE,
  append = TRUE,
  na = "-9999"
)

message("✅ Xobs.txt written to:  ", outfile)
flush.console()