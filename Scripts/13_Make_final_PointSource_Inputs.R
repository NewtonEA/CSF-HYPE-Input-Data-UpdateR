# ============================================================
# Script 13 – Make full, final PSMonthlySeries.txt
# and PointSourceData inputs for CSF-HYPE
#
# Combines downstream PSMonthlySeries outputs into a single, final
# PSMonthlySeries.txt and associated PointSourceData.txt files to be
# used as model inputs to CSF-HYPE.
# ============================================================

message("❯❯❯ Making final Point Source input files")
flush.console()

# ============================================================
# Config
# ============================================================
if (!require(pacman)) install.packages("pacman")
pacman::p_load(
  tidyverse,
  data.table,
  here
)

# ============================================================
# Required inputs
# ============================================================
required_inputs <- c(
  here("PointSourceData", "Discharges", "data_out", "Quantitative_PSMonthlySeries.rds"),
  here("PointSourceData", "Discharges", "data_out", "Qualitative_PSMonthlySeries.rds"),
  here("PointSourceData", "Discharges", "data_out", "Intermittent_PSMonthlySeries.rds"),
  here("PointSourceData", "Discharges", "data_out", "CattleInStreams_Run4_PSMonthlySeries.rds"),
  here("PointSourceData", "Abstractions", "data_out", "Abstractions_PSMonthly_Series.rds"),
  here("PointSourceData", "PointSourceData_master.csv")
)

missing_inputs <- required_inputs[!file.exists(required_inputs)]
if (length(missing_inputs) > 0) {
  stop(
    "❌ Script 13 cannot run. Missing required input files:\n",
    paste(missing_inputs, collapse = "\n")
  )
} else {
  message("✅ Script 13: All required input files are present.")
}

# ============================================================
# Paths
# ============================================================
in_dir  <- here("PointSourceData", "Discharges", "data_out")
out_dir <- here("PointSourceData")

psm_paths <- list(
  Quantitative   = here(in_dir, "Quantitative_PSMonthlySeries.rds"),
  Qualitative    = here(in_dir, "Qualitative_PSMonthlySeries.rds"),
  Intermittent   = here(in_dir, "Intermittent_PSMonthlySeries.rds"),
  CattleStreams  = here(in_dir, "CattleInStreams_Run4_PSMonthlySeries.rds"), 
  Abstractions   = here("PointSourceData", "Abstractions", "data_out",
                        "Abstractions_PSMonthly_Series.rds")
)

psd_master_path <- here("PointSourceData", "PointSourceData_master.csv")

# Final outputs
psmonthly_txt_path <- here(out_dir, "PSMonthlySeries.txt")
psdata_txt_path    <- here(out_dir, "PointSourceData.txt")

# ============================================================
# Helper: read PSMonthlySeries (Format A) and normalise to merge-safe names
#
# Expected input format (as confirmed from your outputs):
#   - Column names are variable names repeated per PSID block (dup names)
#   - Row 1 contains PSIDs (0 in Date col, then PSID repeated per variable)
#   - Row 2 onwards are data (row 2 Date ~ "200001")
#
# Output format:
#   - Column names become unique "var_PSID" (e.g. Flow_6246, INconc_6246)
#   - PSID header row removed
#   - Date retained as character (YYYYMM)
# ============================================================
read_psmonthly_data <- function(path) {
  
  dt <- read_rds(path)
  setDT(dt)
  
  if (nrow(dt) < 3) stop("Too few rows in: ", path)
  if (!"Date" %in% names(dt)) stop("Missing Date column in: ", path)
  
  psid_row <- as.character(unlist(dt[1]))
  vars     <- names(dt)
  
  # Date column PSID marker should be "0"
  if (trimws(psid_row[1]) != "0") {
    stop(
      "Row 1, Date column expected '0' but found '", psid_row[1],
      "' in: ", path
    )
  }
  
  # Build merge-safe names SUBSTANCE_PSID
  new_names <- c("Date", paste0(vars[-1], "_", psid_row[-1]))
  setnames(dt, new_names)
  
  # Drop PSID row
  dt <- dt[-1]
  
  # Ensure Date is character
  dt[, Date := as.character(Date)]
  
  # Convert all non-Date cols to numeric
  num_cols <- setdiff(names(dt), "Date")
  
  dt[, (num_cols) := lapply(.SD, as.numeric), .SDcols = num_cols]
  
  # Should now be unique
  if (anyDuplicated(names(dt)) > 0) {
    dups <- names(dt)[duplicated(names(dt))]
    stop(
      "Still duplicated columns after normalising: ",
      paste(unique(dups), collapse = ", "),
      "\nFile: ", path
    )
  }
  
  dt
}

# ============================================================
# Read & normalise all inputs
# ============================================================
message("📚 Reading + normalising PSMonthlySeries components...")
flush.console()

psm_data_list <- lapply(psm_paths, read_psmonthly_data)

# ============================================================
# Align dates (union of all months) to handle different lengths
# ============================================================

# Ideally inputs should all have the same date range,
# but CattleStreams could potentially be shorter if CCM outputs
# haven't been updated when the pipeline is run
message("🗓️ Aligning date ranges across components...")
flush.console()

all_dates <- unique(do.call(c, lapply(psm_data_list, `[[`, "Date")))
all_dates <- sort(all_dates)

psm_data_list <- lapply(psm_data_list, function(dt) {
  setkey(dt, Date)
  master <- data.table(Date = all_dates)
  setkey(master, Date)
  dt[master]  # KEEP all master dates (left join)
})

# sanity check 

n_expected <- length(all_dates)
bad <- which(vapply(psm_data_list, nrow, integer(1)) != n_expected)

if (length(bad) > 0) {
  stop(
    "Date alignment failed: not all series have the expected number of rows (",
    n_expected, "). Offenders:\n",
    paste(names(psm_data_list)[bad], vapply(psm_data_list[bad], nrow, integer(1)), collapse = "\n")
  )
}


# ============================================================
# Combine into single DT
# ============================================================

message("🧩 Combining PSMonthlySeries components...")
flush.console()

# Remove Date from all but first table
dt_list <- c(
  list(psm_data_list[[1]]),
  lapply(psm_data_list[-1], function(x) x[, !"Date"])
)

merged_dt <- data.table::as.data.table(
  do.call(cbind, dt_list)
)
# ============================================================
# Fill missing dates / gaps with -9999 (explicit missing flag)
# ============================================================
message("🧱 Filling missing values with -9999...")
flush.console()

data_cols <- setdiff(names(merged_dt), "Date")
for (cc in data_cols) {
  idx <- which(is.na(merged_dt[[cc]]))
  if (length(idx)) set(merged_dt, idx, cc, -9999)
}

# ============================================================
# Build and write PSMonthlySeries.txt
# ============================================================

message("🧾 Building HYPE header rows...")
flush.console()

dup <- names(merged_dt)[duplicated(names(merged_dt))]
if (length(dup) > 0) {
  stop(
    "merged_dt has duplicated column names. Examples:\n",
    paste(head(unique(dup), 20), collapse = ", ")
  )
}

cols <- setdiff(names(merged_dt), "Date")

# Header row 1: variable name
header1 <- c("Date", sub("_.*$", "", cols))

# Header row 2: PSID
header2 <- c("0", sub("^.*_", "", cols))

message("💾 Writing final PSMonthlySeries.txt...")
flush.console()

# Write header lines manually
writeLines(
  paste(header1, collapse = "\t"),
  psmonthly_txt_path
)

write(
  paste(header2, collapse = "\t"),
  file = psmonthly_txt_path,
  append = TRUE
)

# Append data table
fwrite(
  merged_dt,
  psmonthly_txt_path,
  sep = "\t",
  quote = FALSE,
  na = "-9999",
  col.names = FALSE,
  append = TRUE
)
# ============================================================
# Build and write PointSourceData.txt (subset master to PSIDs present)
# ============================================================
message("📌 Building final PointSourceData.txt...")
flush.console()

ps_master <- read_csv(psd_master_path, show_col_types = FALSE)

ps_out <- ps_master %>%
  select(PSID, SUBID, ps_type, ps_source)

# Write out (tab separated to match typical HYPE input conventions)
write_tsv(ps_out, psdata_txt_path)

message("🏁 Script 13 complete! Final input files written:")
message(" - ", psmonthly_txt_path)
message(" - ", psdata_txt_path)
flush.console()

# ============================================================
# Declared outputs
# ============================================================
declared_outputs <- c(
  psmonthly_txt_path,
  psdata_txt_path
)
declared_outputs

