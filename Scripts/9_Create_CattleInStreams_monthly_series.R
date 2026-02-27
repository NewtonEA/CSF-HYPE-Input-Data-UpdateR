# ============================================================
# CSF-HYPE CattleInStreams (CIS) PSMonthlySeries updater
#  Revised and Tested December 2025 - Tom Newton
# 
# ============================================================
#
## Notes:
#  - only includes FIO (T1), PP and ON data
#  - Requires latest CCM CIS outputs from Chris Burgess in: 
# /PointSourceData/Discharges/data_in/CattleInStreams
# ============================================================
message("🐄 Starting Cattle-In-Streams PSMonthlySeries processing...")
flush.console()

# ============================================================
# Load required packages
# ============================================================
if (!require(pacman)) install.packages("pacman")
pacman::p_load(
  tidyverse,
  here,
  data.table
)

# ============================================================
# Config
# ============================================================
subdir  <- here("PointSourceData", "Discharges")
in_dir  <- here(subdir, "data_in", "CattleInStreams")
out_dir <- here(subdir, "data_out")

# ============================================================
# Required inputs
# ============================================================
psid_file <- list.files(in_dir, "PointSourceData_direct_", full.names = TRUE)

required_inputs <- c(
  file.path(in_dir, paste0("Direct_T1NP_Conc_run", 1:4, ".txt")),
  psid_file
)

if (length(psid_file) == 0) {
  stop("❌ No PointSourceData_direct_ file found in ", in_dir)
}

missing_inputs <- required_inputs[!file.exists(required_inputs)]

if (length(missing_inputs) > 0) {
  stop(
    "❌ Missing required CIS input files:\n",
    paste(" -", missing_inputs, collapse = "\n")
  )
}

# ============================================================
# Load CIS PointSourceData lookup
# ============================================================
psid_lut <- read_tsv(psid_file) %>%
  select(-c(name, det, WIMS_if_quantPS, WISKI_if_quantPS)) %>%
  rename(ts_source = DataSource)

message("📥 CIS PointSourceData lookup loaded")
flush.console()

# ============================================================
# function to build HYPE PSMonthlySeries for each run
# ============================================================
build_cis_psmonthly <- function(run_no) {
  
  stopifnot(run_no %in% 1:4)
  
  message("⚙️ Processing CIS run ", run_no, "...")
  flush.console()
  
  # Read CIS file
  dt <- fread(
    file.path(in_dir, paste0("Direct_T1NP_Conc_run", run_no, ".txt"))
  )
  
  # Mutate / derive variables
  dt[, Flow   := m3s] #supplied in m3s
  dt[, PPconc := get(paste0("P_conc", run_no)) * 0.001] # convert from ugL to mgL
  dt[, ONconc := get(paste0("N_conc", run_no)) * 0.001] # convert from ugL to mgL
  dt[, T1conc := get(paste0("FIO_conc", run_no))] # supplied in cfu/ml
  dt[, DATE   := format(as.IDate(DATE), "%Y%m")]
  
  # Keep required columns only
  dt <- dt[, .(DATE, PSID, Flow, PPconc, ONconc, T1conc)]
  
  # Long → wide reshape
  long_dt <- melt(
    dt,
    id.vars = c("DATE", "PSID"),
    variable.name = "DET",
    value.name = "Value"
  )
  
  wide_dt <- dcast(
    long_dt,
    DATE ~ PSID + DET,
    value.var = "Value"
  )
  
  # Ordering & NA handling
  setorder(wide_dt, DATE)
  setnames(wide_dt, "DATE", "0_Date")
  
  wide_dt[is.na(wide_dt)] <- 0
  
  # Convert everything to character (to allow rbind headers)
  wide_dt[, (names(wide_dt)) := lapply(.SD, as.character)]
  
  # Build two-line HYPE headers
  cn <- names(wide_dt)
  
  headers1 <- sub("^\\d+_|_.*", "", cn)   # PSID stripped
  headers1 <- gsub("m3_s", "Flow", headers1)
  
  headers2 <- ifelse(
    grepl("^\\d+_", cn),
    sub("_(.*)$", "", cn),
    cn
  )
  
  setnames(wide_dt, headers1)
  
  header_dt <- as.data.table(as.list(headers2))
  setnames(header_dt, headers1)
  
  # Final HYPE-ready output
  output_dt <- rbindlist(
    list(header_dt, wide_dt),
    use.names = TRUE,
    fill = TRUE
  )
  
  return(output_dt)
}

# ============================================================
# Generate PSMonthlySeries for runs 1–4
# ============================================================
for (run in 1:4) {
  out_rds <- file.path(
    out_dir,
    paste0("CattleInStreams_Run", run, "_PSMonthlySeries.rds")
  )
  
  saveRDS(build_cis_psmonthly(run), out_rds)
  
  message("💾 CIS Run ", run, " PSMonthlySeries written: ", basename(out_rds))
  flush.console()
}

# ============================================================
# Save CIS PointSourceData lookup
# ============================================================
psdata_out <- file.path(out_dir, "CattleInStreams_PointSourceData.txt")

write_tsv(psid_lut, psdata_out)

message("💾 CIS PointSourceData lookup written: ", basename(psdata_out))
flush.console()

message("✅ Cattle-In-Streams processing complete!")
flush.console()

# ============================================================
# Declared outputs 
# ============================================================
declared_outputs <- c(
  file.path(out_dir, "CattleInStreams_Run1_PSMonthlySeries.rds"),
  file.path(out_dir, "CattleInStreams_Run2_PSMonthlySeries.rds"),
  file.path(out_dir, "CattleInStreams_Run3_PSMonthlySeries.rds"),
  file.path(out_dir, "CattleInStreams_Run4_PSMonthlySeries.rds"),
  file.path(out_dir, "CattleInStreams_PointSourceData.txt")
)

declared_outputs






