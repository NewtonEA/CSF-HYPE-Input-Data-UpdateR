# ============================================================
# EDM -> HYPE Intermittent PSMonthlySeries

# Script written and tested by Tom Newton, Dec 2025

# This script uses annual Event Duration Monitor (EDM) data returned by the water
# companies to generate a timeseries of intermittent point-source discharges 
# in the HYPE PSMonthlySeries format. Only consented intermittent discharges
# with an EDM system are used on the basis that since 2020 all significant
# storm/CSO overflows must install EDM systems and report the data annually, 
# and those classified as low significance (without EDM) are expected to have 
# less than one spill per year. 
#
## NOTE:
# This script always runs when called.
# It rebuilds intermittent monthly series from the current
# canonical EDM collated dataset, regardless of whether that
# dataset was updated in this pipeline run.

# ============================================================

# ============================================================
# Config
# ============================================================

# Load required packages
if (!require(pacman)) install.packages("pacman")
pacman::p_load(here, 
               tidyverse,
               rnrfa,
               progressr,
               data.table)

message("🟢 Starting EDM -> HYPE intermittent timeseries processing...")
flush.console()

 
# Required inputs
required_inputs <- c(
  here("PointSourceData", "Discharges", "data_out", "EDM_returns_collated", "EDM_returns_collated.rds"),
  here("PointSourceData", "Discharges", "data_out", "All_intermittent_consents.csv"),
  here("ForcingData", "Rainfall", "Pobs.txt"),
  here("PointSourceData", "Discharges", "data_out", "WQ_PSData_lut.txt")
)


missing_inputs <- required_inputs[!file.exists(required_inputs)]

if (length(missing_inputs) > 0) {
  stop(
    "Missing required input files for Script 7:\n",
    paste(" -", missing_inputs, collapse = "\n")
  )
}


# Set sub directory
subdir <- here("PointSourceData", "Discharges")

# ============================================================
# Find collated EDM returns file
# ============================================================
f <- list.files(paste0(subdir, "/data_out/EDM_returns_collated/"),
                "EDM_returns_collated",
                full.names = TRUE)

if (length(f) == 0) {
  stop(paste0("❌ No EDM collated file found in: ", subdir, "/data_out. ",
              "Did you forget to run Collate_EDM_returns.R?"))
} else if (length(f) > 1) {
  stop(paste0("❌ Too many EDM collated files in ", subdir,
              "/data_out - There should only be one collated EDM returns file."))
} else {
  message("📥 Found EDM collated file: ", basename(f)) 
  flush.console()
}

# ============================================================
# Load EDM data and basic filtering/standardisation
# ============================================================
EDM_data <- read_rds(f) %>%
  filter(PERMIT_NUMBER != "null") %>%
  mutate(Water_Company_Name = toupper(Water_Company_Name),
         EA_REGION = case_when(
           Water_Company_Name == "WESSEX WATER" ~ "SW",
           Water_Company_Name == "SOUTH WEST WATER" ~ "SW",
           Water_Company_Name == "SOUTHERN WATER" ~ "SO",
           Water_Company_Name == "THAMES WATER" ~ "TH",
           Water_Company_Name == "ANGLIAN WATER" ~ "AN",
           Water_Company_Name == "SEVERN TRENT WATER" ~ "MI",
           Water_Company_Name == "YORKSHIRE WATER" ~ "NE",
           Water_Company_Name == "NORTHUMBRIAN WATER" ~ "NE",
           Water_Company_Name == "UNITED UTILITIES" ~ "NW",
           TRUE ~ "NRW")) %>%
  filter(PerReport >= 90) %>%
  select(-PerReport)

# ============================================================
# Load intermittent consents
# ============================================================
int_consents_file <- list.files(paste0(subdir, "/data_out/"),
                                full.names = TRUE,
                                pattern = "All_intermittent_consents")

if (length(int_consents_file) == 0) {
  stop(paste0("❌ No intermittent consents file found in ", subdir, "/data_out. ",
              "Expected file: All_intermittent_consents*.csv"))
} else {
  int_consents <- read_csv(int_consents_file, show_col_types = FALSE)
  message("📥 Intermittent consents loaded: ", basename(int_consents_file))
  flush.console()
}

# ============================================================
# Join EDM returns to intermittent consents & aggregate permit versions
# ============================================================
message("🔗 Matching EDM returns to intermittent consents ...")

EDM_matched <- inner_join(EDM_data, int_consents,
                          by = c("PERMIT_NUMBER", "EA_REGION"),
                          relationship = "many-to-many") %>%
  group_by(EA_REGION, PERMIT_NUMBER) %>%
  mutate(
    EFFECTIVE_DATE = case_when(
      if_any(Flow_m3s:T1_cfu_ml, ~ n_distinct(.) > 1) ~ EFFECTIVE_DATE,
      TRUE ~ min(EFFECTIVE_DATE, na.rm = TRUE)
    ),
    REVOCATION_DATE = case_when(
      if_any(Flow_m3s:T1_cfu_ml, ~ n_distinct(.) > 1) ~ REVOCATION_DATE,
      TRUE ~ max(REVOCATION_DATE, na.rm = TRUE)
    ),
    # some returns have duration > hours in year; assume 1 spill = 1 day 
    Duration = case_when(Duration > 8760 & Spills < 366 ~ Spills * 24,
                         TRUE ~ Duration)
  ) %>%
  ungroup() %>%
  # remove variables that aren't needed and won't be accurate once made distinct
  select(-c(PERMIT_VERSION, ISSUED_DATE, OUTLET_NUMBER:EFF_SAMPLE_POINT)) %>%
  distinct(across(c(EA_REGION, PERMIT_NUMBER, EFFECTIVE_DATE, REVOCATION_DATE,
                    Duration:Return_yr, Flow_m3s:T1_cfu_ml)), .keep_all = TRUE)

n_matched <- nrow(EDM_matched)
if (n_matched == 0) stop("❌ No EDM records matched to intermittent consents.")
message("✅ EDM matched to consents; matched rows: ", n_matched)
flush.console()

# rm EDM_data to free memory
rm(EDM_data); gc()

# ============================================================
# Write a record of EDMs used
# ============================================================
edm_out_file <- paste0(subdir, "/data_out/EDM_data_used.txt")
write_tsv(EDM_matched, edm_out_file)
message("💾 EDM_data_used written: ", basename(edm_out_file))
flush.console()

# ============================================================
# Summarise EDM to compute avg days active (assume 1 spill = 1 day)
# ============================================================
message("📊 Summarising EDM to estimate average active days per year ...")

EDM_s <- EDM_matched %>%
  group_by(EA_REGION, PERMIT_NUMBER, SUBID) %>%
  mutate(av_days_active = round(mean(Spills, na.rm = TRUE))) %>%
  select(SUBID, EA_REGION, PERMIT_NUMBER, Return_yr, Spills, av_days_active) %>%
  ungroup() %>%
  distinct()

message("✅ EDM summary complete; unique site-years: ", nrow(EDM_s))
flush.console()
# Trim EDM_matched for later joins
EDM_matched <- EDM_matched %>%
  select(EA_REGION, PERMIT_NUMBER, DISCHARGE_NGR, SUBID, EFFECTIVE_DATE,
         REVOCATION_DATE, Flow_m3s:T1_cfu_ml)

# ============================================================
# Load rainfall (Pobs.txt) and filter to 2000 onwards + SUBIDs of interest
# 2025 update - rewritten using data.table to speed up process
# ============================================================
pobs_path <- here("ForcingData", "Rainfall", "Pobs.txt")
if (!file.exists(pobs_path)) {
  stop(paste0("❌ Pobs.txt not found: ", pobs_path))
}

edm_subids <- unique(as.character(EDM_s$SUBID))

pobs <- fread(pobs_path, select = c("DATE", edm_subids))[
  DATE > "1999-12-31"
][
  , Year := format(as.IDate(DATE), "%Y")
][
  , melt(
    .SD,
    id.vars = c("DATE", "Year"),
    variable.name = "SUBID",
    value.name = "P_mm"
  )
][
  , SUBID := as.integer(SUBID)
]

message("✅ Pobs loaded and filtered")
flush.console()

# ============================================================
# Build active dates by year — choose top rainfall days per site-year
# where n days  = n spills
# 2025 update:
# year-by-year approach to avoid vctrs errors 
# & rewritten in data.table for speed 
# ============================================================

message("⚙️ Determining active (spill) dates per site-year using top rainfall days ...")
flush.console()

setDT(EDM_s)

# standard output schema
standard_cols <- c(
  "DATE", "SUBID", "PERMIT_NUMBER", "P_mm",
  "Spills", "av_days_active", "Return_yr"
)

# years to process
years_to_process <- sort(unique(pobs$Year))

message(
  "ℹ️ Years to process for rainfall–EDM matching: ",
  min(years_to_process), "–", max(years_to_process)
)
flush.console()

message("ℹ️ Running year-by-year spill-date extraction...")
flush.console()

active_list <- vector("list", length(years_to_process))
names(active_list) <- years_to_process

for (i in seq_along(years_to_process)) {
  
  y <- years_to_process[i]
  message("  • Processing year ", y, " (", i, "/", length(years_to_process), ")")
  flush.console()
  
  # join rainfall to EDM for this year
  dt_year <- pobs[
    Year == y
  ][
    EDM_s[Return_yr == y],
    on = "SUBID",
    allow.cartesian = TRUE
  ]
  
  # empty year → empty table with correct schema
  if (nrow(dt_year) == 0) {
    active_list[[i]] <- dt_year[0, ..standard_cols]
    next
  }
  
  # use spills for years with spill data, and av_days_active elsewhere
  driver <- if (y %in% EDM_s$Return_yr) "Spills" else "av_days_active"
  
  # select top rainfall days per site/permit
  out <- dt_year[
    order(SUBID, PERMIT_NUMBER, -P_mm),
    head(
      .SD,
      min(
        .N,
        ceiling(max(get(driver), na.rm = TRUE))
      )
    ),
    by = .(SUBID, PERMIT_NUMBER)
  ]
  
  # enforce schema & types
  missing_cols <- setdiff(standard_cols, names(out))
  if (length(missing_cols) > 0) {
    out[, (missing_cols) := NA_real_]
  }
  
  out[, `:=`(
    DATE           = as.IDate(DATE),
    SUBID          = as.numeric(SUBID),
    PERMIT_NUMBER  = as.character(PERMIT_NUMBER),
    P_mm           = as.numeric(P_mm),
    Spills         = as.numeric(Spills),
    av_days_active = as.numeric(av_days_active),
    Return_yr      = as.character(Return_yr)
  )]
  
  setcolorder(out, standard_cols)
  active_list[[i]] <- out[]
} 

rm(dt_year, out)

# bind all years
active_dates <- rbindlist(active_list, use.names = TRUE, fill = TRUE)
rm(active_list)

if (nrow(active_dates) == 0) stop("❌ No active dates were generated from rainfall and EDM data.")
message("✅ Active dates extracted")

# ============================================================
# Load PSID lookup table (previous output) to assign new PSIDs
# ============================================================
psid_lut_file <- list.files(
  paste0(subdir, "/data_out/"),
  pattern = "WQ_PSData_lut",
  full.names = TRUE
)
if (length(psid_lut_file) == 0) {
  stop(paste0("❌ WQ_PSData_lut not found in ", subdir, "/data_out."))
}
psid_lut <- fread(psid_lut_file)
message("📥 WQ_PSData_lut loaded: ", basename(psid_lut_file))
flush.console()

# Assign PSIDs (unique group id per SUBID + PERMIT_NUMBER added to max existing PSID)
active_dates <- active_dates[
  , .(DATE, SUBID, PERMIT_NUMBER )
][
  , PSID := .GRP + max(psid_lut$PSID), by = .(SUBID, PERMIT_NUMBER)
]


message("🔢 PSIDs assigned to intermittent site-permit groups; unique PSIDs: ",
        length(unique(active_dates$PSID)))
flush.console()

# ============================================================
# Get grid references OUTLET_XY from consents and prepare PSID_xy
# ============================================================
message("📍 Parsing outlet grid references ...")
flush.console()

OUTLET_XY <- osg_parse(int_consents$OUTLET_GRID_REF)

PSID_xy <- int_consents %>%
  select(SUBID, PERMIT_NUMBER) %>%
  mutate(X = OUTLET_XY[[1]],
         Y = OUTLET_XY[[2]]) %>%
  distinct(across(c(SUBID, PERMIT_NUMBER)), .keep_all = TRUE)

message("✅ Outlet XY parsed; unique outlets: ", nrow(PSID_xy))
flush.console()

# ============================================================
# Create intermittent PSDATA lookup table and save
# ============================================================
intWQ_PSData_lut <- active_dates %>%
  select(PSID, SUBID, PERMIT_NUMBER) %>%
  distinct() %>%
  mutate(ps_type = 3,
         ts_source = "Intermittent") %>%
  inner_join(PSID_xy, by = c("SUBID", "PERMIT_NUMBER"), relationship = "many-to-many")

int_psdata_file <- paste0(subdir, "/data_out/int_PSData_lut.txt")
write_tsv(intWQ_PSData_lut, int_psdata_file)
message("💾 int_PSData_lut written: ", basename(int_psdata_file))


# ============================================================
# Join active_dates with matched consents for determinands & dates
# ============================================================
active_dates <- inner_join(active_dates, EDM_matched, relationship = "many-to-many")
message("🔗 Active dates joined to matched EDM/consent details; rows: ", nrow(active_dates))
flush.console()

# ============================================================
# Convert to HYPE PSMonthlySeries format 
# To-do : convert to data.table to speed up processing
# ============================================================
message("🔄 Converting active dates to HYPE PSMonthlySeries structure ...")
flush.console()

# temp lookup for later
temp_lut <- active_dates[, .(
  EFFECTIVE_YM  = as.integer(format(min(EFFECTIVE_DATE,  na.rm = TRUE), "%Y%m")),
  REVOCATION_YM = as.integer(format(max(REVOCATION_DATE, na.rm = TRUE), "%Y%m"))
), by = .(SUBID, PERMIT_NUMBER, PSID)]

# create HYPE-formatted monthly series

DT <- copy(active_dates)# copy to avoid modifying original

# create HYPE-formatted monthly series
DT[, date_hype := as.integer(format(DATE, "%Y%m"))]

# remove unneeded columns
cols_remove <- c("DATE", "SUBID", "PERMIT_NUMBER", "EFFECTIVE_DATE", 
                 "REVOCATION_DATE", "EA_REGION", "DISCHARGE_NGR")
DT[, (cols_remove) := NULL]

# melt to long format
DT_long <- melt(
  DT,
  id.vars = c("date_hype", "PSID"),
  variable.name = "DET",
  value.name = "Value"
)

# aggregate to month: sum Flow_m3s, mean for others
DT_agg <- DT_long[, .(
  Value = if (unique(DET) == "Flow_m3s") sum(Value, na.rm = TRUE) else mean(Value, na.rm = TRUE)
), by = .(date_hype, PSID, DET)]

# cast back to wide format
int_PSMonthly <- dcast(DT_agg, date_hype ~ PSID + DET, value.var = "Value")[order(date_hype)]

# Clean up large objects no longer needed
rm(active_dates, EDM_matched, EDM_s, int_consents, DT, DT_long, DT_agg); gc()


# ============================================================
# Join to complete HYPE dates and fill with zeros where inactive
# ============================================================
message("🔗 Joining with full HYPE date sequence and filling inactive months with 0 ...")
flush.console()

# Prepare dates sequence
dates <- unique(
  data.table(date_hype = as.integer(format(unique(pobs$DATE), "%Y%m")))
) ; rm(pobs)

# join int_PSMonthly to dates to make complete timeseries sequence
int_PSMonthly_ts <- int_PSMonthly[dates, on = "date_hype"]

# transform into long format
int_PSMonthly_ts <- melt(
  int_PSMonthly_ts,
  id.vars = "date_hype",
  variable.name = "PSID_DET",
  value.name = "Value"
)

# extract PSID and DET, split only on the first "_" into PSID and DET
int_PSMonthly_ts[, c("PSID_str", "DET") := tstrsplit(as.character(PSID_DET), "_", fixed = TRUE, keep = 1:2)]
int_PSMonthly_ts[, PSID := as.integer(PSID_str)]
int_PSMonthly_ts[, PSID_str := NULL]
int_PSMonthly_ts[, PSID_DET := NULL]

# join temp_lut to get EFFECTIVE/REVOCATION dates
int_PSMonthly_ts <- int_PSMonthly_ts[temp_lut, on = "PSID", nomatch = 0]

# fill Value = 0 outside EFFECTIVE/REVOCATION window or if NA
int_PSMonthly_ts[is.na(Value) | date_hype < EFFECTIVE_YM | date_hype > REVOCATION_YM, Value := 0]

#  rename DETs to HYPE format 
int_PSMonthly_ts[DET != "Flow", DET := paste0(DET, "conc")]

# remove unnecessary columns
int_PSMonthly_ts[, c("EFFECTIVE_YM", "REVOCATION_YM") := NULL]

# order
setorder(int_PSMonthly_ts, date_hype, PSID, DET)

# reshape to wide format
int_PSMonthly_ts <- dcast(
  int_PSMonthly_ts,
  date_hype ~ PSID + DET,
  value.var = "Value",
  fill = 0
)

# Reformat headers to two-line HYPE PSMonthlySeries format
# The first row contains PSIDs; column names are determinands (DETs)

# rename date_hype
setnames(int_PSMonthly_ts, "date_hype", "0_Date")

# Extract column names
col_names <- names(int_PSMonthly_ts)

# Extract PSID (before first underscore) and DET (after)
psid <- sub("_.*$", "", col_names)
det  <- sub("^\\d+_", "", col_names)

# Replace column names with DETs
setnames(int_PSMonthly_ts, det)

#create PSID row
psid_row <- as.data.table(as.list(psid))
names(psid_row) <- names(int_PSMonthly_ts)

int_PSMonthly_ts <- rbind(
  as.data.frame(psid_row, check.names = FALSE),
  as.data.frame(int_PSMonthly_ts, check.names = FALSE)
)

message("✅ HYPE Intermittent PSMonthlySeries has been created!")
flush.console()

# ============================================================
# Save final Intermittent PSMonthlySeries to data_out
# ============================================================
out_file_rds <- paste0(subdir, "/data_out/Intermittent_PSMonthlySeries.rds")
write_rds(int_PSMonthly_ts, out_file_rds)
message("💾 Intermittent PSMonthlySeries written: ", basename(out_file_rds))


# ============================================================
# Declare outputs 
# ============================================================

declared_outputs <- c(
  here("PointSourceData", "Discharges", "data_out", "EDM_data_used.txt"),
  here("PointSourceData", "Discharges", "data_out", "int_PSData_lut.txt"),
  here("PointSourceData", "Discharges", "data_out", "Intermittent_PSMonthlySeries.rds")
)


