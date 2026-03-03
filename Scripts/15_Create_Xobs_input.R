# ============================================================
# Create XObs.txt input file for CSF-HYPE
# ============================================================

message("🛠️ Creating Xobs file...")
flush.console()

# ============================================================
# Config
# ============================================================

#load packages
if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse,
               here) 
# set paths 
cache_dir <- here("PointSourceData", "Discharges", "api_cache")
xob_dir <- here("XobsData")

# ============================================================
# Required inputs
# ============================================================
required_inputs <- c(
  list.files(cache_dir, pattern = "^HYPE_XOBS_DATA_.*\\.rds$", full.names = TRUE), 
  file.path(xob_dir, "Xobs_sites_SUBID_lut.csv"), 
  here("ForcingData", "_forcing_metadata.rds")
)

missing_inputs <- required_inputs[!file.exists(required_inputs)]
if (length(missing_inputs) > 0) {
  stop(
    "❌ Script 15 cannot run. Missing required input files:\n",
    paste(missing_inputs, collapse = "\n")
  )
} else {
  message("✅ Script 15: All required input files are present.")
}

# ============================================================
# Load inputs
# ============================================================

xob_data <- list.files(
  cache_dir, 
  pattern = "^HYPE_XOBS_DATA_.*\\.rds$",
  full.names = TRUE) %>% 
  read_rds(.)

site_subid_lut <- read_csv(
  file.path(
    xob_dir, "Xobs_sites_SUBID_lut.csv"
    )
)

forcing_meta <- read_rds(here("ForcingData", "_forcing_metadata.rds"))

# ============================================================
# Prepare data and convert to Xob file format
# ============================================================

xob_data_out <- xob_data %>% 
  rename(date = SAMP_SAMPLE_DATE) %>% 
  mutate(
  MEAS_RESULT = case_when(
    MEAS_SIGN == "<" ~ MEAS_RESULT * 0.5, # reduce below MRVs to 50% of value
    .default = MEAS_RESULT # otherwise leave value unchanged
  )
) %>%
  # get SUBID
  inner_join(site_subid_lut, by = c("SMPT_USER_REFERENCE" = "notation")) %>% 
  # if multiple samples taken on same day, take the mean
  group_by(SUBID, date, MEAS_DETERMINAND_CODE) %>% 
  summarise(MEAS_RESULT = mean(MEAS_RESULT, na.rm = TRUE), .groups = "drop") %>% 
  ungroup() %>%
  pivot_wider(names_from = MEAS_DETERMINAND_CODE, values_from = MEAS_RESULT) %>% 
  # apply HYPE substance mapping rules + preferred/fallback logic
  mutate(
    reSS = `0135`,
    reT1 = `2348`,
    reT2 = `0076`,
    reSP = `0180`,
    
    # TP: prefer 0348 else 0192
    reTP = coalesce(`0348`, `0192`),
    
    # IN: prefer 0111+0116 else 0111+0117+0118 if 0116 missing
    reIN = if_else(!is.na(`0116`),
                 `0111` + `0116`,
                 `0111` + `0117` + `0118`)
  ) %>%
  select(date, SUBID, reIN, reSP, reTP, reSS, reT1, reT2) %>%
  arrange(SUBID) %>% 
  # set NAs to HYPE missing value
  mutate(
    across(starts_with("re"), ~ replace_na(.x, -9999))
  )
 

#make full HYPE sequence 
start_date <- as.Date("2000-01-01")
end_date <- forcing_meta$latest_forcing_date

# make full grid (all SUBIDs x all dates)
full_grid <- tidyr::expand_grid(
  SUBID = sort(unique(xob_data_out$SUBID)),
  date  = seq.Date(start_date, end_date, by = "day")
)

# Join to full grid and  fill missing values with -9999 
xob_data_out <- full_grid %>%
  left_join(xob_data_out, by = c("SUBID", "date")) %>%
  mutate(across(starts_with("re"), ~ tidyr::replace_na(.x, -9999))) %>%
  arrange(SUBID, date) %>% 
  # reshape to one column per substance + SUBID 
  pivot_longer(-c(SUBID,date), names_to = "var", values_to = "value") %>%
  mutate(colkey = paste0(var, "__", SUBID)) %>%
  select(date, colkey, value) %>%
  pivot_wider(names_from = colkey, values_from = value)


# build HYPE header rows
data_cols <- setdiff(names(xob_data_out), "date")

# row 2: variable names (date then var repeated for each SUBID column)
var_row <- c("date", sub("__.*$", "", data_cols))

# row 3: SUBIDs (0 for date column, then SUBID for each data column)
subid_row <- c("0", sub("^.*__", "", data_cols))

# format date as yyyymmdd
xob_data_out <- xob_data_out %>%
  mutate(date = format(as.Date(date), "%Y%m%d"))

# ============================================================
# Write to XobsData folder
# ============================================================

outfile <- here("XobsData", "Xobs.txt")

# dynamic comment line ----
comment_line <- paste0(
  "!! Xobs generated from monitoring data on ",
  format(Sys.Date(), "%Y-%m-%d")
)

# ---- write file ----
con <- file(outfile, open = "wt", encoding = "UTF-8")

writeLines(comment_line, con)
writeLines(paste(var_row, collapse = "\t"), con)
writeLines(paste(subid_row, collapse = "\t"), con)

write.table(
  xob_data_out,
  file = con,
  sep = "\t",
  row.names = FALSE,
  col.names = FALSE,
  quote = FALSE,
  na = "-9999"
)

close(con)
