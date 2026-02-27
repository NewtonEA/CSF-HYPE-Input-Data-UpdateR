# ============================================================
# CSF-HYPE PSData master lookup maker 
# This scipt simply combines upstream PSData lookup outputs
# and builds the master PSData lookup 
# ============================================================

message("🧩 Building HYPE master PSData lookup...")
flush.console()

# ============================================================
# load required packages
# ============================================================
if (!require(pacman)) install.packages("pacman")
pacman::p_load(
  here,
  tidyverse
)

# ============================================================
# Required inputs
# ============================================================

# discharge PSData (intermittents and constant discharges)
ps_data_in <- list.files(
  here("PointSourceData", "Discharges", "data_out"),
  "PSData_lut",
  full.names = TRUE
)

if (length(ps_data_in) == 0) {
  stop(
    "❌ No discharge PSData lookup files found in:\n",
    here("PointSourceData", "Discharges", "data_out"), "\n",
    "Ensure upstream discharge scripts have completed successfully."
  )
}

# Abstractions PSData
abs_psdata_file <- list.files(
  here("PointSourceData", "Abstractions", "data_out"),
  "PointSourceData",
  full.names = TRUE
)

if (length(abs_psdata_file) == 0) {
  stop(
    "❌ No Abstractions PSData lookup found in:\n",
    here("PointSourceData", "Abstractions", "data_out"), "\n",
    "Ensure the Abstractions PSMonthlySeries script has run successfully."
  )
}

# WISKI–WIMS LUT
wiski_lut_file <- here(
  "PointSourceData", "Discharges", "data_in", "WISKI_to_WIMS_LUT.txt"
)

if (!file.exists(wiski_lut_file)) {
  stop("❌ Missing WISKI_to_WIMS_LUT.txt in Discharges/data_in/")
}

# ECO omissions list
eco_omissions_file <- here(
  "PointSourceData", "Discharges", "Permits_to_omit_eco.csv"
)

if (!file.exists(eco_omissions_file)) {
  stop("❌ Missing Permits_to_omit_eco.csv in Discharges/")
}

# Cattle in Streams PSData (required at this stage)
cis_psdata_file <- here(
  "PointSourceData", "Discharges", "data_out",
  "CattleInStreams_PointSourceData.txt"
)

if (!file.exists(cis_psdata_file)) {
  stop(
    "❌ Missing Cattle-In-Streams PSData lookup:\n",
    cis_psdata_file, "\n",
    "Ensure Script 9 has completed successfully."
  )
}

message("📥 All required inputs validated")
flush.console()

# ============================================================
# Function to read and combine all files while keeping all unique columns
# ============================================================
read_and_combine_all_columns <- function(file_paths) {
  # Read all files into a list, specifying that all columns should be read as characters
  data_list <- lapply(
    file_paths,
    function(file) read_tsv(file, col_types = cols(.default = "c"))
  )
  
  # Combine all data frames by row, keeping all columns and filling with NAs where necessary
  combined_data <- bind_rows(data_list)
  
  return(combined_data)
}

# ============================================================
# Discharge PSData
# ============================================================

ps_data_out <- read_and_combine_all_columns(ps_data_in) %>% 
  select(-geometry) %>% 
  arrange(as.numeric(PSID)) %>% 
  mutate(
    ps_source = "3",
    # Remove anything that's not a number, not the word "Flow", and not a comma
    quantitative_dets = str_replace_all(
      quantitative_dets,
      "(?<!Flow)([^0-9,]|(?!Flow)[a-zA-Z].*)",
      ""
    ),  
    quantitative_dets = str_replace_all(quantitative_dets, "0117|0116|0118|0111", "IN, ON"),
    quantitative_dets = str_replace_all(quantitative_dets, "0180|0348|0192", "SP, TP, PP,"),                 
    quantitative_dets = str_replace_all(quantitative_dets, "0135", " SS,"),                                   
    quantitative_dets = str_replace_all(quantitative_dets, "2348", " T1,"),
    quantitative_dets = str_replace_all(quantitative_dets, "_3", " Flow"),
    quantitative_dets = str_remove_all(quantitative_dets, "0076"),
    PERMIT_NUMBER = str_replace_all(PERMIT_NUMBER, '^c\\(|\\)$|"', "")
  ) %>%
  mutate(
    quantitative_dets = sapply(quantitative_dets, function(x) {
      # Split the string by comma, trim spaces, and remove duplicates
      unique_parts <- unique(trimws(unlist(strsplit(x, ","))))
      # Reassemble the string with only unique values and add commas
      paste(unique_parts[unique_parts != ""], collapse = ", ")
    })
  )

# ============================================================
# Abstractions PSData
# ============================================================

ps_data_in2 <- read_tsv(
  abs_psdata_file,
  col_types = cols(.default = col_character())
)

wiski_lut <- read_tsv(wiski_lut_file) %>% 
  rename(EFF_SAMPLE_POINT = SMPT_USER_REFERENCE)

eco_omissions <- read_csv(eco_omissions_file)

ps_data_out2 <- bind_rows(ps_data_out, ps_data_in2) %>% 
  left_join(wiski_lut) %>%
  left_join(eco_omissions) %>%
  mutate(
    ps_source = case_when(is.na(ps_source) ~ "3", .default = ps_source), 
    Include_in_eco_runs = coalesce(Include_in_eco_runs, "YES")
  )

# ============================================================
# Cattle in Streams 
# ============================================================

cis_in <- read_tsv(cis_psdata_file) %>% 
  mutate(
    ps_source = "3", 
    across(everything(), as.character)
  )

ps_data_out_final <- bind_rows(ps_data_out2, cis_in)

# ============================================================
# Write master PSData lookup
# ============================================================

output_file <- here("PointSourceData", "PointSourceData_master.csv")

write_csv(
  ps_data_out_final,
  output_file,
  append = FALSE)

message("💾 Master PSData lookup written: ", basename(output_file))
flush.console()

message("✅ PSData master lookup complete!")
flush.console()

# ============================================================
# Declare outputs
# ============================================================

declared_outputs <- c(output_file)

declared_outputs



