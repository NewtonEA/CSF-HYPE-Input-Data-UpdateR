# ============================================================
# CSF-HYPE Quantitative monthly series processing script
# 
# 
#
# ============================================================

message("❯❯❯❯ Starting quantitive monthly series processing...")
flush.console()

# ============================================================
# Load required packages
# ============================================================
if (!require(pacman)) install.packages("pacman")
pacman::p_load(here, 
               tidyverse,
               data.table,
               changepoint
               )

# ============================================================
# Required inputs
# ============================================================

required_inputs <- c(
  # Master PSData lookup
  here("PointSourceData", "PointSourceData_master.csv"),
  
  # Quantitative point source lookup (from Script 5)
  here(
    "PointSourceData", "Discharges", "data_out",
    "HYPE_point_sources_for_quantitative_analysis.txt"
  ),
  
  # Effluent monitoring data cache (at least one file required)
  here("PointSourceData", "Discharges", "api_cache"),
  
  # Quantitative monthly flows (from Script 11)
  here(
    "PointSourceData", "Discharges", "data_out",
    "Quantitative_flows_monthly.rds"
  ),
  
  # Forcing metadata (defines end date of full monthly sequence)
  here("ForcingData", "_forcing_metadata.rds")
)

missing_inputs <- required_inputs[
  !file.exists(required_inputs) & !dir.exists(required_inputs)
]

if (length(missing_inputs) > 0) {
  stop(
    "❌ Missing required inputs for Script 12:\n",
    paste(" -", missing_inputs, collapse = "\n")
  )
}

# Check that at least one effluent monitoring file exists
effluent_files <- list.files(
  here("PointSourceData", "Discharges", "api_cache"),
  pattern = "^HYPE_EFFLUENT_MONITORING_DATA.*\\.rds$",
  full.names = TRUE
)

if (length(effluent_files) == 0) {
  stop(
    "❌ No effluent monitoring data found in api_cache.\n",
    "Ensure upstream effluent API extraction has completed successfully."
  )
}

# ============================================================
# Load & prepare inputs
# ============================================================

# PointSourceData master lookup 
psd_lut_file <- here("PointSourceData", "PointSourceData_master.csv")

psd_lut <- read_csv(psd_lut_file) %>% 
  filter(ts_source == "Numeric")

# load PSIDs for quantitative analysis 
# determined in script5. 
# Defaults added for non quant flows, non quantified dets have 0 concs 

quant_points <- read_tsv(
  here("PointSourceData", "Discharges", "data_out", "HYPE_point_sources_for_quantitative_analysis.txt")) %>% 
  select(PSID, EFF_SAMPLE_POINT, Flow_m3s:quant_for, EFFECTIVE_DATE, REVOCATION_DATE)

#make into tidy lookup
quant_lut <- quant_points %>%
  select(PSID, EFF_SAMPLE_POINT, quant_for) %>%
  separate_rows(quant_for, sep = ",\\s*") %>%
  rename(MEAS_DETERMINAND_CODE = quant_for) %>% 
  filter(MEAS_DETERMINAND_CODE != "0076") # temperature not used (currently)


# Load effluent monitoring data 
eff_file <- list.files(
  here("PointSourceData", "Discharges", "api_cache"),
  pattern = "^HYPE_EFFLUENT_MONITORING_DATA.*\\.rds$", 
  full.names = TRUE
)

eff_data <- read_rds(eff_file) %>% 
  # join to quant_lut, effectively filtering for required sites and dets
  inner_join(
    quant_lut,
    by = c(
      "SMPT_USER_REFERENCE" = "EFF_SAMPLE_POINT",
      "MEAS_DETERMINAND_CODE"
    ), relationship = "many-to-many"
  ) %>% 
  # if measurements are below limit of detection half the value
  mutate(
    MEAS_RESULT = case_when(
      MEAS_SIGN == "<" ~ MEAS_RESULT / 2,
      .default = MEAS_RESULT
      )
    )

# check for completeness (it should be OK, but good to check)

expected_sites <- quant_lut %>%
  #remove sites where only flow is quant otherwise these will appear missing
  filter(MEAS_DETERMINAND_CODE != "Flow_m3s") %>% 
  distinct(EFF_SAMPLE_POINT)

joined_sites <- eff_data %>%
  distinct(SMPT_USER_REFERENCE)

n_expected <- nrow(expected_sites)
n_joined   <- nrow(joined_sites)

missing_sites <- if (n_expected != n_joined) {
  
  warning(
    sprintf(
      "⚠️ Effluent join incomplete: %s of %s expected sites present (%s missing).",
      n_joined, n_expected, n_expected - n_joined
    ),
    call. = FALSE
  )
  
  ms <- quant_lut %>%
    distinct(EFF_SAMPLE_POINT) %>%
    anti_join(
      eff_data %>% distinct(SMPT_USER_REFERENCE),
      by = c("EFF_SAMPLE_POINT" = "SMPT_USER_REFERENCE")
    )
  # writes missing sites if there are any
  write_csv(
    ms,
    here(
      "PointSourceData", "Discharges", "data_out",
      "missing_effluent_sites_after_quant_join.csv"
    )
  )
  
  ms
  
} else {
  
  message(
    sprintf(
      " 🤝 Effluent join check passed: all %s expected sites are present.",
      n_expected
    )
  )
  NULL
}



# ============================================================
# Calculate monthly means with significant changepoints
# ============================================================

setDT(eff_data) # convert to DT

# aggregate to monthly means before any change detection
eff_monthly <- eff_data[
  ,
  .(
    Value = mean(MEAS_RESULT, na.rm = TRUE)
  ),
  by = .(
    PSID,
    SMPT_USER_REFERENCE,
    MEAS_DETERMINAND_CODE,
    year = year(SAMP_SAMPLE_DATE),
    month = month(SAMP_SAMPLE_DATE)
  )
][
  ,
  HYPEDate := as.Date(paste(year, month, "01", sep = "-"))
][
  ,
  .(PSID, SMPT_USER_REFERENCE, MEAS_DETERMINAND_CODE, HYPEDate, Value)
]

#ensure chronological order
setorder(eff_monthly, PSID, MEAS_DETERMINAND_CODE, HYPEDate)

# detect significant changepoints per site and det

# function to detect most significant changepoints (max 3)

detect_changepoints <- function(x, dates,
                                # Change in effluent concentration must persist
                                # for at least 2 years to be considered meaningful
                                minseglen = 24,
                                # Conservative BIC-style penalty to avoid overfitting
                                penalty_mult = 2, 
                                # allow only up to 3 most significant changes
                                max_changes = 3) {
  # c.f. Killick et al. (2012) recommend tuning penalties upward for noisy data
  # PELT (Pruned Exact Linear Time) Finds multiple change points by pruning 
  # candidate solutions to reduce computational complexity - requires penalty: 
  # Ref: https://doi.org/10.1080/01621459.2012.737745
  # Penalty controls how sensitive the changepoinbt algorithin is to detecting 
  # changes. A manual BIC-style penalty scaled to be more conservative, similar in intent to MBIC
  # approach designed for multiple change points and long sequences that
  # avoids overfitting, ref: https://doi.org/10.1111/j.1541-0420.2006.00670.x 

  # Remove NAs (keep x and dates aligned)
  ok <- !is.na(x)
  x <- x[ok]
  dates <- dates[ok]
  n <- length(x)
  
  # If series is too short to support changepoint detection,
  # return overall mean as single regime (1 segment)
  if (n < minseglen * 2) {
    return(list(
      n_changes = 0L,
      cp_dates = as.Date(character(0)),
      segment_means = mean(x, na.rm = TRUE)
    ))
  }
  
  # Manual BIC-style penalty (scaled to be conservative)
  pen <- penalty_mult * log(n)
  cp <- try(changepoint::cpt.mean(x, method = "PELT",
                                  penalty = "Manual",
                                  pen.value = pen,
                                  minseglen = minseglen),
            silent = TRUE)
  
  # If changepoint detection fails, treat as single regime
  if (inherits(cp, "try-error")) {
    return(list(
      n_changes = 0L,
      cp_dates = as.Date(character(0)),
      segment_means = mean(x, na.rm = TRUE)
    ))
  }
  
  # No changepoints detected ==> single regime
  cpts_all <- cpts(cp)
  if (length(cpts_all) == 0) {
    return(list(
      n_changes = 0L,
      cp_dates = as.Date(character(0)),
      segment_means = mean(x, na.rm = TRUE)
    ))
  }
  
  # Compute effect sizes for each change point
  effect_sizes <- sapply(cpts_all, function(cp_idx) {
    mean_after <- mean(x[(cp_idx + 1):n], na.rm = TRUE)
    mean_before <- mean(x[1:cp_idx], na.rm = TRUE)
    abs(mean_after - mean_before)
  })
  
  # Keep the max "max_changes" change points by magnitude
  if (length(cpts_all) > max_changes) {
    top_idx <- order(effect_sizes, decreasing = TRUE)[1:max_changes]
    cpts_kept <- sort(cpts_all[top_idx]) # sort back to chronological order
  } else {
    cpts_kept <- cpts_all
  }
  
  # Compute segment means
  idx <- c(0, cpts_kept, n)
  segment_means <- sapply(seq_len(length(idx) - 1), function(i) {
    mean(x[(idx[i] + 1):idx[i + 1]], na.rm = TRUE)
  })
  
  list(
    n_changes = length(cpts_kept),
    cp_dates = dates[cpts_kept],
    segment_means = segment_means
  )
}


# apply changepoint detection function to effluent data 
message("🕵️️ Detecting significant changepoints in effluent monitoring data...")
flush.console()

cp_results <- eff_monthly[
  ,
  {
    res <- detect_changepoints(Value, HYPEDate)
    .(
      n_changes = as.integer(res$n_changes),
      cp_dates  = list(as.Date(res$cp_dates)),
      segment_means = list(as.numeric(res$segment_means))
    )
  },
  by = .(PSID, SMPT_USER_REFERENCE, MEAS_DETERMINAND_CODE)
]

# ============================================================
#  Create full sequence timeseries of monthly means 
# ============================================================

message("🛠️ Creating full monthly sequence for each site-det...")
flush.console()

# Set the fixed date range (end date determined by model forcing data end date)

forcing_meta <- read_rds(here("ForcingData", "_forcing_metadata.rds"))
start_date <- as.Date("2000-01-01")
end_date   <- forcing_meta$latest_forcing_date
all_months <- seq(start_date, end_date, by = "month")

# Create the full monthly sequence for all site–determinands

site_det <- unique(eff_monthly[, .(PSID, SMPT_USER_REFERENCE, MEAS_DETERMINAND_CODE)])

full_seq <- site_det[, .(HYPEDate = all_months), by = .(PSID, SMPT_USER_REFERENCE, MEAS_DETERMINAND_CODE)]

# Map the changepoints and segment means onto the complete monthly sequence
full_seq <- full_seq[
  cp_results,
  on = .(PSID, SMPT_USER_REFERENCE, MEAS_DETERMINAND_CODE),
  nomatch = 0L
][
  ,
  Value := {
    # Flatten and sort changepoints
    cp_sorted <- sort(unique(unlist(cp_dates)))
    
    # If no changepoints, ensure at least one segment
    if (length(cp_sorted) == 0) cp_sorted <- as.Date(character(0))
    
    # Create segment boundaries (strictly increasing)
    segment_starts <- c(start_date - 1, cp_sorted)
    segment_ends   <- c(cp_sorted, end_date + 31)
    
    # Flatten segment means
    seg_vec <- unlist(segment_means)
    
    # In case seg_vec length mismatches cp_sorted, pad with last value
    if (length(seg_vec) < length(segment_starts)) {
      seg_vec <- c(seg_vec, rep(seg_vec[length(seg_vec)], length(segment_starts) - length(seg_vec)))
    }
    
    # Use cut() to assign each month to a segment
    seg_idx <- cut(
      HYPEDate,
      breaks = unique(c(segment_starts, segment_ends[length(segment_ends)])), # unique ensures no duplicates
      labels = FALSE,
      right = TRUE
    )
    
    # Map to segment mean
    seg_vec[seg_idx]
  },
  by = .(PSID, SMPT_USER_REFERENCE, MEAS_DETERMINAND_CODE)
]

# ============================================================
#  Convert to HYPE substances with rules
# ============================================================

# Effluent monitoring determinands need to be converted to HYPE substances. 
# where:
# HYPE substance IN =  MEAS_DETERMINAND_CODE 0111 + 0116. 
# If 0116 is not available, use IN =  0117 + 0118 + 0111. 
# ON = IN/9
# SP =  MEAS_DETERMINAND_CODE 0180 
# TP = MEAS_DETERMINAND_CODE 0348 or 0192.  Where 0348 is preferred,
# but if 0348 is not available for the site then use 0192 (old method).
# If only one of SP or TP can be established (data permitting),
# then use the ratio between them to calculate the other: 
# where PP = SP/9 and TP = SP + PP).
# If both have data, check that TP > SP. If yes, then keep both. If 
# not, then use TP to calculate SP as a ratio of TP, as above.
# PP = TP - SP (only calculated after TP and SP have been established)
# SS =  MEAS_DETERMINAND_CODE 0135
# All units are already in mg/l so don't need converting. 

message("🔀 Converting to HYPE substances...")
flush.console()


# widen data
hype_ts <- dcast(
  full_seq,
  PSID + HYPEDate ~ MEAS_DETERMINAND_CODE,
  value.var = "Value"
)

# apply rules and convet to HYPE substances
hype_ts <- hype_ts[
  ,
  `:=`(
    ## IN
    IN = fifelse(
      !is.na(`0111`) & !is.na(`0116`),
      `0111` + `0116`,
      `0111` + fifelse(is.na(`0117`), 0, `0117`) +
        fifelse(is.na(`0118`), 0, `0118`)
    ),
    
    ## Preliminary SP 
    SP = `0180`,
    
    ## Preliminary TP (0348 preferred)
    TP = fifelse(!is.na(`0348`), `0348`, `0192`),
    
    ## SS 
    SS = `0135`
  )
]

# Enforce SP–TP consistency & derive PP
hype_ts[
  ,
  `:=`(
    # Final TP 
    TP = fifelse(
      # TP missing but SP exists = derive TP
      is.na(TP) & !is.na(SP),
      SP * 10/9,
      TP
    ),
    
    # Final SP 
    SP = fifelse(
      # SP missing but TP exists = derive SP
      is.na(SP) & !is.na(TP),
      TP / 10,
      fifelse(
        # Both exist but inconsistent = trust TP
        !is.na(SP) & !is.na(TP) & TP < SP,
        TP / 10,
        SP
      )
    )
  )
]

# PP (after SP & TP fixed)
hype_ts[
  ,
  PP := TP - SP
]

# derive ON 

hype_ts[
  ,
  ON := IN / 9
]

# QA 
# No negative concentrations
stopifnot(all(hype_ts[, .(IN, ON, SP, PP, TP, SS)] >= 0, na.rm = TRUE))

# TP must be >= SP
stopifnot(all(hype_ts$TP >= hype_ts$SP, na.rm = TRUE))

# remove non-HYPE dets
keep_cols <- c(
  # identifiers
  "PSID",
  "HYPEDate",
  
  # HYPE substances
  "IN",
  "ON",
  "SP",
  "TP",
  "PP",
  "SS"
)

hype_ts <- hype_ts[, ..keep_cols]

setcolorder(
  hype_ts,
  c("PSID", "HYPEDate", "IN","ON", "SP", "TP", "PP", "SS")
)

# ============================================================
#  Complete quantitative timeseries 
# ============================================================

# apply concentration means with changepoints to quantitative dets
# dets that went through the qualitative route maintain 0 values 
# periods outside the consent window are filled with 0 values 

message("📈 Generating full monthly timeseries with applied trends... ")
flush.console()


setDT(quant_points)

#join quant_points to the full time series
hype_ts <- hype_ts[
  quant_points,
  on = .(PSID),
  allow.cartesian = TRUE
]

#add quantitative flows (output from Script 11)

quant_flows <- read_rds(
  here(
    "PointSourceData", "Discharges", "data_out",
    "Quantitative_flows_monthly.rds"
  )
)

default_flow <- 0.002893519 # STW flow default value

# where Flow_m3s is NA (flagged for quantitatively derived), 
# replace NA with the quantitative flow value

hype_ts[
  quant_flows,
  on = .(PSID, HYPEDate),
  Flow_m3s := fifelse(
    is.na(Flow_m3s),
    i.Flow,
    Flow_m3s
  )
]

#if no quant value exists (WISKI data not sufficient) then use the default value

hype_ts[
  is.na(Flow_m3s),
  Flow_m3s := default_flow
]


# flag whether the consent is active
hype_ts[
  ,
  consent_active := !is.na(EFFECTIVE_DATE) &
    HYPEDate >= EFFECTIVE_DATE &
    HYPEDate <= REVOCATION_DATE
]

#Substance mapping
sub_map <- list(
  IN = "IN_mgl",
  ON = "ON_mgl",
  SP = "SP_mgl",
  PP = "PP_mgl",
  TP = "TP_mgl",
  SS = "SS_mgl"
)

#apply rules 
for (i in names(sub_map)) {
  var <- sub_map[[i]]
  
  hype_ts[
    ,
    (var) := fifelse(
      !consent_active,
      0,                 # outside consent window = zero
      fifelse(
        !is.na(get(var)),
        get(var),       # quant_points overrides
        get(i)          # fallback to hype_ts - no replacement
      )
    )
  ]
}

# zero flows when consent is not active
hype_ts[
  ,
  Flow_m3s := fifelse(
    consent_active,
    Flow_m3s,
    0
  )
]

# make HYPE formatted date
hype_ts[
  ,
  Date := format(HYPEDate, "%Y%m")
]

# remove no longer required columns
hype_ts[, c("IN", "ON", "SP", "PP", "TP", "SS", "T1_cfu_ml","HYPEDate",
            "EFFECTIVE_DATE", "REVOCATION_DATE", "consent_active",
            "EFF_SAMPLE_POINT", "quant_for") := NULL]

# ============================================================
#  Reformat for HYPE PSMonthlySeries input
# ============================================================

message("🔀 Converting to HYPE PSMonthlySeries format...")
flush.console()

#rename variables
setnames(hype_ts, "Flow_m3s", "Flow") # flow_m3s ==> Flow
old_names <- names(hype_ts)
new_names <- gsub("_mgl$", "conc", old_names) # _mgl == conc
setnames(hype_ts, old_names, new_names)

#reshape to long format
hype_ts <- melt(
  hype_ts,
  id.vars = c("Date", "PSID"),
  variable.name = "var",
  value.name = "value"
)
hype_ts <- hype_ts[!is.na(Date)]
setorder(hype_ts, PSID, Date)

# create header identifier
hype_ts[
  ,
  col_id := paste(var, PSID, sep = "_")
]

# rewiden 
hype_ts <- dcast(
  hype_ts,
  Date ~ col_id,
  value.var = "value"
)

# not strictly necessary,
# but set order from lowest PSID with Flow and conc cols together

# define order
var_order <- c("Flow", "INconc", "ONconc", "SPconc", "PPconc", "TPconc", "SSconc") #FIOs (T1 is always qualiative)

data_cols <- names(hype_ts)[-1]  # exclude Date

col_map <- data.table(
  col = data_cols,
  var = sub("_.*$", "", data_cols),
  PSID = as.integer(sub("^.*_", "", data_cols))
)
col_map[, var_rank := match(var, var_order)] # determines order
setorder(col_map, PSID, var_rank) # sets order
hype_ts <- hype_ts[, c("Date", col_map$col), with = FALSE] #reorders by col_map

# extract variable names and PSIDs
cols <- names(hype_ts)[-1]
header1 <- c("Date", sub("_.*$", "", cols))
header2 <- c("0", sub("^.*_", "", cols))

# create DTs from headers
header1_dt <- as.data.table(as.list(header1))
header2_dt <- as.data.table(as.list(header2))

setnames(header1_dt, names(hype_ts))
setnames(header2_dt, names(hype_ts))

hype_ts <- rbind(
  header2_dt,
  hype_ts,
  use.names = FALSE
)
setnames(hype_ts,header1)

# save output
write_rds(
  hype_ts,
  here(
    "PointSourceData", "Discharges", 
       "data_out", "Quantitative_PSMonthlySeries.rds"
    )
)

message("🏁 Quantitative PSMonthlySeries created!")
flush.console()

# ============================================================
# Declared outputs
# ============================================================

declared_outputs <- c(
  # Final HYPE-ready quantitative PSMonthlySeries
  here(
    "PointSourceData", "Discharges",
    "data_out", "Quantitative_PSMonthlySeries.rds"
  ),
  
  # Missing effluent sites after quantitative join (conditional)
  here(
    "PointSourceData", "Discharges", "data_out"
  )
)

declared_outputs

