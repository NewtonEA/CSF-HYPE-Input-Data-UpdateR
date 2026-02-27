# ============================================================
# Prepare STW flow data for CSF-HYPE
# 
#
# Original process written by JBA 
# Adapted and integreated into pipeline by Tom Newton, Feb 2026
#
# ============================================================

message("❯❯❯❯ Preparing STW Flow data...")
flush.console()

# ============================================================
# Config
# ============================================================

#load required packages
if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse,
               here,
               data.table,
               changepoint)

#load master lookup and susbet to quant flow sites (with WISKI IDs)

flow_sites <- read_csv(
  here(
    "PointSourceData", "PointSourceData_master.csv"
    )
  ) %>% 
  filter(
    !is.na(WISKI_STATION)
  ) %>%
  select(
    PSID, WISKI_STATION
  )

# ============================================================
# Required inputs
# ============================================================

required_inputs <- c(
  # Master PSData lookup
  here("PointSourceData", "PointSourceData_master.csv"),
  
  # New WISKI exports (at least one file required)
  here(
    "PointSourceData", "Discharges", "data_in",
    "stw_flow_data", "new_exports"
  ),
  
  # Existing collated data folder (may be empty initially, but must exist)
  here(
    "PointSourceData", "Discharges", "data_in",
    "stw_flow_data", "collated_data"
  ),
  
  # Forcing metadata (used to define model end date)
  here("ForcingData", "_forcing_metadata.rds")
)

missing_inputs <- required_inputs[
  !file.exists(required_inputs) & !dir.exists(required_inputs)
]

if (length(missing_inputs) > 0) {
  stop(
    "❌ Missing required inputs for Script 11:\n",
    paste(" -", missing_inputs, collapse = "\n")
  )
}

  
# ============================================================
# Parse new data & collate with existing
# ============================================================

message("📚Collating new STW flow extracts with existing data...")
flush.console()

# Loop through WISKI effluent flow data export and split the files into
# separate sites. Write out each site to its own file in a folder for all
# sites. This makes subsequent steps simpler and makes checking easier.

# Find and list new data file(s)


new_exports_path <- here(
  "PointSourceData", "Discharges", "data_in",
  "stw_flow_data", "new_exports"
)

new_data <- list.files(
  new_exports_path,
  full.names = TRUE
)

# error if no new data in the exports folder
if (length(new_data) == 0 || !all(file.exists(new_data))) {
  stop(
    paste0(
      "❌ No new WISKI data found in:\n",
       new_exports_path,
      "\n Please add new WISKI data to the new_exports folder and re-run."
    ),
    call. = FALSE
  )
}

# Loop through each file
for (file in new_data) {
  # Read lines of file 
  # The input data contain superscripts (m^3) which need explicitly converting 
  # to UTF-8. I() is used to indicate literal input. 
  STW_flow_lines <- readLines(file)
  STW_flow_lines <- I(iconv(STW_flow_lines, from = "latin1", to = "utf-8"))
  
  
  # Find which rows contain 'Station name' which should be the first row for
  # each site
  first_rows <- grep("Station name", STW_flow_lines) 
  
  # Split sites based on position of first rows and put into a list. Firstly,
  # set a factor for grouping the data for splitting by repeating one to the 
  # number of first rows until we reach the first row of the next site so 
  # that each site has a unique number associated with it (needs to be the 
  # same length as STW_flow_lines), e.g. 1 5495 times then 2 5495 times ... 
  # number of sites 5495 times, assuming the site changes after 5495 rows 
  # (site changes are actually determined automatically by first_rows). 
  split_f <- rep(1:(length(first_rows)), diff(c(first_rows, (length(STW_flow_lines)+1))))
  STW_flows <- split(STW_flow_lines, split_f)
  
  # Loop through the list of separated flows and write to file
  lapply(seq_along(STW_flows), function(i){
    STW_flow <- STW_flows[[i]]
    # Extract site (WISKI) ID and variable name from site metadata.
    #Variable is DayTotal.P.
    WISKI_ID <- stringr::word(STW_flow[grep("Station number", STW_flow)], 2, sep = ",")
    WISKI_ID <- gsub("\"", "", WISKI_ID)  # remove quotation marks as cause issues with saving
   
     # Extract variable name (should be DayTotal.P only)
    ts_line <- STW_flow[grep("Time series name", STW_flow)]
    
    # Extract Day.Total.P 
    variable_raw <- stringr::str_extract(
      ts_line,
      "Day\\.Total\\.[P]"
    )
    
    if (is.na(variable_raw)) {
      
      fallback_name <- paste0(WISKI_ID, "_UNKNOWN_VARIABLE")
      
      warning(
        "Could not extract variable from Time series name for ",
        fallback_name
      )
      
      out_folder <- here(
        "PointSourceData", "Discharges", "data_in",
        "stw_flow_data", "collated_data", "manual_check"
      )
      
      dir.create(out_folder, showWarnings = FALSE, recursive = TRUE)
      
      write(
        STW_flow,
        file.path(out_folder, paste0(fallback_name, ".txt")),
        sep = "\t"
      )
      
      return(NULL)
    }
    
    # Normalise the naming convention
    variable <- gsub("\\.", "", variable_raw)   # Day.Total.P becomes DayTotalP
    variable <- sub("(DayTotal)([P])", "\\1.\\2", variable)
   #variable now == DayTotal.P or DayTotal.O
    
    
    # define unique_name
    unique_name <- paste0(WISKI_ID, "_", variable)

    
    # Check that multiple records are not under one site - previously found 
    # one site with no header so if this happens then it will be in one of 
    # the other sites and appear as duplicate dates. This checks for any duplicated 
    # dates per site and will give a warning if there are any, 
    # then write that output to a manual_check folder to be reviewed
    
    # Identify start of data block
    data_start <- grep("^Time stamp", STW_flow)
    
    if (length(data_start) != 1) {
      warning("Could not uniquely identify data start for ", unique_name,
              " – writing to manual_check")
      
      out_folder <- here(
        "PointSourceData", "Discharges", "data_in",
        "stw_flow_data", "collated_data", "manual_check"
      )
      
      dir.create(out_folder, showWarnings = FALSE, recursive = TRUE)
      out_file_path <- file.path(out_folder, paste0(unique_name, ".txt"))
      
      write(STW_flow, out_file_path, sep = "\t")
      return(NULL)
    }
    
    data_start <- data_start + 1
    
    # Extract dates (first column only)
    dates <- sub(",.*$", "", STW_flow[data_start:length(STW_flow)])
    dates <- dates[nzchar(dates)]
    
    # Identify duplicates
    dup_dates <- unique(dates[duplicated(dates)])
    
    if (length(dup_dates) > 0) {
      
      warning(
        paste(
          "Duplicate dates found for",
          unique_name,
          "-",
          paste(dup_dates, collapse = ", "),
          "– writing to manual_check"
        )
      )
      
      out_folder <- here(
        "PointSourceData", "Discharges", "data_in",
        "stw_flow_data", "collated_data", "manual_check"
      )
      
      dir.create(out_folder, showWarnings = FALSE, recursive = TRUE)
      out_file_path <- file.path(out_folder, paste0(unique_name, ".txt"))
      
      write(STW_flow, out_file_path, sep = "\t")
      
      return(NULL)  # Skip normal processing for this site
    }
    
    
    # Set folder to which to write out the single site based on variable
    out_folder <- here("PointSourceData", "Discharges", "data_in", "stw_flow_data", "collated_data")
    out_file_path <- file.path(out_folder, paste0(unique_name, ".txt"))
    # append to exisiting file (minus header) if it exists
    if (file.exists(out_file_path)) {
      start_row <- grep("Time stamp", STW_flow) + 1 
      write(STW_flow[start_row:length(STW_flow)],
            out_file_path, sep = "\t", append = TRUE)
    } else {
      write(STW_flow, out_file_path, sep = "\t")
    }
  })
}

# ============================================================
#  Check required sites are present and read in relevant data
# ============================================================
message("📖 Reading collated STW flow data...")
flush.console()

collated_path <- here(
  "PointSourceData", "Discharges", "data_in",
  "stw_flow_data", "collated_data"
)

# List all .txt collated files
collated_files <- list.files(collated_path, pattern = "\\.txt$", full.names = TRUE)

# Extract WISKI IDs from filenames (everything before first underscore)
collated_ids <- str_extract(basename(collated_files), "^[^_]+")

# Identify sites in flow_sites$WISKI_STATION that are missing in collated data
missing_sites <- flow_sites$WISKI_STATION[!(flow_sites$WISKI_STATION %in% collated_ids)]

# Report
cat("Number of WISKI stations in master list missing collated data:", length(missing_sites), "\n")

if(length(missing_sites) > 0){
  warning("The following WISKI stations are missing collated data:\n",
          paste(missing_sites, collapse = ", "))
  
# save a record of any missing sites for investivation
 data.frame(
  WISKI_STATION = missing_sites,
  Status = "Missing collated data from WISKI"
) %>%
   write_csv(
     here(
       "PointSourceData", "Discharges", "data_in",
       "stw_flow_data", "missing_WISKI_sites.csv"
       )
   )

} else {
  cat("✅All required WISKI sites have collated data.\n")
}

# function to read collated data (ignores leading rows with metadata)

read_collated_wiski <- function(file) {
  
  result <- list(
    data     = NULL,
    issue    = NULL,
    file     = basename(file),
    WISKI_ID = str_extract(basename(file), "^[^_]+")
  )
  
  lines <- iconv(readLines(file), from = "latin1", to = "UTF-8")
  
  header_row <- grep("^Time stamp", lines)
  
  if (length(header_row) != 1) {
    result$issue <- "Could not uniquely identify 'Time stamp' row"
    return(result)
  }
  
  if (length(lines) <= header_row) {
    result$issue <- "No data rows found after 'Time stamp'"
    return(result)
  }
  
  data_lines <- lines[(header_row + 1):length(lines)]
  
  df <- tryCatch(
    fread(
      text        = data_lines,
      sep         = ",",
      fill        = Inf,
      header      = FALSE,
      showProgress = FALSE
    ),
    error = function(e) {
      result$issue <- paste("Failed to parse data rows:", e$message)
      return(NULL)
    }
  )
  
  if (!is.null(df) && ncol(df) == 1) {
    warning(
      "fread() returned a single column for ",
      result$file,
      " — applying manual column split"
    )
    
    df <- data.table::tstrsplit(
      df[[1]],
      ",",
      fixed = TRUE,
      fill  = Inf
    ) |> as.data.table()
  }
  
  if (is.null(df) || ncol(df) < 3) {
    result$issue <- "Parsed data has fewer than 3 columns"
    return(result)
  }
  
  df <- df[, 1:3]
  setnames(df, c("Date", "Value", "QC_code"))
  
  df[, Date := as.character(Date)]
  df[, Value := suppressWarnings(as.numeric(Value))]
  df[, QC_code := as.character(QC_code)]
  
  if (nrow(df) > 0 && all(is.na(df[["Value"]]))) {
    result$issue <- "All values missing (e.g. '---')"
  }
  
  df[, WISKI_ID := result$WISKI_ID]
  
  result$data <- df
  result
}


# identify required collated files to read

collated_path <- here(
  "PointSourceData", "Discharges", "data_in",
  "stw_flow_data", "collated_data"
)

collated_files <- list.files(
  collated_path,
  pattern = "\\.txt$",
  full.names = TRUE
)

# Extract WISKI IDs
collated_ids <- str_extract(basename(collated_files), "^[^_]+")

# Keep only sites present in flow_sites
keep <- collated_ids %in% flow_sites$WISKI_STATION
collated_files_valid <- collated_files[keep]

# read data into list 
collated_data <- lapply(collated_files_valid, read_collated_wiski)

# Drop any NULLs (files that failed checks)
collated_data <- Filter(Negate(is.null), collated_data)

# identify and log any issues reading data
issues <- do.call(
  rbind,
  Filter(
    Negate(is.null),
    lapply(collated_data, function(x) {
      if (!is.null(x$issue)) {
        data.frame(
          file = x$file,
          WISKI_ID = x$WISKI_ID,
          issue = x$issue,
          stringsAsFactors = FALSE
        )
      }
    })
  )
)


issues_path <- here(
  "PointSourceData", "Discharges", "data_in",
  "stw_flow_data", "read_issues_log.csv"
)
if (!is.null(issues) && nrow(issues) > 0) {
  write.csv(issues, issues_path, row.names = FALSE)
  warning(
    nrow(issues),
    " collated file(s) could not be read cleanly. ",
    "See read_issues_log.csv for details."
  )
}

# ============================================================
# Convert to monthly timeseries 
# ============================================================

# Combine daily data from collated_data
daily_dt <- rbindlist(
  lapply(collated_data, function(x) x$data),
  use.names = TRUE,
  fill = TRUE
)

# Parse date
daily_dt[, Date := as.IDate(Date, format = "%d/%m/%Y")]

#only keep "Good" data, others become NA
daily_dt[QC_code != "G", Value := NA_real_]

# Replace negative values with NA
daily_dt[Value < 0, Value := NA_real_]

# replace very large values with NA
daily_dt[Value > 5000000, Value := NA_real_]

# Convert from m3/day to m3/s
daily_dt[, Value_m3s := Value / 86400]

#ensure no duplicated dates per site 
# this can happen if new exports contain any previously collated records
# just keep the first record - G records in WISKI should never have genuine multiple readings
setorder(daily_dt, WISKI_ID, Date)
daily_dt <- unique(daily_dt, by = c("WISKI_ID", "Date"))


# Aggregate to monthly time series per site
monthly_dt <- daily_dt[
  ,
  .(
    Value_m3s = mean(Value_m3s, na.rm = TRUE),
    n_days    = sum(!is.na(Value_m3s))
  ),
  by = .(
    WISKI_ID,
    Year  = year(Date),
    Month = month(Date)
  )
]

# Create a proper monthly date (1st of month)
monthly_dt[, Date := as.IDate(sprintf("%04d-%02d-01", Year, Month))]

# If less than 10 days of data set value to NA
monthly_dt[n_days < 10, Value_m3s := NA_real_]

# identify sites where all monthly values are NA
all_na_sites <- monthly_dt[
  , .(all_na = all(is.na(Value_m3s))),
  by = WISKI_ID
][
  all_na == TRUE, WISKI_ID
]

# report any missing
# report any sites where all flow values are NA and replace with default
if (length(all_na_sites) > 0) {
  warning(
    "The following WISKI sites have all missing flow values:\n",
    paste(all_na_sites, collapse = ", "),
    "\nDefault values will be used."
  )
  
  # replace NAs with default value for these sites
  default_value <- 0.002893519 # STW default
  monthly_dt[WISKI_ID %in% all_na_sites, Value_m3s := default_value]
}


#add PSIDs to monthly_dt
setDT(flow_sites)
monthly_dt <- flow_sites[
  monthly_dt,
  on = .(WISKI_STATION = WISKI_ID),
  allow.cartesian = TRUE # there can be a one-to-many relationship WISKI_ID to multiple PSIDs
]

# Tidy final structure
setorder(monthly_dt, WISKI_STATION, Date)
monthly_dt[, c("Year", "Month") := NULL]

# ============================================================
# detect significant changepoints 
# ============================================================

# ensure chronological order by site
setorder(monthly_dt, WISKI_STATION, Date)

# Function to detect up to 3 most significant changepoints per site
detect_changepoints_flow <- function(x, dates,
                                     # Change in flow regime must persist
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
  # To have one changepoint, you need two segments
  # Each segment must be at least minseglen long
  if (n < minseglen * 2) { # so minimum 4 year record required 
    return(list(
      n_changes = 0L,
      cp_dates = as.Date(character(0)),
      segment_means = mean(x, na.rm = TRUE)
    ))
  }
  
  # Manual BIC-style penalty (scaled to be conservative)
  pen <- penalty_mult * log(n)
  cp <- try(changepoint::cpt.meanvar( # detec changepoint in mean and variance - hydrologically significant changes with stabilisation i.e. operational change 
    x, method = "PELT", penalty = "Manual", pen.value = pen, minseglen = minseglen
  ), silent = TRUE)
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

# Apply changepoint detection to monthly flow per site
message("🕵️ Detecting significant changepoints in STW flow data...")
flush.console()

cp_flow_results <- monthly_dt[
  ,
  {
    res <- detect_changepoints_flow(Value_m3s, Date)
    .(
      n_changes = as.integer(res$n_changes),
      cp_dates = list(as.Date(res$cp_dates)),
      segment_means = list(as.numeric(res$segment_means))
    )
  },
  by = .(PSID, WISKI_STATION)
]

# ============================================================
#  Create full sequence timeseries of monthly means 
# ============================================================

message("🛠️ Creating quatitative flows monthly timeseries...")
flush.console()

# Set the fixed date range (end date determined by model forcing data end date)

forcing_meta <- read_rds(here("ForcingData", "_forcing_metadata.rds"))
start_date <- as.Date("2000-01-01")
end_date   <- forcing_meta$latest_forcing_date
all_months <- seq(start_date, end_date, by = "month")

# Create the full monthly sequence for all STW flow sites
sites <- unique(monthly_dt[, .(PSID, WISKI_STATION)])

full_seq <- sites[, .(HYPEDate = all_months), by = .(PSID, WISKI_STATION)]

# Map the changepoints and segment means onto the complete monthly sequence
full_seq <- full_seq[
  cp_flow_results,
  on = .(PSID,WISKI_STATION),
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
  by = .(PSID, WISKI_STATION)
]

# For dates where a site has measured flow, replace computed value with measured data

# Ensure monthly_dt has Date column aligned with HYPEDate
monthly_dt[, HYPEDate := as.IDate(Date)]

# Join measured monthly values to full_seq and overwrite Flow where Value_m3s is not NA
full_seq[
  monthly_dt[!is.na(Value_m3s), .(WISKI_STATION, PSID, HYPEDate, Value_m3s)],
  on = .(PSID, WISKI_STATION, HYPEDate),
  Value := i.Value_m3s
]

# Select/rename required output variables 
full_seq <- full_seq[, .(PSID, HYPEDate, Flow = Value)]
setorder(full_seq, PSID, HYPEDate)

#save output 
write_rds(
  full_seq, 
    here(
     "PointSourceData", "Discharges", 
     "data_out", "Quantitative_flows_monthly.rds"
    )
)

# ============================================================
#  Cleanup project folder and log details ready for next update
# ============================================================

# Prepare log information
log_entry <- data.table(
  Last_update = Sys.Date(),
  Collated_WISKI_start = min(daily_dt$Date),
  Collated_WISKI_end = max(daily_dt$Date)
)

# write to stw_flow_data folder
write_csv(
  log_entry,
  here(
    "PointSourceData", "Discharges", "data_in", 
       "stw_flow_data", "flow_data_log.csv"
    )
  ) 

# remove WISKI exports (they are now added to collated data)
file.remove(new_data)

# Done!
message("🏁 Quantitative flows processing complete!")
flush.console()    

# ============================================================
# Declared outputs
# ============================================================

declared_outputs <- c(
  # Monthly quantitative flow timeseries for HYPE
  here(
    "PointSourceData", "Discharges",
    "data_out", "Quantitative_flows_monthly.rds"
  ),
  
  # Missing WISKI site report (only written if applicable)
  here(
    "PointSourceData", "Discharges", "data_in",
    "stw_flow_data", "missing_WISKI_sites.csv"
  ),
  
  # Read/parsing issues log (only written if applicable)
  here(
    "PointSourceData", "Discharges", "data_in",
    "stw_flow_data", "read_issues_log.csv"
  ),
  
  # Update log for STW flow data
  here(
    "PointSourceData", "Discharges", "data_in",
    "stw_flow_data", "flow_data_log.csv"
  )
)

declared_outputs
