# ============================================================
# CSF-HYPE Abstractions PSMonthlySeries updater
#  Revised and Tested Jan 2026 - Tom Newton
# ============================================================
#
# This script builds monthly abstraction point-source time series for HYPE
#  by integrating abstraction RETURNS data (preferred) or WRGIS data (no returns)
#  and outputs HYPE-formatted PSMonthlySeries and PointSourceData
#
## Notes:
#  - Requires latest (all return years since last update) abstraction
# returns data extracted (NALD-BOXI or requested) to be placed in
# the /Abstractions/data_in/returns_data folder of this project directory
# - Required latest WRGIS snapshot adding to :
# /Abstractions/data_in/WRGIS_data
##
### WARNING -  Always rebuilds outputs from current inputs, so will run 
# even if new return data has not be added!
# ============================================================



message("❯❯❯❯ Building abstraction PSMonthly series...")
flush.console()

# ============================================================
# Config
# ============================================================

# install/load required packages 
if (!require(pacman)) install.packages("pacman")
pacman::p_load(here,
               tidyverse,
               sf,
               lubridate,
               rnrfa,
               readxl,
               data.table)


# Required inputs

required_inputs <- c(
  here("GeoData", "Eco_HYPE_SUBBASINS.gpkg"),
  here("ForcingData", "_forcing_metadata.rds"),
  here("PointSourceData", "Abstractions", "data_in", "returns_data"),
  here("PointSourceData", "Abstractions", "data_in", "returns_details"),
  here("PointSourceData", "Abstractions", "data_in", "WRGIS_data"),
  here("PointSourceData", "Abstractions", "data_in", "use_code_ret_frac.csv")
)

missing_inputs <- required_inputs[!file.exists(required_inputs)]

if (length(missing_inputs) > 0) {
  stop(
    "❌ Missing required inputs for Script 8:\n",
    paste(" -", missing_inputs, collapse = "\n")
  )
}

# directory paths
returns_dir <- here("PointSourceData", "Abstractions", "data_in", "returns_data")

# load subbasin polygons
subs <- st_read(here("GeoData", "Eco_HYPE_SUBBASINS.gpkg")) 


# Get latest forcing year  - used to check alignment with abstraction data
forcing_metadata_file <- here("ForcingData", "_forcing_metadata.rds")
forcing_metadata <- readRDS(forcing_metadata_file)
forcing_year <- forcing_metadata$latest_forcing_year


# ============================================================
# Read in and merge all returns Excel workbooks and/or CSV files
# ============================================================

message("📚Reading Abstraction returns workbooks")
flush.console()

# Find Excel workbooks
file.list <- list.files(
  returns_dir,
  pattern = "\\.xlsx$",
  recursive = TRUE,
  full.names = TRUE
)

# Find CSV files
csv.list <- list.files(
  returns_dir,
  pattern = "\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

# Check at least one input exists
if (length(file.list) == 0 && length(csv.list) == 0) {
  stop(
    "❌ No abstraction returns files (.xlsx or .csv) found in returns_data"
  )
}

# ============================================================
# Establish master column structure
# ============================================================

if (length(file.list) > 0) {
  
  headers <- names(
    read_excel(
      file.list[1],
      sheet = 1,
      n_max = 0
    )
  )
  
} else {
  
  headers <- names(
    read_csv(
      csv.list[1],
      n_max = 0,
      show_col_types = FALSE
    )
  )
  
}

# ============================================================
# Helper function to standardise sheets 
# ============================================================

standardise_cols <- function(df, headers) {
  
  missing_cols <- setdiff(headers, names(df))
  
  if (length(missing_cols) > 0) {
    df[missing_cols] <- NA
  }
  
  df[, headers, drop = FALSE] %>%
    mutate(across(everything(), as.character))
  
}

# ============================================================
# Read all Excel sheets from all workbooks
# ============================================================

if (length(file.list) > 0) {
  
  excel_returns <- lapply(file.list, function(f) {
    
    sheets <- excel_sheets(f)
    
    lapply(sheets, function(s) {
      
      df <- read_excel(f, sheet = s)
      
      # Skip empty sheets
      if (nrow(df) == 0) return(NULL)
      
      # Skip non-data sheets
      if (!"Abstraction Date" %in% names(df)) return(NULL)
      
      standardise_cols(df, headers)
      
    }) %>%
      bind_rows()
    
  }) %>%
    bind_rows()
  
} else {
  
  excel_returns <- tibble()
  
}

# ============================================================
# Read all CSV files
# ============================================================

if (length(csv.list) > 0) {
  
  csv_returns <- lapply(csv.list, function(f) {
    
    read_csv(
      f,
      show_col_types = FALSE
    ) %>%
      standardise_cols(headers)
    
  }) %>%
    bind_rows()
  
} else {
  
  csv_returns <- tibble()
  
}

# ============================================================
# Combine Excel and CSV returns
# ============================================================

all_returns <- bind_rows(
  excel_returns,
  csv_returns
) %>% 
  mutate(`Point ID` = as.numeric(`Point ID`))

# the spatial information for abstractions come in seperate workbooks (WQ request ....details), 
# all these workbooks should be added to the returns_details folder

details_files <- list.files(
  here("PointSourceData", "Abstractions",
       "data_in", "returns_details"),
  full.names = TRUE
)

df.details <- lapply(details_files, function(file) {
  
  ext <- tolower(tools::file_ext(file))
  
  df <- if (ext %in% c("xlsx", "xls")) {
    
    suppressWarnings(
      read_excel(file, progress = TRUE)
    )
    
  } else if (ext == "csv") {
    
    suppressWarnings(
      read_csv(file, show_col_types = FALSE)
    )
    
  } else {
    
    return(NULL)
    
  }
  
  df %>%
    mutate(across(everything(), as.character))
  
}) %>%
  bind_rows() %>%
  distinct(`Licence No.`, `Point ID`, .keep_all = TRUE)

#convert the abstraction point NGRs to XYs
XYs <- osg_parse(df.details$`NGR 1`)

#covert to spatial object and join to subbasins  

df.details$X <- XYs[[1]]
df.details$Y <- XYs[[2]]

df.details <- df.details %>%
  filter(`Source Type` != "TW") %>%
  st_as_sf(coords = c("X","Y"), crs= st_crs(subs)) %>% 
  st_join(subs) %>% 
  st_drop_geometry() %>% 
  mutate(`Point ID` = as.numeric(`Point ID`))

#load use code return fraction lookup

return_fractions <- read_csv(here("PointSourceData", "Abstractions", 
                                  "data_in", "use_code_ret_frac.csv"),
                             col_types = c("nn")) %>% 
  mutate(`Use Code` = as.character(Use_Code)) %>% 
  distinct(`Use Code`, .keep_all = TRUE)


message("✅Returns data collated")
flush.console()

rm(
  list = intersect(
    c(
      "excel_returns",
      "csv_returns",
      "headers"
    ),
    ls()
  )
)


# before continuing to process abstractions, check that the abstraction data is
# in alignment with model forcing data

latest_return_yr <- lubridate::year(
  max(
    all_returns$`Abstraction Date`
    )
  )

if (latest_return_yr < forcing_year) {
  
  msg <- paste0(
    "⚠️ Abstraction returns only available up to ", latest_return_yr,
    " but forcing data runs to ", forcing_year
  )
  
  warning(
    msg,
    call. = FALSE,
    immediate. = TRUE
  )
  
  answer <- readline("Do you wish to continue? (y/n): ")
  
  if (tolower(answer) != "y") {
    stop(
      "❌ Process stopped - rerun after adding new Abstraction returns.",
      call. = FALSE
    )
  }
}

# ============================================================
# calculate abstraction quantities to use in HYPE, with adjustements
# ============================================================

# An abstraction licence can have multiple abstraction points (some up to ca.50)
# But on inspection the returns do not consistently give abstracted quantities
# per point (Point ID). Points may be spread over multiple sub-basins. 
# For a given date, many returns give the same abstraction 
# quantity for all points associated with a licence, yet others will have unqiue
# quantities per point - some will have two or more values duplicated over all points! 
# It is unlikely that exactly the same amount has been 
# abstracted from each point on a given date, so assume this is the total 
# amount abstracted and distribute it among all points associated per licence, date.  
# TBD - rewrite in entirely in data.table

message("⚙️ Adjusting returned quantities for HYPE")
flush.console()

## Make sure everything is data.table
setDT(all_returns)
setDT(df.details)
setDT(return_fractions)

# remove returns with no quantity
all_returns <- all_returns[!is.na(`Return Qty`)]

# add details
all_returns <- merge(
  all_returns,
  df.details,
  by = c("Licence No.", "Point ID"),
  all = FALSE,
  allow.cartesian = TRUE
)

# Compute abstraction and returned quantities
all_returns <- merge(
  all_returns,
  return_fractions,
  by = "Use Code",
  all.x = TRUE,
  allow.cartesian = TRUE
)

# adjust quantities to account for fraction assumed to be returned locally
all_returns[, ret_frac := fifelse(is.na(ret_frac), 0, ret_frac)]

all_returns[, `Return Qty` := as.numeric(`Return Qty`)]

all_returns[, Abs_Qty_raw := fifelse(
  ret_frac == 0,
  `Return Qty`,
  `Return Qty` * (1 - ret_frac)
)]

all_returns[, returned_qty := `Return Qty` * ret_frac]

# Remove zero abstractions after adjustment
all_returns <- all_returns[Abs_Qty_raw > 0]

# detect multiple points all with the same returned quantity (i.e. min == max)
all_returns[, `:=`(
  n_points = .N,
  same_qty = (min(Abs_Qty_raw) == max(Abs_Qty_raw))
), by = .(`Licence No.`, `Abstraction Date`)]

# distribute returned quantity where needed
all_returns[, Abs_Qty := Abs_Qty_raw]
all_returns[n_points > 1 & same_qty == TRUE, Abs_Qty := Abs_Qty_raw / n_points]

# select & rename
all_returns <- all_returns[, .(
  SUBID,
  Source_Type      = `Source Type`,
  Abs_Qty,
  Date             = `Abstraction Date`,
  Licence_no       = `Licence No.`,
  Point_ID         = `Point ID`,
  Start_Date       = `Orig. Effective Date`,
  Revocation_date  = `Revoked Date`,
  returned_qty
)]

# rebuild df.details from processed all_returns
df.details <- unique(all_returns[, .(
  SUBID,
  Source_Type,
  Licence_no,
  Point_ID,
  Start_Date,
  Revocation_date
)])

# assume missing revocation dates means still active (set to year 3000)
df.details[, Revocation_date := as.Date(Revocation_date)]
df.details[is.na(Revocation_date), Revocation_date := as.Date("3000-01-01")]


# ============================================================
# convert to monthly abstraction rates per abstraction
# ============================================================

message("🔀 Converting to monthly abstraction rates")
flush.console()

setkey(df.details, Licence_no, Point_ID, Source_Type)

#convert dates to same date format 
all_returns[, Date := as.IDate(
  sub(" .*", "", gsub("/", "-", Date)),
  format = "%Y-%m-%d"
)]

# conversion from m3/month to m3/s based on: 
# standardised month = 30.44 days == 2629800 seconds

abs_monthly <- all_returns[
  , .(Abs_rate = sum(Abs_Qty) / 2629800), 
  by = .(
    Licence_no,
    Point_ID,
    Source_Type,
    date_hype = format(Date, "%Y%m")
  )
][
  df.details, on = .(Licence_no, Point_ID, Source_Type),
  allow.cartesian = TRUE
][
  !is.na(SUBID)
]

returned_quantities <- all_returns[
  , .(Return_rate = sum(returned_qty) / 2629800),
  by = .(
    Licence_no,
    Point_ID,
    Source_Type,
    date_hype = format(Date, "%Y%m")
  )
][
  df.details, on = .(Licence_no, Point_ID, Source_Type),
  allow.cartesian = TRUE
][
  !is.na(SUBID) & Source_Type == "GW"
]


write_rds (
  abs_monthly, 
  here(
    "PointSourceData", "Abstractions","data_in","Abs_returns_monthly.rds"
    )
  )

rm(abs_monthly, all_returns)

# ============================================================
# Process the WRGIS abstraction data
# ============================================================
message("⚙️ Processing WRGIS abstraction data")
flush.console()

sw  <- list.files(here("PointSourceData", "Abstractions", 
                       "data_in", "WRGIS_data"), ".xlsx", full.names = T) %>% 
  read_excel(sheet = 3, skip = 1)

#remove prefixes and special characters from names
names(sw) <- gsub("^SWABS.|[\r\n]+", "", names(sw))


sw <- sw %>% 
  select(Licence_no = Licnumber, 
         X = eastIMP, 
         Y = northIMP,
         Abs_qty = RAPtPAnQM3, 
         start = StartMon, 
         end = EndMon, 
         PERCENT_RT:Dec) %>%
  st_as_sf(coords = c("X","Y"), crs = st_crs(subs)) %>% 
  st_join(subs) %>%
  st_drop_geometry() %>%
  filter(!is.na(SUBID)) %>% 
  distinct(Licence_no, SUBID, .keep_all = TRUE) %>%
  filter(!Licence_no %in% df.details$Licence_no) %>% # remove any abstractions that are present in the returns data
  filter(PERCENT_RT < 100) %>% #remove any abs that are 100% returned locally
  #convert monthly profiles into fractions which the abs_qty can be distributed by
  mutate(across(Jan:Dec, ~ replace_na(.,0)),
         profile_sum = rowSums(across(Jan:Dec)), 
         profile_sum = case_when(profile_sum == 0 ~ 12,
                                 .default = profile_sum), 
         across(Jan:Dec, ~ .x / profile_sum), 
         #CONVERT RETURN PERCENT TO FRACTION
         PERCENT_RT = PERCENT_RT/100, 
         #and make 0 fractions to 1
         # adjust for the returned fraction
         Abs_qty = Abs_qty * (1 - PERCENT_RT)) %>% 
  filter(Abs_qty > 0 ) %>% # remove any abstractions that now have zero quantities following adjustment
  #then multiply the monthly profiles by the abs_qty to distribute the abstraction monhtly
  mutate(across(Jan:Dec, ~ .x * Abs_qty)) %>% 
  select(Jan:Dec, SUBID, Licence_no) %>% 
  pivot_longer(Jan:Dec, names_to = "Month", values_to = "Abs_qty") %>% 
  #convert to numeric representation of months 
  mutate(Month = case_when (Month == "Jan" ~ "01",
                            Month == "Feb" ~ "02",
                            Month == "Mar" ~ "03",
                            Month == "Apr" ~ "04",
                            Month == "May" ~ "05",
                            Month == "Jun" ~ "06",
                            Month == "Jul" ~ "07",
                            Month == "Aug" ~ "08",
                            Month == "Sep" ~ "09",
                            Month == "Oct" ~ "10",
                            Month == "Nov" ~ "11",
                            Month == "Dec" ~ "12"))  %>%
  group_by(SUBID, Month) %>% 
  mutate(Abs_rate = sum(Abs_qty) / 2629800,
         Source_Type = "SW") %>% 
  select(-Month)

## repeat for groundwater abstractions 

gw  <- list.files(here("PointSourceData", "Abstractions", 
                       "data_in", "WRGIS_data"), ".xlsx", full.names = T) %>% 
  read_excel(sheet = 2, skip = 1)

#remove prefixes and special characters from names
names(gw) <- gsub("^GWABS.|[\r\n]+", "", names(gw))

gw <- gw %>% 
  select(Licence_no = LicNumber, 
         X = PTeast, 
         Y = PTnorth,
         Abs_qty = RAPtPAnQM3, 
         start = StartMon, 
         end = EndMon, 
         PERCENT_RT:Dec) %>%
  st_as_sf(coords = c("X","Y"), crs = st_crs(subs)) %>% 
  st_join(subs) %>%
  st_drop_geometry() %>%
  filter(!is.na(SUBID)) %>%
  distinct(Licence_no, SUBID, .keep_all = TRUE) %>%
  filter(!Licence_no %in% df.details$Licence_no) %>% # remove any abstractions that are present in the returns data
  filter(PERCENT_RT < 100) %>% #remove any abs that are 100% returned locally
  #convert monthly profiles into fractions which the abs_qty can be distributed by
  mutate(across(Jan:Dec, ~ replace_na(.,0)),
         profile_sum = rowSums(across(Jan:Dec)), 
         profile_sum = case_when(profile_sum == 0 ~ 12,
                                 .default = profile_sum), 
         across(Jan:Dec, ~ .x / profile_sum), 
         #CONVERT RETURN PERCENT TO FRACTION
         PERCENT_RT = PERCENT_RT/100, 
         #and make 0 fractions to 1
         # adjust for the returned fraction
         Abs_qty = Abs_qty * (1 - PERCENT_RT), 
         #keep a record of the returned qty
         returned_qty = Abs_qty * PERCENT_RT) %>% 
  filter(Abs_qty > 0 ) 

wrgis_gw_returned <- gw %>%
  mutate(across(Jan:Dec, ~ .x * returned_qty)) %>% 
  select(Jan:Dec, SUBID, Licence_no) %>% 
  pivot_longer(Jan:Dec, names_to = "Month", values_to = "returned_qty") %>% 
  #convert to numeric representation of months 
  mutate(Month = case_when (Month == "Jan" ~ "01",
                            Month == "Feb" ~ "02",
                            Month == "Mar" ~ "03",
                            Month == "Apr" ~ "04",
                            Month == "May" ~ "05",
                            Month == "Jun" ~ "06",
                            Month == "Jul" ~ "07",
                            Month == "Aug" ~ "08",
                            Month == "Sep" ~ "09",
                            Month == "Oct" ~ "10",
                            Month == "Nov" ~ "11",
                            Month == "Dec" ~ "12"))  %>%
  group_by(SUBID, Month) %>% 
  mutate(Return_rate = sum(returned_qty) / 2629800,
         Source_Type = "GW_transfer") %>% 
  ungroup() 

gw <- gw %>% # remove any abstractions that now have zero quantities following adjustment
  #then multiply the monthly profiles by the abs_qty to distribute the abstraction monhtly
  filter(PERCENT_RT < 100) %>% #remove any abs that are 100% returned locally
  mutate(across(Jan:Dec, ~ .x * Abs_qty)) %>% 
  select(Jan:Dec, SUBID, Licence_no) %>% 
  pivot_longer(Jan:Dec, names_to = "Month", values_to = "Abs_qty") %>% 
  #convert to numeric representation of months 
  mutate(Month = case_when (Month == "Jan" ~ "01",
                            Month == "Feb" ~ "02",
                            Month == "Mar" ~ "03",
                            Month == "Apr" ~ "04",
                            Month == "May" ~ "05",
                            Month == "Jun" ~ "06",
                            Month == "Jul" ~ "07",
                            Month == "Aug" ~ "08",
                            Month == "Sep" ~ "09",
                            Month == "Oct" ~ "10",
                            Month == "Nov" ~ "11",
                            Month == "Dec" ~ "12"))  %>%
  group_by(SUBID, Month) %>% 
  mutate(Abs_rate = sum(Abs_qty) / 2629800,
         Source_Type = "GW")

# ============================================================
#  Create the full Abstactions PSMonthlySeries
# ============================================================

#read the processed returns back in 

returns_processed <- read_rds(here("PointSourceData", "Abstractions",
                                   "data_in","Abs_returns_monthly.rds"))


#returns only began in 2006, 
#so first need to extract and apply the pattern observed in the returns to extend the series

message("📈 Extracting trends in returns data")
flush.console()

returns_patterns <- returns_processed %>% 
  mutate(month = as.character(str_sub(date_hype,5,6))) %>% 
  group_by(Licence_no, Source_Type,SUBID, month) %>% 
  summarise(Abs_rate = mean(Abs_rate),
            Start_Date = strftime(min(Start_Date), "%Y%m" ))%>% 
  ungroup()

gw_returned_patterns <- returned_quantities %>% 
  mutate(month = as.character(str_sub(date_hype,5,6))) %>% 
  group_by(Licence_no, Source_Type,SUBID, month) %>% 
  summarise(Return_rate = mean(Return_rate),
            Start_Date = strftime(min(Start_Date), "%Y%m" ))%>% 
  ungroup()

returns_monthly <- returns_processed %>% 
  mutate(ts_source = "RETURNS") %>%
  group_by(SUBID, Source_Type, date_hype) %>% 
  #sum the rates from all licenses within a SUBID (PSID)
  mutate(Abs_rate = sum(Abs_rate), 
         Start_Date = min(Start_Date)) %>%
  ungroup() 

gw_returned_monthly <- returned_quantities%>% 
  mutate(ts_source = "RETURNS") %>%
  group_by(SUBID, Source_Type, date_hype) %>% 
  #sum the rates from all licenses within a SUBID (PSID)
  mutate(Return_rate = sum(Return_rate), 
         Start_Date = min(Start_Date)) %>%
  ungroup() 

message("📝 Assigning PSIDs")
flush.console()

PSD_returns <- returns_monthly %>% 
  ungroup() %>%
  select(SUBID, Source_Type, ts_source)  %>% 
  distinct() %>%
  filter(Source_Type != "TW") %>%
  mutate(PSID = seq(30000,length.out = nrow(.), by = 1),
         ps_type = -1, 
         ps_source = case_when(Source_Type == "SW" ~ 1, 
                               Source_Type == "GW" ~ 4))  

returns_monthly <- returns_monthly %>% 
  ungroup() %>% 
  distinct() %>% 
  select(SUBID, Date_0 = date_hype, Abs_rate, Source_Type, ts_source )

Year = seq(2000, 2005)
Month <- sprintf("%02d", 1:12)

#final, full returns ts

message("🛠️ Generating full returns timeseries")
flush.console()

returns_ts <- expand.grid(Year = Year, month = Month) %>% 
  arrange(Year, month) %>% 
  inner_join(returns_patterns, by = "month", relationship = "many-to-many") %>%
  mutate(Date_0 = as.character(paste0(Year, month)), 
         Abs_rate = case_when( Date_0 < Start_Date ~ 0, 
                               .default = Abs_rate)) %>% 
  select(SUBID, Date_0, Abs_rate, Source_Type) %>%
  bind_rows(returns_monthly) %>%
  inner_join(PSD_returns, by = c("SUBID", "Source_Type")) %>% 
  group_by(Date_0, PSID) %>% # aggregate to the psid
  summarise(Abs_rate = sum(Abs_rate)) %>% 
  ungroup() %>%
  mutate(header = paste0("Flow_", PSID)) %>%
  select(Date_0, Abs_rate, header) %>% 
  pivot_wider(names_from = header, values_from = Abs_rate) %>%
  replace(., is.na(.), 0)

## WRGIS source

message("🛠️ Generating full WRGIS timeseries")
flush.console()

wrgis_monthly <- bind_rows(sw, gw) %>% 
  ungroup() %>%
  #aggregate by type and SUBID
  group_by(SUBID, Month, Source_Type) %>% 
  summarise(Abs_qty = sum(Abs_qty)) %>%
  mutate(ts_source = "WRGIS") %>%
  ungroup()

wrgis_gw_returned_monthly <- wrgis_gw_returned %>% 
  ungroup() %>%
  #aggregate by type and SUBID
  group_by(SUBID, Month, Source_Type) %>% 
  rename(Abs_qty = returned_qty) %>%
  summarise(Abs_qty = sum(Abs_qty)) %>%
  mutate(ts_source = "WRGIS") %>%
  ungroup() %>% 
  distinct()


wrgis_psid_origin <- max(PSD_returns$PSID) + 1

PSD_wrgis <- wrgis_monthly %>%
  ungroup() %>%
  select(SUBID, Source_Type, ts_source) %>% 
  distinct() %>%
  filter(Source_Type != "TW") %>%
  mutate(PSID = seq(wrgis_psid_origin,length.out = nrow(.), by = 1),
         ps_type = -1, 
         ps_source = case_when(Source_Type == "SW" ~ 1, 
                               Source_Type == "GW" ~ 4))  

wrgis_psid_origin_gw <- max(PSD_wrgis$PSID) + 1

PSD_wrgis_gw_returned <- wrgis_gw_returned_monthly %>% 
  ungroup() %>% 
  select(SUBID, Source_Type, ts_source) %>% 
  distinct() %>%
  mutate(ps_type = 5, ps_source = 3, # abstracts from GW discharges to SW
         PSID = seq(wrgis_psid_origin_gw,length.out = nrow(.), by = 1))

PSD_wrgis <- bind_rows(PSD_wrgis, PSD_wrgis_gw_returned) ; rm(PSD_wrgis_gw_returned)


# wrgis 

Year = seq(2000, as.numeric(str_sub(max(returns_ts$Date_0),1,4)))

wrgis_ts <- expand.grid(Year = Year, Month = Month) %>% 
  arrange(Year, Month) %>% 
  inner_join(wrgis_monthly, by ="Month", relationship = "many-to-many") %>% 
  mutate(Date_0 = as.character(paste0(Year, Month)), 
         Abs_rate = Abs_qty / 2629800) %>% 
  inner_join(PSD_wrgis, by = c("SUBID", "Source_Type"), relationship = "many-to-many") %>%
  mutate(header = paste0("Flow_", PSID)) %>%
  select(Date_0, Abs_rate, header) %>% 
  pivot_wider(names_from = header, values_from = Abs_rate) %>%
  replace(., is.na(.), 0)

wrgis_ts_gw_returned <- expand.grid(Year = Year, Month = Month) %>% 
  arrange(Year, Month) %>% 
  inner_join(wrgis_gw_returned_monthly, by ="Month", relationship = "many-to-many") %>% 
  mutate(Date_0 = as.character(paste0(Year, Month)), 
         Abs_rate = Abs_qty / 2629800) %>% 
  inner_join(PSD_wrgis, by = c("SUBID", "Source_Type"), relationship = "many-to-many") %>%
  mutate(header = paste0("Flow_", PSID)) %>%
  select(Date_0, Abs_rate, header) %>% 
  pivot_wider(names_from = header, values_from = Abs_rate) %>%
  replace(., is.na(.), 0)

# gw transfers to sw 

gw_psid_origin <- max(PSD_wrgis$PSID) + 1

PSD_gw_returns <- gw_returned_monthly %>%
  ungroup() %>%
  select(SUBID, Source_Type, ts_source)  %>% 
  distinct() %>%
  mutate(PSID = seq(gw_psid_origin,length.out = nrow(.), by = 1),
         ps_type = 5, # type 5 - gw transfers only
         ps_source = 3 )  # surface water discharge (flow only)

gw_returned_monthly <- gw_returned_monthly %>% 
  ungroup() %>% 
  distinct() %>% 
  select(SUBID, Date_0 = date_hype, Return_rate, Source_Type, ts_source )

Year = seq(2000, 2005)
Month <- sprintf("%02d", 1:12)

#final, full gw returns ts


message("🔗Combining all abstraction timeseries")
flush.console()

gw_returned_ts <- expand.grid(Year = Year, month = Month) %>% 
  arrange(Year, month) %>% 
  inner_join(gw_returned_patterns, by = "month", relationship = "many-to-many") %>%
  mutate(Date_0 = as.character(paste0(Year, month)), 
         Return_rate = case_when( Date_0 < Start_Date ~ 0, 
                                  .default = Return_rate)) %>% 
  select(SUBID, Date_0, Return_rate, Source_Type) %>%
  bind_rows(gw_returned_monthly) %>%
  inner_join(PSD_gw_returns, by = c("SUBID", "Source_Type")) %>% 
  group_by(Date_0, PSID) %>% # aggregate to the psid
  summarise(Return_rate = sum(Return_rate)) %>% 
  ungroup() %>%
  mutate(header = paste0("Flow_", PSID)) %>%
  select(Date_0, Return_rate, header) %>% 
  pivot_wider(names_from = header, values_from = Return_rate) %>%
  replace(., is.na(.), 0)

# combine all time series 
all_abs_ts <- inner_join(returns_ts, wrgis_ts, by = "Date_0") %>%
  inner_join(wrgis_ts_gw_returned, by = "Date_0") %>%
  inner_join(gw_returned_ts, by = "Date_0") %>% 
  arrange(Date_0) 


# Extract column names and reformat as 2-line HYPE PSMonthlySeries headers
message("🔡 Formatting for HYPE ")
flush.console()
setDT(all_abs_ts)

cn <- names(all_abs_ts)
headers1 <- sub("_.*", "", cn)
headers2 <- sub(".*_", "", cn)

setnames(all_abs_ts, headers1)

output_df <- rbindlist(
  list(as.list(headers2), all_abs_ts),
  use.names = TRUE
)

# ============================================================
# Save outputs
# ============================================================


# Define output file paths 
output_dir <- here("PointSourceData", "Abstractions", "data_out")

output_rds_file <- here(
  "PointSourceData", "Abstractions", "data_out","Abstractions_PSMonthly_Series.rds"
)

output_txt_file <- here(
  "PointSourceData", "Abstractions", "data_out","Abstractions_PointSourceData.txt"
)

# Remove existing files from previous years 


# Patterns for files to remove (any year)
rds_pattern <- "^Abstractions_PSMonthly_Series_[0-9]{4}\\.rds$"
txt_pattern <- "^Abstractions_PointSourceData_[0-9]{4}\\.txt$"

# Identify files to delete
files_to_delete <- list.files(
  path = output_dir,
  pattern = paste(rds_pattern, txt_pattern, sep = "|"),
  full.names = TRUE
)

# Delete if any exist
if (length(files_to_delete) > 0) {
  file.remove(files_to_delete)
}


# Save RDS
saveRDS(output_df, output_rds_file)

rm(output_df, all_abs_ts)

# Save PointSourceData TXT
abs_psd <- bind_rows(PSD_returns, PSD_wrgis, PSD_gw_returns) %>%
  select(SUBID, PSID, ps_type, ps_source, ts_source)

write_tsv(
  abs_psd,
  output_txt_file,
  append = FALSE)

message(paste0("💾 Abstractions PSMonthlySeries & PSData written to: \n", 
               output_dir))
flush.console()
message("🏁 Script 8 complete!")
flush.console()
# ============================================================
# Declare outputs 
# ============================================================

declared_outputs <- c(
  here(
    "PointSourceData",
    "Abstractions",
    "data_out",
    "Abstractions_PSMonthly_Series.rds"
  ),
  here(
    "PointSourceData",
    "Abstractions",
    "data_out",
    "Abstractions_PointSourceData.txt"
  )
)

declared_outputs

