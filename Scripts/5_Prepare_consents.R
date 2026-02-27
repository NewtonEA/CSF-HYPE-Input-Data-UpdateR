# ============================================================
# Prepare discharge consents for CSF-HYPE
# This script uses the discharge consents to prepare
# quantitative, qualitative and intermittent discharges for
# the PSMonthlySeries HYPE input
#
# Written by Tom Newton 
#  Updated & tested December 2025
#
# ============================================================
message("❯❯❯❯ Preparing consents...")
flush.console()
# ============================================================
# Config
# ============================================================

#load required packages
if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse,
               sf,
               rnrfa,
               here,
               data.table)

message("⚙️Preparing Consents extracts for CSF-HYPE...")
flush.console()

# Check required inputs


required_inputs <- c(
  list.files(here("PointSourceData", "Discharges", "data_out"),
             pattern = "^consent_discharges_used_HYPE_with_consented_means_.*\\.rds$",
             full.names = TRUE),
  here("PointSourceData", "Discharges", "data_in", "default_concentrations_by_industry_type.csv"),
  here("PointSourceData", "Discharges", "data_in", "Fix_wrong_SUBIDs_lut_eco.txt"),
  here("PointSourceData", "Discharges", "data_out", "sites_for_quant_analysis.csv"),
  here("ForcingData", "_forcing_metadata.rds")
)

missing_inputs <- required_inputs[!file.exists(required_inputs)]
if (length(missing_inputs) > 0) {
  stop(
    "❌ Script 5 cannot run. Missing required input files:\n",
    paste(missing_inputs, collapse = "\n")
  )
} else {
  message("✅ Script 5: All required input files are present.")
}


message("❯❯❯❯ Loading consent discharges used by HYPE...")
flush.console()

#set sub directories 
subdir <- here("PointSourceData","Discharges") 
data_out_dir <- file.path(subdir, "data_out")

#load consents to use in HYPE

Final_consents_to_use <-   read_rds(
  file.path(
    data_out_dir,"consent_discharges_used_HYPE_with_consented_means.rds"
    )
  ) %>% 
  # update EFF_SAMPLE_POINT to match the API sample point notation (REGION-SMPT)
  mutate (
    EFF_SAMPLE_POINT = case_when(
      !is.na( 
        EFF_SAMPLE_POINT
        ) ~ paste0(
        EA_REGION, "-", EFF_SAMPLE_POINT
        ),
      .default = EFF_SAMPLE_POINT # preserve NAs
    )
  )
     
# load HYPE defaults lookup 

HYPE_defaults <- read_csv(
  paste0(subdir,"/data_in/default_concentrations_by_industry_type.csv"),
  col_types = c("cnnnnnnnnc")) %>% 
  # replaces non-alphanumeric characters with space
  mutate(Industry_Type =  toupper(str_remove_all(Industry_Type,"[^[:alnum:]]"))) 
  
# ============================================================
# Select consented values,add defaults & convert to HYPE dets
# ============================================================

message("❯❯❯❯ Applying consented values, defaults & converting to HYPE determinands...")
flush.console()

Final_consents_to_use <- Final_consents_to_use %>%
  mutate(Industry_Type = toupper(str_remove_all(
    DSI_TYPE_DESCRIPTION, "[^[:alnum:]]"))) %>%
  # join default by industry type
  inner_join(HYPE_defaults, by=c("Industry_Type"))  %>%
  # ensure all det fields are numeric and date fields are Date 
  mutate(
    across(
      .cols = ends_with("DATE"),
      .fns = as.Date
    ),
    across(
      .cols = matches("_mgl$|_mgl_d$|_m3s$|_m3s_d$|_cfu_ml_d$"),
      .fns = as.numeric
    )
  )

#create SMPT-defaults lookup for numeric sites 

SMPT_defaults <- Final_consents_to_use %>%
  filter(!is.na(EFF_SAMPLE_POINT)) %>% 
  #add EA region ID to sample point to align with API smpt notation
  mutate(EFF_SAMPLE_POINT = paste0(EA_REGION, "-", EFF_SAMPLE_POINT)) %>%
  select(EFF_SAMPLE_POINT,Flow_m3s_d:T1_cfu_ml_d) %>% 
  distinct(EFF_SAMPLE_POINT, .keep_all = T) %>% 
  write_rds(paste0(subdir, "/data_out/HYPE_EFF_SMPT-Default_lut.rds"))


  
Final_consents_to_use <- Final_consents_to_use %>%  
  #select consented values
  mutate(IN_mgl = `111_NH3-N_mgl`+ `116_TN_mgl`, 
         ON_mgl = IN_mgl/9,
         SP_mgl = `180_OP_mgl`,
         TP_mgl = `348_TP_mgl`,
         SS_mgl = `135_SS_mgl`,
         #add default values where no consented values
         # or where consented value is 0 or less (as some are)
         Flow_m3s = case_when(is.na(Flow_m3s) ~ Flow_m3s_d,
                              Flow_m3s <= 0 ~ Flow_m3s_d,
                              T ~ Flow_m3s),
         IN_mgl = case_when(is.na(IN_mgl) ~ IN_mgl_d,
                            IN_mgl <= 0 ~ IN_mgl_d,
                            T ~ IN_mgl),
         ON_mgl = case_when(is.na(ON_mgl) ~ ON_mgl_d,
                            ON_mgl <= 0  ~ ON_mgl_d,
                            T ~ ON_mgl),
         #for TP there are two dets: 0348 and 0192
         #0348 is preferred, if no 0348 then use the old det 0192
         #and if no 0192 then use default
         TP_mgl = case_when(is.na(TP_mgl) ~ `192_TIP_mgl`,
                            T ~ TP_mgl),
         # PP = TP - SP, so create PP for those consented for TP & SP
         #before adding defaults to TP
         PP_mgl =  TP_mgl - SP_mgl,
         TP_mgl = case_when(is.na(TP_mgl) ~ TP_mgl_d,
                            TP_mgl <= 0 ~ TP_mgl_d,
                            T ~ TP_mgl),
         #now add default PP 
         PP_mgl = case_when(is.na(PP_mgl) ~ PP_mgl_d,
                            PP_mgl <= 0 ~ PP_mgl_d,
                            T ~ PP_mgl),
         SP_mgl = case_when(is.na(SP_mgl) ~ SP_mgl_d,
                            SP_mgl <= 0 ~ SP_mgl_d,
                            T ~ SP_mgl),
         SS_mgl = case_when(is.na(SS_mgl) ~ SS_mgl_d,
                            SS_mgl <=0 ~ SS_mgl_d,
                            # some consents in the database have extreme limits for SS
                            SS_mgl > 250 ~ 100, # reduce to 100 mgl as advised by Linda
                            T ~ SS_mgl),
         # discharges are never consented for FIOs so add defaults
         # for selected discharge types and 0s for others
         T1_cfu_ml = T1_cfu_ml_d) %>% 
  select(SUBID,COMPANY_NAME:EFF_SAMPLE_POINT,Flow_m3s,
         IN_mgl, ON_mgl, SP_mgl, PP_mgl, TP_mgl, 
         SS_mgl, T1_cfu_ml, geometry) %>% 
  distinct()
rm(HYPE_defaults)

# ============================================================
# Separate intermittent from constant discharges 
# ============================================================
message("❯❯❯❯ Separating intermittent and constant discharges...")
flush.console()


int_types <- c("Pumping Station on Sewerage Network (water company)",
               "Pumping Stn on unadopted sewerage netwrk (not water co)",
               "CSO on unadopted sewerage network (not water company)",
               "Storm Tank CSO on Sewerage Network (water company)",
               "Sewerage Network CSO (water company)",
               "Sewerage Network Pumping Station (water company)") %>%
  toupper() %>% str_remove_all("[^[:alnum:]]")


intermittents <- Final_consents_to_use %>% 
  #create a new Industry Type variable changing everything to uppercase
  #and remove all non alphabetical characters to ensure accurate filtering
  mutate(Industry_Type = toupper(str_remove_all(
                                DSI_TYPE_DESCRIPTION,"[^[:alnum:]]"))) %>%
  filter(Industry_Type %in% int_types |
           EFFLUENT_TYPE %in% c("SB", "SC", "SD","DA",
                                "TI", "UB", "UC", "UD")| 
           DISCHARGE_SITE_TYPE_CODE %in% c("A3","A4","A8","A7",
                                           "SB", "SC", "SD","DA",
                                           "TI", "UB", "UC", "UD")) %>%
  write_csv(.,
            file.path(
              data_out_dir, "All_intermittent_consents.csv"
              )
            )


message("✔️ Intermittent discharges identified and exported...")
flush.console()

constants <- Final_consents_to_use %>% 
  anti_join(intermittents) %>%
  # a small number of permits have different version numbers
  # but all other unique PS identifiers are the same
  # probably a change of address or something minor
  distinct(., SUBID, PERMIT_NUMBER, EFFECTIVE_DATE,
           REVOCATION_DATE,EFFLUENT_NUMBER, OUTLET_NUMBER, 
           EFF_SAMPLE_POINT, .keep_all = T) 
rm(Final_consents_to_use, intermittents)

# ============================================================
# flag sites & dets that will be quantitative 
# using the output of the Update observation data script 
# ============================================================

#load WIMS to WISKI lookup & quant_PS_lists
# process into quantitative site/det lookup


quant_PS_list <- read_csv( 
  file.path(
    data_out_dir, "sites_for_quant_analysis.csv"
    ),
  col_types = c(.default="c")
  ) 


#flag numeric point-sources
quant_sitedet_lookup <-  quant_PS_list %>%
  mutate(Quant_det  = MEAS_DETERMINAND_CODE, 
         DET = case_when(
           MEAS_DETERMINAND_CODE %in% c("0111", "0116") ~ "IN_mgl",#ON quant too
           MEAS_DETERMINAND_CODE %in% c("0348", "0192") ~ "TP_mgl",#SP&PP quant
           MEAS_DETERMINAND_CODE == "0180" ~ "SP_mgl",#PP is quant too
           MEAS_DETERMINAND_CODE == "2348" ~ "T1_cfu_ml",
           MEAS_DETERMINAND_CODE == "0135" ~ "SS_mgl",
           MEAS_DETERMINAND_CODE == "Flow_m3s" ~ "Flow_m3s",
           T ~ "not_used"))%>% #other dets not currently used in quant analysis
  select(EFF_SAMPLE_POINT = SMPT_USER_REFERENCE, DET) %>%
  filter(DET !="not_used")%>%
  mutate(numeric = "Y") %>%
  distinct()

quant_PS_list <- quant_PS_list %>% 
  select(SMPT_USER_REFERENCE, quant_for) %>% 
  distinct()

# ============================================================
#  flag & split the numeric discharges (those with effluent sampling) 
# from descriptive (those with only consented values and/or defaults)
# and assign PSIDs
# ============================================================

constants <- constants %>%
  pivot_longer(.,Flow_m3s:T1_cfu_ml, names_to = "DET", values_to = "Value") %>%
  left_join(quant_sitedet_lookup) %>% 
  #now set all numeric flagged consent/det combination to NA
  mutate(Value = case_when(
    DET == "IN_mgl" & numeric == "Y" ~ NA_real_, 
    DET == "TP_mgl" & numeric == "Y" ~ NA_real_, 
    DET == "SP_mgl" & numeric == "Y" ~ NA_real_, 
    DET == "SS_mgl" & numeric == "Y" ~ NA_real_,
    DET == "T1_cfu_ml" & numeric == "Y" ~ NA_real_,
    T ~ Value)) %>% 
  select(-numeric) %>% 
  pivot_wider(names_from = DET, values_from = Value) %>%
  # set calculated dets to NA where the det they are calculated from are quant
  mutate(SP_mgl = case_when(is.na(TP_mgl) ~ NA_real_,
                            T ~ SP_mgl),
         PP_mgl = case_when(is.na(SP_mgl) ~ NA_real_,
                            T ~ PP_mgl),
         ON_mgl = case_when(is.na(IN_mgl) ~ NA_real_, 
                            T ~ ON_mgl),
         TP_mgl = case_when(is.na(SP_mgl) & is.na(PP_mgl) ~ NA_real_,
                            T ~ TP_mgl)) %>% 
  left_join(quant_PS_list, by=c("EFF_SAMPLE_POINT" = "SMPT_USER_REFERENCE")) %>% 
  #finally flag PS_type numeric = 1 and descriptive = 2
  mutate(ps_type = case_when(is.na(quant_for)~2, T ~ 1))

# now separate numeric point sources and ensure any non-quant dets are given
# zero concentrations as their values come through consented/default route

numeric <- constants %>% 
  filter(ps_type == 1) %>%
  mutate(across(IN_mgl:T1_cfu_ml, ~ case_when (. !='null' ~ 0))) %>% 
  select(SUBID,EA_REGION, PERMIT_NUMBER, EFFECTIVE_DATE, REVOCATION_DATE,
         EFF_SAMPLE_POINT:ps_type, geometry) %>% 
  #unique PSID for quantitative sources
  group_by(SUBID, EA_REGION,EFF_SAMPLE_POINT) %>% 
  mutate(EFFECTIVE_DATE = min(EFFECTIVE_DATE),
         REVOCATION_DATE = max(REVOCATION_DATE),
         # for eff pts w/o wiski take mean of default/consented values for Flow
         Flow_m3s = mean(Flow_m3s, na.rm = T),
         #and then set flows for PSIDs with WISKI sites to NA
         Flow_m3s = case_when(str_detect(quant_for, "Flow") ~ NA_real_,
                              T ~ Flow_m3s),
         # sometimes an EFF SMPT is linked to multiple permits
         PERMIT_NUMBER = toString(list(unique(PERMIT_NUMBER)))) %>% 
  ungroup() %>%
  distinct(across(-geometry), .keep_all = TRUE) %>% # the precision of geometry changes through some versions causing duplication 
  arrange(SUBID) %>%
  mutate(PSID = seq(1,nrow(.)))

## 10/03/23 quick fix to put numeric PSIDs where the OUTLET_NGR is wrong or 
# imprecise into the correct SUBID based on the WIMS site location

#load fix wrong SUBIDs lookup 

SUBID_fix_lut <- read_tsv(paste0(subdir, 
                                 "/data_in/Fix_wrong_SUBIDs_lut_eco.txt")) 

#join numeric data to the SUBID fix lookup

numeric_fix <- numeric %>% 
  select(-SUBID) %>%
  inner_join(SUBID_fix_lut, by = c("EA_REGION","EFF_SAMPLE_POINT" = "SMPT_REF"),
             relationship = "many-to-many") 

numeric <- anti_join(numeric, numeric_fix, by = c("EA_REGION","EFF_SAMPLE_POINT")) %>%
  bind_rows(numeric) %>% 
  distinct(across(EA_REGION:PSID), .keep_all = T) %>%
  arrange(PSID) ; rm(numeric_fix)

#write to data_out folder 

write_tsv(numeric,
          file.path(
            data_out_dir, "HYPE_point_sources_for_quantitative_analysis.txt"
            )
          )

# ============================================================
# create the qualitative/descriptive TS
# ============================================================

# 'numeric' dets have NAs & not counted when descriptive points are aggregated
descriptive <- constants %>% 
  filter(ps_type == 2) %>% # only permits with no numeric dets
   #convert dates to HYPE yyyymm format
  mutate(EFFECTIVE_DATE = as.numeric(strftime(EFFECTIVE_DATE, "%Y%m")),
         REVOCATION_DATE = as.numeric(strftime(REVOCATION_DATE, "%Y%m"))) %>%
  select(SUBID, PERMIT_NUMBER, EFFECTIVE_DATE,
         REVOCATION_DATE,OUTLET_NUMBER, Flow_m3s:T1_cfu_ml) %>%
  # check number of outlets per permit
  group_by(PERMIT_NUMBER) %>%
  mutate(n_out = n_distinct(OUTLET_NUMBER)) %>%
  ungroup() %>%
 # now aggregate to outlets/permit
  group_by(SUBID,PERMIT_NUMBER, OUTLET_NUMBER, n_out) %>%  # unqiue PS identifier for qualitative PS
  # aggregates versions, effluents and outlets to permit
  mutate(Flow_m3s = (Flow_m3s / n_out), # divide consented flow among the outlets
         across(IN_mgl:T1_cfu_ml, ~ mean(., na.rm = TRUE)), # average the concentrations
         EFFECTIVE_DATE = min(EFFECTIVE_DATE, na.rm = TRUE),
         REVOCATION_DATE = max(REVOCATION_DATE, na.rm = TRUE)) %>%
  ungroup() %>% 
  distinct() %>%
  # set EFFECTIVE_DATE to 200001 for consents effective before 2000
  mutate(EFFECTIVE_DATE = case_when(EFFECTIVE_DATE < 200001 ~ 200001,
                                    T ~ EFFECTIVE_DATE),
         # and make this a date_hype variable to join on later
         date_hype = EFFECTIVE_DATE,
         # assign temporary PSIDs to flag unique discharges at next aggregation 
         PSID = seq(1,nrow(.),1),
         PSID = PSID + max(numeric$PSID)) %>%
  
  pivot_longer(Flow_m3s:T1_cfu_ml, names_to = "DET", values_to = "Value") %>%
  distinct()
rm(constants)

# ============================================================
#create lookup table of PSID SUBID effective and revocation dates 
# ============================================================
qual_lut <- descriptive %>% 
  select(PSID, SUBID, PERMIT_NUMBER, EFFECTIVE_DATE, REVOCATION_DATE,DET, Value) %>%
  distinct()

# ============================================================
# Build qualitative PSMonthlySeries
# ============================================================


#TBD: rewrite in data.table 

message("🛠️ Constructing qualitative PSMonthlySeries time series...")
flush.console()

# reformat to wide data with hype_date as id col
descriptive <- descriptive %>%
  select(-c(PERMIT_NUMBER,SUBID,EFFECTIVE_DATE, REVOCATION_DATE, OUTLET_NUMBER)) %>%
  pivot_wider(names_from = c(PSID,DET), values_from = Value) %>%
  arrange(date_hype)

# create a complete timeseries of HYPE dates 

forcing_meta <- read_rds(here("ForcingData", "_forcing_metadata.rds"))
start_date <- as.Date("2000-01-01")
end_date   <- forcing_meta$latest_forcing_date

hype_PSquali_TS<- data.frame(date_hype = seq(start_date, 
                                             end_date, "month")) %>%
  mutate(date_hype = as.numeric(strftime(date_hype, "%Y%m"))) %>% 
  #join the descriptive PSIDs
  left_join(descriptive, by = "date_hype") %>%
  #convert to long format
  pivot_longer(-date_hype, names_to = "PSID_DET", values_to = "Value") %>%
  #extract PSID and DET back to separate variables
  mutate(PSID = as.numeric(str_extract(PSID_DET, "^\\d+")),
         DET  = str_extract(PSID_DET, "(?<=_).*")) %>%
  select(-Value) %>%
  # join qual_lut by PSID & DET
  inner_join(qual_lut, by=c("PSID","DET")) %>%
  group_by(PSID, DET) %>%
  # fill values between EFFECTIVE_DATE and REVOCATION date 
  # set anything outside of the EFFECTIVE_DATE to REVOCATION_DATE range to 0 
  mutate(Value = case_when(date_hype < EFFECTIVE_DATE ~ 0,
                           date_hype > REVOCATION_DATE ~ 0,
                           T ~ Value)) %>% 
  ungroup() %>%
  select(-c(EFFECTIVE_DATE, REVOCATION_DATE)) %>%
  #aggregate to SUBID and assign final PSID
  group_by(date_hype, SUBID, DET) %>%
  mutate(Value = ifelse(
    DET == "Flow_m3s", 
    sum(Value, na.rm = TRUE), 
    ifelse(is.nan(mean(Value[Value != 0], na.rm = TRUE)),
           0, mean(Value[Value != 0], na.rm = TRUE)))) %>% # Exclude 0s when calculating mean as they are dates outside the active period for a given permit, so shouldn't be counted.  
  ungroup() %>% 
  arrange(SUBID) 


# create final lookup of SUBID to PSID to add to PS Master Lookup

qual_lut <- data.frame(SUBID = unique(hype_PSquali_TS$SUBID))%>% 
  mutate(PSID = seq(1,nrow(.),1),
         PSID = PSID + max(numeric$PSID))

# Transform to HYPE PSMonthlySeries format

hype_PSquali_TS <- hype_PSquali_TS %>%
  select(date_hype, DET, Value, SUBID) %>%
  inner_join(qual_lut, by = "SUBID")%>%
  #rename dets to HYPE format
  mutate(DET = case_when(DET == "Flow_m3s" ~ "Flow", T ~ DET),
         DET = str_replace_all(DET, c("_mgl" = "conc", "_cfu_ml" = "conc"))) %>%
  ungroup() %>%
  distinct() %>%
  select(-SUBID) %>%
  pivot_wider(names_from = c(PSID,DET), values_from = Value) %>%
  #rename date_hype to 0_Date so headers are formatted correctly for HYPE
  rename("0_Date" = date_hype) %>%
  # convert everything to character to allow bind_rows to work below
  mutate(across(everything(), as.character))

#extract column names and reformat as 2-line HYPE PSMonthlySeries headers

setDT(hype_PSquali_TS)
col_names <- names(hype_PSquali_TS)
headers1 <- sub("^\\d+_|_", "", col_names)
headers2 <- sub("_.*", "", col_names)
hype_headers <- as.list(headers2)
hype_PSquali_TS <- rbindlist(
  list(as.data.table(hype_headers), hype_PSquali_TS),
  use.names = FALSE
)
setnames(hype_PSquali_TS, headers1)

#save to data_out folder
write_rds(
  hype_PSquali_TS,
    file.path(
      data_out_dir, "Qualitative_PSMonthlySeries.rds"
      )
  )
message("✔️ 💾 Qualitative PSMonthlySeries created and saved...")
flush.console()

# ============================================================
# create lookup of all numeric and descriptive PSIDs for HYPE Master Lookup
# ============================================================

quant_lut <- numeric %>% 
  select(PSID,SUBID, ps_type, EFF_SAMPLE_POINT,PERMIT_NUMBER,
         quantitative_dets = quant_for, geometry) %>%
  mutate(ts_source = "Numeric",
         X = str_sub(geometry,3,8),
         Y = str_sub(geometry,11,16), 
         #remove brackets
         X = str_remove_all(X, "\\)|\\("),
         Y = str_remove_all(Y,"\\)|\\(" ))

qual_lut <- qual_lut %>% 
  mutate(ps_type = 2,
         EFF_SAMPLE_POINT = NA_character_,
         PERMIT_NUMBER = NA_character_,
         quantitative_dets = NA_character_,
         ts_source = "Descriptive") %>%
  select(PSID, SUBID, ps_type:ts_source)

# write output

WQ_PSData_lut <- bind_rows(
  quant_lut, qual_lut
  ) %>%
  write_tsv(.,
            file.path(
              data_out_dir, "WQ_PSData_lut.txt"
              ),
            append = FALSE
            )
# done
message("✅ Consents prepared!")
flush.console()


declared_outputs <- c(
  here("PointSourceData", "Discharges", "data_out", "HYPE_point_sources_for_quantitative_analysis.txt"),
  here("PointSourceData", "Discharges", "data_out", "All_intermittent_consents.csv"),
  here("PointSourceData", "Discharges", "data_out", "Qualitative_PSMonthlySeries.rds"),
  here("PointSourceData", "Discharges", "data_out", "WQ_PSData_lut.txt"),
  here("PointSourceData", "Discharges", "data_out", "HYPE_EFF_SMPT-Default_lut.rds")
)

declared_outputs
  
         