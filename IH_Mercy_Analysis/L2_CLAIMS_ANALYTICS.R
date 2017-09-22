############ Level 2 #############

### Reads in raw medical claims information,
### de-identifies phi with id_bridge
### formats claims_analytics

### Input Tables: raw_med_claims
###               id_bridge
###               human_flags

### Output Tables: claims_analytics

### Author: Michelle Powell

### Sourced By: claims_analytics()


###########################################
###########################################

##### Column Parameters #####
claims_cpt_code                   <- NA
claims_rev_code                   <- NA
claims_ip_proc_code               <- NA
claims_icd_version                <- NA
claims_billed_date                <- NA
claims_diagnosis_code             <- "Dx Code"
claims_place_service              <- NA
claims_type_service               <- NA
claims_IP_OP                      <- 'Procedure Type'
claims_primary_analysis_amount    <- 'Allowed Amount'
claims_secondary_analysis_amount  <- 'Paid Amount'
claims_deductible_amount          <- NA
claims_coinsurance_amount         <- NA
claims_copay_amount               <- NA

claims_claim_id                   <- "Procedure ID"
claims_claim_line                 <- NA
claims_service_start              <- "Procedure Date"
claims_service_end                <- NA

claims_member_id                  <- NA
claims_last                       <- "Patient Last Name"
claims_first                      <- "Patient First Name"
claims_dob                        <- "Patient Birth Date"
claims_sex                        <- "Patient Gender"
claims_address_1                  <- "Patient Street Address1"
claims_address_2                  <- "Patient Street Address2"
claims_city                       <- "Patient City"
claims_state                      <- "Patient State"
claims_zip                        <- "Patient Zip Code"
claims_patient_relationship       <- "Patient Relationship"

##### Functions #####

format_sex<-function(x, male_start = ".*M.*",female_start = ".*F.*",ignore.case = FALSE){
  y<-as.character(x)
  y<-gsub(pattern = male_start, replacement = "0", x = y, ignore.case = ignore.case)
  y<-gsub(pattern = female_start, replacement = "1", x = y, ignore.case = ignore.case)
  y<-as.numeric(y)
  return(y)
}

format_dates<-function(date_col, format = '%m/%d/%y', latest = "2017-02-02"){
  date_col<-as.Date(date_col, format = format)
  date_col<-as.Date(ifelse(date_col > latest, format(date_col, "19%y-%m-%d"), format(date_col)))
  return(date_col)
}

format_address <- function(address){
  shortcuts <- read_csv(paste0(directory, 'Data/Fixed_Tables/postal_shortcuts.csv')) %>%
    filter(Long != Abbreviation)
  address <-  gsub("[[:punct:]]", "", gsub("-", " ", address))
  address <-  gsub(" apt ", " ", gsub(" unit ", " ", address))
  for(i in 1:length(shortcuts$Long)){
    address <- gsub(paste0(" ",tolower(shortcuts$Long[i])," "), 
                    paste0(" ",tolower(shortcuts$Abbreviation[i])," "),
                    address, ignore.case = TRUE)
    address <- gsub(paste0(" ",tolower(shortcuts$Long[i]),"$"), 
                    paste0(" ",tolower(shortcuts$Abbreviation[i])),
                    address, ignore.case = TRUE)
  }
  return(address)
}

source('../Coding_Logics/ICD_CLASS_LOGIC.R')

source('../Coding_Logics/PREVENTIVE_CARE_LOGIC.R')

source('../Pipeline/L2_IH_DEID.R')

##### Read and Trim Raw Data #####

claims_cols<-c(claims_claim_id, claims_claim_line, claims_cpt_code, claims_rev_code, claims_ip_proc_code,  
               claims_icd_version, claims_diagnosis_code, claims_place_service, claims_type_service, claims_IP_OP,  
               claims_primary_analysis_amount, claims_secondary_analysis_amount, claims_deductible_amount, 
               claims_coinsurance_amount, claims_copay_amount, claims_service_start, claims_service_end,
               claims_billed_date, claims_member_id, claims_last, claims_first, claims_dob, claims_sex,
               claims_address_1, claims_address_2, claims_city, claims_state, claims_zip, claims_patient_relationship)

new_cols<-c("claim_id", 'claim_ln', 'cpt', 'op_rev', 'ip_proc', 'icd_version', 'icd', 'place_service',
            'type_service', 'IP_OP', 'primary_amount', 'secondary_amount', 'deductible', 'coinsurance', 
            'copay', 'start_dt', 'end_dt', 'bill_dt', 'insurer_id', 'last', 'first', 'dob', 'sex',
            'address_1', 'address_2', 'city', 'state', 'zip', 'emp_sp') 


data_raw <- read_csv(paste0(directory, "Data/Raw/raw_claims1.csv"), col_types = cols(.default = "c")) %>%
  union_all(read_csv(paste0(directory, "Data/Raw/raw_claims2.csv"), col_types = cols(.default = "c"))) %>%
  mutate('Paid Amount' = coalesce(`Approved Amount`, `Paid Amount`))

claims_raw<-data_raw[match(claims_cols[!is.na(claims_cols)],colnames(data_raw))]
colnames(claims_raw)<-new_cols[!is.na(claims_cols)]

rm('claims_cpt_code', 'claims_rev_code', 'claims_ip_proc_code', 'claims_icd_version', 'claims_billed_date', 
   'claims_diagnosis_code', 'claims_place_service', 'claims_type_service', 'claims_IP_OP', 'claims_secondary_analysis_amount', 
   'claims_claim_id', 'claims_claim_line', 'claims_service_start', 'claims_service_end', 'claims_member_id',
   'claims_last', 'claims_first', 'claims_dob', 'claims_sex', 'claims_primary_analysis_amount', 'claims_coinsurance_amount',
   'claims_copay_amount', 'claims_deductible_amount', 'claims_address_1', 'claims_address_2', 'claims_city', 
   'claims_patient_relationship', 'claims_state', 'claims_zip')

census_id_bridge <- read_csv(paste0(directory, 'Data/Sub_Tables/id_bridge.csv'))

##### Deidentify Data #####

claims_raw <- claims_raw %>% mutate('address' = ifelse(is.na(address_2), tolower(address_1),
                                                       do.call(paste, list(tolower(address_1), tolower(address_2)))),
                                    'last' = gsub("[[:punct:]]", "", gsub("-", " ", tolower(last))),
                                    'first' = gsub("[[:punct:]]", "", gsub("-", " ", tolower(first))),
                                    'sex' = as.numeric(sex), 
                                    'dob' = format_dates(dob),
                                    'city' = tolower(city),
                                    'state' = tolower(state),
                                    'zip' = str_sub(zip, 1, 5),
                                    'emp_spouse' = case_when(.$emp_sp == "SUBSCRIBER" ~ "e",
                                                             .$emp_sp == "SPOUSE" ~ "s",
                                                             .$emp_sp == 'DEPENDENT' ~ 'd')) %>% filter(emp_spouse != 'd') %>%
  mutate(address = format_address(address)) %>%
  select(-address_1, -address_2, -emp_sp)

claims_raw$ID <- 1:length(claims_raw$claim_id)

claims_raw_deid <- ih_deidentify(data = claims_raw, 
                                 data_id = 'ID',
                                 census_id_bridge = census_id_bridge, 
                                 census_id = 'master_id',
                                 id_match = FALSE, 
                                 pii_match = TRUE, 
                                 fuzzy_match = TRUE)

##### Format Data #####

claims <- claims_raw_deid %>%  
  mutate('start_dt' = mdy(start_dt)) %>%
  filter(start_dt <= analysis_end,
         start_dt >= analysis_start) %>%
  transmute('master_id' = master_id,
            'claim_id' = as.character(claim_id),
            'icd_code' = as.character(icd),
            'icd_10' = case_when(grepl('[[:alpha:]]', str_sub(.$icd, 1, 1), TRUE) &
                                   (str_sub(.$icd, 1, 1) != 'V' |
                                      grepl('[[:alpha:]]', str_sub(.$icd, -1), TRUE)) ~ .$icd),
            'icd_9' = case_when(grepl('[[:digit:]]', str_sub(.$icd, 1, 1), TRUE) |
                                  (str_sub(.$icd, 1, 1) == 'V' & 
                                     grepl('[[:digit:]]', str_sub(.$icd, -1), TRUE)) ~ .$icd),
            'er_flag' = as.numeric(IP_OP %in% c("AMBULANCE AIR/WATER", "AMBULANCE LAND", "EMERG RM HOSPITAL")),
            'IP_OP' = ifelse(IP_OP %in% c("INPATIENT REHAB FACILITY/COMPREHENSIVE", "AMBULANCE LAND",
                                               "INPATIENT PSYCH FACILITY", "INPATIENT HOSPITAL",
                                               "EMERG RM HOSPITAL", "AMBULANCE AIR/WATER"), "IP", "OP"),
            'secondary_amount' = as.numeric(gsub("\\(","-", gsub('[\\$,\\)]', '', gsub('-', '0', secondary_amount)))),
            'primary_amount' = as.numeric(gsub("\\(","-", gsub('[\\$,\\)]', '', gsub('-', '0', primary_amount)))),
            'start_dt' = start_dt) %>% distinct() %>%
  mutate('prev_care' = prev_care(icd_10, icd_9, "X"),
         'icd_category' = icd10_interpret(icd_10)) %>% distinct() %>%
  mutate('icd_category_class' = icd_category_class(icd_category),
         'preventive' = as.numeric(!is.na(prev_care)),
         'non_preventable' = ifelse(icd_category %in% c('maternal care without complications', 'reproductive management', 
                                                        'uncomplicated delivery', 'inherited malformations', 
                                                        'injury and poison'),1,0)) %>%
  left_join(assign_table("human_flags_tab", "Data/Sub_Tables/human_flags.csv"), by = c("master_id" = "master_id")) %>% 
  filter(start_dt <= cov_end_dt,
         start_dt >= cov_start_dt) %>%
  select(master_id, claim_id, IP_OP, icd_category, icd_category_class, preventive, non_preventable, 
         er_flag, start_dt, primary_amount, secondary_amount, icd_code) 

##### Write Data #####

write_csv(claims, paste0(directory, "Data/Sub_Tables/claims_analytics.csv"))
print("claims_analytics written to Data/Sub_Tables")

claims_tab <- claims %>% ungroup()

rm("claims", "claims_cols", "new_cols", "format_sex", "format_dates", "icd10_interpret", 'prev_care', 'census_id_bridge', 'claims_raw', 
   'claims_raw_deid', 'data_raw', 'category_clean', 'icd_category_class', 'format_address')
          