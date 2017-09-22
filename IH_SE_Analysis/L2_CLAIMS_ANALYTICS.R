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
claims_cpt_code                   <- "HCPCS CPT Code"
claims_rev_code                   <- "Revenue Code"
claims_ip_proc_code               <- "ICD Procedure Code 1"
claims_icd_version                <- "ICD Version Code"
claims_billed_date                <- 'Received Date'
claims_diagnosis_code             <- "Diagnosis Code 1"
claims_place_service              <- "Place of Treatment"
claims_type_service               <- "Type of Service"
claims_IP_OP                      <- 'Claim Reporting Category'
claims_primary_analysis_amount    <- NA
claims_secondary_analysis_amount  <- 'Paid Amount'
claims_deductible_amount          <- 'Deductible Amount'
claims_coinsurance_amount         <- 'Coinsurance Amount'
claims_copay_amount               <- 'Copay Amount'

claims_claim_id                   <- "Claim Number"
claims_claim_line                 <- "Line Number"
claims_service_start              <- "First Date of Service"
claims_service_end                <- "Last Date of Service"

claims_member_id                  <- "Member Key"
claims_last                       <- "Patient Last Name"
claims_first                      <- "Patient First Name"
claims_dob                        <- "Patient DOB"
claims_sex                        <- "Patient Gender"

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

source('../Coding_Logics/ICD_CLASS_LOGIC.R')

source('../Coding_Logics/PREVENTIVE_CARE_LOGIC.R')

source('../Coding_Logics/BCBS_PLACE_OF_SERVICE.R')

source('../Pipeline/L2_IH_DEID.R')

##### Read and Trim Raw Data #####

claims_cols<-c(claims_claim_id, claims_claim_line, claims_cpt_code, claims_rev_code, claims_ip_proc_code,  
               claims_icd_version, claims_diagnosis_code, claims_place_service, claims_type_service, claims_IP_OP,  
               claims_primary_analysis_amount, claims_secondary_analysis_amount, claims_deductible_amount, 
               claims_coinsurance_amount, claims_copay_amount, claims_service_start, claims_service_end,
               claims_billed_date, claims_member_id, claims_last, claims_first, claims_dob, claims_sex)

new_cols<-c("claim_id", 'claim_ln', 'cpt', 'op_rev', 'ip_proc', 'icd_version', 'icd', 'place_service',
            'type_service', 'IP_OP', 'primary_amount', 'secondary_amount', 'deductible', 'coinsurance', 
            'copay', 'start_dt', 'end_dt', 'bill_dt', 'insurer_id', 'last', 'first', 'dob', 'sex') 

claims_rows <- read_csv(paste0(directory, "Data/Fixed_Tables/raw_claims_dd.csv"), col_types = cols(.default = "c"))$`Field Name`
data_raw <- read_tsv(paste0(directory, "Data/Raw/raw_claims.txt"), col_types = cols(.default = "c"), col_names = claims_rows) 

claims_raw<-data_raw[match(claims_cols[!is.na(claims_cols)],colnames(data_raw))]
colnames(claims_raw)<-new_cols[!is.na(claims_cols)]

rm('claims_cpt_code', 'claims_rev_code', 'claims_ip_proc_code', 'claims_icd_version', 'claims_billed_date', 
   'claims_diagnosis_code', 'claims_place_service', 'claims_type_service', 'claims_IP_OP', 'claims_secondary_analysis_amount', 
   'claims_claim_id', 'claims_claim_line', 'claims_service_start', 'claims_service_end', 'claims_member_id',
   'claims_last', 'claims_first', 'claims_dob', 'claims_sex', 'claims_primary_analysis_amount', 'claims_coinsurance_amount',
   'claims_copay_amount', 'claims_deductible_amount')

census_id_bridge <- read_csv(paste0(directory, 'Data/Sub_Tables/id_bridge.csv'))

##### Deidentify Data #####

claims_raw_deid <- ih_deidentify(data = claims_raw, data_id = 'insurer_id',
                                 census_id_bridge = census_id_bridge, census_id = 'insurer_id',
                                 id_match = TRUE, pii_match = FALSE, fuzzy_match = FALSE)

##### Format Data #####

set.seed(123456789)
claims <- claims_raw_deid %>% 
  mutate_all(funs(replace(., which(.%in% c('~', '!', '!.', '', ' ')), NA))) %>%
  transmute('master_id' = master_id,
            'claim_id' = as.character(claim_id),
            'claim_line' = as.numeric(claim_ln),
            'proc_code' = coalesce(as.character(trimws(cpt)), 
                             as.character(paste0('U', str_sub(trimws(op_rev), 2)))),
            'ip_proc_icd9v3' = case_when(.$icd_version == '09' & !is.na(.$ip_proc) ~ paste0(str_sub(.$ip_proc, 1, 2),'.',str_sub(.$ip_proc, 3)),
                                         .$icd_version == '09' ~ .$ip_proc, 
                                         TRUE ~ as.character(NA)),
            'ip_proc_icd10pcs' = case_when(.$icd_version == '10' ~ .$ip_proc, 
                                           TRUE ~ as.character(NA)), 
            'icd_10' = case_when(.$icd_version == '10' ~ paste0(str_sub(.$icd, 1, 3),'.',str_sub(.$icd, 4))),
            'icd_9' = case_when(.$icd_version == '09' & str_sub(.$icd,1,1) == 'E' ~ paste0(str_sub(.$icd, 1, 4),'.',str_sub(.$icd, 5)),
                                .$icd_version == '09' ~ paste0(str_sub(.$icd, 1, 3),'.',str_sub(.$icd, 4))),
            'place_service' = bcbs_place_of_service_decode(place_service),
            'type_service' = case_when(.$type_service %in% c('94', '95', '03') ~ 'ambulance',
                                       .$type_service %in% c('15', '14') ~ 'ER',
                                       TRUE ~ as.character(NA)),
            'IP_OP' = ifelse(is.na(IP_OP) & is.na(ip_proc), 'OP', 
                             case_when(.$IP_OP == '1' ~ 'IP',
                                .$IP_OP == '2' ~ 'OP',
                                !is.na(.$ip_proc) ~ 'IP',
                                !is.na(.$master_id) ~ 'OP')),
            'secondary_amount' = as.numeric(secondary_amount),
            'primary_amount' = as.numeric(deductible) + as.numeric(coinsurance) + as.numeric(copay) + as.numeric(secondary_amount),
            'start_dt' = mdy(start_dt),
            'end_dt' = mdy(end_dt),
            'bill_dt' = mdy(bill_dt)) %>%
  mutate('icd_code' = coalesce(icd_10, icd_9)) %>% distinct()

#### Trim, Format, Create, Order Fields #####
          
ER_claim_ids <- claims %>% filter(proc_code %in% c("99281", "99282", "99283", "99284", "99285", "U450", "A0021", "U981") | 
                                    (str_sub(proc_code, 1, 2) == 'A0' & as.numeric(str_sub(proc_code, 3, 5)) > 224) | 
                                    (type_service %in% c('ambulance', 'ER')) | 
                                    (place_service %in% c('ambulance', 'ER'))) %>% distinct(claim_id)

claims <- claims %>% 
  group_by(master_id, claim_id, claim_line) %>% 
  mutate('max_bill' = max(bill_dt),
         'min_bill' = min(bill_dt),
         'primary_amount' = sum(primary_amount),
         'secondary_amount' = sum(secondary_amount)) %>%
  filter(bill_dt == max_bill,
         primary_amount > 0,
         start_dt >= analysis_start,
         start_dt < analysis_date) %>% ungroup() %>% distinct() %>%
  mutate('prev_flag' = prev_care(icd_10, icd_9, proc_code),
         'prev_id' = paste0(claim_id, coalesce(icd_10, icd_9)),
         'bill_dt' = min_bill) %>%
  select(-min_bill, -max_bill)

claims_prev <- claims %>% filter(!is.na(prev_flag)) %>% ungroup()
claims_non_prev <- claims %>% filter(is.na(prev_flag)) %>% ungroup()

claims_prev_also <- claims_non_prev %>% 
  filter(str_sub(icd_10, 1, 1) %in% c("Z", "O") | 
           str_sub(icd_9, 1, 1) == "V" |
           str_sub(icd_9, 1, 2) %in% c('63', '64', '65', '66', '67'),
         prev_id %in% claims_prev$prev_id)

claims_prev <- claims_prev %>% union_all(claims_prev_also) %>% 
  mutate('preventive' = 1)
claims_non_prev <- claims_non_prev %>% anti_join(claims_prev_also) %>%
  mutate('preventive' = 0)

claims <- claims_non_prev %>% union_all(claims_prev) %>%
  group_by(master_id, claim_id, preventive, icd_code) %>%
  summarise("start_dt" = min(start_dt),
            "IP_OP" = min(IP_OP),
            "bill_dt" = max(bill_dt),
            'primary_amount' = sum(primary_amount),
            'secondary_amount' = sum(secondary_amount)) %>% ungroup() %>%
  group_by(master_id, claim_id, preventive) %>%
  summarise("start_dt" = min(start_dt),
            "IP_OP" = last(IP_OP, order_by = order(primary_amount)),
            "bill_dt" = max(bill_dt),
            "icd_code" = icd_code[which(primary_amount == max(primary_amount))][1],
            'primary_amount' = sum(primary_amount),
            'secondary_amount' = sum(secondary_amount)) %>% ungroup() %>%
  mutate("icd_category" = icd10_interpret(icd_code)) %>%
  mutate('icd_category_class' = icd_category_class(icd_category)) %>%
  mutate('icd_category' = category_clean(icd_category)) %>%
  mutate("er_flag" = ifelse(claim_id %in% ER_claim_ids$claim_id, 1, 0), 
         "non_preventable" = ifelse(icd_category %in% c('maternal care without complications', 'reproductive management',
                                                        'uncomplicated delivery', 'inherited malformations', 'injury and poison'),1,0),
         "cost_tier" = case_when(.$primary_amount >= 10000 ~ "$10,000+", 
                                 between(.$primary_amount, 5000, 9999.99) ~ "$5,000-$9,999", 
                                 between(.$primary_amount, 1000, 4999.99) ~ "$1,000-$4,999",
                                 between(.$primary_amount, 500, 999.99) ~ "$500-$999",
                                 between(.$primary_amount, 0, 499.99) ~ "$0-$499", 
                                 TRUE ~ "refund")) %>%
  left_join(assign_table("human_flags_tab", "Data/Sub_Tables/human_flags.csv"), by = c("master_id" = "master_id")) %>% 
  filter(start_dt <= cov_end_dt,
         start_dt >= cov_start_dt) %>%
  select(master_id, claim_id, IP_OP, bill_dt, icd_category, icd_category_class, preventive, non_preventable, 
         er_flag, start_dt, primary_amount, secondary_amount, cost_tier, icd_code) 

##### Write Data #####

write_csv(claims, paste0(directory, "Data/Sub_Tables/claims_analytics.csv"))
print("claims_analytics written to Data/Sub_Tables")

claims_tab <- claims %>% ungroup()

rm("claims", "ER_claim_ids", "claims_cols", "new_cols", "format_sex", "format_dates", "icd10_interpret", 'prev_care', 'claims_prev',
   'claims_prev_also', 'claims_non_prev', 'census_id_bridge', 'claims_id_bridge', 'claims_raw', 'claim_raw_deid', 'data_raw', 'claims_rows',
   'category_clean', 'icd_category_class')
          