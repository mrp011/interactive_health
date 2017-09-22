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

# read.csv("Data/Raw/raw_claims.csv", header = FALSE, nrows = 1) # Print raw column names

claims_procedure_code               <- "Procedure Code"
claims_service_start                <- "Service From Date"
claims_service_end                  <- "Service Through Date"
claims_IP_OP                        <- 'Patient Stay Category'
claims_billed_date                  <- 'Transaction Date'
claims_icd_10                       <- "ICD10 Diagnosis Code 1"
claims_icd_9                        <- "Primary Diagnosis Code"
claims_primary_analysis_amount      <- "Allowed Charge Amount"
claims_secondary_analysis_amount    <- "Plan Payment Amount"
claims_last                         <- "Patient Last Name"
claims_first                        <- "Patient First Name"
claims_dob                          <- "Patient Birth Date"
claims_sex                          <- "Patient Gender"
claims_claim_id                     <- "Claim Serial Number"
claims_claim_line                   <- "Service Line Index"

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

##### Read and Trim Raw Data #####

claims_cols<-c(claims_procedure_code, claims_service_start, claims_service_end, claims_IP_OP, claims_billed_date, 
               claims_icd_10, claims_icd_9, claims_primary_analysis_amount, claims_secondary_analysis_amount, 
               claims_last, claims_first, claims_dob, claims_sex, claims_claim_id, claims_claim_line)
new_cols<-c("proc", "start_dt", "end_dt", "IP_OP", "bill_dt", "icd_10", "icd_9",
            "primary_amount", "secondary_amount", "last", "first", "dob", "sex", "claim_id", "claim_line") 
claims<-read_csv(paste0(directory, "Data/Raw/raw_claims.csv"), col_types = cols(.default = "c"))
claims<-claims[match(claims_cols,colnames(claims))]
colnames(claims)<-new_cols
claims$proc <- iconv(claims$proc, "UTF-8", "UTF-8", sub = " ") 

rm("claims_procedure_code", "claims_service_start", "claims_service_end", "claims_IP_OP", "claims_billed_date", 
   "claims_icd_10", "claims_icd_9", "claims_primary_analysis_amount", "claims_secondary_analysis_amount", 
   "claims_last", "claims_first", "claims_dob", "claims_sex", "claims_claim_id", 'claims_claim_line')

##### Format Data #####

claims<-claims %>%
  transmute('proc_code' = trimws(gsub("^(.+) - .+?$", "\\1", proc)),
            'proc_desc' = trimws(gsub("^.+ - (.+)$", "\\1", proc)), 
            'start_dt' = mdy(start_dt),
            'end_dt' = mdy(end_dt),
            'IP_OP' = str_sub(claims$IP_OP, 1, 2),
            'bill_dt' = mdy(bill_dt),
            'icd_10' = str_split(icd_10, " - ", simplify = TRUE)[,1], 
            'icd_9' = str_split(icd_9, " - ", simplify = TRUE)[,1],
            'secondary_amount' = as.numeric(gsub("\\(","-",gsub('[\\$,\\)]', '', secondary_amount))),
            'primary_amount' = as.numeric(gsub("\\(","-",gsub('[\\$,\\)]', '', primary_amount))),
            'last' = tolower(last),
            'first' = tolower(first),
            'dob' = trimws(str_sub(dob, 1, 8)),
            'sex' = format_sex(sex),
            'claim_id' = claim_id,
            'claim_line' = claim_line) %>%mutate('dob' = format_dates(dob)) 

#### De-Identify, Trim, Format, Create, Order Fields #####

ER_claim_ids <- claims %>% filter(proc_code %in% c("99281", "99282", "99283", "99284", "99285", "U450", "A0021", "U981") | (
  str_sub(proc_code, 1, 2) == 'A0' & as.numeric(str_sub(proc_code, 3, 5)) > 224)) %>% distinct(claim_id)

claims <- claims %>% left_join(read_csv(paste0(directory, "Data/Sub_Tables/id_bridge.csv")), by = c('last', 'sex', 'dob')) %>% filter(!is.na(master_id)) %>% 
  group_by(master_id, claim_id, claim_line) %>% 
  summarise('proc_code' = last(proc_code, order_by = order(bill_dt)), 
            "start_dt" = last(start_dt, order_by = order(bill_dt)),
            'end_dt' = last(end_dt, order_by = order(bill_dt)),
            'IP_OP' = last(IP_OP, order_by = order(bill_dt)),
            'icd_10' = last(icd_10, order_by = order(bill_dt)),
            'icd_9' = last(icd_9, order_by = order(bill_dt)),
            'bill_dt' = first(bill_dt, order_by = order(bill_dt)),
            'primary_amount' = sum(primary_amount),
            'secondary_amount' = sum(secondary_amount)) %>% ungroup() %>%
  mutate('prev_flag' = prev_care(icd_10, icd_9, proc_code),
         'prev_id' = paste0(claim_id, icd_10)) %>% filter(primary_amount > 0) 

claims_prev <- claims %>% filter(!is.na(prev_flag)) %>% ungroup()
claims_non_prev <- claims %>% filter(is.na(prev_flag)) %>% ungroup()

claims_prev_also <- claims_non_prev %>% 
  filter(str_sub(icd_10, 1, 1) %in% c("Z", "O")) %>%
  filter(prev_id %in% claims_prev$prev_id)

claims_prev <- claims_prev %>% union_all(claims_prev_also) %>% 
  mutate('preventive' = 1)
claims_non_prev <- claims_non_prev %>% anti_join(claims_prev_also) %>%
  mutate('preventive' = 0)

claims <- claims_non_prev %>% union_all(claims_prev) %>%
  mutate("icd_code" = coalesce(icd_10, icd_9)) %>%
  group_by(master_id, claim_id, preventive, icd_code) %>%
  summarise("start_dt" = min(start_dt),
            "IP_OP" = min(IP_OP),
            "bill_dt" = max(bill_dt),
            "primary_amount" = sum(primary_amount),
            'secondary_amount' = sum(secondary_amount)) %>% 
  group_by(master_id, claim_id, preventive) %>%
  summarise("start_dt" = min(start_dt),
            "IP_OP" = last(IP_OP, order_by = order(primary_amount)),
            "bill_dt" = max(bill_dt),
            "icd_code" = icd_code[which(primary_amount == max(primary_amount))][1],
            "primary_amount" = sum(primary_amount),
            'secondary_amount' = sum(secondary_amount)) %>% 
  mutate("icd_category" = icd10_interpret(icd_code)) %>%
  mutate('icd_category_class' = icd_category_class(icd_category)) %>%
  mutate('icd_category' = category_clean(icd_category)) %>%
  mutate("er_flag" = ifelse(claim_id %in% ER_claim_ids$claim_id, 1, 0), 
         "non_preventable" = ifelse(icd_category %in% c('maternal care without complications', 'reproductive management',
                                                       'uncomplicated delivery', 'inherited malformations', 'injury and poison'),1,0),
         "cost_tier" = ifelse(primary_amount >= 10000, "$10,000+", 
                       ifelse(between(primary_amount, 5000, 9999.99), "$5,000-$9,999", 
                       ifelse(between(primary_amount, 1000, 4999.99), "$1,000-$4,999",
                       ifelse(between(primary_amount, 500, 999.99), "$500-$999",
                       ifelse(between(primary_amount, 0, 499.99), "$0-$499", "refund")))))) %>%
  left_join(read_csv(paste0(directory, "Data/Sub_Tables/human_flags.csv")), by = c("master_id" = "master_id")) %>% 
  filter(start_dt <= coalesce(cov_end_dt, Sys.Date())) %>%
  select(master_id, claim_id, IP_OP, bill_dt, icd_category, icd_category_class, preventive, non_preventable, 
         er_flag, start_dt, primary_amount, secondary_amount, cost_tier, icd_code) 

##### Write Data #####

write_csv(claims, paste0(directory, "Data/Sub_Tables/claims_analytics.csv"))
print("claims_analytics written to Data/Sub_Tables")

claims_tab <- claims %>% ungroup()

rm("claims", "ER_claim_ids", "claims_cols", "new_cols", "format_sex", "format_dates", "icd10_interpret", 'prev_care', 'claims_prev',
   'claims_prev_also', 'claims_non_prev')
