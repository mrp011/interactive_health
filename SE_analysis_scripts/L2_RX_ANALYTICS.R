############ Level 2 #############

### Reads in raw rx claims information,
### de-identifies phi with id_bridge
### matches drug names with conditions
### formats rx_claims_analytics

### Input Tables: raw_rx_claims
###               id_bridge
###               drug_list

### Output Tables: rx_analytics

### Author: Michelle Powell

### Sourced By: rx_analytics()


###########################################
###########################################

##### Column Parameters #####

# names(read.csv("Data/Raw/rx_2014.csv", header = TRUE, nrows = 1)) # Print raw column names

rx_fill_date                  <- "Date of Service"
rx_label_name                 <- "Drug Name"
rx_purpose                    <- "Most Common Indication" 
rx_supply_days                <- "Days Supply"
rx_paid_date                  <- "Invoice Date"
rx_primary_analysis_amount    <- "Gross Cost"
rx_secondary_analysis_amount  <- "Net Plan Cost"
rx_member_amount              <- "Total Patient Cost"

rx_last                       <- "Last Name"
rx_first                      <- "First Name"
rx_dob                        <- "Birth dte"
rx_sex                        <- "Gender"
rx_ssn                        <- "Dep SSN"
rx_emp_sp_dep                 <- "Relshp Cde"
rx_id                         <- "Patient ID"

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

remove<-function(x,removal){
  x <- trimws(gsub(paste0(" ",removal," "), " ", x))
  x <- trimws(gsub(paste0(" ",removal,"$"), " ", x))
  return(x)
}

trim_extras<-function(x){
  x <- tolower(
    gsub("[-,/]"," ",
         sub(" [0-9].*", "", 
             x[!duplicated(x)])))
  x<-x[!is.na(x)]
  return(x)
}

##### Read and Trim Raw Data #####

rx_cols<-c(rx_fill_date, rx_label_name, rx_purpose, rx_supply_days, rx_paid_date, rx_secondary_analysis_amount, rx_primary_analysis_amount, 
           rx_member_amount, rx_last, rx_first, rx_dob, rx_sex, rx_ssn, rx_emp_sp_dep, rx_id)
new_rx_cols<-c("fill_dt", "label_name", "drug_purpose", "supply_days", "paid_dt", "secondary_amount", "primary_amount", "member_amount", 
               "last", "first", "dob", "sex", "ssn", "emp_spouse", "rx_id")
rx_raw <- read_csv(paste0(directory, "Data/Raw/rx_2014.csv"), col_types = cols(.default = "c")) %>% 
  union_all(read_csv(paste0(directory, "Data/Raw/rx_2015.csv"), col_types = cols(.default = "c"))) %>%
  union_all(read_csv(paste0(directory, "Data/Raw/rx_2016.csv"), col_types = cols(.default = "c")))
rx_claims_raw <- rx_raw[match(rx_cols,colnames(rx_raw))]
colnames(rx_claims_raw)<-new_rx_cols

rm("rx_fill_date", "rx_label_name", "rx_supply_days", "rx_paid_date", "rx_secondary_analysis_amount", "rx_primary_analysis_amount", 
   "rx_member_amount", "rx_last", "rx_first", "rx_dob", "rx_sex", 'rx_id', 'rx_ssn', 'rx_emp_sp_dep')

census_id_bridge <- read_csv(paste0(directory, 'Data/Sub_Tables/id_bridge.csv'))
source('../Pipeline/L2_IH_DEID.R')

##### Format Data #####

rx_claims_all <- rx_claims_raw %>%
  transmute('fill_dt' = mdy(fill_dt),
            'label_name' = tolower(label_name),
            'drug_purpose' = tolower(drug_purpose),
            'supply_days' = as.numeric(gsub("\\(","-", gsub('[\\$,\\)]', '', gsub('-', '0', supply_days)))),
            'paid_dt' = mdy(paid_dt),
            'secondary_amount' = as.numeric(gsub("\\(","-", gsub('[\\$,\\)]', '', gsub('-', '0', secondary_amount)))),
            'primary_amount' = as.numeric(gsub("\\(","-", gsub('[\\$,\\)]', '', gsub('-', '0', primary_amount)))),
            'member_amount' = as.numeric(gsub("\\(","-", gsub('[\\$,\\)]', '',gsub('-', '0', member_amount)))),
            'last' = tolower(last),
            'first' = tolower(first),
            'dob' = format_dates(dob),
            'sex' = format_sex(sex),
            'ssn' = ssn,
            'rx_id' = rx_id,
            'emp_spouse' = ifelse(emp_spouse == 1, 'e', ifelse(emp_spouse == 2, 's', 'd'))) %>%
  filter(emp_spouse != 'd') %>%
  mutate(unique_id = do.call(paste0, list(ssn, dob))) 

##### De-Identify, Trim, Format, Create, Order Fields #####

rx_deid <- ih_deidentify(data = rx_claims_all, data_id = 'rx_id',
                         census_id_bridge = census_id_bridge, census_id = 'unique_id',
                         id_match = TRUE, pii_match = FALSE, fuzzy_match = TRUE) %>%
  mutate(purpose_flag = case_when(.$drug_purpose == "diabetes" ~ "diabetes",
                                  .$drug_purpose == "high blood press/heart disease" ~ "hypertension",
                                  .$drug_purpose == "anemia" ~ "anemia",
                                  .$drug_purpose == "pulmonary hypertension" ~ "hypertension",
                                  .$drug_purpose == "high blood cholesterol" ~ "high-cholesterol")) %>%
  left_join(assign_table("human_flags_tab", "Data/Sub_Tables/human_flags.csv"), by = c("master_id" = "master_id")) %>% 
  filter(fill_dt >= analysis_start, 
         fill_dt < analysis_date,
         fill_dt <= cov_end_dt,
         fill_dt >= cov_start_dt) %>%
  select(master_id, fill_dt, purpose_flag, supply_days, label_name, paid_dt, secondary_amount, member_amount, primary_amount)

if(is.na(rx_purpose)){
  
  ##### Portion Necessary Fields #####
  
  
  drugs<-data.frame('drug' = trim_extras(rx_claims$label_name), 
                    'name' = rx_claims$label_name[!duplicated(rx_claims$label_name)], stringsAsFactors = FALSE)
  
  ##### Read in Drug_lists and create drug list with flags #####
  
  condition_lists<-read_csv(paste0(directory, 'Data/Fixed_Tables/condition_drug_list.csv'), col_types = 'cccc')
  flag_drugs<-rbind.data.frame(data.frame('drug' = trim_extras(condition_lists$`Anemia Drugs`),
                                          'flag' = 'anemia', stringsAsFactors = FALSE),
                               data.frame('drug' = trim_extras(condition_lists$`Diabetes Drugs`),
                                          'flag' = 'diabetes', stringsAsFactors = FALSE),
                               data.frame('drug' = trim_extras(condition_lists$`Hypertension Drugs`),
                                          'flag' = 'hypertension', stringsAsFactors = FALSE), 
                               data.frame('drug' = trim_extras(condition_lists$`Hypercholesterolemia Drugs`),
                                          'flag' = 'high-cholesterol', stringsAsFactors = FALSE))
  
  ##### remove terms of no significance that mess up the text matching #####
  
  removes<-c('ophth', 'hcl', 'tab', 'cap', 'soln', 'oral', 'spray', 'sr', 'lotion', 'ointment', 
             'for', 'susp', 'gel', 'intensol', 'syrup', 'patch', 'weekly', 'er', 'inj', 'nasal',
             'oint', 'cream', 'injector', 'odt', 'syr', 'sod', 'sodium', 'delayed', 'release', 
             'elixer', 'tripack', 'pack', 'packet', 'hydrochloride', 'kit', 'injection', 'intravenous',
             'emergency', 'nebu', 'powder', 'powd' ,'w', '&', 'hct', 'pump', 'film', 'shampoo',
             'solution', 'chew', 'chewable', 'topical', 'capsule', 'tablet', 'conc',
             'pck', 'box', 'syringe', 'packets', 'eye', 'plus', 'relief', 'dispersible', 'particles',
             'cartridge', '(smoking deterent)', 'micro', 'subcutaneous', 'caplets', 'oil')
  
  for(removal in removes){
    drugs$drug <- remove(drugs$drug, removal)
    flag_drugs$drug <- remove(flag_drugs$drug, removal)
  }
  
  drugs$drug <- gsub("[[:space:]]{2,}", " ", drugs$drug)
  drugs$drug <- gsub("succ", "succinate", gsub("hctz", "hydrochlorothiazide", drugs$drug))
  flag_drugs$drug <- gsub("[[:space:]]{2,}", " ", flag_drugs$drug)
  flag_drugs <- flag_drugs[!duplicated(flag_drugs),]
  
  ##### expand drug names for word-by-word comparison #####
  
  claim_drugs<-str_split(drugs$drug, "[[:space:]]", simplify = TRUE)
  flag_drugs_mat<-str_split(flag_drugs$drug, "[[:space:]]", simplify = TRUE)
  
  ##### match drugs #####
  
  for(drug_row in 1:dim(claim_drugs)[1]){
    if(drug_row==1) rm("match_report")
    claim_drug<-claim_drugs[drug_row,]
    find_score<-function(x){
      wt = 10
      len = 6
      matches<-claim_drug[claim_drug %in% x]
      (wt*sum(nchar(matches) >= len) + 
          sum(nchar(matches)[nchar(matches) < len])) /
        (wt*sum(nchar(claim_drug) >= len) +
           sum(nchar(claim_drug)[nchar(claim_drug) < len])) -> score ## places higher weight on big words
      return(score)
    }
    scores<-apply(flag_drugs_mat, 1, find_score)
    if(max(scores)==0) next
    
    match_index<-which(scores==max(scores))
    matches<-as.matrix(cbind(drugs[drug_row,], max(scores), flag_drugs[match_index,]))
    
    if(!exists('match_report')){
      match_report<-matches
    } else {
      match_report<-rbind(match_report, matches)
    }
    rm("find_score", 'claim_drug', 'scores', 'match_index', 'matches')
  }
  
  ##### format output #####
  
  match_report<-as.data.frame(match_report, stringsAsFactors = FALSE)
  colnames(match_report)<-c("drug", 'claim_line', "score", "flag_drug", "flag")
  match_report$score<-as.numeric(match_report$score)
  
  ##### create second string-distance measure and determine matches #####
  
  match_report$jw<-do.call(stringdist, args = list(match_report$drug, match_report$flag_drug, 'jw'))
  full_matches <- match_report %>% group_by(drug, score) %>% filter(jw == max(jw)) %>% 
    mutate('match' = as.numeric(score==1 | jw<.015)) %>% filter(match == 1)
  
  ##### match condition flags back to claims #####
  
  rx_claims$condition_flag<-full_matches$flag[match(rx_claims$label_name, full_matches$claim_line, nomatch = NA)]
  
  rx_claims <- rx_claims %>% mutate(name_matching = case_when(is.na(.$purpose_flag) & is.na(.$condition_flag) ~ 'Match',
                                                              is.na(.$purpose_flag) & !is.na(.$condition_flag) ~ 'Name Extra',
                                                              !is.na(.$purpose_flag) & is.na(.$condition_flag) ~ 'Name Miss',
                                                              .$purpose_flag == .$condition_flag ~ 'Match',
                                                              .$purpose_flag != .$condition_flag ~ 'Name Wrong'))
  drugs_distinct <- distinct(rx_claims, label_name, purpose_flag, condition_flag, name_matching)
  
  print(paste('name-matching success rate:', sum(drugs_distinct$name_matching == 'Match')/dim(drugs_distinct)[1]))
  print(paste('name-matching failure rate:', sum(drugs_distinct$name_matching != 'Match')/dim(drugs_distinct)[1]))
  print(paste('name-matching Miss rate:', sum(drugs_distinct$name_matching == 'Name Miss')/dim(drugs_distinct)[1]))
  print(paste('name-matching Extra rate:', sum(drugs_distinct$name_matching == 'Name Extra')/dim(drugs_distinct)[1]))
  print(paste('name-matching Incorrect rate:', sum(drugs_distinct$name_matching == 'Name Wrong')/dim(drugs_distinct)[1]))
} else {
  rx_deid <- rx_deid %>% rename('condition_flag' = purpose_flag)
}

##### write output #####

write_csv(rx_deid, paste0(directory, "Data/Sub_Tables/rx_analytics.csv"))
print("rx_analytics written to Data/Sub_Tables")

rx_tab <- rx_deid %>% ungroup()

rm("rx_deid", "match_report", "full_matches", "flag_drugs_mat", "flag_drugs", "drugs", "claim_drugs", "condition_lists", "find_score", "rx_purpose",
   "claim_drug", "drug_row", "new_rx_cols", "removal", "removes", "scores", "rx_cols", "remove", "format_sex", "format_dates", "trim_extras",
   'census_id_bridge', 'rx_raw','rx_match_set', 'rx_id_bridge', 'rx_unmatched', 'rx_claims_raw', 'matchers', 'rx_claims_all', 'rx_claims_matched')
