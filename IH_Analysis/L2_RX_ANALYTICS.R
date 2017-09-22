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

# read.csv("Data/Raw/raw_rx.csv", header = FALSE, nrows = 1) # Print raw column names

rx_fill_date                    <- "Date Filled"
rx_label_name                   <- "Label Name"
rx_supply_days                  <- "Days Supply"
rx_paid_date                    <- "Date Paid"
rx_primary_analysis_amount      <- "Total Cost"
rx_secondary_analysis_amount    <- "Plan Cost"
rx_member_amount                <- "MOP Amount"
rx_last                         <- "Member Last Name"
rx_first                        <- "Member First Name"
rx_dob                          <- "Member Birthdate"
rx_sex                           <- "Member Gender"

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

rx_cols<-c(rx_fill_date, rx_label_name, rx_supply_days, rx_paid_date, rx_primary_analysis_amount, rx_secondary_analysis_amount, 
           rx_member_amount, rx_last, rx_first, rx_dob, rx_sex)
new_rx_cols<-c("fill_dt", "label_name", "supply_days", "paid_dt", "primary_amount", "secondary_amount", "member_amount", 
               "last", "first", "dob", "sex")
rx_claims<-read_csv(paste0(directory, "Data/Raw/raw_rx.csv"), col_types = cols(.default = "c"))
rx_claims<-rx_claims[match(rx_cols,colnames(rx_claims))]
colnames(rx_claims)<-new_rx_cols

rm("rx_fill_date", "rx_label_name", "rx_supply_days", "rx_paid_date", "rx_primary_analysis_amount", "rx_secondary_analysis_amount", 
   "rx_member_amount", "rx_last", "rx_first", "rx_dob", "rx_sex")

##### Format Data #####

rx_claims<-rx_claims %>%
  transmute('fill_dt' = mdy(fill_dt),
            'label_name' = tolower(label_name),
            'supply_days' = as.numeric(supply_days),
            'paid_dt' = mdy(paid_dt),
            'primary_amount' = as.numeric(gsub("\\(","-",gsub('[\\$,\\)]', '', primary_amount))),
            'secondary_amount' = as.numeric(gsub("\\(","-",gsub('[\\$,\\)]', '', secondary_amount))),
            'member_amount' = as.numeric(gsub("\\(","-",gsub('[\\$,\\)]', '', member_amount))),
            'last' = tolower(last),
            'first' = tolower(first),
            'dob' = format_dates(dob),
            'sex' = format_sex(sex))

##### De-Identify, Trim, Format, Create, Order Fields #####

rx_claims <- rx_claims %>% left_join(read_csv(paste0(directory, "Data/Sub_Tables/id_bridge.csv")), by = c('last', 'sex', 'dob')) %>% filter(!is.na(master_id)) %>% 
  select(master_id, fill_dt, supply_days, label_name, paid_dt, primary_amount, member_amount, secondary_amount)

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

##### write output #####

write_csv(rx_claims, paste0(directory, "Data/Sub_Tables/rx_analytics.csv"))
print("rx_analytics written to Data/Sub_Tables")

rx_tab <- rx_claims %>% ungroup()

rm("rx_claims", "match_report", "full_matches", "flag_drugs_mat", "flag_drugs", "drugs", "claim_drugs", "condition_lists", "find_score",
   "claim_drug", "drug_row", "new_rx_cols", "removal", "removes", "scores", "rx_cols", "remove", "format_sex", "format_dates", "trim_extras")
