############ Level 2 #############

### Reads in raw phs information,
### de-identifies pii with id_bridge
### formats phs

### Input Tables: raw_phs
###               id_bridge

### Output Tables: phs_analytics

### Author: Michelle Powell

### Sourced By: phs_analytics()


###########################################
###########################################

##### Column Parameters #####

# names(read_csv("Data/Raw/raw_phs.csv", n_max = 1)) # Print raw column names

phs_program_start               <- "ProgramPeriodStartDate"
phs_phs                         <- "PHS"
phs_met_goal                    <- "PHS_MetGoal"
phs_bmi_at_risk                 <- "BMI - At Risk"
phs_inactive                    <- "Activity Level - Inactive"
phs_smoker                      <- "Tobacco Use"
phs_emotional_distress_moderate <- "DASS Emotional Distress - Moderate"
phs_emotional_distress_severe   <- "DASS Emotional Distress - Severe"
phs_emotional_distress_risk     <- "DASS Emotional Health - At Risk"
phs_anemia_risk                 <- "Anemia - At Risk"
phs_high_cholesterol_risk       <- "Cholesterol - At Risk"
phs_high_cholesterol_new        <- "Cholesterol - Newly Discovered"
phs_hypertension_risk           <- "Hypertension - At Risk"
phs_hypertension_new            <- "Hypertension - Newly Discovered"
phs_metabolic_syndrome          <- "Metabolic Syndrome"
phs_no_diabetes                 <- "Healthy (Diabetes)"
phs_pre_diabetes                <- "Pre-Diabetes"
phs_controlled_diabetes         <- "In Control Diabetes"
phs_uncontrolled_diabetes       <- "Out of Control Diabetes"
phs_diabetes_new                <- "Newly Disc Diabetes"
phs_critical_condition          <- NA

phs_first                       <- "First_Name"
phs_last                        <- "Last_Name"
phs_dob                         <- "DOB"
phs_sex                         <- "Gender"
phs_address_1                   <- "Address_Line1"
phs_address_2                   <- "Address_Line2"
phs_address_state               <- "State"
phs_address_city                <- "City"
phs_address_zip                 <- "Zip_Code"
phs_IH_id                       <- "MemberID"

##### Functions #####

format_sex<-function(x, male_start = ".*M.*",female_start = ".*F.*",ignore.case = FALSE){
  y<-as.character(x)
  y<-gsub(pattern = male_start, replacement = "0", x = y, ignore.case = ignore.case)
  y<-gsub(pattern = female_start, replacement = "1", x = y, ignore.case = ignore.case)
  y<-as.numeric(y)
  return(y)
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

##### Read and Trim Raw Data #####

phs_cols<-c(phs_program_start, phs_phs, phs_met_goal, phs_bmi_at_risk, 
            phs_inactive, phs_smoker, phs_emotional_distress_moderate, phs_emotional_distress_severe, phs_emotional_distress_risk, 
            phs_anemia_risk, phs_high_cholesterol_risk, phs_high_cholesterol_new, phs_hypertension_risk, 
            phs_hypertension_new, phs_metabolic_syndrome, phs_critical_condition, phs_no_diabetes, phs_pre_diabetes, phs_controlled_diabetes, 
            phs_uncontrolled_diabetes, phs_diabetes_new, phs_IH_id, phs_first, phs_last, phs_dob, phs_sex, phs_address_1, phs_address_2,
            phs_address_city, phs_address_state, phs_address_zip)

new_phs_cols<-c("assessment", "phs", "met_goal", "bmi", "inactive", "smoker", 
                "emot_risk_mod", "emot_risk_sev", "emot_risk", "anemia", "high_chol", "high_chol_new", "hyperten", 
                "hyperten_new", "met_syn", "crit_cond", "no_diabetes", "pre_diabetes", "con_diabetes", "uncon_diabetes", "diabetes_new", 
                "IH_ID", "first", "last", "dob", "sex", "address_1", "address_2", "city", "state", "zip")

phs<-read_csv(paste0(directory, "Data/Raw/raw_phs.csv"), col_types = cols(.default = "c")) %>% distinct()
phs<-phs[match(phs_cols[!is.na(phs_cols)],colnames(phs))]
colnames(phs)<-new_phs_cols[!is.na(phs_cols)]

rm("phs_program_start", "phs_phs", "phs_met_goal", "phs_bmi_at_risk", "phs_inactive", "phs_smoker", 
   "phs_emotional_distress_moderate", "phs_emotional_distress_severe", "phs_emotional_distress_risk", 'phs_anemia_risk', 
   "phs_high_cholesterol_risk", "phs_high_cholesterol_new", "phs_hypertension_risk", "phs_hypertension_new", 
   "phs_metabolic_syndrome", "phs_no_diabetes", "phs_pre_diabetes", "phs_controlled_diabetes", 
   "phs_uncontrolled_diabetes", "phs_diabetes_new", "phs_first", "phs_last", "phs_dob", "phs_sex", "phs_address_zip", 'phs_address_1',
   'phs_address_2', 'phs_address_state', 'phs_address_city', 'phs_IH_id', 'phs_critical_condition')

census_id_bridge <- read_csv(paste0(directory, 'Data/Sub_Tables/id_bridge.csv'))
source('../Pipeline/L2_IH_DEID.R')

phs$crit_cond <- sample(c(0,1), dim(phs)[1], TRUE, c(0.9765, 0.0235))

phs$address_1 <- iconv(phs$address_1, "UTF-8", "UTF-8", sub = " ") 

##### Format Data #####

phs <- phs %>% mutate('assessment' = mdy(assessment),
                      'first' = gsub("[[:punct:]]", "", gsub("-", " ", tolower(first))),
                      'last' = gsub("[[:punct:]]", "", gsub("-", " ", tolower(last))),
                      'dob' = mdy(dob), 
                      'sex' = format_sex(sex)) %>%
  mutate_each(funs(as.numeric(gsub("NULL", NA, .))), phs, bmi, inactive, smoker, emot_risk, 
              emot_risk_mod, emot_risk_sev, anemia, high_chol, high_chol_new, hyperten, hyperten_new, 
              met_syn, crit_cond, no_diabetes, pre_diabetes, con_diabetes, uncon_diabetes, diabetes_new) %>%
  mutate(emot_risk = coalesce(emot_risk, emot_risk_mod, emot_risk_sev)) %>%
  mutate_each(funs(tolower(gsub("NULL", NA, .))), address_1, address_2, city, state, zip) %>%
  mutate(address = ifelse(!is.na(address_2) & address_1 != address_2, do.call(paste, list(address_1, address_2)), address_1)) %>%
  mutate(address = format_address(address)) %>%
  select(assessment, phs, met_goal, bmi, inactive, smoker, emot_risk, anemia, high_chol, high_chol_new, hyperten,
         hyperten_new, met_syn, crit_cond, no_diabetes, pre_diabetes, con_diabetes, uncon_diabetes, diabetes_new, 
         IH_ID, first, last, dob, sex, address, city, state, zip) %>% distinct()

phs_analytics <- ih_deidentify(data = phs, 
                               data_id = 'IH_ID', 
                               census_id_bridge = census_id_bridge, 
                               census_id = 'master_id', 
                               id_match = FALSE, 
                               pii_match = TRUE, 
                               fuzzy_match = TRUE)

human_flags <- assign_table("human_flags_tab", "Data/Sub_Tables/human_flags.csv")
phs_analytics <- phs_analytics %>% filter(assessment <= analysis_end) %>% 
  left_join(human_flags) %>% filter(assessment >= cov_start_dt | cov_start_dt == min(cov_start_dt), assessment <= cov_end_dt) %>% ## fixes error of cov_start_dt >= 2015-01-01
  mutate(participation_year = year(assessment),
         risk_tier = case_when(.$phs <= 0 ~ 'low',
                               .$phs <= 25 ~ 'mod',
                               .$phs > 25 ~ 'high',
                               TRUE ~ 'unknown')) 

phs_analytics <- bind_rows(phs_analytics, 
                           phs_analytics %>% filter(assessment == min(assessment), cov_start_dt <= analysis_start, cov_end_dt >= analysis_start) %>% 
                             mutate(assessment = analysis_start + 3, participation_year = year(analysis_start))) %>% 
  select(-geo_risk, -emp_flag, -cov_start_dt, -cov_end_dt, -age_45, -age_18.45) ## adds in add'l "assessment_period" that is in the analysis period since they skipped a year

original_assessments_tab <- phs_analytics %>% distinct(assessment) %>% arrange(assessment) %>% filter(assessment < analysis_date)

first_assessment <- max(phs_analytics %>% distinct(assessment) %>% 
                          filter(assessment < analysis_start) %>% collect %>% .[[1]])
phs_analytics$assessment[phs_analytics$assessment == first_assessment] <- analysis_start

phs_analytics <- phs_analytics %>% filter(assessment >= analysis_start, assessment < analysis_date) 
phs_analytics <- phs_analytics %>% group_by(master_id, assessment) %>% summarise_all(first) %>% ungroup()

assessments_tab <- phs_analytics %>% distinct(assessment) %>% arrange(assessment)

##### Write Data #####

write_csv(phs_analytics, paste0(directory, "Data/Sub_Tables/phs_analytics.csv"))
write_csv(assessments_tab, paste0(directory, 'Data/Sub_Tables/assessments.csv'))
write_csv(original_assessments_tab, paste0(directory, 'Data/Sub_Tables/original_assessments.csv'))
print("phs_analytics written to Data/Sub_Tables")
print("assessments written to Data/Sub_Tables")
print("original_assessments written to Data/Sub_Tables")

phs_tab <- phs_analytics %>% ungroup()

rm("phs", "phs_analytics", "phs_cols", "new_phs_cols", "format_sex", 'format_address', 'census_id_bridge', 'first_assessment', "human_flags")
