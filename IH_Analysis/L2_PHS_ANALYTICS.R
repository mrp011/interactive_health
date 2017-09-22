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

# read.table("Data/Raw/raw_phs.txt", header = FALSE, nrows = 1) # Print raw column names

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

phs_first                       <- "First_Name"
phs_last                        <- "Last_Name"
phs_dob                         <- "DOB"
phs_sex                         <- "Gender"

##### Functions #####

format_sex<-function(x, male_start = ".*M.*",female_start = ".*F.*",ignore.case = FALSE){
  y<-as.character(x)
  y<-gsub(pattern = male_start, replacement = "0", x = y, ignore.case = ignore.case)
  y<-gsub(pattern = female_start, replacement = "1", x = y, ignore.case = ignore.case)
  y<-as.numeric(y)
  return(y)
}

format_dates<-function(date_col, format = '%m/%d/%y', latest = today()){
  date_col<-as.Date(date_col, format = format)
  date_col<-as.Date(ifelse(date_col > latest, format(date_col, "19%y-%m-%d"), format(date_col)))
  return(date_col)
}

##### Read and Trim Raw Data #####

phs_cols<-c(phs_program_start, phs_phs, phs_met_goal, phs_bmi_at_risk, 
            phs_inactive, phs_smoker, phs_emotional_distress_moderate, phs_emotional_distress_severe, phs_emotional_distress_risk, 
            phs_anemia_risk, phs_high_cholesterol_risk, phs_high_cholesterol_new, phs_hypertension_risk, 
            phs_hypertension_new, phs_metabolic_syndrome, phs_no_diabetes, phs_pre_diabetes, phs_controlled_diabetes, 
            phs_uncontrolled_diabetes, phs_diabetes_new, phs_first, phs_last, phs_dob, phs_sex)

new_phs_cols<-c("assessment", "phs", "met_goal", "bmi", "inactive", "smoker", 
                "emot_risk_mod", "emot_risk_sev", "emot_risk", "anemia", "high_chol", "high_chol_new", "hyperten", 
                "hyperten_new", "met_syn", "no_diabetes", "pre_diabetes", "con_diabetes", "uncon_diabetes", "diabetes_new", 
                "first", "last", "dob", "sex")

phs<-read_csv(paste0(directory, "Data/Raw/raw_phs.csv"), col_types = cols(.default = "c"))
phs<-phs[match(phs_cols,colnames(phs))]
colnames(phs)<-new_phs_cols

rm("phs_program_start", "phs_phs", "phs_met_goal", "phs_bmi_at_risk", "phs_inactive", "phs_smoker", 
   "phs_emotional_distress_moderate", "phs_emotional_distress_severe", "phs_emotional_distress_risk", 'phs_anemia_risk', 
   "phs_high_cholesterol_risk", "phs_high_cholesterol_new", "phs_hypertension_risk", "phs_hypertension_new", 
   "phs_metabolic_syndrome", "phs_no_diabetes", "phs_pre_diabetes", "phs_controlled_diabetes", 
   "phs_uncontrolled_diabetes", "phs_diabetes_new", "phs_first", "phs_last", "phs_dob", "phs_sex")

##### Format Data #####

phs <- phs %>% mutate('assessment' = mdy(assessment),
                      'first' = tolower(first),
                      'last' = gsub("[[:punct:]]", "", gsub("-", " ", tolower(last))),
                      'dob' = format_dates(dob), 
                      'sex' = format_sex(sex)) %>%
  mutate_each(funs(as.numeric(gsub("NULL", NA, .))), phs, bmi, inactive, smoker, emot_risk, 
              emot_risk_mod, emot_risk_sev, anemia, high_chol, high_chol_new, hyperten, hyperten_new, 
              met_syn, no_diabetes, pre_diabetes, con_diabetes, uncon_diabetes, diabetes_new) %>%
  mutate(emot_risk = coalesce(emot_risk, emot_risk_mod, emot_risk_sev)) %>%
  select(assessment, phs, met_goal, bmi, inactive, smoker, emot_risk, anemia, high_chol, high_chol_new, hyperten,
         hyperten_new, met_syn, no_diabetes, pre_diabetes, con_diabetes, uncon_diabetes, diabetes_new, first, last, dob, sex)

##### De-Identify, Trim, Format, Create, Order Fields #####

human_flags <- assign_table("human_flags_tab", "Data/Sub_Tables/human_flags.csv")
phs_analytics <- phs %>% left_join(read_csv(paste0(directory, "Data/Sub_Tables/id_bridge.csv")), by = c('last', 'sex', 'dob')) %>% filter(!is.na(master_id)) %>%
  select(master_id, assessment, phs, met_goal, bmi, inactive, smoker, met_syn, anemia, emot_risk, high_chol, 
         high_chol_new, hyperten, hyperten_new, no_diabetes, pre_diabetes, con_diabetes, uncon_diabetes, diabetes_new) %>% 
  filter(assessment <= analysis_end) %>% left_join(human_flags) %>% filter(assessment >= cov_start_dt | cov_start_dt == min(cov_start_dt), assessment <= cov_end_dt | is.na(cov_end_dt)) %>%
  mutate(participation_year = year(assessment),
         crit_cond = 0,
         risk_tier = case_when(.$phs <= 0 ~ 'low',
                               .$phs <= 25 ~ 'mod',
                               .$phs > 25 ~ 'high',
                               TRUE ~ 'unknown')) %>% select(-geo_risk, -emp_flag, -cov_start_dt, -cov_end_dt, -age_45, -age_18.45)

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
print("phs_analytics written to Data/Sub_Tables")
print("assessments written to Data/Sub_Tables")

phs_tab <- phs_analytics %>% ungroup()

rm("phs", "phs_cols", "new_phs_cols", "format_sex", "format_dates", 'first_assessment', 'phs_analytics')
