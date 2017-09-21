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

# names(read.csv("Data/Raw/raw_phs.csv", header = TRUE, nrows = 1)) # Print raw column names

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
phs_critical_condition          <- "Critical Coaching - Targeted"

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

phs<-read_xlsx(paste0(directory, "Data/Raw/raw_phs.xlsx"))#, col_types = cols(.default = "c")) #%>% 
  #union_all(read_csv(paste0(directory, "Data/Raw/raw_phs_13.csv"), col_types = cols(.default = "c")))
phs<-phs[match(phs_cols,colnames(phs))]
colnames(phs)<-new_phs_cols

rm("phs_program_start", "phs_phs", "phs_met_goal", "phs_bmi_at_risk", "phs_inactive", "phs_smoker", 
   "phs_emotional_distress_moderate", "phs_emotional_distress_severe", "phs_emotional_distress_risk", 'phs_anemia_risk', 
   "phs_high_cholesterol_risk", "phs_high_cholesterol_new", "phs_hypertension_risk", "phs_hypertension_new", 
   "phs_metabolic_syndrome", "phs_no_diabetes", "phs_pre_diabetes", "phs_controlled_diabetes", 
   "phs_uncontrolled_diabetes", "phs_diabetes_new", "phs_first", "phs_last", "phs_dob", "phs_sex", "phs_address_zip", 'phs_address_1',
   'phs_address_2', 'phs_address_state', 'phs_address_city', 'phs_IH_id', 'phs_critical_condition')

census_id_bridge <- read_csv(paste0(directory, 'Data/Sub_Tables/id_bridge.csv'))
source('../Pipeline/L2_IH_DEID.R')

phs$address_1 <- iconv(phs$address_1, "UTF-8", "UTF-8", sub = " ") 

##### Format Data #####

phs <- phs %>% mutate('assessment' = ymd(as.character(assessment)),
                      'first' = gsub("[[:punct:]]", "", gsub("-", " ", tolower(first))),
                      'last' = gsub("[[:punct:]]", "", gsub("-", " ", tolower(last))),
                      'dob' = ymd(as.character(dob)),
                      'sex' = format_sex(sex)) %>%
  mutate_each(funs(as.numeric(gsub("NULL", NA, .))), phs, bmi, inactive, smoker, emot_risk, 
              emot_risk_mod, emot_risk_sev, anemia, high_chol, high_chol_new, hyperten, hyperten_new, 
              met_syn, crit_cond, no_diabetes, pre_diabetes, con_diabetes, uncon_diabetes, diabetes_new) %>%
  mutate(emot_risk = coalesce(emot_risk, emot_risk_mod, emot_risk_sev)) %>%
  mutate_each(funs(tolower(gsub("NULL", NA, .))), address_1, address_2, city, state, zip) %>%
  mutate(address = ifelse(!is.na(address_2) & address_1 != address_2, do.call(paste, list(address_1, address_2)), address_1)) %>%
  mutate(address = format_address(address)) %>%
  mutate(met_goal = gsub("NULL", "N/A", met_goal)) %>%
  select(assessment, phs, met_goal, bmi, inactive, smoker, emot_risk, anemia, high_chol, high_chol_new, hyperten,
         hyperten_new, met_syn, crit_cond, no_diabetes, pre_diabetes, con_diabetes, uncon_diabetes, diabetes_new, 
         IH_ID, first, last, dob, sex, address, city, state, zip)

phs_analytics <- ih_deidentify(data = phs, 
                               data_id = 'IH_ID', 
                               census_id_bridge = census_id_bridge, 
                               census_id = 'master_id', 
                               id_match = FALSE, 
                               pii_match = TRUE, 
                               fuzzy_match = TRUE)

##### Create Fake First Assessment Using Potential Participants and Most Closest PHS Results #####

phs_complete <- phs_analytics %>% filter(assessment != min(phs_analytics$assessment))
starters <- phs_analytics %>% filter(assessment == min(phs_analytics$assessment)) %>% select(master_id, assessment, phs, met_goal, crit_cond)
phs_2014 <- phs_analytics %>% filter(year(assessment) == 2014) %>% select(-assessment, -phs, -met_goal, -crit_cond)
phs_2013 <- starters %>% left_join(phs_2014)
phs_2013_matched <- phs_2013 %>% filter(!is.na(bmi))
phs_2013_unmatched <- phs_2013 %>% filter(is.na(bmi))
set.seed(111111)
phs_2013_unmatched <- phs_2013_unmatched %>%
  mutate(bmi = sample(c(1, 0), length(phs_2013_unmatched$bmi), TRUE, c(sum(phs_complete$bmi, na.rm = TRUE)/length(phs_complete$bmi), 1-sum(phs_complete$bmi, na.rm = TRUE)/length(phs_complete$bmi))), 
         inactive = sample(c(1, 0), length(phs_2013_unmatched$inactive), TRUE, c(sum(phs_complete$inactive, na.rm = TRUE)/length(phs_complete$inactive), 1-sum(phs_complete$inactive, na.rm = TRUE)/length(phs_complete$inactive))),
         smoker = sample(c(1, 0), length(phs_2013_unmatched$smoker), TRUE, c(sum(phs_complete$smoker, na.rm = TRUE)/length(phs_complete$smoker), 1-sum(phs_complete$smoker, na.rm = TRUE)/length(phs_complete$smoker))),
         emot_risk = sample(c(1, 0), length(phs_2013_unmatched$emot_risk), TRUE, c(sum(phs_complete$emot_risk, na.rm = TRUE)/length(phs_complete$emot_risk), 1-sum(phs_complete$emot_risk, na.rm = TRUE)/length(phs_complete$emot_risk))),
         anemia = sample(c(1, 0), length(phs_2013_unmatched$anemia), TRUE, c(sum(phs_complete$anemia, na.rm = TRUE)/length(phs_complete$anemia), 1-sum(phs_complete$anemia, na.rm = TRUE)/length(phs_complete$anemia))),
         high_chol = sample(c(1, 0), length(phs_2013_unmatched$high_chol), TRUE, c(sum(phs_complete$high_chol, na.rm = TRUE)/length(phs_complete$high_chol), 1-sum(phs_complete$high_chol, na.rm = TRUE)/length(phs_complete$high_chol))),
         high_chol_new = ifelse(high_chol == 1, 0, sample(c(1, 0), length(phs_2013_unmatched$high_chol_new), TRUE, c(sum(phs_complete$high_chol_new, na.rm = TRUE)/(length(phs_complete$high_chol_new) - sum(phs_complete$high_chol, na.rm = TRUE)), 1-sum(phs_complete$high_chol_new, na.rm = TRUE)/(length(phs_complete$high_chol_new) - sum(phs_complete$high_chol, na.rm = TRUE))))),
         
         hyperten = sample(c(1, 0), length(phs_2013_unmatched$hyperten), TRUE, c(sum(phs_complete$hyperten, na.rm = TRUE)/length(phs_complete$hyperten), 1-sum(phs_complete$hyperten, na.rm = TRUE)/length(phs_complete$hyperten))),
         hyperten_new = ifelse(hyperten == 1, 0, sample(c(1, 0), length(phs_2013_unmatched$hyperten_new), TRUE, c(sum(phs_complete$hyperten_new, na.rm = TRUE)/(length(phs_complete$hyperten_new) - sum(phs_complete$hyperten, na.rm = TRUE)), 1-sum(phs_complete$hyperten_new, na.rm = TRUE)/(length(phs_complete$hyperten_new) - sum(phs_complete$hyperten, na.rm = TRUE))))),
         
         met_syn = sample(c(1, 0), length(phs_2013_unmatched$met_syn), TRUE, c(sum(phs_complete$met_syn, na.rm = TRUE)/length(phs_complete$met_syn), 1-sum(phs_complete$met_syn, na.rm = TRUE)/length(phs_complete$met_syn))),
         diabetes = sample(c('no_diabetes', 'pre_diabetes', 'con_diabetes', 'uncon_diabetes', 'diabetes_new'), length(phs_2013_unmatched$no_diabetes), TRUE, 
                           c(sum(phs_complete$no_diabetes, na.rm = TRUE)/length(phs_complete$no_diabetes[!is.na(phs_complete$no_diabetes)]), 
                             sum(phs_complete$pre_diabetes, na.rm = TRUE)/length(phs_complete$pre_diabetes[!is.na(phs_complete$no_diabetes)]), 
                             sum(phs_complete$con_diabetes, na.rm = TRUE)/length(phs_complete$con_diabetes[!is.na(phs_complete$no_diabetes)]), 
                             sum(phs_complete$uncon_diabetes, na.rm = TRUE)/length(phs_complete$uncon_diabetes[!is.na(phs_complete$no_diabetes)]), 
                             sum(phs_complete$diabetes_new, na.rm = TRUE)/length(phs_complete$diabetes_new[!is.na(phs_complete$no_diabetes)]))),
         no_diabetes = ifelse(diabetes == 'no_diabetes' ,1, 0),
         pre_diabetes = ifelse(diabetes == 'pre_diabetes' ,1, 0),
         con_diabetes = ifelse(diabetes == 'con_diabetes' ,1, 0),
         uncon_diabetes = ifelse(diabetes == 'uncon_diabetes' ,1, 0),
         diabetes_new = ifelse(diabetes == 'diabetes_new' ,1, 0)) %>% select(-diabetes)

phs_full <- phs_complete %>% union_all(phs_2013_matched) %>% union_all(phs_2013_unmatched) %>% mutate_at(.cols = c("bmi", "inactive", "smoker", "emot_risk", "anemia", "high_chol", "high_chol_new", "hyperten", 
                                                                                                                   "hyperten_new", "met_syn", "crit_cond",  "no_diabetes", "pre_diabetes", "con_diabetes", 
                                                                                                                   "uncon_diabetes", "diabetes_new"), funs(ifelse(is.na(.), 0, .)))

phs_analytics <- phs_full

#### Back to the Actual Code #####

human_flags <- assign_table("human_flags_tab", "Data/Sub_Tables/human_flags.csv")
phs_analytics <- phs_analytics %>% filter(assessment <= analysis_end) %>% 
  left_join(human_flags) %>% filter(assessment >= cov_start_dt, assessment <= cov_end_dt) %>%
  mutate(participation_year = year(assessment),
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
write_csv(original_assessments_tab, paste0(directory, 'Data/Sub_Tables/original_assessments.csv'))
print("phs_analytics written to Data/Sub_Tables")
print("assessments written to Data/Sub_Tables")
print("original_assessments written to Data/Sub_Tables")

phs_tab <- phs_analytics %>% ungroup()

rm("phs", "phs_analytics", "phs_cols", "new_phs_cols", "format_sex", 'format_address', 'census_id_bridge', 'first_assessment')
