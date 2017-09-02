############ Level 4 #############

### Feeds Cost avoiddance by looking at claims 
### by phs condition.

### Input Tables: phs_analytics
###               human_flags
###               claims_proc_plus
###               claims_rx_plus

### Output Tables: conditions
###                conditions_diff

### Author: Peter Cooman

### Sourced By: claims_per_condition()


###########################################
###########################################

##### Functions #####

claims_by_condition <- function(data, filter_col, filter_value, category_name, subcategory_name){
  filter_criteria <- lazyeval::interp(~ which_column == filter_value, which_column = as.name(filter_col))
  
  data %>%
    filter_(filter_criteria) %>%
    group_by(Assessment) %>%
    summarize(count = length(unique(master_id)),
              total = sum(primary_amount,na.rm=TRUE),
              condition = category_name,
              subcategory = subcategory_name) %>% ungroup()
}

count_by_condition <- function(data, filter_col, filter_value, category_name){
  filter_criteria_1 <- lazyeval::interp(~which_column == filter_value & Assessment == length(assessments)-2, 
                                        .values = list(which_column = as.name(filter_col), Assessment = as.name('Assessment')))
  
  filter_criteria_2 <- lazyeval::interp(~which_column == filter_value & Assessment == length(assessments)-1, 
                                        .values = list(which_column = as.name(filter_col), Assessment = as.name('Assessment')))
  
  tibble(condition = rep(category_name,2),
         Assessment = c(length(assessments)-2, length(assessments)-1),
         count_atrisk = c(data %>% filter_(filter_criteria_1) %>% distinct(master_id) %>% nrow(),
                          data %>% filter_(filter_criteria_1) %>% distinct(master_id) %>%
                            inner_join(data %>% filter_(filter_criteria_2), by = 'master_id') %>%
                            distinct(master_id) %>% nrow()))
}

##### Read in Data #####

df_pii <- assign_table("human_flags_tab", "Data/Sub_Tables/human_flags.csv")  
df_phs <- assign_table("phs_tab", "Data/Sub_Tables/phs_analytics.csv")
df_proc_plus <- assign_table("proc_plus_tab", "Data/Sub_Tables/claims_proc_plus.csv")
df_rx_plus <- assign_table("rx_plus_tab", "Data/Sub_Tables/claims_rx_plus.csv")

##### Filter on Participation and Time #####

assessments <- get_assessments(add_end = TRUE)
durations <- c()

df_proc_plus <- df_proc_plus %>%                                                                       # Look at claims,  
  left_join(df_pii, by='master_id') %>%
  filter(`part_flag` == 1 &                                                                            # Made by participants
           ymd(cov_start_dt) <= ymd(assessments[length(assessments)-2]) &                              # Who were enrolled before the penultimate assessment
           (ymd(cov_end_dt) >= ymd(assessments[length(assessments)]-1) | is.na(cov_end_dt))) %>%       # And were enrolled through the last assessment 
  mutate('Assessment' = 0) %>% 
  select(master_id, claim_id, Year, Month, Quarter, start_dt, primary_amount, cov_start_dt, cov_end_dt, emp_flag, `part_flag`)

df_rx_plus <- df_rx_plus %>%
  left_join(df_pii, by='master_id') %>%
  filter(`part_flag` == 1 & 
           ymd(cov_start_dt) <= ymd(assessments[length(assessments)-2]) &
           (ymd(cov_end_dt) >= ymd(assessments[length(assessments)]-1) | is.na(cov_end_dt))) %>%
  mutate('Assessment' = 0) %>% 
  select(master_id, Year, Month, Quarter, fill_dt, primary_amount, cov_start_dt, cov_end_dt, emp_flag, `part_flag`)

for (i in 1:(length(assessments)-1)) {
  df_proc_plus <- df_proc_plus %>%
    mutate('Assessment' = ifelse(ymd(start_dt) >= ymd(assessments[i]),i,Assessment))                   # decide during what program period each claim was made during
  df_rx_plus <- df_rx_plus %>%                                                                         # This can be done a better way without loops
    mutate('Assessment' = ifelse(ymd(fill_dt) >= ymd(assessments[i]),i,Assessment))
  durations <- c(durations,interval(ymd(assessments[i]),ymd(assessments[i+1]))/ddays(1))               # Counts the days between assessment dates
}

df_phs <- df_phs %>%
  mutate('Assessment' = match(assessment, assessments))

##### get all people with health assessments in BOTH 2015 and 2016 #####
df_phs_filt <- df_phs %>%
  group_by(master_id) %>%
  summarize(Flag = any(assessment == assessments[length(assessments) - 1]) +
              any(assessment == assessments[length(assessments) - 2])) %>%
  filter(Flag == 2) %>% ungroup() %>% select(master_id) 

##### keep only the claims made during the past two assessment periods #####
df_plusplus <- df_phs_filt %>%
  left_join(df_phs, by='master_id') %>%                                                                                        # Only participant records in last two assessments
  left_join(df_proc_plus %>%                                                                                                   # And all of their claims
              filter(Assessment == length(assessments)-1 | Assessment == length(assessments)-2),
            by = c('master_id','Assessment')) %>%
  select(master_id, Assessment, primary_amount, uncon_diabetes, con_diabetes, pre_diabetes, no_diabetes, hyperten,             
         met_syn, anemia, high_chol, bmi, smoker, emot_risk, crit_cond) %>%
  union_all(df_phs_filt %>%
              left_join(df_phs, by='master_id') %>%
              left_join(df_rx_plus %>%
                          filter(Assessment == length(assessments)-1| Assessment == length(assessments)-2),
                        by = c('master_id','Assessment')) %>%
              select(master_id, Assessment, primary_amount, uncon_diabetes, con_diabetes, pre_diabetes, no_diabetes, hyperten, 
                     met_syn, anemia, high_chol, bmi, smoker, emot_risk, crit_cond)
  ) %>%
  filter(Assessment == length(assessments)-2 | Assessment == length(assessments)-1) 

##### for each condition, group by that flag and calculate the metrics #####

df_conditions <- claims_by_condition(df_plusplus, 'uncon_diabetes', 1, "Diabetes", "Out of Control") %>%
  union_all(claims_by_condition(df_plusplus, 'con_diabetes', 1, "Diabetes", "In Control")) %>%
  union_all(claims_by_condition(df_plusplus, 'pre_diabetes', 1, "Pre-Diabetes", "Yes")) %>%
  union_all(claims_by_condition(df_plusplus, 'no_diabetes', 1, "Pre-Diabetes", "No")) %>%
  union_all(claims_by_condition(df_plusplus, 'hyperten', 1, "Hypertension", "Yes")) %>%
  union_all(claims_by_condition(df_plusplus, 'hyperten', 0, "Hypertension", "No")) %>%
  union_all(claims_by_condition(df_plusplus, 'met_syn', 1, "Metabolic Syndrome", "Yes")) %>%
  union_all(claims_by_condition(df_plusplus, 'met_syn', 0, "Metabolic Syndrome", "No")) %>%
  union_all(claims_by_condition(df_plusplus, 'anemia', 1, "Anemia", "Yes")) %>%
  union_all(claims_by_condition(df_plusplus, 'anemia', 0, "Anemia", "No")) %>%
  union_all(claims_by_condition(df_plusplus, 'high_chol', 1, "High LDL Cholesterol", "Yes")) %>%
  union_all(claims_by_condition(df_plusplus, 'high_chol', 0, "High LDL Cholesterol", "No")) %>%
  union_all(claims_by_condition(df_plusplus, 'bmi', 1, "High Triglycerides", "Yes")) %>%
  union_all(claims_by_condition(df_plusplus, 'bmi', 0, "High Triglycerides", "No")) %>%
  union_all(claims_by_condition(df_plusplus, 'smoker', 1, "Smoking", "Smoker")) %>%
  union_all(claims_by_condition(df_plusplus, 'smoker', 0, "Smoking", "Non-Smoker")) %>%
  union_all(claims_by_condition(df_plusplus, 'emot_risk', 1, "Emotional Distress", "At Risk")) %>%
  union_all(claims_by_condition(df_plusplus, 'emot_risk', 0, "Emotional Distress", "Not At Risk")) %>%
  union_all(claims_by_condition(df_plusplus, 'crit_cond', 1, "Critical Conditions", 'Yes')) %>%
  union_all(claims_by_condition(df_plusplus, 'crit_cond', 0, "Critical Conditions", 'No')) %>%
  mutate(pmpm = 30*total/(count*durations[Assessment]),
            total_count = nrow(df_phs_filt)) %>% 
  select(condition, subcategory, Assessment, count, pmpm, total_count)

##### Build conditions_diff #####

df_conditions_atrisk_before <- df_conditions %>%
  filter(subcategory %in% c('Yes', 'Out of Control', 'Smoker', 'At Risk')) %>%
  filter(Assessment ==  length(assessments)-2) %>%
  select(condition, total_count, 'pmpm_atrisk_before' = pmpm, 'count_atrisk_before' = count)
         
df_conditions_atrisk_after <- df_conditions %>%
  filter(subcategory %in% c('Yes', 'Out of Control', 'Smoker', 'At Risk')) %>%
  filter(Assessment ==  length(assessments)-1) %>%
  select(condition, 'pmpm_atrisk_after' = pmpm, 'count_atrisk_after' = count)

df_conditions_notatrisk_after <- df_conditions %>%
  filter(subcategory %in% c('No', 'In Control', 'Non-Smoker', 'Not At Risk')) %>%
  filter(Assessment ==  length(assessments)-1) %>%
  select(condition, 'pmpm_notatrisk_after' = pmpm, 'count_notatrisk_after' = count)

df_conditions_diff <- df_conditions_atrisk_before %>%
  left_join(df_conditions_atrisk_after, by = 'condition') %>%
  left_join(df_conditions_notatrisk_after, by = 'condition')

##### Build df_conditions #####

df_conditions <- count_by_condition(df_plusplus, "uncon_diabetes", 1, "Diabetes") %>%
  union_all(count_by_condition(df_plusplus, "pre_diabetes", 1, "Pre-Diabetes")) %>%
  union_all(count_by_condition(df_plusplus, "hyperten", 1, "Hypertension")) %>%
  union_all(count_by_condition(df_plusplus, "met_syn", 1, "Metabolic Syndrome")) %>%
  union_all(count_by_condition(df_plusplus, "anemia", 1, "Anemia")) %>%
  union_all(count_by_condition(df_plusplus, "high_chol", 1, "High LDL Cholesterol")) %>%
  union_all(count_by_condition(df_plusplus, "bmi", 1, "High Triglycerides")) %>%
  union_all(count_by_condition(df_plusplus, "smoker", 1, "Smoking")) %>%
  union_all(count_by_condition(df_plusplus, "emot_risk", 1, "Emotional Distress")) %>%
  union_all(count_by_condition(df_plusplus, 'crit_cond', 1, 'Critical Conditions')) %>%
  left_join(df_conditions, by = c('condition','Assessment')) %>%
  mutate(Assessment = case_when(.$Assessment == length(assessments)-1 ~ 'After',
                                .$Assessment == length(assessments)-2 ~ 'Before'))

##### Write Data #####

write_csv(df_conditions, paste0(directory, "Data/Build_Tables/conditions.csv"))
write_csv(df_conditions_diff, paste0(directory, "Data/Build_Tables/conditions_diff.csv"))
print("conditions written to Data/Build_Tables")
print("conditions_diff written to Data/Build_Tables")

conditions_tab <- df_conditions
conditions_diff_tab <- df_conditions_diff

rm("df_conditions", "df_conditions_diff", "df_conditions_notatrisk_after", "df_conditions_atrisk_after", "df_conditions_atrisk_before",
   "df_plusplus", "claims_by_condition", "count_by_condition", "df_phs", "df_pii", "df_rx_plus", "df_proc_plus", "assessments", "durations",
   "df_phs_filt", "i")
