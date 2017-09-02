############ Level 5 #############

### This builds top conditions comparison table

### Input Tables: proc_plus
###               rx_plus
###               claims_per_member
###               phs_analytics
###               top_condition_names

### Output Tables: top_condition_specs

### Author: Michelle Powell

### Sourced By: top_conditons()

###########################################
###########################################

##### Read in Data #####

proc_flags <- assign_table("proc_plus_tab", "Data/Sub_Tables/claims_proc_plus.csv") %>%
  mutate(high_chol = as.numeric(high_chol == 1 | high_chol_new == 1), 
         hyperten = as.numeric(hyperten == 1 | hyperten_new == 1),
         diabetes = as.numeric(con_diabetes == 1 | uncon_diabetes == 1 | diabetes_new == 1))
rx_flags <- assign_table('rx_plus_tab', "Data/Sub_Tables/claims_rx_plus.csv") %>%
  mutate(high_chol = as.numeric(high_chol == 1 | high_chol_new == 1), 
         hyperten = as.numeric(hyperten == 1 | hyperten_new == 1),
         diabetes = as.numeric(con_diabetes == 1 | uncon_diabetes == 1 | diabetes_new == 1))
mms <- assign_table('claims_per_member_tab', "Data/Sub_Tables/claims_per_member.csv")
phs <- assign_table("phs_tab", "Data/Sub_Tables/phs_analytics.csv") %>% 
  mutate(high_chol = as.numeric(high_chol == 1 | high_chol_new == 1), 
         hyperten = as.numeric(hyperten == 1 | hyperten_new == 1),
         diabetes = as.numeric(con_diabetes == 1 | uncon_diabetes == 1 | diabetes_new == 1))
top_condition_names <- assign_table("top_condition_names_tab", "Data/Sub_Tables/top_condition_names.csv", pull_col = "top_condition_names")

##### Find Sum of All Claims in Most Recent Year for each condition #####

proc_flags$cond_1 <- proc_flags %>% select_(top_condition_names[1]) %>% collect %>% .[[1]]
proc_flags$cond_2 <- proc_flags %>% select_(top_condition_names[2]) %>% collect %>% .[[1]]
proc_flags$cond_3 <- proc_flags %>% select_(top_condition_names[3]) %>% collect %>% .[[1]]

proc_totals <- proc_flags %>% 
  select_(.dots = c("master_id", "primary_amount", "Year", "part_flag", "start_dt", 
                    "cond_1", "cond_2", "cond_3")) %>%
  filter(part_flag == 1) %>%
  mutate(interaction_1_2 = cond_1*cond_2*(1-cond_3),
         interaction_1_3 = cond_1*cond_3*(1-cond_2),
         interaction_2_3 = cond_2*cond_3*(1-cond_1),
         interaction_1_2_3 = cond_1*cond_2*cond_3,
         cond_1_only = cond_1 - interaction_1_2 - interaction_1_3 - interaction_1_2_3,
         cond_2_only = cond_2 - interaction_1_2 - interaction_2_3 - interaction_1_2_3,
         cond_3_only = cond_3 - interaction_2_3 - interaction_1_3 - interaction_1_2_3) %>% 
  mutate_at(.cols = c("cond_1", "cond_2", 'cond_3', "cond_1_only", "cond_2_only", 'cond_3_only',
                      "interaction_1_2", "interaction_1_3", "interaction_2_3", "interaction_1_2_3"), funs(.*primary_amount)) %>% 
  group_by(Year) %>%
  summarise_at(.cols = c("cond_1", "cond_2", 'cond_3', "cond_1_only", "cond_2_only", 'cond_3_only', 
                         "interaction_1_2", "interaction_1_3", "interaction_2_3", "interaction_1_2_3"), funs(sum(., na.rm = TRUE))) # total hospital spending per condition per year

rx_flags$cond_1 <- rx_flags %>% select_(top_condition_names[1]) %>% collect %>% .[[1]]
rx_flags$cond_2 <- rx_flags %>% select_(top_condition_names[2]) %>% collect %>% .[[1]]
rx_flags$cond_3 <- rx_flags %>% select_(top_condition_names[3]) %>% collect %>% .[[1]]

rx_totals <- rx_flags %>% 
  select_(.dots = c("master_id", "primary_amount", "Year", "part_flag", "fill_dt", 
                    "cond_1", "cond_2", "cond_3", top_condition_names)) %>%
  filter(part_flag == 1) %>%
  mutate(interaction_1_2 = cond_1*cond_2*(1-cond_3),
         interaction_1_3 = cond_1*cond_3*(1-cond_2),
         interaction_2_3 = cond_2*cond_3*(1-cond_1),
         interaction_1_2_3 = cond_1*cond_2*cond_3,
         cond_1_only = cond_1 - interaction_1_2 - interaction_1_3 - interaction_1_2_3,
         cond_2_only = cond_2 - interaction_1_2 - interaction_2_3 - interaction_1_2_3,
         cond_3_only = cond_3 - interaction_2_3 - interaction_1_3 - interaction_1_2_3) %>% 
  mutate_at(.cols = c("cond_1", "cond_2", 'cond_3', "cond_1_only", "cond_2_only", 'cond_3_only',
                      "interaction_1_2", "interaction_1_3", "interaction_2_3", "interaction_1_2_3"), funs(.*primary_amount)) %>% 
  group_by(Year) %>%
  summarise_at(.cols = c("cond_1", "cond_2", 'cond_3', "cond_1_only", "cond_2_only", 'cond_3_only', 
                         "interaction_1_2", "interaction_1_3", "interaction_2_3", "interaction_1_2_3"), funs(sum(., na.rm = TRUE)))

claim_total <- proc_totals %>% union_all(rx_totals) %>% group_by(Year) %>% summarise_all(funs(sum(., na.rm = TRUE)))

##### Determine Portion of Current Year Spending #####

total_spending <- sum(proc_flags %>% filter(part_flag, Year == max(Year)) %>% select(primary_amount) %>% collect %>% .[[1]]) + 
  sum(rx_flags %>% filter(part_flag, Year == max(Year)) %>% select(primary_amount) %>% collect %>% .[[1]])

medical_spending_pie <- tibble("Group" = row.names(t(claim_total %>% filter(Year == max(Year)))),
                               "Total" = t(claim_total %>% filter(Year == max(Year)))[,1]) %>% filter(Group != "Year")
medical_spending_pie <- bind_rows(medical_spending_pie,
                                   tibble("Group" = "All Participants",
                                          "Total" = total_spending - sum(medical_spending_pie[4:10,2]))) %>%
  mutate(Percent = Total/total_spending)

##### Determine Prevalence #####

phs$cond_1 <- phs %>% select_(top_condition_names[1]) %>% collect %>% .[[1]]
phs$cond_2 <- phs %>% select_(top_condition_names[2]) %>% collect %>% .[[1]]
phs$cond_3 <- phs %>% select_(top_condition_names[3]) %>% collect %>% .[[1]]

prev <- phs %>% filter(assessment != min(assessment)) %>% 
  mutate(Year = year(assessment)) %>%
  select(master_id, Year, cond_1, cond_2, cond_3) %>%
  mutate(interaction_1_2 = cond_1*cond_2*(1-cond_3),
         interaction_1_3 = cond_1*cond_3*(1-cond_2),
         interaction_2_3 = cond_2*cond_3*(1-cond_1),
         interaction_1_2_3 = cond_1*cond_2*cond_3,
         cond_1_only = cond_1 - interaction_1_2 - interaction_1_3 - interaction_1_2_3,
         cond_2_only = cond_2 - interaction_1_2 - interaction_2_3 - interaction_1_2_3,
         cond_3_only = cond_3 - interaction_2_3 - interaction_1_3 - interaction_1_2_3) %>% 
  group_by(Year) %>% summarise_all(sum) %>% select(-master_id) %>% ungroup()

total_prev <- dim(phs %>% filter(assessment == max(assessment)))[1]

medical_spending_pie <- tibble("Group" = row.names(t(claim_total %>% filter(Year == max(Year)))),
                               "Total" = t(claim_total %>% filter(Year == max(Year)))[,1]) %>% filter(Group != "Year")
medical_spending_pie <- bind_rows(medical_spending_pie,
                                  tibble("Group" = "All Participants",
                                         "Total" = total_spending - sum(medical_spending_pie[4:10,2]))) %>%
  mutate(Percent = Total/total_spending)











##### Determine mms for each condition in 2016 #####

assessments <- get_assessments()
assessments <- assessments[(length(assessments)-1):length(assessments)]

phs <- phs %>% filter(assessment %in% assessments) %>% # look at last two assessments
  mutate(high_chol = as.numeric(high_chol == 1 | high_chol_new == 1), 
         hyperten = as.numeric(hyperten == 1 | hyperten_new == 1),
         diabetes = as.numeric(con_diabetes == 1 | uncon_diabetes == 1 | diabetes_new == 1)) %>% 
  select_(.dots = c('master_id', 'assessment', top_condition_names))

phs_2 <- phs %>% filter(assessment == max(assessment)) 
phs_1 <- phs %>% filter(assessment == min(assessment) & !(master_id %in% phs_2$master_id)) 

mms_1a <- mms %>% select(master_id, Year, Month, mms_p) %>% left_join(phs %>% filter(assessment == min(assessment))) %>% # looking at people from the first assessment period 
  filter(Year == max(Year) & Month < month(assessments[2])) %>% # only look at mms from 2016, and where the month is before the second assessment period
  mutate_at(.cols = c(top_condition_names[1], top_condition_names[2], top_condition_names[3]), funs(.*mms_p)) %>% 
  group_by() %>% summarise_at(.cols = c(top_condition_names[1], top_condition_names[2], top_condition_names[3]), funs(sum(., na.rm = TRUE)))

mms_1b <- mms %>% select(master_id, Year, Month, mms_p) %>% left_join(phs_1) %>% # looking at people from the first assessment period 
  filter(Year == max(Year) & Month == month(assessments[2])) %>% 
  mutate_at(.cols = c(top_condition_names[1], top_condition_names[2], top_condition_names[3]), funs(.*mms_p)) %>% 
  group_by() %>% summarise_at(.cols = c(top_condition_names[1], top_condition_names[2], top_condition_names[3]), funs(sum(., na.rm = TRUE)))

mms_2 <- mms %>% select(master_id, Year, Month, mms_p) %>% left_join(phs %>% filter(assessment == max(assessment))) %>% 
  filter(Year == max(Year) & Month >= month(assessments[2]) & assessment == assessments[2]) %>%
  mutate_at(.cols = c(top_condition_names[1], top_condition_names[2], top_condition_names[3]), funs(.*mms_p)) %>% 
  group_by() %>%
  summarise_at(.cols = c(top_condition_names[1], top_condition_names[2], top_condition_names[3]), funs(sum(., na.rm = TRUE)))

mms_total <- mms_1a %>% union_all(mms_1b) %>% union_all(mms_2) %>% group_by() %>% summarise_all(funs(sum(., na.rm = TRUE)))

prev <- phs %>% filter(assessment == assessments[2]) %>% group_by() %>%
  summarise_at(.cols = c(top_condition_names[1], top_condition_names[2], top_condition_names[3]), funs(sum(., na.rm = TRUE)))

names_match <- tibble("var" = c("bmi", "smoker", "met_syn", "anemia", "emot_risk", "high_chol", "hyperten",
                                "diabetes", "pre_diabetes"),
                      "label" = c("High Triglycerides", "Smoking", "Metabolic Syndrome", "Anemia", "Emotional Distress",
                                  "High LDL Cholesterol", "Hypertension", "Diabetes", "Pre Diabetes"))

top_condition_specs <- tibble("condition" = names_match$label[match(names(claim_total), names_match$var)],
                               "prev" = as.numeric(prev[1,]),
                               "pmpm" = as.numeric(claim_total[1,]/mms_total[1,]),
                               "total_claims" = as.numeric(claim_total[1,])
                               ) %>% arrange(desc(total_claims))

write_csv(top_condition_specs, paste0(directory, "Data/Build_Tables/top_condition_specs.csv"))
print("top_condition_specs written to Data/Build_Tables")

top_condition_specs_tab <- top_condition_specs

rm("top_condition_specs", "names_match", "prev", "mms_total", "mms_2", "claim_total", "rx_totals", "proc_totals",
   'proc_flags', 'rx_flags', 'phs', 'mms', "assessments", "top_condition_names", 'mms_1a', 'mms_1b', 'phs_1', 'phs_2')
