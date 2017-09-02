############ Level 3 #############

### This script doesnt adequately filter the people in the universe. needs another look if using in the future

### Input Tables: human_flags
###               phs_analytics
###               claims_analytics
###               rx_analytics

### Output Tables: stop_loss_part
###                stop_loss_emp
###                stop_loss_part_emp
###                stop_loss_part_sp
###                stop_loss_phs
###                stop_loss_disease
###                top_condition_names

### Author: Michelle Powell

### Sourced By: 

###########################################
###########################################
options(scipen=999)
##### Define Stop Loss Tier Labels #####
stop_loss_tier_numbers <- round(stop_loss_amount * c(1, 0.5, 0.25, 0.1, 0.05))
stop_loss_tier_number_size <- nchar(stop_loss_tier_numbers)
number_labels_small <- formatC(stop_loss_tier_numbers, digits = 0, format = 'f', big.mark = ",")
number_labels_large <- str_replace(as.character(stop_loss_tier_numbers/1000), "\\.0", "")

stop_loss_tier_levels <- c(paste0('$',number_labels_large[1],'k', ' +'), 
                           paste0('$',number_labels_small[1], '+'),
                           paste0('$',number_labels_large[2], ' - ', number_labels_large[1],'k'),
                           paste0('$',number_labels_small[2], ' - ', number_labels_small[1]),
                           paste0('$',number_labels_large[3], ' - ', number_labels_large[2],'k'),
                           paste0('$',number_labels_small[3], ' - ', number_labels_small[2]),
                           paste0('$',number_labels_large[4], ' - ', number_labels_large[3],'k'),
                           paste0('$',number_labels_small[4], ' - ', number_labels_small[3]),
                           paste0('$',number_labels_large[5], ' - ', number_labels_large[4],'k'),
                           paste0('$',number_labels_small[5], ' - ', number_labels_small[4]),
                           paste0('$1 - ', number_labels_large[5],'k'),
                           paste0('$1 - ', number_labels_small[5]),
                           '$0')
index <- c(which(stop_loss_tier_number_size >= 4)*2-1, 
           which(stop_loss_tier_number_size < 4)*2, 
           ifelse(min(stop_loss_tier_number_size) >= 4, 11, 12), 13)

stop_loss_tier_levels <- stop_loss_tier_levels[index]

stop_loss_decrease <- ifelse(stop_loss_amount >= 20000, 
                             max(10000, round(stop_loss_amount, -5)/10),
                             min(2000, round(stop_loss_amount, -4)/5))

stop_loss_all_tier_levels <- c(paste0('1 $', round(stop_loss_amount+2*stop_loss_decrease)/1000,'k'),
                               paste0('2 $', round(stop_loss_amount+stop_loss_decrease)/1000,'k'),
                               paste0('3 $', round(stop_loss_amount)/1000,'k'),
                               paste0('4 $', round(stop_loss_amount-stop_loss_decrease)/1000,'k'),
                               paste0('5 $', round(stop_loss_amount-2*stop_loss_decrease)/1000,'k'))

##### Read in Data #####

human_flags <- assign_table('human_flags_tab', "Data/Sub_Tables/human_flags.csv")
claims <- assign_table('claims_tab', "Data/Sub_Tables/claims_analytics.csv") 
rx <- assign_table('rx_tab', "Data/Sub_Tables/rx_analytics.csv")
phs <- assign_table('phs_tab', "Data/Sub_Tables/phs_analytics.csv") 

if(paid_amount == 'primary_amount') {
  claims$paid_amount <- claims$primary_amount
  rx$paid_amount <- rx$primary_amount
}else{
  claims$paid_amount <- claims$secondary_amount
  rx$paid_amount <- rx$secondary_amount
}

##### Define Participants as Anyone who was a participant at any time during 2015 or 2016 #####

assessments <- get_assessments(add_end = TRUE)

phs <- phs %>% mutate(Year_1 = as.numeric(assessment %in% assessments[1:2]), # Year_1 participant has an assessment period in 2014 or 2015
                      Year_2 = as.numeric(assessment %in% assessments[2:3])) # Year_2 participant has an assessment period in 2015 or 2016

phs_flag <- phs %>% filter(Year_1 == 1) %>% mutate(Year = year(assessments[2])) %>% # All records for Year_1 participants
  union_all(phs %>% filter(Year_2 == 1) %>% mutate(Year = year(assessments[3]))) %>% # All records for Year_2 participants
  mutate(high_chol = as.numeric(high_chol == 1 | high_chol_new == 1), # coalesce conditions fields designating new/old conditions
         hyperten = as.numeric(hyperten == 1 | hyperten_new == 1),
         diabetes = as.numeric(con_diabetes == 1 | uncon_diabetes == 1 | diabetes_new == 1)) %>% 
  group_by(master_id, Year) %>% # Trims duplicate records, someone is calssified as having a condition if they had the condition on any health assessment
  summarise_each(funs(max), phs, bmi, smoker, met_syn, anemia, emot_risk, high_chol, hyperten, diabetes, pre_diabetes)

##### Unions 2015 census and 2016 census and adds participation, employment, and condition flags #####

all_humans <- human_flags %>% filter(year(cov_start_dt) < year(assessments[3])) %>% mutate(Year = year(assessments[2])) %>% # grabs all people who had covereage before 2016 as 2015 census
  union_all(human_flags %>% filter(is.na(cov_end_dt) | year(cov_end_dt) >= year(assessments[3])) %>% mutate(Year = year(assessments[3]))) %>% # grabs all people who had covereage existing after 2016 as 2016 census
  left_join(phs_flag) %>% # joins pariticipation and condition information based on master_id and year
  mutate(part_flag = case_when(is.na(.$phs) ~ 'Non Participant',
                               TRUE ~ 'Participant'),
         phs_flag = case_when(.$phs > 25            ~ 'High Risk',
                              .$phs > 0 & .$phs <= 25 ~ 'Moderate Risk',
                              .$phs <= 0            ~ 'Low Risk'),
         emp_flag = case_when(.$emp_flag == 1 ~ 'Employee',
                              TRUE ~ 'Spouse')) %>% 
  select(master_id, Year, emp_flag, part_flag, phs_flag, bmi, smoker, met_syn, anemia, emot_risk, high_chol, hyperten, diabetes, pre_diabetes)

##### Calculates the total spending of each person each year #####

all_claims <- claims %>% mutate(Year = year(start_dt)) %>% select(master_id, Year, paid_amount) %>% 
  union_all(rx %>% mutate(Year = year(fill_dt)) %>% select(master_id, Year, paid_amount)) %>%
  group_by(master_id, Year) %>%
  summarise(paid_amount = sum(paid_amount, na.rm = TRUE))

##### Joins Claims Totals to PHS info and flags #####

stop_loss <- all_humans %>% left_join(all_claims) %>% ungroup() %>%
  mutate(paid_amount = ifelse(is.na(paid_amount), 0, paid_amount)) %>%
  mutate(stop_loss_tier = case_when(.$paid_amount >= stop_loss_amount ~ stop_loss_tier_levels[1],
                                    .$paid_amount >= stop_loss_amount*.5 ~ stop_loss_tier_levels[2],
                                    .$paid_amount >= stop_loss_amount*.25 ~ stop_loss_tier_levels[3],
                                    .$paid_amount >= stop_loss_amount*.1 ~ stop_loss_tier_levels[4],
                                    .$paid_amount >= stop_loss_amount*.05 ~ stop_loss_tier_levels[5],
                                    .$paid_amount > 0 ~ stop_loss_tier_levels[6],
                                    .$paid_amount == 0 ~ stop_loss_tier_levels[7]))

stop_loss_all <- all_humans %>% left_join(all_claims) %>% ungroup() %>%
  select(master_id, Year, paid_amount) %>%
  mutate(paid_amount = ifelse(is.na(paid_amount), 0, paid_amount)) %>%
  mutate(stop_loss_tier = case_when(.$paid_amount >= stop_loss_amount + 2*stop_loss_decrease ~ stop_loss_all_tier_levels[1],
                                    .$paid_amount >= stop_loss_amount + stop_loss_decrease   ~ stop_loss_all_tier_levels[2],
                                    .$paid_amount >= stop_loss_amount                        ~ stop_loss_all_tier_levels[3],
                                    .$paid_amount >= stop_loss_amount - stop_loss_decrease   ~ stop_loss_all_tier_levels[4],
                                    .$paid_amount >= stop_loss_amount - 2*stop_loss_decrease ~ stop_loss_all_tier_levels[5])) %>%
  filter(!is.na(stop_loss_tier)) %>%
  group_by(stop_loss_tier) %>%
  summarise(paid_amount = sum(paid_amount),
            count = n()) %>% ungroup() %>% 
  bind_rows(tibble('stop_loss_tier' = stop_loss_all_tier_levels, 'paid_amount' = 0, 'count' = 0)) %>%
  group_by(stop_loss_tier) %>% summarise_all(sum) %>%
  mutate(total_cost_avoided = cumsum(paid_amount),
         total_over_stop_loss = cumsum(count),
         stop_loss_tier = substr(stop_loss_tier, 3, 9))

stop_loss_all <- stop_loss_all %>% 
  mutate(total_change_cost_avoided = total_cost_avoided - stop_loss_all$total_cost_avoided[3],
         color = case_when(.$total_cost_avoided > stop_loss_all$total_cost_avoided[3] ~ 'under stop loss',
                           .$total_cost_avoided < stop_loss_all$total_cost_avoided[3] ~ 'over stop loss',
                           TRUE ~ 'current stop loss'))

##### Calculates the top 3 expensive conditions #####

conditions <- stop_loss %>% group_by(Year) %>%
  summarise_each(funs(sum(.*paid_amount, na.rm = TRUE)), bmi, smoker, met_syn, anemia, emot_risk, high_chol, hyperten, diabetes, pre_diabetes)
top3_conditions <- order(t(conditions[2,-1]), decreasing = TRUE)[1:3]+5
top_condition_names <- names(stop_loss)[top3_conditions]

##### Trims Stop Loss table to only contain top three condition info #####

stop_loss <- stop_loss %>% select_(.dots = c("master_id", "Year", "emp_flag", "part_flag", "phs_flag", 
                                             top_condition_names, "paid_amount", "stop_loss_tier"))

##### Count Participants and Non Participants By Stop-Loss Tier #####

stop_loss_part <- stop_loss %>% 
  group_by(Year, part_flag, stop_loss_tier) %>% 
  summarise(count = n()) %>% 
  ungroup() %>%
  union_all(tibble('Year' = rep(c(year(assessments[2]), year(assessments[3])), each=length(stop_loss_tier_levels)*2),
                   'part_flag' = rep(c('Participant', 'Non Participant'), each=length(stop_loss_tier_levels), times=2),
                   'stop_loss_tier' = rep(stop_loss_tier_levels, times=4),
                   'count' = rep(0L, times=length(stop_loss_tier_levels)*4))) %>%
  group_by(Year, part_flag, stop_loss_tier) %>% summarise(count = sum(count, na.rm=TRUE)) %>% 
  mutate("pcnt" = count/sum(count, na.rm = TRUE)) %>% ungroup()

stop_loss_part <- stop_loss_part %>% filter(part_flag=="Participant") %>% 
  mutate("Participant Count" = count,
         "Participant Percent" = pcnt) %>%
  full_join(stop_loss_part %>% filter(part_flag=="Non Participant") %>% 
              mutate("Non Participant Count" = count,
                     "Non Participant Percent" = pcnt),
            by = c("Year", "stop_loss_tier")) %>% 
  mutate('Exceeds Stop Loss' = ifelse(stop_loss_tier == stop_loss_tier_levels[1], max(stop_loss_part$count), 0)) %>%
  select(Year, stop_loss_tier, `Participant Count`, `Participant Percent`, `Non Participant Count`, `Non Participant Percent`, `Exceeds Stop Loss`)

##### Count Employees and Spouses By Stop-Loss Tier #####

stop_loss_emp <- stop_loss %>% group_by(Year, emp_flag, stop_loss_tier) %>% summarise(count = n()) %>% ungroup() %>%
  union_all(tibble('Year' = rep(c(year(assessments[2]), year(assessments[3])), each=length(stop_loss_tier_levels)*2),
                   'emp_flag' = rep(c('Employee','Spouse'), each=length(stop_loss_tier_levels), times=2),
                   'stop_loss_tier' = rep(stop_loss_tier_levels, times=4),
                   'count' = rep(0L, times=length(stop_loss_tier_levels)*4))) %>%
  group_by(Year, emp_flag, stop_loss_tier) %>% summarise(count = sum(count, na.rm=TRUE)) %>% 
  mutate("pcnt" = count/sum(count, na.rm = TRUE)) %>% ungroup()

stop_loss_emp <- stop_loss_emp %>% filter(emp_flag=="Employee") %>% 
  mutate("Employee Count" = count, 
         "Employee Percent" = pcnt) %>%
  full_join(stop_loss_emp %>% filter(emp_flag=="Spouse") %>% 
              mutate("Spouse Count" = count,
                     "Spouse Percent" = pcnt),
            by = c("Year", "stop_loss_tier")) %>% 
  mutate('Exceeds Stop Loss' = ifelse(stop_loss_tier == stop_loss_tier_levels[1], max(stop_loss_emp$count), 0)) %>%
  select(Year, stop_loss_tier, `Employee Count`, `Employee Percent`, `Spouse Count`, `Spouse Percent`, `Exceeds Stop Loss`)

##### Count Participating and Non Participating Employees By Stop-Loss Tier #####

stop_loss_part_emp <- stop_loss %>%
  filter(emp_flag == 'Employee') %>%
  group_by(Year, part_flag, stop_loss_tier) %>% 
  summarise(count = n()) %>% 
  ungroup() %>%
  union_all(tibble('Year' = rep(c(year(assessments[2]), year(assessments[3])), each=length(stop_loss_tier_levels)*2),
                   'part_flag' = rep(c('Participant', 'Non Participant'), each=length(stop_loss_tier_levels), times=2),
                   'stop_loss_tier' = rep(stop_loss_tier_levels, times=4),
                   'count' = rep(0L, times=length(stop_loss_tier_levels)*4))) %>%
  group_by(Year, part_flag, stop_loss_tier) %>% summarise(count = sum(count, na.rm=TRUE)) %>% 
  mutate("pcnt" = count/sum(count, na.rm = TRUE)) %>% ungroup()

stop_loss_part_emp <- stop_loss_part_emp %>% filter(part_flag=="Participant") %>% 
  mutate("Participant Count" = count,
         "Participant Percent" = pcnt) %>%
  full_join(stop_loss_part_emp %>% filter(part_flag=="Non Participant") %>% 
              mutate("Non Participant Count" = count,
                     "Non Participant Percent" = pcnt),
            by = c("Year", "stop_loss_tier")) %>% 
  mutate('Exceeds Stop Loss' = ifelse(stop_loss_tier == stop_loss_tier_levels[1], max(stop_loss_part_emp$count), 0)) %>%
  select(Year, stop_loss_tier, `Participant Count`, `Participant Percent`, `Non Participant Count`, `Non Participant Percent`, `Exceeds Stop Loss`)

##### Count Participating and Non Participating Spouses By Stop-Loss Tier #####

stop_loss_part_sp <- stop_loss %>%
  filter(emp_flag == 'Spouse') %>%
  group_by(Year, part_flag, stop_loss_tier) %>% 
  summarise(count = n()) %>% 
  ungroup() %>%
  union_all(tibble('Year' = rep(c(year(assessments[2]), year(assessments[3])), each=length(stop_loss_tier_levels)*2),
                   'part_flag' = rep(c('Participant', 'Non Participant'), each=length(stop_loss_tier_levels), times=2),
                   'stop_loss_tier' = rep(stop_loss_tier_levels, times=4),
                   'count' = rep(0L, times=length(stop_loss_tier_levels)*4))) %>%
  group_by(Year, part_flag, stop_loss_tier) %>% summarise(count = sum(count, na.rm=TRUE)) %>% 
  mutate("pcnt" = count/sum(count, na.rm = TRUE)) %>% ungroup()

stop_loss_part_sp <- stop_loss_part_sp %>% filter(part_flag=="Participant") %>% 
  mutate("Participant Count" = count,
         "Participant Percent" = pcnt) %>%
  full_join(stop_loss_part_sp %>% filter(part_flag=="Non Participant") %>% 
              mutate("Non Participant Count" = count,
                     "Non Participant Percent" = pcnt),
            by = c("Year", "stop_loss_tier")) %>% 
  mutate('Exceeds Stop Loss' = ifelse(stop_loss_tier == stop_loss_tier_levels[1], max(stop_loss_part_sp$count), 0)) %>%
  select(Year, stop_loss_tier, `Participant Count`, `Participant Percent`, `Non Participant Count`, `Non Participant Percent`, `Exceeds Stop Loss`)

##### Count PHS scores By Stop-Loss Tier #####

stop_loss_phs <- stop_loss %>% filter(!is.na(phs_flag)) %>% 
  group_by(Year, phs_flag, stop_loss_tier) %>% summarise(count = n()) %>% ungroup() %>%
  union_all(tibble('Year' = rep(c(year(assessments[2]),year(assessments[3])),each=length(stop_loss_tier_levels)*3),
                   'phs_flag' = rep(c('High Risk','Moderate Risk','Low Risk'),each=length(stop_loss_tier_levels), times=2),
                   'stop_loss_tier' = rep(stop_loss_tier_levels ,times=6),
                   'count' = rep(0L, times=length(stop_loss_tier_levels)*6))) %>%
  group_by(Year, phs_flag, stop_loss_tier) %>% summarise(count = sum(count, na.rm=TRUE)) %>% 
  mutate("pcnt" = count/sum(count, na.rm = TRUE)) %>% ungroup()

stop_loss_phs <- stop_loss_phs %>% filter(phs_flag=="High Risk") %>% 
  mutate("High Risk Count" = count,
         "High Risk Percent" = pcnt) %>%
  full_join(stop_loss_phs %>% filter(phs_flag=="Moderate Risk") %>% 
              mutate("Moderate Risk Count" = count,
                     "Moderate Risk Percent" = pcnt), by = c("Year", "stop_loss_tier")) %>% 
  full_join(stop_loss_phs %>% filter(phs_flag=="Low Risk") %>% 
              mutate("Low Risk Count" = count,
                     "Low Risk Percent" = pcnt), by = c("Year", "stop_loss_tier")) %>% 
  mutate('Exceeds Stop Loss' = ifelse(stop_loss_tier == stop_loss_tier_levels[1], max(stop_loss_phs$count), 0)) %>%
  select(Year, stop_loss_tier, `High Risk Count`, `High Risk Percent`, `Moderate Risk Count`, `Moderate Risk Percent`,`Low Risk Count`, `Low Risk Percent`, `Exceeds Stop Loss`)

##### Write Data #####

write_csv(stop_loss_all, paste0(directory, "Data/Build_Tables/stop_loss_all.csv"))
print("stop_loss_all written to Data/Build_Tables")
write_csv(stop_loss_part, paste0(directory, "Data/Build_Tables/stop_loss_participation.csv"))
print("stop_loss_participation written to Data/Build_Tables")
write_csv(stop_loss_part_emp, paste0(directory, "Data/Build_Tables/stop_loss_part_employees.csv"))
print("stop_loss_part_employees written to Data/Build_Tables")
write_csv(stop_loss_part_sp, paste0(directory, "Data/Build_Tables/stop_loss_part_spouses.csv"))
print("stop_loss_part_spouses written to Data/Build_Tables")
write_csv(stop_loss_emp, paste0(directory, "Data/Build_Tables/stop_loss_employment.csv"))
print("stop_loss_employment written to Data/Build_Tables")
write_csv(stop_loss_phs, paste0(directory, "Data/Build_Tables/stop_loss_phs.csv"))
print("stop_loss_phs written to Data/Build_Tables")
write_csv(tibble(top_condition_names), paste0(directory, "Data/Sub_Tables/top_condition_names.csv"))
print("top_condition_names written to Data/Sub_Tables")

stop_loss_all_tab <- stop_loss_all
stop_loss_part_tab <- stop_loss_part
stop_loss_part_emp_tab <- stop_loss_part_emp
stop_loss_part_sp_tab <- stop_loss_part_sp
stop_loss_emp_tab <- stop_loss_emp
stop_loss_phs_tab <- stop_loss_phs
top_condition_names_tab <- tibble(top_condition_names)

rm("all_claims", "all_humans", "claims", "conditions", "human_flags", "phs", "phs_flag", "rx", "stop_loss", "stop_loss_cond", 'stop_loss_all', 'stop_loss_all_tier_levels',
   "stop_loss_emp", "stop_loss_part", "stop_loss_part_emp", "stop_loss_part_sp", "stop_loss_phs", "assessments", "top_condition_names", "top3_conditions", "stop_loss_tier_levels")
