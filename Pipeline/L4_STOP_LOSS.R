############ Level 4 #############

### This builds the stop loss distributions 
### for P vs NP, Emp vs S, P Emp vs NP Emp
### P Sp vs NP Sp and the top three most 
### expensive conditions

### Input Tables: human_flags
###               phs_analytics
###               claims_per_member
###               rx__per_member

### Output Tables: stop_loss_part
###                stop_loss_emp
###                stop_loss_phs

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

##### Read in Data #####

human_flags <- assign_table('human_flags_tab', "Data/Sub_Tables/human_flags.csv") %>% filter(cov_start_dt <= analysis_end)
claims <- assign_table('claims_per_member_tab', "Data/Sub_Tables/claims_per_member.csv")
phs <- assign_table("phs_tab", "Data/Sub_Tables/phs_analytics.csv") %>% 
  filter(participation_year %in% year(get_assessments()[(length(get_assessments())-1):length(get_assessments())]))


claims <- claims %>% group_by(master_id, Year) %>% summarise_if(is.numeric, sum) %>% ungroup() %>% filter(days != 0) %>%
  mutate(p_flag = days_p != 0) %>%
  transmute(master_id = master_id, Year = Year, p_flag,
            primary_amount = total_med_primary_p + total_rx_primary_p + total_med_primary_np + total_rx_primary_np,
            secondary_amount = total_med_secondary_p + total_rx_secondary_p + total_med_secondary_np + total_rx_secondary_np)
  

if(paid_amount == 'primary_amount') {
  claims$paid_amount <- claims$primary_amount
}else{
  claims$paid_amount <- claims$secondary_amount
}

claims <- claims %>% inner_join(human_flags) %>% select(Year, master_id, p_flag, emp_flag, paid_amount) %>% filter(Year == max(Year))

##### adds participation, employment, and condition flags #####

all_flags <- claims %>% left_join(phs) %>% group_by(master_id) %>%
  summarise(p_flag = min(p_flag), emp_flag = min(emp_flag), paid_amount= min(paid_amount),
            risk_tier = last(risk_tier, order_by = order(participation_year))) %>% ungroup() %>%
  transmute(master_id = master_id, Year = unique(claims$Year), 
            emp_flag = ifelse(emp_flag, "Employee", "Spouse"),
            part_flag = ifelse(p_flag, "Participant", "Non Participant"),
            phs_flag = case_when(.$risk_tier == 'low' ~ "Low Risk", .$risk_tier == "mod" ~ "Moderate Risk", .$risk_tier == "high" ~ "High Risk"),
            paid_amount = paid_amount,
            stop_loss_tier = case_when(.$paid_amount >= stop_loss_amount ~ stop_loss_tier_levels[1],
                                       .$paid_amount >= stop_loss_amount*.5 ~ stop_loss_tier_levels[2],
                                       .$paid_amount >= stop_loss_amount*.25 ~ stop_loss_tier_levels[3],
                                       .$paid_amount >= stop_loss_amount*.1 ~ stop_loss_tier_levels[4],
                                       .$paid_amount >= stop_loss_amount*.05 ~ stop_loss_tier_levels[5],
                                       .$paid_amount > 0 ~ stop_loss_tier_levels[6],
                                       .$paid_amount == 0 ~ stop_loss_tier_levels[7]))

##### Count Participants and Non Participants By Stop-Loss Tier #####

stop_loss_part <- all_flags %>% 
  group_by(Year, part_flag, stop_loss_tier) %>% 
  summarise(count = n()) %>% 
  ungroup() %>%
  union_all(tibble('Year' = rep(unique(all_flags$Year), length(stop_loss_tier_levels)*2),
                   'part_flag' = rep(c('Participant', 'Non Participant'), each = length(stop_loss_tier_levels), times = length(unique(all_flags$Year))),
                   'stop_loss_tier' = rep(stop_loss_tier_levels, times=2*length(unique(all_flags$Year))),
                   'count' = rep(0L, times=length(stop_loss_tier_levels)*2*length(unique(all_flags$Year))))) %>%
  group_by(Year, part_flag, stop_loss_tier) %>% summarise(count = sum(count, na.rm=TRUE)) %>% 
  mutate("pcnt" = count/sum(count, na.rm = TRUE)) %>% ungroup() %>% group_by(Year) %>%
  mutate('Exceeds_Stop_Loss' = max(count)) %>% ungroup()

stop_loss_part <- stop_loss_part %>% filter(part_flag=="Participant") %>% 
  mutate("Participant_Count" = count,
         "Participant_Percent" = pcnt) %>%
  full_join(stop_loss_part %>% filter(part_flag=="Non Participant") %>% 
              mutate("Non_Participant_Count" = count,
                     "Non_Participant_Percent" = pcnt),
            by = c("Year", "stop_loss_tier", 'Exceeds_Stop_Loss')) %>% 
  mutate(Exceeds_Stop_Loss = ifelse(stop_loss_tier == stop_loss_tier_levels[1], Exceeds_Stop_Loss, 0)) %>%
  select(Year, stop_loss_tier, Participant_Count, Participant_Percent, Non_Participant_Count, Non_Participant_Percent, Exceeds_Stop_Loss) %>%
  mutate_all(funs(ifelse(is.na(.), 0, .)))

##### Count Employees and Spouses By Stop-Loss Tier #####

stop_loss_emp <- all_flags %>% group_by(Year, emp_flag, stop_loss_tier) %>% summarise(count = n()) %>% ungroup() %>%
  union_all(tibble('Year' = rep(unique(all_flags$Year), length(stop_loss_tier_levels)*2),
                   'emp_flag' = rep(c('Employee','Spouse'), each=length(stop_loss_tier_levels), times = length(unique(all_flags$Year))),
                   'stop_loss_tier' = rep(stop_loss_tier_levels, times=2*length(unique(all_flags$Year))),
                   'count' = rep(0L, times=length(stop_loss_tier_levels)*2*length(unique(all_flags$Year))))) %>%
  group_by(Year, emp_flag, stop_loss_tier) %>% summarise(count = sum(count, na.rm=TRUE)) %>% 
  mutate("pcnt" = count/sum(count, na.rm = TRUE)) %>% ungroup() %>% group_by(Year) %>%
  mutate('Exceeds_Stop_Loss' = max(count)) %>% ungroup()

stop_loss_emp <- stop_loss_emp %>% filter(emp_flag=="Employee") %>% 
  mutate("Employee_Count" = count, 
         "Employee_Percent" = pcnt) %>%
  full_join(stop_loss_emp %>% filter(emp_flag=="Spouse") %>% 
              mutate("Spouse_Count" = count,
                     "Spouse_Percent" = pcnt),
            by = c("Year", "stop_loss_tier", 'Exceeds_Stop_Loss')) %>% 
  mutate(Exceeds_Stop_Loss = ifelse(stop_loss_tier == stop_loss_tier_levels[1], Exceeds_Stop_Loss, 0)) %>%
  select(Year, stop_loss_tier, Employee_Count, Employee_Percent, Spouse_Count, Spouse_Percent, Exceeds_Stop_Loss) %>%
  mutate_all(funs(ifelse(is.na(.), 0, .)))

##### Count PHS scores By Stop-Loss Tier #####

stop_loss_phs <- all_flags %>% filter(!is.na(phs_flag)) %>% 
  group_by(Year, phs_flag, stop_loss_tier) %>% summarise(count = n()) %>% ungroup() %>%
  union_all(tibble('Year' = rep(unique(all_flags$Year), length(stop_loss_tier_levels)*3),
                   'phs_flag' = rep(c('High Risk','Moderate Risk','Low Risk'),each=length(stop_loss_tier_levels), times = length(unique(all_flags$Year))),
                   'stop_loss_tier' = rep(stop_loss_tier_levels ,times=3*length(unique(all_flags$Year))),
                   'count' = rep(0L, times=length(stop_loss_tier_levels)*3*length(unique(all_flags$Year))))) %>%
  group_by(Year, phs_flag, stop_loss_tier) %>% summarise(count = sum(count, na.rm=TRUE)) %>% 
  mutate("pcnt" = count/sum(count, na.rm = TRUE)) %>% ungroup() %>% group_by(Year) %>%
  mutate('Exceeds_Stop_Loss' = max(count)) %>% ungroup()

stop_loss_phs <- stop_loss_phs %>% filter(phs_flag=="High Risk") %>% 
  mutate("High_Risk_Count" = count,
         "High_Risk_Percent" = pcnt) %>%
  full_join(stop_loss_phs %>% filter(phs_flag=="Moderate Risk") %>% 
              mutate("Moderate_Risk_Count" = count,
                     "Moderate_Risk_Percent" = pcnt), by = c("Year", "stop_loss_tier", 'Exceeds_Stop_Loss')) %>% 
  full_join(stop_loss_phs %>% filter(phs_flag=="Low Risk") %>% 
              mutate("Low_Risk_Count" = count,
                     "Low_Risk_Percent" = pcnt), by = c("Year", "stop_loss_tier", 'Exceeds_Stop_Loss')) %>% 
  mutate(Exceeds_Stop_Loss = ifelse(stop_loss_tier == stop_loss_tier_levels[1], Exceeds_Stop_Loss, 0)) %>%
  select(Year, stop_loss_tier, High_Risk_Count, High_Risk_Percent, Moderate_Risk_Count, Moderate_Risk_Percent, Low_Risk_Count, Low_Risk_Percent, Exceeds_Stop_Loss) %>%
  mutate_all(funs(ifelse(is.na(.), 0, .)))

##### Write Data #####

write_csv(stop_loss_part, paste0(directory, "Data/Build_Tables/stop_loss_participation.csv"))
print("stop_loss_participation written to Data/Build_Tables")
write_csv(stop_loss_emp, paste0(directory, "Data/Build_Tables/stop_loss_employment.csv"))
print("stop_loss_employment written to Data/Build_Tables")
write_csv(stop_loss_phs, paste0(directory, "Data/Build_Tables/stop_loss_phs.csv"))
print("stop_loss_phs written to Data/Build_Tables")

stop_loss_part_tab <- stop_loss_part
stop_loss_emp_tab <- stop_loss_emp
stop_loss_phs_tab <- stop_loss_phs

rm("all_flags", "claims", "human_flags", "phs", "stop_loss_emp", "stop_loss_part", "stop_loss_phs", "stop_loss_tier_levels",
   "stop_loss_tier_numbers", "stop_loss_tier_number_size", "number_labels_large", "number_labels_small", "index")
