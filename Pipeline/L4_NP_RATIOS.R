############ Level 4 #############

### % participation vs % of population for all 
### Non-Particpant subgroups

### Input Tables: phs
###               human_flags

### Output Tables: np_ratios

### Author: Peter Cooman

### Sourced By: np_ratios()


###########################################
###########################################

##### Read in Data #####

df_pii <- assign_table('human_flags_tab', "Data/Sub_Tables/human_flags.csv")
df_phs <- assign_table("phs_tab" ,"Data/Sub_Tables/phs_analytics.csv")
df_phs <- df_phs %>% filter(assessment == max(assessment)) 

##### Append demo flags and rename labels

pop <- df_pii %>% 
  filter(is.na(cov_end_dt) | cov_end_dt == max(cov_end_dt)) %>% 
  mutate(p_flag = ifelse(master_id %in% df_phs$master_id, 1, 0),
         sex = ifelse(sex == 0, 'Men', 'Women'),
         age = ifelse(age_45 == 0, '18 to 45', '45 and Over'),
         geo = ifelse(geo_risk == 0, 'in Low Risk Zip', "in High Risk Zip")) %>%
  select(master_id, sex, geo, age, p_flag)

##### calculate percentages per group to get total mms_np and total spend #####

total <- pop %>% group_by() %>%
  summarise(class = "All",
            total_pop = n(),
            participation_rate = sum(p_flag)/n(),
            non_participant_total = sum(1-p_flag))

NP_ratios <- pop %>% 
  # Single factor: sex
  group_by(sex) %>%
  summarise(p_rate = sum(p_flag)/n(),
            pct_pop = n()/total$total_pop,
            pct_np_pop = sum(1-p_flag)/total$non_participant_total,
            count_pop = n()) %>%
  mutate(class = paste0('All ', sex)) %>% 
  # Single factor: age
  union_all(pop %>% group_by(age) %>%
              summarise(p_rate = sum(p_flag)/n(),
                        pct_pop = n()/total$total_pop,
                        pct_np_pop = sum(1-p_flag)/total$non_participant_total,
                        count_pop = n()) %>%
              mutate(class = paste0('All ', age))) %>% 
  # Single factor: geography
  union_all(pop %>% group_by(geo) %>%
              summarise(p_rate = sum(p_flag)/n(),
                        pct_pop = n()/total$total_pop,
                        pct_np_pop = sum(1-p_flag)/total$non_participant_total,
                        count_pop = n()) %>%
              mutate(class = paste0('All ', geo))) %>% 
  # Double factor: sex x age
  union_all(pop %>% group_by(sex, age) %>%
              summarise(p_rate = sum(p_flag)/n(),
                        pct_pop = n()/total$total_pop,
                        pct_np_pop = sum(1-p_flag)/total$non_participant_total,
                        count_pop = n()) %>%
              mutate(class = paste0(sex, " ", age)) %>% ungroup()) %>%
  # Double factor: sex x geography
  union_all(pop %>% group_by(sex, geo) %>%
              summarise(p_rate = sum(p_flag)/n(),
                        pct_pop = n()/total$total_pop,
                        pct_np_pop = sum(1-p_flag)/total$non_participant_total,
                        count_pop = n()) %>%
              mutate(class = paste0(sex, " ", geo)) %>% ungroup()) %>%
  # Double factor: age x geography
  union_all(pop %>% group_by(age, geo) %>%
              summarise(p_rate = sum(p_flag)/n(),
                        pct_pop = n()/total$total_pop,
                        pct_np_pop = sum(1-p_flag)/total$non_participant_total,
                        count_pop = n()) %>%
              mutate(class = paste0(age, " ", geo)) %>% ungroup()) %>%
  # Triple factor: sex x age x geography
  union_all(pop %>% group_by(sex, age, geo) %>%
              summarise(p_rate = sum(p_flag)/n(),
                        pct_pop = n()/total$total_pop,
                        pct_np_pop = sum(1-p_flag)/total$non_participant_total,
                        count_pop = n()) %>%
              mutate(class = paste0(sex, " ", age, " ", geo)) %>% ungroup()) %>%
  select(class, count_pop, p_rate, pct_pop, pct_np_pop) %>%
  mutate(average_pct_pop = .5,
         overall_p_rate = total$participation_rate)

write_csv(NP_ratios, paste0(directory, "Data/Build_Tables/np_ratios.csv"))
print("np_ratios written to Data/Build_Tables")

np_ratios_tab <- NP_ratios

rm("df_phs", "pop", "total", "NP_ratios", "df_pii")

