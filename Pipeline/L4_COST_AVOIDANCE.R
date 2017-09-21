############ Level 4 #############

### Build all of the Tables on the Cost Avoidance Pages

### Input Tables: phs_analytics
###               claims_proc_plus
###               claims_rx_plus

### Output Tables: cost_avoidance
###                cost_avoidance_prev

### Author: Michelle Powell

### Sourced By: cost_avoidance()

###########################################
###########################################

phs <- assign_table("phs_tab", paste0(directory, "Data/Sub_Tables/phs_analytics.csv")) %>% 
  mutate(high_chol = as.numeric(high_chol | high_chol_new),
         hyperten = as.numeric(hyperten | hyperten_new)) %>% 
  select(master_id, "Year" = participation_year, bmi, smoker, emot_risk, anemia, 
         high_chol, hyperten, met_syn, crit_cond, pre_diabetes, uncon_diabetes)
cond_cols <- names(phs %>% select(-master_id, -Year))
cond_cols_no <- do.call(paste0, list(cond_cols, "_no"))

claims_plus <- assign_table("proc_plus_tab", paste0(directory, "Data/Sub_Tables/claims_proc_plus.csv")) %>% filter(!is.na(bmi)) %>%
  mutate(high_chol = as.numeric(high_chol | high_chol_new),
         hyperten = as.numeric(hyperten | hyperten_new)) %>% 
  mutate_at(.cols = cond_cols, funs("id" = ifelse(. ,1, 0), "no" = ifelse(., 0, 1))) %>%
  select_(.dots = c("master_id", "Year", "primary_amount", cond_cols, cond_cols_no))

rx_plus <- assign_table("rx_plus_tab", paste0(directory, "Data/Sub_Tables/claims_rx_plus.csv")) %>% filter(!is.na(bmi)) %>% 
  mutate(high_chol = as.numeric(high_chol | high_chol_new),
         hyperten = as.numeric(hyperten | hyperten_new)) %>% 
  mutate_at(.cols = cond_cols, funs("id" = ifelse(. ,1, 0), "no" = ifelse(., 0, 1))) %>%
  select_(.dots = c("master_id", "Year", "primary_amount", cond_cols, cond_cols_no))

mms <- assign_table("claims_per_member_tab", paste0(directory, "Data/Sub_Tables/claims_per_member.csv")) %>% select(master_id, Year, mms_p) %>%
  group_by(master_id, Year) %>% summarise(mms_p = sum(mms_p)) %>% ungroup()

##### Gets Counts for Conversions and prevalence for each year #####

prevalence <- phs %>% arrange(master_id, desc(Year)) %>%
  mutate_at(.cols = cond_cols, 
            funs("prev" = ifelse(., 1, 0), 
                 "prevconv" = ifelse(master_id == lag(master_id, 1) & 
                                   . == 1 & 
                                   lag(., 1) == 0 &
                                   Year == (lag(Year, 1)-1), 1, 0))) %>% 
  arrange(master_id, Year) %>% group_by(Year) %>% summarise_at(vars(contains("prev")), funs(sum(., na.rm = TRUE))) %>% ungroup() %>%
  melt(id.var = "Year") %>%
  mutate(type = ifelse(str_detect(variable, "conv"), "Conversion", "Prevalence"),
         condition = str_replace(variable, "_prev.*", "")) %>% select(Year, condition, type, "count" = value)

prevalence <- left_join(prevalence %>% filter(type == "Prevalence") %>% rename("prevalence" = count) %>% select(-type),
                   prevalence %>% filter(type == "Conversion") %>% rename("conversion" = count) %>% select(-type)) 

prev_change <- prevalence %>% select(Year, condition, prevalence, conversion) %>% 
  mutate("count" = ifelse(conversion == 0, 0, prevalence - conversion)) %>% select(Year, condition, count) %>%
  mutate(Group = Year, Year = Year + 1)

prev_old <- prevalence %>% select(Year, condition, "count" = prevalence) %>% mutate(Group = Year)

prev <- bind_rows(prev_change, prev_old) %>% filter(between(Group, year(analysis_start), year(analysis_end)-1)) %>% mutate(Year = as.character(Year))

##### Calculates difference in PMPM for each condition ##### 

all_claims <- bind_rows(claims_plus, rx_plus) 

all_pmpm <- all_claims %>%
  group_by(Year, master_id) %>% summarise_at(.cols = c(cond_cols, cond_cols_no, "primary_amount"), funs(sum)) %>% ungroup() %>% left_join(mms) %>%
  mutate_at(.cols = c(cond_cols, cond_cols_no), funs("amt" = primary_amount*ifelse(. != 0, 1, 0), "mms" = mms_p*ifelse(. != 0, 1, 0))) %>%
  group_by(Year) %>% summarise_all(funs(sum(., na.rm = TRUE))) %>% ungroup() %>% melt(id.var = "Year") %>%
  mutate(type = ifelse(str_detect(variable, "_amt"), "Cost", ifelse(str_detect(variable, "_mms"), "MMS", "Delete")),
         condition = str_replace(str_replace(variable, "_mms", ""), "_amt", "")) %>% filter(type != "Delete") %>% 
  select(Year, condition, type, "count" = value)

all_pmpm <- left_join(all_pmpm %>% filter(type == "Cost") %>% rename("cost" = count) %>% select(-type),
                      all_pmpm %>% filter(type == "MMS") %>% rename("mms" = count) %>% select(-type)) %>%
  mutate(pmpm = cost/mms, pmpy = cost/(mms/12)) %>%
  mutate(flag = ifelse(str_detect(condition, "_no"), "No", "Yes"),
         condition = str_replace(condition, "_no", "")) %>% select(Year, condition, flag, pmpm, pmpy, cost, mms)

all_pmpm <- left_join(all_pmpm %>% filter(flag == "Yes") %>% rename("pmpm_yes" = pmpm, "pmpy_yes" = pmpy, "cost_yes" = cost, "mms_yes" = mms) %>% select(-flag),
                      all_pmpm %>% filter(flag == "No") %>% rename("pmpm_no" = pmpm, "pmpy_no" = pmpy, "cost_no" = cost, "mms_no" = mms) %>% select(-flag))

##### Calculates Total Savings For Each Condition #####

savings <- inner_join(prevalence, all_pmpm) %>% mutate(total_savings = conversion*(pmpy_yes - pmpy_no)) %>% group_by(condition) %>%
  summarise(total_savings = sum(total_savings), conversion = sum(conversion), 
            cost_yes = sum(cost_yes), cost_no = sum(cost_no), 
            mms_yes = sum(mms_yes), mms_no = sum(mms_no)) %>% ungroup() %>%
  mutate(pmpy_yes = cost_yes/(mms_yes/12), pmpy_no = cost_no/(mms_no/12), pmpy_diff = pmpy_yes - pmpy_no)

##### Write Data #####

savings_tab <- savings
prevalence_tab <- prev

write_csv(savings, paste0(directory, "Data/Build_Tables/cost_avoidance.csv"))
write_csv(prev, paste0(directory, "Data/Build_Tables/cost_avoidance_prev.csv"))

rm("savings", "prev", "all_pmpm", "all_claims", "prev_old", "prev_change", "prevalence", "mms", "phs", "claims_plus", "rx_plus")

