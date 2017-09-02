############ Level 3 #############

### Feeds funnel charts showing how participants of 
### certian conditions utilize hospital and rx resources

### Input Tables: phs_analytics
###               claims_analytics
###               rx_analytics

### Output Tables: sought_care_tab

### Author: Michelle Powell

### Sourced By: sought_care_tab()


###########################################
###########################################

##### Read and Filter Participant Condition Data #####

phs <- assign_table("phs_tab", "Data/Sub_Tables/phs_analytics.csv")
assessments <- get_assessments()
phs <- phs %>% 
  filter(ymd(assessment) == assessments[length(assessments)]) %>%                             # Only includes current phs info
  select(master_id, high_chol, high_chol_new, hyperten, hyperten_new, con_diabetes, uncon_diabetes, diabetes_new) %>%
  mutate(hypertension = ifelse(hyperten_new == 1, "new", ifelse(hyperten == 1, "old", NA)), 
         cholesterol = ifelse(high_chol_new == 1, "new", ifelse(high_chol == 1, "old", NA)),
         diabetes = ifelse(diabetes_new == 1, "new", ifelse(coalesce(con_diabetes, uncon_diabetes) == 1, "old", NA))) %>%
  select(master_id, hypertension, cholesterol, diabetes)

phs_all <- phs %>% 
  filter(!is.na(hypertension)) %>%                                 # only people with new or old hypertension
  mutate(condition = "hypertension") %>% 
  select(master_id, condition, "discovered" = hypertension) %>% 
  union_all(phs %>%                                               # only people with new or old cholesterol
              filter(!is.na(cholesterol)) %>%  
              mutate(condition = "high-cholesterol") %>% 
              select(master_id, condition, "discovered" = cholesterol)) %>%
  union_all(phs %>%                                                # only people with new or old diabetes
              filter(!is.na(diabetes)) %>% 
              mutate(condition = "diabetes") %>% 
              select(master_id, condition, "discovered" = diabetes))

##### Read and Filter Participant Rx Claims Data #####

rx<-assign_table("rx_tab", "Data/Sub_Tables/rx_analytics.csv") %>% 
  filter(year(fill_dt) >= year(max(assessments)), 
         !is.na(condition_flag)) %>%                           # only rx claims in the same year as the last assessment, and that relate to a condition
  group_by(master_id, condition_flag) %>%
  summarise("n_prescribed" = n()) %>%                                              
  mutate(rx_care = ifelse(n_prescribed > 0, 1, 0)) %>%         # only keep people who have one of the relevant rx claims
  select(master_id, 'condition' = condition_flag, rx_care)

##### Read and Filter Participant Medical Data #####

claims <- assign_table("claims_tab", "Data/Sub_Tables/claims_analytics.csv") %>% 
  filter(year(start_dt) >= year(max(assessments))) %>% distinct(master_id) # only the id's from people who have had any procedure claim in the same year as the last assessment

##### Build Sought Care #####

sought_care_tab <- phs_all %>% 
  left_join(rx) %>%
  mutate(rx_care = ifelse(is.na(rx_care), 0, rx_care)) %>% 
  mutate(med_care = ifelse(master_id %in% claims$master_id, 1, rx_care)) %>%
  group_by(condition, discovered) %>% summarise("prevalence" = n(), 
                                                "med_care" = sum(med_care == 1 | rx_care == 1, na.rm = TRUE), 
                                                "rx_care" = sum(rx_care == 1, na.rm = TRUE)) %>%
  mutate(total = sum(prevalence)) %>% ungroup()

sought_care_tab <- select(sought_care_tab, condition, discovered, total, "count" = prevalence) %>% mutate(tier = "1_prevalence") %>%
  bind_rows(select(sought_care_tab, condition, discovered, total, "count" = rx_care) %>% mutate(tier = "3_sought rx care"),
            select(sought_care_tab, condition, discovered, total, "count" = med_care) %>% mutate(tier = "2_sought med care")) %>% 
  select(condition, discovered, total, tier, count) %>% 
  arrange(condition, discovered, tier)

sought_care_tab_totals_new <- sought_care_tab %>%
  filter(tier == "1_prevalence") %>%
  group_by(condition) %>%
  summarize(discovered = "new",
            total = sum(total),
            tier = "0_total",
            count = sum(count))

sought_care_tab_totals_old <- sought_care_tab_totals_new %>%
  mutate(discovered = "old")

sought_care_tab <- sought_care_tab %>%
  union_all(sought_care_tab_totals_new) %>%
  union_all(sought_care_tab_totals_old) %>%
  arrange(condition,discovered,tier) %>%
  group_by(condition,discovered) %>%
  mutate(pct = count/lag(count,1),
         pct_total = count/total) %>%
  mutate(pct = ifelse(is.na(pct),1,pct)) %>%
  mutate(pct = ifelse(is.infinite(pct),0,pct),
         num_P = nrow(phs),
         count = as.character(count),
         pct = as.character(pct),
         pct_total = as.character(pct_total),
         num_P = as.character(num_P)) %>%
  union_all(
    tibble('condition' = rep(unique(sought_care_tab$condition),2),
           'discovered' = 'old',
           'tier' = c(rep('buffer1',3),rep('buffer2',3)),
           'count' = c('','','','','',''),
           'pct' = c('','','','','',''),
           'pct_total' = c('','','','','',''),
           'num_P' = c('','','','','',''))
  )

##### Write Data #####

write_csv(sought_care_tab, paste0(directory, "Data/Build_Tables/sought_care_tab.csv"))
print("sought_care_tab written to Data/Build_Tables")

rm("sought_care_tab_totals_old", "sought_care_tab_totals_new", "claims", "rx", "phs_all", "phs")
