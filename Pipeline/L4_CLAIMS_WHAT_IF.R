############ Level 4 #############

### list the PMPM for each combination of gender, 
### age, and geographical group. This will be 
### helpful for the "what-if" analysis

### Input Tables: human_flags
###               claims_per_member
###               phs_analytics

### Output Tables: df_claims_whatif
###                claims_per_demo

### Author: Peter Cooman

### Sourced By: claims_whatif()


###########################################
###########################################

##### Read In Data #####

df_pii <- assign_table("human_flags_tab", "Data/Sub_Tables/human_flags.csv")
df_claims <- assign_table("claims_per_member_tab", "Data/Sub_Tables/claims_per_member.csv")
phs <- assign_table("phs_tab", "Data/Sub_Tables/phs_analytics.csv") %>% filter(assessment == max(assessment))

##### Current Population by Demographics #####

pop <- df_pii %>% filter(cov_end_dt == max(cov_end_dt) | is.na(cov_end_dt)) %>% 
  mutate(p_flag = ifelse(master_id %in% phs$master_id, 1, 0),
         sex = ifelse(sex == 0, 'Male', 'Female'),
         age = ifelse(age_45 == 0, '1', '2'),
         geo = ifelse(geo_risk == 0, 'Low Risk Zip', "High Risk Zip")) %>%
  select(master_id, sex, geo, age, p_flag)

all_counts <- pop %>% group_by() %>% 
  summarise(cur_pop = n(), p_cur_pop = sum(p_flag), np_cur_pop = sum(1-p_flag)) %>% 
  mutate(subcategory = 'All', category = 'Total') %>%
  union_all(pop %>% group_by(sex) %>% 
              summarise(cur_pop = n(), 
                        p_cur_pop = sum(p_flag), 
                        np_cur_pop = sum(1-p_flag)) %>%  
              mutate(subcategory = sex, category = 'Gender')) %>%
  union_all(pop %>% group_by(age) %>% 
              summarise(cur_pop = n(), 
                        p_cur_pop = sum(p_flag), 
                        np_cur_pop = sum(1-p_flag)) %>%  
              mutate(subcategory = age, category = 'Age')) %>%
  union_all(pop %>% group_by(geo) %>% 
              summarise(cur_pop = n(), 
                        p_cur_pop = sum(p_flag), 
                        np_cur_pop = sum(1-p_flag)) %>% 
              mutate(subcategory = geo, category = 'Geography')) %>%
  select(category, subcategory, cur_pop, p_cur_pop, np_cur_pop)

##### Current PMPM by Demographics #####

claim_flags <- df_claims %>% left_join(distinct(df_pii, master_id, sex, geo_risk, emp_flag, age_45, age_18.45)) %>% 
  mutate(primary_p = total_med_primary_p + total_rx_primary_p,
         primary_np = total_med_primary_np + total_rx_primary_np,
         sex = ifelse(sex == 0, 'Male', 'Female'),
         age = ifelse(age_45 == 0, '1', '2'),
         geo = ifelse(geo_risk == 0, 'Low Risk Zip', "High Risk Zip")
  ) %>%
  select(master_id, Year, Month, Quarter, sex, geo, age, primary_p, primary_np, mms_p, mms_np)

pmpm_per_demo <- claim_flags %>% group_by() %>% 
  summarise(pmpm_P= sum(primary_p)/sum(mms_p),
            pmpm_NP = sum(primary_np)/sum(mms_np),
            count_P = sum(mms_p),
            count_NP = sum(mms_np)) %>% mutate(subcategory = 'All',
                                                  category = 'Total') %>% 
  union_all(claim_flags %>% group_by(sex) %>%
              summarise(pmpm_P= sum(primary_p)/sum(mms_p),
                        pmpm_NP = sum(primary_np)/sum(mms_np),
                        count_P = sum(mms_p),
                        count_NP = sum(mms_np)) %>% mutate(subcategory = sex, 
                                                              category = 'Gender')) %>%
              union_all(claim_flags %>% group_by(geo) %>%
                          summarise(pmpm_P= sum(primary_p)/sum(mms_p),
                                    pmpm_NP = sum(primary_np)/sum(mms_np),
                                    count_P = sum(mms_p),
                                    count_NP = sum(mms_np)) %>% mutate(subcategory = geo, 
                                                                          category = 'Geography')) %>%
              union_all(claim_flags %>% group_by(age) %>%
                          summarise(pmpm_P= sum(primary_p)/sum(mms_p),
                                    pmpm_NP = sum(primary_np)/sum(mms_np),
                                    count_P = sum(mms_p),
                                    count_NP = sum(mms_np)) %>% mutate(subcategory = age, 
                                                                          category = 'Age'))  %>%
  select(category, subcategory, pmpm_P, pmpm_NP, count_P, count_NP) %>%
  mutate(max_pmpm = max(max(pmpm_P), max(pmpm_NP)),
         max_avoided = max((count_P + count_NP)*(pmpm_NP - pmpm_P)),
         min_avoided = min((count_P + count_NP)*(pmpm_NP - pmpm_P))) %>%
  left_join(all_counts)

##### Build What-if Table #####

build_whatif <- function(i){
  x <- claim_flags %>%
    group_by_(.dots = c('Year', 'Quarter', demo_groups[0:(3-i)])) %>%
    summarize(cost_avoided = sum(primary_np)/sum(mms_np) - sum(primary_p)/sum(mms_p),
              mms_p = sum(mms_p),
              mms_np = sum(mms_np)) %>% 
    left_join(df_mms_quarter) %>%
    mutate(pct_mms_p = mms_p/mms_tot_p,
           pct_mms_np = mms_np/mms_tot_np) %>%
    mutate(insufficient_flag = ifelse(pct_mms_p < 0.01 | pct_mms_np < 0.01, 1, 0),
           level_flag = (3-i))
  return(x)
}
i <- 0

df_mms_quarter <- claim_flags %>% group_by(Year, Quarter) %>% 
  summarise(mms_tot_p = sum(mms_p), mms_tot_np = sum(mms_np))

demo_groups <- names(claim_flags)[c(7,5,6)]

whatif <- build_whatif(i)

while(sum(whatif$insufficient_flag) > 0){
    i <- i + 1
    whatif_next <- build_whatif(i)
    whatif_fill <- whatif %>% filter(insufficient_flag == 1) %>%
      select(Year, Quarter, sex, age, geo, 'mms_p_keep' = mms_p, 'mms_np_keep' = mms_np) %>%
      left_join(whatif_next, by = c("Year", "Quarter", "sex", "age")) %>%
    select(-mms_p, -mms_np) %>% rename("mms_p" = mms_p_keep, "mms_np" = mms_np_keep)
    
    whatif <- whatif %>% filter(insufficient_flag == 0) %>%
      union_all(whatif_fill) %>% arrange(Year, Quarter)
}

whatif_old <- claim_flags %>%
  group_by(Year, Quarter, sex, age) %>%
  summarize(cost_avoided = sum(primary_np)/sum(mms_np) - sum(primary_p)/sum(mms_p),
            mms_p = sum(mms_p),
            mms_np = sum(mms_np)) %>%
  group_by(Year, Quarter) %>%
  summarise(avoided_current = sum(mms_p*cost_avoided),
            avoided_if = sum((mms_p+mms_np)*cost_avoided)) %>% ungroup() %>%
  mutate(cumul_avoided_current = cumsum(avoided_current),
         cumul_avoided_if = cumsum(avoided_if))

whatif <- whatif %>% group_by(Year, Quarter) %>%
  summarise(avoided_current = sum(mms_p*cost_avoided),
            avoided_if = sum((mms_p+mms_np)*cost_avoided)) %>% ungroup() %>%
  mutate(cumul_avoided_current = cumsum(avoided_current),
         cumul_avoided_if = cumsum(avoided_if))

##### Write Data #####

write_csv(whatif, paste0(directory, "Data/Build_Tables/whatif.csv"))
write_csv(pmpm_per_demo, paste0(directory, "Data/Build_Tables/claims_per_demo.csv"))
print("whatif written to Data/Build_Tables")
print("claims_per_demo written to Data/Build_Tables")

claims_whatif_tab <- whatif_old
pmpm_per_demo_tab <- pmpm_per_demo

rm("pmpm_per_demo", "whatif", "all_counts", "claim_flags", "phs", "pop", "df_claims", "df_pii", "i", 
   "demo_groups", "df_mms_quarter", "build_whatif", 'whatif_old', 'whatif_fill', 'whatif_next')
