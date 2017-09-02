############ Level 4 #############

### View into what cost-tiers and types of claims are 
### contributing to the overall PMPM in what magnitudes. 
### Part of the outlier analysis

### Input Tables: claims_proc_plus
###               human_flags
###               claims_per_member

### Output Tables: cost_tier_melted
###                preventability_melted

### Author: Michelle Powell

### Sourced By: cost_shares()


###########################################
###########################################

##### Read and Filter Data #####

claim_plus <- assign_table("proc_plus_tab", "Data/Sub_Tables/claims_proc_plus.csv") 
human_flags <- assign_table("human_flags_tab", "Data/Sub_Tables/human_flags.csv") %>% 
  distinct(master_id, sex, geo_risk, emp_flag, age_45, age_18.45)
claims_per_member <- assign_table("claims_per_member_tab", "Data/Sub_Tables/claims_per_member.csv")

##### Join all Flags and Differentiators to Claims #####

claim_all_flags<-claim_plus %>% left_join(human_flags) %>% 
  select(master_id, Year, Month, Quarter, emp_flag, sex, geo_risk, age_45, age_18.45,
         part_flag, preventive, non_preventable, IP_OP, start_dt,cost_tier,  primary_amount) %>%
  mutate("preventability" = ifelse(preventive == 1, "Preventive", ifelse(non_preventable == 1, "Non-Preventable", "Preventable")),
         "participation" = ifelse(part_flag==1, "Participant", "Non-Participant"))

##### Retrieve and Join Member Month Data #####

mms_tab <- claims_per_member  %>% 
  select(master_id, Year, Month, Quarter, mms_p, mms, mms_np) 
mms_group <- mms_tab %>% group_by(Year, Quarter) %>% summarise(mms_p = sum(mms_p), mms_np= sum(mms_np))
mms_group <- select(mms_group, Year, Quarter, "mms" = mms_p) %>% mutate(participation = "Participant") %>% 
  union_all(select(mms_group, Year, Quarter, "mms" = mms_np) %>% mutate(participation = "Non-Participant"))

##### Build tier_tab_melted #####

tier_tab <- claim_all_flags %>% group_by(Year, Quarter, participation, cost_tier) %>%
  summarise("primary_amount" = sum(primary_amount, na.rm = TRUE)) %>% 
  left_join(mms_group, by = c("Year", "Quarter", "participation")) %>% ungroup() %>%
  transmute(Period = paste0(Year, " - Q", Quarter),
            Participation = factor(participation, levels = c("Participant", "Non-Participant")),
            Cost_Tier = factor(cost_tier, levels = c("$10,000+", "$5,000-$9,999", "$1,000-$4,999", "$500-$999", "$0-$499", "refund")), 
            PMPM = primary_amount/mms) %>% arrange(Period, Participation, Cost_Tier) %>% melt()

##### Build preventability_tab_melted #####

preventability_tab <- claim_all_flags %>% group_by(Year, Quarter, preventability, participation) %>%
  summarise("primary_amount" = sum(primary_amount, na.rm = TRUE)) %>% 
  left_join(mms_group, by = c("Year" = "Year", "Quarter" = "Quarter", "participation" = "participation")) %>% ungroup() %>%
  transmute(Period = paste0(Year, " - Q", Quarter),
            Participation = factor(participation, levels = c("Participant", "Non-Participant")),
            Preventability = factor(preventability, levels = c("Non-Preventable", "Preventive", "Preventable")), 
            PMPM = primary_amount/mms) %>% arrange(Period, Participation, Preventability) %>% melt()

##### Write Data #####

write_csv(tier_tab, paste0(directory, "Data/Build_Tables/cost_tier_melted.csv"))
write_csv(preventability_tab, paste0(directory, "Data/Build_Tables/preventability_melted.csv"))
print("tier_tab_melted written to Data/Build_Tables")
print("preventability_tab_melted written to Data/Build_Tables")

rm("mms_tab", "mms_group", "claims_per_member", "claim_all_flags", "claim_plus", "human_flags")
