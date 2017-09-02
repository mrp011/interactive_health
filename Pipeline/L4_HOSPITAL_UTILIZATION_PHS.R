############ Level 4 #############

### Finds cost share within each PHS risk group  
### for each claim type (mix of services)
### (IP, OP, Rx, ER, PC)

### Input Tables: claims_proc_plus
###               claims_rx_plus

### Output Tables: util_by_phs

### Author: Peter Cooman

### Sourced By: hospital_utilization_phs()


###########################################
###########################################

##### Organizes Medical Claims By Member and Claim Type #####

claim_util <- assign_table("proc_plus_tab", "Data/Sub_Tables/claims_proc_plus.csv") %>%
  filter(part_flag==1) %>%
  mutate(year = as.character(Year),
         preventive = ifelse(preventive == 1, "PC", "NPC"),
         utility = ifelse(er_flag == 1, "ER", IP_OP),
         phs_flag = case_when(.$phs > 25            ~ 'High Risk',
                              .$phs > 0 & .$phs <= 25 ~ 'Moderate Risk',
                              .$phs <= 0 ~ 'Low Risk')) %>%
  select(master_id, year, primary_amount, preventive, utility, phs_flag)

claim_util <- claim_util %>% 
  group_by(year, phs_flag) %>% 
  summarise(utility = 'Med', 
            primary_amount = sum(primary_amount)) %>% ungroup() %>%
  union_all(claim_util %>% 
              group_by(year, phs_flag, preventive) %>% 
              summarise(primary_amount = sum(primary_amount)) %>% ungroup() %>% 
              mutate(utility = preventive)) %>%
  union_all(claim_util %>% 
              group_by(year, phs_flag, utility) %>%
              summarise(primary_amount = sum(primary_amount)) %>% ungroup()) %>%
  select(-preventive)

##### Organizes Rx Claims By Member #####

rx_util <- assign_table("rx_plus_tab", "Data/Sub_Tables/claims_rx_plus.csv") %>%
  filter(part_flag==1) %>%
  mutate(year = as.character(Year),
         utility = "Rx",
         phs_flag = case_when(.$phs > 25            ~ 'High Risk',
                              .$phs > 0 & .$phs <= 25 ~ 'Moderate Risk',
                              .$phs <= 0 ~ 'Low Risk')) %>%
  select(master_id, year, primary_amount, utility, phs_flag)

##### Unions and Joins All Claims and Scales by Member Years #####

util_by_phs <- claim_util %>% union_all(claim_util %>% mutate(year = "All")) %>% union_all(rx_util) %>% 
  union_all(rx_util %>% mutate(year = "All")) %>% group_by(year, utility, phs_flag) %>%
  summarise("tot_allow" = sum(primary_amount))

##### Write Data #####

write_csv(util_by_phs, paste0(directory, "Data/Build_Tables/hosp_util_by_phs.csv"))
print("util_by_phys written to Data/Build_Tables")

util_by_phs_tab <- util_by_phs

rm("util_by_phs", "claim_util", "rx_util")
