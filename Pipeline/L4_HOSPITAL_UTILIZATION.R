############ Level 4 #############

### Finds cost share type averages between participants 
### and non-participant in each claim type 
### (IP, OP, Rx, ER, PC), calculates differences

### Input Tables: claims_per_member
###               claims_proc_plus
###               claims_rx_plus

### Output Tables: util_full
###                util_diff

### Author: Michelle Powell

### Sourced By: hospital_utilization()


###########################################
###########################################

##### Organizes Medical Claims By Member and Claim Type #####

claim_util <- assign_table("proc_plus_tab", "Data/Sub_Tables/claims_proc_plus.csv") %>%
  mutate(year = as.character(Year),
         participation = ifelse(part_flag==1, "Participant", "Non-Participant"),
         preventive = ifelse(preventive == 1, "PC", "NPC"),
         utility = ifelse(er_flag == 1, "ER", ifelse(preventive == 1, "PC", IP_OP))) %>%
  select(master_id, year, participation, primary_amount, utility, preventive)

claim_util <- claim_util %>% 
  group_by(year, participation) %>% 
  summarise(utility = 'Med', 
            primary_amount = sum(primary_amount)) %>% ungroup() %>%
  union_all(claim_util %>% 
              group_by(year, participation, preventive) %>% 
              summarise(primary_amount = sum(primary_amount)) %>% ungroup() %>% 
              mutate(utility = preventive)) %>%
  union_all(claim_util %>% 
              group_by(year, participation, utility) %>%
              summarise(primary_amount = sum(primary_amount)) %>% ungroup()) %>%
  select(-preventive)

##### Organizes Rx Claims By Member #####

rx_util <- assign_table("rx_plus_tab", "Data/Sub_Tables/claims_rx_plus.csv") %>% 
  mutate(year = as.character(Year),
         participation = ifelse(part_flag==1, "Participant", "Non-Participant"),
         utility = "RX") %>%
  group_by(year, participation, utility) %>% summarise(primary_amount = sum(primary_amount)) %>% ungroup()


##### Unions and Joins All Claims and Scales by Member Years #####

util_full <- claim_util %>% union_all(rx_util)


##### Write Data #####

write_csv(util_full, paste0(directory, "Data/Build_Tables/hosp_util_full.csv"))
print("hosp_util_full written to Data/Build_Tables")

util_full_tab <- util_full

rm("util_full", "rx_util", "claim_util")
