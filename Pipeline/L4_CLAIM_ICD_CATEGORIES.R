############ Level 4 #############

### Groups Claims by icd codes for bubble charts

### Input Tables: proc_plus

### Output Tables: claim_icd_categories

### Author: Michelle Powell

### Sourced By: claim_categories()


###########################################
###########################################


##### Functions #####

categorize <- function(membership, care){
  x <- claims %>% filter(Membership == membership & Care == care) %>%
    group_by(Membership, Care, icd_category_class, icd_category) %>% 
    summarise('Usage' = sum(primary_amount)) %>%
    mutate('Total_Usage' = sum(Usage)) %>% ungroup() %>%
    mutate('class_frac' = Total_Usage/sum(Usage),
           'cat_frac' = Usage/sum(Usage)) %>%
    mutate(icd_category_class = ifelse(class_frac >= 0.02, icd_category_class, 'other'),
           icd_category = ifelse(class_frac >= 0.02 & cat_frac > 0.01, icd_category, 'other')) %>%
    group_by(Membership, Care, icd_category_class, icd_category) %>% 
    summarise('Usage' = sum(Usage)) %>%
    mutate('Total_Usage' = sum(Usage)) %>% ungroup() %>%
    select(Membership, Care, 'Subcategory' = icd_category_class, 'Category' = icd_category, Usage, Total_Usage)
  return(x)
}

##### Read and Trim Data #####

claims <- assign_table('proc_plus_tab', 'Data/Sub_Tables/claims_proc_plus.csv')  %>%
  mutate(Membership = ifelse(part_flag == 1, 'Participants', 'Non-Participants'),
         Care = ifelse(preventive == 1, "Preventive Care", 'Non-Preventive Care')) %>% 
  select(Year, Month, Membership, Care, icd_category, icd_category_class, primary_amount, secondary_amount)

##### Build Table #####

claim_categories <- bind_rows(categorize('Participants', 'Preventive Care'),
                              categorize('Participants', 'Non-Preventive Care'),
                              categorize('Non-Participants', 'Preventive Care'),
                              categorize('Non-Participants', 'Non-Preventive Care')) #%>%
  #filter(Category != 'other') # This will remove the 'other' categories to make the bubbles from category fit inside subcategory

##### Write Data #####

write_csv(claim_categories, paste0(directory, "Data/Build_Tables/claim_icd_categories.csv"))
print("claim_icd_categories written to Data/Build_Tables")

claim_icd_categories_tab <- claim_categories

rm("categorize", 'claims', 'claim_categories')
