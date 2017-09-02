############ Level 3 #############

### Flags each claim as to whether it was made 
### by a current participant or not

### Input Tables: phs_analytics
###               claims_analytics
###               rx_analytics
###               human_flags

### Output Tables: claims_proc_plus
###                rx_proc_plus

### Author: Peter Cooman

### Sourced By: claims_plus()


###########################################
###########################################


##### Read and Parse Dates on Data #####

df_pii <- assign_table("human_flags_tab", "Data/Sub_Tables/human_flags.csv")
df_pii$cov_end_dt[is.na(df_pii$cov_end_dt)] <- analysis_date

df_phs <- assign_table("phs_tab", "Data/Sub_Tables/phs_analytics.csv")

df_proc <- assign_table("claims_tab", "Data/Sub_Tables/claims_analytics.csv") %>%
  arrange(master_id) %>%
  mutate('Year' = year(start_dt),
         'Month' = month(start_dt),
         'Quarter' = quarter(start_dt))

df_rx <- assign_table("rx_tab", "Data/Sub_Tables/rx_analytics.csv") %>%
  arrange(master_id) %>%
  mutate('Year' = year(fill_dt),
         'Month' = month(fill_dt),
         'Quarter' = quarter(fill_dt))

##### Build Plus Tables #####

df_proc_new <- df_proc %>%
  mutate('part_flag' = 0) %>%
  filter(1 == 0) 

df_rx_new <- df_rx %>%
  mutate('part_flag' = 0) %>%
  filter(1 == 0)

##### Fill Plus Tables #####

assessments <- get_assessments(add_end = TRUE)

for (i in 1:(length(assessments)-1)) {
  
  # join PII to PHS to determine whether somebody was a P during this period
  df_pii_filt <- df_pii %>%
    left_join(df_phs, by = 'master_id') %>%
    mutate(assessment_found = (ymd(assessment) == assessments[i])) %>%
    group_by(master_id) %>%
    summarize('part_flag' = as.numeric(any(assessment_found,na.rm=TRUE))) %>% ungroup() %>%
    left_join(df_phs %>%
                filter(ymd(assessment) == assessments[i])
              , by=c("master_id"))
  
  # filter the procedures
  df_proc_filt <- df_proc %>%
    filter(ymd(start_dt) >= assessments[i] & ymd(start_dt) < assessments[i+1]) %>%
    left_join(df_pii_filt, by = 'master_id')
  
  # filter the Rx
  df_rx_filt <- df_rx %>%
    filter(ymd(fill_dt) >= assessments[i] & ymd(fill_dt) < assessments[i+1]) %>%
    left_join(df_pii_filt, by = 'master_id')
  
  # add the claims per member per month of the current period to the old set
  # and re-aggregate
  df_proc_new <- union_all(df_proc_new,df_proc_filt)
  df_rx_new <- union_all(df_rx_new,df_rx_filt)
}

##### Write Data #####

write_csv(df_proc_new, paste0(directory, "Data/Sub_Tables/claims_proc_plus.csv"))
write_csv(df_rx_new, paste0(directory, "Data/Sub_Tables/claims_rx_plus.csv"))
print("claims_proc_plus written to Data/Sub_Tables")
print("claims_rx_plus written to Data/Sub_Tables")

proc_plus_tab <- df_proc_new %>% ungroup()
rx_plus_tab <- df_rx_new %>% ungroup()

rm("df_rx_new", "df_proc_new", "df_rx_filt", "df_proc_filt", "df_pii_filt", "df_pii", "df_rx", "df_proc", "df_phs", "i", 'assessments')
