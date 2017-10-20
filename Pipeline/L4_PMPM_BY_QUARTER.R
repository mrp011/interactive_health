############ Level 4 #############

### pmpm and percentage growths per quarter over 2015 
### and 2016 (for different demographic comparisons)

### Input Tables: claims_per_member
###               human_flags

### Output Tables: pmpm_by_year
###                pmpm_by_quarter
###                rx_pmpm_by_year
###                proc_pmpm_by_year

### Author: Peter Cooman

### Sourced By: pmpm_by_quarter()


###########################################
###########################################

##### Functions #####

pct_change <- function(data, x){
  x <- data %>% select_(x) %>% collect %>% .[[1]]
  change <- mean(diff(x))
  pct <- change/x[1]
  return(c(change,pct))
}

##### Read In Data #####

df_claims <- assign_table("claims_per_member_tab", "Data/Sub_Tables/claims_per_member.csv")
df_pii <- assign_table("human_flags_tab", "Data/Sub_Tables/human_flags.csv") %>% distinct(master_id, sex, geo_risk, age_45, age_18.45, emp_flag)

##### Group Claims By Year #####

df_claims_by_year <- df_claims %>%
  left_join(df_pii, by='master_id') %>%
  group_by(Year) %>%
  summarize(pmpm_all = sum(total_med_primary_p + total_rx_primary_p + total_med_primary_np + total_rx_primary_np)/sum(mms_p+mms_np),
            pmpm_p = sum(total_med_primary_p + total_rx_primary_p)/sum(mms_p),
            pmpm_np = sum(total_med_primary_np + total_rx_primary_np)/sum(mms_np),
            pmpm_emp = sum(emp_flag*(total_med_primary_p + total_rx_primary_p + 
                                       total_med_primary_np + total_rx_primary_np))/sum(emp_flag*mms),
            pmpm_sp = sum((1-emp_flag)*(total_med_primary_p + total_rx_primary_p + 
                                          total_med_primary_np + total_rx_primary_np))/sum((1-emp_flag)*mms),
            pmpm_p_emp = sum(emp_flag*(total_med_primary_p + total_rx_primary_p))/sum(emp_flag*mms_p),
            pmpm_np_emp = sum(emp_flag*(total_med_primary_np + total_rx_primary_np))/sum(emp_flag*mms_np),
            pmpm_p_sp = sum((1-emp_flag)*(total_med_primary_p + total_rx_primary_p))/sum((1-emp_flag)*mms_p),
            pmpm_np_sp = sum((1-emp_flag)*(total_med_primary_np + total_rx_primary_np))/sum((1-emp_flag)*mms_np),
            pmpm_male = sum(sex*(total_med_primary_p + total_rx_primary_p + 
                                   total_med_primary_np + total_rx_primary_np))/sum(sex*mms),
            pmpm_female = sum((1-sex)*(total_med_primary_p + total_rx_primary_p + 
                                     total_med_primary_np + total_rx_primary_np))/sum((1-sex)*mms),
            pmpm_old = sum(age_45*(total_med_primary_p + total_rx_primary_p + 
                                  total_med_primary_np + total_rx_primary_np))/sum(age_45*mms),
            pmpm_young = sum(age_18.45*(total_med_primary_p + total_rx_primary_p + 
                                       total_med_primary_np + total_rx_primary_np))/sum(age_18.45*mms),
            total_primary = sum(total_med_primary_p + total_rx_primary_p + total_med_primary_np + total_rx_primary_np),
            total_secondary = sum(total_med_secondary_p + total_rx_secondary_p + total_med_secondary_np + total_rx_secondary_np),
            total_primary_p = sum(total_med_primary_p + total_rx_primary_p),
            total_secondary_p = sum(total_med_secondary_p + total_rx_secondary_p),
            total_primary_np = sum(total_med_primary_np + total_rx_primary_np),
            total_secondary_np = sum(total_med_secondary_np + total_rx_secondary_np),
            total_primary_emp = sum(emp_flag*(total_med_primary_p + total_rx_primary_p + total_med_primary_np + total_rx_primary_np)),
            total_secondary_emp = sum(emp_flag*(total_med_secondary_p + total_rx_secondary_p + total_med_secondary_np + total_rx_secondary_np)),
            total_primary_sp = sum((1-emp_flag)*(total_med_primary_p + total_rx_primary_p + total_med_primary_np + total_rx_primary_np)),
            total_secondary_sp = sum((1-emp_flag)*(total_med_secondary_p + total_rx_secondary_p + total_med_secondary_np + total_rx_secondary_np)),
            total_primary_p_emp  = sum(emp_flag*(total_med_primary_p + total_rx_primary_p)),
            total_secondary_p_emp  = sum(emp_flag*(total_med_secondary_p + total_rx_secondary_p)),
            total_primary_np_emp  = sum(emp_flag*(total_med_primary_np + total_rx_primary_np)),
            total_secondary_np_emp = sum(emp_flag*(total_med_secondary_np + total_rx_secondary_np)),
            total_primary_p_sp  = sum((1-emp_flag)*(total_med_primary_p + total_rx_primary_p)),
            total_secondary_p_sp  = sum((1-emp_flag)*(total_med_secondary_p + total_rx_secondary_p)),
            total_primary_np_sp = sum((1-emp_flag)*(total_med_primary_np + total_rx_primary_np)),
            total_secondary_np_sp = sum((1-emp_flag)*(total_med_secondary_np + total_rx_secondary_np)),
            total_primary_male = sum(sex*(total_med_primary_p + total_rx_primary_p + total_med_primary_np + total_rx_primary_np)),
            total_secondary_male = sum(sex*(total_med_secondary_p + total_rx_secondary_p + total_med_secondary_np + total_rx_secondary_np)),
            total_primary_female = sum((1-sex)*(total_med_primary_p + total_rx_primary_p + total_med_primary_np + total_rx_primary_np)),
            total_secondary_female = sum((1-sex)*(total_med_secondary_p + total_rx_secondary_p + total_med_secondary_np + total_rx_secondary_np)),
            total_primary_old = sum(age_45*(total_med_primary_p + total_rx_primary_p + total_med_primary_np + total_rx_primary_np)),
            total_secondary_old = sum(age_45*(total_med_secondary_p + total_rx_secondary_p + total_med_secondary_np + total_rx_secondary_np)),
            total_primary_young = sum(age_18.45*(total_med_primary_p + total_rx_primary_p + total_med_primary_np + total_rx_primary_np)),
            total_secondary_young = sum(age_18.45*(total_med_secondary_p + total_rx_secondary_p + total_med_secondary_np + total_rx_secondary_np))) %>%
  mutate(`Diff (P - NP)` = as.character(pmpm_p - pmpm_np),
         `Diff (Emp - Sp)` = as.character(pmpm_emp - pmpm_sp),
         `Diff (P Emp - NP Emp)` = as.character(pmpm_p_emp - pmpm_np_emp),
         `Diff (P Sp - NP Sp)` = as.character(pmpm_p_sp - pmpm_np_sp),
         `Diff (F - M)` = as.character(pmpm_female - pmpm_male),
         `Diff (Younger - Older)` = as.character(pmpm_young - pmpm_old))

#df_claims_by_year$Year <- paste0('year_',as.character(df_claims_by_year$Year,sep=''))

##### Organize Percent Changes Year to Year #####

df_claims_by_year_change <- tibble('Year' = c('Change','Pct Change'),
                                   'pmpm_all' = pct_change(df_claims_by_year, 'pmpm_all'),
                                   'pmpm_p'  = pct_change(df_claims_by_year, 'pmpm_p'),
                                   'pmpm_np'  = pct_change(df_claims_by_year, 'pmpm_np'),
                                   'pmpm_emp'  = pct_change(df_claims_by_year, 'pmpm_emp'),
                                   'pmpm_sp'  = pct_change(df_claims_by_year, 'pmpm_sp'),
                                   'pmpm_p_emp'  = pct_change(df_claims_by_year, 'pmpm_p_emp'),
                                   'pmpm_np_emp'  = pct_change(df_claims_by_year, 'pmpm_np_emp'),
                                   'pmpm_p_sp'  = pct_change(df_claims_by_year, 'pmpm_p_sp'),
                                   'pmpm_np_sp'  = pct_change(df_claims_by_year, 'pmpm_np_sp'),
                                   'pmpm_male' = pct_change(df_claims_by_year, 'pmpm_male'),
                                   'pmpm_female' = pct_change(df_claims_by_year, 'pmpm_female'),
                                   'pmpm_old' = pct_change(df_claims_by_year, 'pmpm_old'),
                                   'pmpm_young' = pct_change(df_claims_by_year, 'pmpm_young'),
                                   'total_primary' = pct_change(df_claims_by_year, 'total_primary'),
                                   'total_primary_p'  = pct_change(df_claims_by_year, 'total_primary_p'),
                                   'total_primary_np'  = pct_change(df_claims_by_year, 'total_primary_np'),
                                   'total_primary_emp'  = pct_change(df_claims_by_year, 'total_primary_emp'),
                                   'total_primary_sp'  = pct_change(df_claims_by_year, 'total_primary_sp'),
                                   'total_primary_p_emp'  = pct_change(df_claims_by_year, 'total_primary_p_emp'),
                                   'total_primary_np_emp'  = pct_change(df_claims_by_year, 'total_primary_np_emp'),
                                   'total_primary_p_sp'  = pct_change(df_claims_by_year, 'total_primary_p_sp'),
                                   'total_primary_np_sp'  = pct_change(df_claims_by_year, 'total_primary_np_sp'),
                                   'total_primary_male' = pct_change(df_claims_by_year, 'total_primary_male'),
                                   'total_primary_female' = pct_change(df_claims_by_year, 'total_primary_female'),
                                   'total_primary_old' = pct_change(df_claims_by_year, 'total_primary_old'),
                                   'total_primary_young' = pct_change(df_claims_by_year, 'total_primary_young'),
                                   'total_secondary' = pct_change(df_claims_by_year, 'total_secondary'),
                                   'total_secondary_p'  = pct_change(df_claims_by_year, 'total_secondary_p'),
                                   'total_secondary_np'  = pct_change(df_claims_by_year, 'total_secondary_np'),
                                   'total_secondary_emp'  = pct_change(df_claims_by_year, 'total_secondary_emp'),
                                   'total_secondary_sp'  = pct_change(df_claims_by_year, 'total_secondary_sp'),
                                   'total_secondary_p_emp'  = pct_change(df_claims_by_year, 'total_secondary_p_emp'),
                                   'total_secondary_np_emp'  = pct_change(df_claims_by_year, 'total_secondary_np_emp'),
                                   'total_secondary_p_sp'  = pct_change(df_claims_by_year, 'total_secondary_p_sp'),
                                   'total_secondary_np_sp'  = pct_change(df_claims_by_year, 'total_secondary_np_sp'),
                                   'total_secondary_male' = pct_change(df_claims_by_year, 'total_secondary_male'),
                                   'total_secondary_female' = pct_change(df_claims_by_year, 'total_secondary_female'),
                                   'total_secondary_old' = pct_change(df_claims_by_year, 'total_secondary_old'),
                                   'total_secondary_young' = pct_change(df_claims_by_year, 'total_secondary_young'),
                                   'Diff (P - NP)'  = c('',''),
                                   'Diff (Emp - Sp)'  = c('',''),
                                   'Diff (P Emp - NP Emp)'  = c('',''),
                                   'Diff (P Sp - NP Sp)'  = c('',''),
                                   'Diff (F - M)'  = c('',''),
                                   'Diff (Younger - Older)'  = c('',''))

##### Build pmpm_by_year #####

df_claims_by_year <- rbind(df_claims_by_year,
                           df_claims_by_year_change)

##### Build totals_by_year #####

totals_by_year <- bind_rows(
  df_claims_by_year %>% 
  select('year' = Year,
         'all' = total_primary,
         'participants' = total_primary_p,
         'non-participants' = total_primary_np,
         'employees' = total_primary_emp,
         'spouses' = total_primary_sp,
         'p-employees' = total_primary_p_emp,
         'np-employees' = total_primary_np_emp,
         'p-spouses' = total_primary_p_sp,
         'np-spouses' = total_primary_np_sp,
         'male' = total_primary_male,
         'female' = total_primary_female,
         'old' = total_primary_old,
         'young' = total_primary_young) %>% 
  mutate(total = ifelse(paid_amount == 'primary_amount', 'Paid', 'Allowed')),
  df_claims_by_year %>% 
    select('year' = Year,
           'all' = total_secondary,
           'participants' = total_secondary_p,
           'non-participants' = total_secondary_np,
           'employees' = total_secondary_emp,
           'spouses' = total_secondary_sp,
           'p-employees' = total_secondary_p_emp,
           'np-employees' = total_secondary_np_emp,
           'p-spouses' = total_secondary_p_sp,
           'np-spouses' = total_secondary_np_sp,
           'male' = total_secondary_male,
           'female' = total_secondary_female,
           'old' = total_secondary_old,
           'young' = total_secondary_young) %>% 
    mutate(total = ifelse(paid_amount == 'secondary_amount', 'Paid', 'Allowed')))



##### Build proc_pmpm_by_year #####

df_proc_claims_by_year <- df_claims %>%
  left_join(df_pii, by='master_id') %>%
  group_by(Year) %>%
  summarize(pmpm_all = sum(total_med_primary_p + total_med_primary_np)/sum(mms_p+mms_np),
            pmpm_p = sum(total_med_primary_p)/sum(mms_p),
            pmpm_np = sum(total_med_primary_np)/sum(mms_np),
            pmpm_emp = sum(emp_flag*(total_med_primary_p + total_med_primary_np))/sum(emp_flag*mms),
            pmpm_sp = sum((1-emp_flag)*(total_med_primary_p + total_med_primary_np))/sum((1-emp_flag)*mms),
            pmpm_p_emp = sum(emp_flag*(total_med_primary_p))/sum(emp_flag*mms_p),
            pmpm_np_emp = sum(emp_flag*(total_med_primary_np))/sum(emp_flag*mms_np),
            pmpm_p_sp = sum((1-emp_flag)*(total_med_primary_p))/sum((1-emp_flag)*mms_p),
            pmpm_np_sp = sum((1-emp_flag)*(total_med_primary_np))/sum((1-emp_flag)*mms_np))

##### Build rx_pmpm_by_year #####

df_rx_claims_by_year <- df_claims %>%
  left_join(df_pii, by='master_id') %>%
  group_by(Year) %>%
  summarize(pmpm_all = sum(total_rx_primary_p + total_rx_primary_np)/sum(mms_p+mms_np),
            pmpm_p = sum(total_rx_primary_p)/sum(mms_p),
            pmpm_np = sum(total_rx_primary_np)/sum(mms_np),
            pmpm_emp = sum(emp_flag*(total_rx_primary_p + total_rx_primary_np))/sum(emp_flag*mms),
            pmpm_sp = sum((1-emp_flag)*(total_rx_primary_p + total_rx_primary_np))/sum((1-emp_flag)*mms),
            pmpm_p_emp = sum(emp_flag*(total_rx_primary_p))/sum(emp_flag*mms_p),
            pmpm_np_emp = sum(emp_flag*(total_rx_primary_np))/sum(emp_flag*mms_np),
            pmpm_p_sp = sum((1-emp_flag)*(total_rx_primary_p))/sum((1-emp_flag)*mms_p),
            pmpm_np_sp = sum((1-emp_flag)*(total_rx_primary_np))/sum((1-emp_flag)*mms_np))

##### Build pmpm_by_quarter #####

df_claims_by_quarter <- df_claims %>%
  left_join(df_pii, by='master_id') %>%
  group_by(Year,Quarter) %>%
  summarize(pmpm_all = sum(total_med_primary_p + total_rx_primary_p + total_med_primary_np + total_rx_primary_np)/sum(mms_p+mms_np),
            pmpm_p = sum(total_med_primary_p + total_rx_primary_p)/sum(mms_p),
            pmpm_np = sum(total_med_primary_np + total_rx_primary_np)/sum(mms_np),
            pmpm_emp = sum(emp_flag*(total_med_primary_p + total_rx_primary_p + 
                                       total_med_primary_np + total_rx_primary_np))/sum(emp_flag*mms),
            pmpm_sp = sum((1-emp_flag)*(total_med_primary_p + total_rx_primary_p + 
                                          total_med_primary_np + total_rx_primary_np))/sum((1-emp_flag)*mms),
            pmpm_p_emp = sum(emp_flag*(total_med_primary_p + total_rx_primary_p))/sum(emp_flag*mms_p),
            pmpm_np_emp = sum(emp_flag*(total_med_primary_np + total_rx_primary_np))/sum(emp_flag*mms_np),
            pmpm_p_sp = sum((1-emp_flag)*(total_med_primary_p + total_rx_primary_p))/sum((1-emp_flag)*mms_p),
            pmpm_np_sp = sum((1-emp_flag)*(total_med_primary_np + total_rx_primary_np))/sum((1-emp_flag)*mms_np))

##### Add delayed values to calculate percentage change #####

#df_delayed <- rbind(df_claims_by_quarter[1,],df_claims_by_quarter[c(1:7),])
#colnames(df_delayed) <- c('Year_d','Quarter_d', 'pmpm_all_d','pmpm_p_d', 'pmpm_np_d', 'pmpm_emp_d', 'pmpm_sp_d', 
#                          'pmpm_p_emp_d', 'pmpm_np_emp_d', 'pmpm_p_sp_d','pmpm_np_sp_d')
#df_claims_by_quarter <- cbind(df_claims_by_quarter, df_delayed)

##### Write Data #####

write_csv(df_claims_by_year, paste0(directory, "Data/Build_Tables/pmpm_by_year.csv"))
write_csv(totals_by_year, paste0(directory, "Data/Build_Tables/totals_by_year.csv"))
write_csv(df_proc_claims_by_year, paste0(directory, "Data/Build_Tables/proc_pmpm_by_year.csv"))
write_csv(df_rx_claims_by_year, paste0(directory, "Data/Build_Tables/rx_pmpm_by_year.csv"))
#write_csv(df_claims_by_quarter, paste0(directory, "Data/Build_Tables/pmpm_by_quarter.csv"))

print('totals_by_year written to Data/Build_Tables')
print("pmpm_by_year written to Data/Build_Tables")
print("proc_pmpm_by_year written to Data/Build_Tables")
print("rx_pmpm_by_year written to Data/Build_Tables")
#print("pmpm_by_quarter written to Data/Build_Tables")

totals_by_year -> totals_by_year_tab
df_claims_by_year -> pmpm_by_year_tab
df_proc_claims_by_year -> proc_pmpm_by_year_tab
df_rx_claims_by_year -> rx_pmpm_by_year_tab
#df_claims_by_quarter -> pmpm_by_quarter_tab

rm("df_claims", "df_claims_by_quarter", "df_rx_claims_by_year", "df_proc_claims_by_year", 
   'df_claims_by_year', "df_claims_by_year_change", "df_pii", 'totals_by_year')
