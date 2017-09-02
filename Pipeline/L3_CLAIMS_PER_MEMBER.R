############ Level 3 #############

### account for all claims for each member in each 
### month/quarter/year, split up in different 
### categories (total, preventive, Rx, other medical). 
### This table will help feed all the PMPM trend lines.

### Input Tables: phs_analytics
###               human_flags
###               claims_analytics
###               rx_analytics

### Output Tables: claims_per_member

### Author: Peter Cooman

### Sourced By: claims_per_member()


###########################################
###########################################

##### Read In Data #####

df_pii <- assign_table("human_flags_tab", "Data/Sub_Tables/human_flags.csv")
df_pii$cov_end_dt[is.na(df_pii$cov_end_dt)] <- analysis_date

df_phs <- assign_table("phs_tab" ,"Data/Sub_Tables/phs_analytics.csv")

df_proc <- assign_table("claims_tab" ,"Data/Sub_Tables/claims_analytics.csv") %>%
  arrange(master_id) %>%
  mutate('Year' = year(start_dt),
         'Month' = month(start_dt),
         'Quarter' = quarter(start_dt)
         ) %>%
  select(master_id, Year, Quarter, Month, start_dt, primary_amount, secondary_amount)

df_rx <- assign_table("rx_tab" ,"Data/Sub_Tables/rx_analytics.csv") %>%
  arrange(master_id) %>%
  mutate('Year' = year(fill_dt),
         'Month' = month(fill_dt),
         'Quarter' = quarter(fill_dt)
         ) %>%
  select(master_id, Year, Quarter, Month, fill_dt, primary_amount, secondary_amount)

##### Build Empty Tables #####

df_claims_pm <- tibble('master_id' = rep(df_pii$master_id, each = length(seq(from=analysis_start, to=analysis_end, by='month'))),
                       'Year' = rep(year(seq(from=analysis_start, to=analysis_end, by='month')),
                                    nrow(df_pii)),
                       'Quarter' = rep(quarter(seq(from=analysis_start, to=analysis_end, by='month')),nrow(df_pii)),
                       'Month' = rep(month(seq(from=analysis_start, to=analysis_end, by='month')),
                                     nrow(df_pii)),
                       'total_med_primary_p' = rep(0, length(seq(from=analysis_start, to=analysis_end, by='month'))*nrow(df_pii)),
                       'total_med_primary_np' = rep(0, length(seq(from=analysis_start, to=analysis_end, by='month'))*nrow(df_pii)),
                       'total_med_secondary_p' = rep(0, length(seq(from=analysis_start, to=analysis_end, by='month'))*nrow(df_pii)),
                       'total_med_secondary_np' = rep(0, length(seq(from=analysis_start, to=analysis_end, by='month'))*nrow(df_pii)),
                       'total_rx_primary_p' = rep(0, length(seq(from=analysis_start, to=analysis_end, by='month'))*nrow(df_pii)),
                       'total_rx_primary_np' = rep(0, length(seq(from=analysis_start, to=analysis_end, by='month'))*nrow(df_pii)),
                       'total_rx_secondary_p' = rep(0, length(seq(from=analysis_start, to=analysis_end, by='month'))*nrow(df_pii)),
                       'total_rx_secondary_np' = rep(0, length(seq(from=analysis_start, to=analysis_end, by='month'))*nrow(df_pii)),
                       'days_p' = rep(0, length(seq(from=analysis_start, to=analysis_end, by='month'))*nrow(df_pii)),
                       'days' = rep(0, length(seq(from=analysis_start, to=analysis_end, by='month'))*nrow(df_pii)))

##### Loop Through to Fill Table #####

# For each assessment period, match everybody to the PHS table
# Those who have the correct health assessment date anywhere in their PHS rows, get a Participation Flag = 1, 
# to indicate they are a Participant for THIS period.
# Extend their id's with the calendar (--> 24 rows each)
# Match this to the Procedures and Rx tables
# We use the part_flag to determine whether a Procedure or Rx cost should be counted towards the 'Total as P' or 'Total as NP' column
# We can do the same for the 'days_p', we first construct an interval that spans a particular month, then calculate the number of 
# days of interval between that month and the health assessment period we're currently analyzing.
# Again, the part_flag helps us to determine whether these should be counted as Participant days.
# Now, some months will have no procedures or Rx, while other months will have multiple. So we sum all Procedures and Rx as P/NP
# for each combination of id, year, quarter and month.
# This completes the analysis for one assessment period. We need to repeat this for all periods, taking the union of the different periods.
# Again, if somebody was a participant during multiple periods, there may be multiple rows per id,year,quarter,month combination
# so we do a finalgroping along these combinations and calculate the final sums.

assessments <- get_assessments(add_end = TRUE)

for (i in 1:(length(assessments)-1)) {
  
  # join PII to PHS to determine whether somebody was a P during this period
  df_pii_filt <- df_pii %>%
    left_join(df_phs, by = 'master_id') %>%
    mutate(assessment_found = (assessment == assessments[i])) %>%
    group_by(master_id, cov_start_dt, cov_end_dt) %>%
    summarize('part_flag' = any(assessment_found, na.rm=TRUE)) %>% ungroup()
  
  # create year, quarter, month table
  df_claims_pm_new <- tibble('master_id' = rep(df_pii_filt$master_id, each = length(seq(from=analysis_start, to=analysis_end, by='month'))),
                             'part_flag' = rep(df_pii_filt$part_flag, each = length(seq(from=analysis_start, to=analysis_end, by='month'))),
                             'cov_start_dt' = rep(df_pii_filt$cov_start_dt, each = length(seq(from=analysis_start, to=analysis_end, by='month'))),
                             'cov_end_dt' = rep(df_pii_filt$cov_end_dt, each = length(seq(from=analysis_start, to=analysis_end, by='month'))),
                             'Year' = rep(year(seq(from=analysis_start, to=analysis_end, by='month')), nrow(df_pii_filt)),
                             'Quarter' = rep(quarter(seq(from=analysis_start, to=analysis_end, by='month')), nrow(df_pii_filt)),
                             'Month' = rep(month(seq(from=analysis_start, to=analysis_end, by='month')), nrow(df_pii_filt)))
  
  # filter the procedures
  df_proc_filt <- df_proc %>%
    filter(start_dt >= assessments[i], start_dt < assessments[i+1]) %>%
    group_by(master_id, Year, Quarter, Month) %>%
    summarize('total_med_primary' = sum(primary_amount, na.rm = TRUE),
              'total_med_secondary' = sum(secondary_amount, na.rm = TRUE)) %>% ungroup()
  
  # filter the Rx
  df_rx_filt <- df_rx %>%
    filter(fill_dt >= assessments[i], fill_dt < assessments[i+1]) %>%
    group_by(master_id, Year, Quarter, Month) %>%
    summarize('total_rx_primary' = sum(primary_amount, na.rm = TRUE),
              'total_rx_secondary' = sum(secondary_amount, na.rm = TRUE)) %>% ungroup()
  
  # the current health assessment period
  inter <- interval(assessments[i], assessments[i+1])
  
  # the claims per member per month for this assessment period
  df_claims_pm_new <- df_claims_pm_new %>%
    mutate('days_p' = ifelse(cov_start_dt > assessments[i],
                                         part_flag*lubridate::intersect(interval(mdy(paste0(Month,'-01-',Year)),
                                                                                          mdy(paste0(Month,'-01-',Year)) + months(1)),
                                                                                 interval(cov_start_dt, assessments[i+1]))/ddays(1),
                                         part_flag*lubridate::intersect(interval(mdy(paste0(Month,'-01-',Year)),
                                                                                          mdy(paste0(Month,'-01-',Year)) + months(1)),
                                                                                 inter)/ddays(1)),
           'days' = lubridate::intersect(interval(mdy(paste0(Month,'-01-',Year,sep='')),
                                                                        mdy(paste0(Month,'-01-',Year,sep='')) + months(1)),
                                                               interval(ymd(cov_start_dt), ymd(cov_end_dt)))/ddays(1)) %>%
    group_by(master_id, Year, Quarter, Month, part_flag) %>%
    summarise(days_p = sum(days_p),
              days = sum(days)) %>% ungroup() %>%
    left_join(df_proc_filt, by = c('master_id', 'Year', 'Quarter','Month')) %>% 
    left_join(df_rx_filt, by = c('master_id', 'Year', 'Quarter', 'Month')) %>% 
    group_by(master_id, Year, Quarter, Month) %>%
    # filter(!is.infinite(days_p)) %>%
    summarize('total_med_primary_p' = sum(part_flag*total_med_primary, na.rm = TRUE),
              'total_med_primary_np' = sum((1-part_flag)*total_med_primary, na.rm = TRUE),
              'total_med_secondary_p' = sum(part_flag*total_med_secondary, na.rm = TRUE),
              'total_med_secondary_np' = sum((1-part_flag)*total_med_secondary, na.rm = TRUE),
              'total_rx_primary_p' = sum(part_flag*total_rx_primary, na.rm = TRUE),
              'total_rx_primary_np' = sum((1-part_flag)*total_rx_primary, na.rm = TRUE),
              'total_rx_secondary_p' = sum(part_flag*total_rx_secondary, na.rm = TRUE),
              'total_rx_secondary_np' = sum((1-part_flag)*total_rx_secondary, na.rm = TRUE),
              'days_p' = max(days_p, na.rm = TRUE),
              'days' = max(days, na.rm = TRUE)) %>% ungroup()
  
  ## Test: Sum of P and NP should total Sum of primary_amount and secondary_amount in Procedures table and Rx table
  print(paste0("period: ", i))
  print(paste0("Medical primary amount: ", sum(df_proc_filt$total_med_primary,na.rm=TRUE), " = ", 
        sum(df_claims_pm_new$total_med_primary_p,na.rm=TRUE)+sum(df_claims_pm_new$total_med_primary_np,na.rm=TRUE),
        " - ", sum(df_proc_filt$total_med_primary,na.rm=TRUE) == sum(df_claims_pm_new$total_med_primary_p,na.rm=TRUE)+sum(df_claims_pm_new$total_med_primary_np,na.rm=TRUE)))
  print(paste0("Medical secondary amount: ", sum(df_proc_filt$total_med_secondary,na.rm=TRUE), " = ", 
        sum(df_claims_pm_new$total_med_secondary_p,na.rm=TRUE)+sum(df_claims_pm_new$total_med_secondary_np,na.rm=TRUE),
        " - ", sum(df_proc_filt$total_med_secondary,na.rm=TRUE) == sum(df_claims_pm_new$total_med_secondary_p,na.rm=TRUE)+sum(df_claims_pm_new$total_med_secondary_np,na.rm=TRUE)))
  print(paste0("Rx primary amount: ", sum(df_rx_filt$total_rx_primary,na.rm=TRUE), " = ", 
        sum(df_claims_pm_new$total_rx_primary_p,na.rm=TRUE) + sum(df_claims_pm_new$total_rx_primary_np,na.rm=TRUE),
        " - ", sum(df_rx_filt$total_rx_primary,na.rm=TRUE) == sum(df_claims_pm_new$total_rx_primary_p,na.rm=TRUE) + sum(df_claims_pm_new$total_rx_primary_np,na.rm=TRUE)))
  print(paste0("Rx secondary amount: ", sum(df_rx_filt$total_rx_secondary,na.rm=TRUE), " = ", 
        sum(df_claims_pm_new$total_rx_secondary_p,na.rm=TRUE) + sum(df_claims_pm_new$total_rx_secondary_np,na.rm=TRUE), 
        " - ", sum(df_rx_filt$total_rx_secondary,na.rm=TRUE) == sum(df_claims_pm_new$total_rx_secondary_p,na.rm=TRUE) + sum(df_claims_pm_new$total_rx_secondary_np,na.rm=TRUE)))
  
  # add the claims per member per month of the current period to the old set
  # and re-aggregate
  df_claims_pm <- union_all(df_claims_pm,df_claims_pm_new) %>%
    filter(!is.infinite(days_p)) %>%
    group_by(master_id, Year, Quarter, Month) %>%
    summarize('total_med_primary_p' = sum(total_med_primary_p),
              'total_med_primary_np' = sum(total_med_primary_np),
              'total_med_secondary_p' = sum(total_med_secondary_p),
              'total_med_secondary_np' = sum(total_med_secondary_np),
              'total_rx_primary_p' = sum(total_rx_primary_p),
              'total_rx_primary_np' = sum(total_rx_primary_np),
              'total_rx_secondary_p' = sum(total_rx_secondary_p),
              'total_rx_secondary_np' = sum(total_rx_secondary_np),
              'days_p' = sum(days_p,na.rm=TRUE),
              'days' = max(days,na.rm=TRUE)) %>% ungroup()
}

##### Finish claims_per_member #####

df_claims_pm <- df_claims_pm %>%
  mutate('days_p' = ifelse(days_p > days, days, days_p),
         'days_np' = days - days_p) %>%
  mutate('mms' = days/(days_in_month(Month) + leap_year(Year)*(Month == 2)),
         'mms_p' = days_p/(days_in_month(Month) + leap_year(Year)*(Month == 2)),
         'mms_np' = days_np/(days_in_month(Month) + leap_year(Year)*(Month == 2)))

##### Write Data #####

write_csv(df_claims_pm, paste0(directory, "Data/Sub_Tables/claims_per_member.csv"))
print("claims_per_member written to Data/Sub_Tables")

claims_per_member_tab <- df_claims_pm

rm("df_pii", "df_phs", "df_rx", "df_proc", "df_claims_pm", "assessments", "df_pii_filt", 
   "df_claims_pm_new", "df_proc_filt", "df_rx_filt", "inter", "i")
