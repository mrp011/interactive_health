### Aditional Tables 

### Participation Year over Year

pct_change <- function(x){
  x <- counts_yr %>% select_(x) %>% collect %>% .[[1]]
  change <- mean(diff(x))
  pct <- change/x[1]
  return(pct)
}

all_humans <- assign_table("human_flags_tab", "Data/Sub_Tables/human_flags.csv")
phs <- assign_table("phs_tab", "Data/Sub_Tables/phs_analytics.csv")

phs <- phs %>% filter(assessment != min(assessment)) %>% mutate(Year = year(assessment)) %>%
  select(master_id, Year, phs) %>%
  left_join(all_humans) %>%
  filter(year(cov_start_dt) <= Year,
         year(cov_end_dt) >= Year)
  

data_full <- tibble('master_id' = rep(all_humans$master_id, each = length(seq(from = analysis_start, to = analysis_end, by='year'))),
                    'cov_start_dt' = rep(all_humans$cov_start_dt, each = length(seq(from = analysis_start, to = analysis_end, by = 'year'))),
                    'cov_end_dt' = rep(all_humans$cov_end_dt, each = length(seq(from = analysis_start, to = analysis_end, by = 'year'))),
                    'emp_flag' = rep(all_humans$emp_flag, each = length(seq(from = analysis_start, to = analysis_end, by = 'year'))),
                    'Year' = rep(year(seq(from = analysis_start, to = analysis_end, by='year')), nrow(all_humans))) %>%
  filter(year(cov_start_dt) <= Year,
         year(cov_end_dt) >= Year) %>%
  left_join(phs) %>%
  mutate(phs_flag = as.numeric(!is.na(phs)),
         high_risk = as.numeric(phs > 25),
         mod_risk = as.numeric(phs > 0 & phs <= 25),
         low_risk = as.numeric(phs <= 0))

counts_yr <- data_full %>% group_by(Year) %>% 
  summarise(all_humans = n(), 
            participants = sum(phs_flag),
            employees = sum(emp_flag),
            high_risk = sum(high_risk, na.rm = TRUE),
            mod_risk = sum(mod_risk, na.rm = TRUE),
            low_risk = sum(low_risk, na.rm = TRUE)) %>% ungroup() %>%
  mutate(non_particpants = all_humans - participants, 
         spouses = all_humans - employees,
         Year = as.character(Year))

trends <- counts_yr %>% bind_rows(tibble('Year' = 'Trend',
                                         'all_humans' = pct_change('all_humans'),
                                         'participants' = pct_change('participants'),
                                         'employees' = pct_change('employees'),
                                         'high_risk' = pct_change('high_risk'),
                                         'mod_risk' = pct_change('mod_risk'),
                                         'low_risk' = pct_change('low_risk'),
                                         'non_particpants' = pct_change('non_particpants'),
                                         'spouses' = pct_change('spouses')))

write_csv(trends, "Data/Build_Tables/membership_levels.csv")

membership_trends_tab <- trends

rm("trends", "counts_yr", "data_full", 'phs', 'all_humans', "pct_change")


#### Trend Summary Demographics

all_humans <- assign_table("human_flags_tab", "Data/Sub_Tables/human_flags.csv")
phs <- assign_table("phs_tab", "Data/Sub_Tables/phs_analytics.csv")

phs <- phs %>% filter(assessment == max(assessment)) %>% select(master_id)

all_flags <- all_humans %>% 
  filter(cov_end_dt > analysis_date | is.na(cov_end_dt)) %>% 
  transmute(master_id = master_id, 
            employment = ifelse(emp_flag, 'Employee', 'Spouse'), 
            sex = ifelse(sex, 'Female', 'Male'),
            age = ifelse(age_45, 'Age 45 and Older', 'Under 45'),
            participation = ifelse(master_id %in% phs$master_id, 'Participant', 'Non-Participant'))

total_demographics <- bind_cols(
  all_flags %>% group_by(employment) %>% summarise(emp_percent = n()/dim(all_flags)[1]) %>% ungroup(),
  all_flags %>% group_by(sex) %>% summarise(sex_percent = n()/dim(all_flags)[1]) %>% ungroup(),
  all_flags %>% group_by(age) %>% summarise(age_percent = n()/dim(all_flags)[1]) %>% ungroup(),
  all_flags %>% group_by(participation) %>% summarise(part_percent = n()/dim(all_flags)[1]) %>% ungroup()
)

write_csv(total_demographics, "Data/Build_Tables/trend_summary_demographics.csv")

total_demographics_tab <- total_demographics

rm("total_demographics", "all_flags", 'phs', 'all_humans')







