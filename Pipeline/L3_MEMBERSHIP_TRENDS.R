############ Level 3 #############

### Build Membership Tables

### Input Tables: human_flags_tab
###               phs_analytics

### Output Tables: cost_avoidance
###                cost_avoidance_prev

### Author: Michelle Powell

### Sourced By: membership()

###########################################
###########################################
options(scipen = 999)
##### Functions #####

pct_change <- function(x, pct = FALSE){
  x <- trends %>% select_(x) %>% collect %>% .[[1]]
  change <- mean(diff(x))
  percent <- ifelse(pct, change, change/x[1])
  return(percent)
}

##### Read In Data #####

all_humans <- assign_table("human_flags_tab", "Data/Sub_Tables/human_flags.csv")
phs <- assign_table("phs_tab", "Data/Sub_Tables/phs_analytics.csv")

phs <- phs %>% filter(participation_year >= year(analysis_start)) %>%
  select(master_id, "Year" = participation_year, phs) %>%
  left_join(all_humans) %>%
  filter(year(cov_start_dt) <= Year,
         year(cov_end_dt) >= Year | is.na(cov_end_dt))
  
##### Processing #####

data_full <- tibble('master_id' = rep(all_humans$master_id, each = length(seq(from = analysis_start, to = analysis_end, by='year'))+1),
                    'cov_start_dt' = rep(all_humans$cov_start_dt, each = length(seq(from = analysis_start, to = analysis_end, by = 'year'))+1),
                    'cov_end_dt' = rep(all_humans$cov_end_dt, each = length(seq(from = analysis_start, to = analysis_end, by = 'year'))+1),
                    'emp_flag' = rep(all_humans$emp_flag, each = length(seq(from = analysis_start, to = analysis_end, by = 'year'))+1),
                    'Date' = rep(c(seq(from = analysis_start, to = analysis_end, by='year'), analysis_date), nrow(all_humans))) %>%
  mutate(Year = ifelse(Date != analysis_date, year(Date), year(analysis_end))) %>%
  mutate(current = ifelse((Date != analysis_date) | (Date == analysis_date &
                            (cov_end_dt >= analysis_date | is.na(cov_end_dt)) & 
                            (cov_start_dt <= analysis_date)), TRUE, FALSE)) %>%
  filter(current) %>%
  left_join(phs) %>%
  mutate(phs_flag = as.numeric(!is.na(phs)),
         high_risk = as.numeric(phs > 25),
         mod_risk = as.numeric(phs > 0 & phs <= 25),
         low_risk = as.numeric(phs <= 0)) %>%
  mutate(Year = year(Date)) %>%
  filter(year(cov_start_dt) <= Year, 
         year(cov_end_dt) >= Year | is.na(cov_end_dt)) 

counts_yr <- data_full %>% group_by(Date) %>% 
  summarise(Year = min(Year),
            all_humans = n(), 
            participants = sum(phs_flag),
            employees = sum(emp_flag),
            high_risk = sum(high_risk, na.rm = TRUE),
            mod_risk = sum(mod_risk, na.rm = TRUE),
            low_risk = sum(low_risk, na.rm = TRUE)) %>% ungroup() %>%
  mutate(non_particpants = all_humans - participants, 
         spouses = all_humans - employees,
         Year = ifelse(Year == max(Year), "Today", as.character(Year)))

trends <- counts_yr %>% mutate_at(vars(participants, employees, high_risk, mod_risk, low_risk, non_particpants, spouses),
                                  funs(pct = ./all_humans))

trends_today <- trends %>% filter(Year == "Today")

trends <- trends %>% filter(Year != "Today")

##### Build Trend Charts #####

trends <- trends %>% bind_rows(trends_today,
                               tibble('Year' = 'Trend',
                                      'all_humans' = pct_change('all_humans'),
                                      'participants' = pct_change('participants'),
                                      'employees' = pct_change('employees'),
                                      'high_risk' = pct_change('high_risk'),
                                      'mod_risk' = pct_change('mod_risk'),
                                      'low_risk' = pct_change('low_risk'),
                                      'non_particpants' = pct_change('non_particpants'),
                                      'spouses' = pct_change('spouses'),
                                      'participants_pct' = pct_change('participants_pct', TRUE),
                                      'employees_pct' = pct_change('employees_pct', TRUE),
                                      'high_risk_pct' = pct_change('high_risk_pct', TRUE),
                                      'mod_risk_pct' = pct_change('mod_risk_pct', TRUE),
                                      'low_risk_pct' = pct_change('low_risk_pct', TRUE),
                                      'non_particpants_pct' = pct_change('non_particpants_pct', TRUE),
                                      'spouses_pct' = pct_change('spouses_pct', TRUE)))

##### Write Data #####

write_csv(trends, paste0(directory, "Data/Build_Tables/membership_levels.csv"))
print("membership_levels written to Data/Build_Tables")

membership_trends_tab <- trends

rm("trends", "counts_yr", "data_full", 'phs', 'all_humans', "pct_change")
