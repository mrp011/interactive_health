############ Level 4 #############

### 

### Input Tables: phs_analytics
###               human_flags
###               claims_per_member

### Output Tables: totals_by_year_cohorts
###                pmpm_by_year_cohorts

### Author: Michelle Powell

### Sourced By: claim_cohorts()


###########################################
###########################################

##### Functions #####

get_phs_cohort <- function(phs_level_start, phs_level_end){
  get_phs_cohort_1 <- function(phs_level, period){
    i <- case_when(period == 'start' ~ 1,
                   period == 'end' ~ as.numeric(length(get_assessments())))
    left <- case_when(phs_level == 'high' ~ 25.1,
                      phs_level %in% c('moderate', 'mod', 'med', 'mid') ~ 0.1,
                      phs_level == 'low' ~ -100)
    right <- case_when(phs_level == 'high' ~ 300,
                       phs_level %in% c('moderate', 'mod', 'med', 'mid') ~ 25,
                       phs_level == 'low' ~ 0)
    x <- phs %>%
      right_join(full_participants) %>%
      filter(assessment == get_assessments()[i], between(phs, left, right)) %>% distinct(master_id)
    return(x)
  }
  x <- get_phs_cohort_1(phs_level_start, 'start')
  y <- get_phs_cohort_1(phs_level_end, 'end')
  z <- inner_join(x, y) %>% collect %>% .[[1]]
}

pct_change_cohort <- function(x){
  x <- claims_by_year_cohorts %>% select_(x) %>% collect %>% .[[1]]
  change <- mean(diff(x))
  pct <- change/x[1]
  return(c(change,pct))
}

#### Read In Data #####

human_flags <- assign_table("human_flags_tab", "Data/Sub_Tables/human_flags.csv")  
phs <- assign_table("phs_tab", "Data/Sub_Tables/phs_analytics.csv")
claims_per_member <- assign_table("claims_per_member_tab", "Data/Sub_Tables/claims_per_member.csv")

full_members <- human_flags %>% filter(cov_start_dt <= analysis_start, cov_end_dt >= analysis_date | is.na(cov_end_dt)) 

if(dim(full_members)[1] == 0){
  full_members <- human_flags %>% filter(cov_start_dt <= analysis_start, cov_end_dt == max(cov_end_dt) | is.na(cov_end_dt))   
}

full_participants <- phs %>% group_by(master_id) %>% summarise(ct = n()) %>% ungroup() %>%
  filter(ct == length(get_assessments())) %>% distinct(master_id) %>% inner_join(full_members)

full_non_participants <- full_members %>% anti_join(phs, by = 'master_id')

low_low <- get_phs_cohort('low', 'low')
low_mod <- get_phs_cohort('low', 'moderate')
low_high <- get_phs_cohort('low', 'high')

mod_low <- get_phs_cohort('mod', 'low')
mod_mod <- get_phs_cohort('mod', 'moderate')
mod_high <- get_phs_cohort('mod', 'high')

high_low <- get_phs_cohort('high', 'low')
high_mod <- get_phs_cohort('high', 'moderate')
high_high <- get_phs_cohort('high', 'high')

improved <- c(mod_low, high_mod, high_low)
fallen <- c(low_mod, mod_high, low_high)
maintained <- c(mod_mod, high_high)

##### Add cohort flags to claims #####

claims_per_member_cohorts <- claims_per_member %>%
  mutate(full_member = master_id %in% full_members$master_id,
         full_participant  = master_id %in% full_participants$master_id,
         full_non_participant = master_id %in% full_non_participants$master_id,
         low_low = master_id %in% low_low,
         improved = master_id %in% improved,
         fallen = master_id %in% fallen,
         maintained = master_id %in% maintained,
         total_primary = total_med_primary_p + total_rx_primary_p + total_med_primary_np + total_rx_primary_np,
         total_secondary = total_med_secondary_p + total_rx_secondary_p + total_med_secondary_np + total_rx_secondary_np) %>% 
  filter(full_member)

##### Calculate totals and pmpms for each cohort #####

claims_by_year_cohorts <- claims_per_member_cohorts %>%
  left_join(human_flags, by='master_id') %>%
  group_by(Year) %>%
  summarize(pmpm_all = sum(total_primary)/sum(mms),
            pmpm_p = sum(full_participant*total_primary)/sum(full_participant*mms),
            pmpm_np = sum(full_non_participant*total_primary)/sum(full_non_participant*mms),
            pmpm_low_low = sum(total_primary*low_low)/sum(mms*low_low),
            pmpm_improved = sum(total_primary*improved)/sum(mms*improved),
            pmpm_fallen = sum(total_primary*fallen)/sum(mms*fallen),
            pmpm_maintained = sum(total_primary*maintained)/sum(mms*maintained)) %>%
  mutate(`Diff (P - NP)` = as.character(pmpm_p - pmpm_np))

##### Build PMPM PHS Cohorts #####

pmpm_phs_cohorts <- claims_by_year_cohorts %>% 
  select(Year, pmpm_p, pmpm_low_low, pmpm_improved, pmpm_fallen, pmpm_maintained) %>%
  melt(id.vars = 'Year') %>%
  
  select(Year, 'cohort' = variable, 'pmpm' = value) %>%
  mutate_at(vars(pmpm), funs(ifelse(is.na(.), 0, .)))

  #mutate(start_state = gsub('.+_(.+)_.+', '\\1', variable),
  #       end_state = gsub('.+_.+_(.+)', '\\1', variable)) %>%
  #mutate(start_state = case_when(.$start_state == 'low' ~ 1,
  #                               .$start_state == 'mod' ~ 2,
  #                               .$start_state == 'high' ~ 3,
  #                               .$start_state == 'pmpm_p' ~ 4),
  #       end_state = case_when(.$end_state == 'low' ~ 1,
  #                             .$end_state == 'mod' ~ 2,
  #                             .$end_state == 'high' ~ 3,
  #                             .$end_state == 'pmpm_p' ~ 4)) %>%
  #mutate(color = case_when(abs(.$Year - year(analysis_start)) <= abs(.$Year - year(get_assessments()[length(get_assessments())])) ~ .$start_state,
  #                         abs(.$Year - year(analysis_start)) > abs(.$Year - year(get_assessments()[length(get_assessments())])) ~ .$end_state)) %>%
  #select(Year, 'cohort' = variable, 'pmpm' = value, 'color' = color) %>%
  #mutate_all(funs(ifelse(is.na(.), 0, .)))

##### Calculate Trend #####

claims_by_year_cohorts_change <- tibble('Year' = c('Change','Pct Change'),
                                        'pmpm_all' = pct_change_cohort('pmpm_all'),
                                        'pmpm_p'  = pct_change_cohort('pmpm_p'),
                                        'pmpm_np'  = pct_change_cohort('pmpm_np'),
                                        'pmpm_low_low' = pct_change_cohort('pmpm_low_low'),
                                        'pmpm_improved' = pct_change_cohort('pmpm_improved'),
                                        'pmpm_fallen' = pct_change_cohort('pmpm_fallen'),
                                        'pmpm_maintained' = pct_change_cohort('pmpm_maintained'),
                                        'Diff (P - NP)'  = c('',''))

##### Calculate Cohort Size #####

claims_by_year_cohorts_counts <- tibble('Year' = c('Cohort Size'),
                                        'pmpm_all' = dim(full_members)[1],
                                        'pmpm_p'  = dim(full_participants)[1],
                                        'pmpm_np'  = dim(full_non_participants)[1],
                                        'pmpm_low_low' = length(low_low),
                                        'pmpm_improved' = length(improved),
                                        'pmpm_fallen' = length(fallen),
                                        'pmpm_maintained' = length(maintained)) %>%
  mutate(`Diff (P - NP)` = as.character(pmpm_p - pmpm_np))

##### Build claims_by_year_cohorts #####

claims_by_year_cohorts <- rbind(claims_by_year_cohorts,
                                claims_by_year_cohorts_change,
                                claims_by_year_cohorts_counts) %>%
  mutate_all(funs(ifelse(is.na(.), 0, .)))

##### Write Data #####

write_csv(claims_by_year_cohorts, paste0(directory, "Data/Build_Tables/pmpm_by_year_cohorts.csv"))
write_csv(pmpm_phs_cohorts, paste0(directory, "Data/Build_Tables/pmpm_phs_cohorts.csv"))

print("pmpm_by_year_cohorts written to Data/Build_Tables")
print("pmpm_phs_cohorts written to Data/Build_Tables")

claims_by_year_cohorts -> pmpm_by_year_cohorts_tab
pmpm_phs_cohorts -> pmpm_phs_cohorts_tab

rm('claims_by_year_cohorts', 'claims_by_year_cohorts_change', 'claims_per_member_cohorts', 
   'human_flags', 'phs', 'claims_per_member', 'full_participants', 'full_non_participants', 'pmpm_phs_cohorts',
   'full_members', 'low_low', 'low_mod', 'low_high', 'mod_low', 'mod_mod', 'mod_high', 'high_low', 'high_mod', 'high_high')
