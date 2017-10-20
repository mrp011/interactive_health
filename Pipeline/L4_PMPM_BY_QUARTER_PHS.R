############ Level 4 #############

### Feeds PHS by risk group Tableau Page

### Input Tables: claims_per_member
###               phs_analytics

### Output Tables: pmpm_by_year_phs
###                pmpm_by_quarter_ph
###                pmpm_metgoals_by_year

### Author: Peter Cooman

### Sourced By: pmpm_by_quarter_phs()


###########################################
###########################################

##### Functions #####

pct_change <- function(data, x){
  x <- data %>% select_(x) %>% collect %>% .[[1]]
  change <- mean(diff(x))
  pct <- change/x[1]
  return(c(change,pct))
}

##### Read In Data, Filter, Group #####

phs <- assign_table("phs_tab", "Data/Sub_Tables/phs_analytics.csv")
max_assessment <- max(year(phs$assessment))
phs <- phs %>%
  mutate(met_goal = ifelse(year(assessment) != max_assessment | met_goal == 'N/A', 'Z', met_goal)) %>%
  group_by(master_id) %>%
  summarise(phs = last(phs, order_by = order(assessment)),
            met_goal = min(met_goal, na.rm = TRUE)) %>%
  mutate(met_goal_yes = ifelse(met_goal == 'Y', 1, 0),
         met_goal_no = ifelse(met_goal == 'N', 1, 0),
         high_risk = ifelse(phs > 25, 1, 0),
         mod_risk = ifelse(phs <= 25 & phs > 0, 1, 0),
         low_risk = ifelse(phs <= 0, 1, 0)) %>% ungroup()

claims <- assign_table("claims_per_member_tab", "Data/Sub_Tables/claims_per_member.csv") %>%
  inner_join(phs, by = 'master_id') %>% 
  transmute(Year = Year, Quarter = Quarter, Month = Month, master_id = master_id,
            high_risk = high_risk, mod_risk = mod_risk, low_risk = low_risk, 
            met_goal_yes = met_goal_yes, met_goal_no = met_goal_no,
            total_med_primary = total_med_primary_p,
            total_med_secondary = total_med_secondary_p,
            total_rx_primary = total_rx_primary_p,
            total_rx_secondary = total_rx_secondary_p,
            total_primary = total_med_primary_p + total_rx_primary_p,
            total_secondary = total_med_secondary_p + total_rx_secondary_p,
            mms = mms_p)

##### Build pmpm_by_quarter_phs #####

df_claims_by_quarter <- claims %>%
  group_by(Year, Quarter) %>%
  summarize(pmpm_high = sum(total_primary*high_risk)/sum(mms*high_risk),
            pmpm_moderate = sum(total_primary*mod_risk)/sum(mms*mod_risk),
            pmpm_low = sum(total_primary*low_risk)/sum(mms*low_risk),
            pmpm_MetGoal = sum(total_primary*met_goal_yes)/sum(mms*met_goal_yes),
            pmpm_DidNotMeetGoal = sum(total_primary*met_goal_no)/sum(mms*met_goal_no))

##### Build pmpm_by_year_phs #####

df_total_claims_by_year <- claims %>%
  group_by(Year) %>%
  summarize(pmpm_high = sum(total_primary*high_risk)/sum(mms*high_risk),
            pmpm_moderate = sum(total_primary*mod_risk)/sum(mms*mod_risk),
            pmpm_low = sum(total_primary*low_risk)/sum(mms*low_risk)) 

df_total_claims_by_year <- df_total_claims_by_year %>% mutate(Year = as.character(Year)) %>%
  bind_rows(tibble("Year" = c("Change", "Percent_Change"), 
                   "pmpm_high" = pct_change(df_total_claims_by_year, "pmpm_high"),
                   "pmpm_moderate" = pct_change(df_total_claims_by_year, "pmpm_moderate"),
                   "pmpm_low" = pct_change(df_total_claims_by_year, "pmpm_low")))

##### Build pmpm_metgoals_by_year #####

df_metgoals_by_year <- claims %>%
  group_by(Year) %>%
  summarize(pmpm_MetGoal = sum(total_primary*met_goal_yes)/sum(mms*met_goal_yes),
            pmpm_DidNotMeetGoal = sum(total_primary*met_goal_no)/sum(mms*met_goal_no))

df_metgoals_by_year <- df_metgoals_by_year %>% mutate(Year = as.character(Year)) %>%
  bind_rows(tibble("Year" = c("Change", "Percent_Change", "Cohort_N"), 
                   "pmpm_MetGoal" = c(pct_change(df_metgoals_by_year, "pmpm_MetGoal"), sum(phs$met_goal_yes)),
                   "pmpm_DidNotMeetGoal" = c(pct_change(df_metgoals_by_year, "pmpm_DidNotMeetGoal"), sum(phs$met_goal_no))))

##### Write Data #####

write_csv(df_total_claims_by_year, paste0(directory, "Data/Build_Tables/pmpm_by_year_phs.csv"))
#write_csv(df_claims_by_quarter, paste0(directory, "Data/Build_Tables/pmpm_by_quarter_phs.csv"))
write_csv(df_metgoals_by_year, paste0(directory, "Data/Build_Tables/pmpm_metgoals_by_year.csv"))
write_csv(claims, paste0(directory, "Data/Sub_Tables/claims_per_phs.csv"))

print("pmpm_by_year_phs written to Data/Build_Tables")
#print("pmpm_by_quarter_phs written to Data/Build_Tables")
print("pmpm_metgoals_by_year written to Data/Build_Tables")

df_total_claims_by_year -> pmpm_by_year_phs_tab
#df_claims_by_quarter -> pmpm_by_quarter_phs_tab
df_metgoals_by_year -> pmpm_metgoals_by_year_tab
claims -> claims_per_phs_tab

rm("df_metgoals_by_year", "df_total_claims_by_year", "df_claims_by_quarter", "claims", 'max_assessment')
