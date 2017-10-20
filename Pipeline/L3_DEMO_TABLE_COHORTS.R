############ Level 3 #############

### 

### Input Tables: phs_analytics
###               human_flags
###               claims_per_member

### Output Tables: totals_by_year_cohorts
###                pmpm_by_year_cohorts

### Author: Michelle Powell

### Sourced By: demo_table_cohorts()

###########################################
###########################################

##### Functions #####

get_count <- function(start, end, position_1, position_2, link){
  assessments <- get_assessments(add_end = TRUE, original = TRUE)
  i_start <- which(year(assessments) == start)
  i_end <- which(year(assessments) == end)
  x <- full_participant_phs %>% filter(participation_year == year(assessments[i_start]), phs == position_1) %>% distinct(master_id)
  y <- full_participant_phs %>% filter(participation_year == year(assessments[i_end]), phs == position_2) %>% distinct(master_id)
  z <- inner_join(x, y)
  if(link == 'link') return(dim(z)[1])
  if(link == 'node') return(dim(x)[1])
}

#### Read In Data #####

human_flags <- assign_table("human_flags_tab", "Data/Sub_Tables/human_flags.csv")  
phs <- assign_table("phs_tab", "Data/Sub_Tables/phs_analytics.csv")

full_members <- human_flags %>% filter(cov_start_dt <= analysis_start, cov_end_dt >= analysis_date | is.na(cov_end_dt)) 

if(dim(full_members)[1] == 0){
  full_members <- human_flags %>% filter(cov_start_dt <= analysis_start, cov_end_dt == max(cov_end_dt) | is.na(cov_end_dt))   
}

full_participants <- phs %>% group_by(master_id) %>% summarise(ct = n()) %>% ungroup() %>%
  filter(ct == length(get_assessments())) %>% distinct(master_id) %>% inner_join(full_members)

full_participant_phs <- phs %>% right_join(full_participants) %>% select(master_id, participation_year, assessment, phs) %>%
  mutate(phs = case_when(.$phs > 25              ~ 1,
                         .$phs > 0 & .$phs <= 25 ~ 2,
                         .$phs <= 0              ~ 3))

full_non_participants <- full_members %>% anti_join(phs, by = 'master_id')

#### Build Loop For Demo Tree Creation #####

years <- year(get_assessments(original = TRUE))

d_all <- data.frame("category" = c(),
                    'start' = c(),
                    'end' = c(), 
                    'position_1' = c(),
                    'position_2' = c(),
                    'tier' = c(),
                    'link' = c(),
                    'year' = c(),
                    stringsAsFactors = FALSE)

  d1 <- tibble('start' = years[1],
               'end' = years[length(years)],
               'phs_level' = c('Low Risk', 'Moderate Risk', 'High Risk'),
               'position_1' = c(3, 2, 1),
               'tier' = 0)
  d2 <- tibble('start' = years[length(years)],
               'phs_level' = c('Low Risk', 'Moderate Risk', 'High Risk'),
               'position_2' = c(3, 2, 1),
               'color' = c('low', 'mod', 'high'))
  
  d_links <- full_join(d1, d2, by = c('end' = 'start')) %>%
    mutate(category = do.call(paste, list(phs_level.x, start, "to", phs_level.y, end)),
           link = 'link',
           year = '') %>%
    select(category, start, end, tier, position_1, position_2, link, color, year)
  
  d_nodes <- d1 %>% mutate(category = do.call(paste0, list(start, phs_level)),
                           link = 'node',
                           position_2 = position_1,
                           color = phs_level,
                           year = ifelse(phs_level == 'Low Risk', as.character(start), '')) %>%
    select(category, start, end, tier, position_1, position_2, link, color, year) %>%
    union_all(d2 %>% mutate(category = do.call(paste0, list(start, phs_level)),
                            link = 'node',
                            color = phs_level,
                            position_1 = position_2,
                            end = start+1,
                            tier = 1,
                            year = ifelse(phs_level == 'Low Risk', as.character(start), '')) %>%
                select(category, start, end, tier, position_1, position_2, link, color, year)) 
  
  d_all <- bind_rows(d_all, d_links, d_nodes) %>% distinct()

##### Build PHS Cohort Membership #####

d_full <- d_all %>%
  mutate(value = mapply(get_count, start, end, position_1, position_2, link)) %>%
  mutate(node_value_label = ifelse(link == 'node', as.character(value), ''),
         percent = ifelse(link == 'node', as.character(value/dim(full_participants)[1]), ''),
         value = ifelse(link == 'node', (2-(value/dim(full_participants)[1])-.1)*value, value),
         label = ifelse(link == 'node', str_replace(category, '[[:number:]]+', ''), ''),
         color = do.call(paste, list(color, link)))

##### Write Data ######

write_csv(d_full, paste0(directory, "Data/Build_Tables/phs_cohorts_membership.csv"))

print('phs_cohorts_membership Written to Data/Build_Tables')

d_full -> phs_cohorts_membership_tab

rm('d_full', 'd_links', 'd_nodes', 'd1', 'd2', 'full_members', 'full_non_participants', 'full_participant_phs', 
   'full_participants', 'human_flags', 'phs', 'd_all')
