############ Level 3 #############

### Simple counts of total number of participants, 
### total number of non-participants,  
### number of participant and non-participant employees and spouses.
### Creates crosstabs of demographic info.

### Input Tables: human_flags
###               phs_analytics

### Output Tables: model
###                demo_tree_P_vs_NP
###                demo_tree_Emp_vs_Sp
###                demo_tree_Emp_P_vs_NP
###                demo_tree_Sp_P_vs_NP
###                demo_xtabs
###                demo_xtabs_phs_tab

### Author: Peter Cooman

### Sourced By: demo_tables()


###########################################
###########################################

##### Functions #####

create_crosstab <- function(data, cross_tab) {
  data %>% group_by_(.dots = lazyeval::lazy(cross_tab)) %>% 
    summarise_each(funs(sum), Values_all_P, Values_all_NP, Values_all_Emp, Values_all_Sp, Values_Emp_P, Values_Emp_NP, Values_Sp_P, Values_Sp_NP) %>%
    mutate_each(funs(./sum(.)), Values_all_P, Values_all_NP, Values_all_Emp, Values_all_Sp, Values_Emp_P, Values_Emp_NP, Values_Sp_P, Values_Sp_NP)
}

##### Read in Data #####

df_pii <- assign_table("human_flags_tab", "Data/Sub_Tables/human_flags.csv") %>%
  filter(cov_end_dt > analysis_date | is.na(cov_end_dt)) %>% 
  distinct(master_id, sex, geo_risk, emp_flag, age_45, age_18.45)

if(dim(df_pii)[1] == 0){
  df_pii <- assign_table("human_flags_tab", "Data/Sub_Tables/human_flags.csv") %>%
    filter(cov_end_dt == max(cov_end_dt)) %>% 
    distinct(master_id, sex, geo_risk, emp_flag, age_45, age_18.45)
}

df_phs <- assign_table("phs_tab", "Data/Sub_Tables/phs_analytics.csv")

##### Label Ids #####

flags <- df_pii %>% left_join(df_phs) %>%
  group_by(master_id, sex, geo_risk, age_45) %>%
  summarize("Values_all_P" = as.numeric(any(assessment == max(df_phs$assessment))),
            "Values_all_NP" = as.numeric(!any(assessment == max(df_phs$assessment)) || is.na(assessment)),
            "Values_all_Emp" = as.numeric(any(emp_flag == 1)),
            "Values_all_Sp" = as.numeric(any(emp_flag == 0))) %>% ungroup() %>%
  mutate("Values_all_P" = ifelse(is.na(Values_all_P), 0, Values_all_P),
         "Values_Emp_P" = Values_all_P*Values_all_Emp,
         "Values_Emp_NP" = Values_all_NP*Values_all_Emp,
         "Values_Sp_P" = Values_all_P*Values_all_Sp,
         "Values_Sp_NP" = Values_all_NP*Values_all_Sp,
         "Gender" = ifelse(sex, "Female", "Male"),
         "Age" = ifelse(age_45, "45 and over", '< 45'),
         "Geography" = ifelse(geo_risk, 'High Risk Zip Code','Low Risk Zip Code')) 

flags_phs <- df_pii %>% left_join(df_phs) %>% 
  filter(assessment == max(ymd(df_phs$assessment))) %>%
  mutate("Gender" = ifelse(sex, "Female", "Male"),
         "Age" = ifelse(age_45, "45 and over", '< 45'),
         "Geography" = ifelse(geo_risk, 'High Risk Zip Code','Low Risk Zip Code'),
         "phs_flag" = case_when(.$phs > 25            ~ 'High Risk',
                                .$phs > 0 & .$phs <= 25 ~ 'Moderate Risk',
                                .$phs <= 0 ~ 'Low Risk'))

##### Count Populations #####

df_counts <- flags %>% group_by() %>%
  summarise_each(funs(sum), Values_all_P, Values_all_NP, Values_all_Emp, Values_all_Sp, Values_Emp_P, Values_Emp_NP, Values_Sp_P, Values_Sp_NP) %>% 
  mutate(Values_all = Values_all_Emp + Values_all_Sp) %>% ungroup()

df_counts_phs <- flags_phs %>% group_by() %>%
  summarise(Values_all_High = sum(phs_flag == 'High Risk'),
            Values_all_Moderate = sum(phs_flag == 'Moderate Risk'),
            Values_all_Low = sum(phs_flag == 'Low Risk'))
  

##### Build demo_xtabs #####

demo_xtabs_tab <- flags %>% create_crosstab(Gender) %>% union_all(create_crosstab(flags, Age)) %>% union_all(create_crosstab(flags,Geography)) %>% 
  mutate("Category" = ifelse(!is.na(Gender), "Gender", ifelse(!is.na(Age), "Age", "Geography")),
         "Subcategory" = coalesce(Gender, Age, Geography)) %>%
  select(Category, Subcategory, Values_all_P, Values_all_NP, Values_all_Emp, Values_all_Sp, Values_Emp_P, Values_Emp_NP, Values_Sp_P, Values_Sp_NP) %>%
  bind_cols(df_counts[rep(1, 6),])

##### Build demo_phs_xtabs #####

demo_xtabs_phs_tab <- flags_phs %>%
  mutate("Category" = 'Gender',
          "Subcategory" = Gender) %>%
  union_all(flags_phs %>%
              mutate("Category" = 'Age',
                     "Subcategory" = Age)) %>%
  union_all(flags_phs 
            %>% mutate("Category" = 'Geography',
                       "Subcategory" = Geography)) %>%
  group_by(Category,Subcategory) %>%
  summarise(Value_all_High = sum(phs_flag == 'High Risk'),
            Value_all_Moderate = sum(phs_flag == 'Moderate Risk'),
            Value_all_Low = sum(phs_flag == 'Low Risk'))

##### Build phs_count_by_year_tab #####

phs_count_by_year_tab <- flags_phs %>%
  mutate(Year = as.character(year(assessment))) %>% filter(!is.na(Year)) %>%
  group_by(Year) %>%
  summarise(Count_all_High = sum(phs_flag == 'High Risk'),
            Count_all_Moderate = sum(phs_flag == 'Moderate Risk'),
            Count_all_Low = sum(phs_flag == 'Low Risk'))

phs_count_by_year_tab <- bind_rows(phs_count_by_year_tab %>% mutate(Level = 'Low', Count = Count_all_Low) %>% select(Year, Level, Count),
                               phs_count_by_year_tab %>% mutate(Level = 'Moderate', Count = Count_all_Moderate) %>% select(Year, Level, Count),
                               phs_count_by_year_tab %>% mutate(Level = 'High', Count = Count_all_High) %>% select(Year, Level, Count)) %>%
  group_by(Year) %>% mutate(Percent = Count/sum(Count))


##### Build Model #####

model <- tibble('Link' = c(rep('link',49),'node'),
                'T' = c(seq(-6,6,0.25), -6)) %>%
  mutate('Sigmoid Function' = 1/(1+exp(1)^(-T)))

model_phs <- tibble('Link' = c(rep('link',49),'node'),
                'T' = c(seq(-6,6,0.25), -6)) %>%
  mutate('Sigmoid Function' = 1/(1+exp(1)^(-T)))

##### Build demo_tree_P_vs_NP #####

demo_tree_P_vs_NP <- tibble('Category' = c('Root to P', 'Root to NP', 'All', 'Participants', 'Non-Participants'),
                            'Label' = c('','','All','Participants','Non-Participants'),
                            'Tier' = c(0,0,0,1,1),
                            'Position 1' = c(2,2,2,1,3),
                            'Position 2' = c(1,3,2,1,3),
                            'Value_curve' = c(df_counts$Values_all_P, df_counts$Values_all_NP, 2*df_counts$Values_all, 2*df_counts$Values_all_P, 2*df_counts$Values_all_NP),
                            'Value_node' = c('','',as.character(df_counts$Values_all),as.character(df_counts$Values_all_P), as.character(df_counts$Values_all_NP)),
                            'Pct_node' = c('','',as.character(df_counts$Values_all/df_counts$Values_all),as.character(df_counts$Values_all_P/df_counts$Values_all), as.character(df_counts$Values_all_NP/df_counts$Values_all)),
                            'Link' = c(rep('link',2),rep('node',3)))

##### Build demo_tree_Emp_vs_Sp #####

demo_tree_Emp_vs_Sp <- tibble('Category' = c('Root to Emp', 'Root to Sp', 'All', 'Employees', 'Spouses'),
                              'Label' = c('','','All','Employees','Spouses'),
                              'Tier' = c(0,0,0,1,1),
                              'Position 1' = c(2,2,2,1,3),
                              'Position 2' = c(1,3,2,1,3),
                              'Value_curve' = c(df_counts$Values_all_Emp, df_counts$Values_all_Sp,2*df_counts$Values_all,2*df_counts$Values_all_Emp,2*df_counts$Values_all_Sp),
                              'Value_node' = c('','',as.character(df_counts$Values_all),as.character(df_counts$Values_all_Emp), as.character(df_counts$Values_all_Sp)),
                              'Pct_node' = c('','',as.character(df_counts$Values_all/df_counts$Values_all),as.character(df_counts$Values_all_Emp/df_counts$Values_all), as.character(df_counts$Values_all_Sp/df_counts$Values_all)),
                              'Link' = c(rep('link',2),rep('node',3)))

##### Build demo_tree_Emp_P_vs_NP #####

demo_tree_Emp_P_vs_NP <- tibble('Category' = c('Root to Emp', 'Root to Sp', 'Emp to P Emp', 'Emp to NP Emp','All', 'Employees', 'Spouses', 'Participating Employees', 'Non-Participating Employees'),
                                'Label' = c('','','','','All','Employees','Spouses','Participants','Non-Participants'),
                                'Tier' = c(0,0,1,1,0,1,1,2,2),
                                'Position 1' = c(2,2,1,1,2,1,3,1,3),
                                'Position 2' = c(1,3,1,3,2,1,3,1,3),
                                'Value_curve' = c(df_counts$Values_all_Emp, df_counts$Values_all_Sp,df_counts$Values_Emp_P,df_counts$Values_Emp_NP,2*df_counts$Values_all,2*df_counts$Values_all_Emp,2*df_counts$Values_all_Sp,2*df_counts$Values_Emp_P,2*df_counts$Values_Emp_NP),
                                'Value_node' = c('','','','',as.character(df_counts$Values_all),as.character(df_counts$Values_all_Emp), as.character(df_counts$Values_all_Sp),as.character(df_counts$Values_Emp_P),as.character(df_counts$Values_Emp_NP)),
                                'Pct_node' = c('','','','',as.character(df_counts$Values_all/df_counts$Values_all),as.character(df_counts$Values_all_Emp/df_counts$Values_all), as.character(df_counts$Values_all_Sp/df_counts$Values_all),as.character(df_counts$Values_Emp_P/df_counts$Values_all_Emp),as.character(df_counts$Values_Emp_NP/df_counts$Values_all_Emp)),
                                'Link' = c(rep('link',4),rep('node',5)))

##### Build demo_tree_Sp_P_vs_NP #####

demo_tree_Sp_P_vs_NP <- tibble('Category' = c('Root to Emp', 'Root to Sp', 'Sp to P Sp', 'Sp to NP Sp','All', 'Employees', 'Spouses', 'Participating Spouses', 'Non-Participating Spouses'),
                               'Label' = c('','','','','All','Employees','Spouses','Participants','Non-Participants'),
                               'Tier' = c(0,0,1,1,0,1,1,2,2),
                               'Position 1' = c(2,2,3,3,2,1,3,1,3),
                               'Position 2' = c(1,3,1,3,2,1,3,1,3),
                               'Value_curve' = c(df_counts$Values_all_Emp, df_counts$Values_all_Sp,df_counts$Values_Sp_P,df_counts$Values_Sp_NP,2*df_counts$Values_all,2*df_counts$Values_all_Emp,2*df_counts$Values_all_Sp,2*df_counts$Values_Sp_P,2*df_counts$Values_Sp_NP),
                               'Value_node' = c('','','','',as.character(df_counts$Values_all),as.character(df_counts$Values_all_Emp), as.character(df_counts$Values_all_Sp),as.character(df_counts$Values_Sp_P),as.character(df_counts$Values_Sp_NP)),
                               'Pct_node' = c('','','','',as.character(df_counts$Values_all/df_counts$Values_all),as.character(df_counts$Values_all_Emp/df_counts$Values_all), as.character(df_counts$Values_all_Sp/df_counts$Values_all),as.character(df_counts$Values_Sp_P/df_counts$Values_all_Sp),as.character(df_counts$Values_Sp_NP/df_counts$Values_all_Sp)),
                               'Link' = c(rep('link',4),rep('node',5)))

##### Build demo_tree_phs #####

demo_tree_phs <- tibble('Category' = c('Root to Low', 'Root to High', 'Root to Moderate', 'All Participants', 'Low Risk', 'High Risk', 'Moderate Risk'),
                        'Label' = c('','','', 'All Participants', 'Low Risk', 'High Risk', 'Moderate Risk'),
                        'Tier' = c(0,0,0,0,1,1,1),
                        'Position 1' = c(2,2,2,2,3,1,2),
                        'Position 2' = c(3,1,2,2,3,1,2),
                        'Value_curve' = c(df_counts_phs$Values_all_Low, df_counts_phs$Values_all_High, df_counts_phs$Values_all_Moderate, 2*df_counts$Values_all_P, 2*df_counts_phs$Values_all_Low, 2*df_counts_phs$Values_all_High, 2*df_counts_phs$Values_all_Moderate),
                        'Value_node' = c('', '', '', as.character(df_counts$Values_all_P), as.character(df_counts_phs$Values_all_Low), as.character(df_counts_phs$Values_all_High), as.character(df_counts_phs$Values_all_Moderate)),
                        'Pct_node' = c('', '', '', as.character(df_counts$Values_all_P/df_counts$Values_all_P), as.character(df_counts_phs$Values_all_Low/df_counts$Values_all_P), as.character(df_counts_phs$Values_all_High/df_counts$Values_all_P), as.character(df_counts_phs$Values_all_Moderate/df_counts$Values_all_P)),
                        'Link' = c(rep('link',3),rep('node',4)))

##### Write Data #####

write_csv(model, paste0(directory, "Data/Build_Tables/model.csv"))
write_csv(demo_tree_P_vs_NP, paste0(directory, "Data/Build_Tables/demo_tree_P_vs_NP.csv"))
write_csv(demo_tree_Emp_vs_Sp, paste0(directory, "Data/Build_Tables/demo_tree_Emp_vs_Sp.csv"))
write_csv(demo_tree_Emp_P_vs_NP, paste0(directory, "Data/Build_Tables/demo_tree_Emp_P_vs_NP.csv"))
write_csv(demo_tree_Sp_P_vs_NP, paste0(directory, "Data/Build_Tables/demo_tree_Sp_P_vs_NP.csv"))
write_csv(demo_xtabs_tab, paste0(directory, "Data/Build_Tables/demo_xtabs.csv"))
write_csv(demo_xtabs_phs_tab, paste0(directory, "Data/Build_Tables/demo_phs_xtabs.csv"))
write_csv(phs_count_by_year_tab, paste0(directory, "Data/Build_Tables/phs_count_by_year.csv"))
write_csv(demo_tree_phs, paste0(directory, 'Data/Build_Tables/demo_tree_phs.csv'))

print("demo_xtabs written to Data/Build_Tables")
print("demo_phs_xtabs written to Data/Build_Tables")
print("model written to Data/Build_Tables")
print("demo_tree_P_vs_NP written to Data/Build_Tables")
print("demo_tree_Emp_vs_Sp written to Data/Build_Tables")
print("demo_tree_Emp_P_vs_NP written to Data/Build_Tables")
print("demo_tree_Sp_P_vs_NP written to Data/Build_Tables")
print("phs_count_by_year written to Data/Build_Tables")

rm("df_counts", "df_phs", "df_pii", "flags", "create_crosstab")

