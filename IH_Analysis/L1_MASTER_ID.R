############ Level 1 #############

### Reads in raw census information to create master ID's 

### Input Tables: raw_census
###               risky_zips

### Output Tables: id_bridge
###                human_flags

### Author: Michelle Powell

### Sourced By: master_id()


###########################################
###########################################

##### Column Parameters #####

# read.csv("Data/Raw/raw_census.csv", header = FALSE, nrows = 1) # Print raw column names

census_last_name       <- "Dep Last Name"
census_first_name      <- "First Name"
census_sex             <- "Gender"
census_dob             <- "DOB"
census_zip             <- "Zip"
census_start_date      <- "Eff Date"
census_end_date        <- "Ind Term Date"
census_employee_spouse <- 'Dep Code'

##### Functions #####

format_sex<-function(x, male_start = ".*M.*",female_start = ".*F.*",ignore.case = FALSE){
  y<-as.character(x)
  y<-gsub(pattern = male_start, replacement = "0", x = y, ignore.case = ignore.case)
  y<-gsub(pattern = female_start, replacement = "1", x = y, ignore.case = ignore.case)
  y<-as.numeric(y)
  return(y)
}

format_dates<-function(date_col, format = '%m/%d/%y', latest = "2017-02-02"){
  date_col<-as.Date(date_col, format = format)
  date_col<-as.Date(ifelse(date_col > latest, format(date_col, "19%y-%m-%d"), format(date_col)))
  return(date_col)
}

##### Read and Trim Raw Data #####

census_cols<-c(census_last_name, census_first_name, census_sex, census_dob, census_zip, 
               census_start_date, census_end_date, census_employee_spouse)
new_census_cols <- c("last", "first", "sex", "dob", "zip", "cov_start_dt", "cov_end_dt", 'emp_spouse')
census <- read_csv(paste0(directory, "Data/Raw/raw_census.csv"), col_types = cols(.default = "c"))
census <-census[match(census_cols, colnames(census))]
colnames(census) <- new_census_cols
risk_zips<-read_csv(paste0(directory, "Data/Fixed_Tables/risky_zips.csv"), col_types = 'c')$ZipCode

rm("census_last_name", "census_first_name", "census_sex", "census_dob", "census_zip", 
   "census_start_date", "census_end_date", "census_employee_spouse")

##### Format Data #####

id_bridge <- census %>% transmute('last' = tolower(last), 
                                  'first' = tolower(first), 
                                  'sex' = format_sex(sex, male_start = 'm', female_start = 'f'), 
                                  'dob' = ymd(dob),
                                  'zip' = str_sub(zip, 1, 5),
                                  'cov_start_dt' = ymd(cov_start_dt), 
                                  'cov_end_dt' = ymd(cov_end_dt),
                                  'emp_spouse' = emp_spouse) %>% 
  filter(emp_spouse %in% c("e", "s")) %>% arrange(dob, last) %>%
  mutate('geo_risk' = as.numeric(zip %in% risk_zips),
         'emp_flag' = as.numeric(emp_spouse == 'e'),
         'master_id' = seq(from = 999999 - trunc(dim(.)[1]*trunc(trunc(899999/dim(.)[1])/2)), 
                           by = trunc(trunc(899999/dim(.)[1])/2), 
                           length.out = dim(.)[1])) %>% 
  select(master_id, last, first, sex, dob, cov_start_dt, cov_end_dt, geo_risk, emp_flag)

human_flags <- id_bridge %>% 
  mutate('age' = round(interval(dob, as.Date('2017-01-01'))/duration(1,'years'))) %>% 
  mutate('age_45' = as.numeric(age>=45), 
         'age_18.45' = as.numeric(age >= 18 & age < 45)) %>%
  select(master_id, sex, geo_risk, emp_flag, cov_start_dt, cov_end_dt, age_45, age_18.45)

##### Write Data #####

write_csv(id_bridge, paste0(directory, "Data/Sub_Tables/id_bridge.csv"))
write_csv(human_flags, paste0(directory, "Data/Sub_Tables/human_flags.csv"))
print("human_flags written to Data/Sub_Tables")
print("id_bridge written to Data/Sub_Tables")

human_flags_tab <- human_flags %>% ungroup()

rm("human_flags", "id_bridge", "risk_zips", "census", "census_cols", "new_census_cols", "format_sex", "format_dates")
