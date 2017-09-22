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

census_last_name       <- "LAST"
census_first_name      <- 'FIRST'
census_sex             <- "Gender"
census_dob             <- "DOB"
census_zip             <- "Supplemental Zip Code"
census_state           <- "Supplemental State"
census_city            <- "Supplemental City"
census_address_1       <- "Supplemental Address Line 1"
census_address_2       <- "Supplemental Address Line 2"
census_start_date      <- "Start Date"
census_end_date        <- "Stop Date"
census_employee_spouse <- "Relationship"
census_insurer_id      <- "Mercy ID"
census_IH_ID           <- "IH PID"

##### Functions #####

format_sex<-function(x, male_start = ".*M.*",female_start = ".*F.*",ignore.case = FALSE){
  y<-as.character(x)
  y<-gsub(pattern = male_start, replacement = "0", x = y, ignore.case = ignore.case)
  y<-gsub(pattern = female_start, replacement = "1", x = y, ignore.case = ignore.case)
  y<-as.numeric(y)
  return(y)
}

format_address <- function(address){
  shortcuts <- read_csv(paste0(directory, 'Data/Fixed_Tables/postal_shortcuts.csv')) %>%
    filter(Long != Abbreviation)
  address <-  gsub("[[:punct:]]", "", gsub("-", " ", address))
  address <-  gsub(" apt ", " ", gsub(" unit ", " ", address))
  for(i in 1:length(shortcuts$Long)){
    address <- gsub(paste0(" ",tolower(shortcuts$Long[i])," "), 
                    paste0(" ",tolower(shortcuts$Abbreviation[i])," "),
                    address, ignore.case = TRUE)
    address <- gsub(paste0(" ",tolower(shortcuts$Long[i]),"$"), 
                    paste0(" ",tolower(shortcuts$Abbreviation[i])),
                    address, ignore.case = TRUE)
  }
  return(address)
}

source('../Pipeline/L2_IH_DEID.R')

##### Read and Trim Raw Data #####

census_cols<-c(census_last_name, census_first_name, census_sex, census_dob, census_zip, census_state, census_city, census_address_1, census_address_2,
               census_start_date, census_end_date, census_employee_spouse, census_insurer_id, census_IH_ID)
new_census_cols <- c("last", "first", "sex", "dob", "zip", "state", "city", "address_1", "address_2", "cov_start_dt", "cov_end_dt", 'emp_spouse', 'insurer_id', 'ih_id')

census <- read_csv(paste0(directory, "Data/Raw/raw_census_2.csv"), col_types = cols(.default = "c")) %>%
  full_join(read_csv(paste0(directory, "Data/Raw/raw_census.csv"), col_types = cols(.default = "c")), by = c("Mercy ID" = "Lawson ID")) %>% distinct()

census <- census %>% mutate("IH PID" = coalesce(`IH -PID`, `IH PID`),
                            "LAST" = coalesce(LAST.y, LAST.x),
                            'FIRST' = coalesce(FIRST.y, FIRST.x),
                            'MIDDLE' = coalesce(MIDDLE.y, MIDDLE.x),
                            'Benefit Date 1' = coalesce(`Benefit Date 1.y`, `Benefit Date 1.x`),
                            'Cov Opt Desc' = coalesce(`Cov Opt Desc.y`, `Cov Opt Desc.x`),
                            'Supplemental Address Line 1' = coalesce(`Supplemental Address Line 1.y`, `Supplemental Address Line 1.x`),
                            'Supplemental Address Line 2' = coalesce(`Supplemental Address Line 2.y`, `Supplemental Address Line 2.x`),
                            'Supplemental City' = coalesce(`Supplemental City.y`, `Supplemental City.x`),
                            'Supplemental State' = coalesce(`Supplemental State.y`, `Supplemental State.x`),
                            'Supplemental Zip Code' = coalesce(`Supplemental Zip Code.y`, `Supplemental Zip Code.x`)) %>%
  select(`Mercy ID`, `IH PID`, `LAST`, `FIRST`, `MIDDLE`, `Relationship`, `DOB`, Gender, `Benefit Date 1`, `Cov Opt Desc`,
         `Supplemental Address Line 1`, `Supplemental Address Line 2`, `Supplemental City`, `Supplemental State`, `Supplemental Zip Code`,
         `Status`, `Total FTE`, `Start Date`, `Stop Date`, `Cov Opt`, `Dependent Count`, PL, `Process Level Name`, Dept, `Department Description`,
         `Job Code`, `Job Code Description`, `Location Code`, `Location Description`, `Supplemental County`)


census <- census[match(census_cols, colnames(census))]
colnames(census) <- new_census_cols
risk_zips<-read_csv(paste0(directory, "Data/Fixed_Tables/risky_zips.csv"), col_types = 'c')$ZipCode

rm('census_IH_ID', "census_last_name", "census_first_name", "census_sex", "census_dob", "census_zip", "census_state", "census_city", "census_address_1", "census_address_2",
   "census_start_date", "census_end_date", "census_employee_spouse", 'census_insurer_id')

##### Format Data #####

census_full <- census %>% transmute('insurer_id' = insurer_id,
                                    'ih_id' = ih_id,
                                    'last' = gsub("[[:punct:]]", "", gsub("-", " ", tolower(last))),
                                    'first' = gsub("[[:punct:]]", "", gsub("-", " ", tolower(first))),
                                    'sex' = format_sex(sex, male_start = 'M', female_start = 'F'), 
                                    'dob' = mdy(dob),
                                    'address' = ifelse(is.na(address_2), tolower(address_1),
                                                       do.call(paste, list(tolower(address_1), tolower(address_2)))),
                                    'city' = tolower(city),
                                    'state' = tolower(state),
                                    'zip' = str_sub(zip, 1, 5),
                                    'cov_start_dt' = mdy(cov_start_dt), 
                                    'cov_end_dt' = mdy(cov_end_dt),
                                    'emp_spouse' = case_when(.$emp_spouse == "X" ~ "e",
                                                             .$emp_spouse == "S" ~ "s")) %>%
  distinct() %>% filter(!is.na(emp_spouse)) %>%
  mutate(unique_id = do.call(paste0, list(insurer_id, dob))) %>% 
  mutate(address = format_address(address)) %>%
  filter(!is.na(cov_start_dt),
         !is.na(cov_end_dt),
         !is.na(dob))

##### Create Master_ID's With Distinct Individuals #####

census_master_id <- census_full %>% distinct(unique_id) %>% arrange(unique_id) %>%
  mutate(master_id = seq(from = 999999 - trunc(dim(.)[1]*trunc(trunc(899999/dim(.)[1])/2)), 
                         by = trunc(trunc(899999/dim(.)[1])/2), 
                         length.out = dim(.)[1]))

id_bridge <- census_full %>% left_join(census_master_id) %>%
  distinct(master_id, unique_id, insurer_id, ih_id, last, first, sex, dob, address, city, state, zip)

id_bridge_2 <- id_bridge %>% rename('master_id_2' = master_id,
                                    'unique_id_2' = unique_id,
                                    'insurer_id_2' = insurer_id)
id_dedupe <- ih_deidentify(data = id_bridge_2, 
                           census_id_bridge = id_bridge, 
                           data_id = 'master_id_2', 
                           census_id = 'master_id', 
                           id_match = FALSE, 
                           pii_match = TRUE, 
                           fuzzy_match = FALSE,
                           return_id_bridge = TRUE) %>% 
  filter(master_id != master_id_2) %>% arrange(master_id) %>%
  mutate(master_id_a = pmax(master_id, master_id_2),
         master_id_b = pmin(master_id, master_id_2)) %>% 
  distinct(master_id_a, master_id_b)

census_master_id$master_id[match(id_dedupe$master_id_a, census_master_id$master_id)] <- id_dedupe$master_id_b

##### Build id_bridge for matching to other data sources #####

id_bridge <- census_full %>% left_join(census_master_id) %>% ungroup() %>%
  distinct(master_id, unique_id, insurer_id, ih_id, last, first, sex, dob, address, city, state, zip)

##### Build Human Flags Table #####

# Determine Who has been - and will therefore always be considered - and Employee #
employee_flag <- census_full %>% left_join(census_master_id) %>% 
  filter(emp_spouse == 'e') %>% distinct(master_id)

# assign sex and zip to latest version of data, determine continuity of coverage #
human_flags <- census_full %>% left_join(census_master_id) %>% 
  distinct(master_id, dob, cov_start_dt, cov_end_dt, sex, zip) %>%
  group_by(master_id) %>%
  mutate(sex = last(sex, order_by = order(cov_start_dt)),
         zip = last(zip, order_by = order(cov_start_dt))) %>% ungroup() %>% distinct() %>%
  group_by(master_id) %>% arrange(master_id, cov_start_dt) %>%
  mutate(continuous = (master_id == lag(master_id, 1) & cov_start_dt == lag(cov_end_dt, 1))) %>%
  mutate(gap = ifelse(is.na(continuous), FALSE, !continuous)) %>%
  mutate(continuous = ifelse(is.na(continuous), FALSE, continuous)) %>% ungroup()

# filter out individuals with gaps in coverage for loops #
human_flags_gaps <- human_flags %>% filter(gap) %>% distinct() %>%
  select(master_id, 'gap_date' = cov_start_dt)
human_flags_gappers <- human_flags %>% filter(master_id %in% human_flags_gaps$master_id) %>% select(-continuous, -gap)

# coalesce continuous coverage records for complete start and end dates #
human_flags <- human_flags %>% anti_join(human_flags_gappers) %>% 
  group_by(master_id) %>% arrange(master_id, cov_start_dt) %>%
  mutate(cov_start_dt = min(cov_start_dt),
         cov_end_dt = max(cov_end_dt)) %>% select(-continuous, -gap) %>% ungroup() %>% distinct()

# loop through individuals with gaps in coverage and assign appropriate dates #
for(id in human_flags_gaps$master_id){
  gapper <- human_flags_gappers %>% filter(master_id == id)
  gap_dates <- c(min(gapper$cov_start_dt), human_flags_gaps$gap_date[human_flags_gaps$master_id == id])
  gapper$cov_start_dt <- gap_dates[findInterval(gapper$cov_start_dt, gap_dates)]
  gapper <- gapper %>% group_by(cov_start_dt) %>% mutate(cov_end_dt = max(cov_end_dt)) %>% ungroup() %>% distinct()
  human_flags <- bind_rows(human_flags, gapper)
}

# create flags #
human_flags <- human_flags %>% distinct() %>%
  mutate('age' = round(interval(dob, analysis_date)/duration(1,'years'))) %>% 
  mutate('geo_risk' = as.numeric(zip %in% risk_zips),
         'emp_flag' = as.numeric(master_id %in% employee_flag$master_id),
         'age_45' = as.numeric(age >= 45), 
         'age_18.45' = as.numeric(age >= 18 & age < 45)) %>%
  select(master_id, sex, geo_risk, emp_flag, cov_start_dt, cov_end_dt, age_45, age_18.45) %>% distinct()

##### Write Data #####

write_csv(id_bridge, paste0(directory, "Data/Sub_Tables/id_bridge.csv"))
write_csv(human_flags, paste0(directory, "Data/Sub_Tables/human_flags.csv"))
print("human_flags written to Data/Sub_Tables")
print("id_bridge written to Data/Sub_Tables")

human_flags_tab <- human_flags %>% ungroup()

rm("human_flags", "id_bridge", "risk_zips", "census", "census_cols", "new_census_cols", 
   'census_full', 'census_master_id', 'census_rows', 'employee_flag', 'gapper', 'gap_dates',
   'id', "format_sex", "format_address", 'human_flags_gappers', 'human_flags_gaps', 'id_bridge_2',
   'id_dedupe')

