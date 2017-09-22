##################################################################
###########               Control Pannel               ###########
##################################################################
setwd("~/Desktop/Interactive Health/IH_Mercy_Analysis/")

library(readr)
library(lubridate)
library(dplyr)
library(stringr)
library(stringdist)
library(tibble)
library(reshape2)
library(ggplot2)
library(readxl)
options(scipen = 999)

######### Variables To Assign:

company_name <- "MERCY HEALTH" # All caps, used to label tableau reports
directory <- "~/Desktop/Interactive Health/IH_Mercy_Analysis/"
stop_loss_amount <- 80000
analysis_date <- mdy('01/01/2017')
analysis_start <- mdy('01/01/2015')
analysis_end <- mdy('12/31/2016')
paid_amount <- 'secondary_amount'

##### Data Pull From S3 #####

### This is recorded here for covenience, These commands must be run in 
### your terminal with the appropriate file locations

"
source activate civis
cd ~/Desktop/'Interactive Health'/'IH_Mercy_Analysis'/
aws s3 ls s3://interactivehealth-civis/project/
aws s3 cp s3://interactivehealth-civis/project/'Mercy_ID_Data20142015.xlsx' Data/Raw/raw_claims1.xlsx
aws s3 cp s3://interactivehealth-civis/project/'Mercy_ID_Data2016.xlsx' Data/Raw/raw_claims2.xlsx
aws s3 cp s3://interactivehealth-civis/project/'Mercy_DS_Claims_Analysis_Output.xlsx' Data/Raw/raw_phs.xlsx
aws s3 cp s3://interactivehealth-civis/project/'LDIData_20141001_20161231.txt' Data/Raw/raw_rx.txt
aws s3 cp s3://interactivehealth-civis/project/'Mercy Medical Enrolled 2015-2016_with PID.csv' Data/Raw/raw_census.csv
aws s3 cp s3://interactivehealth-civis/project/'Mercy Medical Enrolled 2015-2016.6.30.17.csv' Data/Raw/raw_census_2.csv
"

##### Common Functions #####

assign_table <- function(table_name, written_path, pull_col = NA){
  if(exists(table_name)){
    x <- get(table_name)
  } else {
    x <- read_csv(paste0(directory, written_path))
  }
  if(!is.na(pull_col)){
    x <- x %>% select_(pull_col) %>% collect %>% .[[1]]
  }
  return(x)
}

get_assessments <- function(add_end = FALSE, original = FALSE) {
  if(!original){
    x <- assign_table('assessments_tab', paste0('Data/Sub_Tables/assessments.csv'), pull_col = 'assessment')
  }  else {
    x <- assign_table('original_assessments_tab', paste0('Data/Sub_Tables/original_assessments.csv'), pull_col = 'assessment')
  }
  if(add_end) x <- c(x, analysis_date)
  return(x)
}

##################################################################
###########              Script Functions              ###########
##################################################################
run_pipeline <- function(parse_raw_data = TRUE){
  ### Level 1 ###
  
  run_master_id <- function(){
    print("running L1_MASTER_ID.R")
    source(file = "L1_MASTER_ID.R", chdir = TRUE)
  }
  
  ### Level 2 ###
  
  run_phs_analytics <- function(){
    print("running L2_PHS_ANALYTICS.R")
    source(file = "L2_PHS_ANALYTICS.R", chdir = TRUE)
  }
  
  run_claims_analytics <- function(){
    print("running L2_CLAIMS_ANALYTICS.R")
    source(file = "L2_CLAIMS_ANALYTICS.R", chdir = TRUE)
  }
  
  run_rx_analytics <- function(){
    print("running L2_RX_ANALYTICS.R")
    source(file = "L2_RX_ANALYTICS.R", chdir = TRUE)
  }
  
  ### Level 3 ###
  
  run_claims_per_member <- function(){
    print("running L3_CLAIMS_PER_MEMBER.R")
    source(file = "../Pipeline/L3_CLAIMS_PER_MEMBER.R", chdir = TRUE)
  }
  
  run_claims_per_phs_risk <- function(){
    print("running L3_CLAIMS_PER_PHS_RISK.R")
    source(file = "../Pipeline/L3_CLAIMS_PER_PHS_RISK.R", chdir = TRUE)
  }
  
  run_claims_plus <- function(){
    print("running L3_CLAIMS_PLUS.R")
    source(file = "../Pipeline/L3_CLAIMS_PLUS.R", chdir = TRUE)
  }
  
  run_demo_tables <- function(){
    print("running L3_DEMO_TABLES.R")
    source(file = "../Pipeline/L3_DEMO_TABLES.R", chdir = TRUE)
  }
  
  run_stop_loss_tables <- function(){
    print("running L3_STOP_LOSS.R")
    source(file = "../Pipeline/L3_STOP_LOSS.R", chdir = TRUE)
  }
  
  run_sought_care <- function(){
    print("running L3_SOUGHT_CARE.R")
    source(file = "../Pipeline/L3_SOUGHT_CARE.R", chdir = TRUE)
  }
  
  run_demo_table_cohorts <- function(){
    print('running L3_DEMO_TABLE_COHORTS.R')
    source(file = '../Pipeline/L3_DEMO_TABLE_COHORTS.R', chdir = TRUE)
  }
  
  run_membership <- function(){
    print("running L3_MEMBERSHIP_TRENDS.R")
    source(file = "../Pipeline/L3_MEMBERSHIP_TRENDS.R")
  }
  
  ### Level 4 ###
  
  run_claims_per_condition <- function(){
    print("running L4_CLAIMS_PER_CONDITION.R")
    source(file = "../Pipeline/L4_CLAIMS_PER_CONDITION.R", chdir = TRUE)
  }
  
  run_claims_whatif <- function(){
    print("running L4_CLAIMS_WHAT_IF.R")
    source(file = "../Pipeline/L4_CLAIMS_WHAT_IF.R", chdir = TRUE)
  }
  
  run_hospital_utilization <- function(){
    print("running L4_HOSPITAL_UTILIZATION.R")
    source(file = "../Pipeline/L4_HOSPITAL_UTILIZATION.R", chdir = TRUE)
  }
  
  run_pmpm_by_quarter <- function(){
    print("running L4_PMPM_BY_QUARTER.R")
    source(file = "../Pipeline/L4_PMPM_BY_QUARTER.R", chdir = TRUE)
  }
  
  run_pmpm_by_quarter_phs <- function(){
    print("running L4_PMPM_BY_QUARTER_PHS.R")
    source(file = "../Pipeline/L4_PMPM_BY_QUARTER_PHS.R", chdir = TRUE)
  }
  
  run_cost_shares <- function(){
    print("running L4_COST_SHARE.R")
    source(file = "../Pipeline/L4_COST_SHARES.R", chdir = TRUE)
  }
  
  run_np_ratios <- function(){
    print("running L4_NP_RATIOS.R")
    source(file = "../Pipeline/L4_NP_RATIOS.R", chdir = TRUE)
  }
  
  run_hospital_utilization_phs <- function(){
    print("running L4_HOSPITAL_UTILIZATION_PHS.R")
    source(file = "../Pipeline/L4_HOSPITAL_UTILIZATION_PHS.R", chdir = TRUE)
  }
  
  run_claim_categories <- function(){
    print("running L4_CLAIM_ICD_CATEGORIES.R")
    source(file = '../Pipeline/L4_CLAIM_ICD_CATEGORIES.R', chdir = TRUE)
  }
  
  run_claim_cohorts <- function(){
    print('running L4_CLAIM_COHORTS.R')
    source(file = '../Pipeline/L4_CLAIM_COHORTS.R', chdir = TRUE)
  }
  
  run_top_conditions_metrics <- function(){
    print("running L4_TOP_CONDITIONS_METRICS.R")
    source(file = "../Pipeline/L4_TOP_CONDITIONS_METRICS.R", chdir = TRUE)
  }
  
  run_cost_avoidance <- function(){
    print("running L4_COST_AVOIDANCE.R")
    source(file = "../Pipeline/L4_COST_AVOIDANCE.R", chdir = TRUE)
  }
  
  ### Level 5 ###
  
  run_claims_cumulative <- function(){
    print("running L5_CLAIMS_CUMULATIVE.R")
    source(file = "../Pipeline/L5_CLAIMS_CUMULATIVE.R", chdir = TRUE)
  }
  
  run_top_conditions <- function(){
    print("running L5_TOP_CONDITIONS.R")
    source(file = "../Pipeline/L5_TOP_CONDITONS.R", chdir = TRUE)
  }
  
  ### Level 6 ###
  
  run_pmpy_spend_phs <- function(){
    print("running L6_PMPM_SPEND_PHS.R")
    source(file = "../Pipeline/L6_PMPY_SPEND_PHS.R", chdir = TRUE)
  }
  
  ##################################################################
  ###########               Run Functions                ###########
  ##################################################################
  
  if(parse_raw_data){
    run_master_id()
    
    run_phs_analytics()
    run_claims_analytics()
    run_rx_analytics() 
  }
  run_claims_per_member()
  run_claims_plus()
  run_sought_care()
  run_stop_loss_tables()
  run_demo_tables()
  run_demo_table_cohorts()
  run_membership()
  
  run_hospital_utilization()
  run_hospital_utilization_phs()
  run_pmpm_by_quarter()
  #run_cost_shares()
  run_pmpm_by_quarter_phs()
  #run_claims_whatif()
  #run_claims_per_condition()
  #run_np_ratios()
  #run_claim_categories()
  run_claim_cohorts()
  run_top_conditions_metrics()
  run_cost_avoidance()
  
  #run_claims_cumulative()
  #run_top_conditions()
  
  #run_pmpy_spend_phs()
  
  label_table <- tibble("main" = company_name,
                        "dates" = paste0(year(analysis_start), " - ", year(analysis_end)))
  write_csv(label_table, paste0(directory, "Data/Build_Tables/labels.csv"))
}
