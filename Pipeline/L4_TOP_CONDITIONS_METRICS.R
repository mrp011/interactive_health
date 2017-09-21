############ Level 4 #############

### Build all of the Tables on the Top 3 Conditions Page

### Input Tables: phs_analytics
###               human_flags
###               claims_proc_plus
###               claims_rx_plus
###               top_condition_names
###               claims_per_member

### Output Tables: condition_counts
###                condition_circles
###                condition_bars
###                condition_prevalence
###                condition_spending
###                condition_demographics

### Author: Michelle Powell

### Sourced By: top_conditions_metrics()

###########################################
###########################################

########## Functions ##########

deid_condition <- function(data){
  data <- data %>% filter(!is.na(bmi)) %>% 
    mutate(high_chol = as.numeric(high_chol | high_chol_new),
           hyperten = as.numeric(hyperten | hyperten_new),
           diabetes = as.numeric(con_diabetes | uncon_diabetes | diabetes_new))
  data <- as.data.frame(data)
  data$cond_1 <- data[,which(names(data)==top_conditions[1])]
  data$cond_2 <- data[,which(names(data)==top_conditions[2])]
  data$cond_3 <- data[,which(names(data)==top_conditions[3])]
  data <- data %>% mutate(cond = as.numeric(cond_1 | cond_2 | cond_3), 
                          no_cond = 1 - cond,
                          no_cond_1 = 1 - cond_1,
                          no_cond_2 = 1 - cond_2,
                          no_cond_3 = 1 - cond_3)
  return(data)
}

translate <- function(names_col, remove_ = FALSE){
  translator <- tibble("header_r" = c("bmi", "inactive", "smoker", "emot_risk", "anemia", "high_chol", 
                                      "hyperten", "met_syn", "crit_cond", "diabetes", "cond", "no_cond"),
                       "header_tableau" = c("High_Triglycerides", "Inactive", "Smoking", "Emotional_Risk", 
                                            "Anemia", "High_Cholesterol", "Hypertension", "Metabolic_Syndrome", 
                                            "Critical_Condition", "Diabetes", "Any_Top_3_Condition", "No_Top_3_Condition"))
  x <- translator$header_tableau[match(names_col, translator$header_r)]
  if (remove_) x <- gsub("_", " ", x)
  return(x)
}

translate_cond <- function(value, remove_ = FALSE){
  top_conditions_full_names <- translate(top_conditions, remove_ = FALSE)
  x <- case_when(value == "cond_1" ~ paste0(top_conditions_full_names[1]),
                 value == "cond_2" ~ paste0(top_conditions_full_names[2]),
                 value == "cond_3" ~ paste0(top_conditions_full_names[3]),
                 value == "cond_1_1" ~ paste(top_conditions_full_names[1], "Only", sep = "_"),
                 value == "cond_2_2" ~ paste(top_conditions_full_names[2], "Only", sep = "_"),
                 value == "cond_3_3" ~ paste(top_conditions_full_names[3], "Only", sep = "_"),
                 value == "cond_1_2" ~ paste(top_conditions_full_names[1], "and", top_conditions_full_names[2], sep = "_"),
                 value == "cond_1_3" ~ paste(top_conditions_full_names[1], "and", top_conditions_full_names[3], sep = "_"),
                 value == "cond_2_3" ~ paste(top_conditions_full_names[2], "and", top_conditions_full_names[3], sep = "_"),
                 value == "cond_1_2_3" ~ "All Conditions",
                 value == "cond" ~ "Any_Top_3_Condition",
                 value == "no_cond" ~ "No_Top_3_Condition")
  if (remove_) x <- gsub("_", " ", x)
  return(x)
}

distance <- function(r, R, A){
  d_ <- seq(from = 0, to = r+R, by = 0.001)
  diff <- rep(100, length(d_))
  for(i in 1:length(d_)){
    d <- d_[i]
    a <- (r^2)*acos(((d^2)+(r^2)-(R^2))/(2*d*r)) + (R^2)*acos(((d^2)+(R^2)-(r^2))/(2*d*R)) -1/2*sqrt((-d+r+R)*(d+r-R)*(d-r+R)*(d+r+R))
    a <- ifelse(is.nan(a), 100, a)
    diff[i] <- abs(A-a)
  }
  ind <- which(diff == min(diff))
  diff[ind]
  return(d_[ind])
}

gen_circle <- function(x, center_x, center_y, r, negative = FALSE){
  y = sqrt(r^2 - (x-center_x)^2)
  y <- ifelse(is.nan(y), NA, ifelse(negative, center_y - y, center_y + y))
  return(y)
}

pct_change <- function(data, x){
  x <- data %>% select_(x) %>% collect %>% .[[1]]
  change <- mean(diff(x))
  pct <- change/x[1]
  return(pct)
}

########## Read De-ID and Format Data ##########

top_conditions <- assign_table("top_condition_names", "Data/Sub_Tables/top_condition_names.csv", pull_col = 'top_condition_names')
human_flags <- assign_table("human_flags", "Data/Sub_Tables/human_flags.csv")
cond_cols <- c("cond_1", "cond_2", "cond_3", "cond", "no_cond")
cond_cols_all <- c("cond_1", "cond_2", "cond_3", "cond",  "no_cond_1", "no_cond_2", "no_cond_3", "no_cond")

mms <- assign_table("claims_per_member_tab", "Data/Sub_Tables/claims_per_member.csv") %>% select(master_id, Year, mms_p) %>%
  group_by(master_id, Year) %>% summarise(mms_p = sum(mms_p)) %>% ungroup()

claims_plus <- assign_table("proc_plus_tab", "Data/Sub_Tables/claims_proc_plus.csv") %>% deid_condition(.) %>%
  select_(.dots = c("master_id", "Year", "primary_amount", cond_cols_all)) 

rx_plus <- assign_table("rx_plus_tab", "Data/Sub_Tables/claims_rx_plus.csv") %>% deid_condition(.) %>%
  select_(.dots = c("master_id", "Year", "primary_amount", cond_cols_all)) 

phs <- assign_table("phs_tab", "Data/Sub_Tables/phs_analytics.csv") %>% deid_condition(.) %>%
  select_(.dots = c("master_id", "participation_year", cond_cols))

########## Process Data ##########

demo <- phs  %>%
  left_join(human_flags) %>% filter(participation_year == year(analysis_end),
                                    is.na(cov_end_dt) | cov_end_dt >= analysis_date) %>% 
  mutate(male = 1-sex,
         female = sex,
         geo_high_risk = geo_risk,
         geo_low_risk = 1-geo_risk,
         age_old = age_45,
         age_young = age_18.45) %>% select_(.dots = c(cond_cols, "male", "female", "geo_high_risk", "geo_low_risk", "age_old", "age_young"))

all_claims <- bind_rows(claims_plus, rx_plus)
total_claims <- sum(all_claims$primary_amount)

counts <- demo %>% select_(.dots = cond_cols) %>% 
  mutate(cond_1_1 = cond_1 & !cond_2 & !cond_3,
         cond_1_2 = cond_1 & cond_2 & !cond_3,
         cond_1_3 = cond_1 & !cond_2 & cond_3,
         cond_2_2 = !cond_1 & cond_2 & !cond_3,
         cond_2_3 = !cond_1 & cond_2 & cond_3,
         cond_3_3 = !cond_1 & !cond_2 & cond_3,
         cond_1_2_3 = cond_1 & cond_2 & cond_3) %>% summarise_all(sum) %>% ungroup() %>%
  select(cond_1, cond_2, cond_3, cond_1_1, cond_1_2, cond_1_3, cond_2_2, cond_2_3, cond_3_3, cond_1_2_3, cond, no_cond)

percents <- counts %>% mutate_all(funs(./sum(counts$no_cond, counts$cond)))

counts_percents <- bind_rows(counts, percents) %>%
  mutate(variable = c("counts", "percents"))

########## Build Condition Bars ##########

percent_bars <- all_claims %>% filter(Year >= year(analysis_end)) %>% mutate_at(.cols = cond_cols_all, funs(primary_amount*.)) %>%
  select_(.dots = cond_cols_all) %>% summarise_all(funs(sum)) %>% 
  melt(variable.name = "variable", value.name = "total_spending") %>%
  mutate(condition = rep(c(translate(top_conditions, TRUE), "Any Top 3 Condition"), 2))

##### Build Prevalence Table #####

prevalence <- phs %>% filter(participation_year >= year(analysis_start)) %>% group_by(participation_year) %>% summarise_at(.cols = cond_cols, funs(sum(.)))

prevalence <- prevalence %>% mutate(participation_year = as.character(participation_year)) %>%
  bind_rows(tibble("participation_year" = "Trend",
                   "cond_1" = pct_change(prevalence, "cond_1"),
                   "cond_2" = pct_change(prevalence, "cond_2"),
                   "cond_3" = pct_change(prevalence, "cond_3"),
                   "cond" = pct_change(prevalence, "cond"),
                   "no_cond" = pct_change(prevalence, "no_cond"))) %>% melt(id.vars = "participation_year") %>%
  mutate(condition = translate_cond(variable, TRUE))

##### Build Spending Table #####

spending <- all_claims %>%
  group_by(Year, master_id) %>% summarise_at(.cols = c(cond_cols, "primary_amount"), funs(sum)) %>% ungroup() %>% left_join(mms) %>%
  mutate_at(.cols = cond_cols, funs("amt" = primary_amount*ifelse(. != 0, 1, 0), "mms" = mms_p*ifelse(. != 0, 1, 0))) %>%
  group_by(Year) %>% summarise_all(funs(sum)) %>% ungroup() %>% select(-master_id, -cond_1, -cond_2, -cond_3, -cond, -no_cond, -primary_amount, -mms_p) %>%
  transmute(Year = as.character(Year),
            cond_1 = cond_1_amt/cond_1_mms, 
            cond_2 = cond_2_amt/cond_2_mms, 
            cond_3 = cond_3_amt/cond_3_mms, 
            cond = cond_amt/cond_mms, 
            no_cond = no_cond_amt/no_cond_mms)
  

spending <- spending %>% bind_rows(tibble("Year" = "Trend",
                                          "cond_1" = pct_change(spending, "cond_1"),
                                          "cond_2" = pct_change(spending, "cond_2"),
                                          "cond_3" = pct_change(spending, "cond_3"),
                                          "cond" = pct_change(spending, "cond"),
                                          "no_cond" = pct_change(spending, "no_cond"))) %>% melt(id.vars = "Year") %>%
  mutate(condition = translate_cond(variable, TRUE))

##### Build Demographics Table #####

demo_no_cond <- select(demo, -cond_1, -cond_2, -cond_3, -cond) %>%
  summarise_at(.cols = c("male", "female", "geo_high_risk", "geo_low_risk", "age_old", "age_young"), funs(sum(no_cond*.)/counts$no_cond)) %>%
  melt() %>% mutate(condition = "no_cond",
                    demographic = rep(c('Gender', 'Geography', 'Age'), each =  2))

demo_cond <- select(demo, -cond_1, -cond_2, -cond_3, -no_cond) %>%
  summarise_at(.cols = c("male", "female", "geo_high_risk", "geo_low_risk", "age_old", "age_young"), funs(sum(cond*.)/counts$cond)) %>%
  melt() %>% mutate(condition = "cond",
                    demographic = rep(c('Gender', 'Geography', 'Age'), each =  2))

demo_cond_1 <- select(demo, -no_cond, -cond_2, -cond_3, -cond) %>%
  summarise_at(.cols = c("male", "female", "geo_high_risk", "geo_low_risk", "age_old", "age_young"), funs(sum(cond_1*.)/counts$cond_1)) %>%
  melt() %>% mutate(condition = "cond_1",
                    demographic = rep(c('Gender', 'Geography', 'Age'), each =  2))

demo_cond_2 <- select(demo, -cond_1, -no_cond, -cond_3, -cond) %>%
  summarise_at(.cols = c("male", "female", "geo_high_risk", "geo_low_risk", "age_old", "age_young"), funs(sum(cond_2*.)/counts$cond_2)) %>%
  melt() %>% mutate(condition = "cond_2",
                    demographic = rep(c('Gender', 'Geography', 'Age'), each =  2))

demo_cond_3 <- select(demo, -cond_1, -cond_2, -no_cond, -cond) %>%
  summarise_at(.cols = c("male", "female", "geo_high_risk", "geo_low_risk", "age_old", "age_young"), funs(sum(cond_3*.)/counts$cond_3)) %>%
  melt() %>% mutate(condition = "cond_3",
                    demographic = rep(c('Gender', 'Geography', 'Age'), each =  2))

demo_all <- bind_rows(demo_no_cond, demo_cond, demo_cond_1, demo_cond_2, demo_cond_3) %>%
  mutate(cond_variable = condition, condition = translate_cond(condition, TRUE))

##### Build Venn Diagram #####

# Assign areas of circles based on total prevalence counts
a3 <- counts$cond_3*(9*pi/counts$cond_1)
a2 <- counts$cond_2*(9*pi/counts$cond_1)
a1 <- counts$cond_1*(9*pi/counts$cond_1)
# Determine radii of circles
r1 <- sqrt(a1/pi)
r2 <- sqrt(a2/pi)
r3 <- sqrt(a3/pi)
# Assign areas of intersections based on prevalence of overlap in conditions
a12 <- (counts$cond_1_2 + counts$cond_1_2_3)*(9*pi/counts$cond_1)
a13 <- (counts$cond_1_3 + counts$cond_1_2_3)*(9*pi/counts$cond_1)
a23 <- (counts$cond_2_3 + counts$cond_1_2_3)*(9*pi/counts$cond_1)
# find distances (to nearest .001) between circles
d12 <- distance(r1, r2, a12)
d13 <- distance(r1, r3, a13)
d23 <- distance(r2, r3, a23)
# find angle off verticle of each top circle
s3 <- d23/2
s2 <- d23/2
t2 <- asin(s2/d12)
t3 <- asin(s3/d13)
# set x and y of center circle
x1 <- 5
y1 <- 5
# find x and y of other two circles
x3 <- x1 + d13*cos(pi/2-t3)
x2 <- x1 - d12*cos(pi/2-t2)
y3 <- y1 + d13*sin(pi/2-t3)
y2 <- y1 + d12*sin(pi/2-t2)
# write a function to generate circles

circles <- tibble("x" = rep(seq(from = 0, to = 10, by = 0.01), 2),
                  "negative" = rep(c(TRUE, FALSE), each = 1001))

circles <- circles %>%
  mutate(y_1 = gen_circle(x, x1, y1, r1, negative),
         y_2 = gen_circle(x, x2, y2, r2, negative),
         y_3 = gen_circle(x, x3, y3, r3, negative))

circles_long <- bind_rows(circles %>% filter(!is.na(y_1)) %>% mutate(condition = translate(top_conditions[1], TRUE), y = y_1, variable = "cond_1") %>% select(variable, condition, x, y),
                          circles %>% filter(!is.na(y_2)) %>% mutate(condition = translate(top_conditions[2], TRUE), y = y_2, variable = "cond_2") %>% select(variable, condition, x, y),
                          circles %>% filter(!is.na(y_3)) %>% mutate(condition = translate(top_conditions[3], TRUE), y = y_3, variable = "cond_3") %>% select(variable, condition, x, y))

##### Build Counts and Percents Table #####

counts_percents <- t(counts_percents)
counts_percents <- as.data.frame(counts_percents)
counts_percents$variable <- rownames(counts_percents)
rownames(counts_percents) <- NULL
colnames(counts_percents) <- c("Prevalence", "Percent of Total Participants", "Condition")
counts_percents <- counts_percents %>% filter(Condition != "variable") %>% mutate(Condition_Name = translate_cond(Condition, TRUE))

##### Write Data #####

top_3_counts_percents <- counts_percents
top_3_circles <- circles_long
top_3_percent_bars <- percent_bars
top_3_prevalence <- prevalence
top_3_spending <- spending
top_3_demo_all <- demo_all

write_csv(counts_percents, paste0(directory, "Data/Build_Tables/condition_counts.csv"))
write_csv(circles_long, paste0(directory, "Data/Build_Tables/condition_circles.csv"))
write_csv(percent_bars, paste0(directory, "Data/Build_Tables/condition_bars.csv"))
write_csv(prevalence, paste0(directory, "Data/Build_Tables/condition_prevalence.csv"))
write_csv(spending, paste0(directory, "Data/Build_Tables/condition_spending.csv"))
write_csv(demo_all, paste0(directory, "Data/Build_Tables/condition_demographics.csv"))

rm ("deid_condition", "translate", "translate_cond", "distance", "gen_circle", "pct_change", "top_conditions", "human_flags", "cond_cols", "cond_cols_all", 
    "mms", "claims_plus", "rx_plus", "phs", "demo", "counts", "percents", "counts_percents", "all_claims", "total_claims", "percent_bars", "prevalence",
    "spending", "demo_no_cond", "demo_cond", "demo_cond_1", "demo_cond_2", "demo_cond_3", "demo_all", "a1", "a2", "a3", "r1", "r2", "r3", "a12", "a23",
    "a13", "d12", "d13", "d23", "s2", "s3", "x1", "x2", "x3", "y1", "y2", "y3", "circles", "circles_long")
