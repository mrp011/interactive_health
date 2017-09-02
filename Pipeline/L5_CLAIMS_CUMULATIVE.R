############ Level 5 #############

### This builds the cumulative claims charts

### Input Tables: human_flags
###               claims_per_member

### Output Tables: claims_cumulative
###                claims_per_member_plus

### Author: Michelle Powell

### Sourced By: claims_cumulative()

###########################################
###########################################

##### Functions #####
percent_by_year <- function(year_col, value_col){
  x <- case_when(year_col == min(year_col) ~ value_col/sum(value_col*(year_col == min(year_col)), na.rm = TRUE),
                 year_col == max(year_col) ~ value_col/sum(value_col*(year_col == max(year_col)), na.rm = TRUE))
  return(x)
}

cumulative_sums <- function(suffix, class){
  claims_col <- paste0("claims_", suffix, "_percent_by_year")
  mms_col <- paste0("mms_", suffix, "_percent_by_year")
  x <- claims_per_member_plus %>% 
    filter_(lazyeval::interp(~mms > 0, mms = as.name(mms_col))) %>% 
    select_("master_id", "Year", claims_col, mms_col)
  
  x1 <- x %>% filter(Year == min(Year)) %>% arrange_(lazyeval::interp(~ desc(col), col = as.name(claims_col))) %>%
    mutate_at(funs(cumsum(.)), .cols = c(mms_col, claims_col)) 
  x1 <- left_join(x1[which.min(0.8 - x1[,3] >= 0),c(2,4)], x1[min(which(x1[,3] == max(x1[,3]))),c(2,4)], by = "Year") 
  x1[1,3] <- 1-x1[1,3]
  names(x1)[2:3] <- c("Percent Members Accountable For 80% Spend", "Percent Members Accountable For Zero Spend")
  
  x2 <- x %>% filter(Year == max(Year)) %>% arrange_(lazyeval::interp(~ desc(col), col = as.name(claims_col))) %>% 
    mutate_at(funs(cumsum(.)), .cols = c(mms_col, claims_col))
  x2 <- left_join(x2[which.min(0.8 - x2[,3] >= 0),c(2,4)], x2[min(which(x2[,3] == max(x2[,3]))),c(2,4)], by = "Year") 
  x2[1,3] <- 1-x2[1,3]
  names(x2)[2:3] <- c("Percent Members Accountable For 80% Spend", "Percent Members Accountable For Zero Spend")
  
  x <- x1 %>% union_all(x2) %>%
    mutate(class = paste0(class)) %>%
    select(class, Year, `Percent Members Accountable For 80% Spend`, `Percent Members Accountable For Zero Spend`)
}

##### Read in Data #####

claims_per_member_plus <- assign_table("claims_per_member_tab", "Data/Sub_Tables/claims_per_member.csv")

claims_per_phs <- assign_table("claims_per_phs_tab", "Data/Sub_Tables/claims_per_phs.csv") 

human_flags <- assign_table("human_flags_tab", "Data/Sub_Tables/human_flags.csv")

##### Isolate Employment Flags #####

human_flags <- human_flags %>% filter(year(cov_start_dt) < max(claims_per_member_plus$Year)) %>% mutate(Year = min(claims_per_member_plus$Year)) %>%
  union_all(human_flags %>% filter(is.na(cov_end_dt) | year(cov_end_dt) >= max(claims_per_member_plus$Year)) %>% mutate(Year = max(claims_per_member_plus$Year))) %>%
  select(master_id, Year, emp_flag) %>% distinct()

##### Format Claims per Member for Different Cohorts #####

claims_per_phs <- claims_per_phs %>% group_by(master_id, Year) %>%
  summarise(claims_high = sum(total_primary*high_risk, na.rm = TRUE),
            claims_med = sum(total_primary*mod_risk, na.rm = TRUE),
            claims_low = sum(total_primary*low_risk, na.rm = TRUE),
            mms_high = sum(mms*high_risk)/12,
            mms_med = sum(mms*mod_risk)/12,
            mms_low = sum(mms*low_risk)/12)

claims_per_member_plus <- claims_per_member_plus %>% group_by(master_id, Year) %>% 
  summarise(claims_p = sum(total_med_primary_p + total_rx_primary_p, na.rm = TRUE),
            claims_np = sum(total_med_primary_np + total_rx_primary_np, na.rm = TRUE),
            mms_p = sum(mms_p)/12,
            mms_np = sum(mms_np)/12) %>% ungroup() %>%
  full_join(human_flags) %>% filter(!(is.na(emp_flag))) %>%
  left_join(claims_per_phs) %>%
  mutate(claims_all = claims_p + claims_np,
         claims_emp = (claims_p + claims_np) * emp_flag,
         claims_sp = (claims_p + claims_np) * (1-emp_flag),
         claims_emp_p = claims_p * emp_flag,
         claims_emp_np = claims_np * emp_flag,
         claims_sp_p = claims_p * (1-emp_flag),
         claims_sp_np = claims_np * (1 - emp_flag),
         mms_all = (mms_p + mms_np),
         mms_emp = (mms_p + mms_np) * emp_flag,
         mms_sp = (mms_p + mms_np) * (1-emp_flag),
         mms_emp_p = mms_p * emp_flag,
         mms_emp_np = mms_np * emp_flag,
         mms_sp_p = mms_p * (1-emp_flag),
         mms_sp_np = mms_np * (1 - emp_flag)) %>%
  mutate(claims_all = ifelse(mms_all == 0, 0, claims_all/mms_all),
         claims_p = ifelse(mms_p == 0, 0, claims_p/mms_p),
         claims_np = ifelse(mms_np ==0, 0, claims_np/mms_np),
         claims_emp = ifelse(mms_emp == 0, 0, claims_emp/mms_emp),
         claims_sp = ifelse(mms_sp == 0, 0, claims_sp/mms_sp),
         claims_emp_p = ifelse(mms_emp_p == 0, 0, claims_emp_p/mms_emp_p),
         claims_emp_np = ifelse(mms_emp_np == 0, 0, claims_emp_np/mms_emp_np),
         claims_sp_p = ifelse(mms_sp_p == 0, 0, claims_sp_p/mms_sp_p),
         claims_sp_np = ifelse(mms_sp_np == 0, 0, claims_sp_np/mms_sp_np),
         claims_high = ifelse(mms_high == 0, 0, claims_high/mms_high),
         claims_med = ifelse(mms_med == 0, 0, claims_med/mms_med),
         claims_low = ifelse(mms_low == 0, 0, claims_low/mms_low)) %>%
  mutate_at(vars(starts_with("claims")),
            funs(identity, percent_by_year(Year, .))) %>%
  mutate_at(vars(starts_with("mms")),
            funs(identity, percent_by_year(Year, .))) %>%
  select(master_id, Year, emp_flag, claims_all, claims_p, claims_np, claims_emp, claims_sp, claims_emp_p, 
         claims_emp_np, claims_sp_p, claims_sp_np, claims_high, claims_med, claims_low, 
         claims_all_percent_by_year, claims_p_percent_by_year, claims_np_percent_by_year, claims_emp_percent_by_year, claims_sp_percent_by_year, 
         claims_emp_p_percent_by_year, claims_emp_np_percent_by_year, claims_sp_p_percent_by_year, claims_sp_np_percent_by_year, 
         claims_high_percent_by_year, claims_med_percent_by_year, claims_low_percent_by_year, mms_all_percent_by_year, mms_p_percent_by_year, 
         mms_np_percent_by_year, mms_emp_percent_by_year, mms_sp_percent_by_year, mms_emp_p_percent_by_year, mms_emp_np_percent_by_year, 
         mms_sp_p_percent_by_year, mms_sp_np_percent_by_year, mms_high_percent_by_year, mms_med_percent_by_year, mms_low_percent_by_year)

claims_cumulative <- cumulative_sums("all", "All") %>%
  union_all(cumulative_sums("p", "Participants")) %>% 
  union_all(cumulative_sums("np", "Non-Participants")) %>% 
  union_all(cumulative_sums("emp", "Employees")) %>% 
  union_all(cumulative_sums("sp", "Spouses")) %>% 
  union_all(cumulative_sums("emp_p", "Participating Employees")) %>% 
  union_all(cumulative_sums("emp_np", "Non-Participating Employees")) %>% 
  union_all(cumulative_sums("sp_p", "Participating Spouses")) %>% 
  union_all(cumulative_sums("sp_np", "Non-Participating Spouses")) %>%
  union_all(cumulative_sums("high", "High Risk")) %>%
  union_all(cumulative_sums("med", "Moderate Risk")) %>%
  union_all(cumulative_sums("low", "Low Risk"))
claims_cumulative <- claims_cumulative %>%
  mutate("80% Spend Change" = c(NA, diff(`Percent Members Accountable For 80% Spend`)),
         "Zero Spend Change" = c(NA, diff(`Percent Members Accountable For Zero Spend`))) %>%
  mutate("80% Spend Change" = ifelse(Year == 2016, `80% Spend Change`, NA),
         "Zero Spend Change" = ifelse(Year == 2016, `Zero Spend Change`, NA))

write_csv(claims_cumulative, paste0(directory, "Data/Build_Tables/claims_cumulative.csv"))
print("claims_cumulative written to Data/Build_Tables")
write_csv(claims_per_member_plus, paste0(directory, "Data/Sub_Tables/claims_per_member_plus_plus.csv"))
print("claims_per_member_plus written to Data/Sub_Tables")

claims_cumulative_tab <- claims_cumulative
claims_per_member_plus_tab <- claims_per_member_plus

rm("claims_cumulative", "claims_per_member_plus", "human_flags", "cumulative_sums", "percent_by_year", "claims_per_phs")

################ Plots For Inspiration ###################

cumulative_plot <- function(comp_1, comp_2, reference){
  map <- tibble(labels = c("Participants", "Non-Participants", "All", "Employees", "Spouses", 
                           "Participating Employees", "Non-Participating Employees", 
                           "Participating Spouses", "Non-Participating Spouses",
                           "High Risk", "Moderate Risk", "Low Risk"),
                colors = c("dodgerblue3", "lightskyblue2", "gray76", "springgreen4", "palegreen", 
                           "firebrick3", "lightpink1", 
                           "mediumpurple3", "thistle3", 
                           "firebrick", "yellow3", "mediumseagreen"))
  p1 <- ggplot(subset(claims_cumulative_tab, class == reference), 
                     aes(Year, `Percent Members Accountable For 80% Spend` , color = reference)) +
    geom_point(na.rm = TRUE, size = 2) +
    geom_line() +
    geom_point(data = subset(claims_cumulative_tab, class == comp_2), 
              aes(Year, `Percent Members Accountable For 80% Spend` , color = comp_2), 
              na.rm = TRUE, size = 2) +
    geom_line(data = subset(claims_cumulative_tab, class == comp_2), 
              aes(Year, `Percent Members Accountable For 80% Spend` , color = comp_2)) +
    geom_point(data = subset(claims_cumulative_tab, class == comp_1),
              aes(Year, `Percent Members Accountable For 80% Spend` , color = comp_1),
              na.rm = TRUE, size = 2) +
    geom_line(data = subset(claims_cumulative_tab, class == comp_1),
              aes(Year, `Percent Members Accountable For 80% Spend` , color = comp_1)) +
    scale_x_continuous("Year", c(2015, 2016), labels = c("2015", "2016")) +
    scale_y_continuous("Percent of Population", limits = c(0,max(subset(claims_cumulative_tab, class %in% c(comp_1,comp_2,reference))[,3]))) +
    scale_color_manual("Cohort", 
                       values = c("Participants" = "dodgerblue3", 
                                  "Non-Participants" = "lightskyblue2", 
                                  "All" = "gray76",
                                  "Employees" = "springgreen4", 
                                  "Spouses" = "palegreen",
                                  "Participating Employees" = "firebrick3", 
                                  "Non-Participating Employees" = "lightpink1",
                                  "Participating Spouses" = "mediumpurple3", 
                                  "Non-Participating Spouses" = "thistle3",
                                  "High Risk" = "firebrick",
                                  "Moderate Risk" = "yellow3",
                                  "Low Risk" = "mediumseagreen")) +
    labs(x = "Year",
         y = "Percent of Population",
         title = "Percent Members Accountable For 80% Spend",
         subtitle = paste0(comp_1, " Vs ", comp_2)) +
    theme_bw() +
    theme(legend.position = "none")
  p2 <- ggplot(subset(claims_cumulative_tab, class == reference), 
               aes(Year, `Percent Members Accountable For Zero Spend` , color = reference)) +
    geom_point(na.rm = TRUE, size = 2) +
    geom_line() +
    geom_point(data = subset(claims_cumulative_tab, class == comp_2), 
               aes(Year, `Percent Members Accountable For Zero Spend` , color = comp_2), 
               na.rm = TRUE, size = 2) +
    geom_line(data = subset(claims_cumulative_tab, class == comp_2), 
              aes(Year, `Percent Members Accountable For Zero Spend` , color = comp_2)) +
    geom_point(data = subset(claims_cumulative_tab, class == comp_1),
               aes(Year, `Percent Members Accountable For Zero Spend` , color = comp_1),
               na.rm = TRUE, size = 2) +
    geom_line(data = subset(claims_cumulative_tab, class == comp_1),
              aes(Year, `Percent Members Accountable For Zero Spend` , color = comp_1)) +
    scale_x_continuous("Year", c(2015, 2016), labels = c("2015", "2016")) +
    scale_y_continuous("Percent of Population", limits = c(0,max(subset(claims_cumulative_tab, class %in% c(comp_1,comp_2,reference))[,4]))) +
    scale_color_manual("Cohort", 
                       values = c("Participants" = "dodgerblue3", 
                                  "Non-Participants" = "lightskyblue2", 
                                  "All" = "gray76",
                                  "Employees" = "springgreen4", 
                                  "Spouses" = "palegreen",
                                  "Participating Employees" = "firebrick3", 
                                  "Non-Participating Employees" = "lightpink1",
                                  "Participating Spouses" = "mediumpurple3", 
                                  "Non-Participating Spouses" = "thistle3",
                                  "High Risk" = "firebrick",
                                  "Moderate Risk" = "yellow3",
                                  "Low Risk" = "mediumseagreen")) +
    labs(x = "Year",
         y = "Percent of Population",
         title = "Percent Members Accountable For Zero Spend",
         subtitle = paste0(comp_1, " Vs ", comp_2)) +
    theme_bw()
  gridExtra::grid.arrange(grobs = list(p1, p2), main = "Top and Bottom Comparisons", ncol = 2)
}

# cumulative_plot("Non-Participants", "Participants", "All")
# cumulative_plot("Employees", "Spouses", "All")
# cumulative_plot("Participating Employees", "Non-Participating Employees", "Employees")
# cumulative_plot("Participating Spouses", "Non-Participating Spouses", "Spouses")
# cumulative_plot("Low Risk", "Moderate Risk", "High Risk")
