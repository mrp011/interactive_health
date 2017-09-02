############ Level 6 #############

### This builds the pmpy vs % spend

### Input Tables: claims_per_member_plus

### Output Tables: pmpy_spend_phs

### Author: Michelle Powell

### Sourced By: pmpy_spend_phs()

###########################################
###########################################

member_claims <- assign_table("claims_per_member_plus_tab", "Data/Sub_Tables/claims_per_member_plus.csv")

high <- member_claims %>% filter(mms_high_percent_by_year > 0) %>% select(Year, claims_high, claims_high_percent_by_year)

high_buckets <- hist(high$claims_high, breaks = round(dim(high)[1]/5))$mids

high$buckets <- high_buckets[sapply(high$claims_high, function(x) which.min(abs(x - high_buckets)))]

high <- high  %>%
  union_all(tibble('Year' = rep(c(min(high$Year), max(high$Year)), each = length(high_buckets)+1), 
                   'buckets' = c(0, high_buckets, 0, high_buckets), 
                   'claims_high_percent_by_year' = rep(0, length(high_buckets)*2+2))) %>% 
  group_by(Year, buckets) %>% 
  summarise(percent_spend = sum(claims_high_percent_by_year), class = "High Risk") %>% 
  arrange(Year, buckets)

moderate <- member_claims %>% filter(mms_med_percent_by_year > 0) %>% select(Year, claims_med, claims_med_percent_by_year)

mod_buckets <- hist(moderate$claims_med, breaks = round(dim(moderate)[1]/5))$mids

moderate$buckets <- mod_buckets[sapply(moderate$claims_med, function(x) which.min(abs(x - mod_buckets)))]

moderate <- moderate  %>%
  union_all(tibble('Year' = rep(c(min(moderate$Year), max(moderate$Year)), each = length(mod_buckets)+1), 
                   'buckets' = c(0, mod_buckets, 0, mod_buckets), 
                   'claims_med_percent_by_year' = rep(0, length(mod_buckets)*2+2))) %>% 
  group_by(Year, buckets) %>% 
  summarise(percent_spend = sum(claims_med_percent_by_year), class = "Moderate Risk") %>% 
  arrange(Year, buckets)

low <- member_claims %>% filter(mms_low_percent_by_year > 0) %>% select(Year, claims_low, claims_low_percent_by_year)

low_buckets <- hist(low$claims_low, breaks = round(dim(low)[1]/5))$mids

low$buckets <- low_buckets[sapply(low$claims_low, function(x) which.min(abs(x - low_buckets)))]

low <- low  %>%
  union_all(tibble('Year' = rep(c(min(low$Year), max(low$Year)), each = length(low_buckets)+1), 
                   'buckets' = c(0, low_buckets, 0, low_buckets), 
                   'claims_low_percent_by_year' = rep(0, length(low_buckets)*2+2))) %>% 
  group_by(Year, buckets) %>% 
  summarise(percent_spend = sum(claims_low_percent_by_year), class = "Low Risk") %>% 
  arrange(Year, buckets)

pmpy_spend <- low %>% union_all(moderate) %>% union_all(high)

write_csv(pmpy_spend, paste0(directory, "Data/Build_Tables/pmpy_spend_phs.csv"))
print("pmpy_spend_phs written to Data/Build_Tables")

pmpy_spend_phs_tab <- pmpy_spend

rm("member_claims", "high", "low", "moderate", "pmpy_spend", "high_buckets", "low_buckets", "mod_buckets")

