library(here)
library(tidyverse)

# Load raw data set
df <- read_csv(here("data", "raw", "global_ads_performance_dataset.csv"))

any(is.na(df)) # check for N/A
any(duplicated(df)) # check for duplicates


#Clean
df_clean <- df %>%
  mutate(across(c(platform, campaign_type, industry, country), as.factor))

str(df_clean)

levels(df_clean$country)

write_csv(df_clean, here("data", "cleaned", "global_ads_performance_dataset_cleaned.csv"))

#loading the Rds version which contains the categorical data with labels
saveRDS(df_clean, here("data", "cleaned", "global_ads_performance_dataset_cleaned.rds"))

df_clean <- readRDS(here("data", "cleaned", "global_ads_performance_dataset_cleaned.rds")) # loading the factored data set

print(head(df_clean)) # now it's factored

# checking for logical issues

sum(df_clean$conversions > df_clean$clicks) 
# should be 0 — you can't have more conversions than clicks

sum(df_clean$clicks > df_clean$impressions)
# should be 0 — clicks must always be less than or equal to impressions

mean(abs(df_clean$CTR - (df_clean$clicks / df_clean$impressions)))
# should be very close to 0 (small e-06 or smaller) — just rounding differences

mean(abs(df_clean$CPC - (df_clean$ad_spend / df_clean$clicks)))
# should be ~0 — CPC is defined as ad_spend / clicks

mean(abs(df_clean$CPA - (df_clean$ad_spend / df_clean$conversions)))
# should be ~0 — CPA should equal ad_spend / conversions

mean(abs(df_clean$ROAS - (df_clean$revenue / df_clean$ad_spend)))
# should be ~0 — ROAS should equal revenue / ad_spend


