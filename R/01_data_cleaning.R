library(here)
library(tidyverse)

df <- read_csv(here("data", "raw", "global_ads_performance_dataset.csv"))

print(head(df))
