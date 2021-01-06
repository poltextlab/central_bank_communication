# Reproduction code for creating the main dataset

library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
library(readr)
library(zoo)


# 1 loading the macro controls
macro_df <- read_csv("data/input/data_macro.csv")

# 2 loading the sentiment scores
sentiment_df <- read_csv("data/output/sentiment_scores_v2.csv") %>% 
  mutate(date_match = as.character(format(as.yearmon(date), "%Y-%m"))) %>% 
  rename(date_text = date) %>% 
  select(id_pr, date_text, date_match, net_hawk_pr, lm_baseline, net_hawk_agh)

# 3 loading the monetary council cohesion scores
# hand removing 2 time point (2008-10 is NA and 2008-12 had 2 meeting in one month: one with 1 and the other 0.8 value, and we drop the latter in row 39) that causes duplication during the merging
cohesion_df <- read_csv("data/output/mt_coherence.csv") %>% 
  mutate(date_match = as.character(format(as.yearmon(date), "%Y-%m"))) %>% 
  select(-date) %>% 
  drop_na() %>% 
  slice(-39)

# 4 loading the yield scores
yield_scores <- read_csv("data/output/log_yield_scores.csv")

  
# 4 ecb interest sheet
ecb_rate <- read_csv("data/input/ecb_rates.csv") %>% 
  select(date, monthly_mlf) %>% 
  drop_na() %>% 
  mutate(date = mdy(date),
         date_match = as.character(format(as.yearmon(date), "%Y-%m")))


# 5 fed interest sheet
fed_rate <- read_csv("data/input/FEDFUNDS.csv") %>% 
  mutate(DATE = mdy(DATE)) %>% 
  rename(date = DATE, fed_i = FEDFUNDS) %>%
  mutate(date_match = as.character(format(as.yearmon(date), "%Y-%m"))) %>% 
  group_by(date_match) %>% 
  mutate(effective_fed_i = mean(fed_i)) %>% 
  ungroup()


# 6 ecb assets
ecb_bs <- read_csv("data/input/ECBASSETSW.csv") %>% 
  rename(date = DATE, ecb_bs = ECBASSETSW) %>%
  mutate(date_match = as.character(format(as.yearmon(date), "%Y-%m")))


# 7 fed assets
fed_bs <- read_csv("data/input/fed_assets.csv") %>% 
  rename(date = DATE, fed_bs = WALCL) %>%
  mutate(date_match = as.character(format(as.yearmon(date), "%Y-%m")))


# 5 combining the datasets
# all left joins use the date_match variable as key
# adding the MNB governors to the dataset (baseline: Zsigmond JARAI)
dataset_final <- macro_df %>% 
  left_join(yield_scores, by = "date_match") %>% 
  left_join(cohesion_df, by = "date_match") %>% 
  left_join(sentiment_df, by = "date_match") %>%
  left_join(ecb_rate, by = "date_match") %>% 
  left_join(fed_rate, by = "date_match") %>% 
  left_join(ecb_bs, by = "date_match") %>% 
  left_join(fed_bs, by = "date_match") %>% 
  mutate(simor = ifelse(date_text >= "2007-03-02" & date_text <="2013-03-02", 1, 0),
         matolcsy = ifelse(date_text >= "2013-03-03", 1, 0),
         net_hawk_pr = ifelse(net_hawk_pr <= 0, 0.00001, net_hawk_pr),
          net_hawk_agh = ifelse(net_hawk_agh <= 0, 0.00001, net_hawk_agh)) %>% 
  mutate(log_nethawkish = log(net_hawk_pr),
         log_baseline_PR = log(lm_baseline),
         log_agh_PR = log(net_hawk_agh)) %>% 
  rename(date = Date, Simor = simor, Matolcsy = matolcsy, nethawkish = net_hawk_pr, log_intra_year = log_short_term, log_1to3year = log_medium_term) %>% 
  select(-date_match, -contains("."))



# 6 export the finished combined dataset for the analysis in Stata (note the NAs are coded as )
write_csv(dataset_final, "data/output/dataset_complete_v2.csv", na = ".")
