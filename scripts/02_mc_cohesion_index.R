# Code to replicate the Monetary Council cohesion index

library(dplyr)
library(readxl)
library(readr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(zoo)

# importing the voting records from excel
mt_voting_raw <- read_excel("data/input/mnb-mt-voting.xlsx", sheet = "r_import")

# column names from the excel sheet
mnb_cols <- c("name", "(Voted to increase)", "(Voted to reduce)", "(Voted to maintain)", "Meetings attended", "10/24/2005", "11/28/2005", "12/19/2005", "1/23/2006", "2/27/2006", "3/20/2006", "4/24/2006", "5/22/2006", "6/19/2006", "7/24/2006", "8/28/2006", "9/25/2006", "10/24/2006", "11/20/2006", "12/18/2006", "1/22/2007", "2/26/2007", "3/26/2007", "4/23/2007", "5/21/2007", "6/25/2007", "7/23/2007", "8/27/2007", "9/24/2007", "10/29/2007", "11/26/2007", "12/17/2007", "1/21/2008", "2/25/2008", "3/31/2008", "4/28/2008", "5/26/2008", "6/23/2008", "7/21/2008", "8/25/2008", "9/29/2008", "10/20/2008", "10/22/2008", "11/24/2008", "12/8/2008", "12/22/2008", "1/19/2009", "2/23/2009", "3/23/2009", "4/20/2009", "5/25/2009", "6/22/2009", "7/27/2009", "8/24/2009", "9/28/2009", "10/19/2009", "11/23/2009", "12/21/2009", "1/25/2010", "2/22/2010", "3/29/2010", "4/26/2010", "5/31/2010", "6/21/2010", "7/19/2010", "8/23/2010", "9/27/2010", "10/25/2010", "11/29/2010", "12/20/2010", "1/24/2011", "2/21/2011", "3/28/2011", "4/18/2011", "5/16/2011", "6/20/2011", "7/26/2011", "8/23/2011", "9/20/2011", "10/25/2011", "11/29/2011", "12/20/2011", "1/24/2012", "2/28/2012", "3/27/2012", "4/24/2012", "5/29/2012", "6/26/2012", "7/24/2012", "8/28/2012", "9/25/2012", "10/30/2012", "11/27/2012", "12/18/2012", "1/29/2013", "2/26/2013", "3/26/2013", "4/23/2013", "5/28/2013", "6/25/2013", "7/23/2013", "8/27/2013", "9/24/2013", "10/29/2013", "11/26/2013", "12/17/2013", "1/21/2014", "2/18/2014", "3/25/2014", "4/29/2014", "5/27/2014", "6/24/2014", "7/22/2014", "8/26/2014", "9/23/2014", "10/28/2014", "11/25/2014", "12/16/2014", "1/27/2015", "2/24/2015", "3/24/2015", "4/21/2015", "5/26/2015", "6/23/2015", "7/21/2015", "8/25/2015", "9/22/2015", "10/20/2015", "11/17/2015", "12/15/2015", "1/26/2016", "2/23/2016", "3/22/2016", "4/26/2016", "5/24/2016", "6/21/2016", "7/26/2016", "8/23/2016", "9/20/2016", "10/25/2016", "11/22/2016", "12/20/2016", "1/24/2017", "2/28/2017", "3/28/2017", "4/25/2017", "5/23/2017", "6/20/2017", "7/18/2017", "8/22/2017", "9/19/2017", "10/24/2017", "11/21/2017", "12/19/2017", "1/30/2018", "2/27/2018", "3/27/2018", "4/24/2018", "5/22/2018", "6/19/2018", "7/24/2018", "8/21/2018", "9/18/2018", "10/16/2018", "11/20/2018", "12/18/2018", "1/29/2019", "2/26/2019", "3/26/2019", "4/30/2019", "5/28/2019", "6/25/2019", "7/23/2019", "8/27/2019", "9/24/2019")


# adding the column names to the imported data
names(mt_voting_raw) <- mnb_cols


# calculating the cohesion index
#
# step 1:
# reshaping data into long format from wide
#
# step 2:
# group by meeting date 
#
# step 3: 
# in the mutate function, we create the mt_cohesion index
#
# step 4: selecting the distinct meeting - index pairs, and dropping duplicate rows

mt_df <- mt_voting_raw %>% 
  # step 1
  gather("10/24/2005":"9/24/2019", key = "date", value = "vote") %>% 
  drop_na() %>% 
  mutate(date = mdy(date),
         vote = as.numeric(vote)) %>% 
  # step 2
  group_by(date) %>% 
  # step 3
  mutate(mt_cohesion = case_when(sum(vote) == 0 ~ 1,
                                 sum(vote != 0) / n() < 0.5 ~ 1 - (sum(vote != 0) / n()),
                                 sum(vote) != 0 ~ (sum(vote != 0) / n()))) %>% 
  ungroup() %>% 
  # step 4
  select(date, mt_cohesion) %>% 
  distinct(date, mt_cohesion)



# skew
macro_df <- read_csv("data/input/data_macro.csv") %>% 
  select(date_match, i) %>% 
  mutate(i_lag = lag(i))


skew <- mt_voting_raw %>% 
  # step 1
  gather("10/24/2005":"9/24/2019", key = "date", value = "vote") %>% 
  drop_na() %>% 
  mutate(date = mdy(date),
         vote = as.numeric(vote),
         date_match = as.character(format(as.yearmon(date), "%Y-%m"))) %>% 
  select(name, date, date_match, vote) %>% 
  left_join(macro_df) %>% 
  mutate(vote = vote * 100,
         voted_for = vote + i_lag,
         i_actual = i) %>% 
  group_by(date) %>% 
  mutate(skew = mean(voted_for) - i_actual) %>% 
  ungroup() %>% 
  select(date, skew) %>% 
  distinct(date, skew)


mt_df_final <- left_join(mt_df, skew)



# export
write_csv(mt_df_final, file = "data/output/mt_coherence.csv")

