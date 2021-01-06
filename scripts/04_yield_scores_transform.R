library(readr)
library(dplyr)
library(stringr)
library(zoo)

# function to rescale the yield scores to between 1-0
# we also add 0.001 to the 0 values to avoid -Inf when we log transform
minmax_scale <- function(x) {
  resc <- (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  
  resc <- ifelse(resc == 0, resc + 0.001, resc)
  
  return(resc)
  
}

# import data
yield_scores <- read_csv("data/output/yield_scores.csv")

# steps in the mutate function:
# 1 transform all variables from total to long_term with the minmax_scale function
# 2 log transform the variables
# 3 create the date match variable
yield_final <- yield_scores %>% 
  mutate_at(.vars = vars(total:long_term), .funs = list(resc = minmax_scale)) %>% 
  mutate_at(.vars = vars(total_resc:long_term_resc), list(ln = log)) %>% 
  mutate(date_match = gsub(pattern = "_", replacement =  "-", x = Date),
         date_match = as.Date(str_c(date_match, "-01", sep = "")),
         date_match = as.character(format(zoo::as.yearmon(date_match), "%Y-%m"))) %>% 
  select(date_match, ends_with("_ln"), ends_with("_resc"), starts_with("r"))

# create better names
names(yield_final)[2:7] <- c("log_total", "log_in_year", "log_out_year", "log_short_term", "log_medium_term", "log_long_term")

# export
write_csv(yield_final, "data/output/log_yield_scores.csv")
