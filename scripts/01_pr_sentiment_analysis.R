
library(readr)
library(zoo)
library(dplyr)
library(stringr)
library(quanteda)
library(ggplot2)
library(lubridate)


# function for the sentiment analysis with our dictionary
# x = tokens object from quanteda
# dictionary = the name of our dictionary
# window = the lookup window for the tokens_keep 
# grouped = option, defaults to TRUE which aggregates by sentence sentiment up to document level

monetary_sentiment <- function(x, monetary_dictionary, window = 10) {
  
  # doing it step by step
  # keep tokens that are within 10 tokens from a term on the normal macro terms list
  macro_normal <- tokens_keep(x, pattern = phrase(monetary_dictionary["terms_normal"]), window = 10)
  
  monetary_sent_normal <- dfm(macro_normal) %>%
    dfm_weight(scheme = "prop") %>%
    dfm(dictionary = monetary_dictionary[c("hawk_normal", "dove_normal")]) %>%
    convert(to = "data.frame") %>%
    mutate(id = str_extract(doc_id, "[0-9]+?(?=\\.)"))
  
  
  deficit_term <- tokens_keep(x, pattern = phrase(monetary_dictionary["deficit_term"]), window = 10)
  
  deficit_term_sent <- dfm(deficit_term) %>%
    dfm_weight(scheme = "prop") %>%
    dfm(dictionary = monetary_dictionary[c("deficit_hawk", "deficit_dove")]) %>%
    convert(to = "data.frame") %>%
    mutate(id = str_extract(doc_id, "[0-9]+?(?=\\.)"))
  
  
  
  
  
  inflation_term <- tokens_keep(x, pattern = phrase(monetary_dictionary["inflation_term"]), window = 10)
  
  inflation_term_sent <- dfm(inflation_term) %>%
    dfm_weight(scheme = "prop") %>%
    dfm(dictionary = monetary_dictionary[c("inflation_hawk", "inflation_dove")]) %>%
    convert(to = "data.frame") %>%
    mutate(id = str_extract(doc_id, "[0-9]+?(?=\\.)"))
  
  
  
  
  
  unemp_term <- tokens_keep(x, pattern = phrase(monetary_dictionary["unemp_term"]), window = 10)
  
  unemp_term_sent <- dfm(unemp_term) %>%
    dfm_weight(scheme = "prop") %>%
    dfm(dictionary = monetary_dictionary[c("unemp_hawk", "unemp_dove")]) %>%
    convert(to = "data.frame") %>%
    mutate(id = str_extract(doc_id, "[0-9]+?(?=\\.)"))
  
  
  
  
  interest_term <- tokens_keep(x, pattern = phrase(monetary_dictionary["interest_term"]), window = 10)
  
  interest_term_sent <- dfm(interest_term) %>%
    dfm_weight(scheme = "prop") %>%
    dfm(dictionary = monetary_dictionary[c("interest_hawk", "interest_dove")]) %>%
    convert(to = "data.frame") %>%
    mutate(id = str_extract(doc_id, "[0-9]+?(?=\\.)"))
  
  
  monetary_results_final <- monetary_sent_normal %>%
    left_join(deficit_term_sent, by = c("doc_id", "id")) %>% 
    left_join(inflation_term_sent, by = c("doc_id", "id")) %>%
    left_join(unemp_term_sent, by = c("doc_id", "id")) %>%
    left_join(interest_term_sent, by = c("doc_id", "id")) %>% 
    mutate(hawk = hawk_normal + deficit_hawk + inflation_hawk + unemp_hawk + interest_hawk,
           dove = dove_normal + deficit_dove + inflation_dove + unemp_dove + interest_dove) %>%
    group_by(id) %>%
    mutate_if(is.numeric, .funs = list(doc = sum)) %>%
    ungroup() %>%
    mutate(net_hawk_doc = (hawk_doc - dove_doc)  + 1) %>%
    mutate(id = as.numeric(id)) %>%
    select(id, ends_with("_doc")) %>%
    distinct()
  
  return(monetary_results_final)
}


# 1 data import ----
# mnb press release dataframe
pr_clean <- readr::read_csv("data/input/mnb_pr_corpus.csv")


# hand coded monetary dictionary
monetary_dict <- readRDS("data/input/monetary_dictionary2.rds")



# 2 data prep ----
# corpus
pr_corp <- corpus(pr_clean) %>%
  corpus_reshape(to = "sentences")

# tokens
pr_toks <- tokens(pr_corp, remove_punct = TRUE) %>% 
  tokens_tolower()
  


# 3 sentiment analysis ----
# using the custom function defined at the beginning of the script
pr_sentiment <- monetary_sentiment(pr_toks, monetary_dict, window = 5)

pr_sentiment_df <- left_join(pr_clean, pr_sentiment) %>% 
  mutate(id = str_c("pr", row_number(), sep = "_")) %>% 
  select(id_pr = id, date, net_hawk_pr = net_hawk_doc)


#write_csv(pr_sentiment_df, "data/output/press_release_sentiment.csv")



###############################################################
# 4 baseline L-M analysis -------------
# SENTIMENT SCORES FOR THE APPENDIX MATERIALS
###############################################################
#
# Using the Loughran-McDonald dictionary with a standard lookup and with the focused lookup (where the L-M dictionary items are subtituted for our macroeconomic terms)
# 
# sentiment analysis with both dictionary
pr_corp_baseline <- corpus(pr_clean)

pr_toks_baseline <- tokens(pr_corp_baseline, remove_punct = TRUE) %>%
  tokens_tolower()

# LM dictionary for the baseline analysis
# MINUTES
load("data/input/data_dictionary_LoughranMcDonald.rda")


# baseline result with simply applying the L-M dictionary to the press release corpus
pr_dfm_baseline <- dfm(pr_toks_baseline) %>% 
  dfm_weight(scheme = "prop")

baseline_pr <- dfm(pr_dfm_baseline, dictionary = data_dictionary_LoughranMcDonald[1:2]) %>% 
  convert(to = "data.frame") %>% 
  mutate(id_pr = str_c("pr", row_number(), sep = "_"),
         PRnet_positive_baseline = (positive - negative)  + 1) %>% 
  rename(pos_baseline = positive, neg_baseline = negative) %>% 
  select(id_pr, PRnet_positive_baseline)



# 5 AGH2019 ----
agh2019 <- read_rds("data/input/ag_2019.rds")


cpi_term <- tokens_keep(pr_toks, pattern = phrase(agh2019["cpi_term"]), window = 10)

cpi_sent <- dfm(cpi_term) %>%
  dfm_weight(scheme = "prop") %>%
  dfm(dictionary = agh2019[c("cpi_hawkish", "cpi_dovish")]) %>% 
  convert(to = "data.frame") %>% 
  mutate(id = str_extract(doc_id, "[0-9]+?(?=\\.)"))


ip_term <- tokens_keep(pr_toks, pattern = phrase(agh2019["ip_term"]), window = 10)

ip_term_sent <- dfm(ip_term) %>%
  dfm_weight(scheme = "prop") %>%
  dfm(dictionary = agh2019[c("ip_hawkish", "ip_dovish")]) %>%
  convert(to = "data.frame") %>%
  mutate(id = str_extract(doc_id, "[0-9]+?(?=\\.)"))




cs_term <- tokens_keep(pr_toks, pattern = phrase(agh2019["cs_term"]), window = 10)

cs_term_sent <- dfm(cs_term) %>%
  dfm_weight(scheme = "prop") %>%
  dfm(dictionary = agh2019[c("cs_hawkish", "cs_dovish")]) %>%
  convert(to = "data.frame") %>%
  mutate(id = str_extract(doc_id, "[0-9]+?(?=\\.)"))





econ_terms <- tokens_keep(pr_toks, pattern = phrase(agh2019["econ_terms"]), window = 10)

econ_terms_sent <- dfm(econ_terms) %>%
  dfm_weight(scheme = "prop") %>%
  dfm(dictionary = agh2019[c("econ_hawkish", "econ_dovish")]) %>%
  convert(to = "data.frame") %>%
  mutate(id = str_extract(doc_id, "[0-9]+?(?=\\.)"))


ru_term <- tokens_keep(pr_toks, pattern = phrase(agh2019["ru_term"]), window = 10)

ru_term_sent <- dfm(ru_term) %>%
  dfm_weight(scheme = "prop") %>%
  dfm(dictionary = agh2019[c("ru_hawkish", "ru_dovish")]) %>%
  convert(to = "data.frame") %>%
  mutate(id = str_extract(doc_id, "[0-9]+?(?=\\.)"))




emp_term <- tokens_keep(pr_toks, pattern = phrase(agh2019["emp_term"]), window = 10)

emp_term_sent <- dfm(emp_term) %>%
  dfm_weight(scheme = "prop") %>%
  dfm(dictionary = agh2019[c("emp_hawkish", "emp_dovish")]) %>%
  convert(to = "data.frame") %>%
  mutate(id = str_extract(doc_id, "[0-9]+?(?=\\.)"))




lm_term <- tokens_keep(pr_toks, pattern = phrase(agh2019["lm_term"]), window = 10)

lm_term_sent <- dfm(lm_term) %>%
  dfm_weight(scheme = "prop") %>%
  dfm(dictionary = agh2019[c("lm_hawkish", "lm_dovish")]) %>%
  convert(to = "data.frame") %>%
  mutate(id = str_extract(doc_id, "[0-9]+?(?=\\.)"))



unemp_term <- tokens_keep(pr_toks, pattern = phrase(agh2019["unemp_term"]), window = 10)

unemp_term_sent <- dfm(unemp_term) %>%
  dfm_weight(scheme = "prop") %>%
  dfm(dictionary = agh2019[c("unemp_hawkish", "unemp_dovish")]) %>%
  convert(to = "data.frame") %>%
  mutate(id = str_extract(doc_id, "[0-9]+?(?=\\.)"))




agh2019_results_final <- cpi_sent %>%
  left_join(ip_term_sent, by = c("doc_id", "id")) %>% 
  left_join(cs_term_sent, by = c("doc_id", "id")) %>%
  left_join(econ_terms_sent, by = c("doc_id", "id")) %>%
  left_join(ru_term_sent, by = c("doc_id", "id")) %>%
  left_join(emp_term_sent, by = c("doc_id", "id")) %>%
  left_join(lm_term_sent, by = c("doc_id", "id")) %>% 
  left_join(unemp_term_sent, by = c("doc_id", "id")) %>% 
  mutate(hawk = ip_hawkish + econ_hawkish + emp_hawkish + unemp_hawkish,
         dove = ip_dovish + econ_dovish + emp_dovish + unemp_dovish) %>%
  group_by(id) %>%
  mutate_if(is.numeric, .funs = list(doc = sum)) %>%
  ungroup() %>%
  mutate(net_hawk_doc = (hawk_doc - dove_doc)  + 1) %>%
  mutate(id = as.numeric(id)) %>%
  select(id, ends_with("_doc")) %>%
  distinct()

agh2019_merge <- agh2019_results_final %>% 
  mutate(id_pr = str_c("pr", row_number(), sep = "_")) %>% 
  select(id_pr, net_hawk_agh = net_hawk_doc)


# combining the baseline results for the press release corpus
baseline_results_pr <- pr_clean %>%
  mutate(id_pr = str_c("pr", row_number(), sep = "_")) %>%
  left_join(baseline_pr) %>%
  left_join(agh2019_merge) %>% 
  select(-id, -text, -year, -date)


# combining the results ----
sentiment_results_pr <- pr_sentiment_df %>%
  left_join(baseline_results_pr, by = "id_pr") %>% 
  select(id_pr, date, net_hawk_pr, lm_baseline = PRnet_positive_baseline, net_hawk_agh)  

# EXPORT SENTIMENT SCORES
write_csv(sentiment_results_pr, "data/output/sentiment_scores_v2.csv")
