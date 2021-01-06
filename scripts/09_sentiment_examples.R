library(tidyverse)
library(quanteda)



macro_data <- read_csv("data/output/dataset_complete_v2.csv", na = ".")

minmax <- macro_data %>% 
  mutate(min_sen = ifelse(nethawkish == min(nethawkish), 1, 0),
         max_sen = ifelse(nethawkish == max(nethawkish), 1, 0)) %>% 
  filter(min_sen == 1 | max_sen == 1) %>% 
  select(date_text, min_sen, max_sen, nethawkish)


minmax$date_text



# vignette for most dovish press release 
pr_clean <- readr::read_csv("data/input/mnb_pr_corpus.csv") %>% 
#   filter(date == "2011-09-20") for dovish
  filter(date == "2018-06-19")


pr_corp <- corpus(pr_clean) %>%
  corpus_reshape(to = "sentences")

pr_toks <- tokens(pr_corp, remove_punct = TRUE) %>%
  tokens_tolower()

monetary_dictionary <- readRDS("data/input/monetary_dictionary2.rds")



# doing it step by step
# keep tokens that are within 10 tokens from a term on the normal macro terms list
macro_normal <- tokens_keep(pr_toks, pattern = phrase(monetary_dictionary["terms_normal"]), window = 5)

monetary_sent_normal <- dfm(macro_normal) %>%
  dfm_weight(scheme = "prop") %>%
  dfm(dictionary = monetary_dictionary[c("dove_normal", "hawk_normal")]) %>%
  convert(to = "data.frame") %>%
  mutate(id = str_extract(document, "[0-9]+?(?=\\.)"))




deficit_term <- tokens_keep(pr_toks, pattern = phrase(monetary_dictionary["deficit_term"]), window = 5)

deficit_term_sent <- dfm(deficit_term) %>%
  dfm_weight(scheme = "prop") %>%
  dfm(dictionary = monetary_dictionary[c("deficit_hawk", "deficit_dove")]) %>%
  convert(to = "data.frame") %>%
  mutate(id = str_extract(document, "[0-9]+?(?=\\.)"))





inflation_term <- tokens_keep(pr_toks, pattern = phrase(monetary_dictionary["inflation_term"]), window = 5)

inflation_term_sent <- dfm(inflation_term) %>%
  dfm_weight(scheme = "prop") %>%
  dfm(dictionary = monetary_dictionary[c("inflation_hawk", "inflation_dove")]) %>%
  convert(to = "data.frame") %>%
  mutate(id = str_extract(document, "[0-9]+?(?=\\.)"))





unemp_term <- tokens_keep(pr_toks, pattern = phrase(monetary_dictionary["unemp_term"]), window = 5)

unemp_term_sent <- dfm(unemp_term) %>%
  dfm_weight(scheme = "prop") %>%
  dfm(dictionary = monetary_dictionary[c("unemp_hawk", "unemp_dove")]) %>%
  convert(to = "data.frame") %>%
  mutate(id = str_extract(document, "[0-9]+?(?=\\.)"))




interest_term <- tokens_keep(pr_toks, pattern = phrase(monetary_dictionary["interest_term"]), window = 5)

interest_term_sent <- dfm(interest_term) %>%
  dfm_weight(scheme = "prop") %>%
  dfm(dictionary = monetary_dictionary[c("interest_hawk", "interest_dove")]) %>%
  convert(to = "data.frame") %>%
  mutate(id = str_extract(document, "[0-9]+?(?=\\.)"))


most_dovish <- data.frame(original_sentence = rep(NA, times = nrow(deficit_term_sent)),
                          
                      macro_normal = rep(NA, times = nrow(deficit_term_sent)),
                      hawk_normal = rep(NA, times = nrow(deficit_term_sent)),
                      dove_normal = rep(NA, times = nrow(deficit_term_sent)),
                      
                      deficit_term = rep(NA, times = nrow(deficit_term_sent)),
                      deficit_dove = rep(NA, times = nrow(deficit_term_sent)),
                      deficit_hawk = rep(NA, times = nrow(deficit_term_sent)),
                      
                      inflation_term = rep(NA, times = nrow(inflation_term_sent)),
                      inflation_dove = rep(NA, times = nrow(inflation_term_sent)),
                      inflation_hawk = rep(NA, times = nrow(inflation_term_sent)),
                      
                      unemp_term = rep(NA, times = nrow(unemp_term_sent)),
                      unemp_dove = rep(NA, times = nrow(unemp_term_sent)),
                      unemp_hawk = rep(NA, times = nrow(unemp_term_sent)),
                      
                      interest_term = rep(NA, times = nrow(interest_term_sent)),
                      interest_dove = rep(NA, times = nrow(interest_term_sent)),
                      interest_hawk = rep(NA, times = nrow(interest_term_sent)),
                      row_id = 1:nrow(interest_term_sent))



for (i in 1:nrow(most_dovish)) {
  most_dovish[[i, "original_sentence"]] <- str_c(pr_toks[[i]], collapse = " ")
  
  most_dovish[[i, "macro_normal"]] <- str_c(macro_normal[[i]], collapse = " ")
  most_dovish[[i, "hawk_normal"]] <- monetary_sent_normal[[i, "hawk_normal"]]
  most_dovish[[i, "dove_normal"]] <- monetary_sent_normal[[i, "dove_normal"]]
  
  most_dovish[[i, "deficit_term"]] <- str_c(deficit_term[[i]], collapse = " ")
  # most_dovish[[i, "deficit_dove"]] <- deficit_term_sent[[i, "deficit_dove"]]
  # most_dovish[[i, "deficit_hawk"]] <- deficit_term_sent[[i, "deficit_hawk"]]

  most_dovish[[i, "inflation_term"]] <- str_c(inflation_term[[i]], collapse = " ")
  most_dovish[[i, "inflation_hawk"]] <- inflation_term_sent[[i, "inflation_hawk"]]
  most_dovish[[i, "inflation_dove"]] <- inflation_term_sent[[i, "inflation_dove"]]

  most_dovish[[i, "unemp_term"]] <- str_c(unemp_term[[i]], collapse = " ")
  # most_dovish[[i, "unemp_hawk"]] <- unemp_term_sent[[i, "unemp_hawk"]]
  # most_dovish[[i, "unemp_dove"]] <- unemp_term_sent[[i, "unemp_dove"]]

  most_dovish[[i, "interest_term"]] <- str_c(interest_term[[i]], collapse = " ")
  # most_dovish[[i, "interest_hawk"]] <- interest_term_sent[[i, "hawk_normal"]]
  # most_dovish[[i, "interest_dove"]] <- interest_term_sent[[i, "dove_normal"]]

  
}

write_csv(most_dovish, "temp_dev/most_dovish_2011-09-20.csv")



most_hawkish <- data.frame(original_sentence = rep(NA, times = nrow(deficit_term_sent)),
                          
                          macro_normal = rep(NA, times = nrow(deficit_term_sent)),
                          hawk_normal = rep(NA, times = nrow(deficit_term_sent)),
                          dove_normal = rep(NA, times = nrow(deficit_term_sent)),
                          
                          deficit_term = rep(NA, times = nrow(deficit_term_sent)),
                          deficit_dove = rep(NA, times = nrow(deficit_term_sent)),
                          deficit_hawk = rep(NA, times = nrow(deficit_term_sent)),
                          
                          inflation_term = rep(NA, times = nrow(inflation_term_sent)),
                          inflation_dove = rep(NA, times = nrow(inflation_term_sent)),
                          inflation_hawk = rep(NA, times = nrow(inflation_term_sent)),
                          
                          unemp_term = rep(NA, times = nrow(unemp_term_sent)),
                          unemp_dove = rep(NA, times = nrow(unemp_term_sent)),
                          unemp_hawk = rep(NA, times = nrow(unemp_term_sent)),
                          
                          interest_term = rep(NA, times = nrow(interest_term_sent)),
                          interest_dove = rep(NA, times = nrow(interest_term_sent)),
                          interest_hawk = rep(NA, times = nrow(interest_term_sent)),
                          row_id = 1:nrow(interest_term_sent))



for (i in 1:nrow(most_hawkish)) {
  most_hawkish[[i, "original_sentence"]] <- str_c(pr_toks[[i]], collapse = " ")
  
  most_hawkish[[i, "macro_normal"]] <- str_c(macro_normal[[i]], collapse = " ")
  most_hawkish[[i, "hawk_normal"]] <- monetary_sent_normal[[i, "hawk_normal"]]
  most_hawkish[[i, "dove_normal"]] <- monetary_sent_normal[[i, "dove_normal"]]
  
  most_hawkish[[i, "deficit_term"]] <- str_c(deficit_term[[i]], collapse = " ")
  most_hawkish[[i, "deficit_dove"]] <- deficit_term_sent[[i, "deficit_dove"]]
  most_hawkish[[i, "deficit_hawk"]] <- deficit_term_sent[[i, "deficit_hawk"]]
  
  most_hawkish[[i, "inflation_term"]] <- str_c(inflation_term[[i]], collapse = " ")
  most_hawkish[[i, "inflation_hawk"]] <- inflation_term_sent[[i, "inflation_hawk"]]
  most_hawkish[[i, "inflation_dove"]] <- inflation_term_sent[[i, "inflation_dove"]]
  
  most_hawkish[[i, "unemp_term"]] <- str_c(unemp_term[[i]], collapse = " ")
  most_hawkish[[i, "unemp_hawk"]] <- unemp_term_sent[[i, "unemp_hawk"]]
  most_hawkish[[i, "unemp_dove"]] <- unemp_term_sent[[i, "unemp_dove"]]
  
  most_hawkish[[i, "interest_term"]] <- str_c(interest_term[[i]], collapse = " ")
  # most_hawkish[[i, "interest_hawk"]] <- interest_term_sent[[i, "hawk_normal"]]
  # most_hawkish[[i, "interest_dove"]] <- interest_term_sent[[i, "dove_normal"]]
  
  
}


write_csv(most_hawkish, "temp_dev/most_hawkish_2018-06-19.csv")
