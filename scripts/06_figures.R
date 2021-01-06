# Reproduction code for the tables and figures 

library(readr)
library(lubridate)
library(dplyr)
library(tidyr)
library(stringr)
library(quanteda)
library(xtable)
library(ggplot2)
library(patchwork)



# FIG 1 ----

macro_data <- read_csv("data/output/dataset_complete_v2.csv", na = ".")


i_base <- first(macro_data$i)


yield_base <- first(macro_data$total_resc)


# indexing the interest rate and yield score
yield_data <- macro_data %>% 
  mutate(i_index = (i / i_base) * 100,
         yield_index = (total_resc / yield_base) * 100) %>% 
  select(date_text, i_index, yield_index) %>% 
  gather("i_index":"yield_index", key = "index_type", value = "index_value")


ggplot(yield_data, aes(date_text, index_value, linetype = index_type)) +
  geom_line(size = 0.7) +
  scale_x_date(date_breaks = "1 year", date_labels = "'%y", limits = c(as.Date("2005-01-24"), as.Date("2018-12-18"))) +
  scale_linetype_manual(values=c("solid", "dashed"), 
                        name="",
                        labels=c("Interest rate", "Yield score")) +
  labs(y = "Index, 2005/01/01 = 100",
       x = "") +
  theme_classic() +
  theme(legend.position = "bottom")


# export (both png and pdf)
ggsave("figures/FIG1_yield_score.tiff", device = "tiff", dpi = "retina")
ggsave("figures/FIG1_yield_score.pdf")





# FIG 2 ------
# sentiment time series
macro_data <- read_csv("data/output/dataset_complete_v2.csv", na = ".")

sentiment_ts <- macro_data %>% 
  select(date_text, nethawkish)

ggplot(sentiment_ts, aes(date_text, nethawkish)) +
  geom_line() + 
  geom_vline(xintercept = as.Date("2012-05-29"), linetype = "dashed") +
  labs(x = "",
       y = "Net hawkish sentiment") +
  scale_x_date(date_breaks = "1 year", date_labels = "'%y", limits = c(as.Date("2005-01-24"), as.Date("2018-12-18"))) +
  theme_classic()


# export (both png and pdf)
ggsave("figures/FIG2_sentiment_ts.tiff", device = "tiff", dpi = "retina")
ggsave("figures/FIG2_sentiment_ts.pdf")





# FIG 3 -------
# shock sim ----------
shock_matrix <- read_rds("temp_dev/shock_matrix.rds") %>% 
  as_tibble()


figa <- ggplot(shock_matrix, aes(Period, intra_year_hawkish_central)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = intra_year_hawkish_ll95, ymax = intra_year_hawkish_ul95), alpha = 0.2, linetype = "dashed", color = "black", size = 1) +
  labs(title = "A",
       y = "Net hawkish sentiment") +
  theme_classic()


figb <- ggplot(shock_matrix, aes(Period, intra_year_i_central)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = intra_year_i_ll95, ymax = intra_year_i_ul95), alpha = 0.2, linetype = "dashed", color = "black", size = 1) +
  labs(title = "B",
       y = "Interest rate") +
  theme_classic()

shock_plots <- figa + figb

ggsave(plot = shock_plots, filename = "figures/FIG3_shock_plot.tiff", dpi = "retina")
ggsave(plot = shock_plots, filename = "figures/FIG3_shock_plot.pdf")


#####################################
# FIG 7 ----
#####################################
s_l_h <- ggplot(shock_matrix, aes(Period, long_term_hawkish_central)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = long_term_hawkish_ll95, ymax = long_term_hawkish_ul95 ), alpha = 0.2, linetype = "dashed", color = "black", size = 1) +
  labs(title = "A (long-term)",
       y = "Net hawkish sentiment") +
  theme_classic()


s_l_i <- ggplot(shock_matrix, aes(Period, long_term_i_central)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = long_term_i_ll95, ymax = long_term_i_ul95), alpha = 0.2, linetype = "dashed", color = "black", size = 1) +
  labs(title = "B (long-term)",
       y = "Interest rate") +
  theme_classic()


fig7 <- s_l_h + s_l_i

ggsave(plot = fig7, filename = "figures/FIG7_shock_long.tiff", dpi = "retina")
ggsave(plot = fig7, filename = "figures/FIG7_shock_long.pdf")


#####################################
# FIG 8 -----
#####################################
s_m_h <- ggplot(shock_matrix, aes(Period, one_to_three_year_hawkish_central)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = one_to_three_year_hawkish_ll95, ymax = one_to_three_year_hawkish_ul95 ), alpha = 0.2, linetype = "dashed", color = "black", size = 1) +
  labs(title = "A (1-3 years)",
       y = "Net hawkish sentiment") +
  theme_classic()


s_m_i <- ggplot(shock_matrix, aes(Period, one_to_three_year_i_central)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = one_to_three_year_i_ll95, ymax = one_to_three_year_i_ul95), alpha = 0.2, linetype = "dashed", color = "black", size = 1) +
  labs(title = "B (1-3 years)",
       y = "Interest rate") +
  theme_classic()


fig8 <- s_m_h + s_m_i

ggsave(plot = fig8, filename = "figures/FIG8_shock_1-3.tiff", dpi = "retina")
ggsave(plot = fig8, filename = "figures/FIG8_shock_1-3.pdf")



# FIG 9 ----
mc_cohesion <- read_csv("data/input/mc_cohesion.csv", na = ".")

# plotting
ggplot(mc_cohesion, aes(date, mt_cohesion)) +
  geom_line(aes(linetype = gov), size = 0.7) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2005-10-24", "2018-12-18"))) +
  scale_linetype_manual(labels=c("Zsigmond Jarai", "Andras Simor", "Gyorgy Matolcsy"), breaks=c("jarai", "simor", "matolcsy"), values=c("dotted", "dashed", "solid")) +
  labs(x = "",
       y = "Cohesion index") +
  theme_classic() +
  theme(legend.title=element_blank(),
        legend.position="bottom")

# export (both png and pdf)
ggsave("figures/FIG9_cohesion_index.tiff", device = "tiff", dpi = "retina")
ggsave("figures/FIG9_cohesion_index.pdf")


# TABLE 1 corpus summary ----
# data
# mnb press release dataframe
pr_clean <- read_csv("data/input/mnb_pr_corpus.csv")


# corpus 
pr_corpus <- corpus(pr_clean)

# corpus summary table
corpus_summary <- as_tibble(summary(pr_corpus, n = 180)) %>% 
  mutate(year = as.integer(year)) %>% 
  group_by(year) %>% 
  summarise(N = n(), `mean wc` = mean(Tokens), `sd wc` = sd(Tokens), `min wc` = min(Tokens), `max wc` = max(Tokens))

# optional .doc output
#print(xtable(corpus_summary), type = "html", file = "figures/summary_table.doc", include.rownames = FALSE)

print(xtable(corpus_summary), type = "latex", include.rownames = FALSE)


write_csv(corpus_summary, "figures/summary_table.csv")








# Table 2 dictionary table
monetary_dict <- read_rds("data/input/monetary_dictionary2.rds")
names(monetary_dict)

macro_term <- names(monetary_dict)

list_items <- c(paste(monetary_dict$terms_normal, collapse = ", "), 
                paste(monetary_dict$hawk_normal, collapse = ", "),
                paste(monetary_dict$dove_normal, collapse = ", "),
                paste(monetary_dict$deficit_term, collapse = ", "),
                paste(monetary_dict$deficit_dove, collapse = ", "),
                paste(monetary_dict$deficit_hawk, collapse = ", "),
                paste(monetary_dict$inflation_term, collapse = ", "),
                paste(monetary_dict$inflation_dove, collapse = ", "),
                paste(monetary_dict$inflation_hawk, collapse = ", "),
                paste(monetary_dict$unemp_term, collapse = ", "),
                paste(monetary_dict$unemp_dove, collapse = ", "),
                paste(monetary_dict$unemp_hawk, collapse = ", "),
                paste(monetary_dict$interest_term, collapse = ", "),
                paste(monetary_dict$interest_dove, collapse = ", "),
                paste(monetary_dict$interest_hawk, collapse = ", "))

monetary_dict_table <- data.frame(
  macro_terms = macro_term,
  list_items = list_items
)

print(xtable(monetary_dict_table), include.rownames = FALSE)

write_csv(monetary_dict_table, "figures/dictionary_table.csv")

# optional .doc output
#print(xtable(monetary_dict_table), type = "html", file = "figures/dictionary_table.doc", include.rownames = FALSE)









