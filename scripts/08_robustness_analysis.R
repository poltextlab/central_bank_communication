# the Newey-West OLS used in the appendix robustness check

library(dplyr)
library(stringr)
library(sandwich)
library(lmtest)
library(haven)
library(lubridate)
library(texreg)


# dataframe

df_mnb <- readr::read_csv("data/output/dataset_complete_v2.csv", na = ".") %>% 
  mutate(d_log_intra_year = log_intra_year - lag(log_intra_year, n = 1),
         d_log_long_term = log_long_term - lag(log_long_term, n = 1),
         d_log_1to3year = log_1to3year - lag(log_1to3year, n = 1),
         d_log_nethawkish = log_nethawkish - lag(log_nethawkish, n = 1),
         d_u = u - lag(u, n = 1),
         d_core_i_yoy = core_i_yoy - lag(core_i_yoy, n = 1),
         d_usd = USD - lag(USD, n = 1),
         d_eur = EUR - lag(EUR, n = 1),
         d_i = i - lag(i, n = 1),
         d_fed_i = effective_fed_i - lag(effective_fed_i, n = 1),
         d_monthly_mlf = monthly_mlf - lag(monthly_mlf, n = 1),
         d_fed_bs = fed_bs - lag(fed_bs, n = 1),
         d_ecb_bs = ecb_bs - lag(ecb_bs, n = 1)) %>% 
  rename(simor = Simor, matolcsy = Matolcsy)





# sentiment + policy rate ----
# Using all time frame
dvs <- c("d_log_intra_year", "d_log_long_term", "d_log_1to3year")



# lhs
lhs_pr_2 <- c(
  "log_nethawkish + d_u + m3 + d_core_i_yoy + d_monthly_mlf + d_fed_i + simor + matolcsy + mt_cohesion", 
  "log_nethawkish + d_i + d_u + m3 + d_core_i_yoy + d_monthly_mlf + d_fed_i + simor + matolcsy + mt_cohesion")



# building the list of regressions
models_list_2 <- NULL

for (i in 1:length(dvs)) {
  for (j in 1:length(lhs_pr_2)) {
    models_l_2 <- paste(dvs[i], lhs_pr_2[j], sep = " ~ ")
    
    models_list_2 <- rbind(models_list_2, models_l_2)
  }
}


models_2 <- as.data.frame(models_list_2)


# running the 6 OLS regression and storing them in a list
ols_estimates_2 <- lapply(models_2$V1, function(x) lm(as.formula(x), data = df_mnb))

# naming the list elements
ols_names_2 <- str_c("dv", rep(dvs, each = length(lhs_pr_2)), sep = "_")

names(ols_estimates_2) <- ols_names_2


# having the summaries of the models
ols_summary_2 <- lapply(ols_estimates_2, summary)


# performing the Newey West standard error correction 
for (i in 1:length(ols_summary_2)) {
  ols_summary_2[[i]]$coefficients <- unclass(coeftest(ols_estimates_2[[i]], vcov = NeweyWest(ols_estimates_2[[i]], lag = 12)))
}

# renaming
names(ols_summary_2) <- ols_names_2


# EXPORT OF TABLE WITH POLICY RATE ONLY
nw_correction_2 <- lapply(ols_estimates_2, function(x) coeftest(x, vcov = NeweyWest(x, lag = 12)))

# for the export we need to separate the standard errors
nw_se_2 <- vector(mode = "list", length = length(nw_correction_2))

for (i in 1:length(nw_correction_2)) {
  nw_se_2[[i]] <- nw_correction_2[[i]][, 2]
}


# do the same separation for the new p-values as well
nw_pval_2 <- vector(mode = "list", length = length(nw_correction_2))

for (i in 1:length(nw_correction_2)) {
  nw_pval_2[[i]] <- nw_correction_2[[i]][, 4]
}


# the renaming thing
custom_coefs_2 <- list("log_nethawkish" = "ln(Nethawkish)",
                       "d_i" = "Policy rate",
                       "d_u" = "Unemployment rate",
                       "m3" = "M3", 
                       "d_core_i_yoy" = "Core inflation",
                       "d_monthly_mlf" = "ecb rate",
                       "d_fed_i" = "fed rate",
                       "simor" = "Simor",
                       "matolcsy" = "Matolcsy",
                       "mt_cohesion" = "Council cohesion")

note <- "*** $p < 0.001$, ** $p < 0.01$, * $p < 0.05$, standard errors in parenthesis"


screenreg(ols_estimates_2[1:6], override.se = list(nw_se_2[[1]], nw_se_2[[2]], nw_se_2[[3]], nw_se_2[[4]], nw_se_2[[5]], nw_se_2[[6]]), override.pvalues = list(nw_pval_2[[1]], nw_pval_2[[2]], nw_pval_2[[3]], nw_pval_2[[4]], nw_pval_2[[5]], nw_pval_2[[6]]), custom.coef.map = custom_coefs_2, digits = 3, custom.note = note, doctype = FALSE, caption = "")

texreg(ols_estimates_2[1:6], override.se = list(nw_se_2[[1]], nw_se_2[[2]], nw_se_2[[3]], nw_se_2[[4]], nw_se_2[[5]], nw_se_2[[6]]), override.pvalues = list(nw_pval_2[[1]], nw_pval_2[[2]], nw_pval_2[[3]], nw_pval_2[[4]], nw_pval_2[[5]], nw_pval_2[[6]]), custom.coef.map = custom_coefs_2, digits = 3, custom.note = note, doctype = FALSE, caption = "")

