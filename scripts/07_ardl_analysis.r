library(ARDL)
library(zoo) #  cbind.zoo
library(xts) #  xts
library(readxl)
library(urca)
library(stR) # decompose 
library(stats)
library(ggplot2)
library(strucchange)
library(tidyverse)
library(tsoutliers)
library(car) # QQ plot
library(dynamac)

################################################################################
# TABLE 3 --- ADF AND PP TESTS --- PANEL A
################################################################################


time_series_data <- read.csv("data/dataset_complete_v2.csv")
time_series_data <- time_series_data[, c("date", "log_intra_year", "log_long_term", "log_1to3year",
                                         "i", "nethawkish", "log_nethawkish")]
time_series_data <- time_series_data %>%
  mutate(intra_year = exp(log_intra_year),
         long_term = exp(log_long_term),
         one_to_three_year = exp(log_1to3year))

# Outliers
series_to_adjust <- time_series_data$nethawkish
out <- tso(as.ts(series_to_adjust), types = "AO") # AO - additive outlier
h_adj <- out$yadj

time_series_data$nethawkish <- h_adj

#######################################
# THE SELECTED VARIABLE CAN BE SET HERE
#######################################

time_series <- time_series_data$intra_year
# plot(time_series, type = "l")
## ADF
### Level
summary(ur.df(time_series, type = c("drift"), lags=12, selectlags = "AIC"))
plot(ur.df(time_series, type = c("drift"), lags=12, selectlags = "AIC"))

### Difference    
summary(ur.df(diff(time_series), type = c("drift"), lags=12, selectlags = "AIC"))
plot(ur.df(diff(time_series), type = c("drift"), lags=12, selectlags = "AIC"))


## Phillips - Perron teszt
### Level
summary(ur.pp(time_series, type = c("Z-tau"), model = c("constant")))
plot(ur.pp(time_series, type = c("Z-tau"), model = c("constant")))

## Difference 
summary(ur.pp(diff(time_series), type = c("Z-tau"), model = c("constant")))
plot(ur.pp(diff(time_series, type = c("Z-tau"), model = c("constant"))))




################################################################################
# TABLE 3 --- ADF AND PP TESTS --- PANEL B
################################################################################

time_series_data <- read.csv("data/dataset_complete_v2.csv")
time_series_data <- time_series_data[, c("date", "log_intra_year", "log_long_term", "log_1to3year",
                                         "i", "nethawkish", "log_nethawkish")]
time_series_data <- time_series_data %>%
  mutate(intra_year = exp(log_intra_year),
         long_term = exp(log_long_term),
         one_to_three_year = exp(log_1to3year))

# Outliers
series_to_adjust <- time_series_data$nethawkish
out <- tso(as.ts(series_to_adjust), types = "AO") # AO - additive outlier
h_adj <- out$yadj

time_series_data$nethawkish <- h_adj


time_series_data <- time_series_data[1:89, ]     

#######################################
# THE SELECTED VARIABLE CAN BE SET HERE
#######################################

time_series <- time_series_data$intra_year
# plot(time_series, type = "l")
## ADF
### Level
summary(ur.df(time_series, type = c("drift"), lags=12, selectlags = "AIC"))
plot(ur.df(time_series, type = c("drift"), lags=12, selectlags = "AIC"))

### Difference    
summary(ur.df(diff(time_series), type = c("drift"), lags=12, selectlags = "AIC"))
plot(ur.df(diff(time_series), type = c("drift"), lags=12, selectlags = "AIC"))


## Phillips - Perron teszt
### Level
summary(ur.pp(time_series, type = c("Z-tau"), model = c("constant")))
plot(ur.pp(time_series, type = c("Z-tau"), model = c("constant")))

## Difference 
summary(ur.pp(diff(time_series), type = c("Z-tau"), model = c("constant")))
plot(ur.pp(diff(time_series, type = c("Z-tau"), model = c("constant"))))


################################################################################
# TABLE 3 --- ADF AND PP TESTS --- PANEL C
################################################################################


time_series_data <- read.csv("data/dataset_complete_v2.csv")
time_series_data <- time_series_data[, c("date", "log_intra_year", "log_long_term", "log_1to3year",
                                         "i", "nethawkish", "log_nethawkish")]
time_series_data <- time_series_data %>%
  mutate(intra_year = exp(log_intra_year),
         long_term = exp(log_long_term),
         one_to_three_year = exp(log_1to3year))

# Outliers
series_to_adjust <- time_series_data$nethawkish
out <- tso(as.ts(series_to_adjust), types = "AO") # AO - additive outlier

h_adj <- out$yadj

time_series_data$nethawkish <- h_adj

time_series_data <- time_series_data[90:168, ]     

#######################################
# THE SELECTED VARIABLE CAN BE SET HERE
#######################################

time_series <- time_series_data$intra_year
# plot(time_series, type = "l")
## ADF
### Level
summary(ur.df(time_series, type = c("trend"), lags=12, selectlags = "AIC"))
plot(ur.df(time_series, type = c("trend"), lags=12, selectlags = "AIC"))

### Difference    
summary(ur.df(diff(time_series), type = c("trend"), lags=12, selectlags = "AIC"))
plot(ur.df(diff(time_series), type = c("trend"), lags=12, selectlags = "AIC"))


## Phillips - Perron teszt
### Level
summary(ur.pp(time_series, type = c("Z-tau"), model = c("trend")))
plot(ur.pp(time_series, type = c("Z-tau"), model = c("trend")))

## Difference 
summary(ur.pp(diff(time_series), type = c("Z-tau"), model = c("trend")))
plot(ur.pp(diff(time_series, type = c("Z-tau"), model = c("trend"))))


################################################################################
# TABLE 4 PREPARATIONS
################################################################################
time_series_data <- read.csv("data/dataset_complete_v2.csv")
time_series_data <- time_series_data[, c("date", "log_intra_year", "log_long_term", "log_1to3year",
                                         "i", "nethawkish", "log_nethawkish")]
time_series_data <- time_series_data %>%
  mutate(intra_year = exp(log_intra_year),
         long_term = exp(log_long_term),
         one_to_three_year = exp(log_1to3year))

# Outliers
series_to_adjust <- time_series_data$nethawkish
out <- tso(as.ts(series_to_adjust), types = "AO") # AO - additive outlier

h_adj <- out$yadj

time_series_data$nethawkish <- h_adj

################################################################################
# TABLE 4 --- MODEL 1 --- LONG_TERM NETHAWKISH full time series
################################################################################

# nethawkish  i 
## ARDL cointegration
### Order selection
ARDL_package_models <- auto_ardl(long_term ~ i  nethawkish, 
                                 data = time_series_data, max_order = 12, selection = "AIC")
# model selection
ARDL_package_models$top_orders
# best model
best_ARDL <- ARDL_package_models$best_model


# best_ARDL <- ardl(long_term ~ i  nethawkish , data = time_series_data, order = c(1,1,1))

# output statistics
summary(best_ARDL)

## UNRESTRICTED ARDL MODELL
uecm <- uecm(best_ARDL, case = 3)
summary(uecm)

## RESTRICTED ARDL  MODELL
recm<- recm(best_ARDL, case = 3)
summary(recm)


    #####################################
    # TABLE 5 --- COLUMN 1/9
    #####################################

# Bounds tests - F-stat 
# 1%
tbounds <- bounds_f_test(uecm, case = 3, alpha = 0.01)
tbounds$tab
# 5%
tbounds <- bounds_f_test(uecm, case = 3, alpha = 0.05)
tbounds$tab
# 10%
tbounds <- bounds_f_test(uecm, case = 3, alpha = 0.1)
tbounds$tab

################################################################################
# 
summary(uecm)

# seed
set.seed(020990)
dynamac_model <- dynardl(long_term ~ i  nethawkish, data = time_series_data,
                         lags = list("long_term" = 1, "i" = 1, "nethawkish" = 1),
                         diffs = c("i", "nethawkish"),
                         lagdiffs = list("i" = c(1:2)),
                         ec = TRUE, simulate = TRUE,
                         shockvar = "i", range = 20, sims = 10000)
summary(dynamac_model)

# 
dynardl.auto.correlated(dynamac_model)

# QQ plot
qqPlot(dynamac_model$model$residuals, ylab="Model residuals")


# shock simulation
# with 95/90/75 ci
dynardl.simulation.plot(dynamac_model, type = "area", response = "levels", bw = TRUE, start.period = 5)

# central values
dynamac_model$simulation$central

################################################################################
# TABLE 4 --- MODEL 2 --- LONG_TERM NETHAWKISH  i 1-89
################################################################################
      # Els� id�szak
      time_series_data <- time_series_data[1:89, ]
      # 
      ## ARDL cointegration
      ### Order selection
      ARDL_package_models <- auto_ardl(long_term ~ i  nethawkish, 
                                       data = time_series_data, max_order = 12, selection = "AIC")
      # model selection
      ARDL_package_models$top_orders
      # best model
      best_ARDL <- ARDL_package_models$best_model
      
      
      # best_ARDL <- ardl(long_term ~ i  nethawkish , data = time_series_data, order = c(1,1,1))
      
      # output statistics
      summary(best_ARDL)
      
      ## UNRESTRICTED ARDL MODELL
      uecm <- uecm(best_ARDL, case = 3)
      summary(uecm)
      
      ## RESTRICTED ARDL  MODELL
      recm<- recm(best_ARDL, case = 3)
      summary(recm)
      
            #####################################
            # TABLE 5 --- COLUMN 2/9
            #####################################
      
      # Bounds tests - F-stat 
      # 1%
      tbounds <- bounds_f_test(uecm, case = 3, alpha = 0.01)
      tbounds$tab
      # 5%
      tbounds <- bounds_f_test(uecm, case = 3, alpha = 0.05)
      tbounds$tab
      # 10%
      tbounds <- bounds_f_test(uecm, case = 3, alpha = 0.1)
      tbounds$tab
      
      ################################################################################
      # 
      summary(uecm)
      
      # seed
      set.seed(020990)
      dynamac_model <- dynardl(long_term ~ i  nethawkish, data = time_series_data,
                               lags = list("long_term" = 1, "i" = 1, "nethawkish" = 1),
                               diffs = c("i", "nethawkish"),
                               # lagdiffs = list("i" = c(1:2)),
                               ec = TRUE, simulate = TRUE,
                               shockvar = "i", range = 20, sims = 10000)
      summary(dynamac_model)
      
      # 
      dynardl.auto.correlated(dynamac_model)
      
      # QQ plot
      qqPlot(dynamac_model$model$residuals, ylab="Model residuals")
      
      
      # shock simulation
      # with 95/90/75 ci
      dynardl.simulation.plot(dynamac_model, type = "area", response = "levels", bw = TRUE, start.period = 5)
      
      # central values
      dynamac_model$simulation$central

################################################################################
# TABLE 4 --- MODEL 3 --- LONG_TERM NETHAWKISH  i 90-168
################################################################################

time_series_data <- read.csv("data/dataset_complete_v2.csv")
time_series_data <- time_series_data[, c("date", "log_intra_year", "log_long_term", "log_1to3year",
                                               "i", "nethawkish", "log_nethawkish")]
time_series_data <- time_series_data %>%
        mutate(intra_year = exp(log_intra_year),
               long_term = exp(log_long_term),
               one_to_three_year = exp(log_1to3year))
      
# outliers
series_to_adjust <- time_series_data$nethawkish
out <- tso(as.ts(series_to_adjust), types = "AO") # AO - additive outlier
# AO kiugr� �rt�kre sz�rt
h_adj <- out$yadj

# ts.plot(cbind(series_to_adjust, h_adj), col = c("blue", "red"))

time_series_data$nethawkish <- h_adj

# M�sodik id�szak
time_series_data <- time_series_data[90:168, ]

# 
## ARDL cointegration
### Order selection
ARDL_package_models <- auto_ardl(long_term ~ i  nethawkish, 
                                 data = time_series_data, max_order = 12, selection = "AIC")
# model selection
ARDL_package_models$top_orders


best_ARDL <- ardl(long_term ~ i  nethawkish , data = time_series_data, order = c(1,1,1))

# output statistics
summary(best_ARDL)

## UNRESTRICTED ARDL MODELL
uecm <- uecm(best_ARDL, case = 3)
summary(uecm)

## RESTRICTED ARDL  MODELL
recm<- recm(best_ARDL, case = 3)
summary(recm)


      #####################################
      # TABLE 5 --- COLUMN 3/9
      #####################################

# Bounds tests - F-stat 
# 1%
tbounds <- bounds_f_test(uecm, case = 3, alpha = 0.01)
tbounds$tab
# 5%
tbounds <- bounds_f_test(uecm, case = 3, alpha = 0.05)
tbounds$tab
# 10%
tbounds <- bounds_f_test(uecm, case = 3, alpha = 0.1)
tbounds$tab

################################################################################
# 
summary(uecm)

## Interest rate shock responses
# seed
set.seed(020990)
## Interest rate shock responses
dynamac_model <- dynardl(long_term ~ i  nethawkish, data = time_series_data,
                         lags = list("long_term" = 1, "i" = 1, "nethawkish" = 1),
                         diffs = c("i", "nethawkish"),
                         # lagdiffs = list("i" = c(1:2)),
                         ec = TRUE, simulate = TRUE,
                         shockvar = "i", range = 30, sims = 10000)
summary(dynamac_model)

# 
dynardl.auto.correlated(dynamac_model)

##################################
# FIG 6 --- QQ plot --- LONG TERM
##################################
qqPlot(dynamac_model$model$residuals, ylab="Model residuals")


# shock simulation
# with 95/90/75 ci
dynardl.simulation.plot(dynamac_model, type = "area", response = "levels", bw = TRUE, start.period = 5,
                        xlab = "Periods", ylab = "Long_term yield values")

# central values
dynamac_model$simulation$central


## Nethawkish shock responses
dynamac_model <- dynardl(long_term ~ i  nethawkish, data = time_series_data,
                         lags = list("long_term" = 1, "i" = 1, "nethawkish" = 1),
                         diffs = c("i", "nethawkish"),
                         # lagdiffs = list("i" = c(1:2)),
                         ec = TRUE, simulate = TRUE,
                         shockvar = "nethawkish", range = 30, sims = 10000)

dynardl.simulation.plot(dynamac_model, type = "area", response = "levels", bw = TRUE, start.period = 5,
                        xlab = "Periods", ylab = "Long_term yield values")

# central values
dynamac_model$simulation$central
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

      
      time_series_data <- read.csv("data/dataset_complete_v2.csv")
      time_series_data <- time_series_data[, c("date", "log_intra_year", "log_long_term", "log_1to3year",
                                               "i", "nethawkish", "log_nethawkish")]
      time_series_data <- time_series_data %>%
        mutate(intra_year = exp(log_intra_year),
               long_term = exp(log_long_term),
               one_to_three_year = exp(log_1to3year))
      
      # outliers
      series_to_adjust <- time_series_data$nethawkish
      out <- tso(as.ts(series_to_adjust), types = "AO") # AO - additive outlier
     
      h_adj <- out$yadj
      
      # ts.plot(cbind(series_to_adjust, h_adj), col = c("blue", "red"))
      
      time_series_data$nethawkish <- h_adj
      
################################################################################
# TABLE 4 --- MODEL 4 --- 1to3_year nethawkish  i, full time time series
################################################################################
      
      # 
      ## ARDL cointegration
      ### Order selection
      ARDL_package_models <- auto_ardl(one_to_three_year ~ i  nethawkish, 
                                       data = time_series_data, max_order = 12, selection = "AIC")
      # model selection
      ARDL_package_models$top_orders
      # best model
      best_ARDL <- ARDL_package_models$best_model
      
      
      # best_ARDL <- ardl(one_to_three_year ~ i  nethawkish , data = time_series_data, order = c(1,1,1))
      
      # output statistics
      summary(best_ARDL)
      
      ## UNRESTRICTED ARDL MODELL
      uecm <- uecm(best_ARDL, case = 3)
      summary(uecm)
      
      ## RESTRICTED ARDL  MODELL
      recm<- recm(best_ARDL, case = 3)
      summary(recm)
      
          #####################################
          # TABLE 5 --- COLUMN 4/9
          #####################################
      
      # Bounds tests - F-stat 
      # 1%
      tbounds <- bounds_f_test(uecm, case = 3, alpha = 0.01)
      tbounds$tab
      # 5%
      tbounds <- bounds_f_test(uecm, case = 3, alpha = 0.05)
      tbounds$tab
      # 10%
      tbounds <- bounds_f_test(uecm, case = 3, alpha = 0.1)
      tbounds$tab
      
      ################################################################################
      # 
      summary(uecm)
      
      # seed
      set.seed(020990)
      dynamac_model <- dynardl(one_to_three_year ~ i  nethawkish, data = time_series_data,
                               lags = list("one_to_three_year" = 1, "nethawkish" = 1, "i" = 1),
                               diffs = c("nethawkish", "i"),
                               lagdiffs = list("one_to_three_year" = c(1:3), "nethawkish" = c(1:3), "i" = c(1:4)),
                               ec = TRUE, simulate = TRUE,
                               shockvar = "i", range = 20, sims = 10000)
      summary(dynamac_model)
      
      # 
      dynardl.auto.correlated(dynamac_model)
      
      # QQ plot
      qqPlot(dynamac_model$model$residuals, ylab="Model residuals")
      
      
      # shock simulation
      # with 95/90/75 ci
      dynardl.simulation.plot(dynamac_model, type = "area", response = "levels", bw = TRUE, start.period = 5)
      
      # central values
      dynamac_model$simulation$central


################################################################################
# TABLE 4 --- MODEL 5 --- 1to3_year NETHAWKISH  i 1-89
################################################################################
      # Els� id�szak
      time_series_data <- time_series_data[1:89, ]
      
      # 
      ## ARDL cointegration
      ### Order selection
      ARDL_package_models <- auto_ardl(one_to_three_year ~ i  nethawkish, 
                                       data = time_series_data, max_order = 12, selection = "AIC")
      # model selection
      ARDL_package_models$top_orders
      
      # best model
      best_ARDL <- ARDL_package_models$best_model
      
      # output statistics
      summary(best_ARDL)
      
      ## UNRESTRICTED ARDL MODELL
      uecm <- uecm(best_ARDL, case = 3)
      summary(uecm)
      
      ## RESTRICTED ARDL  MODELL
      recm<- recm(best_ARDL, case = 3)
      summary(recm)
      
            #####################################
            # TABLE 5 --- COLUMN 5/9
            #####################################
      
      # Bounds tests - F-stat 
      # 1%
      tbounds <- bounds_f_test(uecm, case = 3, alpha = 0.01)
      tbounds$tab
      # 5%
      tbounds <- bounds_f_test(uecm, case = 3, alpha = 0.05)
      tbounds$tab
      # 10%
      tbounds <- bounds_f_test(uecm, case = 3, alpha = 0.1)
      tbounds$tab
      
      ################################################################################
      # 
      summary(uecm)
      
      ## Interest rate shock responses
      # seed
      set.seed(020990)
      ## Interest rate shock responses
      # one_to_three_year 1-89
      dynamac_model <- dynardl(one_to_three_year ~ i  nethawkish, data = time_series_data,
                               lags = list("one_to_three_year" = 1, "nethawkish" = 1, "i" = 1),
                               diffs = c("nethawkish", "i"),
                               lagdiffs = list("one_to_three_year" = c(1:2), "nethawkish" = c(1), "i" = c(1)),
                               ec = TRUE, simulate = TRUE,
                               shockvar = "i", range = 20, sims = 10000)
      summary(dynamac_model)
      
      # 
      dynardl.auto.correlated(dynamac_model)
      
      # QQ plot
      qqPlot(dynamac_model$model$residuals, ylab="Model residuals")
      
      
      # shock simulation
      # with 95/90/75 ci
      dynardl.simulation.plot(dynamac_model, type = "area", response = "levels", bw = TRUE, start.period = 5,
                              xlab = "Periods", ylab = "Long_term yield values")
      
      # central values
      dynamac_model$simulation$central
      
      
      ## Nethawkish shock responses
      dynamac_model <- dynardl(long_term ~ i  nethawkish, data = time_series_data,
                               lags = list("long_term" = 1, "i" = 1, "nethawkish" = 1),
                               diffs = c("i", "nethawkish"),
                               # lagdiffs = list("i" = c(1:2)),
                               ec = TRUE, simulate = TRUE,
                               shockvar = "nethawkish", range = 30, sims = 10000)
      
      dynardl.simulation.plot(dynamac_model, type = "area", response = "levels", bw = TRUE, start.period = 5,
                              xlab = "Periods", ylab = "Long_term yield values")
      
      # central values
      dynamac_model$simulation$central

################################################################################
# TABLE 4 --- MODEL 6 --- 1to3_year NETHAWKISH  i 90-168
################################################################################
      
      time_series_data <- read.csv("data/dataset_complete_v2.csv")
      time_series_data <- time_series_data[, c("date", "log_intra_year", "log_long_term", "log_1to3year",
                                               "i", "nethawkish", "log_nethawkish")]
      time_series_data <- time_series_data %>%
        mutate(intra_year = exp(log_intra_year),
               long_term = exp(log_long_term),
               one_to_three_year = exp(log_1to3year))
      
      # outliers
      series_to_adjust <- time_series_data$nethawkish
      out <- tso(as.ts(series_to_adjust), types = "AO") # AO - additive outlier
     
      h_adj <- out$yadj
      
      # ts.plot(cbind(series_to_adjust, h_adj), col = c("blue", "red"))
      
      time_series_data$nethawkish <- h_adj
      
      
      
      # M�sodik id�szak
      time_series_data <- time_series_data[90:168, ]
      
      # 
      ## ARDL cointegration
      ### Order selection
      ARDL_package_models <- auto_ardl(one_to_three_year ~ i  nethawkish, 
                                       data = time_series_data, max_order = 12, selection = "AIC")
      # model selection
      ARDL_package_models$top_orders
      
      # best model
      best_ARDL <- ardl(one_to_three_year ~ i  nethawkish , data = time_series_data, order = c(1,1,1))
      
      # output statistics
      summary(best_ARDL)
      
      ## UNRESTRICTED ARDL MODELL
      uecm <- uecm(best_ARDL, case = 3)
      summary(uecm)
      
      ## RESTRICTED ARDL  MODELL
      recm<- recm(best_ARDL, case = 3)
      summary(recm)
      
      
          #####################################
          # TABLE 5 --- COLUMN 6/9
          #####################################
      
      
      
      # Bounds tests - F-stat 
      # 1%
      tbounds <- bounds_f_test(uecm, case = 3, alpha = 0.01)
      tbounds$tab
      # 5%
      tbounds <- bounds_f_test(uecm, case = 3, alpha = 0.05)
      tbounds$tab
      # 10%
      tbounds <- bounds_f_test(uecm, case = 3, alpha = 0.1)
      tbounds$tab
      
      ################################################################################
      # 
      summary(uecm)
      
      ## Interest rate shock responses
      # seed
      set.seed(020990)
      ## Interest rate shock responses
      # one_to_three_year 1-89
      dynamac_model <- dynardl(one_to_three_year ~ i  nethawkish, data = time_series_data,
                               lags = list("one_to_three_year" = 1, "nethawkish" = 1, "i" = 1),
                               diffs = c("nethawkish", "i"),
                               #lagdiffs = list("one_to_three_year" = c(1:2), "nethawkish" = c(1), "i" = c(1)),
                               ec = TRUE, simulate = TRUE,
                               shockvar = "i", range = 40, sims = 10000)
      summary(dynamac_model)
      
      # 
      dynardl.auto.correlated(dynamac_model)
      
      ##################################
      # FIG 5 --- QQ plot --- 1 TO 3 YEAR
      ##################################
      qqPlot(dynamac_model$model$residuals, ylab="Model residuals")
      
      
      # shock simulation - 
      # with 95/90/75 ci
      dynardl.simulation.plot(dynamac_model, type = "area", response = "levels", bw = TRUE, start.period = 5,
                              xlab = "Period", ylab = "1to3year yield values")
      
      # central values
      dynamac_model$simulation$central
      
      
      ## Nethawkish shock responses
      dynamac_model <- dynardl(long_term ~ i  nethawkish, data = time_series_data,
                               lags = list("long_term" = 1, "i" = 1, "nethawkish" = 1),
                               diffs = c("i", "nethawkish"),
                               # lagdiffs = list("i" = c(1:2)),
                               ec = TRUE, simulate = TRUE,
                               shockvar = "nethawkish", range = 30, sims = 10000)
      
      dynardl.simulation.plot(dynamac_model, type = "area", response = "levels", bw = TRUE, start.period = 5,
                              xlab = "Period", ylab = "1to3year yield values")
      
      # central values
      dynamac_model$simulation$central
      
      
      
################################################################################
# TABLE 4 --- MODEL 7 --- INTRA_YEAR nethawkish  i, full time time series
################################################################################      
      
      
      
      time_series_data <- read.csv("data/dataset_complete_v2.csv")
      time_series_data <- time_series_data[, c("date", "log_intra_year", "log_long_term", "log_1to3year",
                                               "i", "nethawkish", "log_nethawkish")]
      time_series_data <- time_series_data %>%
        mutate(intra_year = exp(log_intra_year),
               long_term = exp(log_long_term),
               one_to_three_year = exp(log_1to3year))
      
      # outliers
      series_to_adjust <- time_series_data$nethawkish
      out <- tso(as.ts(series_to_adjust), types = "AO") # AO - additive outlier
     
      h_adj <- out$yadj
      
      # ts.plot(cbind(series_to_adjust, h_adj), col = c("blue", "red"))
      
      time_series_data$nethawkish <- h_adj
      

      
      # 
      ## ARDL cointegration
      ### Order selection
      ARDL_package_models <- auto_ardl(intra_year ~ i  nethawkish, 
                                       data = time_series_data, max_order = 12, selection = "AIC")
      # model selection
      ARDL_package_models$top_orders
      # best model
      best_ARDL <- ARDL_package_models$best_model
      
      
      # best_ARDL <- ardl(long_term ~ i  nethawkish , data = time_series_data, order = c(1,1,1))
      
      # output statistics
      summary(best_ARDL)
      
      ## UNRESTRICTED ARDL MODELL
      uecm <- uecm(best_ARDL, case = 3)
      summary(uecm)
      
      ## RESTRICTED ARDL  MODELL
      recm<- recm(best_ARDL, case = 3)
      summary(recm)
      
      
          #####################################
          # TABLE 5 --- COLUMN 7/9
          #####################################
      
      
      # Bounds tests - F-stat 
      # 1%
      tbounds <- bounds_f_test(uecm, case = 3, alpha = 0.01)
      tbounds$tab
      # 5%
      tbounds <- bounds_f_test(uecm, case = 3, alpha = 0.05)
      tbounds$tab
      # 10%
      tbounds <- bounds_f_test(uecm, case = 3, alpha = 0.1)
      tbounds$tab
      
      ################################################################################
      # 
      summary(uecm)
      
      # seed
      set.seed(020990)
      dynamac_model <- dynardl(intra_year ~ i  nethawkish, data = time_series_data,
                               lags = list("intra_year" = 1, "i" = 1, "nethawkish" = 1),
                               diffs = c( "i", "nethawkish"),
                               lagdiffs = list("i" = c(1:4), "nethawkish" = c(1:2), "intra_year" = c(1)),
                               ec = TRUE, simulate = TRUE,
                               shockvar = "i", range = 20, sims = 10000)
      summary(dynamac_model)
      
      # 
      dynardl.auto.correlated(dynamac_model)
      
      # QQ plot
      qqPlot(dynamac_model$model$residuals, ylab="Model residuals")
      
      
      # shock simulation
      # with 95/90/75 ci
      dynardl.simulation.plot(dynamac_model, type = "area", response = "levels", bw = TRUE, start.period = 5)
      
      # central values
      dynamac_model$simulation$central      
      
      
      
################################################################################
# TABLE 4 --- MODEL 8 --- INTRA_YEAR NETHAWKISH  i 1-89
################################################################################      
      # Els� id�szak
      time_series_data <- time_series_data[1:89, ] 
      
      # 
      ## ARDL cointegration
      ### Order selection
      ARDL_package_models <- auto_ardl(intra_year ~ i  nethawkish, 
                                       data = time_series_data, max_order = 12, selection = "AIC")
      # model selection
      ARDL_package_models$top_orders
      # best model
      best_ARDL <- ARDL_package_models$best_model
      
      
      # best_ARDL <- ardl(long_term ~ i  nethawkish , data = time_series_data, order = c(1,1,1))
      
      # output statistics
      summary(best_ARDL)
      
      ## UNRESTRICTED ARDL MODELL
      uecm <- uecm(best_ARDL, case = 3)
      summary(uecm)
      
      ## RESTRICTED ARDL  MODELL
      recm<- recm(best_ARDL, case = 3)
      summary(recm)
      
      
          #####################################
          # TABLE 5 --- COLUMN 8/9
          #####################################
      
      # Bounds tests - F-stat 
      # 1%
      tbounds <- bounds_f_test(uecm, case = 3, alpha = 0.01)
      tbounds$tab
      # 5%
      tbounds <- bounds_f_test(uecm, case = 3, alpha = 0.05)
      tbounds$tab
      # 10%
      tbounds <- bounds_f_test(uecm, case = 3, alpha = 0.1)
      tbounds$tab
      
      ################################################################################
      # 
      summary(uecm)
      
      # seed
      set.seed(020990)
      dynamac_model <- dynardl(intra_year ~ i  nethawkish, data = time_series_data,
                               lags = list("intra_year" = 1, "i" = 1, "nethawkish" = 1),
                               diffs = c( "i", "nethawkish"),
                               lagdiffs = list("nethawkish" = c(1), "intra_year" = c(1:2)),
                               ec = TRUE, simulate = TRUE,
                               shockvar = "i", range = 20, sims = 10000)
      summary(dynamac_model)
      
      # 
      dynardl.auto.correlated(dynamac_model)
      
      # QQ plot
      qqPlot(dynamac_model$model$residuals, ylab="Model residuals")
      
      
      # shock simulation
      # with 95/90/75 ci
      dynardl.simulation.plot(dynamac_model, type = "area", response = "levels", bw = TRUE, start.period = 5)
      
      # central values
      dynamac_model$simulation$central      
      
################################################################################
# TABLE 4 --- MODEL 9 --- INTRA_YEAR NETHAWKISH  i 90-168
################################################################################      
      
      time_series_data <- read.csv("data/dataset_complete_v2.csv")
      time_series_data <- time_series_data[, c("date", "log_intra_year", "log_long_term", "log_1to3year",
                                               "i", "nethawkish", "log_nethawkish")]
      time_series_data <- time_series_data %>%
        mutate(intra_year = exp(log_intra_year),
               long_term = exp(log_long_term),
               one_to_three_year = exp(log_1to3year))
      
      # outliers
      series_to_adjust <- time_series_data$nethawkish
      out <- tso(as.ts(series_to_adjust), types = "AO") # AO - additive outlier
     
      h_adj <- out$yadj
      
      # ts.plot(cbind(series_to_adjust, h_adj), col = c("blue", "red"))
      
      time_series_data$nethawkish <- h_adj
      
      
      # Els� id�szak
      time_series_data <- time_series_data[90:168, ] 
      
      # 
      ## ARDL cointegration
      ### Order selection
      ARDL_package_models <- auto_ardl(intra_year ~ i  nethawkish, 
                                       data = time_series_data, max_order = 12, selection = "AIC")
      # model selection
      ARDL_package_models$top_orders

      
      best_ARDL <- ardl(intra_year ~ i  nethawkish , data = time_series_data, order = c(1,1,1))
      
      # output statistics
      summary(best_ARDL)
      
      ## UNRESTRICTED ARDL MODELL
      uecm <- uecm(best_ARDL, case = 3)
      summary(uecm)
      
      ## RESTRICTED ARDL  MODELL
      recm<- recm(best_ARDL, case = 3)
      summary(recm)
      
      
            #####################################
            # TABLE 5 --- COLUMN 9/9
            #####################################
      
      # Bounds tests - F-stat 
      # 1%
      tbounds <- bounds_f_test(uecm, case = 3, alpha = 0.01)
      tbounds$tab
      # 5%
      tbounds <- bounds_f_test(uecm, case = 3, alpha = 0.05)
      tbounds$tab
      # 10%
      tbounds <- bounds_f_test(uecm, case = 3, alpha = 0.1)
      tbounds$tab
      
      ################################################################################
      # 
      summary(uecm)
      
      # seed
      set.seed(020990)
      dynamac_model <- dynardl(intra_year ~ i  nethawkish, data = time_series_data,
                               lags = list("intra_year" = 1, "i" = 1, "nethawkish" = 1),
                               diffs = c( "i", "nethawkish"),
                               ec = TRUE, simulate = TRUE,
                               shockvar = "i", range = 30, sims = 10000)
      summary(dynamac_model)
      
      # 
      dynardl.auto.correlated(dynamac_model)
      
      ##################################
      # FIG 4 --- QQ plot --- INTRA YEAR
      ##################################
      qqPlot(dynamac_model$model$residuals, ylab="Model residuals")
      
      
      #############################
      # FIG 3 --- I SHOCK
      #############################
      
      # shock simulation - 
      # with 95/90/75 ci
      dynardl.simulation.plot(dynamac_model, type = "area", response = "levels", bw = TRUE, start.period = 5,
                              ylab = "Intra_year yield values", xlab = "Period")
      
      # central values
      dynamac_model$simulation$central         
      
      
      # shock simulation - nethawhisk-ra
      dynamac_model <- dynardl(intra_year ~ i  nethawkish, data = time_series_data,
                               lags = list("intra_year" = 1, "i" = 1, "nethawkish" = 1),
                               diffs = c( "i", "nethawkish"),
                               ec = TRUE, simulate = TRUE,
                               shockvar = "nethawkish", range = 30, sims = 10000)
      
      
      #############################
      # FIG 3 --- NETHAWKISH SHOCK
      #############################
      
      
      # with 95/90/75 ci
      dynardl.simulation.plot(dynamac_model, type = "area", response = "levels", bw = TRUE, start.period = 5,
                              ylab = "Intra_year yield values", xlab = "Period")
      
      # central values
      dynamac_model$simulation$central
      
################################################################################
# APPENDIX A --- TABLE 6 --- JOHANSEN TESTS
################################################################################       
      
      time_series_data <- read.csv("data/dataset_complete_v2.csv")
      time_series_data <- time_series_data[, c("date", "log_intra_year", "log_long_term", "log_1to3year",
                                               "i", "nethawkish", "log_nethawkish")]
      time_series_data <- time_series_data %>%
        mutate(intra_year = exp(log_intra_year),
               long_term = exp(log_long_term),
               one_to_three_year = exp(log_1to3year))
      
      # outliers
      series_to_adjust <- time_series_data$nethawkish
      out <- tso(as.ts(series_to_adjust), types = "AO") # AO - additive outlier
     
      h_adj <- out$yadj
      
      # ts.plot(cbind(series_to_adjust, h_adj), col = c("blue", "red"))
      
      time_series_data$nethawkish <- h_adj      
      

      time_series_data <- time_series_data[90:168, ]      
      
# Johansen tests 
      
      jotest <- ca.jo(time_series_data[, c("long_term", "i", "nethawkish")], type="trace", 
                      K=2, ecdet="none", spec="longrun")
      summary(jotest)     
      
      # Long term  i
      jotest <- ca.jo(time_series_data[, c("long_term", "i")], type="trace", 
                      K=2, ecdet="none", spec="longrun")
      summary(jotest) 
      
      
      # Long term  nethawkish
      jotest <- ca.jo(time_series_data[, c("long_term", "nethawkish")], type="trace", 
                      K=2, ecdet="none", spec="longrun")
      summary(jotest)       
      
      ###
      
      jotest <- ca.jo(time_series_data[, c("one_to_three_year", "i", "nethawkish")], type="trace", 
                      K=2, ecdet="none", spec="longrun")
      summary(jotest)     
      
      # one_to_three_year  i
      jotest <- ca.jo(time_series_data[, c("one_to_three_year", "i")], type="trace", 
                      K=2, ecdet="none", spec="longrun")
      summary(jotest) 
      
      
      # one_to_three_year  nethawkish
      jotest <- ca.jo(time_series_data[, c("one_to_three_year", "nethawkish")], type="trace", 
                      K=2, ecdet="none", spec="longrun")
      summary(jotest)           
      
      
      ###
      
      jotest <- ca.jo(time_series_data[, c("intra_year", "i", "nethawkish")], type="trace", 
                      K=2, ecdet="none", spec="longrun")
      summary(jotest)     
      
      # intra_year  i
      jotest <- ca.jo(time_series_data[, c("intra_year", "i")], type="trace", 
                      K=2, ecdet="none", spec="longrun")
      summary(jotest) 
      
      
      # intra_year  nethawkish
      jotest <- ca.jo(time_series_data[, c("intra_year", "nethawkish")], type="trace", 
                      K=2, ecdet="none", spec="longrun")
      summary(jotest)        
      
      
################################################################################
# APPENDIX B --- TABLE 7 --- ROBUSTNESS
################################################################################       

      
      ################################################################################
      
      time_series_data <- read.csv("data/dataset_complete_v2.csv")
      time_series_data <- time_series_data[, c("date", "log_intra_year", "log_long_term", "log_1to3year",
                                               "i", "lm_baseline", "net_hawk_agh")]
      time_series_data <- time_series_data %>%
        mutate(intra_year = exp(log_intra_year),
               long_term = exp(log_long_term),
               one_to_three_year = exp(log_1to3year))
      
      
      # outliers
      series_to_adjust <- time_series_data$lm_baseline
      out <- tso(as.ts(series_to_adjust), types = "AO") # AO - additive outlier
     
      h_adj <- out$yadj
      
      ts.plot(cbind(series_to_adjust, h_adj), col = c("blue", "red"))
      
      time_series_data$lm_baseline <- h_adj
      
      
      # outliers
      series_to_adjust <- time_series_data$net_hawk_agh
      out <- tso(as.ts(series_to_adjust), types = "AO") # AO - additive outlier
     
      h_adj <- out$yadj
      
      ts.plot(cbind(series_to_adjust, h_adj), col = c("blue", "red"))
      
      time_series_data$net_hawk_agh <- h_adj
      
      
      time_series_data <- time_series_data[90:168, ]
      
      
      ################################################################################
      # ADF  PP tests - lm_baseline
      time_series <- time_series_data$lm_baseline
      # plot(time_series, type = "l")
      ## ADF
      ### Level
      summary(ur.df(time_series, type = c("trend"), lags=12, selectlags = "AIC"))
      
      ### Difference    
      summary(ur.df(diff(time_series), type = c("trend"), lags=12, selectlags = "AIC"))
      
      ## Phillips - Perron teszt
      ### Level
      summary(ur.pp(time_series, type = c("Z-tau"), model = c("trend")))
      
      ### Difference 
      summary(ur.pp(diff(time_series), type = c("Z-tau"), model = c("trend")))
      
      
      # ADF + PP tests - net_hawk_agh
      time_series <- time_series_data$net_hawk_agh
      # plot(time_series, type = "l")
      ## ADF
      ### Level
      summary(ur.df(time_series, type = c("trend"), lags=12, selectlags = "AIC"))
      
      ### Difference    
      summary(ur.df(diff(time_series), type = c("trend"), lags=12, selectlags = "AIC"))
      
      ## Phillips - Perron teszt
      ### Level
      summary(ur.pp(time_series, type = c("Z-tau"), model = c("trend")))
      
      ### Difference 
      summary(ur.pp(diff(time_series), type = c("Z-tau"), model = c("trend")))    
      
      ################################################################################    
      ################################################################################
      ################################################################################
      
      ## ARDL cointegration
      ### Order selection
      ARDL_package_models <- auto_ardl(intra_year ~ i  lm_baseline, 
                                       data = time_series_data, max_order = 12, selection = "AIC")
      # model selection
      ARDL_package_models$top_orders
      
      
      best_ARDL <- ardl(intra_year ~ i  lm_baseline , data = time_series_data, order = c(1,1,1))
      
      # output statistics
      summary(best_ARDL)
      
      ## UNRESTRICTED ARDL MODELL
      uecm <- uecm(best_ARDL, case = 3)
      summary(uecm)
      
      ## RESTRICTED ARDL  MODELL
      recm<- recm(best_ARDL, case = 3)
      summary(recm)
      
      
      # Bounds tests - F-stat 
      # 1%
      tbounds <- bounds_f_test(uecm, case = 3, alpha = 0.01)
      tbounds$tab
      # 5%
      tbounds <- bounds_f_test(uecm, case = 3, alpha = 0.05)
      tbounds$tab
      # 10%
      tbounds <- bounds_f_test(uecm, case = 3, alpha = 0.1)
      tbounds$tab
      
      ################################################################################
      # 
      summary(uecm)
      
      # seed
      set.seed(020990)
      dynamac_model <- dynardl(intra_year ~ i  lm_baseline, data = time_series_data,
                               lags = list("intra_year" = 1, "i" = 1, "lm_baseline" = 1),
                               diffs = c( "i", "lm_baseline"),
                               ec = TRUE, simulate = TRUE,
                               shockvar = "i", range = 30, sims = 10000)
      summary(dynamac_model)
      
      # 
      dynardl.auto.correlated(dynamac_model)
      
      # QQ plot
      qqPlot(dynamac_model$model$residuals, ylab="Model residuals")
      
      
      # shock simulation - 
      # with 95/90/75 ci
      dynardl.simulation.plot(dynamac_model, type = "area", response = "levels", bw = TRUE, start.period = 5,
                              ylab = "Intra_year yield values", xlab = "Period")
      
      # central values
      dynamac_model$simulation$central         
      
      
      # shock simulation - nethawhisk-ra
      dynamac_model <- dynardl(intra_year ~ i  lm_baseline, data = time_series_data,
                               lags = list("intra_year" = 1, "i" = 1, "lm_baseline" = 1),
                               diffs = c( "i", "lm_baseline"),
                               ec = TRUE, simulate = TRUE,
                               shockvar = "lm_baseline", range = 30, sims = 10000)
      
      # with 95/90/75 ci
      dynardl.simulation.plot(dynamac_model, type = "area", response = "levels", bw = TRUE, start.period = 5,
                              ylab = "Intra_year yield values", xlab = "Period")
      
      # central values
      dynamac_model$simulation$central
      
      ################################################################################    
      ################################################################################
      ################################################################################
      # Modellez�s az "ARDL package" alapj�n - net_hawk_agh �s i is van benne
      ## ARDL cointegration
      ### Order selection
      ARDL_package_models <- auto_ardl(intra_year ~ i  net_hawk_agh, 
                                       data = time_series_data, max_order = 12, selection = "AIC")
      # model selection
      ARDL_package_models$top_orders
      
      
      best_ARDL <- ardl(intra_year ~ i  net_hawk_agh , data = time_series_data, order = c(1,1,1))
      
      # output statistics
      summary(best_ARDL)
      
      ## UNRESTRICTED ARDL MODELL
      uecm <- uecm(best_ARDL, case = 3)
      summary(uecm)
      
      ## RESTRICTED ARDL  MODELL
      recm<- recm(best_ARDL, case = 3)
      summary(recm)
      
      
      # Bounds tests - F-stat 
      # 1%
      tbounds <- bounds_f_test(uecm, case = 3, alpha = 0.01)
      tbounds$tab
      # 5%
      tbounds <- bounds_f_test(uecm, case = 3, alpha = 0.05)
      tbounds$tab
      # 10%
      tbounds <- bounds_f_test(uecm, case = 3, alpha = 0.1)
      tbounds$tab
      
      ################################################################################
      # 
      summary(uecm)
      
      # seed
      set.seed(020990)
      dynamac_model <- dynardl(intra_year ~ i  net_hawk_agh, data = time_series_data,
                               lags = list("intra_year" = 1, "i" = 1, "net_hawk_agh" = 1),
                               diffs = c( "i", "net_hawk_agh"),
                               ec = TRUE, simulate = TRUE,
                               shockvar = "i", range = 30, sims = 10000)
      summary(dynamac_model)
      
      # 
      dynardl.auto.correlated(dynamac_model)
      
      # QQ plot
      qqPlot(dynamac_model$model$residuals, ylab="Model residuals")
      
      
      # shock simulation - 
      # with 95/90/75 ci
      dynardl.simulation.plot(dynamac_model, type = "area", response = "levels", bw = TRUE, start.period = 5,
                              ylab = "Intra_year yield values", xlab = "Period")
      
      # central values
      dynamac_model$simulation$central         
      
      
      # shock simulation - nethawhisk-ra
      dynamac_model <- dynardl(intra_year ~ i  net_hawk_agh, data = time_series_data,
                               lags = list("intra_year" = 1, "i" = 1, "net_hawk_agh" = 1),
                               diffs = c( "i", "net_hawk_agh"),
                               ec = TRUE, simulate = TRUE,
                               shockvar = "net_hawk_agh", range = 30, sims = 10000)
      
      # with 95/90/75 ci
      dynardl.simulation.plot(dynamac_model, type = "area", response = "levels", bw = TRUE, start.period = 5,
                              ylab = "Intra_year yield values", xlab = "Period")
      
      # central values
      dynamac_model$simulation$central
