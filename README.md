# central_bank_communication
Replication materials for the 'The effect of central bank communication on sovereign bond yields: The case of Hungary' article


- [central_bank_communication](#central_bank_communication)
  - [01_pr_sentiment_analysis.R](#01_pr_sentiment_analysisr)
  - [02_mc_cohesion_index.R](#02_mc_cohesion_indexr)
  - [03_yield_scores.DO](#03_yield_scoresdo)
  - [04_yield_scores_transform.R](#04_yield_scores_transformr)
  - [05_final_dataset_prep.R](#05_final_dataset_prepr)
    - [Variables in the dataset](#variables-in-the-dataset)
  - [06_figures.R](#06_figuresr)
  - [07_ardl_analysis](#07_ardl_analysis)
  - [08_robustness_analysis.R](#08_robustness_analysisr)
  - [09_sentiment_examples.R](#09_sentiment_examplesr)
  - [System information](#system-information)

This repository contains the neccesary source codes to reproduce the analysis in the article. The R source code is in the `scripts` folder, the needed data is in the `data/input` and `data/output` folders. For a detailed description of each script see below.


## 01_pr_sentiment_analysis.R

This script replicates the sentiment scores using the press release text data and the dictionary we created. The text analysis is done in R with the `quanteda` package. The sentiment scores for the Newey-West OLS robustness check using the L-M and AGH dictionaries are also computed in this script. 


## 02_mc_cohesion_index.R


This script replicates the monetary council cohesion index calculation. It uses as input the excel sheet available at the webpage of the Central Bank of Hungary (MNB): https://www.mnb.hu/en/monetary-policy/the-monetary-council/voting-records-of-the-monetary-council-members (last accessed: 2020/05/06).
The scripts contains the step by step instructions for the index reproductions. The input file used for this script is `data/input/mnb-mt-voting.xlsx`.


## 03_yield_scores.DO

The do file creates the `scores.xlsx` from the `data/input/yields.xlsx` file, which contains the PCA scores for the short, medium and long term bond yields.


## 04_yield_scores_transform.R


The R script transforms the PCA scores from the Stata output by rescaling and log transforming the variables.

## 05_final_dataset_prep.R

The output of this script is the final dataset which is used for creating the figures and tables in the article. NB: Missing data is coded as '.' (for Stata compatibility).

This script assembles the parts created above and merges them into the macro controls. The macro controls are in the `data/input/data_macro.csv` file. 


### Variables in the dataset

- **date**: Year-month string
- **i**: Central bank base rate
- **USD**: USD/HUF exchange rate
- **EUR**: EUR/HUF exchange rate
- **u**: Unemployment rate
- **m3**: M3 money stock
- **core_i_yoy**: Core inflation, year on year change
- **pmi**: Purchasing Managers' Index
- **log_total**: logarithm of the rescaled combined yield factor scores (all time horizon)
- **log_in_year**: logarithm of the rescaled r3m r6m yield factor scores
- **log_out_year**: logarithm of the rescaled r1y r3y r5y r10y r15y yield factor scores
- **log_intra_year**: same as log_in_year
- **log_1to3year**: logarithm of the rescaled r1y r3y yield factor scores
- **log_long_term**: logarithm of the rescaled r5y r10y r15y factor scores
- **total_resc**: rescaled combined yield factor scores (all time horizon)
- **in_year_resc**: rescaled r3m r6m yield factor scores
- **out_year_resc**: rescaled r1y r3y r5y r10y r15y yield factor scores
- **short_term_resc**: same as in_year_resc
- **medium_term_resc**: rescaled r1y r3y yield factor scores
- **long_term_resc**: rescaled r5y r10y r15y factor scores
- **r3m**: yields of bonds with maturity rates of 3 months
- **r6m**: yields of bonds with maturity rates of 6 months
- **r1y**: yields of bonds with maturity rates of 1 years
- **r3y**: yields of bonds with maturity rates of 3 years
- **r5y**: yields of bonds with maturity rates of 5 years
- **r10y**: yields of bonds with maturity rates of 10 years
- **r15y**: yields of bonds with maturity rates of 15 years
- **mt_cohesion**: The cohesion index of the monetary council
- **date_text**: The release date of the press release
- **nethawkish**: the Hawkish sentiment score computed with our dictionary on the press release corpus
- **lm_baseline**: sentiment scores with dictionary baseline using the Loughran-McDonald dictionary (net positive) on the the press release corpus
- **net_hawk_agh**: the Hawkish sentiment score computed with the AGH dictionary
- **monthly_mlf**: ECB interest rates
- **fed_i**: US FED interest rates
- **effective_fed_i**: Effective US FED interest rates
- **ecb_bs**: ECB balance sheet 
- **fed_bs**: FED balance sheet
- **Simor**: Central bank governor dummy. 1 for months when Andras Simor was the governor
- **Matolcsy**: Central bank governor dummy. 1 for months when Gyorgy Matolcsy was the governor



## 06_figures.R

This script recreates the Tables 1-2, Figures 1-3; 7-9.


## 07_ardl_analysis


## 08_robustness_analysis.R

The code for the Newey-West OLS robustness check in Appendix C, and produces the results in Table 9.

## 09_sentiment_examples.R

The code to replicate the sentiment calculation highlights in Table 10 in Appendix D.

## System information

**R Session Info**

```
> sessionInfo()
R version 4.0.0 (2020-04-24)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 18363)

Matrix products: default

locale:
[1] LC_COLLATE=English_United States.1252
[2] LC_CTYPE=English_United States.1252
[3] LC_MONETARY=English_United States.1252
[4] LC_NUMERIC=C
[5] LC_TIME=English_United States.1252
system code page: 1250

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base

loaded via a namespace (and not attached):
[1] compiler_4.0.0
```

**Stata version info**

```
Stata/IC 16.0 for Windows (64-bit x86-64)
```
