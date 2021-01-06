* Script for recreating the yield scores
import delimited "data/input/yields.csv", clear

* Full period
pca r3m r6m r1y r3y r5y r10y r15y
predict total, score
 
 
* In-year/Out-year decomposition
** in-year
pca r3m r6m
predict in_year, score


** out-year
pca r1y r3y r5y r10y r15y
predict out_year, score


* short_term/medium_term/long_term
** short_term (less than 1 year)
pca r3m r6m
predict short_term, score

** medium_term (1-3 years)
pca r1y r3y
predict medium_term, score

** long_term (3 years)
pca r5y r10y r15y
predict long_term, score



export delimited using "data/output/yield_scores"