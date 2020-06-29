# Starter code

# load files from github
kaggle <- read.csv(url("https://raw.githubusercontent.com/Reinalynn/MSDS692/master/Data/kaggle.csv"), header = TRUE, stringsAsFactors = FALSE)
county_data <- read.csv(url("https://raw.githubusercontent.com/Reinalynn/MSDS692/master/Data/County_data.csv"), header = TRUE, stringsAsFactors = FALSE)
cases0624 <- read.csv(url("https://raw.githubusercontent.com/Reinalynn/MSDS692/master/Data/cases0624.csv"), header = TRUE, stringsAsFactors = FALSE)
deaths0624 <- read.csv(url("https://raw.githubusercontent.com/Reinalynn/MSDS692/master/Data/deaths0624.csv"), header = TRUE, stringsAsFactors = FALSE)
US_7cases <- read.csv(url("https://raw.githubusercontent.com/Reinalynn/MSDS692/master/Data/US_7cases.csv"), header = TRUE, stringsAsFactors = FALSE)
US_7deaths <- read.csv(url("https://raw.githubusercontent.com/Reinalynn/MSDS692/master/Data/US_7deaths.csv"), header = TRUE, stringsAsFactors = FALSE)

library(astsa)
library(broom)
library(caret)
library(DAAG)
library(dplyr)
library(dynlm)
library(forecast)
library(fpp2)
library(funModeling)
library(knitr)
library(MARSS)
library(MTS)
library(quantmod)
library(readxl)
library(reshape)
library(sf)
library(tidyverse)
library(tidycensus)
library(tmap)
library(tmaptools)
library(tseries)
library(urbnmapr)
library(urca)
library(varhandle)
library(vars)

head(kaggle)
head(county_data)
cases0624[1:5, c(1, 2, 3, 4, 5, 159)]
deaths0624[1:5, c(1, 2, 3, 4, 5, 159)]

# References
# https://kevinkotze.github.io/ts-7-tut/
# https://www.r-econometrics.com/timeseries/varintro/ 
# https://datascienceplus.com/time-series-analysis-using-arima-model-in-r/
# https://www.medrxiv.org/content/10.1101/2020.04.17.20069237v1.full.pdf
# https://subscription.packtpub.com/book/big_data_and_business_intelligence/9781783552078/1/ch01lvl1sec08/multivariate-time-series-analysis
# https://otexts.com/fpp2/lagged-predictors.html
# https://a-little-book-of-r-for-time-series.readthedocs.io/en/latest/src/timeseries.html
# http://uc-r.github.io/ts_exp_smoothing
# https://rstudio-pubs-static.s3.amazonaws.com/303786_f1b99d6b7e9346c4b1488a174bab839a.html
# https://robjhyndman.com/eindhoven/3-2-Dynamic-Regression.pdf
# https://www.rdocumentation.org/packages/forecast/versions/5.4/topics/Arima
# https://community.rstudio.com/t/how-to-compare-a-forecast-model-to-actual-data-and-what-is-uncertainty/23598/2
# https://blogs.oracle.com/datascience/introduction-to-forecasting-with-arima-in-r
# https://www.otexts.org/fpp2
# DataCamp course on ARIMA with R
# use work flow @ https://datascienceplus.com/time-series-analysis-using-arima-model-in-r/
# VAR for combined counties and add in other variables
# try multiple counties and focus on one variable (cases or deaths)
# https://bookdown.org/singh_pratap_tejendra/intro_time_series_r/multivariate-ts-analysis.html
# http://past.rinfinance.com/agenda/2013/talk/RueyTsay.pdf
# https://subscription.packtpub.com/book/big_data_and_business_intelligence/9781783552078/1/ch01lvl1sec08/multivariate-time-series-analysis
# https://stats.stackexchange.com/questions/99907/how-to-test-the-influence-of-external-factors-on-time-series
# https://bookdown.org/ccolonescu/RPoE4/
# http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/
# https://www.statmethods.net/stats/regression.html
# http://r-statistics.co/Model-Selection-in-R.html
# https://bookdown.org/ccolonescu/RPoE4/simplelm.html
# http://www.sthda.com/english/articles/38-regression-model-validation/157-cross-validation-essentials-in-r/
# https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/
# https://medium.com/@urban_institute/how-to-create-state-and-county-maps-easily-in-r-577d29300bb2
# https://mgimond.github.io/ES218/Week12a.html
# https://mgimond.github.io/Spatial/mapping-data-in-r.html
# http://zevross.com/blog/2018/10/02/creating-beautiful-demographic-maps-in-r-with-the-tidycensus-and-tmap-packages/
# https://cran.r-project.org/web/packages/tmap/tmap.pdf
# https://www.rdocumentation.org/packages/vars/versions/1.5-3/topics/VARselect
# https://cran.r-project.org/web/packages/vars/vars.pdf