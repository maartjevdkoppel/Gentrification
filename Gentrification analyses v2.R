# Perils of “revitalization”? 
# Gentrification, political participation and the support from immigrant-origin voters for social democratic and multicultural parties in Amsterdam 

# Maartje van de Koppel
# Data analysis
# Last update: 30/10/21

# Set-up ------------------------------------------------------------------------------------------------------------ 

# Clean environment
  rm(list=ls())

# TO DO: elect only libraries used
  library(raster)     
  library(foreign) 
  library(tidyverse)
  library(ggplot2)
  library(readxl)
  library(dplyr)
  library(stats)
  library(tidyr)
  library(expss)

# Import data -------------------------------------------------------------------------------------------------------

# Read data
  fulldata <- readRDS("/Users/Maartje/Desktop/gentrification_data_long_revised.rds")
  
# Prepare data for analysis ----------------------------------------------------------------------------------------- 
  
# Subset to 2018 only
  subdata <- fulldata[ which(fulldata$year=='2018'), ]
  
# Redefine categories for migration background
  # Turkish, Moroccan, Surinamese & Antillean vs. other migration vs. no migration background
  subdata$imm_TMSA  <- subdata$imm_Tur + subdata$imm_Mar + subdata$imm_Sur + subdata$imm_Ant
  subdata$imm_other <- subdata$imm_otherNW + subdata$imm_W
  
  var_lab(subdata$imm_TMSA)  = "% Turkish, Moroccan, Surinamese or Antillean migration background"
  var_lab(subdata$imm_other) = "% Other migration background"
  
# Check for missing data
  varlist <- list('PVDA', 'DENK', 'BIJ1', 'imm_Sur', 'imm_Ant', 'imm_Tur', 'imm_Mar', 'imm_otherNW', 'imm_W',
                  'age_18t26', 'age_66plus', 'edu_low', 'edu_high', 'housing_soc', 'housing_pub', 'unempl', 'netHHincome',
                  'MCparties', 'turnout', 'netincome_delta2005', 'netincome_delta2009', 'netincome_delta2013',
                  'housing_soc_delta2005','housing_soc_delta2009', 'housing_soc_delta2013', 'housing_pub_delta2005', 
                  'housing_pub_delta2009', 'housing_pub_delta2013', 'imm_TMSA', 'imm_other')
  
  for (n in varlist){print(n)
    print(summary(subdata[,n]))} 
  # only substantive number of missings for the '% social housing' variable
      # 2017:       13 NAs
      # ∆2005-2017: 21 NAs
      # ∆2009-2017: 18 NAs
      # ∆2013-2017: 17 NAs
   # due to dependency on survey data for these numbers;
   # need at least 50 respondents in a neighbourhood

# Check if neighbourhoods with missing data on social housing are significantly different
  subdata$missing_housing_soc <- ifelse(is.na(subdata$housing_soc) | is.na(subdata$housing_soc_delta2005) |
                                        is.na(subdata$housing_soc_delta2009) | is.na(subdata$housing_soc_delta2013), 1, 0)
  fre(subdata$missing_housing_soc) # 25.3% (21 out of 83 neighbourhoods) is missing on this variable 
  
  # On the dependent variables - no significant differences
  t.test(PVDA      ~ missing_housing_soc, data = subdata, alternative = "two.sided")
  t.test(MCparties ~ missing_housing_soc, data = subdata, alternative = "two.sided")
  t.test(turnout   ~ missing_housing_soc, data = subdata, alternative = "two.sided")
  
  # On the main predictors - many significant differences
  t.test(imm_TMSA              ~ missing_housing_soc, data = subdata, alternative = "two.sided") # significantly different
  t.test(netincome_delta2005   ~ missing_housing_soc, data = subdata, alternative = "two.sided") # significantly different
  t.test(netincome_delta2009   ~ missing_housing_soc, data = subdata, alternative = "two.sided") # significantly different
  t.test(netincome_delta2013   ~ missing_housing_soc, data = subdata, alternative = "two.sided") # significantly different
  t.test(housing_pub_delta2013 ~ missing_housing_soc, data = subdata, alternative = "two.sided") # significantly different   
  
  # On the control variables - many significant differences
  t.test(imm_other   ~ missing_housing_soc, data = subdata, alternative = "two.sided")
  t.test(age_18t26   ~ missing_housing_soc, data = subdata, alternative = "two.sided") # significantly different
  t.test(age_66plus  ~ missing_housing_soc, data = subdata, alternative = "two.sided") # significantly different
  t.test(edu_low     ~ missing_housing_soc, data = subdata, alternative = "two.sided")
  t.test(edu_high    ~ missing_housing_soc, data = subdata, alternative = "two.sided")
  t.test(housing_soc ~ missing_housing_soc, data = subdata, alternative = "two.sided")
  t.test(housing_pub ~ missing_housing_soc, data = subdata, alternative = "two.sided") # significantly different
  t.test(unempl      ~ missing_housing_soc, data = subdata, alternative = "two.sided") # significantly different
  t.test(netHHincome ~ missing_housing_soc, data = subdata, alternative = "two.sided") # significantly different
  
  # NOTE: since the data one social housing contains too many neighbourhoods with missing data that significantly 
  # differ from the other neighbourhoods, we cannot use this variable as a reliable indicator for gentrification in this analysis.
  
# Check if manually combined neighbourhoods are significantly different
  # On the dependent variables - no significant differences
  t.test(PVDA      ~ bc_combined, data = subdata, alternative = "two.sided")
  t.test(MCparties ~ bc_combined, data = subdata, alternative = "two.sided")
  t.test(turnout   ~ bc_combined, data = subdata, alternative = "two.sided")
  
  # On the main predictors - no significant differences
  t.test(imm_TMSA              ~ bc_combined, data = subdata, alternative = "two.sided")
  t.test(netincome_delta2005   ~ bc_combined, data = subdata, alternative = "two.sided") 
  t.test(netincome_delta2009   ~ bc_combined, data = subdata, alternative = "two.sided") 
  t.test(netincome_delta2013   ~ bc_combined, data = subdata, alternative = "two.sided") 
  t.test(housing_soc_delta2005 ~ bc_combined, data = subdata, alternative = "two.sided") 
  t.test(housing_soc_delta2009 ~ bc_combined, data = subdata, alternative = "two.sided")
  t.test(housing_soc_delta2013 ~ bc_combined, data = subdata, alternative = "two.sided")   
  t.test(housing_pub_delta2013 ~ bc_combined, data = subdata, alternative = "two.sided")   
   
  # On the control variables - no significant differences
  t.test(imm_other   ~ bc_combined, data = subdata, alternative = "two.sided")
  t.test(age_18t26   ~ bc_combined, data = subdata, alternative = "two.sided")
  t.test(age_66plus  ~ bc_combined, data = subdata, alternative = "two.sided")
  t.test(edu_low     ~ bc_combined, data = subdata, alternative = "two.sided")
  t.test(edu_high    ~ bc_combined, data = subdata, alternative = "two.sided")
  t.test(housing_soc ~ bc_combined, data = subdata, alternative = "two.sided")
  t.test(housing_pub ~ bc_combined, data = subdata, alternative = "two.sided")
  t.test(unempl      ~ bc_combined, data = subdata, alternative = "two.sided")
  t.test(netHHincome ~ bc_combined, data = subdata, alternative = "two.sided")
  
# Check OLS regression assumptions ------------------------------------------------------------------------------------------------- 

  # Typically, for each of the independent variables (predictors), the following plots are drawn to visualize the following behavior:
    # Scatter plot: Visualize the linear relationship between the predictor and response
    # Box plot: To spot any outlier observations in the variable. Having outliers in your predictor can drastically affect the predictions as they can easily affect the direction/slope of the line of best fit.
    # Density plot: To see the distribution of the predictor variable. Ideally, a close to normal distribution (a bell shaped curve), without being skewed to the left or right is preferred. Let us see how to make each one of them.
  
  
  
# TO DO
  # assumption checks for regression
  # collinearity checks for gentrification indicators: can they be put in one model?
  

# Analysis ------------------------------------------------------------------------------------------------- 

  # TO DO: remove observations with missings on the main repredictors 
  # Especially important for social housing models!
  
  