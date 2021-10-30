# Perils of “revitalization”? 
# Gentrification, political participation and the support from immigrant-origin voters for social democratic and multicultural parties in Amsterdam 

# Maartje van de Koppel
# Data analysis
# Last update: 29/10/21

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
                  'housing_soc_delta2005','housing_soc_delta2009', 'housing_soc_delta2013', 'housing_pub_delta2013', 
                  'imm_TMSA', 'imm_other')
  
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
  fre(subdata$missing_housing_soc) # approx. 25% is missing on this variable 
  
# Check if manually combined neighbourhoods are significantly different
  


  
# Assumption checks ------------------------------------------------------------------------------------------------- 

# TO DO
  # check whether manually combined neighbourhoods are significantly different from others
    # t-tests on bc_combined and all other variables
  # assumption checks for regression
  # collinearity checks for gentrification indicators: can they be put in one model?
  


