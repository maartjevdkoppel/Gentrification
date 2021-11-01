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
  library(mctest)
  library(texreg)


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
  t.test(housing_pub_delta2005 ~ bc_combined, data = subdata, alternative = "two.sided") 
  t.test(housing_pub_delta2009 ~ bc_combined, data = subdata, alternative = "two.sided")
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
  
# Create separate databases, so observations with missings can be removed
  # For all three gentrification options: 4, 8 and 12 years change
  # For all three dependent variables
  
  # PVDA option 1: gentrification over 4 years
  data.pvda.op1 <- select(subdata, c(bc_code, bc_name, PVDA, housing_pub, netHHincome, imm_TMSA, imm_other,
                                     edu_low, edu_high, age_18t26, age_66plus, unempl,
                                     housing_pub_delta2013, netincome_delta2013))
  data.pvda.op1 <- data.pvda.op1[complete.cases(data.pvda.op1),] # 1 observation deleted
  
  # PVDA option 2: gentrification over 8 years
  data.pvda.op2 <- select(subdata, c(bc_code, bc_name, PVDA, housing_pub, netHHincome, imm_TMSA, imm_other,
                                     edu_low, edu_high, age_18t26, age_66plus, unempl,
                                     housing_pub_delta2009, netincome_delta2009))
  data.pvda.op2 <- data.pvda.op2[complete.cases(data.pvda.op2),] # 1 observation deleted
  
  # PVDA option 3: gentrification over 12 years
  data.pvda.op3 <- select(subdata, c(bc_code, bc_name, PVDA, housing_pub, netHHincome, imm_TMSA, imm_other,
                                     edu_low, edu_high, age_18t26, age_66plus, unempl,
                                     housing_pub_delta2005, netincome_delta2005))
  data.pvda.op3 <- data.pvda.op3[complete.cases(data.pvda.op3),] # 2 observations deleted
  
  # MC parties option 1: gentrification over 4 years
  data.mc.op1 <- select(subdata, c(bc_code, bc_name, MCparties, housing_pub, netHHincome, imm_TMSA, imm_other,
                                     edu_low, edu_high, age_18t26, age_66plus, unempl,
                                     housing_pub_delta2013, netincome_delta2013))
  data.mc.op1 <- data.mc.op1[complete.cases(data.mc.op1),] # 1 observation deleted
  
  # MC parties option 2: gentrification over 8 years
  data.mc.op2 <- select(subdata, c(bc_code, bc_name, MCparties, housing_pub, netHHincome, imm_TMSA, imm_other,
                                   edu_low, edu_high, age_18t26, age_66plus, unempl,
                                   housing_pub_delta2009, netincome_delta2009))
  data.mc.op2 <- data.mc.op2[complete.cases(data.mc.op2),] # 1 observation deleted
  
  # MC parties option 3: gentrification over 12 years
  data.mc.op3 <- select(subdata, c(bc_code, bc_name, MCparties, housing_pub, netHHincome, imm_TMSA, imm_other,
                                   edu_low, edu_high, age_18t26, age_66plus, unempl,
                                   housing_pub_delta2005, netincome_delta2005))
  data.mc.op3 <- data.mc.op3[complete.cases(data.mc.op3),] # 2 observations deleted
  
  # Turnout option 1: gentrification over 4 years
  data.turn.op1 <- select(subdata, c(bc_code, bc_name, turnout, housing_pub, netHHincome, imm_TMSA, imm_other,
                                   edu_low, edu_high, age_18t26, age_66plus, unempl,
                                   housing_pub_delta2013, netincome_delta2013))
  data.turn.op1 <- data.turn.op1[complete.cases(data.turn.op1),] # 1 observation deleted
  
  # Turnout option 2: gentrification over 8 years
  data.turn.op2 <- select(subdata, c(bc_code, bc_name, turnout, housing_pub, netHHincome, imm_TMSA, imm_other,
                                     edu_low, edu_high, age_18t26, age_66plus, unempl,
                                     housing_pub_delta2009, netincome_delta2009))
  data.turn.op2 <- data.turn.op2[complete.cases(data.turn.op2),] # 1 observation deleted
  
  # Turnout option 3: gentrification over 12 years
  data.turn.op3 <- select(subdata, c(bc_code, bc_name, turnout, housing_pub, netHHincome, imm_TMSA, imm_other,
                                     edu_low, edu_high, age_18t26, age_66plus, unempl,
                                     housing_pub_delta2005, netincome_delta2005))
  data.turn.op3 <- data.turn.op3[complete.cases(data.turn.op3),] # 2 observations deleted

  
# Analysis: PVDA ------------------------------------------------------------------------------------------------- 

# FOR EXPORTING TABLE
# SEE https://cran.r-project.org/web/packages/jtools/vignettes/summ.html#Table_output_for_Word_and_RMarkdown_documents
  
# OPTION 1 - Gentrification as change over 4 years (2013-2017)
  # Model 1: composition effects
  pvda.op1.m1 <- lm(PVDA ~ housing_pub + netHHincome
                    + imm_TMSA + imm_other +
                    + edu_low + edu_high + age_18t26 + age_66plus + unempl, 
                    data = data.pvda.op1)
  summary(pvda.op1.m1)
  
  # Model 2: add gentrification (change variables)
  pvda.op1.m2 <- lm(PVDA ~ housing_pub + netHHincome
                   + imm_TMSA + imm_other +
                   + edu_low + edu_high + age_18t26 + age_66plus + unempl
                   + housing_pub_delta2013 + netincome_delta2013, 
                   data = data.pvda.op1)
  summary(pvda.op1.m2)
  
  # Model 3: interaction effect
  pvda.op1.m3 <- lm(PVDA ~ housing_pub + netHHincome
                    + imm_TMSA + imm_other +
                    + edu_low + edu_high + age_18t26 + age_66plus + unempl
                    + housing_pub_delta2013 + netincome_delta2013
                    + housing_pub_delta2013:imm_TMSA + netincome_delta2013:imm_TMSA,
                    data = data.pvda.op1)
  summary(pvda.op1.m3)
  
  # Export regression table
  wordreg(l = list(pvda.op1.m1, pvda.op1.m2, pvda.op1.m3), file = "/Desktop/pvda_op1.doc", 
          groups = list("Neighbourhood composition" = 2:10, "Gentrification" = 11:12, "Interaction effects" = 13:14))
  
# OPTION 2 - Gentrification as change over 8 years (2009-2017)
  # Model 1: composition effects
  pvda.op2.m1 <- lm(PVDA ~ housing_pub + netHHincome
                    + imm_TMSA + imm_other +
                    + edu_low + edu_high + age_18t26 + age_66plus + unempl, 
                    data = data.pvda.op2)
  summary(pvda.op2.m1)
  
  # Model 2: add gentrification (change variables)
  pvda.op2.m2 <- lm(PVDA ~ housing_pub + netHHincome
                    + imm_TMSA + imm_other +
                    + edu_low + edu_high + age_18t26 + age_66plus + unempl
                    + housing_pub_delta2009 + netincome_delta2009, 
                    data = data.pvda.op2)
  summary(pvda.op2.m2)
  
  # Model 3: interaction effect
  pvda.op2.m3 <-
    
  # Export regression table
      
# OPTION 3 - Gentrification as change over 12 years (2005-2017)
  # Model 1: composition effects
  pvda.op3.m1 <- lm(PVDA ~ housing_pub + netHHincome
                      + imm_TMSA + imm_other +
                      + edu_low + edu_high + age_18t26 + age_66plus + unempl, 
                      data = data.pvda.op3)
  summary(pvda.op3.m1)
  
  # Model 2: add gentrification (change variables)
  pvda.op3.m2 <- lm(PVDA ~ housing_pub + netHHincome
                    + imm_TMSA + imm_other +
                    + edu_low + edu_high + age_18t26 + age_66plus + unempl
                    + housing_pub_delta2005 + netincome_delta2005, 
                    data = data.pvda.op3)
  summary(pvda.op3.m2)
  
  # Model 3: interaction effect
  pvda.op3.m3 <-
  
  # Export regression table
  
# Analysis: MC parties ------------------------------------------------------------------------------------------------- 
  
  # TO DO: remove observations with missings on the main repredictors 
  
# OPTION 1 - Gentrification as change over 4 years (2013-2017)
  # Model 1: composition effects
  
  # Model 2: add gentrification (change variables)
  # Model 3: interaction effect
  
# OPTION 2 - Gentrification as change over 8 years (2009-2017)
  # Model 1: composition effects
  # Model 2: add gentrification (change variables)
  # Model 3: interaction effect
  
# OPTION 3 - Gentrification as change over 12 years (2005-2017)
  # Model 1: composition effects
  # Model 2: add gentrification (change variables)
  # Model 3: interaction effect
  
  
# Analysis: Turnout ------------------------------------------------------------------------------------------------- 
  
  # TO DO: remove observations with missings on the main repredictors 
  
# OPTION 1 - Gentrification as change over 4 years (2013-2017)
  # Model 1: composition effects
  
  # Model 2: add gentrification (change variables)
  # Model 3: interaction effect
  
# OPTION 2 - Gentrification as change over 8 years (2009-2017)
  # Model 1: composition effects
  # Model 2: add gentrification (change variables)
  # Model 3: interaction effect
  
# OPTION 3 - Gentrification as change over 12 years (2005-2017)
  # Model 1: composition effects
  # Model 2: add gentrification (change variables)
  # Model 3: interaction effect
  
  
# Check OLS regression assumptions ------------------------------------------------------------------------------------------------- 
  
  # TO DO
  # assumption checks for regression
  # collinearity checks for gentrification indicators: can they be put in one model?
  
  # SEE:
    # http://r-statistics.co/Linear-Regression.html
    # https://www.scribbr.com/statistics/multiple-linear-regression/
  
  
  # Typically, for each of the independent variables (predictors), the following plots are drawn to visualize the following behavior:
  # Scatter plot: Visualize the linear relationship between the predictor and response
  # Box plot: To spot any outlier observations in the variable. Having outliers in your predictor can drastically affect the predictions as they can easily affect the direction/slope of the line of best fit.
  # Density plot: To see the distribution of the predictor variable. Ideally, a close to normal distribution (a bell shaped curve), without being skewed to the left or right is preferred. Let us see how to make each one of them.
  
  
  
 
  
  