# Perils of “revitalization”? 
# Gentrification, political participation, and support for social democrats in Amsterdam 

# Maartje van de Koppel
# Data analysis
# Last update: 19/10/22

# Set-up ------------------------------------------------------------------------------------------------------------ 

# Set up working directory
# TODO remove
setwd("/Users/Maartje/Desktop/LJA/Paper politicologenetmaal/Link-Jong-Amsterdam/Data/Analysis")

# TODO: select only libraries used
# TODO: add explanations of use

#NOTE: give permission to maptools to use the gpclib package:
  # check status with gpclibPermitStatus()
  # grant permission with gpclibPermit()

library(maptools)     #map making
library(rgdal)        #reading shapefiles for map making
library(gpclib)       #dependency rgdal
library(broom)        #tidying shapefiles to dataframe
library(raster)     
library(foreign) 
library(tidyverse)    #tidyverse collection
library(ggplot2)
library(readxl)
library(dplyr)
library(plyr)         #TODO: needed for join function?
library(stats)
library(tidyr)       
library(expss)
library(mctest)
library(texreg)       #exporting regression tables to word
library(emmeans)
library(sf)           #reading shapefiles
library(RColorBrewer) #colour scales for maps

# Import data -------------------------------------------------------------------------------------------------------

# Read data
  fulldata <- readRDS("gentrification_data_long_revised.rds")
  
# Prepare data for analysis ----------------------------------------------------------------------------------------- 
  
# Subset to 2018 only
  subdata <- fulldata[which(fulldata$year=='2018'), ]
  
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
  
  # On the dependent variables 
  t.test(PVDA      ~ missing_housing_soc, data = subdata, alternative = "two.sided")
  t.test(MCparties ~ missing_housing_soc, data = subdata, alternative = "two.sided")
  t.test(turnout   ~ missing_housing_soc, data = subdata, alternative = "two.sided")
  
  # On the main predictors 
  t.test(imm_TMSA              ~ missing_housing_soc, data = subdata, alternative = "two.sided") # significantly different
  t.test(netincome_delta2005   ~ missing_housing_soc, data = subdata, alternative = "two.sided") # significantly different
  t.test(netincome_delta2009   ~ missing_housing_soc, data = subdata, alternative = "two.sided") # significantly different
  t.test(netincome_delta2013   ~ missing_housing_soc, data = subdata, alternative = "two.sided") # significantly different
  t.test(housing_pub_delta2013 ~ missing_housing_soc, data = subdata, alternative = "two.sided") # significantly different   
  
  # On the control variables
  t.test(imm_other   ~ missing_housing_soc, data = subdata, alternative = "two.sided")
  t.test(age_18t26   ~ missing_housing_soc, data = subdata, alternative = "two.sided") # significantly different
  t.test(age_66plus  ~ missing_housing_soc, data = subdata, alternative = "two.sided") # significantly different
  t.test(edu_low     ~ missing_housing_soc, data = subdata, alternative = "two.sided")
  t.test(edu_high    ~ missing_housing_soc, data = subdata, alternative = "two.sided")
  t.test(housing_soc ~ missing_housing_soc, data = subdata, alternative = "two.sided")
  t.test(housing_pub ~ missing_housing_soc, data = subdata, alternative = "two.sided") # significantly different
  t.test(unempl      ~ missing_housing_soc, data = subdata, alternative = "two.sided") # significantly different
  t.test(netHHincome ~ missing_housing_soc, data = subdata, alternative = "two.sided") # significantly different
  
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
  # For all three gentrification options: 4 (main), 8 and 12 years change (robustness checks)
  # For both dependent variables
  
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
  
  #TODO: remove
  ## MC parties option 1: gentrification over 4 years
  #data.mc.op1 <- select(subdata, c(bc_code, bc_name, MCparties, housing_pub, netHHincome, imm_TMSA, imm_other,
  #                                   edu_low, edu_high, age_18t26, age_66plus, unempl,
  #                                   housing_pub_delta2013, netincome_delta2013))
  #data.mc.op1 <- data.mc.op1[complete.cases(data.mc.op1),] # 1 observation deleted
  #
  ## MC parties option 2: gentrification over 8 years
  #data.mc.op2 <- select(subdata, c(bc_code, bc_name, MCparties, housing_pub, netHHincome, imm_TMSA, imm_other,
  #                                 edu_low, edu_high, age_18t26, age_66plus, unempl,
  #                                 housing_pub_delta2009, netincome_delta2009))
  #data.mc.op2 <- data.mc.op2[complete.cases(data.mc.op2),] # 1 observation deleted
  #
  ## MC parties option 3: gentrification over 12 years
  #data.mc.op3 <- select(subdata, c(bc_code, bc_name, MCparties, housing_pub, netHHincome, imm_TMSA, imm_other,
  #                                 edu_low, edu_high, age_18t26, age_66plus, unempl,
  #                                 housing_pub_delta2005, netincome_delta2005))
  #data.mc.op3 <- data.mc.op3[complete.cases(data.mc.op3),] # 2 observations deleted
  
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

# OPTION 1 - Gentrification as change over 4 years (2013-2017)
  # Model 1: composition effects
  pvda.op1.m1 <- lm(PVDA ~ housing_pub + netHHincome
                    + imm_TMSA + imm_other +
                    + edu_low + edu_high + age_18t26 + age_66plus + unempl, 
                    data = data.pvda.op1)
  summary(pvda.op1.m1)
  
  # Model 2: add gentrification
  pvda.op1.m2 <- lm(PVDA ~ housing_pub + netHHincome
                   + imm_TMSA + imm_other +
                   + edu_low + edu_high + age_18t26 + age_66plus + unempl
                   + housing_pub_delta2013 + netincome_delta2013, 
                   data = data.pvda.op1)
  summary(pvda.op1.m2)
  
  #TODO: remove if no longer in theory
  ## Model 3: interaction effect
  #pvda.op1.m3 <- lm(PVDA ~ housing_pub + netHHincome
  #                  + imm_TMSA + imm_other +
  #                  + edu_low + edu_high + age_18t26 + age_66plus + unempl
  #                  + housing_pub_delta2013 + netincome_delta2013
  #                  + housing_pub_delta2013:imm_TMSA + netincome_delta2013:imm_TMSA,
  #                  data = data.pvda.op1)
  #summary(pvda.op1.m3)
  
  # Export regression table
  wordreg(l = list(pvda.op1.m1, pvda.op1.m2), file = "pvda_gentr_4years.doc", 
          groups = list("Neighbourhood composition" = 2:10, "Gentrification" = 11:12, "Interaction effects" = 13:14))
  
  #TODO: remove if no longer in theory
  ## Predicted probabilities plot: change in public housing
  #pvda.op1.preddata <- with(data.pvda.op1, list(imm_TMSA = seq(5,55,25), housing_pub_delta2013 = seq(-25,10,1), 
  #                                                    housing_pub=mean(housing_pub),
  #                                                    netHHincome=mean(netHHincome),
  #                                                    imm_other  =mean(imm_other),
  #                                                    edu_low    =mean(edu_low),
  #                                                    edu_high   =mean(edu_high),
  #                                                    age_18t26  =mean(age_18t26),
  #                                                    age_66plus =mean(age_66plus),
  #                                                    unempl     =mean(unempl),
  #                                                    netincome_delta2013=mean(netincome_delta2013)))
  #jpeg("pvda_4years_pubhousing_plot.jpg", width = 500, height = 500)
  #emmip(pvda.op1.m3, imm_TMSA ~ housing_pub_delta2013, at=pvda.op1.preddata, CIs=TRUE) + labs(title="Predicted levels of PvdA support at different levels of change \nin public housing (∆t = 4), by level of TMSA migration background")
  #dev.off()
  
  #TODO: remove if no longer in theory
  ## Predicted probabilities plot: change in net household income 
  #pvda.op1.preddata <- with(data.pvda.op1, list(imm_TMSA = seq(5,55,25), netincome_delta2013 = seq(0,40,1), 
  #                                              housing_pub=mean(housing_pub),
  #                                              netHHincome=mean(netHHincome),
  #                                              imm_other  =mean(imm_other),
  #                                              edu_low    =mean(edu_low),
  #                                              edu_high   =mean(edu_high),
  #                                              age_18t26  =mean(age_18t26),
  #                                              age_66plus =mean(age_66plus),
  #                                              unempl     =mean(unempl),
  #                                              housing_pub_delta2013=mean(housing_pub_delta2013)))
  #jpeg("pvda_4years_netincome_plot.jpg", width = 500, height = 500)
  #emmip(pvda.op1.m3, imm_TMSA ~ netincome_delta2013, at=pvda.op1.preddata, CIs=TRUE) + labs(title="Predicted levels of PvdA support at different levels of change \n in net household income (∆t = 4), by level of TMSA migration background")
  #dev.off()
  
  
# Analysis: turnout ------------------------------------------------------------------------------------------------- 
  
# OPTION 1 - Gentrification as change over 4 years (2013-2017)
  # Model 1: composition effects
  turn.op1.m1 <- lm(turnout ~ housing_pub + netHHincome
                  + imm_TMSA + imm_other +
                  + edu_low + edu_high + age_18t26 + age_66plus + unempl, 
                  data = data.turn.op1)
  summary(turn.op1.m1)
  
  # Model 2: add gentrification (change variables)
  turn.op1.m2 <- lm(turnout ~ housing_pub + netHHincome
                    + imm_TMSA + imm_other +
                    + edu_low + edu_high + age_18t26 + age_66plus + unempl
                    + housing_pub_delta2013 + netincome_delta2013, 
                    data = data.turn.op1)
  summary(turn.op1.m2)
  
  # Export regression table
  wordreg(l = list(turn.op1.m1, turn.op1.m2), file = "turnout_gentr_4years.doc", 
          groups = list("Neighbourhood composition" = 2:10, "Gentrification" = 11:12, "Interaction effects" = 13:14))
  
  ## Model 3: interaction effect
  #turn.op1.m3 <- lm(turnout ~ housing_pub + netHHincome
  #                  + imm_TMSA + imm_other +
  #                  + edu_low + edu_high + age_18t26 + age_66plus + unempl
  #                  + housing_pub_delta2013 + netincome_delta2013
  #                  + housing_pub_delta2013:imm_TMSA + netincome_delta2013:imm_TMSA, 
  #                  data = data.turn.op1)
  #summary(turn.op1.m3)
  
  ## Predicted probabilities plot: change in public housing
  #turn.op1.preddata <- with(data.turn.op1, list(imm_TMSA = seq(5,55,25), housing_pub_delta2013 = seq(-25,10,1), 
  #                                              housing_pub=mean(housing_pub),
  #                                              netHHincome=mean(netHHincome),
  #                                              imm_other  =mean(imm_other),
  #                                              edu_low    =mean(edu_low),
  #                                              edu_high   =mean(edu_high),
  #                                              age_18t26  =mean(age_18t26),
  #                                              age_66plus =mean(age_66plus),
  #                                              unempl     =mean(unempl),
  #                                              netincome_delta2013=mean(netincome_delta2013)))
  #jpeg("turnout_4years_pubhousing_plot.jpg", width = 500, height = 500)
  #emmip(turn.op1.m3, imm_TMSA ~ housing_pub_delta2013, at=turn.op1.preddata, CIs=TRUE) + labs(title="Predicted levels of turnout at different levels of change in \npublic housing (∆t = 4), by level of TMSA migration background")
  #dev.off()
  
  ## Predicted probabilities plot: change in net income
  #turn.op1.preddata <- with(data.turn.op1, list(imm_TMSA = seq(5,55,25), netincome_delta2013 = seq(0,50,1), 
  #                                              housing_pub=mean(housing_pub),
  #                                              netHHincome=mean(netHHincome),
  #                                              imm_other  =mean(imm_other),
  #                                              edu_low    =mean(edu_low),
  #                                              edu_high   =mean(edu_high),
  #                                              age_18t26  =mean(age_18t26),
  #                                              age_66plus =mean(age_66plus),
  #                                              unempl     =mean(unempl),
  #                                              housing_pub_delta2013=mean(housing_pub_delta2013)))
  #jpeg("turnout_4years_netincome_plot.jpg", width = 500, height = 500)
  #emmip(turn.op1.m3, imm_TMSA ~ netincome_delta2013, at=turn.op1.preddata, CIs=TRUE) + labs(title="Predicted levels of turnout at different levels of change in \nnet household income (∆t = 4), by level of TMSA migration background")
  #dev.off()

  
# OLD -- Analysis: MC parties ------------------------------------------------------------------------------------------------- 
##TODO: remove if not included in theory anymore!
#
## OPTION 1 - Gentrification as change over 4 years (2013-2017)
## Model 1: composition effects
#mc.op1.m1 <- lm(MCparties ~ housing_pub + netHHincome
#                + imm_TMSA + imm_other +
#                  + edu_low + edu_high + age_18t26 + age_66plus + unempl, 
#                data = data.mc.op1)
#summary(mc.op1.m1)
#
## Model 2: add gentrification (change variables)
#mc.op1.m2 <- lm(MCparties ~ housing_pub + netHHincome
#                + imm_TMSA + imm_other +
#                  + edu_low + edu_high + age_18t26 + age_66plus + unempl
#                + housing_pub_delta2013 + netincome_delta2013, 
#                data = data.mc.op1)
#summary(mc.op1.m2)  
#
## Model 3: interaction effect
#mc.op1.m3 <- lm(MCparties ~ housing_pub + netHHincome
#                + imm_TMSA + imm_other +
#                  + edu_low + edu_high + age_18t26 + age_66plus + unempl
#                + housing_pub_delta2013 + netincome_delta2013
#                + housing_pub_delta2013:imm_TMSA + netincome_delta2013:imm_TMSA, 
#                data = data.mc.op1)
#summary(mc.op1.m3)
#
## Export regression table
#wordreg(l = list(mc.op1.m1, mc.op1.m2, mc.op1.m3), file = "mcparties_gentr_4years.doc", 
#        groups = list("Neighbourhood composition" = 2:10, "Gentrification" = 11:12, "Interaction effects" = 13:14))
#
## Predicted probabilities plot: change in public housing
#mc.op1.preddata <- with(data.mc.op1, list(imm_TMSA = seq(5,55,25), housing_pub_delta2013 = seq(-25,10,1), 
#                                          housing_pub=mean(housing_pub),
#                                          netHHincome=mean(netHHincome),
#                                          imm_other  =mean(imm_other),
#                                          edu_low    =mean(edu_low),
#                                          edu_high   =mean(edu_high),
#                                          age_18t26  =mean(age_18t26),
#                                          age_66plus =mean(age_66plus),
#                                          unempl     =mean(unempl),
#                                          netincome_delta2013=mean(netincome_delta2013)))
#jpeg("mcparties_4years_pubhousing_plot.jpg", width = 500, height = 500)
#emmip(mc.op1.m3, imm_TMSA ~ housing_pub_delta2013, at=mc.op1.preddata, CIs=TRUE) + labs(title="Predicted levels of multicultural party support at different levels \nof change in public housing (∆t = 4), by level of TMSA \nmigration background")
#dev.off()
#
## Predicted probabilities plot: change in net household income
#mc.op1.preddata <- with(data.mc.op1, list(imm_TMSA = seq(5,55,25), netincome_delta2013 = seq(0,50,1), 
#                                          housing_pub=mean(housing_pub),
#                                          netHHincome=mean(netHHincome),
#                                          imm_other  =mean(imm_other),
#                                          edu_low    =mean(edu_low),
#                                          edu_high   =mean(edu_high),
#                                          age_18t26  =mean(age_18t26),
#                                          age_66plus =mean(age_66plus),
#                                          unempl     =mean(unempl),
#                                          housing_pub_delta2013=mean(housing_pub_delta2013)))
#jpeg("mcparties_4years_netincome_plot.jpg", width = 500, height = 500)
#emmip(mc.op1.m3, imm_TMSA ~ netincome_delta2013, at=mc.op1.preddata, CIs=TRUE) + labs(title="Predicted levels of multicultural party support at different levels \nof change in net household income (∆t = 4), by level of TMSA \nmigration background")
#dev.off()
#
#
## OPTION 2 - Gentrification as change over 8 years (2009-2017)
## Model 1: composition effects
#mc.op2.m1 <- lm(MCparties ~ housing_pub + netHHincome
#                + imm_TMSA + imm_other +
#                  + edu_low + edu_high + age_18t26 + age_66plus + unempl, 
#                data = data.mc.op2)
#summary(mc.op2.m1)
#
## Model 2: add gentrification (change variables)
#mc.op2.m2 <- lm(MCparties ~ housing_pub + netHHincome
#                + imm_TMSA + imm_other +
#                  + edu_low + edu_high + age_18t26 + age_66plus + unempl
#                + housing_pub_delta2009 + netincome_delta2009, 
#                data = data.mc.op2)
#summary(mc.op2.m2)  
#
## Model 3: interaction effect
#mc.op2.m3 <- lm(MCparties ~ housing_pub + netHHincome
#                + imm_TMSA + imm_other +
#                  + edu_low + edu_high + age_18t26 + age_66plus + unempl
#                + housing_pub_delta2009 + netincome_delta2009
#                + housing_pub_delta2009:imm_TMSA + netincome_delta2009:imm_TMSA, 
#                data = data.mc.op2)
#summary(mc.op2.m3)
#
## Export regression table
#wordreg(l = list(mc.op2.m1, mc.op2.m2, mc.op2.m3), file = "mcparties_gentr_8years.doc", 
#        groups = list("Neighbourhood composition" = 2:10, "Gentrification" = 11:12, "Interaction effects" = 13:14))
#
## Predicted probabilities plot: change in public housing
#mc.op2.preddata <- with(data.mc.op2, list(imm_TMSA = seq(5,55,25), housing_pub_delta2009 = seq(-25,10,1), 
#                                          housing_pub=mean(housing_pub),
#                                          netHHincome=mean(netHHincome),
#                                          imm_other  =mean(imm_other),
#                                          edu_low    =mean(edu_low),
#                                          edu_high   =mean(edu_high),
#                                          age_18t26  =mean(age_18t26),
#                                          age_66plus =mean(age_66plus),
#                                          unempl     =mean(unempl),
#                                          netincome_delta2009=mean(netincome_delta2009)))
#jpeg("mcparties_8years_pubhousing_plot.jpg", width = 500, height = 500)
#emmip(mc.op2.m3, imm_TMSA ~ housing_pub_delta2009, at=mc.op2.preddata, CIs=TRUE) + labs(title="Predicted levels of multicultural party support at different levels \nof change in public housing (∆t = 8), by level of TMSA \nmigration background")
#dev.off()
#
## Predicted probabilities plot: change in net household income
#mc.op2.preddata <- with(data.mc.op2, list(imm_TMSA = seq(5,55,25), netincome_delta2009 = seq(0,50,1), 
#                                          housing_pub=mean(housing_pub),
#                                          netHHincome=mean(netHHincome),
#                                          imm_other  =mean(imm_other),
#                                          edu_low    =mean(edu_low),
#                                          edu_high   =mean(edu_high),
#                                          age_18t26  =mean(age_18t26),
#                                          age_66plus =mean(age_66plus),
#                                          unempl     =mean(unempl),
#                                          housing_pub_delta2009=mean(housing_pub_delta2009)))
#jpeg("mcparties_8years_netincome_plot.jpg", width = 500, height = 500)
#emmip(mc.op2.m3, imm_TMSA ~ netincome_delta2009, at=mc.op2.preddata, CIs=TRUE) + labs(title="Predicted levels of multicultural party support at different levels \nof change in net household income (∆t = 8), by level of TMSA \nmigration background")
#dev.off()
#
## OPTION 3 - Gentrification as change over 12 years (2005-2017)
## Model 1: composition effects
#mc.op3.m1 <- lm(MCparties ~ housing_pub + netHHincome
#                + imm_TMSA + imm_other +
#                  + edu_low + edu_high + age_18t26 + age_66plus + unempl, 
#                data = data.mc.op3)
#summary(mc.op3.m1)
#
## Model 2: add gentrification (change variables)
#mc.op3.m2 <- lm(MCparties ~ housing_pub + netHHincome
#                + imm_TMSA + imm_other +
#                  + edu_low + edu_high + age_18t26 + age_66plus + unempl
#                + housing_pub_delta2005 + netincome_delta2005, 
#                data = data.mc.op3)
#summary(mc.op3.m2)  
#
## Model 3: interaction effect
#mc.op3.m3 <- lm(MCparties ~ housing_pub + netHHincome
#                + imm_TMSA + imm_other +
#                  + edu_low + edu_high + age_18t26 + age_66plus + unempl
#                + housing_pub_delta2005 + netincome_delta2005
#                + housing_pub_delta2005:imm_TMSA + netincome_delta2005:imm_TMSA, 
#                data = data.mc.op3)
#summary(mc.op3.m3)
#
## Export regression table
#wordreg(l = list(mc.op3.m1, mc.op3.m2, mc.op3.m3), file = "mcparties_gentr_12years.doc", 
#        groups = list("Neighbourhood composition" = 2:10, "Gentrification" = 11:12, "Interaction effects" = 13:14))
#
## Predicted probabilities plot
#mc.op3.preddata <- with(data.mc.op3, list(imm_TMSA = seq(5,55,25), housing_pub_delta2005 = seq(-25,10,1), 
#                                          housing_pub=mean(housing_pub),
#                                          netHHincome=mean(netHHincome),
#                                          imm_other  =mean(imm_other),
#                                          edu_low    =mean(edu_low),
#                                          edu_high   =mean(edu_high),
#                                          age_18t26  =mean(age_18t26),
#                                          age_66plus =mean(age_66plus),
#                                          unempl     =mean(unempl),
#                                          netincome_delta2005=mean(netincome_delta2005)))
#jpeg("mcparties_12years_pubhousing_plot.jpg", width = 500, height = 500)
#emmip(mc.op3.m3, imm_TMSA ~ housing_pub_delta2005, at=mc.op3.preddata, CIs=TRUE) + labs(title="Predicted levels of multicultural party support at different levels \nof change in public housing (∆t = 12), by level of TMSA \nmigration background")
#dev.off()
#
## Predicted probabilities plot
#mc.op3.preddata <- with(data.mc.op3, list(imm_TMSA = seq(5,55,25), netincome_delta2005 = seq(0,50,1), 
#                                          housing_pub=mean(housing_pub),
#                                          netHHincome=mean(netHHincome),
#                                          imm_other  =mean(imm_other),
#                                          edu_low    =mean(edu_low),
#                                          edu_high   =mean(edu_high),
#                                          age_18t26  =mean(age_18t26),
#                                          age_66plus =mean(age_66plus),
#                                          unempl     =mean(unempl),
#                                          housing_pub_delta2005=mean(housing_pub_delta2005)))
#jpeg("mcparties_12years_netincome_plot.jpg", width = 500, height = 500)
#emmip(mc.op3.m3, imm_TMSA ~ netincome_delta2005, at=mc.op3.preddata, CIs=TRUE) + labs(title="Predicted levels of multicultural party support at different levels \nof change in net household income (∆t = 12), by level of TMSA \nmigration background")
#dev.off()
  
  
  
  
# Robustness check 1: alternative gentrification definition ----------------------------------------------------------------
  
# PVDA models
  
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
  
  # Export regression table
  wordreg(l = list(pvda.op2.m1, pvda.op2.m2), file = "pvda_gentr_8years.doc", 
          groups = list("Neighbourhood composition" = 2:10, "Gentrification" = 11:12, "Interaction effects" = 13:14))
  
  #TODO: remove
  ## Model 3: interaction effect
  #pvda.op2.m3 <- lm(PVDA ~ housing_pub + netHHincome
  #                  + imm_TMSA + imm_other +
  #                    + edu_low + edu_high + age_18t26 + age_66plus + unempl
  #                  + housing_pub_delta2009 + netincome_delta2009
  #                  + housing_pub_delta2009:imm_TMSA + netincome_delta2009:imm_TMSA, 
  #                  data = data.pvda.op2)
  #summary(pvda.op2.m3)  
  
  #TODO: remove
  ## Predicted probabilities plot: change in public housing
  #pvda.op2.preddata <- with(data.pvda.op2, list(imm_TMSA = seq(5,55,25), housing_pub_delta2009 = seq(-25,10,1), 
  #                                              housing_pub=mean(housing_pub),
  #                                              netHHincome=mean(netHHincome),
  #                                              imm_other  =mean(imm_other),
  #                                              edu_low    =mean(edu_low),
  #                                              edu_high   =mean(edu_high),
  #                                              age_18t26  =mean(age_18t26),
  #                                              age_66plus =mean(age_66plus),
  #                                              unempl     =mean(unempl),
  #                                              netincome_delta2009=mean(netincome_delta2009)))
  #jpeg("pvda_8years_pubhousing_plot.jpg", width = 500, height = 500)
  #emmip(pvda.op2.m3, imm_TMSA ~ housing_pub_delta2009, at=pvda.op2.preddata, CIs=TRUE) + labs(title="Predicted levels of PvdA support at different levels of change \nin public housing (∆t = 8), by level of TMSA migration background")
  #dev.off()
  #
  ## Predicted probabilities plot: change in net household income
  #pvda.op2.preddata <- with(data.pvda.op2, list(imm_TMSA = seq(5,55,25), netincome_delta2009 = seq(0,50,1), 
  #                                              housing_pub=mean(housing_pub),
  #                                              netHHincome=mean(netHHincome),
  #                                              imm_other  =mean(imm_other),
  #                                              edu_low    =mean(edu_low),
  #                                              edu_high   =mean(edu_high),
  #                                              age_18t26  =mean(age_18t26),
  #                                              age_66plus =mean(age_66plus),
  #                                              unempl     =mean(unempl),
  #                                              housing_pub_delta2009=mean(housing_pub_delta2009)))
  #jpeg("pvda_8years_netincome_plot.jpg", width = 500, height = 500) 
  #emmip(pvda.op2.m3, imm_TMSA ~ netincome_delta2009, at=pvda.op2.preddata, CIs=TRUE) + labs(title="Predicted levels of PvdA support at different levels of change \nin net household income (∆t = 8), by level of TMSA migration background")
  #dev.off()
  
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
  
  # Export regression table
  wordreg(l = list(pvda.op3.m1, pvda.op3.m2), file = "pvda_gentr_12years.doc", 
          groups = list("Neighbourhood composition" = 2:10, "Gentrification" = 11:12, "Interaction effects" = 13:14))
  
  
  #TODO: remove
  ## Model 3: interaction effect
  #pvda.op3.m3 <- lm(PVDA ~ housing_pub + netHHincome
  #                  + imm_TMSA + imm_other +
  #                    + edu_low + edu_high + age_18t26 + age_66plus + unempl
  #                  + housing_pub_delta2005 + netincome_delta2005
  #                  + housing_pub_delta2005:imm_TMSA + netincome_delta2005:imm_TMSA,
  #                  data = data.pvda.op3)
  #summary(pvda.op3.m3)
  
  ## Predicted probabilities plot: change in public housing
  #pvda.op3.preddata <- with(data.pvda.op3, list(imm_TMSA = seq(5,55,25), housing_pub_delta2005 = seq(-25,10,1), 
  #                                              housing_pub=mean(housing_pub),
  #                                              netHHincome=mean(netHHincome),
  #                                              imm_other  =mean(imm_other),
  #                                              edu_low    =mean(edu_low),
  #                                              edu_high   =mean(edu_high),
  #                                              age_18t26  =mean(age_18t26),
  #                                              age_66plus =mean(age_66plus),
  #                                              unempl     =mean(unempl),
  #                                              netincome_delta2005=mean(netincome_delta2005)))
  #jpeg("pvda_12years_pubhousing_plot.jpg", width = 500, height = 500)
  #emmip(pvda.op3.m3, imm_TMSA ~ housing_pub_delta2005, at=pvda.op3.preddata, CIs=TRUE) + labs(title="Predicted levels of PvdA support at different levels of change \nin public housing (∆t = 12), by level of TMSA migration background")
  #dev.off()
  #
  ## Predicted probabilities plot: change in net household income
  #pvda.op3.preddata <- with(data.pvda.op3, list(imm_TMSA = seq(5,55,25), netincome_delta2005 = seq(0,50,1), 
  #                                              housing_pub=mean(housing_pub),
  #                                              netHHincome=mean(netHHincome),
  #                                              imm_other  =mean(imm_other),
  #                                              edu_low    =mean(edu_low),
  #                                              edu_high   =mean(edu_high),
  #                                              age_18t26  =mean(age_18t26),
  #                                              age_66plus =mean(age_66plus),
  #                                              unempl     =mean(unempl),
  #                                              housing_pub_delta2005=mean(housing_pub_delta2005)))
  #jpeg("pvda_12years_netincome_plot.jpg", width = 500, height = 500)
  #emmip(pvda.op3.m3, imm_TMSA ~ netincome_delta2005, at=pvda.op3.preddata, CIs=TRUE) + labs(title="Predicted levels of PvdA support at different levels of change \nin net household income (∆t = 12), by level of TMSA migration background")
  #dev.off()
  
# Turnout models
  
  # OPTION 2 - Gentrification as change over 8 years (2009-2017)
  # Model 1: composition effects
  turn.op2.m1 <- lm(turnout ~ housing_pub + netHHincome
                    + imm_TMSA + imm_other +
                      + edu_low + edu_high + age_18t26 + age_66plus + unempl, 
                    data = data.turn.op2)
  summary(turn.op2.m1)
  
  # Model 2: add gentrification (change variables)
  turn.op2.m2 <- lm(turnout ~ housing_pub + netHHincome
                    + imm_TMSA + imm_other +
                      + edu_low + edu_high + age_18t26 + age_66plus + unempl
                    + housing_pub_delta2009 + netincome_delta2009, 
                    data = data.turn.op2)
  summary(turn.op2.m2)
  
  # Export regression table
  wordreg(l = list(turn.op2.m1, turn.op2.m2), file = "turnout_gentr_8years.doc", 
          groups = list("Neighbourhood composition" = 2:10, "Gentrification" = 11:12, "Interaction effects" = 13:14))
  
  #TODO: remove
  ## Model 3: interaction effect
  #turn.op2.m3 <- lm(turnout ~ housing_pub + netHHincome
  #                  + imm_TMSA + imm_other +
  #                    + edu_low + edu_high + age_18t26 + age_66plus + unempl
  #                  + housing_pub_delta2009 + netincome_delta2009
  #                  + housing_pub_delta2009:imm_TMSA + netincome_delta2009:imm_TMSA, 
  #                  data = data.turn.op2)
  #summary(turn.op2.m3)
  
  ## Predicted probabilities plot: change in public housing
  #turn.op2.preddata <- with(data.turn.op2, list(imm_TMSA = seq(5,55,25), housing_pub_delta2009 = seq(-25,10,1), 
  #                                              housing_pub=mean(housing_pub),
  #                                              netHHincome=mean(netHHincome),
  #                                              imm_other  =mean(imm_other),
  #                                              edu_low    =mean(edu_low),
  #                                              edu_high   =mean(edu_high),
  #                                              age_18t26  =mean(age_18t26),
  #                                              age_66plus =mean(age_66plus),
  #                                              unempl     =mean(unempl),
  #                                              netincome_delta2009=mean(netincome_delta2009)))
  #jpeg("turnout_8years_pubhousing_plot.jpg", width = 500, height = 500)
  #emmip(turn.op2.m3, imm_TMSA ~ housing_pub_delta2009, at=turn.op2.preddata, CIs=TRUE) + labs(title="Predicted levels of turnout at different levels of change in \npublic housing (∆t = 8), by level of TMSA migration background")
  #dev.off()
  #
  ## Predicted probabilities plot: change in net household income
  #turn.op2.preddata <- with(data.turn.op2, list(imm_TMSA = seq(5,55,25), netincome_delta2009 = seq(0,50,1), 
  #                                              housing_pub=mean(housing_pub),
  #                                              netHHincome=mean(netHHincome),
  #                                              imm_other  =mean(imm_other),
  #                                              edu_low    =mean(edu_low),
  #                                              edu_high   =mean(edu_high),
  #                                              age_18t26  =mean(age_18t26),
  #                                              age_66plus =mean(age_66plus),
  #                                              unempl     =mean(unempl),
  #                                              housing_pub_delta2009=mean(housing_pub_delta2009)))
  #jpeg("turnout_8years_netincome_plot.jpg", width = 500, height = 500)
  #emmip(turn.op2.m3, imm_TMSA ~ netincome_delta2009, at=turn.op2.preddata, CIs=TRUE) + labs(title="Predicted levels of turnout at different levels of change in \nnet household income (∆t = 8), by level of TMSA migration background")
  #dev.off()
  
  # OPTION 3 - Gentrification as change over 12 years (2005-2017)
  # Model 1: composition effects
  turn.op3.m1 <- lm(turnout ~ housing_pub + netHHincome
                    + imm_TMSA + imm_other +
                      + edu_low + edu_high + age_18t26 + age_66plus + unempl, 
                    data = data.turn.op3)
  summary(turn.op3.m1)
  
  # Model 2: add gentrification (change variables)
  turn.op3.m2 <- lm(turnout ~ housing_pub + netHHincome
                    + imm_TMSA + imm_other +
                      + edu_low + edu_high + age_18t26 + age_66plus + unempl
                    + housing_pub_delta2005 + netincome_delta2005, 
                    data = data.turn.op3)
  summary(turn.op3.m2)
  
  # Export regression table
  wordreg(l = list(turn.op3.m1, turn.op3.m2), file = "turnout_gentr_12years.doc", 
          groups = list("Neighbourhood composition" = 2:10, "Gentrification" = 11:12, "Interaction effects" = 13:14))
  
  #TODO: remove
  ## Model 3: interaction effect
  #turn.op3.m3 <- lm(turnout ~ housing_pub + netHHincome
  #                  + imm_TMSA + imm_other +
  #                    + edu_low + edu_high + age_18t26 + age_66plus + unempl
  #                  + housing_pub_delta2005 + netincome_delta2005
  #                  + housing_pub_delta2005:imm_TMSA + netincome_delta2005:imm_TMSA, 
  #                  data = data.turn.op3)
  #summary(turn.op3.m3)
  
  ## Predicted probabilities plot: change in public housing
  #turn.op3.preddata <- with(data.turn.op3, list(imm_TMSA = seq(5,55,25), housing_pub_delta2005 = seq(-25,10,1), 
  #                                              housing_pub=mean(housing_pub),
  #                                              netHHincome=mean(netHHincome),
  #                                              imm_other  =mean(imm_other),
  #                                              edu_low    =mean(edu_low),
  #                                              edu_high   =mean(edu_high),
  #                                              age_18t26  =mean(age_18t26),
  #                                              age_66plus =mean(age_66plus),
  #                                              unempl     =mean(unempl),
  #                                              netincome_delta2005=mean(netincome_delta2005)))
  #jpeg("turnout_12years_pubhousing_plot.jpg", width = 500, height = 500)
  #emmip(turn.op3.m3, imm_TMSA ~ housing_pub_delta2005, at=turn.op3.preddata, CIs=TRUE) + labs(title="Predicted levels of turnout at different levels of change in \npublic housing (∆t = 12), by level of TMSA migration background")
  #dev.off()
  
  ## Predicted probabilities plot: change in net household income
  #turn.op3.preddata <- with(data.turn.op3, list(imm_TMSA = seq(5,55,25), netincome_delta2005 = seq(1,50,1), 
  #                                              housing_pub=mean(housing_pub),
  #                                              netHHincome=mean(netHHincome),
  #                                              imm_other  =mean(imm_other),
  #                                              edu_low    =mean(edu_low),
  #                                              edu_high   =mean(edu_high),
  #                                              age_18t26  =mean(age_18t26),
  #                                              age_66plus =mean(age_66plus),
  #                                              unempl     =mean(unempl),
  #                                              housing_pub_delta2005=mean(housing_pub_delta2005)))
  #jpeg("turnout_12years_netincome_plot.jpg", width = 500, height = 500)
  #emmip(turn.op3.m3, imm_TMSA ~ netincome_delta2005, at=turn.op3.preddata, CIs=TRUE) + labs(title="Predicted levels of turnout at different levels of change in \nnet household income (∆t = 12), by level of TMSA migration background")
  #dev.off()
  
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
  


  
 
  
  
