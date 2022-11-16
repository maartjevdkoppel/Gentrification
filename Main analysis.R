# Perils of “revitalization”? 
# Gentrification, political participation, and support for social democrats in Amsterdam 

# Maartje van de Koppel
# Data analysis
# Last update: 11/11/22

#TODO: update title and last updated date

# Set-up ------------------------------------------------------------------------------------------------------------ 

# Set up working directory
# TODO remove
setwd("/Users/Maartje/Desktop/LJA/Paper politicologenetmaal/Link-Jong-Amsterdam/Data/Analysis")

# TODO: select only libraries used
# TODO: add explanations of use

#NOTE: give permission to maptools to use the gpclib package:
  # check status with gpclibPermitStatus()
  # grant permission with gpclibPermit()
#TODO: remove all old mapmaking packages, also new because moved to another script?

library(foreign) 
library(tidyverse)    #tidyverse collection
library(ggplot2)
library(readxl)
library(dplyr)
library(stats)
library(tidyr)       
library(stargazer)    #exporting regression tables
library(car)          #calculating variance inflation factors
library(vtable)       #descriptive statistics table
library(expss)        #variable labelling functions
#library(rempsyc)     #t-test tables
library(broom)        #cleaning t-test results
library(purrr)        #cleaning t-test results

# Import data -------------------------------------------------------------------------------------------------------

# Read data
fulldata <- readRDS("gentrification_data_long_revised.rds")
  
# Prepare data for analysis ----------------------------------------------------------------------------------------- 
  
# Subset to 2018 only
subdata <- fulldata[which(fulldata$year=='2018'), ]
  
#Change in net income: turn into percentual change
subdata <- subdata %>% mutate(netincome_delta2013 = (netincome_delta2013 / (netHHincome - netincome_delta2013))*100,
                              netincome_delta2009 = (netincome_delta2009 / (netHHincome - netincome_delta2009))*100,
                              netincome_delta2005 = (netincome_delta2005 / (netHHincome - netincome_delta2005))*100)

# Redefine categories for migration background
# Turkish, Moroccan, Surinamese & Antillean vs. other migration vs. no migration background
subdata$imm_TMSA  <- subdata$imm_Tur + subdata$imm_Mar + subdata$imm_Sur + subdata$imm_Ant
subdata$imm_other <- subdata$imm_otherNW + subdata$imm_W

var_lab(subdata$imm_TMSA)  = "% Turkish, Moroccan, Surinamese or Antillean migration background"
var_lab(subdata$imm_other) = "% Other migration background"

#Check if neighbourhoods with missing data on social housing are significantly different
#Create variable to identify observations with missing values on change in social housing
subdata$missing_housing_soc <- ifelse(is.na(subdata$housing_soc_delta2013), 1, 0)

#Perform t-tests
t1  <- t.test(PVDA                  ~ missing_housing_soc, data = subdata, alternative = "two.sided")
t2  <- t.test(turnout               ~ missing_housing_soc, data = subdata, alternative = "two.sided")
t3  <- t.test(housing_pub_delta2013 ~ missing_housing_soc, data = subdata, alternative = "two.sided") 
t4  <- t.test(netincome_delta2013   ~ missing_housing_soc, data = subdata, alternative = "two.sided") 
t5  <- t.test(housing_pub           ~ missing_housing_soc, data = subdata, alternative = "two.sided") 
t6  <- t.test(netHHincome           ~ missing_housing_soc, data = subdata, alternative = "two.sided") 
t7  <- t.test(imm_TMSA              ~ missing_housing_soc, data = subdata, alternative = "two.sided")
t8  <- t.test(imm_other             ~ missing_housing_soc, data = subdata, alternative = "two.sided")
t9  <- t.test(age_18t26             ~ missing_housing_soc, data = subdata, alternative = "two.sided") 
t10 <- t.test(age_66plus            ~ missing_housing_soc, data = subdata, alternative = "two.sided") 
t11 <- t.test(edu_low               ~ missing_housing_soc, data = subdata, alternative = "two.sided")
t12 <- t.test(edu_high              ~ missing_housing_soc, data = subdata, alternative = "two.sided")
t13 <- t.test(unempl                ~ missing_housing_soc, data = subdata, alternative = "two.sided") 

#List dependent variables
var.names = c("PvdA vote share", "Turnout", 
              "Change % public housing", "Change in avergae net income",
              "% public housing", "Average net income",
              "%Turkish, Moroccan, Surinamese, Antillean migration background", "% other migration background",
              "% youth (18-26)", "% elderly (66+)", "% lower educated", "% higher educated", "% unemployed")

#Gather all t-test results 
t.tests <- map_df(list(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13), tidy)

#Clean up table
t.tests.table <- t.tests %>% 
  mutate(variable = var.names,
         p.value = ifelse(p.value < 0.001, "<.001", round(p.value, digits = 3)),
         statistic = round(statistic, digits = 2),
         parameter = round(parameter, digits = 2),
         conf = paste0("[", round(conf.low, digits = 2), ", ", round(conf.high, digits = 2), "]")) %>%
  select(c(variable, statistic, parameter, p.value, conf))

  
# Check if manually combined neighbourhoods are significantly different
#Perform t-tests
t14 <- t.test(PVDA                  ~ bc_combined, data = subdata, alternative = "two.sided")
t15 <- t.test(turnout               ~ bc_combined, data = subdata, alternative = "two.sided")
t16 <- t.test(housing_pub_delta2013 ~ bc_combined, data = subdata, alternative = "two.sided") 
t17 <- t.test(netincome_delta2013   ~ bc_combined, data = subdata, alternative = "two.sided") 
t18 <- t.test(housing_pub           ~ bc_combined, data = subdata, alternative = "two.sided") 
t19 <- t.test(netHHincome           ~ bc_combined, data = subdata, alternative = "two.sided") 
t20 <- t.test(imm_TMSA              ~ bc_combined, data = subdata, alternative = "two.sided")
t21 <- t.test(imm_other             ~ bc_combined, data = subdata, alternative = "two.sided")
t22 <- t.test(age_18t26             ~ bc_combined, data = subdata, alternative = "two.sided") 
t23 <- t.test(age_66plus            ~ bc_combined, data = subdata, alternative = "two.sided") 
t24 <- t.test(edu_low               ~ bc_combined, data = subdata, alternative = "two.sided")
t25 <- t.test(edu_high              ~ bc_combined, data = subdata, alternative = "two.sided")
t26 <- t.test(unempl                ~ bc_combined, data = subdata, alternative = "two.sided") 

#Gather all t-test results 
t.tests.combined <- map_df(list(t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26), tidy)

#Clean up table
t.tests.table.combined <- t.tests.combined %>% 
  mutate(variable = var.names,
         p.value = ifelse(p.value < 0.001, "<.001", round(p.value, digits = 3)),
         statistic = round(statistic, digits = 2),
         parameter = round(parameter, digits = 2),
         conf = paste0("[", round(conf.low, digits = 2), ", ", round(conf.high, digits = 2), "]")) %>%
  select(c(variable, statistic, parameter, p.value, conf))


# Create separate databases to maximise number of observations for each model given missing values
  
# PVDA option 1: gentrification over 4 years
data.pvda.op1 <- select(subdata, c(bc_code, bc_name, PVDA, housing_pub, netHHincome, imm_TMSA, imm_other,
                                   edu_low, edu_high, age_18t26, age_66plus, unempl,
                                   housing_pub_delta2013, netincome_delta2013))
data.pvda.op1 <- data.pvda.op1[complete.cases(data.pvda.op1),] # 1 observation deleted

# PVDA option 2: gentrification over 8 and 12 years
data.pvda.op2 <- select(subdata, c(bc_code, bc_name, PVDA, housing_pub, netHHincome, imm_TMSA, imm_other,
                                   edu_low, edu_high, age_18t26, age_66plus, unempl,
                                   housing_pub_delta2009, netincome_delta2009, housing_pub_delta2005, netincome_delta2005))
data.pvda.op2 <- data.pvda.op2[complete.cases(data.pvda.op2),] # 1 observation deleted

# Turnout option 1: gentrification over 4 years
data.turn.op1 <- select(subdata, c(bc_code, bc_name, turnout, housing_pub, netHHincome, imm_TMSA, imm_other,
                                 edu_low, edu_high, age_18t26, age_66plus, unempl,
                                 housing_pub_delta2013, netincome_delta2013))
data.turn.op1 <- data.turn.op1[complete.cases(data.turn.op1),] # 1 observation deleted

# Turnout option 2: gentrification over 8 and 12 years
data.turn.op2 <- select(subdata, c(bc_code, bc_name, turnout, housing_pub, netHHincome, imm_TMSA, imm_other,
                                   edu_low, edu_high, age_18t26, age_66plus, unempl,
                                   housing_pub_delta2009, netincome_delta2009, housing_pub_delta2005, netincome_delta2005))
data.turn.op2 <- data.turn.op2[complete.cases(data.turn.op2),] # 1 observation deleted

# Drop outliers
    
# Drop one observation with very great change in public housing
data.pvda.op1 <- data.pvda.op1 %>% filter(housing_pub_delta2013 > -20)

# Drop one observation with turnout over 100%
data.turn.op1 <- data.turn.op1 %>% filter(turnout < 100 & housing_pub_delta2013 > -20)
  
# Descriptive statistics ----------------------------------------------------------------------
  
#Remove variables labels for sumtable()  
table_data <- expss::unvr(subdata)

#Create and export descriptives table
sumtable(table_data,
         vars = c("turnout", "PVDA",
                  "housing_pub_delta2013", "netincome_delta2013",
                  "housing_pub", "netHHincome",
                  "imm_TMSA", "imm_other", "age_18t26", "age_66plus",
                  "unempl", "edu_high", "edu_low"),
         labels = c("Turnout", "PvdA vote share",
                    "Change in % public housing", "Change in average net income per household",
                    "% public housing", "Average net income per household",
                    "% Turkish, Moroccan, Surinamese or Antillean background",
                    "% other migration background",
                    "% youth (18-26)", "% elderly (66+)", "% unemployed inhabitants",
                    "% lower educated", "% higher educated"),
         summ = c("mean(x)", "sd(x)", "min(x)", "max(x)"),
         title = "Descriptive statistics",
         note = "Data: O&S Amsterdam, own adaptation.",
         digits = 3,
         out = "browser"
         )
  
# Analysis: PVDA ------------------------------------------------------------------------------------------------- 

# OPTION 1 - Gentrification as change over 4 years (2013-2017)
# Model 1: composition effects
pvda.op1.m1 <- lm(PVDA ~ housing_pub + netHHincome + imm_TMSA + imm_other +
                  edu_low + edu_high + age_18t26 + age_66plus + unempl, 
                  data = data.pvda.op1)
summary(pvda.op1.m1)

# Model 2: add gentrification
pvda.op1.m2 <- lm(PVDA ~ housing_pub + netHHincome + imm_TMSA + imm_other +
                 edu_low + edu_high + age_18t26 + age_66plus + unempl +
                 housing_pub_delta2013 + netincome_delta2013, 
                 data = data.pvda.op1)
summary(pvda.op1.m2)

# Export regression table
stargazer(pvda.op1.m1, pvda.op1.m2,
  title = "OLS regression model for PvdA support in the 2018 Amsterdam municipal election",
  #TODO: may need to place footnote on exact def of other african etc.
  covariate.labels = c("% public housing", "Average net income", "% Turkish, Moroccan, Surinamese, Antillean migration background",
            "% other migration background", "% lower educated",
            "% higher educated", "% youth (18-26)", "% elderly (66+)", "% unemployed", 
            "Change in  % public housing", "Change in average net income"),
  dep.var.labels = "PvdA vote share",
  column.labels = c("Model 1", "Model 2"),
  model.numbers = FALSE,
  star.cutoffs = c(0.05, 0.01, 0.001),
  omit.stat = c("f", "ser"),
  single.row = TRUE,
  out = "pvda_gentr_4years.html")

# Effect size
# in standard deviations
pvda.op1.m2$coefficients[["housing_pub_delta2013"]]*sd(data.pvda.op1$housing_pub_delta2013)/sd(data.pvda.op1$PVDA)
# compare extreme ends in data
diff(range(data.pvda.op1$housing_pub_delta2013))*pvda.op1.m2$coefficients[["housing_pub_delta2013"]]

# in standard deviations
pvda.op1.m2$coefficients[["netincome_delta2013"]]*sd(data.pvda.op1$netincome_delta2013)/sd(data.pvda.op1$PVDA)
# compare extreme ends in data
diff(range(data.pvda.op1$netincome_delta2013))*pvda.op1.m2$coefficients[["netincome_delta2013"]]
  
  
# Analysis: turnout ------------------------------------------------------------------------------------------------- 
  
# OPTION 1 - Gentrification as change over 4 years (2013-2017)
# Model 1: composition effects
turn.op1.m1 <- lm(turnout ~ housing_pub + netHHincome + imm_TMSA + imm_other +
                edu_low + edu_high + age_18t26 + age_66plus + unempl, 
                data = data.turn.op1)
summary(turn.op1.m1)

# Model 2: add gentrification (change variables)
turn.op1.m2 <- lm(turnout ~ housing_pub + netHHincome + imm_TMSA + imm_other +
                  edu_low + edu_high + age_18t26 + age_66plus + unempl +
                  housing_pub_delta2013 + netincome_delta2013, 
                  data = data.turn.op1)
summary(turn.op1.m2)
  
# Export regression table
stargazer(turn.op1.m1, turn.op1.m2,
  title = "OLS regression model for turnout in the 2018 Amsterdam municipal election",
  #TODO: may need to place footnote on exact def of other african etc.
  covariate.labels = c("% public housing", "Average net income", "% Turkish, Moroccan, Surinamese, Antillean migration background",
                       "% other migration background", "% lower educated",
                       "% higher educated", "% youth (18-26)", "% elderly (66+)", "% unemployed", 
                       "Change in  % public housing", "Change in average net income"),
  dep.var.labels = "Turnout",
  column.labels = c("Model 1", "Model 2"),
  model.numbers = FALSE,
  star.cutoffs = c(0.05, 0.01, 0.001),
  omit.stat = c("f", "ser"),
  single.row = TRUE,
  out = "turnout_gentr_4years.html")
  
# Effect size
# in standard deviations
turn.op1.m2$coefficients[["housing_pub_delta2013"]]*sd(data.turn.op1$housing_pub_delta2013)/sd(data.turn.op1$turnout)
# compare extreme ends in data
diff(range(data.turn.op1$housing_pub_delta2013))*turn.op1.m2$coefficients[["housing_pub_delta2013"]]

# in standard deviations
turn.op1.m2$coefficients[["netincome_delta2013"]]*sd(data.turn.op1$netincome_delta2013)/sd(data.turn.op1$turnout)
# compare extreme ends in data
diff(range(data.turn.op1$netincome_delta2013))*turn.op1.m2$coefficients[["netincome_delta2013"]]

# Robustness check 1: alternative time periods for gentrification ----------------------------------------------------------------
  
# PVDA models
  
# Model 1: composition effects
pvda.op2.m1 <- lm(PVDA ~ housing_pub + netHHincome + imm_TMSA + imm_other +
                  edu_low + edu_high + age_18t26 + age_66plus + unempl, 
                  data = data.pvda.op2)
summary(pvda.op2.m1)

# Model 2: add gentrification as change over 8 years (2009-2017)
pvda.op2.m2 <- lm(PVDA ~ housing_pub + netHHincome + imm_TMSA + imm_other +
                  edu_low + edu_high + age_18t26 + age_66plus + unempl + 
                  housing_pub_delta2009 + netincome_delta2009, 
                  data = data.pvda.op2)
summary(pvda.op2.m2)

# Model 3: add gentrification as change over 12 years (2005-2017)
pvda.op3.m2 <- lm(PVDA ~ housing_pub + netHHincome + imm_TMSA + imm_other +
                  edu_low + edu_high + age_18t26 + age_66plus + unempl + 
                  housing_pub_delta2005 + netincome_delta2005, 
                  data = data.pvda.op2)
summary(pvda.op3.m2)
  
#Export regression table
stargazer(pvda.op2.m1, pvda.op2.m2, pvda.op3.m2, 
    title = "OLS regression model for PvdA support with gentrification defined as 8-year and 12-year change",
    #TODO: may need to place footnote on exact def of other african etc.
    covariate.labels = c("% public housing", "Average net income",
                         "% Turkish, Moroccan, Surinamese, Antillean migration background",
                         "% other migration background", "% lower educated",
                         "% higher educated", "% youth (18-26)", "% elderly (66+)", "% unemployed", 
                         "Change in % public housing (2009-2017)", "Change in average net income (2009-2017)", 
                         "Change in % public housing (2005-2017)", "Change in average net income (2005-2017)"),
    dep.var.labels = c("PvdA vote share"),
    column.labels = c("Model 1", "Model 2","Model 3"),
    model.numbers = FALSE,
    star.cutoffs = c(0.05, 0.01, 0.001),
    omit.stat = c("f", "ser"),
    single.row = TRUE,
    out = "pvda_gentr_8+12years.html")
  
  
# Turnout models
  
# Model 1: composition effects
turn.op2.m1 <- lm(turnout ~ housing_pub + netHHincome + imm_TMSA + imm_other +
                  edu_low + edu_high + age_18t26 + age_66plus + unempl, 
                  data = data.turn.op2)
summary(turn.op2.m1)

# Model 2: add gentrification as change over 8 years (2009-2017)
turn.op2.m2 <- lm(turnout ~ housing_pub + netHHincome + imm_TMSA + imm_other +
                  edu_low + edu_high + age_18t26 + age_66plus + unempl +
                  housing_pub_delta2009 + netincome_delta2009, 
                  data = data.turn.op2)
summary(turn.op2.m2)

# Model 3: add gentrification as change over 12 years (2005-2017)
turn.op3.m2 <- lm(turnout ~ housing_pub + netHHincome + imm_TMSA + imm_other +
                  edu_low + edu_high + age_18t26 + age_66plus + unempl + 
                  housing_pub_delta2005 + netincome_delta2005, 
                  data = data.turn.op2)
summary(turn.op3.m2)
  
# Export regression table
stargazer(turn.op2.m1, turn.op2.m2, turn.op3.m2, 
          title = "OLS regression model for turnout with gentrification defined as 8-year and 12-year change",
          #TODO: may need to place footnote on exact def of other african etc.
          covariate.labels = c("% public housing", "Average net income",
                               "% Turkish, Moroccan, Surinamese, Antillean migration background",
                               "% other migration background", "% lower educated",
                               "% higher educated", "% youth (18-26)", "% elderly (66+)", "% unemployed", 
                               "Change in % public housing (2009-2017)", "Change in average net income (2009-2017)", 
                               "Change in % public housing (2005-2017)", "Change in average net income (2005-2017)"),
          dep.var.labels = c("Turnout"),
          column.labels = c("Model 1", "Model 2","Model 3"),
          model.numbers = FALSE,
          star.cutoffs = c(0.05, 0.01, 0.001),
          omit.stat = c("f", "ser"),
          single.row = TRUE,
          out = "turnout_gentr_8+12years.html")
 
# Robustness check 2: analysis including outliers -----------------------------------------------------------------------------

# OUTLIER IN PUBLIC HOUSING CHANGE

# Prepare data
data.pvda.op1.outliers <- select(subdata, c(bc_code, bc_name, PVDA, housing_pub, netHHincome, imm_TMSA, imm_other,
                                 edu_low, edu_high, age_18t26, age_66plus, unempl,
                                 housing_pub_delta2013, netincome_delta2013))
data.pvda.op1.outliers <- data.pvda.op1.outliers[complete.cases(data.pvda.op1.outliers),]

# Model 1: composition effects
pvda.op1.m1.outliers <- lm(PVDA ~ housing_pub + netHHincome + imm_TMSA + imm_other +
                           edu_low + edu_high + age_18t26 + age_66plus + unempl, 
                  data = data.pvda.op1.outliers)
summary(pvda.op1.m1.outliers)

# Model 2: add gentrification
pvda.op1.m2.outliers <- lm(PVDA ~ housing_pub + netHHincome + imm_TMSA + imm_other +
                           edu_low + edu_high + age_18t26 + age_66plus + unempl + 
                           housing_pub_delta2013 + netincome_delta2013, 
                  data = data.pvda.op1.outliers)
summary(pvda.op1.m2.outliers)

# Export regression table
stargazer(pvda.op1.m1.outliers, pvda.op1.m2.outliers,
  title = "OLS regression model for PvdA support in the 2018 Amsterdam municipal election, \\textit{including outliers}",
  #TODO: may need to place footnote on exact def of other african etc.
  covariate.labels = c("% public housing","Average net income", "% Turkish, Moroccan, Surinamese, Antillean migration background",
                       "% other migration background", "% lower educated",
                       "% higher educated", "% youth (18-26)", "% elderly (66+)", "% unemployed", 
                       "Change in  % public housing", "Change in average net income"),
  dep.var.labels = "PvdA vote share",
  column.labels = c("Model 1", "Model 2"),
  model.numbers = FALSE,
  star.cutoffs = c(0.05, 0.01, 0.001),
  omit.stat = c("f", "ser"),
  single.row = TRUE, 
  out = "pvda_outliers.html")


# OUTLIER IN TURNOUT 

#Prepare data
data.turn.op1.outliers <- select(subdata, c(bc_code, bc_name, turnout, housing_pub, netHHincome, imm_TMSA, imm_other,
                                   edu_low, edu_high, age_18t26, age_66plus, unempl,
                                   housing_pub_delta2013, netincome_delta2013))
data.turn.op1.outliers <- data.turn.op1.outliers[complete.cases(data.turn.op1.outliers),]

# Model 1: composition effects
turn.op1.m1.outliers <- lm(turnout ~ housing_pub + netHHincome + imm_TMSA + imm_other +
                           edu_low + edu_high + age_18t26 + age_66plus + unempl, 
                           data = data.turn.op1.outliers)
summary(turn.op1.m1.outliers)

# Model 2: add gentrification
turn.op1.m2.outliers <- lm(turnout ~ housing_pub + netHHincome + imm_TMSA + imm_other +
                           edu_low + edu_high + age_18t26 + age_66plus + unempl +
                           housing_pub_delta2013 + netincome_delta2013, 
                           data = data.turn.op1.outliers)
summary(turn.op1.m2.outliers)

# Export regression table 
stargazer(turn.op1.m1.outliers, turn.op1.m2.outliers,
    title = "OLS regression model for turnout in the 2018 Amsterdam municipal election, \\textit{including outliers}",
    #TODO: may need to place footnote on exact def of other african etc.
    covariate.labels = c("% public housing", "Average net income", "% Turkish, Moroccan, Surinamese, Antillean migration background",
                         "% other migration background", "% lower educated",
                         "% higher educated", "% youth (18-26)", "% elderly (66+)", "% unemployed", 
                         "Change in  % public housing", "Change in average net income"),
    dep.var.labels = "Turnout",
    column.labels = c("Model 1", "Model 2"),
    model.numbers = FALSE,
    star.cutoffs = c(0.05, 0.01, 0.001),
    omit.stat = c("f", "ser"),
    single.row = TRUE,
    out = "turnout_outliers.html")

# Robustness check 3: alternative gentrification indicator -------------------------------------------------------------------------

#PVDA
#Subset data 
data.pvda.sochousing <- select(subdata, c(bc_code, bc_name, PVDA, housing_pub, housing_soc, netHHincome, imm_TMSA, imm_other,
                                          edu_low, edu_high, age_18t26, age_66plus, unempl,
                                          housing_pub_delta2013, netincome_delta2013, housing_soc_delta2013))
data.pvda.sochousing <- data.pvda.sochousing[complete.cases(data.pvda.sochousing),] #17 observations with missing values deleted 

#Model 1: composition effects
pvda.sochousing.m1 <- lm(PVDA ~ housing_pub + netHHincome + housing_soc + imm_TMSA + imm_other +
                         edu_low + edu_high + age_18t26 + age_66plus + unempl, 
                         data = data.pvda.sochousing)
summary(pvda.sochousing.m1)

#Model 2: add gentrification
pvda.sochousing.m2 <- lm(PVDA ~ housing_pub + netHHincome + housing_soc + imm_TMSA + imm_other +
                         edu_low + edu_high + age_18t26 + age_66plus + unempl +
                         housing_pub_delta2013 + netincome_delta2013 + housing_soc_delta2013, 
                         data = data.pvda.sochousing)
summary(pvda.sochousing.m2)

#Export regression table 
stargazer(pvda.sochousing.m1, pvda.sochousing.m2,
          title = "OLS regression model for PvdA support in the 2018 Amsterdam municipal election, with additional gentrification indicator",
          #TODO: consider if title can be shorter and better at explaining
          #TODO: may need to place footnote on exact def of other african etc.
          covariate.labels = c("% public housing", "Average net income", "% social housing",
                               "% Turkish, Moroccan, Surinamese, Antillean migration background",
                               "% other migration background", "% lower educated",
                               "% higher educated", "% youth (18-26)", "% elderly (66+)", "% unemployed", 
                               "Change in  % public housing", "Change in average net income", "Change in % social housing"),
          dep.var.labels = "PvdA vote share",
          column.labels = c("Model 1", "Model 2"),
          model.numbers = FALSE,
          star.cutoffs = c(0.05, 0.01, 0.001),
          omit.stat = c("f", "ser"),
          single.row = TRUE,
          out = "pvda_social_housing.html")

# Effect size
# in standard deviations
pvda.sochousing.m2$coefficients[["housing_soc_delta2013"]]*sd(data.pvda.sochousing$housing_soc_delta2013)/sd(data.pvda.sochousing$PVDA)
# compare extreme ends in data
diff(range(data.pvda.sochousing$housing_soc_delta2013))*pvda.sochousing.m2$coefficients[["housing_soc_delta2013"]]


#Turnout
#Subset data 
data.turnout.sochousing <- select(subdata, c(bc_code, bc_name, turnout, housing_pub, housing_soc, netHHincome, imm_TMSA, imm_other,
                                             edu_low, edu_high, age_18t26, age_66plus, unempl,
                                             housing_pub_delta2013, netincome_delta2013, housing_soc_delta2013))
data.turnout.sochousing <- data.turnout.sochousing[complete.cases(data.turnout.sochousing),] #17 observations with missing values deleted 

#Model 1: composition effects
turnout.sochousing.m1 <- lm(turnout ~ housing_pub + netHHincome + housing_soc + imm_TMSA + imm_other +
                           edu_low + edu_high + age_18t26 + age_66plus + unempl, 
                         data = data.turnout.sochousing)
summary(turnout.sochousing.m1)

#Model 2: add gentrification
turnout.sochousing.m2 <- lm(turnout ~ housing_pub + netHHincome + housing_soc + imm_TMSA + imm_other +
                            edu_low + edu_high + age_18t26 + age_66plus + unempl +
                            housing_pub_delta2013 + netincome_delta2013 + housing_soc_delta2013, 
                            data = data.turnout.sochousing)
summary(turnout.sochousing.m2)

#Export regression table 
stargazer(turnout.sochousing.m1, turnout.sochousing.m2,
          title = "OLS regression model for turnout in the 2018 Amsterdam municipal election, with additional gentrification indicator",
          #TODO: consider if title can be shorter and better at explaining
          #TODO: may need to place footnote on exact def of other african etc.
          covariate.labels = c("% public housing", "Average net income", "% social housing",
                               "% Turkish, Moroccan, Surinamese, Antillean migration background",
                               "% other migration background", "% lower educated",
                               "% higher educated", "% youth (18-26)", "% elderly (66+)", "% unemployed", 
                               "Change in  % public housing", "Change in average net income", "Change in % social housing"),
          dep.var.labels = "Turnout",
          column.labels = c("Model 1", "Model 2"),
          model.numbers = FALSE,
          star.cutoffs = c(0.05, 0.01, 0.001),
          omit.stat = c("f", "ser"),
          single.row = TRUE,
          out = "turnout_social_housing.html")

# Effect size
# in standard deviations
turnout.sochousing.m2$coefficients[["housing_soc_delta2013"]]*sd(data.turnout.sochousing$housing_soc_delta2013)/sd(data.turnout.sochousing$turnout)
# compare extreme ends in data
diff(range(data.turnout.sochousing$housing_soc_delta2013))*turnout.sochousing.m2$coefficients[["housing_soc_delta2013"]]


# Check OLS regression assumptions ------------------------------------------------------------------------------------------------- 


#TODO: sort out this code, maybe remove completely? if not reported in paper

#PVDA MODELS
#PvdA support and change in public housing
ggplot(data = data.pvda.op1,
       aes(y = PVDA, x = housing_pub_delta2013)) +
  geom_point()

#PvdA support and change in net household income
ggplot(data = data.pvda.op1,
       aes(y = PVDA, x = netincome_delta2013)) +
  geom_point()

#excluding observations with high values on net income
data.pvda.op1 %>%
  filter(netincome_delta2013 < 15) %>%
  ggplot(aes(y = PVDA, x = netincome_delta2013)) +
  geom_point()


#TURNOUT MODELS
#Turnout and change in public housing
ggplot(data = data.turn.op1,
       aes(y = turnout, x = housing_pub_delta2013)) +
  geom_point()

#Turnout and change in net household income
ggplot(data = data.turn.op1,
       aes(y = turnout, x = netincome_delta2013)) +
  geom_point()




#General diagnostic plots
#TODO: keep?
pvda.diag.metrics <- augment(pvda.op1.m2)
head(pvda.diag.metrics)

#plot residuals versus fitted
ggplot(pvda.diag.metrics, aes(housing_pub_delta2013, PVDA)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = housing_pub_delta2013, yend = .fitted), color = "red", size = 0.3)
ggsave("pvda_rvf.png")


#diagnostic plots wth base r
jpeg("pvda_diagnostic plots.jpg", width = 750, height = 750)
par(mfrow = c(2, 2))
plot(pvda.op1.m2)
dev.off()

jpeg("turnout_diagnostic plots.jpg", width = 750, height = 750)
par(mfrow = c(2, 2))
plot(turn.op1.m2)
dev.off()

# Multicollinearity ------------------------------------------------------------------

#VIF scores
vif(pvda.op1.m2)
vif(turn.op1.m2)

#TODO: remove code?
#Check correlations of independent variables
iv <- select(subdata, housing_pub, netHHincome, imm_TMSA, imm_other, 
             edu_low, edu_high, age_18t26, age_66plus, unempl, housing_pub_delta2013, netincome_delta2013)

cor.matrix <- cor(iv, use = "complete.obs")

#Correlation plot
library(corrplot)
png("correlation_plot.png", width = 750, height = 750)
corrplot(cor.matrix, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
dev.off()


#change in social housing var check
iv_sochousing <- select(subdata, housing_pub, housing_soc, netHHincome, imm_TMSA, imm_other, 
             edu_low, edu_high, age_18t26, age_66plus, unempl, housing_pub_delta2013, netincome_delta2013,
             housing_soc_delta2013)

cor.matrix_sochousing <- cor(subdata, use = "complete.obs")

library(corrplot)
png("correlation_plot_socialhousing.png", width = 750, height = 750)
corrplot(cor.matrix_sochousing, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
dev.off()

#Check correlation of main predictors: gentrification variables
cor(subdata$netincome_delta2013, subdata$housing_pub_delta2013)
