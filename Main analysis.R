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
library(stargazer)    #exporting regression tables
library(car)          #calculating variance inflation factors
library(vtable)       #descriptive statistics table
library(expss)        #variable labelling functions
library(rempsyc)

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
  #TODO: remove this from script once done?
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
  subdata$missing_housing_soc <- ifelse(is.na(subdata$housing_soc_delta2013), 1, 0)
  fre(subdata$missing_housing_soc)
  
  t.test.results <- nice_t_test(data = subdata,
                      response = c("turnout", "PVDA", "housing_pub_delta2013", "netincome_delta2013",
                                   "housing_pub", "netHHincome", "imm_TMSA", "imm_other",
                                   "age_18t26", "age_66plus", "unempl", "edu_low", "edu_high"),
                      group = "missing_housing_soc",
                      warning = TRUE)
  
  #Export t-test table
  t.test_table <- nice_table(t.test.results)
  
  
  
# Check if manually combined neighbourhoods are significantly different
  # On the dependent variables 
  t.test(PVDA      ~ bc_combined, data = subdata, alternative = "two.sided")
  t.test(MCparties ~ bc_combined, data = subdata, alternative = "two.sided")
  t.test(turnout   ~ bc_combined, data = subdata, alternative = "two.sided")
  
  # On the independent variables
  t.test(imm_TMSA              ~ bc_combined, data = subdata, alternative = "two.sided")
  t.test(netincome_delta2005   ~ bc_combined, data = subdata, alternative = "two.sided") 
  t.test(netincome_delta2009   ~ bc_combined, data = subdata, alternative = "two.sided") 
  t.test(netincome_delta2013   ~ bc_combined, data = subdata, alternative = "two.sided") 
  t.test(housing_pub_delta2005 ~ bc_combined, data = subdata, alternative = "two.sided") 
  t.test(housing_pub_delta2009 ~ bc_combined, data = subdata, alternative = "two.sided")
  t.test(housing_pub_delta2013 ~ bc_combined, data = subdata, alternative = "two.sided")   
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
#TODO: is this needed?
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

# Drop outliers
    
  # Drop one observation with very great change in public housing
  data.pvda.op1 <- data.pvda.op1 %>% filter(housing_pub_delta2013 > -20)
  
  # Also drop one observation with turnout over 100%
  data.turn.op1 <- data.turn.op1 %>% filter(turnout < 100 & housing_pub_delta2013 > -20)
  
# Descriptive statistics ----------------------------------------------------------------------
  
  table_data <- expss::unvr(subdata)
  
  sumtable(table_data,
           vars = c("turnout", "PVDA",
                    "housing_pub_delta2013", "netincome_delta2013",
                    "housing_pub", "netHHincome",
                    "imm_TMSA", "imm_other", "age_18t26", "age_66plus",
                    "unempl", "edu_high", "edu_low"),
           labels = c("Turnout", "PvdA vote share",
                      "Change in % public housing", "Change in average net income per household",
                      "% public housing", "Average net income per household",
                      "% inhabitants with Turkish, Moroccan, Surinamese or Antillean background",
                      "% inhabitants with (other) African, Latin American or Asian background",
                      "% youth (18-26)", "% elderly (66+)", "% unemployed inhabitants",
                      "% inhabitants with lower education", "% inhabitants with higher education"),
           summ = c("mean(x)", "sd(x)", "min(x)", "max(x)"),
           title = "Descriptive statistics",
           note = "Note: Data from O&S Amsterdam.",
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
  #TODO: FIX, currently order of coefficients is off!
  stargazer(pvda.op1.m1, pvda.op1.m2,
    title = "OLS regression model for PvdA support in the 2018 Amsterdam municipal election",
    #TODO: may need to place footnote on exact def of other african etc.
    #NOTE: ordering of variables somehow incorrectly changes display of coefs
    covariate.labels = c("% public housing", "Average net income", "% Turkish, Moroccan, Surinamese, Antillean",
              "% (other) African, Latin American, Asian", "% lower educated",
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
  # Export regression table
  #TODO: FIX, currently order of coefficients is off!
  stargazer(turn.op1.m1, turn.op1.m2,
    title = "OLS regression model for turnout in the 2018 Amsterdam municipal election",
    #TODO: may need to place footnote on exact def of other african etc.
    #NOTE: ordering of variables somehow incorrectly changes display of coefs
    covariate.labels = c("% public housing", "Average net income", "% Turkish, Moroccan, Surinamese, Antillean",
                         "% (other) African, Latin American, Asian", "% lower educated",
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
  
# Robustness check 1: alternative gentrification definition ----------------------------------------------------------------
  
# PVDA models
  
  # OPTION 2 - Gentrification as change over 8 years (2009-2017)
  # Model 1: composition effects
  pvda.op2.m1 <- lm(PVDA ~ housing_pub + netHHincome + imm_TMSA + imm_other +
                    edu_low + edu_high + age_18t26 + age_66plus + unempl, 
                    data = data.pvda.op2)
  summary(pvda.op2.m1)
  
  # Model 2: add gentrification (change variables)
  pvda.op2.m2 <- lm(PVDA ~ housing_pub + netHHincome + imm_TMSA + imm_other +
                    edu_low + edu_high + age_18t26 + age_66plus + unempl + 
                    housing_pub_delta2009 + netincome_delta2009, 
                    data = data.pvda.op2)
  summary(pvda.op2.m2)
  
  # Export regression table
  stargazer(pvda.op2.m1, pvda.op2.m2,
    title = "Regression model for PvdA support with gentrification defined as 8-year change",
    #TODO: may need to place footnote on exact def of other african etc.
    #NOTE: ordering of variables somehow incorrectly changes display of coefs
    covariate.labels = c("% public housing", "Average net income", "% Turkish, Moroccan, Surinamese, Antillean",
                         "% (other) African, Latin American, Asian", "% lower educated",
                         "% higher educated", "% youth (18-26)", "% elderly (66+)", "% unemployed", 
                         "Change in  % public housing (2009-2017)", "Change in average net income (2009-2017)"),
    dep.var.labels = "PvdA vote share",
    column.labels = c("Model 1", "Model 2"),
    model.numbers = FALSE,
    star.cutoffs = c(0.05, 0.01, 0.001),
    omit.stat = c("f", "ser"),
    out = "pvda_gentr_8years.html")
  
  # OPTION 3 - Gentrification as change over 12 years (2005-2017)
  # Model 1: composition effects
  pvda.op3.m1 <- lm(PVDA ~ housing_pub + netHHincome + imm_TMSA + imm_other +
                    edu_low + edu_high + age_18t26 + age_66plus + unempl, 
                    data = data.pvda.op3)
  summary(pvda.op3.m1)

  # Model 2: add gentrification (change variables)
  pvda.op3.m2 <- lm(PVDA ~ housing_pub + netHHincome + imm_TMSA + imm_other +
                    edu_low + edu_high + age_18t26 + age_66plus + unempl + 
                    housing_pub_delta2005 + netincome_delta2005, 
                    data = data.pvda.op3)
  summary(pvda.op3.m2)
  
  # Export regression table
  stargazer(pvda.op3.m1, pvda.op3.m2,
    title = "Regression model for PvdA support with gentrification defined as 12-year change",
    #TODO: may need to place footnote on exact def of other african etc.
    #NOTE: ordering of variables somehow incorrectly changes display of coefs
    covariate.labels = c("% public housing", "Average net income", "% Turkish, Moroccan, Surinamese, Antillean",
                         "% (other) African, Latin American, Asian", "% lower educated",
                         "% higher educated", "% youth (18-26)", "% elderly (66+)", "% unemployed", 
                         "Change in  % public housing (2005-2017)", "Change in average net income (2005-2017)"),
    dep.var.labels = "PvdA vote share",
    column.labels = c("Model 1", "Model 2"),
    model.numbers = FALSE,
    star.cutoffs = c(0.05, 0.01, 0.001),
    omit.stat = c("f", "ser"),
    out = "pvda_gentr_12years.html")
  
# Turnout models
  
  # OPTION 2 - Gentrification as change over 8 years (2009-2017)
  # Model 1: composition effects
  turn.op2.m1 <- lm(turnout ~ housing_pub + netHHincome + imm_TMSA + imm_other +
                    edu_low + edu_high + age_18t26 + age_66plus + unempl, 
                    data = data.turn.op2)
  summary(turn.op2.m1)
  
  # Model 2: add gentrification (change variables)
  turn.op2.m2 <- lm(turnout ~ housing_pub + netHHincome + imm_TMSA + imm_other +
                    edu_low + edu_high + age_18t26 + age_66plus + unempl +
                    housing_pub_delta2009 + netincome_delta2009, 
                    data = data.turn.op2)
  summary(turn.op2.m2)
  
  # Export regression table
  stargazer(turn.op2.m1, turn.op2.m2,
    title = "Regression model for turnout with gentrification defined as 8-year change",
    #TODO: may need to place footnote on exact def of other african etc.
    #NOTE: ordering of variables somehow incorrectly changes display of coefs
    covariate.labels = c("% public housing", "Average net income", "% Turkish, Moroccan, Surinamese, Antillean",
                         "% (other) African, Latin American, Asian", "% lower educated",
                         "% higher educated", "% youth (18-26)", "% elderly (66+)", "% unemployed", 
                         "Change in  % public housing (2009-2017)", "Change in average net income (2009-2017)"),
    dep.var.labels = "Turnout",
    column.labels = c("Model 1", "Model 2"),
    model.numbers = FALSE,
    star.cutoffs = c(0.05, 0.01, 0.001),
    omit.stat = c("f", "ser"),
    out = "turnout_gentr_8years.html")

  # OPTION 3 - Gentrification as change over 12 years (2005-2017)
  # Model 1: composition effects
  turn.op3.m1 <- lm(turnout ~ housing_pub + netHHincome + imm_TMSA + imm_other +
                    edu_low + edu_high + age_18t26 + age_66plus + unempl, 
                    data = data.turn.op3)
  summary(turn.op3.m1)
  
  # Model 2: add gentrification (change variables)
  turn.op3.m2 <- lm(turnout ~ housing_pub + netHHincome + imm_TMSA + imm_other +
                    edu_low + edu_high + age_18t26 + age_66plus + unempl + 
                    housing_pub_delta2005 + netincome_delta2005, 
                    data = data.turn.op3)
  summary(turn.op3.m2)
  
  # Export regression table
  stargazer(turn.op3.m1, turn.op3.m2,
    title = "Regression model for turnout with gentrification defined as 12-year change",
    #TODO: may need to place footnote on exact def of other african etc.
    #NOTE: ordering of variables somehow incorrectly changes display of coefs
    covariate.labels = c("% public housing","Average net income", "% Turkish, Moroccan, Surinamese, Antillean",
                         "% (other) African, Latin American, Asian", "% lower educated",
                         "% higher educated", "% youth (18-26)", "% elderly (66+)", "% unemployed", 
                         "Change in  % public housing (2005-2017)", "Change in average net income (2005-2017)"),
    dep.var.labels = "Turnout",
    column.labels = c("Model 1", "Model 2"),
    model.numbers = FALSE,
    star.cutoffs = c(0.05, 0.01, 0.001),
    omit.stat = c("f", "ser"),
    out = "turnout_gentr_12years.html")
 
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
  
  #NOTE: no outliers on PVDA variable

  #NOTE: for public housing, observation with >-20 in public housing may be 
  #a low leverage outlier. No very clear linear relation, but no other shape either
  
  #NOTE: for net income, there are several observations with (rare) high values on
  #change in net income -- excluding above 15, there is still no very clear linear relation, 
  #but no other shape appears either

  
#TURNOUT MODELS
#Turnout and change in public housing
ggplot(data = data.turn.op1,
       aes(y = turnout, x = housing_pub_delta2013)) +
  geom_point()

#Turnout and change in net household income
ggplot(data = data.turn.op1,
       aes(y = turnout, x = netincome_delta2013)) +
  geom_point()
  
  #NOTE: for turnout there is one outlier that might be influential 



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

#plot shows: observations with >-20 in pub housing might be an outlier, but low leverage

#diagnostic plots wth base r
jpeg("pvda_diagnostic plots.jpg", width = 750, height = 750)
par(mfrow = c(2, 2))
plot(pvda.op1.m2)
dev.off()

jpeg("turnout_diagnostic plots.jpg", width = 750, height = 750)
par(mfrow = c(2, 2))
plot(turn.op1.m2)
dev.off()

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
  #NOTE: ordering of variables somehow incorrectly changes display of coefs
  covariate.labels = c("% public housing","Average net income", "% Turkish, Moroccan, Surinamese, Antillean",
                       "% (other) African, Latin American, Asian", "% lower educated",
                       "% higher educated", "% youth (18-26)", "% elderly (66+)", "% unemployed", 
                       "Change in  % public housing", "Change in average net income"),
  dep.var.labels = "PvdA vote share",
  column.labels = c("Model 1", "Model 2"),
  model.numbers = FALSE,
  star.cutoffs = c(0.05, 0.01, 0.001),
  omit.stat = c("f", "ser"),
  single.row = TRUE, 
  out = "pvda_outliers.html")

  #NOTE: without public housing outlier, the effect is slightly bigger;
  # with: b = 0.08, not significant
  # without: b = 0.13, one star significance
  #TODO: remove note

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
    #NOTE: ordering of variables somehow incorrectly changes display of coefs
    covariate.labels = c("% public housing", "Average net income", "% Turkish, Moroccan, Surinamese, Antillean",
                         "% (other) African, Latin American, Asian", "% lower educated",
                         "% higher educated", "% youth (18-26)", "% elderly (66+)", "% unemployed", 
                         "Change in  % public housing", "Change in average net income"),
    dep.var.labels = "Turnout",
    column.labels = c("Model 1", "Model 2"),
    model.numbers = FALSE,
    star.cutoffs = c(0.05, 0.01, 0.001),
    omit.stat = c("f", "ser"),
    single.row = TRUE,
    out = "turnout_outliers.html")
  
  #NOTE: effect of change in pub housing turns smaller, (0.57 to 0.24), no longer significant
  # effect of change in net income CHANGES SIGN (0.33 to -0.23)
    # both not significant
    # excluding the outlier makes effect in line with hypothesis
  #TODO: remove note

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

#Check correlation of main predictors: gentrification variables
cor(subdata$netincome_delta2013, subdata$housing_pub_delta2013)
