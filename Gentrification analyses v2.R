# Perils of “revitalization”? 
# Gentrification, political participation and the support from immigrant-origin voters for social democratic and multicultural parties in Amsterdam 

# Maartje van de Koppel
# Data analysis
# Last update: 29/10/21

# Set-up ----------------------------------------------------------------------------------------------------- #
rm(list=ls())

library(raster)     # select only libraries used
library(foreign) 
library(tidyverse)
library(ggplot2)
library(readxl)
library(dplyr)
library(stats)
library(tidyr)
library(expss)

# Importing data --------------------------------------------------------------------------------------------- #

# TO DO: check whether data is correct
  data <- read.csv("/Users/Maartje/Desktop/LJA/Paper politicologenetmaal/Analyses/data/data_sub_merged_long 2.csv", header=TRUE)

# Label the variables
  var_lab(data$PVDA) = "% PvdA vote"
  var_lab(data$MPP)  = "% M+ vote"
  var_lab(data$DENK) = "% DENK vote"
  var_lab(data$BIJ1) = "% BIJ1 vote"
  var_lab(data$MCparties) = "% Multicultural parties vote"
  var_lab(data$imm_Sur) = "% Surinamese"
  var_lab(data$imm_Ant) = "% Antillean"
  var_lab(data$imm_Tur) = "% Turkish"
  var_lab(data$imm_Mar) = "% Moroccan"
  var_lab(data$imm_otherNW) = "% Other non-western immigrant"
  var_lab(data$imm_W) = "% Western immigrant "
  var_lab(data$imm_autoch) = "% Autochthonous"
  var_lab(data$age_18t26) =  "% 0 to 18 year-olds"
  var_lab(data$age_18t26) = "% 18 to 26 year-olds"
  var_lab(data$age_66plus) = "% 66 plus"
  var_lab(data$unempl) = "% Unemployed"
  var_lab(data$edu_low) = "% Lower educated"
  var_lab(data$edu_mid) = "% Medium educated"
  var_lab(data$edu_hi) = "% Higher educated"
  var_lab(data$housing_soc) =  "% Social housing"
  var_lab(data$housing_soc_delta2005) = "∆ % Social housing since 2005"
  var_lab(data$housing_soc_delta2009) = "∆ % Social housing since 2006"
  var_lab(data$housing_soc_delta2013) = "∆ % Social housing since 2013"
  var_lab(data$housing_soc_delta) = "∆ % Social housing (t-1)"
  var_lab(data$housing_pub_delta) = "∆ % Public housing (t-1)"
  var_lab(data$PVDA_delta2006) = "∆ % PvdA vote since 2005"
  var_lab(data$PVDA_delta2010) = "∆ % PvdA vote since 2006"
  var_lab(data$PVDA_delta2014) = "∆ % PvdA vote since 2013"
  var_lab(data$PVDA_delta) = "∆ % PvdA vote since previous election"
  var_lab(data$unempl) = "% Recipients unemployment benefits"
  var_lab(data$turnout) = "Turnout"




