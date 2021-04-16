# Set-up ---------------------------------------------------------------------------------------
rm(list=ls())

library(raster)
library(foreign) 
library(tidyverse)
library(ggplot2) 

# Reading data ---------------------------------------------------------------------------------

#Buurtcombinatie codes and names (2020 version)
#Data can be retrieved from: https://data.amsterdam.nl/datasets/5L2CSm77FLZGYA/registratie-gebieden/
#From the website, download the 'Gebieden Wijk' (CSV) file. 
bc_namen <- read.csv("/Users/Maartje/Desktop/LJA/Data POLetmaal/Overzicht buurten Amsterdam.csv", header = TRUE)
bc_namen <- bc_namen[,2:3]
bc_namen <- bc_namen %>% rename(bc_code = code)

#Add all possible combinations of buurtcombinaties codes and names
bc_namen$string <- paste0(bc_namen$bc_code, bc_namen$naam)
x <- expand.grid(left=bc_namen$string, right=bc_namen$string)
x$lcode  <- substring(x$left , 1, 3)
x$rcode  <- substring(x$right, 1, 3)
x$lname  <- substring(x$left , 4)
x$rname  <- substring(x$right, 4)
x$bc_code<- paste0(x$lcode,  "+" , x$rcode)
x$naam   <- paste0(x$lname, " + ", x$rname)
bc_namen <- rbind(bc_namen[,1:2], x[,7:8])

#2006-2018 election data
data2006 <- read.csv("/Users/Maartje/Desktop/LJA/Data POLetmaal/2006_buurtcombinaties.csv", header = TRUE)
data2010 <- read.csv("/Users/Maartje/Desktop/LJA/Data POLetmaal/2010_buurtcombinaties.csv", header = TRUE)
data2014 <- read.csv("/Users/Maartje/Desktop/LJA/Data POLetmaal/2014_stembureaus.csv"     , header = TRUE)
data2018 <- read.csv("/Users/Maartje/Desktop/LJA/Data POLetmaal/2018_buurtcombinaties.csv", header = TRUE)

# Preparing data files -------------------------------------------------------------------------

#Remove empty rows
data2006 <- data2006[complete.cases(data2006[,4]),]
data2010 <- data2010[complete.cases(data2010[,4]),]
data2014 <- data2014[complete.cases(data2014[,4]),]
data2018 <- data2018[complete.cases(data2018[,4]),]

#Fix mistake in 2010 data
data2010$bc <- if_else(data2010$bc == "E13+E12 ", "E13+E12", data2010$bc)
data2014$bc <- if_else(data2014$bc == "N71",      "N60+N71", data2014$bc)

#Add BC full name variable to 2014 and 2018 data files (match on BC code)
#2018 data is matched with 2020 file on names, 2014 is matched with BC names from 2010 data
bc_namen_2010 <- data2010[,1:2]
K47 <- data.frame("Museumkwartier", "K47")
K50 <- data.frame("Duivelseiland",  "K50")
names(K47) <- c("naam.buurtcombinatie", "bc")
names(K50) <- c("naam.buurtcombinatie", "bc")
bc_namen_2014 <- rbind(K47, K50, bc_namen_2010)

data2018 <- data2018 %>% rename(bc_code = wijk.std.gb)

data2014 <- merge(data2014, bc_namen_2014, by="bc")
data2018 <- merge(data2018, bc_namen,      by="bc_code") 

#Aggregate stembureaus into buurtcombinaties
#S3.SD.summed <- aggregate(sendata$S3..Acknowledgement, by=list(speaker.debate.ID=sendata$speaker.debate.ID), FUN=sum)
data2014 <- data2014[,c(1,5:36)]
data2014 <- aggregate(data2014[,2:32], by=list(bc=data2014$bc, naam.buurtcombinatie=data2014$naam.buurtcombinatie), FUN=sum)

#Add years to variable names
names(data2006) <- paste0(names(data2006), "_2006")
names(data2010) <- paste0(names(data2010), "_2010")
names(data2014) <- paste0(names(data2014), "_2014")
names(data2018) <- paste0(names(data2018), "_2018")

data2006 <- data2006 %>% rename(bc_naam = naam.buurtcombinatie_2006)
data2010 <- data2010 %>% rename(bc_naam = naam.buurtcombinatie_2010)
data2014 <- data2014 %>% rename(bc_naam = naam.buurtcombinatie_2014)
data2018 <- data2018 %>% rename(bc_naam = naam_2018)

#Make percentages for 2014 and 2018
data2014[,4:32] <- 100 * data2014[,4:32]/data2014$totaal_2014
data2018[,7:34] <- 100 * data2018[,7:34]/data2018$geldige.stembiljetten_2018

# Merging data ---------------------------------------------------------------------------------

#Merge
data <- merge(data2006, data2010, by="bc_naam", all=TRUE)
data <- merge(data,     data2014, by="bc_naam", all=TRUE)
data <- merge(data,     data2018, by="bc_naam", all=TRUE)

#Reorder columns
data <- data[,c(1,2,16,39,71,3:15,17:38,40:70,72:104)]

#Subset to PvdA & multicultural parties only
subdata <- data[,c(1:5,8,21,44,78,65,85,96)]

#Export data to CSV (full & subset)
write.csv(data,   "/Users/Maartje/Desktop/LJA/data_allepartijen.csv", row.names = FALSE)
write.csv(subdata,"/Users/Maartje/Desktop/LJA/data_subset.csv",       row.names = FALSE)

