# Perils of “revitalization”? 
# Gentrification, political participation and the support from immigrant-origin voters for social democratic and multicultural parties in Amsterdam 

# Maartje van de Koppel
# Data generation script 
# Last update: 04/10/21

# Set-up -----------------------------------------------------------------------------------------------------
rm(list=ls())

library(raster)
library(foreign) 
library(tidyverse)
library(ggplot2)
library(readxl)
library(dplyr)
library(stats)
library(tidyr)

# VOTE SHARES - Reading data ---------------------------------------------------------------------------------

# Buurtcombinatie codes and names (2020 version)
# Data can be retrieved from: https://data.amsterdam.nl/datasets/5L2CSm77FLZGYA/registratie-gebieden/
# From the website, download the 'Gebieden Wijk' (CSV) file. 
bc_namen <- read.csv("/Users/Maartje/Desktop/LJA/Data POLetmaal/Overzicht buurten Amsterdam.csv", header = TRUE)
bc_namen <- bc_namen[,2:3]
bc_namen <- bc_namen %>% rename(bc_code = code)

# Add all possible combinations of buurtcombinaties codes and names
bc_namen$string <- paste0(bc_namen$bc_code, bc_namen$naam)
x <- expand.grid(left=bc_namen$string, right=bc_namen$string)
x$lcode  <- substring(x$left , 1, 3)
x$rcode  <- substring(x$right, 1, 3)
x$lname  <- substring(x$left , 4)
x$rname  <- substring(x$right, 4)
x$bc_code<- paste0(x$lcode,  "+" , x$rcode)
x$naam   <- paste0(x$lname, " + ", x$rname)
bc_namen <- rbind(bc_namen[,1:2], x[,7:8])

# 2006-2018 election data
data2006 <- read.csv("/Users/Maartje/Desktop/LJA/Data POLetmaal/2006_buurtcombinaties.csv", header = TRUE)
data2010 <- read.csv("/Users/Maartje/Desktop/LJA/Data POLetmaal/2010_buurtcombinaties.csv", header = TRUE)
data2014 <- read.csv("/Users/Maartje/Desktop/LJA/Data POLetmaal/2014_stembureaus.csv"     , header = TRUE)
data2018 <- read.csv("/Users/Maartje/Desktop/LJA/Data POLetmaal/2018_buurtcombinaties.csv", header = TRUE)

data2006perc <- read.csv("/Users/Maartje/Desktop/LJA/Data POLetmaal/2006_buurtcombinaties_procenten.csv", header = TRUE)
data2010perc <- read.csv("/Users/Maartje/Desktop/LJA/Data POLetmaal/2010_buurtcombinaties_procenten.csv", header = TRUE)
data2006perc <- data2006perc[complete.cases(data2006perc[,4]),]
data2010perc <- data2010perc[complete.cases(data2010perc[,4]),]

# VOTE SHARES - Preparing data files -------------------------------------------------------------------------

# Remove empty rows
data2006 <- data2006[complete.cases(data2006[,4]),]
data2010 <- data2010[complete.cases(data2010[,4]),]
data2014 <- data2014[complete.cases(data2014[,4]),]
data2018 <- data2018[complete.cases(data2018[,4]),]

# Fix mistakes in 2010 and 2014 data
data2010$bc                   <- if_else(data2010$bc == "E13+E12 ", "E13+E12", data2010$bc)
data2014$bc                   <- if_else(data2014$bc == "N71", "N60+N71", data2014$bc)
data2010$naam.buurtcombinatie <- if_else(data2010$naam.buurtcombinatie == "Middelveldsche Akerpolder/ Sloten", "Middelveldsche Akerpolder/Sloten", data2010$naam.buurtcombinatie)

# Add BC full name variable to 2014 and 2018 data files (match on BC code)
# 2018 data is matched with 2020 file on names, 2014 is matched with BC names from 2010 data
bc_namen_2010 <- data2010[,1:2]
K47 <- data.frame("Museumkwartier", "K47")
K50 <- data.frame("Duivelseiland",  "K50")
names(K47) <- c("naam.buurtcombinatie", "bc")
names(K50) <- c("naam.buurtcombinatie", "bc")
bc_namen_2014 <- rbind(K47, K50, bc_namen_2010)

data2018 <- data2018 %>% rename(bc_code = wijk.std.gb)

data2014 <- merge(data2014, bc_namen_2014, by="bc")
data2018 <- merge(data2018, bc_namen,      by="bc_code") 

# Aggregate stembureaus into buurtcombinaties
# S3.SD.summed <- aggregate(sendata$S3..Acknowledgement, by=list(speaker.debate.ID=sendata$speaker.debate.ID), FUN=sum)
data2014 <- data2014[,c(1,5:36)]
data2014 <- aggregate(data2014[,2:32], by=list(bc=data2014$bc, naam.buurtcombinatie=data2014$naam.buurtcombinatie), FUN=sum)

# Add years to variable names
names(data2006) <- paste0(names(data2006), "_2006")
names(data2010) <- paste0(names(data2010), "_2010")
names(data2014) <- paste0(names(data2014), "_2014")
names(data2018) <- paste0(names(data2018), "_2018")

data2006 <- data2006 %>% rename(bc_naam     = naam.buurtcombinatie_2006)
data2010 <- data2010 %>% rename(bc_naam     = naam.buurtcombinatie_2010)
data2014 <- data2014 %>% rename(bc_naam     = naam.buurtcombinatie_2014)
data2018 <- data2018 %>% rename(bc_naam     = naam_2018)
data2018 <- data2018 %>% rename(totaal_2018 = geldige.stembiljetten_2018)

# Make percentages for 2014 and 2018 - NO LONGER NEEDED
#data2014[,4:32] <- 100 * data2014[,4:32]/data2014$totaal_2014
#data2018[,7:34] <- 100 * data2018[,7:34]/data2018$geldige.stembiljetten_2018

# VOTE SHARES - Merging data ---------------------------------------------------------------------------------

# Merge
data <- merge(data2006, data2010, by="bc_naam", all=TRUE)
data <- merge(data,     data2014, by="bc_naam", all=TRUE)
data <- merge(data,     data2018, by="bc_naam", all=TRUE)

# Reorder columns
data <- data[,c(1,2,17,40,72,3:16,18:39,41:71,73:105)]

# Subset to PvdA, multicultural parties & GroenLinks only
subdata <- data[,c(1:5,6,8,9,11,20,22,23,25,42,45,47,72:74,79,66,82,86,97)]

# Rename 2014 Groenlinks variable 
subdata <- subdata %>% rename(GL_2014 = GROENLINKS_2014)

# Export data to CSV (full & subset)
write.csv(data,   "/Users/Maartje/Desktop/LJA/data_allepartijen.csv", row.names = FALSE)
write.csv(subdata,"/Users/Maartje/Desktop/LJA/data_subset.csv",       row.names = FALSE)

# NEIGHBOURHOOD DATA - Reading data ---------------------------------------------------------------------------

# Read 'Basisbestand Gebieden Amsterdam' (BBGA) for neighbourhood variables
# Data can be retrieved from https://data.amsterdam.nl/datasets/G5JpqNbhweXZSw/basisbestand-gebieden-amsterdam-bbga/
columnnames <- names(read_xlsx("/Users/Maartje/Desktop/LJA/Data POLetmaal/Buurtkenmerken (versie 10-3-21).xlsx", n_max = 0))
columntypes <- ifelse(grepl("^[A-Z]", columnnames),"numeric", "guess")
buurtdata <- read_xlsx("/Users/Maartje/Desktop/LJA/Data POLetmaal/Buurtkenmerken (versie 10-3-21).xlsx", sheet = 1, col_names = TRUE, col_types = columntypes)

# Read 'Woningvoorraad 1995-2014' and 'Woningvoorraad 2011-2021' for data on changes in housing stock over time
# Data can be retrieved from https://data.amsterdam.nl/datasets/XBHSC-xM8UmROQ/tijdreeksen-wijken/
housingdata_0509 <- read_csv("/Users/Maartje/Desktop/LJA/Data POLetmaal/Woningvoorraad 1995-2014.csv", col_names = TRUE)
housingdata_1317 <- read_csv("/Users/Maartje/Desktop/LJA/Data POLetmaal/Woningvoorraad 2011-2021.csv", col_names = TRUE)

# Read several files for additional gentrification variables: standardised net income per household, newly built properties
# Data retrieved from OIS Amsterdam

# 2005 income data
income2005 <- read_xls("/Users/Maartje/Desktop/LJA/Data POLetmaal/2008_stadsdelen_40.xls", skip = 9, sheet = 1, col_names = FALSE, col_types = "text")
income2005 <- income2005 %>% select(c(`...1`, `...2`, `...8`)) %>% rename(`bc_code` = `...1`) %>% rename(`bc_naam` = `...2`) %>% rename(`net_std_income_household` = `...8`)
income2005 <- income2005 %>% dplyr::filter(str_length(`bc_code`) == 3 & `bc_code` != "ASD") # Keep only neighbourhood observations
income2005$net_std_income_household <- gsub(",", ".", income2005$net_std_income_household) # Make sure use of commas and periods is consistent
income2005[income2005 == "x"] <- NA # Set "x" to missing
income2005$net_std_income_household <- as.numeric(income2005$net_std_income_household) # Set to numeric

# 2009 income data
income2009  <- read_xls("/Users/Maartje/Desktop/LJA/Data POLetmaal/2011_stadsdelen_46.xls", skip = 9, sheet = 1, col_names = FALSE, col_types = "text")
income2009$bc_code <- substr(income2009$`...1`, 1, 3)
income2009  <- income2009[income2009$bc_code != "ASD" & !grepl(" ", income2009$bc_code) & !is.na(income2009$bc_code),] # Keep only neighbourhood observations
income2009$bc_naam <- substring(income2009$`...1`, 5)
income2009  <- income2009 %>% rename(`net_std_income_household` = `...7`)
income2009$net_std_income_household <- gsub('[,]', '\\.', income2009$net_std_income_household) # Make sure use of commas and periods is consistent
income2009[income2009 == "x"] <- NA # Set "x" to missing
income2009 <- income2009 %>% select(c(bc_code, bc_naam, net_std_income_household))
income2009$net_std_income_household <- as.numeric(income2009$net_std_income_household) # Set to numeric

# 2013 income data
## LET OP: hier nog een bc_naam aan koppelen? #########
columnnames <- names(read_xlsx("/Users/Maartje/Desktop/LJA/Data POLetmaal/2016_stadsdelen_3_21.xlsx", n_max = 0))
columntypes <- ifelse(grepl("^[A-Z]", columnnames),"numeric", "guess")
income2013  <- read_excel("/Users/Maartje/Desktop/LJA/Data POLetmaal/2016_stadsdelen_3_21.xlsx", skip = 8, sheet = 1, col_names = FALSE, col_types = "text")
income2013$bc_code <- substr(income2013$`...1`, 1, 3)
income2013  <- income2013 %>% dplyr::filter(str_length(`bc_code`) == 3 & `bc_code` != "ASD" & !grepl(" ", bc_code) & str_length(`...1`) == 3 ) # Keep only neighbourhood observations
income2013  <- income2013 %>% rename(`net_std_income_household` = `...13`)
income2013$net_std_income_household <- gsub('[,]', '\\.', income2013$net_std_income_household) # Make sure use of commas and periods is consistent
income2013[income2013 == "x"] <- NA # Set "x" to missing
income2013  <- income2013 %>% select(bc_code, net_std_income_household)
income2013$net_std_income_household <- as.numeric(income2013$net_std_income_household) # Set to numeric

# 2017 income data
## LET OP: hier nog een bc_naam aan koppelen? #########
income2017  <- read_xlsx("/Users/Maartje/Desktop/LJA/Data POLetmaal/2019_stadsdelen_3_21.xlsx", skip = 8, sheet = 1, col_names = FALSE, col_types = "text")
income2017$bc_code <- substr(income2017$`...1`, 1, 3)
income2017  <- income2017 %>% dplyr::filter(str_length(`bc_code`) == 3 & `bc_code` != "ASD" & !grepl(" ", bc_code) & str_length(`...1`) == 3 ) # Keep only neighbourhood observations
income2017  <- income2017 %>% rename(`net_std_income_household` = `...13`)
income2017$net_std_income_household <- gsub('[,]', '\\.', income2017$net_std_income_household) # Make sure use of commas and periods is consistent
income2017[income2017 == "x"] <- NA # Set "x" to missing  
income2017  <- income2017 %>% select(bc_code, net_std_income_household)
income2017$net_std_income_household <- as.numeric(income2017$net_std_income_household) # Set to numeric

# 2005 new buildings data
## LET OP: hier nog een bc_naam aan koppelen? ##########
buildings2005 <- read_xls("/Users/Maartje/Desktop/LJA/Data POLetmaal/2005_buurten_z_bouwperiode.xls", skip = 4, sheet = 1, col_names = FALSE)
buildings2005$bc_code <- substr(buildings2005$`...1`, 1, 3)
buildings2005 <- buildings2005[buildings2005$bc_code != "tot" & !is.na(buildings2005$bc_code),] # Keep only neighbourhood observations
buildings2005 <- buildings2005 %>% select(c(bc_code, `...11`)) %>% rename(`builtafter2000` = `...11`) 
buildings2005 <- aggregate(buildings2005$builtafter2000, by=list(buildings2005$bc_code), FUN=sum)
buildings2005 <- buildings2005 %>% rename(bc_code = Group.1) %>% rename(builtafter2000 = x)

# 2009 new buildings data
buildings2009 <- read_xls("/Users/Maartje/Desktop/LJA/Data POLetmaal/2009_buurten_z_bouwperiode.xls", skip = 4, sheet = 1, col_names = FALSE)
buildings2009$bc_code <- substr(buildings2009$`...1`, 1, 3)
buildings2009 <- buildings2009[buildings2009$bc_code != "tot" & !is.na(buildings2009$bc_code),] # Keep only neighbourhood observations
buildings2009 <- buildings2009 %>% select(c(bc_code, `...11`)) %>% rename(`builtafter2000` = `...11`) 
buildings2009 <- aggregate(buildings2009$builtafter2000, by=list(buildings2009$bc_code), FUN=sum)
buildings2009 <- buildings2009 %>% rename(bc_code = Group.1) %>% rename(builtafter2000 = x)

# 2013 new buildings data
## LET OP: hier nog een bc_naam aan koppelen? ##########
buildings2013 <- read_xlsx("/Users/Maartje/Desktop/LJA/Data POLetmaal/2013_buurten_z_bouwperiode.xlsx", skip = 4, sheet = 1, col_names = FALSE)
buildings2013$bc_code <- substr(buildings2013$`...1`, 1, 3)
buildings2013 <- buildings2013[buildings2013$bc_code != "tot" & !is.na(buildings2013$bc_code),] # Keep only neighbourhood observations
buildings2013 <- buildings2013 %>% select(c(bc_code, `...11`)) %>% rename(`builtafter2000` = `...11`) 
buildings2013 <- aggregate(buildings2013$builtafter2000, by=list(buildings2013$bc_code), FUN=sum)
buildings2013 <- buildings2013 %>% rename(bc_code = Group.1) %>% rename(builtafter2000 = x)

# 2017 new buildings data
## LET OP: hier nog een bc_naam aan koppelen? ##########
buildings2017 <- read_xlsx("/Users/Maartje/Desktop/LJA/Data POLetmaal/2017_stadsdelen_7_04.xlsx", skip = 5, sheet = 1, col_names = FALSE)
buildings2017$bc_code <- substr(buildings2017$`...1`, 1, 3)
buildings2017 <- buildings2017 %>% dplyr::filter(str_length(`bc_code`) == 3 & `bc_code` != "ASD" & !grepl(" ", bc_code) & str_length(`...1`) == 3 ) # Keep only neighbourhood observations
buildings2017 <- buildings2017 %>% select(c(bc_code, `...11`, `...12`)) %>% rename(`built2001-2010` = `...11`) %>% rename(`builtafter2010` = `...12`) 
buildings2017[buildings2017 == "-"] <- "0"
buildings2017$`built2001-2010` <- as.numeric(buildings2017$`built2001-2010`) # Set to numeric
buildings2017$builtafter2010   <- as.numeric(buildings2017$builtafter2010)   # Set to numeric
buildings2017$builtafter2000 <- buildings2017$`built2001-2010` + buildings2017$builtafter2010 # Add columns for one 'after 2000' category
buildings2017 <- buildings2017 %>% select(c(bc_code, builtafter2000))

# Read data for employment variable
# Data retrieved from OIS Amsterdam
employ_2005 <- read_xls( "/Users/Maartje/Desktop/LJA/Data POLetmaal/Data Laure/2005_wwb_awb.xls",  skip = 4, col_names = TRUE)
employ_2009 <- read_xls( "/Users/Maartje/Desktop/LJA/Data POLetmaal/Data Laure/2009_wwb.xls",  skip = 4, col_names = TRUE)
employ_2013 <- read_xlsx("/Users/Maartje/Desktop/LJA/Data POLetmaal/Data Laure/2013_wwb.xlsx", skip = 4, col_names = TRUE)
employ_2017 <- read_xlsx("/Users/Maartje/Desktop/LJA/Data POLetmaal/Data Laure/2017_wwb.xlsx", skip = 3, col_names = TRUE)

employ_2005 <- employ_2005 %>% dplyr::filter(str_length(`bc/std`)   == 3 & `bc/std`   != "ASD") # Keep only correct observations
employ_2009 <- employ_2009 %>% dplyr::filter(str_length(`bc/std`)   == 3 & `bc/std`   != "ASD")
employ_2013 <- employ_2013 %>% dplyr::filter(str_length(`bc/std`)   == 3 & `bc/std`   != "ASD")
employ_2017 <- employ_2017 %>% dplyr::filter(str_length(`wijk/std`) == 3 & `wijk/std` != "ASD")

issomething <- function(x) {x== "*" | x== "-"} # set missing values to N/A
employ_2005[issomething(employ_2005)] <- NA 
employ_2009[issomething(employ_2009)] <- NA 
employ_2013[issomething(employ_2013)] <- NA 
employ_2017[issomething(employ_2017)] <- NA 

employ_2005 <- employ_2005 %>% select(c(`bc/std`, `2005...5`))        %>% rename(`brtk2005` = `bc/std`)   %>% rename(employ = `2005...5`)
employ_2009 <- employ_2009 %>% select(c(`bc/std`, `2009...5`))        %>% rename(`brtk2005` = `bc/std`)   %>% rename(employ = `2009...5`)
employ_2013 <- employ_2013 %>% select(c(`bc/std`, `2013...4`))        %>% rename(`bc-code`  = `bc/std`)   %>% rename(employ = `2013...4`)
employ_2017 <- employ_2017 %>% select(c(`wijk/std`, levensonderhoud)) %>% rename(`bc-code`  = `wijk/std`) %>% rename(employ = levensonderhoud)

employ_2005$employ <- as.numeric(employ_2005$employ)
employ_2009$employ <- as.numeric(employ_2009$employ)
employ_2013$employ <- as.numeric(employ_2013$employ)
employ_2017$employ <- as.numeric(employ_2017$employ)

# Add 2015 BC codes to 2005 and 2009 employment data
# Retrieve 2015 codes from transition document: [add source]
bc_overgang <- read_xlsx("/Users/Maartje/Desktop/LJA/Data POLetmaal/2015_overgang 2005 2010 2015.xlsx", sheet = 2, col_names = TRUE )
bc_overgang <- bc_overgang  %>% select(c(brtk2005, brtk2015)) 

employ_2005 <- merge(employ_2005, bc_overgang, by="brtk2005", x.all=TRUE) # Add 2015 codes to employment data
employ_2009 <- merge(employ_2009, bc_overgang, by="brtk2005", x.all=TRUE)

employ_2005 <- employ_2005 %>% rename(`bc-code` = `brtk2015`) # Rename BC code variable for later merging
employ_2009 <- employ_2009 %>% rename(`bc-code` = `brtk2015`)

employ_2005 <- employ_2005 %>% select(c(`bc-code`, employ)) # Drop 2005 BC code, is now redundant
employ_2009 <- employ_2009 %>% select(c(`bc-code`, employ))

# NEIGHBOURHOOD DATA - Preparing data ---------------------------------------------------------------------------

# HOUSINGDATA

# Remove empty rows from housingdata_1317
housingdata_1317 <- housingdata_1317[complete.cases(housingdata_1317),]

# Create bc_code variable for merging
housingdata_0509$bc_code <- substr(housingdata_0509$`naam bc/std`, 1, 3) # extract BC code
housingdata_1317$bc_code <- substr(housingdata_1317$`indeling buurtcombinaties 2015 1)`, 1, 3) # extract BC code

# Subset to relevant years: 2005, 2009, 2013 and 2017
housingdata_0509 <- select(housingdata_0509, c(bc_code, `2005`, `2009`))
housingdata_1317 <- select(housingdata_1317, c(bc_code, `2013`, `2017`))

# Convert to long data, needed to merge with 'buurtdata'
housingdata_0509 <- pivot_longer(housingdata_0509, cols = `2005`:`2009`, names_to = "jaar", values_to = "WVOORRBAG")
housingdata_1317 <- pivot_longer(housingdata_1317, cols = `2013`:`2017`, names_to = "jaar", values_to = "WVOORRBAG")

# Combine the two datasets
housingdata <- rbind(housingdata_0509, housingdata_1317)

# Set WVOORRBAG to numeric
housingdata$WVOORRBAG <- as.numeric(housingdata$WVOORRBAG)

# BUURTDATA

# Subset to neighbourhood-level data only ('Wijken')
unique(buurtdata$niveaunaam)
buurtdata <- buurtdata %>% filter(buurtdata$niveaunaam == "Wijken")

# Select relevant variables - drop all others
independentvars <- c("gebiedcode15", "gebiednaam", "jaar", "BEVTOTAAL", "BEVSUR", "BEVANTIL", 
                     "BEVTURK", "BEVMAROK", "BEVOVNW", "BEVWEST", 
                     "BEVAUTOCH", "BEV0_18", "BEV18_26", "BEV27_65", 
                     "BEV66PLUS", "BEVOPLLAAG_P", "BEVOPLMID_P", 
                     "BEVOPLHOOG_P", "BEV15_19", "BEV20_24", "BEV25_29",
                     "BEV30_34", "BEV35_39", "BEV40_44", "BEV45_49", 
                     "BEV50_54", "BEV55_59", "BEV60_64", "BEV65_69",
                     "BEV70_74", "PREGWERKL", "BEVPOTBBV15_65", "BEVPOTBBV15_64", "WHUURTSLG_P", "WCORHUUR")
buurtdata <- buurtdata[independentvars]
# NOTE: Unemployment information is unavailable for 2005 and 2009 (from 2010 onwards).
# NOTE: Education information is unavailable for 2006.

# Merge housing stock data into 'buurtdata'
buurtdata <- buurtdata %>% rename(bc_code = gebiedcode15) # rename merge variable 
buurtdata <- merge(buurtdata, housingdata, by=c("bc_code", "jaar"), all = TRUE)

# Create variable for percentage of population aged 15-74
# This variable is needed to convert the education variables from relative to absolute numbers,
# which is necessary when buurtcombinatie observations are combined (only those observations with N/A)
buurtdata$BEV15_74 <- (buurtdata$BEV15_19 + buurtdata$BEV20_24 + buurtdata$BEV25_29 + buurtdata$BEV30_34 +
                         buurtdata$BEV35_39 + buurtdata$BEV40_44 + buurtdata$BEV45_49 + buurtdata$BEV50_54 +
                         buurtdata$BEV55_59 + buurtdata$BEV60_64 + buurtdata$BEV65_69 + buurtdata$BEV70_74)

# Transform relative education variables into absolute variables
buurtdata$BEVOPLLAAG <- (buurtdata$BEVOPLLAAG_P * buurtdata$BEV15_74) / 100
buurtdata$BEVOPLMID  <- (buurtdata$BEVOPLMID_P  * buurtdata$BEV15_74) / 100
buurtdata$BEVOPLHOOG <- (buurtdata$BEVOPLHOOG_P * buurtdata$BEV15_74) / 100

# Transform relative social housing variables into absolute variables
buurtdata$WHUURTSLG <- (buurtdata$WHUURTSLG_P * buurtdata$WVOORRBAG) / 100 

# Drop relative variables 
buurtdata = subset(buurtdata, select = -c(BEVOPLLAAG_P, BEVOPLMID_P, BEVOPLHOOG_P, WHUURTSLG_P))

# Rename _ with -, because we later want there to only be one underscore
names(buurtdata) <- str_replace(names(buurtdata), "_", "-")

# Create different data frames for relevant years
buurtdata2005 <- buurtdata %>% filter(buurtdata$jaar == 2005)
buurtdata2009 <- buurtdata %>% filter(buurtdata$jaar == 2009)
buurtdata2013 <- buurtdata %>% filter(buurtdata$jaar == 2013)
buurtdata2017 <- buurtdata %>% filter(buurtdata$jaar == 2017)

# Add employment data to buurtdata
buurtdata2005 <- merge(buurtdata2005, employ_2005, by="bc-code", all=TRUE)
buurtdata2009 <- merge(buurtdata2009, employ_2009, by="bc-code", all=TRUE)
buurtdata2013 <- merge(buurtdata2013, employ_2013, by="bc-code", all=TRUE)
buurtdata2017 <- merge(buurtdata2017, employ_2017, by="bc-code", all=TRUE)

# Add years to variable names
names(buurtdata2005) <- paste0(names(buurtdata2005), "_2005")
names(buurtdata2009) <- paste0(names(buurtdata2009), "_2009")
names(buurtdata2013) <- paste0(names(buurtdata2013), "_2013")
names(buurtdata2017) <- paste0(names(buurtdata2017), "_2017")

# NEIGHBOURHOOD + VOTE SHARES - Merging data ---------------------------------------------------------------------------

# Rename merge variables
subdata       <- subdata       %>% rename(bc_code = bc_code_2018)
buurtdata2005 <- buurtdata2005 %>% rename(bc_code = `bc-code_2005`)
buurtdata2009 <- buurtdata2009 %>% rename(bc_code = `bc-code_2009`)
buurtdata2013 <- buurtdata2013 %>% rename(bc_code = `bc-code_2013`)
buurtdata2017 <- buurtdata2017 %>% rename(bc_code = `bc-code_2017`)

# Merge vote share data + neighbourhood data 
subdata_buurt <- merge(subdata,       buurtdata2005, by="bc_code", all=TRUE)
subdata_buurt <- merge(subdata_buurt, buurtdata2009, by="bc_code", all=TRUE)
subdata_buurt <- merge(subdata_buurt, buurtdata2013, by="bc_code", all=TRUE)
subdata_buurt <- merge(subdata_buurt, buurtdata2017, by="bc_code", all=TRUE)

# NEIGHBOURHOOD + VOTE SHARES - Merging neighbourhoods ------------------------------------------------------------------

# Back-up data 
subdata_buurt_backup <- subdata_buurt
subdata_buurt <- subdata_buurt_backup

# Drop redundant variables: jaar, gebiednaam & 2006-2014 BC codes
subdata_buurt = subset(subdata_buurt, select = -c(jaar_2005, gebiednaam_2005, jaar_2009, gebiednaam_2009, jaar_2013, 
                                                  gebiednaam_2013, jaar_2017, gebiednaam_2017, bc_2006, bc_2010, bc_2014))

# Replace missings with empty string for bc_code and bc_naam
subdata_buurt$bc_code <- ifelse(is.na(subdata_buurt$bc_code), "", subdata_buurt$bc_code)
subdata_buurt$bc_naam <- ifelse(is.na(subdata_buurt$bc_naam), "", subdata_buurt$bc_naam)

# ----------------------------------------------------------------------------------------------------------------------------------------------------------------

# Create dummy variable: indicate whether neighbourhood was created through combination of others (by hand, see below)
subdata_buurt$BCcombined <- 0

# Rename all neighbourhoods to be merged
# Specify renaming function
rename.bc <- function(x, condition, code, name, combined = FALSE){
  x$bc_code <- ifelse(condition, code, x$bc_code) # change BC code
  x$bc_naam <- ifelse(condition, name, x$bc_naam) # change BC name
  if(combined){x$BCcombined <- ifelse(condition, 1, x$BCcombined)}
  x
}

# De Krommert: Chassébuurt + Geuzenbuurt
condition     <- subdata_buurt$bc_naam == "De Krommert" | subdata_buurt$bc_naam == "Chassébuurt" | subdata_buurt$bc_naam == "Geuzenbuurt"
subdata_buurt <- rename.bc(subdata_buurt, condition, "E40+E75", "De Krommert: Chassébuurt + Geuzenbuurt", TRUE)

# Diamantbuurt/Zuid Pijp
condition     <- subdata_buurt$bc_naam == "Diamantbuurt" | subdata_buurt$bc_naam == "Zuid Pijp"
subdata_buurt <- rename.bc(subdata_buurt, condition, "K26", "Diamantbuurt/Zuid Pijp")

# Museumkwartier + Duivelseiland 
condition     <- subdata_buurt$bc_naam == "Duivelseiland" | subdata_buurt$bc_naam == "Museumkwartier" | subdata_buurt$bc_naam == "Museumkwartier + Duivelseiland" | subdata_buurt$bc_code == "K50"
subdata_buurt <- rename.bc(subdata_buurt, condition, "K47+K50", "Museumkwartier + Duivelseiland", TRUE)

# Buikslotermeer + Elzenhagen
condition     <- subdata_buurt$bc_naam == "Elzenhagen" | subdata_buurt$bc_naam == "Buikslotermeer"
subdata_buurt <- rename.bc(subdata_buurt, condition, "N69+N74", "Buikslotermeer + Elzenhagen", TRUE)

# Frankendael + De Omval/Overamstel
condition     <- subdata_buurt$bc_naam == "Frankendael" | subdata_buurt$bc_naam == "Frankendael + De Omval" | subdata_buurt$bc_naam == "De Omval" | subdata_buurt$bc_naam == "Omval/Overamstel"
subdata_buurt <- rename.bc(subdata_buurt, condition, "M55+M58", "Frankendael + De Omval/Overamstel", TRUE)

# IJburg West + Zeeburgereiland/Nieuwe Diep + Indische Buurt Oost
condition     <- subdata_buurt$bc_naam == "IJburg West" | subdata_buurt$bc_naam == "IJburg West + Zeeburgereiland/Nieuwe Diep" | subdata_buurt$bc_naam == "Indische Buurt Oost" | subdata_buurt$bc_naam == "Indische Buurt Oost + Zeeburgereiland/Nieuwe Diep" | subdata_buurt$bc_naam == "Zeeburgereiland/Nieuwe Diep"
subdata_buurt <- rename.bc(subdata_buurt, condition, "M32+M34+M35", "IJburg West + Zeeburgereiland/Nieuwe Diep + Indische Buurt Oost", TRUE)

# IJplein/Vogelbuurt + Nieuwendammerham/Noordelijke IJ-oevers Oost
condition     <- subdata_buurt$bc_naam == "IJplein/Vogelbuurt + Nieuwendammerham" | subdata_buurt$bc_naam == "IJplein/Vogelbuurt + Noordelijke IJ-oevers Oost" | subdata_buurt$bc_code == "N61" | subdata_buurt$bc_code == "N72"
subdata_buurt <- rename.bc(subdata_buurt, condition, "N61+N72", "IJplein/Vogelbuurt + Nieuwendammerham/Noordelijke IJ-oevers Oost", TRUE)

# Middelveldsche Akerpolder/Sloten
condition     <- subdata_buurt$bc_naam == "Middelveldsche Akerpolder" | subdata_buurt$bc_naam == "Middelveldsche Akerpolder/Sloten"
subdata_buurt <- rename.bc(subdata_buurt, condition, "F84", "Middelveldsche Akerpolder/Sloten")

# Nieuwendam-Noord/Waterlandpleinbuurt
condition     <- subdata_buurt$bc_naam == "Nieuwendam-Noord" | subdata_buurt$bc_naam == "Waterlandpleinbuurt"
subdata_buurt <- rename.bc(subdata_buurt, condition, "N68", "Nieuwendam-Noord/Waterlandpleinbuurt")

# Prinses Irenebuurt e.o./Station Zuid/WTC e.o.
condition     <- subdata_buurt$bc_naam == "Prinses Irenebuurt e.o." | subdata_buurt$bc_naam == "Station Zuid/WTC e.o."
subdata_buurt <- rename.bc(subdata_buurt, condition, "K59", "Prinses Irenebuurt e.o./Station Zuid/WTC e.o.")

# Slotermeer-Noordoost + Spieringhorn + Westelijk Havengebied + Bedrijventerrein Sloterdijk
condition     <- subdata_buurt$bc_naam == "Slotermeer-Noordoost" | subdata_buurt$bc_naam == "Slotermeer-Noordoost + Spieringhorn" | subdata_buurt$bc_naam == "Westelijk Havengebied + Bedrijventerrein Sloterdijk" | subdata_buurt$bc_code == "B10" | subdata_buurt$bc_code == "F11" | subdata_buurt$bc_code == "B11" | subdata_buurt$bc_code == "F75"
subdata_buurt <- rename.bc(subdata_buurt, condition, "F76+F75+B10+F11", "Slotermeer-Noordoost + Spieringhorn + Westelijk Havengebied + Bedrijventerrein Sloterdijk", TRUE)

# Slotervaart: Slotervaart Noord + Slotervaart Zuid
condition     <- subdata_buurt$bc_naam == "Slotervaart" | subdata_buurt$bc_naam == "Slotervaart Noord" | subdata_buurt$bc_naam == "Slotervaart Zuid"
subdata_buurt <- rename.bc(subdata_buurt, condition, "F85+F89", "Slotervaart: Slotervaart Noord + Slotervaart Zuid", TRUE)

# Volewijck + Buiksloterham/Volewijck + Noordelijke IJ-oevers West
condition     <- subdata_buurt$bc_naam == "Volewijck + Buiksloterham" | subdata_buurt$bc_naam == "Volewijck + Noordelijke IJ-oevers West" | subdata_buurt$bc_code == "N60" | subdata_buurt$bc_code == "N71"
subdata_buurt <- rename.bc(subdata_buurt, condition, "N60+N71", "Volewijck + Buiksloterham/Volewijck + Noordelijke IJ-oevers West", TRUE)

# Following neighbourhoods were consistent in vote share data, but failed to merge with the neighbourhood characteristics data (e.g. not in neighbourhood data as a combined code)
# Spaarndammer- en Zeeheldenbuurt + Houthavens
condition     <- subdata_buurt$bc_code == "E12" | subdata_buurt$bc_code == "E13" | subdata_buurt$bc_code == "E13+E12"
subdata_buurt <- rename.bc(subdata_buurt, condition, "E13+12", "Spaarndammer- en Zeeheldenbuurt + Houthavens")

# Landlust + Sloterdijk
condition     <- subdata_buurt$bc_code == "E36" | subdata_buurt$bc_code == "E37" | subdata_buurt$bc_code == "E37+E36"
subdata_buurt <- rename.bc(subdata_buurt, condition, "E37+E36", "Landlust + Sloterdijk") 

# Tuindorp Buiksloot + Nieuwendammerdijk/Buiksloterdijk
condition     <- subdata_buurt$bc_code == "N63+N64" | subdata_buurt$bc_code == "N63" | subdata_buurt$bc_code == "N64"
subdata_buurt <- rename.bc(subdata_buurt, condition, "N63+N64", "Tuindorp Buiksloot + Nieuwendammerdijk/Buiksloterdijk")

# Holendrecht/Reigersbos + Amstel III/Bullewijk
condition     <- subdata_buurt$bc_code == "T96+T92" | subdata_buurt$bc_code == "T96" | subdata_buurt$bc_code == "T92"
subdata_buurt <- rename.bc(subdata_buurt, condition, "T96+T92", "Holendrecht/Reigersbos + Amstel III/Bullewijk")

# ----------------------------------------------------------------------------------------------------------------------------------------------------------------

# Drop neighbourhoods 
# 'M50' is not present in the voting data, 'Z99' and 'X99' are false codes or non-existent neighbourhood
# 'K23' ('Zuidas') is dropped as it was newly created in 2018 from 4 constitutive parts (K59, K52, K90 and K91) which also continue to exist
subdata_buurt <- subdata_buurt[subdata_buurt$bc_code != "M50" & subdata_buurt$bc_code != "Z99" & subdata_buurt$bc_code != "K23" & subdata_buurt$bc_code != "X99",]

# New back-up of data
subdata_buurt_backup2 <- subdata_buurt
subdata_buurt <- subdata_buurt_backup2

# Aggregate all neighbourhoods to be merged 
# Aggregate function cannot deal with NA -- use 999999 as placeholder for 0 to retain NAs in merged data
iszero <- function(x) {x== 0}
placeholder <- 999999 # make 999999 constant
subdata_buurt[iszero(subdata_buurt)] <- placeholder # set 0 to 999999
subdata_buurt[is.na(subdata_buurt)] <- 0 # set missing to 0
subdata_buurt <- aggregate(subdata_buurt[,-c(1:2)], by=list(bc_code=subdata_buurt$bc_code, bc_naam=subdata_buurt$bc_naam), FUN=sum) # aggregate data with sum
subdata_buurt[iszero(subdata_buurt)] <- NA # set 0 to missing
subdata_buurt[,-c(1:2)] <- subdata_buurt[,-c(1:2)] %% placeholder # all modulo 999999

# Dummify BCcombined again, since 'aggregate' summed observations and it no longer 0 or 1 for all
subdata_buurt$BCcombined <- ifelse(subdata_buurt$BCcombined != 0, 1, 0)

# Clone BCcombined so there are separate variables for 2005, 2009, 2013 and 2017: needed for restructuring to long data
subdata_buurt$BCcombined_2009 <- subdata_buurt$BCcombined
subdata_buurt$BCcombined_2013 <- subdata_buurt$BCcombined
subdata_buurt$BCcombined_2017 <- subdata_buurt$BCcombined
subdata_buurt <- subdata_buurt %>% rename(BCcombined_2005 = BCcombined)

# COMPLETE DATASET - Percentage variables ----------------------------------------------------------------

# Make relative unemployment variable: share of WWB receivers in the working-age population (15-64)
subdata_buurt$WWB_2005 <- (subdata_buurt$employ_2005 / subdata_buurt$`BEVPOTBBV15-64_2005`) * 100
subdata_buurt$WWB_2009 <- (subdata_buurt$employ_2009 / subdata_buurt$`BEVPOTBBV15-64_2009`) * 100 
subdata_buurt$WWB_2013 <- (subdata_buurt$employ_2013 / subdata_buurt$`BEVPOTBBV15-64_2013`) * 100 
subdata_buurt$WWB_2017 <- (subdata_buurt$employ_2017 / subdata_buurt$`BEVPOTBBV15-64_2017`) * 100 

# Housing variables - divide by WVOORRBAG
house_vars <- startsWith(names(subdata_buurt), "WHUUR") | startsWith(names(subdata_buurt), "WCOR")
house_vars_2005 <- house_vars & endsWith(names(subdata_buurt), "2005")
house_vars_2009 <- house_vars & endsWith(names(subdata_buurt), "2009")
house_vars_2013 <- house_vars & endsWith(names(subdata_buurt), "2013")
house_vars_2017 <- house_vars & endsWith(names(subdata_buurt), "2017")

subdata_buurt[,house_vars_2005] <- (subdata_buurt[,house_vars_2005] / subdata_buurt$WVOORRBAG_2005) * 100
subdata_buurt[,house_vars_2009] <- (subdata_buurt[,house_vars_2009] / subdata_buurt$WVOORRBAG_2009) * 100
subdata_buurt[,house_vars_2013] <- (subdata_buurt[,house_vars_2013] / subdata_buurt$WVOORRBAG_2013) * 100
subdata_buurt[,house_vars_2017] <- (subdata_buurt[,house_vars_2017] / subdata_buurt$WVOORRBAG_2017) * 100

# Unemployment variable - divide by BEV15-65
work_vars      <- startsWith(names(subdata_buurt), "PREG")
work_vars_2005 <- work_vars & endsWith(names(subdata_buurt), "2005")
work_vars_2009 <- work_vars & endsWith(names(subdata_buurt), "2009")
work_vars_2013 <- work_vars & endsWith(names(subdata_buurt), "2013")
work_vars_2017 <- work_vars & endsWith(names(subdata_buurt), "2017")

subdata_buurt[,work_vars_2005] <- (subdata_buurt[,work_vars_2005] / subdata_buurt$`BEVPOTBBV15-65_2005`) * 100
subdata_buurt[,work_vars_2009] <- (subdata_buurt[,work_vars_2009] / subdata_buurt$`BEVPOTBBV15-65_2009`) * 100
subdata_buurt[,work_vars_2013] <- (subdata_buurt[,work_vars_2013] / subdata_buurt$`BEVPOTBBV15-65_2013`) * 100
subdata_buurt[,work_vars_2017] <- (subdata_buurt[,work_vars_2017] / subdata_buurt$`BEVPOTBBV15-65_2017`) * 100

# Education variables - divide by BEV15-74
edu_vars      <- startsWith(names(subdata_buurt), "BEVOPL")
edu_vars_2005 <- edu_vars & endsWith(names(subdata_buurt), "2005")
edu_vars_2009 <- edu_vars & endsWith(names(subdata_buurt), "2009")
edu_vars_2013 <- edu_vars & endsWith(names(subdata_buurt), "2013")
edu_vars_2017 <- edu_vars & endsWith(names(subdata_buurt), "2017")

subdata_buurt[,edu_vars_2005] <- (subdata_buurt[,edu_vars_2005] / subdata_buurt$`BEV15-74_2005`) * 100
subdata_buurt[,edu_vars_2009] <- (subdata_buurt[,edu_vars_2009] / subdata_buurt$`BEV15-74_2009`) * 100
subdata_buurt[,edu_vars_2013] <- (subdata_buurt[,edu_vars_2013] / subdata_buurt$`BEV15-74_2013`) * 100
subdata_buurt[,edu_vars_2017] <- (subdata_buurt[,edu_vars_2017] / subdata_buurt$`BEV15-74_2017`) * 100

# Population characteristics - divide by BEVTOTAAL
buurt_vars <- startsWith(names(subdata_buurt), "BEV") & !edu_vars & !startsWith(names(subdata_buurt), "BEVTOTAAL")
buurt_vars_2005 <- buurt_vars & endsWith(names(subdata_buurt), "2005")
buurt_vars_2009 <- buurt_vars & endsWith(names(subdata_buurt), "2009")
buurt_vars_2013 <- buurt_vars & endsWith(names(subdata_buurt), "2013")
buurt_vars_2017 <- buurt_vars & endsWith(names(subdata_buurt), "2017")

subdata_buurt[,buurt_vars_2005] <- (subdata_buurt[,buurt_vars_2005] / subdata_buurt$BEVTOTAAL_2005) * 100
subdata_buurt[,buurt_vars_2009] <- (subdata_buurt[,buurt_vars_2009] / subdata_buurt$BEVTOTAAL_2009) * 100
subdata_buurt[,buurt_vars_2013] <- (subdata_buurt[,buurt_vars_2013] / subdata_buurt$BEVTOTAAL_2013) * 100
subdata_buurt[,buurt_vars_2017] <- (subdata_buurt[,buurt_vars_2017] / subdata_buurt$BEVTOTAAL_2017) * 100

# Rename PvdA to PVDA for consistency
names(subdata_buurt) <- str_replace(names(subdata_buurt), "PvdA", "PVDA")

# Vote shares - divide by total number of valid votes 
subdata_buurt$PVDA_2006 <- (subdata_buurt$PVDA_2006 / subdata_buurt$totaal_2006) * 100
subdata_buurt$PVDA_2010 <- (subdata_buurt$PVDA_2010 / subdata_buurt$totaal_2010) * 100
subdata_buurt$PVDA_2014 <- (subdata_buurt$PVDA_2014 / subdata_buurt$totaal_2014) * 100
subdata_buurt$PVDA_2018 <- (subdata_buurt$PVDA_2018 / subdata_buurt$totaal_2018) * 100
subdata_buurt$MPP_2014  <- (subdata_buurt$MPP_2014  / subdata_buurt$totaal_2014) * 100
subdata_buurt$DENK_2018 <- (subdata_buurt$DENK_2018 / subdata_buurt$totaal_2018) * 100
subdata_buurt$BIJ1_2018 <- (subdata_buurt$BIJ1_2018 / subdata_buurt$totaal_2018) * 100
subdata_buurt$GL_2006   <- (subdata_buurt$GL_2006   / subdata_buurt$totaal_2006) * 100
subdata_buurt$GL_2010   <- (subdata_buurt$GL_2010   / subdata_buurt$totaal_2010) * 100
subdata_buurt$GL_2014   <- (subdata_buurt$GL_2014   / subdata_buurt$totaal_2014) * 100
subdata_buurt$GL_2018   <- (subdata_buurt$GL_2018   / subdata_buurt$totaal_2018) * 100

# Variable for total vote share for multicultural parties: add vote shares for MPP (2014), DENK and BIJ1 (both 2018)
# There are no multicultural parties represented in the 2006 and 2010 elections
subdata_buurt$MCparties_2014 <- subdata_buurt$MPP_2014                             # For 2014: only the vote share of M+
subdata_buurt$MCparties_2018 <- subdata_buurt$DENK_2018 + subdata_buurt$BIJ1_2018  # For 2018: add vote shares of DENK and BIJ1
  
# Variables for turnout
subdata_buurt$turnout_2006 <- (subdata_buurt$totaal_2006 / subdata_buurt$kies.gerechtigden_2006) * 100
subdata_buurt$turnout_2010 <- (subdata_buurt$totaal_2010 / subdata_buurt$kiesge.rechtigden_2010) * 100
subdata_buurt$turnout_2014 <- (subdata_buurt$totaal_2014 / subdata_buurt$kg_2014)                * 100
subdata_buurt$turnout_2018 <- (subdata_buurt$totaal_2018 / subdata_buurt$opgeroepenen_2018)      * 100

# Variables for changes in social housing
subdata_buurt$WHUURTSLGdelta2005_2009 <- subdata_buurt$WHUURTSLG_2009 - subdata_buurt$WHUURTSLG_2005 # all differences with 2005
subdata_buurt$WHUURTSLGdelta2005_2013 <- subdata_buurt$WHUURTSLG_2013 - subdata_buurt$WHUURTSLG_2005
subdata_buurt$WHUURTSLGdelta2005_2017 <- subdata_buurt$WHUURTSLG_2017 - subdata_buurt$WHUURTSLG_2005

subdata_buurt$WHUURTSLGdelta2009_2013 <- subdata_buurt$WHUURTSLG_2013 - subdata_buurt$WHUURTSLG_2009 # all differences with 2009
subdata_buurt$WHUURTSLGdelta2009_2017 <- subdata_buurt$WHUURTSLG_2017 - subdata_buurt$WHUURTSLG_2009

subdata_buurt$WHUURTSLGdelta2013_2017 <- subdata_buurt$WHUURTSLG_2017 - subdata_buurt$WHUURTSLG_2013 # all differences with 2013

subdata_buurt$`WHUURTSLG_t-1_2009` <- subdata_buurt$WHUURTSLG_2009 - subdata_buurt$WHUURTSLG_2005 # all differences with t-1
subdata_buurt$`WHUURTSLG_t-1_2013` <- subdata_buurt$WHUURTSLG_2013 - subdata_buurt$WHUURTSLG_2009
subdata_buurt$`WHUURTSLG_t-1_2017` <- subdata_buurt$WHUURTSLG_2017 - subdata_buurt$WHUURTSLG_2013

# Variables for changes in public housing
subdata_buurt$`WCORHUURt-1_2009` <- subdata_buurt$WCORHUUR_2009 - subdata_buurt$WCORHUUR_2005 # all differences with t-1
subdata_buurt$`WCORHUURt-1_2013` <- subdata_buurt$WCORHUUR_2013 - subdata_buurt$WCORHUUR_2009
subdata_buurt$`WCORHUURt-1_2017` <- subdata_buurt$WCORHUUR_2017 - subdata_buurt$WCORHUUR_2013

# Variables for changes in party support
subdata_buurt$GLdelta2010_2018 <- subdata_buurt$GL_2018 - subdata_buurt$GL_2010
subdata_buurt$GLdelta2014_2018 <- subdata_buurt$GL_2018 - subdata_buurt$GL_2014

subdata_buurt$PVDAdelta2006_2010 <- subdata_buurt$PVDA_2010 - subdata_buurt$PVDA_2006 # all differences with 2006
subdata_buurt$PVDAdelta2006_2014 <- subdata_buurt$PVDA_2014 - subdata_buurt$PVDA_2006
subdata_buurt$PVDAdelta2006_2018 <- subdata_buurt$PVDA_2018 - subdata_buurt$PVDA_2006

subdata_buurt$PVDAdelta2010_2014 <- subdata_buurt$PVDA_2014 - subdata_buurt$PVDA_2010 # all differences with 2010
subdata_buurt$PVDAdelta2010_2018 <- subdata_buurt$PVDA_2018 - subdata_buurt$PVDA_2010

subdata_buurt$PVDAdelta2014_2018 <- subdata_buurt$PVDA_2018 - subdata_buurt$PVDA_2014 # all differences with 2014

subdata_buurt$`PVDAt-1_2010` <- subdata_buurt$PVDA_2010 - subdata_buurt$PVDA_2006 # all differences with t-1
subdata_buurt$`PVDAt-1_2014` <- subdata_buurt$PVDA_2014 - subdata_buurt$PVDA_2010
subdata_buurt$`PVDAt-1_2018` <- subdata_buurt$PVDA_2018 - subdata_buurt$PVDA_2014

# COMPLETE DATASET - Reshaping ---------------------------------------------------------------------------

# Rename 2005, 2009, 2013, 2017 to +1
names(subdata_buurt) <- str_replace(names(subdata_buurt), "2005", "2006")
names(subdata_buurt) <- str_replace(names(subdata_buurt), "2009", "2010")
names(subdata_buurt) <- str_replace(names(subdata_buurt), "2013", "2014")
names(subdata_buurt) <- str_replace(names(subdata_buurt), "2017", "2018")

# Reshape all year-dependent variables to long structure 
subdata_buurt_longest <- subdata_buurt %>% pivot_longer(
  cols = kies.gerechtigden_2006:`PVDAt-1_2018`, # Change if adding more variables to dataset!
  names_to = c("kolomnaam", "jaar"), 
  names_pattern = "(.*)_(.*)",
  values_to = "waarde"
)

# Recover columns for year-dependent variables 
subdata_buurt_long <- subdata_buurt_longest %>% pivot_wider(
  names_from = kolomnaam,
  values_from = waarde
)

# COMPLETE DATASET - Tidy & export ---------------------------------------------------------------------------

# Add variable indicating measurement year for neighbourhood variables (election year - 1)
subdata_buurt_long$jaar           <- as.numeric(subdata_buurt_long$jaar)
subdata_buurt_long$jaar_buurtvars <- subdata_buurt_long$jaar - 1

# Drop redundant variables, i.e. not needed for analysis
subdata_buurt_long = subset(subdata_buurt_long, select = -c(totaal, BEVTOTAAL, `BEV15-19`, `BEV20-24`, 
                                                            `BEV25-29`, `BEV30-34`, `BEV35-39`, 
                                                            `BEV40-44`, `BEV45-49`, `BEV50-54`, 
                                                            `BEV55-59`, `BEV60-64`, `BEV65-69`, 
                                                            `BEV70-74`, `BEVPOTBBV15-65`, `BEV15-74`, 
                                                            WVOORRBAG, kg, kiesge.rechtigden, opgeroepenen,
                                                            kies.gerechtigden))

# Rename variables 
subdata_buurt_long <- subdata_buurt_long %>% rename(bc_name                = bc_naam)
subdata_buurt_long <- subdata_buurt_long %>% rename(year                   = jaar)
subdata_buurt_long <- subdata_buurt_long %>% rename(year_BCvars            = jaar_buurtvars)
subdata_buurt_long <- subdata_buurt_long %>% rename(imm_Sur                = BEVSUR)
subdata_buurt_long <- subdata_buurt_long %>% rename(imm_Ant                = BEVANTIL)
subdata_buurt_long <- subdata_buurt_long %>% rename(imm_Tur                = BEVTURK)
subdata_buurt_long <- subdata_buurt_long %>% rename(imm_Mar                = BEVMAROK)
subdata_buurt_long <- subdata_buurt_long %>% rename(imm_otherNW            = BEVOVNW)
subdata_buurt_long <- subdata_buurt_long %>% rename(imm_W                  = BEVWEST)
subdata_buurt_long <- subdata_buurt_long %>% rename(imm_autoch             = BEVAUTOCH)
subdata_buurt_long <- subdata_buurt_long %>% rename(age_0t18               = `BEV0-18`)
subdata_buurt_long <- subdata_buurt_long %>% rename(age_18t26              = `BEV18-26`)
subdata_buurt_long <- subdata_buurt_long %>% rename(age_27t65              = `BEV27-65`)
subdata_buurt_long <- subdata_buurt_long %>% rename(age_66plus             = BEV66PLUS)
subdata_buurt_long <- subdata_buurt_long %>% rename(unempl                 = PREGWERKL)
subdata_buurt_long <- subdata_buurt_long %>% rename(edu_low                = BEVOPLLAAG)
subdata_buurt_long <- subdata_buurt_long %>% rename(edu_mid                = BEVOPLMID)
subdata_buurt_long <- subdata_buurt_long %>% rename(edu_high               = BEVOPLHOOG)
subdata_buurt_long <- subdata_buurt_long %>% rename(housing_soc            = WHUURTSLG)
subdata_buurt_long <- subdata_buurt_long %>% rename(housing_soc_delta2005  = WHUURTSLGdelta2006)
subdata_buurt_long <- subdata_buurt_long %>% rename(housing_soc_delta2009  = WHUURTSLGdelta2010)
subdata_buurt_long <- subdata_buurt_long %>% rename(housing_soc_delta2013  = WHUURTSLGdelta2014)
subdata_buurt_long <- subdata_buurt_long %>% rename(PVDA_delta2006         = PVDAdelta2006)
subdata_buurt_long <- subdata_buurt_long %>% rename(PVDA_delta2010         = PVDAdelta2010)
subdata_buurt_long <- subdata_buurt_long %>% rename(PVDA_delta2014         = PVDAdelta2014)       
subdata_buurt_long <- subdata_buurt_long %>% rename(bc_combined            = BCcombined)
subdata_buurt_long <- subdata_buurt_long %>% rename(housing_soc_delta      = `WHUURTSLG_t-1`)
subdata_buurt_long <- subdata_buurt_long %>% rename(PVDA_delta             = `PVDAt-1`)
subdata_buurt_long <- subdata_buurt_long %>% rename(housing_pub_delta     = `WCORHUURt-1`)

# Reorder columns
# TO CHANGE!
#subdata_buurt_long <- subdata_buurt_long[,c(1:3,33,4:7,25,29:33,8:23,26:28,24)] # Change if adding more variables to dataset!
#subdata_buurt_long <- subdata_buurt_long[,c(1:3,34,4:7,25,30:33,8:23,26:29,24)] 
  
# Export long data
write.csv(subdata_buurt_long,"/Users/Maartje/Desktop/LJA/data_sub_merged_long.csv", row.names = FALSE)


