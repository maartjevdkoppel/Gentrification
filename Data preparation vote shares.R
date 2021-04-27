# Set-up ---------------------------------------------------------------------------------------
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

# Subset to PvdA & multicultural parties only
subdata <- data[,c(1:5,8,9,22,23,45,72,74,79,66,86,97)]
subdata <- subdata[,c(1:5,7,9,10,13:16,6,8,11,12)] # reorder columns in subdata

# Export data to CSV (full & subset)
write.csv(data,   "/Users/Maartje/Desktop/LJA/data_allepartijen.csv", row.names = FALSE)
write.csv(subdata,"/Users/Maartje/Desktop/LJA/data_subset.csv",       row.names = FALSE)

# NEIGHBOURHOOD DATA - Reading data ---------------------------------------------------------------------------

# Read 'Basisbestand Gebieden Amsterdam' (BBGA) for neighbourhood variables
# Data can be retrieved from https://data.amsterdam.nl/datasets/G5JpqNbhweXZSw/basisbestand-gebieden-amsterdam-bbga/
columnnames <- names(read_xlsx("/Users/Maartje/Desktop/LJA/Data POLetmaal/Buurtkenmerken (versie 10-3-21).xlsx", n_max = 0))
columntypes <- ifelse(grepl("^[A-Z]", columnnames),"numeric", "guess")
buurtdata <- read_xlsx("/Users/Maartje/Desktop/LJA/Data POLetmaal/Buurtkenmerken (versie 10-3-21).xlsx", sheet = 1, col_names = TRUE, col_types = columntypes)

# NEIGHBOURHOOD DATA - Preparing data ---------------------------------------------------------------------------

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
                     "BEV70_74", "PREGWERKL")
buurtdata <- buurtdata[independentvars]
# NOTE: Unemployment information is unavailable for 2005 and 2009 (from 2010 onwards).
# NOTE: Education information is unavailable for 2006.

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

# Drop relative education variables 
buurtdata = subset(buurtdata, select = -c(BEVOPLLAAG_P, BEVOPLMID_P, BEVOPLHOOG_P))

# Rename _ with -, because we later want there to only be one underscore
names(buurtdata) <- str_replace(names(buurtdata), "_", "-")

# Create different data frames for relevant years
buurtdata2005 <- buurtdata %>% filter(buurtdata$jaar == 2005)
buurtdata2009 <- buurtdata %>% filter(buurtdata$jaar == 2009)
buurtdata2013 <- buurtdata %>% filter(buurtdata$jaar == 2013)
buurtdata2017 <- buurtdata %>% filter(buurtdata$jaar == 2017)

# Add years to variable names
names(buurtdata2005) <- paste0(names(buurtdata2005), "_2005")
names(buurtdata2009) <- paste0(names(buurtdata2009), "_2009")
names(buurtdata2013) <- paste0(names(buurtdata2013), "_2013")
names(buurtdata2017) <- paste0(names(buurtdata2017), "_2017")

# NEIGHBOURHOOD + VOTE SHARES - Merging data ---------------------------------------------------------------------------

# Rename merge variables
subdata       <- subdata       %>% rename(bc_code = bc_code_2018)
buurtdata2005 <- buurtdata2005 %>% rename(bc_code = gebiedcode15_2005)
buurtdata2009 <- buurtdata2009 %>% rename(bc_code = gebiedcode15_2009)
buurtdata2013 <- buurtdata2013 %>% rename(bc_code = gebiedcode15_2013)
buurtdata2017 <- buurtdata2017 %>% rename(bc_code = gebiedcode15_2017)

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

# Rename all neighbourhoods to be merged
# Specify renaming function
rename.bc <- function(x, condition, code, name){
  x$bc_code <- ifelse(condition, code, x$bc_code) # change BC code
  x$bc_naam <- ifelse(condition, name, x$bc_naam) # change BC name
  x
}

# De Krommert: Chassébuurt + Geuzenbuurt
condition     <- subdata_buurt$bc_naam == "De Krommert" | subdata_buurt$bc_naam == "Chassébuurt" | subdata_buurt$bc_naam == "Geuzenbuurt"
subdata_buurt <- rename.bc(subdata_buurt, condition, "E40+E75", "De Krommert: Chassébuurt + Geuzenbuurt")

# Diamantbuurt/Zuid Pijp
condition     <- subdata_buurt$bc_naam == "Diamantbuurt" | subdata_buurt$bc_naam == "Zuid Pijp"
subdata_buurt <- rename.bc(subdata_buurt, condition, "K26", "Diamantbuurt/Zuid Pijp")

# Museumkwartier + Duivelseiland 
condition     <- subdata_buurt$bc_naam == "Duivelseiland" | subdata_buurt$bc_naam == "Museumkwartier" | subdata_buurt$bc_naam == "Museumkwartier + Duivelseiland"
subdata_buurt <- rename.bc(subdata_buurt, condition, "K47+K50", "Museumkwartier + Duivelseiland")

# Buikslotermeer + Elzenhagen
condition     <- subdata_buurt$bc_naam == "Elzenhagen" | subdata_buurt$bc_naam == "Buikslotermeer"
subdata_buurt <- rename.bc(subdata_buurt, condition, "N69+N74", "Buikslotermeer + Elzenhagen")

# Frankendael + De Omval/Overamstel
condition     <- subdata_buurt$bc_naam == "Frankendael" | subdata_buurt$bc_naam == "Frankendael + De Omval" | subdata_buurt$bc_naam == "De Omval" | subdata_buurt$bc_naam == "Omval/Overamstel"
subdata_buurt <- rename.bc(subdata_buurt, condition, "M55+M58", "Frankendael + De Omval/Overamstel")

# IJburg West + Zeeburgereiland/Nieuwe Diep + Indische Buurt Oost
condition     <- subdata_buurt$bc_naam == "IJburg West" | subdata_buurt$bc_naam == "IJburg West + Zeeburgereiland/Nieuwe Diep" | subdata_buurt$bc_naam == "Indische Buurt Oost" | subdata_buurt$bc_naam == "Indische Buurt Oost + Zeeburgereiland/Nieuwe Diep" | subdata_buurt$bc_naam == "Zeeburgereiland/Nieuwe Diep"
subdata_buurt <- rename.bc(subdata_buurt, condition, "M32+M34+M35", "IJburg West + Zeeburgereiland/Nieuwe Diep + Indische Buurt Oost")

# IJplein/Vogelbuurt + Nieuwendammerham/Noordelijke IJ-oevers Oost
condition     <- subdata_buurt$bc_naam == "IJplein/Vogelbuurt + Nieuwendammerham" | subdata_buurt$bc_naam == "IJplein/Vogelbuurt + Noordelijke IJ-oevers Oost" | subdata_buurt$bc_code == "N61" | subdata_buurt$bc_code == "N72"
subdata_buurt <- rename.bc(subdata_buurt, condition, "N61+N72", "IJplein/Vogelbuurt + Nieuwendammerham/Noordelijke IJ-oevers Oost")

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
condition     <- subdata_buurt$bc_naam == "Slotermeer-Noordoost" | subdata_buurt$bc_naam == "Slotermeer-Noordoost + Spieringhorn" | subdata_buurt$bc_naam == "Westelijk Havengebied + Bedrijventerrein Sloterdijk" | subdata_buurt$bc_code == "B10" | subdata_buurt$bc_code == "F11"
subdata_buurt <- rename.bc(subdata_buurt, condition, "F76+F75+B10+F11", "Slotermeer-Noordoost + Spieringhorn + Westelijk Havengebied + Bedrijventerrein Sloterdijk")

# Slotervaart: Slotervaart Noord + Slotervaart Zuid
condition     <- subdata_buurt$bc_naam == "Slotervaart" | subdata_buurt$bc_naam == "Slotervaart Noord" | subdata_buurt$bc_naam == "Slotervaart Zuid"
subdata_buurt <- rename.bc(subdata_buurt, condition, "F85+F89", "Slotervaart: Slotervaart Noord + Slotervaart Zuid")

# Volewijck + Buiksloterham/Volewijck + Noordelijke IJ-oevers West
condition     <- subdata_buurt$bc_naam == "Volewijck + Buiksloterham" | subdata_buurt$bc_naam == "Volewijck + Noordelijke IJ-oevers West" | subdata_buurt$bc_code == "N60" | subdata_buurt$bc_code == "N71"
subdata_buurt <- rename.bc(subdata_buurt, condition, "N60+N71", "Volewijck + Buiksloterham/Volewijck + Noordelijke IJ-oevers West")

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

# Drop neighbourhoods 'M50' (not present in voting data) and 'Z99' (false BC code)
subdata_buurt <- subdata_buurt[subdata_buurt$bc_code != "M50" & subdata_buurt$bc_code != "Z99",]

# New back-up of data
subdata_buurt_backup <- subdata_buurt
subdata_buurt <- subdata_buurt_backup

# Aggregate all neighbourhoods to be merged 
# Aggregate function cannot deal with NA -- use 999999 as placeholder for 0 to retain NAs in merged data
iszero <- function(x) {x== 0}
placeholder <- 999999 # make 999999 constant
subdata_buurt[iszero(subdata_buurt)] <- placeholder # set 0 to 999999
subdata_buurt[is.na(subdata_buurt)] <- 0 # set missing to 0
subdata_buurt <- aggregate(subdata_buurt[,3:129], by=list(bc_code=subdata_buurt$bc_code, bc_naam=subdata_buurt$bc_naam), FUN=sum) # aggregate data with sum
subdata_buurt[iszero(subdata_buurt)] <- NA # set 0 to missing
subdata_buurt[,3:129] <- subdata_buurt[,3:129] %% placeholder # all modulo 999999

# COMPLETE DATASET - Percentage variables ----------------------------------------------------------------

# Neighbourhood characteristics 
buurt_vars_2005 <- c(15:42)
buurt_vars_2009 <- c(44:71)
buurt_vars_2013 <- c(73:100)
buurt_vars_2017 <- c(102:129)

subdata_buurt[,buurt_vars_2005] <- (subdata_buurt[,buurt_vars_2005] / subdata_buurt$BEVTOTAAL_2005) * 100
subdata_buurt[,buurt_vars_2009] <- (subdata_buurt[,buurt_vars_2009] / subdata_buurt$BEVTOTAAL_2009) * 100
subdata_buurt[,buurt_vars_2013] <- (subdata_buurt[,buurt_vars_2013] / subdata_buurt$BEVTOTAAL_2013) * 100
subdata_buurt[,buurt_vars_2017] <- (subdata_buurt[,buurt_vars_2017] / subdata_buurt$BEVTOTAAL_2017) * 100

# Vote shares 
subdata_buurt$PVDA_2006 <- (subdata_buurt$PVDA_2006 / subdata_buurt$totaal_2006) * 100
subdata_buurt$PvdA_2010 <- (subdata_buurt$PvdA_2010 / subdata_buurt$totaal_2010) * 100
subdata_buurt$PVDA_2014 <- (subdata_buurt$PVDA_2014 / subdata_buurt$totaal_2014) * 100
subdata_buurt$PvdA_2018 <- (subdata_buurt$PvdA_2018 / subdata_buurt$totaal_2018) * 100
subdata_buurt$MPP_2014  <- (subdata_buurt$MPP_2014  / subdata_buurt$totaal_2014) * 100
subdata_buurt$DENK_2018 <- (subdata_buurt$DENK_2018 / subdata_buurt$totaal_2018) * 100
subdata_buurt$BIJ1_2018 <- (subdata_buurt$BIJ1_2018 / subdata_buurt$totaal_2018) * 100

# Rename PvdA to PVDA for consistency
names(subdata_buurt) <- str_replace(names(subdata_buurt), "PvdA", "PVDA")

# Variables for changes in party support
subdata_buurt$PVDAdelta2006_2010 <- subdata_buurt$PVDA_2010 - subdata_buurt$PVDA_2006
subdata_buurt$PVDAdelta2006_2014 <- subdata_buurt$PVDA_2014 - subdata_buurt$PVDA_2006
subdata_buurt$PVDAdelta2006_2018 <- subdata_buurt$PVDA_2018 - subdata_buurt$PVDA_2006

subdata_buurt$PVDAdelta2010_2014 <- subdata_buurt$PVDA_2014 - subdata_buurt$PVDA_2010
subdata_buurt$PVDAdelta2010_2018 <- subdata_buurt$PVDA_2018 - subdata_buurt$PVDA_2010

subdata_buurt$PVDAdelta2014_2018 <- subdata_buurt$PVDA_2018 - subdata_buurt$PVDA_2014

# COMPLETE DATASET - Reshaping ---------------------------------------------------------------------------

# Rename 2005, 2009, 2013, 2017 to +1
names(subdata_buurt) <- str_replace(names(subdata_buurt), "2005", "2006")
names(subdata_buurt) <- str_replace(names(subdata_buurt), "2009", "2010")
names(subdata_buurt) <- str_replace(names(subdata_buurt), "2013", "2014")
names(subdata_buurt) <- str_replace(names(subdata_buurt), "2017", "2018")

# Reshape all year-dependent variables to long structure 
subdata_buurt_longest <- subdata_buurt %>% pivot_longer(
  cols = PVDA_2006:PVDAdelta2014_2018, # Change if adding more variables to dataset!
  names_to = c("kolomnaam", "jaar"), 
  names_pattern = "(.*)_(.*)",
  values_to = "waarde"
)

# Recover columns for year-dependent variables 
subdata_buurt_long <- subdata_buurt_longest %>% pivot_wider(
  names_from = kolomnaam,
  values_from = waarde
)

# Add variable indicating measurement year for neighbourhood variables (election year - 1)
subdata_buurt_long$jaar           <- as.numeric(subdata_buurt_long$jaar)
subdata_buurt_long$jaar_buurtvars <- subdata_buurt_long$jaar - 1
subdata_buurt_long                <- subdata_buurt_long[,c(1:3,41,4:40)] # Change if adding more variables to dataset!

# Export long data
write.csv(subdata_buurt_long,"/Users/Maartje/Desktop/LJA/data_sub_merged_long.csv", row.names = FALSE)

# TO DO
# V Get vote share data in absolute numbers
# V Correct all relative variables to absolute: only education variable?
# V Make into percentage variables again
# Collect gentrification data
# Collect missing education + unemployment data
# V Transform into long data
# V Create change in party support variables (absolute change)
