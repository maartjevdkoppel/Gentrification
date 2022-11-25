# Perils of “revitalization”? 
# Gentrification, political participation, and support for social democrats in Amsterdam 
#TODO: update title+date

# Maartje van de Koppel
# Descriptive maps
# Last update: 25/11/22


# Set-up ------------------------------------------------------------------------------------------------------------ 

# Set up working directory
# TODO remove
setwd("/Users/Maartje/Desktop/LJA/Paper politicologenetmaal/Link-Jong-Amsterdam/Data/Analysis")

# Load required packages
library(broom)        #tidying shapefiles to dataframe
library(raster)       #working with spatial data
library(tidyverse)    #tidyverse collection
library(ggplot2)      #for plots
library(readxl)       #importing excel files
library(plyr)         
library(dplyr)
library(tidyr)       
library(sf)           #reading shapefiles 
library(RColorBrewer) #colour scales for maps
library(gridExtra)    #combining ggplots into one figure

# Data ------------------------------------------------------------------------------------------------------------ 

#Import 2022 data
#Data can be obtained here: https://onderzoek.amsterdam.nl/dataset/verkiezingen-gemeenteraad-2022  
additional_2022 <- read_xlsx("2022_gemeenteraadsverkiezingen_wijk_stadsdeel_5cb9f5e19c.xlsx", skip = 1) 

election_2022 <- additional_2022 %>%
  select(wijkcode, wijknaam, kiesgerechtigden, `geldige stembiljetten`, `Partij van de Arbeid (P.v.d.A.)`) %>%
  mutate(turnout_2022 = (`geldige stembiljetten` / kiesgerechtigden)*100,
         PVDA_2022 = (`Partij van de Arbeid (P.v.d.A.)` / `geldige stembiljetten`)*100) %>%
  dplyr::rename(bc_code = wijkcode,
         bc_naam = wijknaam)

#Import shapefile for neighbourhoods (based on 2015 bc code)
geodata <- st_read("bc2015def_region.shp") %>%
  dplyr::rename(bc_code = BC2015,
                bc_naam = NAAM)

#Import neighbourhood data
#Read data
fulldata <- readRDS("gentrification_data_long_revised.rds")

#Subset to years 
subdata      <- fulldata[which(fulldata$year=='2018'), ]
subdata_2006 <- fulldata[which(fulldata$year=='2006'), ]
subdata_2010 <- fulldata[which(fulldata$year=='2010'), ]
subdata_2014 <- fulldata[which(fulldata$year=='2014'), ]

# MERGE UNITS: SPATIAL DATA  -----------------------------------

#Rename all neighbourhoods to be merged
#Specify renaming function
rename.bc <- function(x, condition, code, name){
  x$bc_code <- ifelse(condition, code, x$bc_code) # change BC code
  x$bc_naam <- ifelse(condition, name, x$bc_naam) # change BC name
  x
}

# De Krommert: Chassébuurt + Geuzenbuurt
condition     <- geodata$bc_naam == "De Krommert" | geodata$bc_naam == "Chassébuurt" | geodata$bc_naam == "Geuzenbuurt"
geodata <- rename.bc(geodata, condition, "E40+E75", "De Krommert: Chassébuurt + Geuzenbuurt")

# Diamantbuurt/Zuid Pijp
condition     <- geodata$bc_naam == "Diamantbuurt" | geodata$bc_naam == "Zuid Pijp"
geodata <- rename.bc(geodata, condition, "K26", "Diamantbuurt/Zuid Pijp")

# Museumkwartier + Duivelseiland 
condition     <- geodata$bc_naam == "Duivelseiland" | geodata$bc_naam == "Museumkwartier" | geodata$bc_naam == "Museumkwartier + Duivelseiland" | geodata$bc_code == "K50" | geodata$bc_code == "K47"
geodata <- rename.bc(geodata, condition, "K47+K50", "Museumkwartier + Duivelseiland")

# Buikslotermeer + Elzenhagen
condition     <- geodata$bc_naam == "Elzenhagen" | geodata$bc_naam == "Buikslotermeer"
geodata <- rename.bc(geodata, condition, "N69+N74", "Buikslotermeer + Elzenhagen")

# Frankendael + De Omval/Overamstel
condition     <- geodata$bc_naam == "Frankendael" | geodata$bc_naam == "Frankendael + De Omval" | geodata$bc_naam == "De Omval" | geodata$bc_naam == "Omval/Overamstel"
geodata <- rename.bc(geodata, condition, "M55+M58", "Frankendael + De Omval/Overamstel")

# IJburg West + Zeeburgereiland/Nieuwe Diep + Indische Buurt Oost
condition     <- geodata$bc_naam == "IJburg West" | geodata$bc_naam == "IJburg West + Zeeburgereiland/Nieuwe Diep" | geodata$bc_naam == "Indische Buurt Oost" | geodata$bc_naam == "Indische Buurt Oost + Zeeburgereiland/Nieuwe Diep" | geodata$bc_naam == "Zeeburgereiland/Nieuwe Diep"
geodata <- rename.bc(geodata, condition, "M32+M34+M35", "IJburg West + Zeeburgereiland/Nieuwe Diep + Indische Buurt Oost")

# IJplein/Vogelbuurt + Nieuwendammerham/Noordelijke IJ-oevers Oost
condition     <- geodata$bc_naam == "IJplein/Vogelbuurt + Nieuwendammerham" | geodata$bc_naam == "IJplein/Vogelbuurt + Noordelijke IJ-oevers Oost" | geodata$bc_code == "N61" | geodata$bc_code == "N72"
geodata <- rename.bc(geodata, condition, "N61+N72", "IJplein/Vogelbuurt + Nieuwendammerham/Noordelijke IJ-oevers Oost")

# Middelveldsche Akerpolder/Sloten
condition     <- geodata$bc_naam == "Middelveldsche Akerpolder" | geodata$bc_naam == "Middelveldsche Akerpolder/Sloten"
geodata <- rename.bc(geodata, condition, "F84", "Middelveldsche Akerpolder/Sloten")

# Nieuwendam-Noord/Waterlandpleinbuurt
condition     <- geodata$bc_naam == "Nieuwendam-Noord" | geodata$bc_naam == "Waterlandpleinbuurt"
geodata <- rename.bc(geodata, condition, "N68", "Nieuwendam-Noord/Waterlandpleinbuurt")

# Prinses Irenebuurt e.o./Station Zuid/WTC e.o.
condition     <- geodata$bc_naam == "Prinses Irenebuurt e.o." | geodata$bc_naam == "Station Zuid/WTC e.o."
geodata <- rename.bc(geodata, condition, "K59", "Prinses Irenebuurt e.o./Station Zuid/WTC e.o.")

# Slotermeer-Noordoost + Spieringhorn + Westelijk Havengebied + Bedrijventerrein Sloterdijk
condition     <- geodata$bc_naam == "Slotermeer-Noordoost" | geodata$bc_naam == "Slotermeer-Noordoost + Spieringhorn" | geodata$bc_naam == "Westelijk Havengebied + Bedrijventerrein Sloterdijk" | geodata$bc_code == "B10" | geodata$bc_code == "F11" | geodata$bc_code == "B11" | geodata$bc_code == "F75"
geodata <- rename.bc(geodata, condition, "F76+F75+B10+F11", "Slotermeer-Noordoost + Spieringhorn + Westelijk Havengebied + Bedrijventerrein Sloterdijk")

# Slotervaart: Slotervaart Noord + Slotervaart Zuid
condition     <- geodata$bc_naam == "Slotervaart" | geodata$bc_naam == "Slotervaart Noord" | geodata$bc_naam == "Slotervaart Zuid"
geodata <- rename.bc(geodata, condition, "F85+F89", "Slotervaart: Slotervaart Noord + Slotervaart Zuid")

# Volewijck + Buiksloterham/Volewijck + Noordelijke IJ-oevers West
condition     <- geodata$bc_naam == "Volewijck + Buiksloterham" | geodata$bc_naam == "Volewijck + Noordelijke IJ-oevers West" | geodata$bc_code == "N60" | geodata$bc_code == "N71"
geodata <- rename.bc(geodata, condition, "N60+N71", "Volewijck + Buiksloterham/Volewijck + Noordelijke IJ-oevers West")

# Following neighbourhoods were consistent in vote share data, but failed to merge with the neighbourhood characteristics data (e.g. not in neighbourhood data as a combined code)
# Spaarndammer- en Zeeheldenbuurt + Houthavens
condition     <- geodata$bc_code == "E12" | geodata$bc_code == "E13" | geodata$bc_code == "E13+E12"
geodata <- rename.bc(geodata, condition, "E13+12", "Spaarndammer- en Zeeheldenbuurt + Houthavens")

# Landlust + Sloterdijk
condition     <- geodata$bc_code == "E36" | geodata$bc_code == "E37" | geodata$bc_code == "E37+E36"
geodata <- rename.bc(geodata, condition, "E37+E36", "Landlust + Sloterdijk") 

# Tuindorp Buiksloot + Nieuwendammerdijk/Buiksloterdijk
condition     <- geodata$bc_code == "N63+N64" | geodata$bc_code == "N63" | geodata$bc_code == "N64"
geodata <- rename.bc(geodata, condition, "N63+N64", "Tuindorp Buiksloot + Nieuwendammerdijk/Buiksloterdijk")

# Holendrecht/Reigersbos + Amstel III/Bullewijk
condition     <- geodata$bc_code == "T96+T92" | geodata$bc_code == "T96" | geodata$bc_code == "T92"
geodata <- rename.bc(geodata, condition, "T96+T92", "Holendrecht/Reigersbos + Amstel III/Bullewijk")


# MERGE UNITS: ELECTION DATA  -----------------------------------

# De Krommert: Chassébuurt + Geuzenbuurt
condition     <- election_2022$bc_naam == "De Krommert" | election_2022$bc_naam == "Chassébuurt" | election_2022$bc_naam == "Geuzenbuurt"
election_2022 <- rename.bc(election_2022, condition, "E40+E75", "De Krommert: Chassébuurt + Geuzenbuurt")

# Diamantbuurt/Zuid Pijp
condition     <- election_2022$bc_naam == "Diamantbuurt" | election_2022$bc_naam == "Zuid Pijp"
election_2022 <- rename.bc(election_2022, condition, "K26", "Diamantbuurt/Zuid Pijp")

# Museumkwartier + Duivelseiland 
condition     <- election_2022$bc_naam == "Duivelseiland" | election_2022$bc_naam == "Museumkwartier" | election_2022$bc_naam == "Museumkwartier + Duivelseiland" | election_2022$bc_code == "K50" | election_2022$bc_code == "K47"
election_2022 <- rename.bc(election_2022, condition, "K47+K50", "Museumkwartier + Duivelseiland")

# Buikslotermeer + Elzenhagen
condition     <- election_2022$bc_naam == "Elzenhagen" | election_2022$bc_naam == "Buikslotermeer"
election_2022 <- rename.bc(election_2022, condition, "N69+N74", "Buikslotermeer + Elzenhagen")

# Frankendael + De Omval/Overamstel
condition     <- election_2022$bc_naam == "Frankendael" | election_2022$bc_naam == "Frankendael + De Omval" | election_2022$bc_naam == "De Omval" | election_2022$bc_naam == "Omval/Overamstel"
election_2022 <- rename.bc(election_2022, condition, "M55+M58", "Frankendael + De Omval/Overamstel")

# IJburg West + Zeeburgereiland/Nieuwe Diep + Indische Buurt Oost
condition     <- election_2022$bc_naam == "IJburg West" | election_2022$bc_naam == "IJburg West + Zeeburgereiland/Nieuwe Diep" | election_2022$bc_naam == "Indische Buurt Oost" | election_2022$bc_naam == "Indische Buurt Oost + Zeeburgereiland/Nieuwe Diep" | election_2022$bc_naam == "Zeeburgereiland/Nieuwe Diep"
election_2022 <- rename.bc(election_2022, condition, "M32+M34+M35", "IJburg West + Zeeburgereiland/Nieuwe Diep + Indische Buurt Oost")

# IJplein/Vogelbuurt + Nieuwendammerham/Noordelijke IJ-oevers Oost
condition     <- election_2022$bc_naam == "IJplein/Vogelbuurt + Nieuwendammerham" | election_2022$bc_naam == "IJplein/Vogelbuurt + Noordelijke IJ-oevers Oost" | election_2022$bc_code == "N61" | election_2022$bc_code == "N72"
election_2022 <- rename.bc(election_2022, condition, "N61+N72", "IJplein/Vogelbuurt + Nieuwendammerham/Noordelijke IJ-oevers Oost")

# Middelveldsche Akerpolder/Sloten
condition     <- election_2022$bc_naam == "Middelveldsche Akerpolder" | election_2022$bc_naam == "Middelveldsche Akerpolder/Sloten"
election_2022 <- rename.bc(election_2022, condition, "F84", "Middelveldsche Akerpolder/Sloten")

# Nieuwendam-Noord/Waterlandpleinbuurt
condition     <- election_2022$bc_naam == "Nieuwendam-Noord" | election_2022$bc_naam == "Waterlandpleinbuurt"
election_2022 <- rename.bc(election_2022, condition, "N68", "Nieuwendam-Noord/Waterlandpleinbuurt")

# Prinses Irenebuurt e.o./Station Zuid/WTC e.o.
condition     <- election_2022$bc_naam == "Prinses Irenebuurt e.o." | election_2022$bc_naam == "Station Zuid/WTC e.o."
election_2022 <- rename.bc(election_2022, condition, "K59", "Prinses Irenebuurt e.o./Station Zuid/WTC e.o.")

# Slotermeer-Noordoost + Spieringhorn + Westelijk Havengebied + Bedrijventerrein Sloterdijk
condition     <- election_2022$bc_naam == "Slotermeer-Noordoost" | election_2022$bc_naam == "Slotermeer-Noordoost + Spieringhorn" | election_2022$bc_naam == "Westelijk Havengebied + Bedrijventerrein Sloterdijk" | election_2022$bc_code == "B10" | election_2022$bc_code == "F11" | election_2022$bc_code == "B11" | election_2022$bc_code == "F75"
election_2022 <- rename.bc(election_2022, condition, "F76+F75+B10+F11", "Slotermeer-Noordoost + Spieringhorn + Westelijk Havengebied + Bedrijventerrein Sloterdijk")

# Slotervaart: Slotervaart Noord + Slotervaart Zuid
condition     <- election_2022$bc_naam == "Slotervaart" | election_2022$bc_naam == "Slotervaart Noord" | election_2022$bc_naam == "Slotervaart Zuid"
election_2022 <- rename.bc(election_2022, condition, "F85+F89", "Slotervaart: Slotervaart Noord + Slotervaart Zuid")

# Volewijck + Buiksloterham/Volewijck + Noordelijke IJ-oevers West
condition     <- election_2022$bc_naam == "Volewijck + Buiksloterham" | election_2022$bc_naam == "Volewijck + Noordelijke IJ-oevers West" | election_2022$bc_code == "N60" | election_2022$bc_code == "N71"
election_2022 <- rename.bc(election_2022, condition, "N60+N71", "Volewijck + Buiksloterham/Volewijck + Noordelijke IJ-oevers West")

# Following neighbourhoods were consistent in vote share data, but failed to merge with the neighbourhood characteristics data (e.g. not in neighbourhood data as a combined code)
# Spaarndammer- en Zeeheldenbuurt + Houthavens
condition     <- election_2022$bc_code == "E12" | election_2022$bc_code == "E13" | election_2022$bc_code == "E13+E12"
election_2022 <- rename.bc(election_2022, condition, "E13+12", "Spaarndammer- en Zeeheldenbuurt + Houthavens")

# Landlust + Sloterdijk
condition     <- election_2022$bc_code == "E36" | election_2022$bc_code == "E37" | election_2022$bc_code == "E37+E36"
election_2022 <- rename.bc(election_2022, condition, "E37+E36", "Landlust + Sloterdijk") 

# Tuindorp Buiksloot + Nieuwendammerdijk/Buiksloterdijk
condition     <- election_2022$bc_code == "N63+N64" | election_2022$bc_code == "N63" | election_2022$bc_code == "N64"
election_2022 <- rename.bc(election_2022, condition, "N63+N64", "Tuindorp Buiksloot + Nieuwendammerdijk/Buiksloterdijk")

# Holendrecht/Reigersbos + Amstel III/Bullewijk
condition     <- election_2022$bc_code == "T96+T92" | election_2022$bc_code == "T96" | election_2022$bc_code == "T92"
election_2022 <- rename.bc(election_2022, condition, "T96+T92", "Holendrecht/Reigersbos + Amstel III/Bullewijk")

# COMBINE SPATIAL AND ELECTION DATA -----------------

#Combine election_2022 with geodata
geo_2022 <- merge(geodata, election_2022, by="bc_code")  


# TURNOUT 2022 MAP -------------------------------------------

#Select necessary variable: geometry + variable to be visualised + bc_code (needed for merge)
geo_2022_turnout  <- subset(geo_2022, select=c(turnout_2022, geometry, bc_code))

#Merge areas for combined neighbourhoods 
geo_2022_turnout <- aggregate(geo_2022_turnout[,1:2], 
                              by=list(geo_2022_turnout$bc_code), 
                              do_union = TRUE, 
                              FUN=mean) %>% #take average of turnout
  subset(select=-c(Group.1))

#Make the map
geo_2022_turnout %>%
  mutate(turnout_factor = 
           factor(
             ifelse(turnout_2022 < 20, "0-20",
                    ifelse(turnout_2022 < 40, "20-40",
                           ifelse(turnout_2022 < 60, "40-60",
                                  ifelse(turnout_2022 < 80, "60-80",
                                         ifelse(turnout_2022 < 100, "80-100", ">100"))))),
             levels = c(">100", "80-100", "60-80", "40-60", "20-40", "0-20"), #reverse order to list high numbers first in legend
             ordered = TRUE)) %>%
  ggplot() +
  geom_sf(aes(fill = turnout_factor),
          color = "white") +   #borders in white
  theme_void() + 
  scale_fill_brewer(palette = "Blues",
                    direction = -1) + #darker colours for higher turnout
  labs(fill = "Turnout in %")
ggsave("map_2022_turnout.png", width = 2370, height = 1558, units = "px")

# PVDA 2022 MAP -------------------------------------------

#Select necessary variable: geometry + variable to be visualised + bc_code (needed for merge)
geo_2022_pvda  <- subset(geo_2022, select=c(PVDA_2022, geometry, bc_code))

#Merge areas for combined neighbourhoods 
geo_2022_pvda <- aggregate(geo_2022_pvda[,1:2], 
                           by=list(geo_2022_pvda$bc_code), 
                           do_union = TRUE, 
                           FUN=mean) %>% #take average of pvda
  subset(select=-c(Group.1))

#Make the map
geo_2022_pvda %>%
  mutate(pvda_factor = 
           factor(
             ifelse(PVDA_2022 < 10, "5-10",
                    ifelse(PVDA_2022 < 15, "10-15",
                           ifelse(PVDA_2022 < 20, "15-20","20-25"))),
             levels = c("20-25", "15-20", "10-15", "5-10"), #reverse order to list high numbers first in legend
             ordered = TRUE)) %>%
  ggplot() +
  geom_sf(aes(fill = pvda_factor),
          color = "white") +
  theme_void() + 
  scale_fill_brewer(palette = "Blues",
                    direction = -1) + #darker colours for higher pvda vote shares
  labs(fill = "Vote share in %")
ggsave("map_2022_pvda.png", width = 2370, height = 1558, units = "px")


# GENTRIFICATION: PUBLIC HOUSING -------------------------------------------

#Combine geodata with neighbourhood data
geosubdata <- merge(geodata, subdata, by="bc_code")  


#Select necessary variable: geometry + variable to be visualised + bc_code (needed for merge)
geosubdata_pubhousing  <- subset(geosubdata, select=c(housing_pub_delta2013, geometry, bc_code))

#Merge areas for combined neighbourhoods 
geosubdata_pubhousing <- aggregate(geosubdata_pubhousing[,1:2], 
                                   by=list(geosubdata_pubhousing$bc_code), 
                                   do_union = TRUE, 
                                   FUN=mean) %>% #take average of %public housing
  subset(select=-c(Group.1))

#Make map
#Custom color palette, based on colorbrewer
rev(brewer.pal(7, "RdBu"))
palette4 <- c("#2166AC", "#67A9CF", "#F7F7F7", "#FDDBC7", "#EF8A62", "#B2182B") #blue to red

geosubdata_pubhousing %>%
  #Turn continuous variable into factor for clearer plotting
  mutate(pubhousing_factor = 
           factor(
             ifelse(housing_pub_delta2013 > 5, ">5",
                    ifelse(housing_pub_delta2013 > 0, "0 to 5",
                           ifelse(housing_pub_delta2013 > -5, "-5 to 0",
                                  ifelse(housing_pub_delta2013 > -10, "-10 to -5",
                                         ifelse(housing_pub_delta2013 > -15, "-15 to -10",
                                                ifelse(housing_pub_delta2013 > -20, "-20 to -15", "<-20")))))),
             levels = c(">5", "0 to 5", "-5 to 0", "-10 to -5", "-15 to -10", "-20 to -15", "<-20"),
             ordered = TRUE)) %>%
  ggplot() + 
  geom_sf(aes(fill = pubhousing_factor),
          color = "white") + 
  theme_void() + 
  scale_fill_manual(values = palette4) + 
  labs(fill = "Change in percentage points")
ggsave("map_publichousing_2013_2017.png", width = 2370, height = 1558, units = "px")


# GENTRIFICATION: NET INCOME -------------------------------------------

#Turn into percentage changes
geosubdata <- geosubdata %>% mutate(netincome_delta2013 = (netincome_delta2013 / (netHHincome - netincome_delta2013))*100,
                                    netincome_delta2009 = (netincome_delta2009 / (netHHincome - netincome_delta2009))*100,
                                    netincome_delta2005 = (netincome_delta2005 / (netHHincome - netincome_delta2005))*100)

#Select necessary variable: geometry + variable to be visualised + bc_code (needed for merge)
geosubdata_netincome <- subset(geosubdata, select=c(netincome_delta2013, geometry, bc_code))

#Merge areas for combined neighbourhoods 
geosubdata_netincome <- aggregate(geosubdata_netincome[,1:2], 
                                  by=list(geosubdata_netincome$bc_code), 
                                  do_union = TRUE, 
                                  FUN=mean) %>% #take average of net income
  subset(select=-c(Group.1))

#Make map
geosubdata_netincome %>%
  #Turn continuous variable into factor for clearer plotting
  mutate(netincome_factor = 
           factor(
             ifelse(netincome_delta2013 < 5, "0-5",
                    ifelse(netincome_delta2013 < 10, "5-10",
                           ifelse(netincome_delta2013 < 15, "10-15",
                                  ifelse(netincome_delta2013 < 20, "15-20", 
                                         ifelse(netincome_delta2013 < 25, "20-25",
                                                ifelse(netincome_delta2013 < 30, "25-30", ">30")))))),
             levels = c("0-5", "5-10", "10-15", "15-20", "20-25", "25-30", ">30"),
             ordered = TRUE)) %>%
  ggplot() + 
  geom_sf(mapping = aes(fill = netincome_factor),
          color = "white") +  #white neighbourhood borders
  theme_void() + 
  scale_fill_brewer(palette = "Blues") + 
  labs(fill = "Percentage change")
ggsave("map_netincome_2013_2017.png", width = 2370, height = 1558, units = "px")

# PVDA 2018 MAP -----------------------------------------------------------

# Select necessary variable: geometry + variable to be visualised + bc_code (needed for merge)
geosubdata_pvda <- subset(geosubdata, select=c(PVDA, geometry, bc_code))

# Merge areas for combined neighbourhoods 
geosubdata_pvda <- aggregate(geosubdata_pvda[,1:2], 
                             by=list(geosubdata_pvda$bc_code), 
                             do_union = TRUE, 
                             FUN=mean) %>% #take average
  subset(select=-c(Group.1))

#Make map
geosubdata_pvda %>%
  #Turn continuous variable into factor for clearer plotting
  mutate(pvda_factor = 
           factor(
             ifelse(PVDA < 7.5, "5-7.5",
                    ifelse(PVDA < 10, "7.5-10",
                           ifelse(PVDA < 12.5, "10-12.5", 
                                  ifelse(PVDA < 15, "12.5-15", ">15")))),
             levels = c(">15", "12.5-15", "10-12.5", "7.5-10", "5-7.5"), #reverse order to list high numbers first in legend
             ordered = TRUE)) %>%
  ggplot() + 
  geom_sf(mapping = aes(fill = pvda_factor),
          color = "white") +  #white neighbourhood borders
  theme_void() + 
  scale_fill_brewer(palette = "Blues",
                    direction = -1) +  #darker colours for higher pvda
  labs(fill = "Vote share in %")
ggsave("map_2018_pvda.png", width = 2370, height = 1558, units = "px")

# TURNOUT 2018 MAP ------------------------------------------------------------

#Select necessary variable: geometry + variable to be visualised + bc_code (needed for merge)
geosubdata_turnout <- subset(geosubdata, select=c(turnout, geometry, bc_code))

#Merge areas for combined neighbourhoods 
geosubdata_turnout <- aggregate(geosubdata_turnout[,1:2], 
                             by=list(geosubdata_turnout$bc_code), 
                             do_union = TRUE, 
                             FUN=mean) %>% #take average
  subset(select=-c(Group.1))

#Export 2018 map
map_2018_turnout <- geosubdata_turnout %>%
  mutate(turnout_factor = 
           factor(
             ifelse(turnout < 20, "0-20",
                    ifelse(turnout < 40, "20-40",
                           ifelse(turnout < 60, "40-60",
                                  ifelse(turnout < 80, "60-80",
                                         ifelse(turnout < 100, "80-100", ">100"))))),
             levels = c(">100", "80-100", "60-80", "40-60", "20-40", "0-20"), #reverse order to list high numbers first in legend
             ordered = TRUE)) %>%
  ggplot() +
  geom_sf(aes(fill = turnout_factor),
          color = "white") + #borders in white
  theme_void() + 
  scale_fill_brewer(palette = "Blues", #turnout in blue
                    direction = -1) + #darker colours for higher turnout 
  labs(fill = "Turnout in %")
ggsave("map_2018_turnout.png", width = 2370, height = 1558, units = "px")

#Save object without legend and incl. year for comparative map
map_2018_turnout <- map_2018_turnout + 
  theme(legend.position = "none") +
  labs(title = "2018")

#TURNOUT: COMPARATIVE 2006-2018 -------------------------------------------------------------

#Combine geodata with neighbourhood datasets
geosubdata_2006 <- merge(geodata, subdata_2006, by="bc_code") 
geosubdata_2010 <- merge(geodata, subdata_2010, by="bc_code")
geosubdata_2014 <- merge(geodata, subdata_2014, by="bc_code") 

#Select necessary variables: geometry + variable to be visualised + bc_code (needed for merge)
geosubdata_2006_turnout <- subset(geosubdata_2006, select=c(turnout, geometry, bc_code))
geosubdata_2010_turnout <- subset(geosubdata_2010, select=c(turnout, geometry, bc_code))
geosubdata_2014_turnout <- subset(geosubdata_2014, select=c(turnout, geometry, bc_code))

# Merge areas for combined neighbourhoods 
geosubdata_2006_turnout <- aggregate(geosubdata_2006_turnout[,1:2], 
                                     by=list(geosubdata_2006_turnout$bc_code), 
                                     do_union = TRUE, 
                                     FUN=mean) %>% #take average 
  subset(select=-c(Group.1)) 

geosubdata_2010_turnout <- aggregate(geosubdata_2010_turnout[,1:2], 
                                     by=list(geosubdata_2010_turnout$bc_code), 
                                     do_union = TRUE, 
                                     FUN=mean) %>% #take average 
  subset(select=-c(Group.1)) 

geosubdata_2014_turnout <- aggregate(geosubdata_2014_turnout[,1:2], 
                                     by=list(geosubdata_2014_turnout$bc_code), 
                                     do_union = TRUE, 
                                     FUN=mean) %>% #take average 
  subset(select=-c(Group.1)) 

#Make plots per year
map_2006_turnout <- geosubdata_2006_turnout %>%
  filter(!is.na(turnout)) %>%
  mutate(turnout_factor = 
           factor(
             ifelse(turnout < 20, "0-20",
                    ifelse(turnout < 40, "20-40",
                           ifelse(turnout < 60, "40-60",
                                  ifelse(turnout < 80, "60-80",
                                         ifelse(turnout < 100, "80-100", ">100"))))),
             levels = c(">100", "80-100", "60-80", "40-60", "20-40", "0-20"), #reverse order to list high numbers first in legend
             ordered = TRUE)) %>%
  ggplot() +
  geom_sf(aes(fill = turnout_factor),
          color = "white") +   #borders in white
  theme_void() + 
  scale_fill_brewer(palette = "Blues", 
                    direction = -1) + #darker colours for higher turnout
  theme(legend.position = "none") + 
  labs(title = "2006", 
    fill = "Turnout in %")

map_2010_turnout <- geosubdata_2010_turnout %>%
  mutate(turnout_factor = 
           factor(
             ifelse(turnout < 20, "0-20",
                    ifelse(turnout < 40, "20-40",
                           ifelse(turnout < 60, "40-60",
                                  ifelse(turnout < 80, "60-80",
                                         ifelse(turnout < 100, "80-100", ">100"))))),
             levels = c(">100", "80-100", "60-80", "40-60", "20-40", "0-20"), #reverse order to list high numbers first in legend
             ordered = TRUE)) %>%
  ggplot() +
  geom_sf(aes(fill = turnout_factor),
          color = "white") +   #borders in white
  theme_void() + 
  scale_fill_brewer(palette = "Blues", 
                    direction = -1) + #darker colours for higher turnout
  theme(legend.position = "none") + 
  labs(title = "2010", 
    fill = "Turnout in %")

map_2014_turnout <-geosubdata_2014_turnout %>%
  mutate(turnout_factor = 
           factor(
             ifelse(turnout < 20, "0-20",
                    ifelse(turnout < 40, "20-40",
                           ifelse(turnout < 60, "40-60",
                                  ifelse(turnout < 80, "60-80",
                                         ifelse(turnout < 100, "80-100", ">100"))))),
             levels = c(">100", "80-100", "60-80", "40-60", "20-40", "0-20"), #reverse order to list high numbers first in legend
             ordered = TRUE)) %>%
  ggplot() +
  geom_sf(aes(fill = turnout_factor),
          color = "white") +   #borders in white
  theme_void() + 
  scale_fill_brewer(palette = "Blues", 
                    direction = -1) + #darker colours for higher turnout
  theme(legend.position = "bottom") +
  labs(title = "2014", 
    fill = "Turnout in %")

#Combined plot
#Get legend from 2014 map (all categories) to use as common legend
#Function from: https://stackoverflow.com/questions/12539348/ggplot-separate-legend-and-plot
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- get_legend(map_2014_turnout) #save legend as object
map_2014_turnout <- map_2014_turnout + theme(legend.position="none") #remove legend from 2014 map

#Change legend position
blankPlot <- ggplot()+geom_blank(aes(1,1)) + 
  cowplot::theme_nothing()

turnout_comparative <- arrangeGrob(map_2006_turnout, map_2010_turnout, 
             map_2014_turnout, map_2018_turnout,
             legend, blankPlot,
             ncol=2, nrow = 3, 
             widths = c(2.7, 2.7), heights = c(2.5, 2.5, 0.2))
ggsave("map_turnout_comparative.png", plot = turnout_comparative, 
       width = 2844, height = 1870, units = "px")

#PVDA: COMPARATIVE 2006-2018 -------------------------------------------------------------

#Select necessary variables: geometry + variable to be visualised + bc_code (needed for merge)
geosubdata_2006_pvda <- subset(geosubdata_2006, select=c(PVDA, geometry, bc_code))
geosubdata_2010_pvda <- subset(geosubdata_2010, select=c(PVDA, geometry, bc_code))
geosubdata_2014_pvda <- subset(geosubdata_2014, select=c(PVDA, geometry, bc_code))

# Merge areas for combined neighbourhoods 
geosubdata_2006_pvda <- aggregate(geosubdata_2006_pvda[,1:2], 
                                  by=list(geosubdata_2006_pvda$bc_code), 
                                  do_union = TRUE, 
                                  FUN=mean) %>% #take average 
  subset(select=-c(Group.1)) 

geosubdata_2010_pvda <- aggregate(geosubdata_2010_pvda[,1:2], 
                                  by=list(geosubdata_2010_pvda$bc_code), 
                                  do_union = TRUE, 
                                  FUN=mean) %>% #take average 
  subset(select=-c(Group.1)) 

geosubdata_2014_pvda <- aggregate(geosubdata_2014_pvda[,1:2], 
                                  by=list(geosubdata_2014_pvda$bc_code), 
                                  do_union = TRUE, 
                                  FUN=mean) %>% #take average 
  subset(select=-c(Group.1)) 

#Make plots per year
map_2006_pvda <- geosubdata_2006_pvda %>%
  filter(!is.na(PVDA)) %>%
  mutate(pvda_factor = 
           factor(
             ifelse(PVDA < 10, "5-10",
                ifelse(PVDA < 15, "10-15",
                  ifelse(PVDA < 20, "15-20",
                    ifelse(PVDA < 25, "20-25",
                      ifelse(PVDA < 30, "25-30",
                         ifelse(PVDA < 40, "30-40",
                            ifelse(PVDA < 50, "40-50",
                              ifelse(PVDA < 60, "50-60", ">60")))))))),
             levels = c(">60", "50-60", "40-50", "30-40", "25-30", "20-25", "15-20", "10-15", "5-10"), #reverse order to list high numbers first in legend
             ordered = TRUE)) %>%
  ggplot() +
  geom_sf(aes(fill = pvda_factor),
          color = "white") +   #borders in white
  theme_void() + 
  scale_fill_brewer(palette = "Blues", 
                    direction = -1, #darker colours for higher PvdA vote shares
                    drop = FALSE) + #legend becomes common legend: show all categories
  theme(legend.position = "bottom") + 
  labs(title = "2006", 
       fill = "Vote share in %")

map_2010_pvda <- geosubdata_2010_pvda %>%
  filter(!is.na(PVDA)) %>%
  mutate(pvda_factor = 
           factor(
             ifelse(PVDA < 10, "5-10",
                    ifelse(PVDA < 15, "10-15",
                           ifelse(PVDA < 20, "15-20",
                                  ifelse(PVDA < 25, "20-25",
                                         ifelse(PVDA < 30, "25-30",
                                                ifelse(PVDA < 40, "30-40",
                                                       ifelse(PVDA < 50, "40-50",
                                                              ifelse(PVDA < 60, "50-60", ">60")))))))),
             levels = c(">60", "50-60", "40-50", "30-40", "25-30", "20-25", "15-20", "10-15", "5-10"), #reverse order to list high numbers first in legend
             ordered = TRUE)) %>%
  ggplot() +
  geom_sf(aes(fill = pvda_factor),
          color = "white") +   #borders in white
  theme_void() + 
  scale_fill_brewer(palette = "Blues",
                    direction = -1, 
                    drop = FALSE) + #darker colours for higher PvdA vote shares
  theme(legend.position = "none") + 
  labs(title = "2010",
       fill = "Vote share in %")

map_2014_pvda <- geosubdata_2014_pvda %>%
  filter(!is.na(PVDA)) %>%
  mutate(pvda_factor = 
           factor(
             ifelse(PVDA < 10, "5-10",
                    ifelse(PVDA < 15, "10-15",
                           ifelse(PVDA < 20, "15-20",
                                  ifelse(PVDA < 25, "20-25",
                                         ifelse(PVDA < 30, "25-30",
                                                ifelse(PVDA < 40, "30-40",
                                                       ifelse(PVDA < 50, "40-50",
                                                              ifelse(PVDA < 60, "50-60", ">60")))))))),
             levels = c(">60", "50-60", "40-50", "30-40", "25-30", "20-25", "15-20", "10-15", "5-10"), #reverse order to list high numbers first in legend
             ordered = TRUE)) %>%
  ggplot() +
  geom_sf(aes(fill = pvda_factor),
          color = "white") +   #borders in white
  theme_void() + 
  scale_fill_brewer(palette = "Blues",
                    direction = -1, 
                    drop = FALSE) + #darker colours for higher PvdA vote shares
  theme(legend.position = "none") + 
  labs(title = "2014",
       fill = "Vote share in %")

map_2018_pvda <- geosubdata_pvda %>%
  filter(!is.na(PVDA)) %>%
  mutate(pvda_factor = 
           factor(
             ifelse(PVDA < 10, "5-10",
                    ifelse(PVDA < 15, "10-15",
                           ifelse(PVDA < 20, "15-20",
                                  ifelse(PVDA < 25, "20-25",
                                         ifelse(PVDA < 30, "25-30",
                                                ifelse(PVDA < 40, "30-40",
                                                       ifelse(PVDA < 50, "40-50",
                                                              ifelse(PVDA < 60, "50-60", ">60")))))))),
             levels = c(">60", "50-60", "40-50", "30-40", "25-30", "20-25", "15-20", "10-15", "5-10"), #reverse order to list high numbers first in legend
             ordered = TRUE)) %>%
  ggplot() +
  geom_sf(aes(fill = pvda_factor),
          color = "white") +   #borders in white
  theme_void() + 
  scale_fill_brewer(palette = "Blues",
                    direction = -1, 
                    drop = FALSE) + #darker colours for higher PvdA vote shares
  theme(legend.position = "none") + 
  labs(title = "2018",
       fill = "Vote share in %")


#Combined plot
#Get legend from 2006 map (all categories) to use as common legend
legend <- get_legend(map_2006_pvda) #save legend as object
map_2006_pvda <- map_2006_pvda + theme(legend.position="none") #remove legend from 2014 map

#Change legend position
blankPlot <- ggplot()+geom_blank(aes(1,1)) + 
  cowplot::theme_nothing()

pvda_comparative <- arrangeGrob(map_2006_pvda, map_2010_pvda, 
                                   map_2014_pvda, map_2018_pvda,
                                   legend, blankPlot,
                                   ncol=2, nrow = 3, 
                                   widths = c(2.7, 2.7), heights = c(2.5, 2.5, 0.45))
ggsave("map_pvda_comparative.png", plot = pvda_comparative, 
       width = 2844, height = 1870, units = "px")


##### OLD ######## -------------------------------------------------------------------------------------------------------------------------------

#TODO: remove

#library(rgdal)
#library(ggplot2)
#library(broom)
#library(tidyverse)
#library(gpclib)
#library(maptools)
#library(readxl)
#library(RColorBrewer)
#
#shp <- readOGR("bc2015def_region.shp", stringsAsFactors = F)
#summary(shp@data)
#
##Plotting empty neighbourhood outlines only
#map <- ggplot() + geom_polygon(data = shp, aes(x = long, y = lat, group = group), colour = "black", fill = NA)
#map + theme_void()
#
##Add information to plot
## Data can be obtained here: https://onderzoek.amsterdam.nl/dataset/verkiezingen-gemeenteraad-2022  
#additional_2022 <- read_xlsx("2022_gemeenteraadsverkiezingen_wijk_stadsdeel_5cb9f5e19c.xlsx", skip = 1) 
#
#election_2022 <- additional_2022 %>%
#  select(wijkcode, wijknaam, kiesgerechtigden, `geldige stembiljetten`, `Partij van de Arbeid (P.v.d.A.)`) %>%
#  mutate(turnout_2022 = (`geldige stembiljetten` / kiesgerechtigden)*100,
#         PVDA_2022 = (`Partij van de Arbeid (P.v.d.A.)` / `geldige stembiljetten`)*100) %>%
#  dplyr::rename(bc_code = wijkcode,
#         bc_naam = wijknaam)
#
## Transform the shapefile into a dataframe
#shp_df <- broom::tidy(shp, region = "BC2015")
#
## Merge with election data
#geodata <- left_join(shp_df, election_2022, by = c("id" = "bc_code"))
#
##Make the mapf
##TODO: consider removing fill for NA
#
#  #Continuous scale
#  ggplot() + 
#    geom_polygon(data = geodata,
#                 aes(x = long, y = lat, group = group, fill = turnout_2022), 
#                 colour = "white") + #white neighbourhood borders\
#    theme_void() +
#    scale_fill_gradient(low = "#084594", #TODO: change colours
#                        high = "#FB6A4A",
#                        limits = c(0,200),
#                        breaks = c(20, 40, 60, 80, 100),
#                        guide = guide_colourbar(label = TRUE, #add percentages
#                                                barheight = 10)) + #make bar longer
#    labs(title = "Turnout at the 2022 municipal elections",
#         fill = "Turnout in %")
#
#  #Discrete scale
#  geodata %>%
#    mutate(turnout_factor = 
#             factor(
#               ifelse(turnout_2022 < 21, "0-20",
#                ifelse(turnout_2022 > 21 & turnout_2022 < 40, "21-40",
#                 ifelse(turnout_2022 > 41 & turnout_2022 < 60, "41-60",
#                  ifelse(turnout_2022 > 61 & turnout_2022 < 80, "61-80",
#                    ifelse(turnout_2022 > 81 & turnout_2022 < 100, "81-100", ">100"))))),
#               #levels = c("0-20", "21-40", "41-60", "61-80", "81-100", ">100"),
#               levels = c(">100", "81-100", "61-80", "41-60", "21-40", "0-20"), #reverse order to list high numbers first in legend
#               ordered = TRUE)) %>%
#    ggplot() + 
#    geom_polygon(aes(x = long, y = lat, group = group, 
#                     fill = turnout_factor), 
#                 colour = "white") + #white neighbourhood borders
#    theme_void() + 
#    scale_fill_brewer(palette = "Blues", #turnout in blue
#                      na.value = "gray",  #missing neighbourhoods in grey
#                      direction = -1) + #darker colours for higher turnout
#    labs(title = "Turnout at the 2022 municipal elections", #TODO: consider removing
#         fill = "Turnout in %")



