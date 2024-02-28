#Impacts of Shifting Climate Variables on Flow, Nutrient Dynamics, and Macroinvertebrate Abundances
#SUPER Nutrient data exploration and analysis
#Billy Johnson, Gabriella Vieira
#Mentor: Carolina Barbosa
#2/10/2024



#Packages Needed:
#install.packages('dataRetrieval')
#install.packages("tidyverse")
#install.packages("flextable")
#install.packages("ggdark")
#install.packages("ggtheme")
#install.packages(c("httr", "jsonlite"))
#install.packages("neonUtilities")


#Load packages 
library(tibble)
library(rlang)
library(tidyverse) #which includes ggplot2
library(tmap) # interactive and static maps
library(sf) # to manage spatial data to make maps
library(tigris) # to import census tract spatial data
library(flextable)
library(ggdark)
library(ggplot2)
library(viridis)
library(RColorBrewer)
library(ggthemes)
library(terra)
library(mapproj)
library(dataRetrieval)
library(httr)
library(jsonlite)
library(neonUtilities)



#Read data
final_df <- read.csv("data//Final_DF.csv")

#SUbset by site ID

ARIK <- final_df[final_df$siteID == 'ARIK',]
COMO <- final_df[final_df$siteID == 'COMO',]
WLOU <- final_df[final_df$siteID == 'WLOU',]



#SECTION 1: EXPLORATORY ANALYSIS

range(final_df$disDaily_lps, na.rm=TRUE)
range(final_df$nitDaily_mcmolpl, na.rm=TRUE)
range(final_df$precipDaily_mm, na.rm=TRUE)
range(final_df$precipDaily_mm, na.rm=TRUE)
range(final_df$Cond_daily, na.rm=TRUE)
range(final_df$DO_daily, na.rm=TRUE)
range(final_df$pH_daily, na.rm=TRUE)
range(final_df$Cholophyll_daily, na.rm=TRUE)
range(final_df$Turb_daily, na.rm=TRUE)
range(final_df$fDOM_daily, na.rm=TRUE)
range(final_df$tempMonthly_C, na.rm=TRUE)
range(final_df$disMonthly_lps, na.rm=TRUE)
range(final_df$nitMonthly_mcmolpl, na.rm=TRUE)
range(final_df$precipMonthly_mm, na.rm=TRUE)
range(final_df$precipMonthly_mm, na.rm=TRUE)
range(final_df$Cond_monthly, na.rm=TRUE)
range(final_df$DO_monthly, na.rm=TRUE)
range(final_df$pH_monthly, na.rm=TRUE)
range(final_df$Cholophyll_monthly, na.rm=TRUE)
range(final_df$Turb_monthly, na.rm=TRUE)
range(final_df$fDOM_monthly, na.rm=TRUE)
range(final_df$tempMonthly_C, na.rm=TRUE)

plot(final_df$Date, final_df$tempDaily_C)


