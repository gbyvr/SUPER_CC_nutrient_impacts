# SUPER Research Project Data Exploration and Analysis
#Gabriella Vieira
#Mentor: Carolina Barbosa
#1/31/2023


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
watertemp_clean <- read.csv("Spring_2024//ESS_221//Research_Project//data//watertemp_clean.csv")
waterqual_clean <- read.csv("Spring_2024//ESS_221//Research_Project//data//waterqual.csv")
nitrate <- read.csv("Spring_2024//ESS_221//Research_Project//data//nitrate.csv")
Discharge <- read.csv("Spring_2024//ESS_221//Research_Project//data//dischargeNeon.csv")
precip_raw <- read.csv("Spring_2024//ESS_221//Research_Project//data//neonPrecipitation.csv")


#SECTION 1: CHANGE THE DATA FREQUENCY


#SECTION 1.1: Nitrate (from 15-minute frequency to daily and monthly averages)

#SECTION 1.1.A: Add date columns
nitrate$Year <- substr(nitrate$endDateTime, 1, 4)
nitrate$Month <- substr(nitrate$endDateTime, 6, 7)
nitrate$Day <- substr(nitrate$endDateTime, 9, 10)

#SECTION 1.1.B: Remove outliers
Q1 <- quantile(nitrate$surfWaterNitrateMean, 0.25)
Q3 <- quantile(nitrate$surfWaterNitrateMean, 0.75)
IQR <- Q3 - Q1
lower_fence <- Q1 - 1.5 * IQR
upper_fence <- Q3 + 1.5 * IQR
#Filtering to exclude outliers
nooutliers_newDF <- nitrate %>%
  dplyr::filter(surfWaterNitrateMean>= lower_fence & surfWaterNitrateMean <= upper_fence)

#SECTION 1.1.C: Take daily and monthly averages 
#Daily nitrate averages
N_daily <- nitrate %>%
  group_by(siteID, Year, Month, Day) %>%
  filter(Year >= 2018) %>% 
  summarise(Mean_Nitrate = mean(surfWaterNitrateMean, na.rm = TRUE))

#Monthly averages
N_monthly <- nitrate %>%
  group_by(siteID, Year, Month) %>%
  filter(Year >= 2018) %>% 
  summarise(Mean_Nitrate = mean(surfWaterNitrateMean, na.rm = TRUE))




#SECTION 1.1.D: Full join on nitrate dataframes
All_nitrate <- full_join(N_daily, N_monthly, by = c("siteID", "Year", "Month")) %>%
  rename(Avg_daily = Mean_Nitrate.x, Avg_monthly = Mean_Nitrate.y)


#OUTLIER REMOVAL FOR NITRATE VALUES COMPLETE



#SECTION 1.2: Other water quality metrics (from 1-minute frequency to daily and monthly averages)

#SECTION 1.2.A: Add date columns
waterqual_clean$Year <- substr(waterqual_clean$endDateTime, 1, 4)
waterqual_clean$Month <- substr(waterqual_clean$endDateTime, 6, 7)
waterqual_clean$Day <- substr(waterqual_clean$endDateTime, 9, 10)

#SECTION 1.2.B: Remove outliers
Q1 <- quantile(waterqual_clean$pH, 0.25, na.rm = TRUE)
Q3 <- quantile(waterqual_clean$pH, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
lower_fence <- Q1 - 1.5 * IQR
upper_fence <- Q3 + 1.5 * IQR
#Filtering to exclude outliers
nooutliers_newDF <- waterqual_clean %>%
  dplyr::filter(pH>= lower_fence & pH <= upper_fence)

#SECTION 1.2.C: Take daily and monthly averages 
#Daily water quality averages
WQ_daily <- waterqual_clean %>%
  group_by(siteID, Year, Month, Day) %>%
  filter(Year >= 2018) %>% 
  summarise(
    Conductance = mean(specificConductance, na.rm = TRUE), 
    DO = mean(dissolvedOxygen, na.rm = TRUE),
    pH = mean(pH, na.rm = TRUE), 
    Chrolophyll = mean(chlorophyll, na.rm = TRUE), 
    Turbidity = mean(turbidity, na.rm = TRUE), 
    fDOM = mean(fDOM, na.rm = TRUE)
  )

#Monthly averages
WQ_monthly <- waterqual_clean %>%
  group_by(siteID, Year, Month) %>%
  filter(Year >= 2018) %>% 
  summarise(
    Conductance = mean(specificConductance, na.rm = TRUE), 
    DO = mean(dissolvedOxygen, na.rm = TRUE),
    pH = mean(pH, na.rm = TRUE), 
    Chrolophyll = mean(chlorophyll, na.rm = TRUE), 
    Turbidity = mean(turbidity, na.rm = TRUE), 
    fDOM = mean(fDOM, na.rm = TRUE)
  )

#SECTION 1.2.D: Full join on water quality dataframes
All_wq <- full_join(WQ_daily, WQ_monthly, by = c("siteID", "Year", "Month")) %>%
  rename(Cond_daily = Conductance.x, DO_daily = DO.x, pH_daily = pH.x, Cholophyll_daily = Chrolophyll.x, Turb_daily = Turbidity.x, fDOM_daily = fDOM.x, Cond_monthly = Conductance.y, DO_monthly = DO.y, pH_monthly = pH.y, Chlorophyll_monthly = Chrolophyll.y, Turb_monthly = Turbidity.y, fDOM_monthly = fDOM.y)


#OUTLIER REMOVAL STILL NEEDED FOR METRICS OTHER THAN PH




#SECTION 1.3: Discharge (from 1-minute frequency to daily and monthly averages)

#SECTION 1.3.A: Add date columns
Discharge$Year <- substr(Discharge$endDate, 1, 4)
Discharge$Month <- substr(Discharge$endDate, 6, 7)
Discharge$Day <- substr(Discharge$endDate, 9, 10)

#SECTION 1.3.B: Remove outliers
Q1 <- quantile(Discharge$maxpostDischarge, 0.25, na.rm = TRUE)
Q3 <- quantile(Discharge$maxpostDischarge, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
lower_fence <- Q1 - 1.5 * IQR
upper_fence <- Q3 + 1.5 * IQR
#Filtering to exclude outliers
nooutliers_newDF <- Discharge %>%
  dplyr::filter(maxpostDischarge>= lower_fence & maxpostDischarge <= upper_fence)


#SECTION 1.3.C: Take daily and monthly averages 
#Daily discharge averages
dis_daily <- Discharge %>%
  group_by(siteID, Year, Month, Day) %>%
  filter(Year >= 2018) %>% 
  summarise(Mean_discharge = mean(maxpostDischarge, na.rm = TRUE))

#Monthly averages
dis_monthly <- Discharge %>%
  group_by(siteID, Year, Month) %>%
  filter(Year >= 2018) %>% 
  summarise(Mean_discharge = mean(maxpostDischarge, na.rm = TRUE))

#SECTION 1.3.D: Full join on discharge dataframes
All_dis <- full_join(dis_daily, dis_monthly, by = c("siteID", "Year", "Month")) %>%
  rename(Avg_daily = Mean_discharge.x, Avg_monthly = Mean_discharge.y)



#SECTION 1.4: NEON Precipitation (from 30-minute frequency to daily and monthly averages)

#SECTION 1.4.A: Add date columns
precip_raw$Year <- substr(precip_raw$endDateTime, 1, 4)
precip_raw$Month <- substr(precip_raw$endDateTime, 6, 7)
precip_raw$Day <- substr(precip_raw$endDateTime, 9, 10)

#SECTION 1.4.B: Remove outliers
Q1 <- quantile(precip_raw$maxpostDischarge, 0.25, na.rm = TRUE)
Q3 <- quantile(precip_raw$maxpostDischarge, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
lower_fence <- Q1 - 1.5 * IQR
upper_fence <- Q3 + 1.5 * IQR
#Filtering to exclude outliers
nooutliers_newDF <- Discharge %>%
  dplyr::filter(maxpostDischarge>= lower_fence & maxpostDischarge <= upper_fence)


#SECTION 1.4.C: Take daily and monthly averages 
#Daily precipiation averages
precip_daily <- precip_raw %>%
  group_by(siteID, Year, Month, Day) %>%
  filter(Year >= 2018) %>% 
  summarise(
    priPrecip = mean(priPrecipBulk, na.rm = TRUE),
    secPrecip = mean(secPrecipBulk, na.rm = TRUE)
  )

#Monthly averages
precip_monthly <- precip_raw %>%
  group_by(siteID, Year, Month) %>%
  filter(Year >= 2018) %>% 
  summarise(
    priPrecip = mean(priPrecipBulk, na.rm = TRUE),
    secPrecip = mean(secPrecipBulk, na.rm = TRUE)
  )


#SECTION 1.4.D: Full join on precipitation dataframes
All_precip <- full_join(precip_daily, precip_monthly, by = c("siteID", "Year", "Month")) %>%
  rename(priDailyAvg = priPrecip.x, secDailyAvg = secPrecip.x, priMonthlyAvg = priPrecip.y, secMonthlyAvg = secPrecip.y)




#SECTION 1.5: Water temperature (from 30-minute frequency to daily and monthly averages)

#SECTION 1.2.A: Add date columns
watertemp_clean$Year <- substr(watertemp_clean$endDateTime, 1, 4)
watertemp_clean$Month <- substr(watertemp_clean$endDateTime, 6, 7)
watertemp_clean$Day <- substr(watertemp_clean$endDateTime, 9, 10)

#SECTION 1.2.B: Remove outliers
Q1 <- quantile(watertemp_clean$surfWaterTempMean, 0.25, na.rm = TRUE)
Q3 <- quantile(watertemp_clean$surfWaterTempMean, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
lower_fence <- Q1 - 1.5 * IQR
upper_fence <- Q3 + 1.5 * IQR
#Filtering to exclude outliers
nooutliers_newDF <- watertemp_clean %>%
  dplyr::filter(pH>= lower_fence & pH <= upper_fence)

#SECTION 1.2.C: Take daily and monthly averages 
#Daily water quality averages
WT_daily <- watertemp_clean %>%
  group_by(siteID, Year, Month, Day) %>%
  filter(Year >= 2018) %>% 
  summarise(Mean_temp = mean(surfWaterTempMean, na.rm = TRUE))

#Monthly averages
WT_monthly <- watertemp_clean %>%
  group_by(siteID, Year, Month) %>%
  filter(Year >= 2018) %>% 
  summarise(Mean_temp = mean(surfWaterTempMean, na.rm = TRUE))


#SECTION 1.5.D: Full join on water temperature dataframes
All_wt <- full_join(WT_daily, WT_monthly, by = c("siteID", "Year", "Month")) %>%
  rename(Temp_daily = Mean_temp.x, Temp_monthly = Mean_temp.y)






#Master dataframe
All_nitrate <- full_join(N_daily, N_monthly, by = c("siteID", "Year", "Month"))
All_nitrate <- rename(All_nitrate, Daily_avg = Mean_Nitrate.x, Monthly_avg = Mean_Nitrate.y)

