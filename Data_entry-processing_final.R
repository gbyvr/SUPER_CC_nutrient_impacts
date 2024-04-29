#Impacts of Shifting Climate Variables on Nutrient Dynamics 
#and Macroinvertebrate Abundance in CO streams 
#Entry and processing
#Billy Johnson, Gabriella Vieira
#Mentor: Carolina Barbosa
#11/20/2023


#Packages Needed:
library(tidyverse)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(httr)
library(jsonlite)
library(tibble)
library(rlang)




#Read data
watertemp_clean <- read.csv("data//watertemp_clean.csv")
waterqual_clean <- read.csv("data//waterqual.csv")
nitrate <- read.csv("data//nitrate.csv")
Discharge <- read.csv("data//dischargeNeon.csv")
precip_raw <- read.csv("data//neonPrecipitation.csv")
cleanInvert <- read.csv("data//CleanInvert.csv")
final_df <- read.csv("data//Final_df.csv")


# Invertebrate Data
load("~/Desktop/Colorado State University Class Things/SUPER PROJECT/SUPER Project/DataMacro/invert.RData")

# Clean Invertebrate Data
load("~/Desktop/Colorado State University Class Things/SUPER PROJECT/SUPER Project/DataMacro/CleanInvert.RData")



#CHANGE DATA FREQUENCY FOR ALL VARIABLES


#SECTION 1: Nitrate (from 15-minute frequency to daily and monthly averages)

#SECTION 1.1: Add date columns
nitrate$Year <- substr(nitrate$endDateTime, 1, 4)
nitrate$Month <- substr(nitrate$endDateTime, 6, 7)
nitrate$Day <- substr(nitrate$endDateTime, 9, 10)

#SECTION 1.2: Remove outliers
Q1 <- quantile(nitrate$surfWaterNitrateMean, 0.25)
Q3 <- quantile(nitrate$surfWaterNitrateMean, 0.75)
IQR <- Q3 - Q1
lower_fence <- Q1 - 1.5 * IQR
upper_fence <- Q3 + 1.5 * IQR
#Filtering to exclude outliers
nooutliers_newDF <- nitrate %>%
  dplyr::filter(surfWaterNitrateMean>= lower_fence & surfWaterNitrateMean <= upper_fence)

#SECTION 1.3: Take daily and monthly averages 
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

#SECTION 1.4: Full join on nitrate dataframes
All_nitrate <- full_join(N_daily, N_monthly, by = c("siteID", "Year", "Month")) %>%
  rename(nitDaily_mcmolpl = Mean_Nitrate.x, nitMonthly_mcmolpl = Mean_Nitrate.y) %>% 
  mutate(Date = ymd(paste(Year, Month, Day, sep = "-")))





#SECTION 2: Other water quality metrics (from 1-minute frequency to daily and monthly averages)


#SECTION 2.1: Add date columns
waterqual_clean$Year <- substr(waterqual_clean$endDateTime, 1, 4)
waterqual_clean$Month <- substr(waterqual_clean$endDateTime, 6, 7)
waterqual_clean$Day <- substr(waterqual_clean$endDateTime, 9, 10)


#SECTION 2.2: Remove outliers
Q1 <- quantile(waterqual_clean$pH, 0.25, na.rm = TRUE)
Q3 <- quantile(waterqual_clean$pH, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
lower_fence <- Q1 - 1.5 * IQR
upper_fence <- Q3 + 1.5 * IQR
#Filtering to exclude outliers
nooutliers_newDF <- waterqual_clean %>%
  dplyr::filter(pH>= lower_fence & pH <= upper_fence)



Q1 <- quantile(waterqual_clean$specificConductance, 0.25, na.rm = TRUE)
Q3 <- quantile(waterqual_clean$specificConductance, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
lower_fence <- Q1 - 1.5 * IQR
upper_fence <- Q3 + 1.5 * IQR
#Filtering to exclude outliers
nooutliers_newDF <- waterqual_clean %>%
  dplyr::filter(specificConductance>= lower_fence & specificConductance <= upper_fence)


Q1 <- quantile(waterqual_clean$dissolvedOxygen, 0.25, na.rm = TRUE)
Q3 <- quantile(waterqual_clean$dissolvedOxygen, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
lower_fence <- Q1 - 1.5 * IQR
upper_fence <- Q3 + 1.5 * IQR
#Filtering to exclude outliers
nooutliers_newDF <- waterqual_clean %>%
  dplyr::filter(dissolvedOxygen>= lower_fence & dissolvedOxygen <= upper_fence)


Q1 <- quantile(waterqual_clean$chlorophyll, 0.25, na.rm = TRUE)
Q3 <- quantile(waterqual_clean$chlorophyll, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
lower_fence <- Q1 - 1.5 * IQR
upper_fence <- Q3 + 1.5 * IQR
#Filtering to exclude outliers
nooutliers_newDF <- waterqual_clean %>%
  dplyr::filter(chlorophyll>= lower_fence & chlorophyll <= upper_fence)


Q1 <- quantile(waterqual_clean$turbidity, 0.25, na.rm = TRUE)
Q3 <- quantile(waterqual_clean$turbidity, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
lower_fence <- Q1 - 1.5 * IQR
upper_fence <- Q3 + 1.5 * IQR
#Filtering to exclude outliers
nooutliers_newDF <- waterqual_clean %>%
  dplyr::filter(turbidity>= lower_fence & turbidity <= upper_fence)


Q1 <- quantile(waterqual_clean$fDOM, 0.25, na.rm = TRUE)
Q3 <- quantile(waterqual_clean$fDOM, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
lower_fence <- Q1 - 1.5 * IQR
upper_fence <- Q3 + 1.5 * IQR
#Filtering to exclude outliers
nooutliers_newDF <- waterqual_clean %>%
  dplyr::filter(fDOM>= lower_fence & fDOM <= upper_fence)





#SECTION 2.3: Take daily and monthly averages 
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

#SECTION 2.4: Full join on water quality dataframes
All_wq <- full_join(WQ_daily, WQ_monthly, by = c("siteID", "Year", "Month")) %>%
  rename(Cond_daily = Conductance.x, DO_daily = DO.x, pH_daily = pH.x, Cholophyll_daily = Chrolophyll.x, Turb_daily = Turbidity.x, fDOM_daily = fDOM.x, Cond_monthly = Conductance.y, DO_monthly = DO.y, pH_monthly = pH.y, Chlorophyll_monthly = Chrolophyll.y, Turb_monthly = Turbidity.y, fDOM_monthly = fDOM.y) %>% 
  mutate(Date = ymd(paste(Year, Month, Day, sep = "-")))





#SECTION 3: Discharge (from 1-minute frequency to daily and monthly averages)

#SECTION 3.1: Add date columns
Discharge$Year <- substr(Discharge$endDate, 1, 4)
Discharge$Month <- substr(Discharge$endDate, 6, 7)
Discharge$Day <- substr(Discharge$endDate, 9, 10)

#SECTION 3.2: Remove outliers
Q1 <- quantile(Discharge$maxpostDischarge, 0.25, na.rm = TRUE)
Q3 <- quantile(Discharge$maxpostDischarge, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
lower_fence <- Q1 - 1.5 * IQR
upper_fence <- Q3 + 1.5 * IQR
#Filtering to exclude outliers
nooutliers_newDF <- Discharge %>%
  dplyr::filter(maxpostDischarge>= lower_fence & maxpostDischarge <= upper_fence)

#SECTION 3.3: Take daily and monthly averages 
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

#SECTION 3.4: Full join on discharge dataframes
All_dis <- full_join(dis_daily, dis_monthly, by = c("siteID", "Year", "Month")) %>%
  rename(disDaily_lps = Mean_discharge.x, disMonthly_lps = Mean_discharge.y) %>% 
  mutate(Date = ymd(paste(Year, Month, Day, sep = "-")))




#SECTION 4: NEON Precipitation (from 30-minute frequency to daily and monthly averages)

#SECTION 4.1: Add date columns
precip_raw$Year <- substr(precip_raw$endDateTime, 1, 4)
precip_raw$Month <- substr(precip_raw$endDateTime, 6, 7)
precip_raw$Day <- substr(precip_raw$endDateTime, 9, 10)

#SECTION 4.2: Remove outliers (take out)
Q1 <- quantile(precip_raw$maxpostDischarge, 0.25, na.rm = TRUE)
Q3 <- quantile(precip_raw$maxpostDischarge, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
lower_fence <- Q1 - 1.5 * IQR
upper_fence <- Q3 + 1.5 * IQR
#Filtering to exclude outliers
nooutliers_newDF <- Discharge %>%
  dplyr::filter(maxpostDischarge>= lower_fence & maxpostDischarge <= upper_fence)

#SECTION 4.3: Take daily and monthly averages 
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
    secPrecip = mean(secPrecipBulk, na.rm = TRUE))

#SECTION 4.4: Full join on precipitation dataframes
All_precip <- full_join(precip_daily, precip_monthly, by = c("siteID", "Year", "Month")) %>%
  ungroup() %>% 
  rename(priDailyAvg = priPrecip.x, secDailyAvg = secPrecip.x, priMonthlyAvg = priPrecip.y, secMonthlyAvg = secPrecip.y) %>% 
  mutate(precipDaily_mm = rowMeans(select(., priDailyAvg, secDailyAvg), na.rm = TRUE)) %>%
  select(-priDailyAvg, -secDailyAvg) %>% 
  mutate(precipMonthly_mm = rowMeans(select(., priMonthlyAvg, secMonthlyAvg), na.rm = TRUE)) %>%
  select(-priMonthlyAvg, -secMonthlyAvg)%>% 
  mutate(Date = ymd(paste(Year, Month, Day, sep = "-")))




#SECTION 5: Water temperature (from 30-minute frequency to daily and monthly averages)

#SECTION 5.1: Add date columns
watertemp_clean$Year <- substr(watertemp_clean$endDateTime, 1, 4)
watertemp_clean$Month <- substr(watertemp_clean$endDateTime, 6, 7)
watertemp_clean$Day <- substr(watertemp_clean$endDateTime, 9, 10)

#SECTION 5.2: Remove outliers
Q1 <- quantile(watertemp_clean$surfWaterTempMean, 0.25, na.rm = TRUE)
Q3 <- quantile(watertemp_clean$surfWaterTempMean, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
lower_fence <- Q1 - 1.5 * IQR
upper_fence <- Q3 + 1.5 * IQR
#Filtering to exclude outliers
nooutliers_newDF <- watertemp_clean %>%
  dplyr::filter(surfWaterTempMean>= lower_fence & surfWaterTempMean <= upper_fence)

#SECTION 5.3: Take daily and monthly averages 
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

#SECTION 5.4: Full join on water temperature dataframes
All_wt <- full_join(WT_daily, WT_monthly, by = c("siteID", "Year", "Month")) %>%
  rename(tempDaily_C = Mean_temp.x, tempMonthly_C = Mean_temp.y) %>% 
  mutate(Date = ymd(paste(Year, Month, Day, sep = "-")))






#SECTION 6: Macroinvertebrate data

#SECTION 6.1: Create tibbles from the list for field data and taxonomy data

invert.tax <-
  invert[[5]] %>% 
  as_tibble()

str(invert.tax)


invert.field <-
  invert[[3]] %>% 
  as_tibble()

str(invert.field)


# Macroinvertebrate data

invert.all <-
  inner_join(invert.tax, invert.field, by = "sampleID")

str(invert.all)



# SECTION 6.2: Load in the data for the Water Quality
waterqual_clean <- read_csv("DataWaterQuality/waterqual.csv")

# Clean the water quality data

clean_wq <- waterqual_clean %>% 
  rename(collectDate.x = startDateTime)


# Combine the macroinvertebrate data and the water quality data

combined.data <-
  left_join(NEONFILE.CLEAN2, waterqual_clean, by = )




NEONFILE.CLEAN2 <- invert.all %>% 
  select(collectDate.x,sampleID, scientificName, estimatedTotalCount, benthicArea, siteID.x) %>% 
  separate(collectDate.x, c("collectDate", "time"), sep = " ") %>% 
  mutate(Sample = substr(sampleID, 1, 11)) %>% 
  group_by(collectDate, time, scientificName, siteID.x ) %>% 
  filter(scientificName %in% c("Plecoptera sp.", "Trichoptera sp.", ("Ephemeroptera sp."))) %>% 
  summarize(inv.density = sum(estimatedTotalCount)/sum(benthicArea)) 

kable(NEONFILE.CLEAN2)
save(NEONFILE.CLEAN2, file = "dataMacro/CleanInvert.RData")


Q1 <- quantile(cleanInvert$surfWaterTempMean, 0.25, na.rm = TRUE)
Q3 <- quantile(watertemp_clean$surfWaterTempMean, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
lower_fence <- Q1 - 1.5 * IQR
upper_fence <- Q3 + 1.5 * IQR
#Filtering to exclude outliers
nooutliers_newDF <- watertemp_clean %>%
  dplyr::filter(surfWaterTempMean>= lower_fence & surfWaterTempMean <= upper_fence)


#SECTION 6.2: Take daily and monthly averages

# Take daily averages 
MI_daily <- cleanInvert %>%
  group_by(siteID.x, collectDate.x, scientificName) %>%
  summarise(dailyDensity = mean(inv.density, na.rm = TRUE))
  


# Add date columns
MI_daily$Year <- substr(MI_daily$collectDate.x, 1, 4)
MI_daily$Month <- substr(MI_daily$collectDate.x, 6, 7)
MI_daily$Day <- substr(MI_daily$collectDate.x, 9, 10)
MI_daily$Date <- substr(MI_daily$collectDate.x, 1, 10)


# Take monthly averages
MI_monthly <- MI_daily %>%
  group_by(siteID.x, scientificName, Year, Month) %>%
  mutate(monthlyDensity = mean(dailyDensity, na.rm = TRUE))%>% 
  mutate(Date = ymd(paste(Year, Month, Day, sep = "-")))




#SECTION 6.3: Full join on macroinverebrate dataframes

# Seclect for desired columns
All_mcroinv <- MI_monthly %>%
  select(siteID.x, Date, scientificName, dailyDensity, monthlyDensity) %>%
  rename(siteID = siteID.x)%>% 
  mutate(Date = ymd(paste(Year, Month, Day, sep = "-")))






#SECTION 7: Left join on all variables

#Complete dataframe
final_df <- purrr::reduce(list(All_dis, All_nitrate, All_precip, All_wq, All_wt), 
                         dplyr::left_join, by = c('Date', 'siteID')) %>% 
  select(-c("Year"|"Month"|"Day")) %>% 
  select(-matches("\\."))



Final_df <- purrr::reduce(list(final_df, All_mcroinv), 
                          dplyr::full_join, by = c('Date', 'siteID')) %>% 
  select(-c("Year"|"Month")) %>% 
  select(-matches("\\."))



#Rearrange column order
Final_df <- Final_df[, c("siteID", "Date", "disDaily_lps", "nitDaily_mcmolpl", "precipDaily_mm", 
                         "Cond_daily", "DO_daily", "pH_daily", "Cholophyll_daily", "Turb_daily", "fDOM_daily", "tempDaily_C", 
                         "disMonthly_lps", "nitMonthly_mcmolpl", "precipMonthly_mm", 
                         "Cond_monthly", "DO_monthly", "pH_monthly", "Chlorophyll_monthly", "Turb_monthly", "fDOM_monthly", 
                         "tempMonthly_C", "scientificName", "dailyDensity", "monthlyDensity")]



#SECTION 8: Export to csv
#Export to CSV
write.csv(Final_df, file = "data//Final_DF_2.csv", row.names = FALSE)


