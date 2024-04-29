#Impacts of Shifting Climate Variables on Nutrient Dynamics 
#and Macroinvertebrate Abundance in CO streams
#Data collection 
#Billy Johnson, Gabriella Vieira
#Mentor: Carolina Barbosa
#11/16/2023




#Load packages
library(dataRetrieval)
library(tidyverse)
library(flextable)
library(neonUtilities)
library(tibble)



#RETREIVE DATA FROM NEON USING NEONUTILITIES 



#SECTION 1: Discharge data

#SECTION 1.1: Load products 
discharge <- loadByProduct(dpID = "DP4.00130.001",
                           site = c("ARIK", "COMO", "WLOU"),
                           startdate = "2016-01",
                           enddate = "2023-01",
                           check.size = F)

#SECTION 1.2: Tibble the discharge data
dischargeNeon <- as_tibble(discharge$csd_continuousDischarge[, c("siteID", "endDate", "equivalentStage", "nonSystematicUnc", "systematicUnc", "stageUnc","maxpostDischarge", "withParaUncQLower2Std", "withParaUncQUpper2Std", "withRemnUncQLower2Std", "withRemnUncQUpper2Std", "dischargeFinalQF", "dischargeFinalQFSciRvw")])

#SECTION 1.3: Export to CSV file
write.csv(dischargeNeon, "data//dischargeNeon.csv", row.names = FALSE)





#SECTION 2: Water temperature data

#SECTION 2.1: Load products 
Watertemp <- loadByProduct(dpID = "DP1.20053.001",
                           site = c("ARIK", "COMO", "WLOU"),
                           startdate = "2016-01",
                           enddate = "2023-01",
                           check.size = F)

#SECTION 2.2: Tibble the water temperature data
water_temp30 <- as_tibble(Watertemp$TSW_30min[, c("siteID", "startDateTime", "endDateTime", "surfWaterTempMean", "surfWaterTempMinimum", "surfWaterTempMaximum", "surfWaterTempVariance")])

watertemp_clean <- water_temp30[complete.cases(water_temp30$surfWaterTempMean), ] #Remove NA values

#SECTION 2.3: Export to CSV file
write.csv(watertemp_clean, "data//watertemp_clean.csv", row.names = FALSE)





#SECTION 3: Water quality data

#SECTION 3.1: Load products 
Waterquality <- loadByProduct(dpID = "DP1.20288.001",
                              site = c("ARIK", "COMO", "WLOU"),
                              startdate = "2016-01",
                              enddate = "2023-01",
                              check.size = F)

#SECTION 3.2: Tibble the water quality data
water_qual <- as_tibble(Waterquality$waq_instantaneous[, c("siteID", "startDateTime", "endDateTime", "specificConductance","specificCondFinalQF", "dissolvedOxygen", "pH", "chlorophyll", "turbidity", "fDOM")])

waterqual_clean <- water_qual[complete.cases(water_qual$specificConductance), ] #Remove NA values in the conductance field

#SECTION 3.3: Export to CSV file
write.csv(waterqual_clean, "data//waterqual.csv", row.names = FALSE)





#SECTION 4: Nitrate data


#SECTION 4.1: Load products 
nitrate <- loadByProduct(dpID = "DP1.20033.001",
                         site = c("ARIK", "COMO", "WLOU"),
                         startdate = "2016-01",
                         enddate = "2023-01",
                         check.size = F)


#SECTION 4.2: Tibble the nitrate data
nitrate_raw <- as_tibble(nitrate$NSW_15_minute[, c("siteID", "startDateTime", "endDateTime", "surfWaterNitrateMean","surfWaterNitrateMinimum", "surfWaterNitrateMaximum", "surfWaterNitrateVariance")])

nitrate_clean <- nitrate_raw[complete.cases(nitrate_raw$surfWaterNitrateMean), ] #Remove NA values

#SECTION 4.3: Export to CSV file
write.csv(nitrate_clean, "data//nitrate.csv", row.names = FALSE)

write.csv(dischargevars, file = "data//dischargevars.csv", row.names = FALSE) #Table containing variable codes, definitions, and measurement units






#SECTION 5: Precipitation data

#SECTION 5.1: Load products
precip_neon <- loadByProduct(dpID = "DP1.00006.001",
                             site = c("ARIK", "COMO", "WLOU"),
                             startdate = "2016-01",
                             enddate = "2023-01",
                             check.size = F)

#SECTION 5.2: Tibble the precipitation data
precip_prim <- as_tibble(precip_neon$PRIPRE_30min[, c("siteID", "startDateTime", "endDateTime", "priPrecipBulk","priPrecipNumPts", "priPrecipExpUncert", "priPrecipFinalQF")])

precip_sec <- as_tibble(precip_neon$SECPRE_30min[, c("siteID", "startDateTime", "endDateTime", "secPrecipBulk","secPrecipExpUncert", "secPrecipRangeQF", "secPrecipSciRvwQF")])

#Merge primary and secondary dataframes
precip_raw <- bind_rows(precip_prim, precip_sec)

#SECTION 5.3: Export to CSV file
write.csv(precip_raw, "data//neonPrecipitation.csv", row.names = FALSE)







#SECTION 2.6: Macro invertebrate data
#SECTION 2.6.A: Load products
#SECTION 2.6.B: Tibble the macro invertebrate data
#SECTION 2.6.C: Export to CSV file