#Impacts of Shifting Climate Variables on Nutrient Dynamics 
#and Macroinvertebrate Abundance in CO streams
#Data exploration and analysis
#Billy Johnson, Gabriella Vieira
#Mentor: Carolina Barbosa
#4/3/2024


#load packages
library(here)
library(tidyverse)
library(feather)
library(readr)
library(dplyr)
library(ggplot2)
library(ggfortify)
library(readxl)
library(plotly)
library(broom)
library(ggpubr)
library(rstatix)
library(trend)
library(xts)
library(lubridate)
library(seasonal)
library(kableExtra)


#read data
final_df <- read.csv("data//Final_DF_2.csv")
Macro_DF <- load("DataMacro/CleanInvert.RData")



#SECTION 1: SUBSET MONTHLY DATA

# Extract month and year from Date column
final_df$Date <- as.Date(final_df$Date)
final_df <- final_df %>%
  mutate(YearMonth = format(Date, "%Y-%m"))

# Group by YearMonth and summarize to calculate average for each variable
monthly_data <- final_df %>%
  group_by(siteID, YearMonth) %>%
  summarize(
    disMonthly_lps = mean(disMonthly_lps, na.rm = TRUE),
    nitMonthly_mcmolpl = mean(nitMonthly_mcmolpl, na.rm = TRUE),
    precipMonthly_mm = mean(precipMonthly_mm, na.rm = TRUE),
    Cond_monthly = mean(Cond_monthly, na.rm = TRUE),
    DO_monthly = mean(DO_monthly, na.rm = TRUE),
    pH_monthly = mean(pH_monthly, na.rm = TRUE),
    Chlorophyll_monthly = mean(Chlorophyll_monthly, na.rm = TRUE),
    Turb_monthly = mean(Turb_monthly, na.rm = TRUE),
    fDOM_monthly = mean(fDOM_monthly, na.rm = TRUE),
    tempMonthly_C = mean(tempMonthly_C, na.rm = TRUE),
  ) %>% 
  rename(Date = YearMonth)


#convert to date class
monthly_data$Date <- as.Date(paste(monthly_data$Date, "-01", sep = ""), format = "%Y-%m-%d")







#SECTION 2: TEST FOR CHANGES OVER TIME (MANN-KENDALL AND SEN'S SLOPE)

#Mann-Kendall test for data trends

#Separate sites by siteID for analysis
ARIK <- subset(monthly_data, siteID == "ARIK")
COMO <- subset(monthly_data, siteID == "COMO")
WLOU <- subset(monthly_data, siteID == "WLOU")




# Variable 1: Precipitation

#ARIK

#Mann Kendall test
#Create separate frame for variables
mk_vars <- ARIK[ c("Date", "precipMonthly_mm")]

# Check the class of precipMonthly_mm
class(mk_vars$precipMonthly_mm)

# Check for missing values in precipMonthly_mm
sum(is.na(mk_vars$precipMonthly_mm))

# Remove missing values
mk_vars <- na.omit(mk_vars)

# Check the class and structure of monthly_data after removing missing values
str(mk_vars)

# Now try mk.test
mk.test(mk_vars$precipMonthly_mm)

#Evidence of a significant negative trend, proceed to Sen's slope


#Sen's slope
sens.slope(mk_vars$precipMonthly_mm)







#WLOU

#Create separate dataframe for variables
mk_vars <- WLOU[ c("Date", "precipMonthly_mm")]

# Check the class of precipMonthly_mm
class(mk_vars$precipMonthly_mm)

# Check for missing values in precipMonthly_mm
sum(is.na(mk_vars$precipMonthly_mm))

# Remove missing values
mk_vars <- na.omit(mk_vars)

# Check the class and structure of monthly_data after removing missing values
str(mk_vars)

# Now try mk.test
mk.test(mk_vars$precipMonthly_mm)




# Variable 2: Discharge

#ARIK
#Create separate frame for variables
mk_vars <- ARIK[ c("Date", "disMonthly_lps")]

# Check the class of disMonthly_lps
class(mk_vars$disMonthly_lps)

# Check for missing values in disMonthly_lps
sum(is.na(mk_vars$disMonthly_lps))

# Remove missing values
mk_vars <- na.omit(mk_vars)

# Check the class and structure of monthly_data after removing missing values
str(mk_vars)

# Now try mk.test
mk.test(mk_vars$disMonthly_lps)

#evidence of a significant negative trend, perform sen slope test
sens.slope(mk_vars$disMonthly_lps)



#WLOU


#Create separate dataframe for variables
mk_vars <- WLOU[ c("Date", "disMonthly_lps")]

# Check the class of disMonthly_lps
class(WLOU$mk_vars)

# Check for missing values in disMonthly_lps
sum(is.na(WLOU$mk_vars))

# Remove missing values
mk_vars <- na.omit(mk_vars)

# Check the class and structure of monthly_data after removing missing values
str(mk_vars)

# Now try mk.test
mk.test(mk_vars$disMonthly_lps)






# Variable 3: Nitrate

#ARIK

#Create separate frame for variables
mk_vars <- ARIK[ c("Date", "nitMonthly_mcmolpl")]

# Check the class of nitMonthly_mcmolpl
class(mk_vars$nitMonthly_mcmolpl)

# Check for missing values in nitMonthly_mcmolpl
sum(is.na(mk_vars$nitMonthly_mcmolpl))

# Remove missing values
mk_vars <- na.omit(mk_vars)

# Check the class and structure of monthly_data after removing missing values
str(mk_vars)

# Now try mk.test
mk.test(mk_vars$nitMonthly_mcmolpl)







#WLOU

#Create separate dataframe for variables
mk_vars <- WLOU[ c("Date", "nitMonthly_mcmolpl")]

# Check the class of nitMonthly_mcmolpl
class(mk_vars$nitMonthly_mcmolpl)

# Check for missing values in nitMonthly_mcmolpl
sum(is.na(mk_vars$nitMonthly_mcmolpl))

# Remove missing values
mk_vars <- na.omit(mk_vars)

# Check the class and structure of monthly_data after removing missing values
str(mk_vars)

# Now try mk.test
mk.test(mk_vars$nitMonthly_mcmolpl)


#Sen's slope
sens.slope(mk_vars$nitMonthly_mcmolpl)








# Variable 4: Temperature

#ARIK

#Create separate frame for variables
mk_vars <- ARIK[ c("Date", "tempMonthly_C")]

# Check the class of tempMonthly_C
class(mk_vars$tempMonthly_C)

# Check for missing values in tempMonthly_C
sum(is.na(mk_vars$tempMonthly_C))

# Remove missing values
mk_vars <- na.omit(mk_vars)

# Check the class and structure of monthly_data after removing missing values
str(mk_vars)

# Now try mk.test
mk.test(mk_vars$tempMonthly_C)








#WLOU

#Create separate dataframe for variables
mk_vars <- WLOU[ c("Date", "tempMonthly_C")]

# Check the class of tempMonthly_C
class(mk_vars$tempMonthly_C)

# Check for missing values in tempMonthly_C
sum(is.na(mk_vars$tempMonthly_C))

# Remove missing values
mk_vars <- na.omit(mk_vars)

# Check the class and structure of monthly_data after removing missing values
str(mk_vars)

# Now try mk.test
mk.test(mk_vars$tempMonthly_C)








# Variable 5: Conductivity

#ARIK

#Create separate frame for variables
mk_vars <- ARIK[ c("Date", "Cond_monthly")]

# Check the class of Cond_monthly
class(mk_vars$Cond_monthly)

# Check for missing values in Cond_monthly
sum(is.na(mk_vars$Cond_monthly))

# Remove missing values
mk_vars <- na.omit(mk_vars)

# Check the class and structure of monthly_data after removing missing values
str(mk_vars)

# Now try mk.test
mk.test(mk_vars$Cond_monthly)

#Sen's slope
sens.slope(mk_vars$Cond_monthly)






#COMO

#Create separate frame for variables
mk_vars <- COMO[ c("Date", "Cond_monthly")]

# Check the class of Cond_monthly
class(mk_vars$Cond_monthly)

# Check for missing values in Cond_monthly
sum(is.na(mk_vars$Cond_monthly))

# Remove missing values
mk_vars <- na.omit(mk_vars)

# Check the class and structure of monthly_data after removing missing values
str(mk_vars)

# Now try mk.test
mk.test(mk_vars$Cond_monthly)

#Sen's slope
sens.slope(mk_vars$Cond_monthly)









#WLOU

#Create separate frame for variables
mk_vars <- WLOU[ c("Date", "Cond_monthly")]

# Check the class of Cond_monthly
class(mk_vars$Cond_monthly)

# Check for missing values in Cond_monthly
sum(is.na(mk_vars$Cond_monthly))

# Remove missing values
mk_vars <- na.omit(mk_vars)

# Check the class and structure of monthly_data after removing missing values
str(mk_vars)

# Now try mk.test
mk.test(mk_vars$Cond_monthly)

#Sen's slope
sens.slope(mk_vars$Cond_monthly)




# Variable 6: DO

#ARIK

#Create separate frame for variables
mk_vars <- ARIK[ c("Date", "DO_monthly")]

# Check the class of DO_monthly
class(mk_vars$DO_monthly)

# Check for missing values in DO_monthly
sum(is.na(mk_vars$DO_monthly))

# Remove missing values
mk_vars <- na.omit(mk_vars)

# Check the class and structure of monthly_data after removing missing values
str(mk_vars)

# Now try mk.test
mk.test(mk_vars$DO_monthly)

#Sen's slope
sens.slope(mk_vars$DO_monthly)




#COMO

#Create separate frame for variables
mk_vars <- COMO[ c("Date", "DO_monthly")]

# Check the class of DO_monthly
class(mk_vars$DO_monthly)

# Check for missing values in DO_monthly
sum(is.na(mk_vars$DO_monthly))

# Remove missing values
mk_vars <- na.omit(mk_vars)

# Check the class and structure of monthly_data after removing missing values
str(mk_vars)

# Now try mk.test
mk.test(mk_vars$DO_monthly)

#Sen's slope
sens.slope(mk_vars$DO_monthly)






#WLOU

#Create separate frame for variables
mk_vars <- WLOU[ c("Date", "DO_monthly")]

# Check the class of DO_monthly
class(mk_vars$DO_monthly)

# Check for missing values in DO_monthly
sum(is.na(mk_vars$DO_monthly))

# Remove missing values
mk_vars <- na.omit(mk_vars)

# Check the class and structure of monthly_data after removing missing values
str(mk_vars)

# Now try mk.test
mk.test(mk_vars$DO_monthly)
#Sen's slope
sens.slope(mk_vars$DO_monthly)






# Variable 7: pH

#ARIK

#Create separate frame for variables
mk_vars <- ARIK[ c("Date", "pH_monthly")]

# Check the class of pH_monthly
class(mk_vars$pH_monthly)

# Check for missing values in pH_monthly
sum(is.na(mk_vars$pH_monthly))

# Remove missing values
mk_vars <- na.omit(mk_vars)

# Check the class and structure of monthly_data after removing missing values
str(mk_vars)

# Now try mk.test
mk.test(mk_vars$pH_monthly)

#Sen's slope
sens.slope(mk_vars$pH_monthly)





#COMO

#Create separate frame for variables
mk_vars <- COMO[ c("Date", "pH_monthly")]

# Check the class of pH_monthly
class(mk_vars$pH_monthly)

# Check for missing values in pH_monthly
sum(is.na(mk_vars$pH_monthly))

# Remove missing values
mk_vars <- na.omit(mk_vars)

# Check the class and structure of monthly_data after removing missing values
str(mk_vars)

# Now try mk.test
mk.test(mk_vars$pH_monthly)

#Sen's slope
sens.slope(mk_vars$pH_monthly)







#WLOU

#Create separate frame for variables
mk_vars <- WLOU[ c("Date", "pH_monthly")]

# Check the class of pH_monthly
class(mk_vars$pH_monthly)

# Check for missing values in pH_monthly
sum(is.na(mk_vars$pH_monthly))

# Remove missing values
mk_vars <- na.omit(mk_vars)

# Check the class and structure of monthly_data after removing missing values
str(mk_vars)

# Now try mk.test
mk.test(mk_vars$pH_monthly)

#evidence of a significant positive trend, perform sen slope test
sens.slope(mk_vars$pH_monthly)





# Variable 8: chlorophyll

#ARIK

#Create separate frame for variables
mk_vars <- ARIK[ c("Date", "Chlorophyll_monthly")]

# Check the class of Chlorophyll_monthly
class(mk_vars$Chlorophyll_monthly)

# Check for missing values in Chlorophyll_monthly
sum(is.na(mk_vars$Chlorophyll_monthly))

# Remove missing values
mk_vars <- mk_vars %>% 
  filter(Chlorophyll_monthly <= 5000) %>% 
  filter(Chlorophyll_monthly >= -200) %>% 
  na.omit(mk_vars)

# Check the class and structure of monthly_data after removing missing values
str(mk_vars)

# Now try mk.test
mk.test(mk_vars$Chlorophyll_monthly)

#evidence of a significant negative trend, perform sen slope test
sens.slope(mk_vars$Chlorophyll_monthly)









# Variable 8: Turbidity

#ARIK

#Create separate frame for variables
mk_vars <- ARIK[ c("Date", "Turb_monthly")]

# Check the class of Turb_monthly
class(mk_vars$Turb_monthly)

# Check for missing values in Turb_monthly
sum(is.na(mk_vars$Turb_monthly))

# Remove missing values
mk_vars <- na.omit(mk_vars)

# Check the class and structure of monthly_data after removing missing values
str(mk_vars)

# Now try mk.test
mk.test(mk_vars$Turb_monthly)

#evidence of a significant negative trend, perform sen slope test
sens.slope(mk_vars$Turb_monthly)





#WLOU

#Create separate frame for variables
mk_vars <- WLOU[ c("Date", "Turb_monthly")]

# Check the class of Turb_monthly
class(mk_vars$Turb_monthly)

# Check for missing values in Turb_monthly
sum(is.na(mk_vars$Turb_monthly))

# Remove missing values
mk_vars <- na.omit(mk_vars)

# Check the class and structure of monthly_data after removing missing values
str(mk_vars)

# Now try mk.test
mk.test(mk_vars$Turb_monthly)

#evidence of a significant negative trend, perform sen slope test
sens.slope(mk_vars$Turb_monthly)





# Variable 9: fDOM

#ARIK

#Create separate frame for variables
mk_vars <- ARIK[ c("Date", "fDOM_monthly")]

# Check the class of fDOM_monthly
class(mk_vars$fDOM_monthly)

# Check for missing values in fDOM_monthly
sum(is.na(mk_vars$fDOM_monthly))

# Remove missing values
mk_vars <- na.omit(mk_vars)

# Check the class and structure of monthly_data after removing missing values
str(mk_vars)

# Now try mk.test
mk.test(mk_vars$fDOM_monthly)

#evidence of a significant positive trend, perform sen slope test
sens.slope(mk_vars$fDOM_monthly)




#WLOU

#Create separate frame for variables
mk_vars <- WLOU[ c("Date", "fDOM_monthly")]

# Check the class of fDOM_monthly
class(mk_vars$fDOM_monthly)

# Check for missing values in fDOM_monthly
sum(is.na(mk_vars$fDOM_monthly))

# Remove missing values
mk_vars <- na.omit(mk_vars)

# Check the class and structure of monthly_data after removing missing values
str(mk_vars)

# Now try mk.test
mk.test(mk_vars$fDOM_monthly)








#SECTION 3: TEST FOR CORRELATIONS BETWEEN NITRATE AND OTHER VARIABLES

#ARIK
#Kendall's tau test for Nitrate and precip

ARIK %>% filter(!is.na(nitMonthly_mcmolpl),
                !is.na(precipMonthly_mm)) %>% 
  summarise(correlation = tidy(cor.test(precipMonthly_mm, nitMonthly_mcmolpl, method = "kendall"))$estimate[1],
            p_value = tidy(cor.test(precipMonthly_mm, nitMonthly_mcmolpl, method = "kendall"))$p.value)



#Spearman's correlation test for nitrate and precip
ARIK %>%
  filter(!is.na(nitMonthly_mcmolpl),                
         !is.na(precipMonthly_mm)) %>% 
  with(cor.test(precipMonthly_mm, nitMonthly_mcmolpl,method = "spearman"))



#Kendall's tau test for Nitrate and temperature

ARIK %>% filter(!is.na(nitMonthly_mcmolpl),
                !is.na(tempMonthly_C)) %>% 
  summarise(correlation = tidy(cor.test(tempMonthly_C, nitMonthly_mcmolpl, method = "kendall"))$estimate[1],
            p_value = tidy(cor.test(tempMonthly_C, nitMonthly_mcmolpl, method = "kendall"))$p.value)



#Spearman's correlation test for nitrate and temperature
ARIK %>%
  filter(!is.na(nitMonthly_mcmolpl),                
         !is.na(tempMonthly_C)) %>% 
  with(cor.test(tempMonthly_C, nitMonthly_mcmolpl,method = "spearman"))



#Kendall's tau test for Nitrate and discharge

ARIK %>% filter(!is.na(nitMonthly_mcmolpl),
                !is.na(disMonthly_lps)) %>% 
  summarise(correlation = tidy(cor.test(disMonthly_lps, nitMonthly_mcmolpl, method = "kendall"))$estimate[1],
            p_value = tidy(cor.test(disMonthly_lps, nitMonthly_mcmolpl, method = "kendall"))$p.value)



#Spearman's correlation test for nitrate and discharge
ARIK %>%
  filter(!is.na(nitMonthly_mcmolpl),                
         !is.na(disMonthly_lps)) %>% 
  with(cor.test(disMonthly_lps, nitMonthly_mcmolpl,method = "spearman"))




#WLOU
#Kendall's tau test for Nitrate and precip

WLOU %>% filter(!is.na(nitMonthly_mcmolpl),
                !is.na(precipMonthly_mm)) %>% 
  summarise(correlation = tidy(cor.test(precipMonthly_mm, nitMonthly_mcmolpl, method = "kendall"))$estimate[1],
            p_value = tidy(cor.test(precipMonthly_mm, nitMonthly_mcmolpl, method = "kendall"))$p.value)



#Spearman's correlation test for nitrate and precip
WLOU %>%
  filter(!is.na(nitMonthly_mcmolpl),                
         !is.na(precipMonthly_mm)) %>% 
  with(cor.test(precipMonthly_mm, nitMonthly_mcmolpl,method = "spearman"))



#Kendall's tau test for Nitrate and temperature

WLOU %>% filter(!is.na(nitMonthly_mcmolpl),
                !is.na(tempMonthly_C)) %>% 
  summarise(correlation = tidy(cor.test(tempMonthly_C, nitMonthly_mcmolpl, method = "kendall"))$estimate[1],
            p_value = tidy(cor.test(tempMonthly_C, nitMonthly_mcmolpl, method = "kendall"))$p.value)



#Spearman's correlation test for nitrate and temperature
WLOU %>%
  filter(!is.na(nitMonthly_mcmolpl),                
         !is.na(tempMonthly_C)) %>% 
  with(cor.test(tempMonthly_C, nitMonthly_mcmolpl,method = "spearman"))



#Kendall's tau test for Nitrate and discharge

WLOU %>% filter(!is.na(nitMonthly_mcmolpl),
                !is.na(disMonthly_lps)) %>% 
  summarise(correlation = tidy(cor.test(disMonthly_lps, nitMonthly_mcmolpl, method = "kendall"))$estimate[1],
            p_value = tidy(cor.test(disMonthly_lps, nitMonthly_mcmolpl, method = "kendall"))$p.value)



#Spearman's correlation test for nitrate and discharge
WLOU %>%
  filter(!is.na(nitMonthly_mcmolpl),                
         !is.na(disMonthly_lps)) %>% 
  with(cor.test(disMonthly_lps, nitMonthly_mcmolpl,method = "spearman"))











#SECTION 4: DATA VISUALIZATION 


#RQ1: changes in nitrate concentrations over 

#We want to represent the stable nitrate concentrations over time

monthly_data %>%
  filter(siteID != "COMO",
         nitMonthly_mcmolpl <= 30,
         !is.na(nitMonthly_mcmolpl)) %>%
  ggplot(aes(x = Date)) +
  geom_line(aes(y = nitMonthly_mcmolpl, linetype = siteID), linewidth = .9) +
  geom_smooth(aes(y = nitMonthly_mcmolpl, linetype = siteID), method = 'lm', color = "darkgreen") +  # Add geom_smooth for trend lines
  scale_y_continuous(name = "Nitrate (micromol/L)") +
  labs(color = "Variables") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, family = "serif"),
        axis.text = element_text(family = "serif"),  # Change font family for axis labels
        axis.title = element_text(family = "serif"),  # Change font family for axis titles
        legend.text = element_text(family = "serif"),  # Change font family for legend text
        legend.title = element_text(family = "serif"))  # Change font family for legend title



# Same graph, no trendlines
monthly_data %>% 
  filter(siteID!= "COMO",
         nitMonthly_mcmolpl <= 30,
         !is.na(nitMonthly_mcmolpl)) %>%
  ggplot(aes(x = Date)) +
  geom_line(aes(y = nitMonthly_mcmolpl, linetype = siteID), linewidth = 1) +
  scale_y_continuous(
    name = "Nitrate (micromol/L)",) +
  labs(color = "Variables") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, family = "serif"))





#RQ 2: changes in precip, temperature, and discharge

#ARIK precip decreasing

monthly_data %>%
  filter(siteID == "ARIK",
         precipMonthly_mm >= -0.05,
         !is.na(precipMonthly_mm)) %>%
  ggplot(aes(x = Date)) +
  geom_line(aes(y = precipMonthly_mm, linetype = siteID), 
            show.legend = FALSE,
            linewidth = .8) +
  geom_smooth(aes(y = precipMonthly_mm, linetype = siteID), 
              method = 'lm', 
              color = "darkblue",
              show.legend = FALSE) +  # Add geom_smooth for trend lines
  scale_y_continuous(name = "Precipitation (mm)") +
  labs(color = "Variables") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, family = "serif"),
        axis.text = element_text(family = "serif"),  # Change font family for axis labels
        axis.title = element_text(family = "serif"),  # Change font family for axis titles
        legend.text = element_text(family = "serif"),  # Change font family for legend text
        legend.title = element_text(family = "serif"))  # Change font family for legend title







#ARIK discharge decreasing

monthly_data %>%
  filter(siteID == "ARIK",
         !is.na(disMonthly_lps)) %>%
  ggplot(aes(x = Date)) +
  geom_line(aes(y = disMonthly_lps, linetype = siteID), 
            show.legend = FALSE, 
            linewidth = .8) +
  geom_smooth(aes(y = disMonthly_lps, linetype = siteID), 
              method = 'lm', 
              color = "darkorange",
              show.legend = FALSE) +  # Add geom_smooth for trend lines
  scale_y_continuous(name = "Discharge (L/s)") +
  labs(color = "Variables") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, family = "serif"),
        axis.text = element_text(family = "serif"),  # Change font family for axis labels
        axis.title = element_text(family = "serif"),  # Change font family for axis titles
        legend.text = element_text(family = "serif"),  # Change font family for legend text
        legend.title = element_text(family = "serif"))  # Change font family for legend title




#ARIK temperature stays the same

monthly_data %>%
  filter(siteID == "ARIK",
         tempMonthly_C >= -0.05,
         !is.na(tempMonthly_C)) %>%
  ggplot(aes(x = Date)) +
  geom_line(aes(y = tempMonthly_C, linetype = siteID), linewidth = .9) +
  geom_smooth(aes(y = tempMonthly_C, linetype = siteID), method = 'lm', color = "darkorange") +  # Add geom_smooth for trend lines
  scale_y_continuous(name = "Temperature (C)") +
  labs(color = "Variables") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, family = "serif"),
        axis.text = element_text(family = "serif"),  # Change font family for axis labels
        axis.title = element_text(family = "serif"),  # Change font family for axis titles
        legend.text = element_text(family = "serif"),  # Change font family for legend text
        legend.title = element_text(family = "serif"))  # Change font family for legend title













# ARIK Chlorophyll with trendline
monthly_data %>% 
  filter(siteID == "ARIK",
         Chlorophyll_monthly <= 5000,
         Chlorophyll_monthly >= -200) %>% 
  ggplot(aes(x = Date, y = Chlorophyll_monthly)) +
  geom_line() +
  geom_smooth(method = "lm", color = "blue") +
  labs(x = "Date", y = "Chlorophyll") +
  ggtitle("Chlorophyll over time (ARIK)") 





# ARIK turbidity with trendline
monthly_data %>% 
  filter(siteID == "ARIK",
         !is.na(Turb_monthly)) %>%
  ggplot(aes(x = Date, y = Turb_monthly)) +
  geom_line(linewidth = 1.1, color = "black")+
  labs(x = 'Date',
       y = 'Turbidity')+
  geom_smooth(method = 'lm',
              formula = 'y ~ x',
              color = "purple") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, family = "serif"),
        axis.text = element_text(family = "serif"),  # Change font family for axis labels
        axis.title = element_text(family = "serif"),  # Change font family for axis titles
        legend.text = element_text(family = "serif"),  # Change font family for legend text
        legend.title = element_text(family = "serif"))  # Change font family for legend title






# ARIK fDOM with trendline
monthly_data %>% 
  filter(siteID == "ARIK",
         !is.na(fDOM_monthly)) %>%
  ggplot(aes(x = Date, y = fDOM_monthly)) +
  geom_line(linewidth = 1.1, color = "black")+
  labs(x = 'Date',
       y = 'fDOM')+
  geom_smooth(method = 'lm',
              formula = 'y ~ x',
              color = "cyan") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, family = "serif"),
        axis.text = element_text(family = "serif"),  # Change font family for axis labels
        axis.title = element_text(family = "serif"),  # Change font family for axis titles
        legend.text = element_text(family = "serif"),  # Change font family for legend text
        legend.title = element_text(family = "serif"))  # Change font family for legend title

















# ARIK relationship between nitrate and discharge plotly


monthly_data %>% 
  filter(siteID == "ARIK",
         !is.na(nitMonthly_mcmolpl)) %>%  
  plot_ly(x = ~Date, 
          y = ~nitMonthly_mcmolpl,
          type = 'scatter',
          mode = 'lines',
          name = "Nitrate (micromol/L)",
          line = list(color = "black")) %>% 
  add_trace(y = ~disMonthly_lps, 
            type = 'scatter',
            mode = 'lines',
            name = "Discharge (L/s)", 
            yaxis = "y2",
            line = list(color = 'darkorange', dash = 'dash')) %>%  # Set dash attribute to 'dash'
  layout(yaxis = list(title = "Nitrate", side = "left"),
         yaxis2 = list(title = "Discharge", side = "right", overlaying = "y"))







# fig 7: WLOU pH with trendline
monthly_data %>% 
  filter(siteID == "WLOU",
         !is.na(pH_monthly)) %>%
  ggplot(aes(x = Date, y = pH_monthly)) +
  geom_line(linewidth = .9, color = "black")+
  labs(x = 'Date',
       y = 'pH')+
  geom_smooth(method = 'lm',
              formula = 'y ~ x',
              color = "plum") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, family = "serif"),
        axis.text = element_text(family = "serif"),  # Change font family for axis labels
        axis.title = element_text(family = "serif"),  # Change font family for axis titles
        legend.text = element_text(family = "serif"),  # Change font family for legend text
        legend.title = element_text(family = "serif"))  # Change font family for legend title








# WLOU turbidity with trendline
monthly_data %>% 
  filter(siteID == "WLOU",
         !is.na(Turb_monthly)) %>%
  ggplot(aes(x = Date, y = Turb_monthly)) +
  geom_line(linewidth = .9, color = "black")+
  labs(x = 'Date',
       y = 'Turbidity')+
  geom_smooth(method = 'lm',
              formula = 'y ~ x',
              color = "purple") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, family = "serif"),
        axis.text = element_text(family = "serif"),  # Change font family for axis labels
        axis.title = element_text(family = "serif"),  # Change font family for axis titles
        legend.text = element_text(family = "serif"),  # Change font family for legend text
        legend.title = element_text(family = "serif"))  # Change font family for legend title










#WLOU relationship between nitrate and temperature

monthly_data %>% 
  filter(siteID == "WLOU",
         !is.na(nitMonthly_mcmolpl)) %>%  
  plot_ly(x = ~Date, 
          y = ~nitMonthly_mcmolpl,
          type = 'scatter',
          mode = 'lines',
          name = "Nitrate",
          line = list(color = "black")) %>% 
  add_trace(y = ~tempMonthly_C, 
            type = 'scatter',
            mode = 'lines',
            name = "Temperature", 
            yaxis = "y2",
            line = list(color = 'purple', dash = 'dash')) %>%  # Set dash attribute to 'dash'
  layout(yaxis = list(title = "Nitrate (micromol/L)", side = "left"),
         yaxis2 = list(title = "Temperature (C)", side = "right", overlaying = "y"))





#Fig 10: WLOU relationship between nitrate and discharge
monthly_data %>% 
  filter(siteID == "WLOU",
         !is.na(nitMonthly_mcmolpl)) %>%  
  plot_ly(x = ~Date, 
          y = ~nitMonthly_mcmolpl,
          type = 'scatter',
          mode = 'lines',
          name = "Nitrate",
          line = list(color = "black")) %>% 
  add_trace(y = ~disMonthly_lps, 
            type = 'scatter',
            mode = 'lines',
            name = "Discharge", 
            yaxis = "y2",
            line = list(color = 'darkorange', dash = 'dash')) %>%  # Set dash attribute to 'dash'
  layout(yaxis = list(title = "Nitrate (micromol/L)", side = "left"),
         yaxis2 = list(title = "Discharge (L/s)", side = "right", overlaying = "y"))




#ARIK fDOM
monthly_data %>% 
  filter(siteID == "ARIK", !is.na(fDOM_monthly)) %>%
  ggplot( aes(x = Date, y = fDOM_monthly)) +
  geom_line(linewidth = 1)+
  labs(x = '',
       y = 'fDOM',
       title = 'Monthly fDOM (ARIK)')+
  geom_smooth(method = 'lm',
              formula = 'y ~ x',
              color= "darkgreen") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))



#Macroinvertebrate data

# Discharge
Final_DF %>% 
  ggplot(aes(x = Date, y = disDaily_lps, color = siteID))+
  geom_line()+
  facet_wrap(vars(siteID))

# Nitrate
Final_DF %>% 
  ggplot(aes(x = Date, y = nitDaily_mcmolpl, color = siteID))+
  geom_point()+
  facet_wrap(vars(siteID), dir = "v")

# Precipitation
Final_DF %>% 
  ggplot(aes(x = Date, y = precipDaily_mm, color = siteID))+
  geom_point()+
  facet_wrap(vars(siteID), dir = "v")

# COND
Final_DF %>% 
  ggplot(aes(x = Date, y = Cond_daily, color = siteID))+
  geom_point()+
  facet_wrap(vars(siteID), dir = "v")

# DO
Final_DF %>% 
  ggplot(aes(x = Date, y = DO_daily, color = siteID))+
  geom_point()+
  facet_wrap(vars(siteID), dir = "v")

# pH
Final_DF %>% 
  ggplot(aes(x = Date, y = pH_daily, color = siteID))+
  geom_point()+
  facet_wrap(vars(siteID), dir = "v")

# Chlorophyll
Final_DF %>% 
  ggplot(aes(x = Date, y = Cholophyll_daily, color = siteID))+
  geom_point()+
  facet_wrap(vars(siteID), dir = "v")

# Turbidity
Final_DF %>% 
  ggplot(aes(x = Date, y = Turb_daily, color = siteID))+
  geom_point()+
  facet_wrap(vars(siteID), dir = "v")

# Temperature
Final_DF %>% 
  ggplot(aes(x = Date, y = tempDaily_C, color = siteID))+
  geom_point()+
  facet_wrap(vars(siteID), dir = "v")






