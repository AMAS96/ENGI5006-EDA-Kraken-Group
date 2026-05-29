# This code is to the Project

#Author: Rafael Silva
#Editor : Alejandra and Rafael
#Last Modified: 27/05/2026

## Load Packages ----

pacman::p_load(tidyverse, tidytuesdayR)
library(readxl)
library(readr)

# Rainfall ----

## Load Rainfall data
Rainfall_Edinburgh <- read_csv("~/Rafael Silva/MESTRADO/Engineering Data Analytics/Group Data/Rainfall - Edinburgh.csv")%>%
  mutate(Station = "Edinburgh")


Rainfall_Aerodrome_SA <- read_csv("~/Rafael Silva/MESTRADO/Engineering Data Analytics/Group Data/Rainfall - Aerodrome SA.csv")%>%
  mutate(Station = "Woomera")

Rainfall_Adelaide_Airport <- read_csv("~/Rafael Silva/MESTRADO/Engineering Data Analytics/Group Data/Rainfall - Adelaide Airport.csv")%>%
  mutate(Station = "Adelaide Airport")

## Joining the variables together
### Renaming Data and creating filter

Kraken_Rainfall_Annual <- bind_rows(Rainfall_Adelaide_Airport,Rainfall_Edinburgh,Rainfall_Aerodrome_SA)
Kraken_Rainfall_Annual <- Kraken_Rainfall_Annual%>%
  rename("Station number" =`Bureau of Meteorology station number`)%>%
  rename("Rainfall (mm)" = `Rainfall amount (millimetres)`)%>%
  select("Station number","Station","Year","Month","Day","Rainfall (mm)")

Kraken_Rainfall_Annual <- Kraken_Rainfall_Annual%>%
  filter(Year >= 1990 & Year <= 2025)%>%
  group_by(Year, Station) %>%
  summarise("Rainfall (mm)" = sum(`Rainfall (mm)`, na.rm = TRUE),
            .groups = "drop_last")

# Rainfall Summary

stats_summary1 <- Kraken_Rainfall_Annual %>%
  group_by(`Station`) %>%
  summarise(
    n = n(),
    Minimum = min(`Rainfall (mm)`, na.rm = TRUE),
    Median = median(`Rainfall (mm)`, na.rm = TRUE),
    Mean = mean(`Rainfall (mm)`, na.rm = TRUE),
    Maximum = max(`Rainfall (mm)`, na.rm = TRUE),
    Q1 = quantile(`Rainfall (mm)`, 0.25, na.rm = TRUE),
    Q3 = quantile(`Rainfall (mm)`, 0.75, na.rm = TRUE),
    iqr = IQR(`Rainfall (mm)`, na.rm = TRUE))

print(stats_summary1)

# Maximum Precipitation and the year

max_rainfall_years <- Kraken_Rainfall_Annual %>%
  group_by(Station) %>%
  filter(`Rainfall (mm)` == max(`Rainfall (mm)`, na.rm = TRUE)) %>%
  select(Station, Year, `Rainfall (mm)`)

print(max_rainfall_years)


# Temperature ----

# Loading Temperature data
Temperature_Edinburgh <- read_csv("~/Rafael Silva/MESTRADO/Engineering Data Analytics/Group Data/Temperature - Edinburgh.csv")%>%
  mutate(Station = "Edinburgh")

Temperature_Aerodrome_SA <- read_csv("~/Rafael Silva/MESTRADO/Engineering Data Analytics/Group Data/Temperature - Aerodrome SA.csv")%>%
  mutate(Station = "Woomera")

Temperature_Adelaide_Airport <- read_csv("~/Rafael Silva/MESTRADO/Engineering Data Analytics/Group Data/Temperature - Adelaide Airport.csv")%>%
  mutate(Station = "Adelaide Airport")

## Joining the variables together
### Renaming Data and creating filter 

Kraken_Temperature_Annual <- bind_rows(Temperature_Adelaide_Airport,Temperature_Aerodrome_SA,Temperature_Edinburgh)%>%
  rename("Temperature" = `Maximum temperature (Degree C)`,
         "Station number" = `Bureau of Meteorology station number`)%>%
  select("Station number","Station","Year","Month","Day","Temperature")

Kraken_Temperature_Annual <- Kraken_Temperature_Annual %>%
  filter(Year >= 1990 & Year <= 2025)%>%
  group_by(Year, Station) %>%
  summarise( "Mean Max Temperature (C)" = mean(`Temperature`, na.rm = TRUE))

# 4. Statistical Summary of the period (1884 - 2025)
stats_summary2 <- Kraken_Temperature_Annual %>%
  group_by(`Station`) %>%
  summarise(
    n = n() ,
    Minimum = min(`Mean Max Temperature (C)`, na.rm = TRUE),
    Median = median(`Mean Max Temperature (C)`, na.rm = TRUE),
    Mean = mean(`Mean Max Temperature (C)`, na.rm = TRUE),
    Maximum = max(`Mean Max Temperature (C)`, na.rm = TRUE),
    Q1 = quantile(`Mean Max Temperature (C)`, 0.25, na.rm = TRUE),
    Q3 = quantile(`Mean Max Temperature (C)`, 0.75, na.rm = TRUE),
    iqr = IQR(`Mean Max Temperature (C)`, na.rm = TRUE))

print(stats_summary2)

# Maximum Temperature and the year

max_temperature_years <- Kraken_Temperature_Annual %>%
  group_by(Station) %>%
  filter(`Mean Max Temperature (C)` == min(`Mean Max Temperature (C)`, na.rm = TRUE)) %>%
  select(Station, Year, `Mean Max Temperature (C)`)

print(max_temperature_years)
# Generating graphs - Time range of the graph is from 1990 to 2025 ----

## QQ Plot (To find normality assumptions - one way ANOVA)
# RAINFALL
Kraken_Rainfall_Annual %>%
  filter(`Rainfall (mm)` >0) %>%
  ggplot(aes(sample = `Rainfall (mm)`)) +
  stat_qq(color = "black")+
  stat_qq_line(color = "red")+
  facet_wrap(~Station)+ #TO Merge all in one plot
  labs(x = "Theoretical", y = "Sample", title = "Normal QQ Plot: Annual Rainfall")+
  theme_bw()

# TEMPERATURE
Kraken_Temperature_Annual %>%
  ggplot(aes(sample = `Mean Max Temperature (C)`)) +
  stat_qq(color = "black")+
  stat_qq_line(color = "red")+
  stat_qq_line(distribution = stats::qnorm, color = "red") + 
  stat_qq(distribution = stats::qnorm)+
  facet_wrap(~Station)+ #TO Merge all in one plot
  labs(title = "Normal QQ Plot: Mean Max Annual Temperature",
       x = "Theoretical", y= "Sample")+
  theme_bw()

## Boxplot 
# RAINFALL
ggplot(Kraken_Rainfall_Annual, aes(x = Station, y = `Rainfall (mm)`, fill = Station)) +  
  geom_boxplot(color = "black") +
  scale_fill_manual(values = c("Adelaide Airport" = "#FF8204", 
                               "Woomera" = "#BF5CCA", 
                               "Edinburgh" = "#0F7175")) +
  labs(title = "Annual Rainfall Distribution",
       x = "Station",
       y = "Annual Rainfall (mm)", 
       fill = "Station") + 
  theme_bw()

# TEMPERATURE
ggplot(Kraken_Temperature_Annual, aes(x = Station, y = `Mean Max Temperature (C)`, fill = Station)) +  
  geom_boxplot(color = "black") +
  scale_fill_manual(values = c("Adelaide Airport" = "#FF8204", 
                               "Woomera" = "#BF5CCA", 
                               "Edinburgh" = "#0F7175")) +
  labs(title = "Annual Temperature",
       x = "Station",
       y = "Annual Temperature (C)", 
       fill = "Station") + 
  theme_bw()

## Scatterplot
# RAINFALL
ggplot(Kraken_Rainfall_Annual, aes(x = `Year`, y = `Rainfall (mm)`, color = `Station`))+
  geom_point(size = 1.5) +
  scale_color_manual(values = c("Adelaide Airport" = "#FF8204", 
                                "Woomera" = "#BF5CCA", 
                                "Edinburgh" = "#0F7175")) +
  labs(title = "Annual Rainfall",
       x = "Year", y = "Precipitation") +
  facet_wrap(~Station, nrow = 1)+
  theme_bw()

Kraken_Rainfall_Annual%>%
  group_by(Station) %>%
  summarise(correlation = cor(Year, `Rainfall (mm)`, use = "complete.obs"))

# TEMPERATURE
ggplot(Kraken_Temperature_Annual, aes(x = `Year`, y = `Mean Max Temperature (C)`, color = `Station`))+
  geom_point(size = 1.5) +
  scale_color_manual(values = c("Adelaide Airport" = "#FF8204", 
                                "Woomera" = "#BF5CCA", 
                                "Edinburgh" = "#0F7175")) +
  labs(title = "Annual Temperature",
       x = "Year", y = "Mean Max Temperature (C)") +
  facet_wrap(~Station)+
  theme_bw()

Kraken_Temperature_Annual %>%
  group_by(Station) %>%
  summarize(correlation = cor(Year, `Mean Max Temperature (C)`, use = "complete.obs"))

## ANOVA methods
#RAINFALL
Rainfall_annual <- Kraken_Rainfall_Annual%>%
  group_by(`Station`)%>%
  summarise(
    n = n(), 
    mean = mean(`Rainfall (mm)`),
    sd = sd(`Rainfall (mm)`)
  )

print(Rainfall_annual)

# Run the ANOVA model:

rain_model <- aov(`Rainfall (mm)` ~ Station, data = Kraken_Rainfall_Annual)

# Print a summary of the ANOVA results: F-Statistic

summary(rain_model)

#Run TukeyHSD model

TukeyHSD(rain_model)

rrainfall <- max(Rainfall_annual$sd)/min(Rainfall_annual$sd)

print(rrainfall)


#TEMPERATURE
Temperature_annual <- Kraken_Temperature_Annual%>%
  group_by(`Station`)%>%
  summarise(
  n = n(),
  mean = mean(`Mean Max Temperature (C)`),
  sd = sd(`Mean Max Temperature (C)`)
)

print(Temperature_annual)

# Run the ANOVA model:

temp_model <- aov(`Mean Max Temperature (C)` ~ Station, data = Kraken_Temperature_Annual)

# Print a summary of the ANOVA results:

summary(temp_model)

# Run a Tukey HSD test:

TukeyHSD(temp_model)

rtemp <- max(Temperature_annual$sd)/min(Temperature_annual$sd)

print(rtemp) 

cor(Kraken_Temperature_Annual$`Mean Max Temperature (C)`,Kraken_Rainfall_Annual$`Rainfall (mm)`, use = "complete.obs")

Kraken_Combined <- inner_join(Kraken_Rainfall_Annual, Kraken_Temperature_Annual, by = c("Year", "Station"))
ggplot(Kraken_Combined, aes(x = `Mean Max Temperature (C)`, y = `Rainfall (mm)`))+
  geom_point()+
  annotate("text", x = 26.5, y= 650, label = "r = -0.848",
           size = 15, fontface = "italic")

  