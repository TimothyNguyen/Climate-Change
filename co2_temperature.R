rm(list=ls())
# install.packages("choroplethrMaps")
library(shiny)
library(ggplot2) 
library(plotly)
library(tidyr)
library(dplyr)
library(countrycode)
library(choroplethr)
library(readr)

global_temp <- 
  read.csv(
    "./data/GlobalTemperatures.csv", 
    header = TRUE, 
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

yearly_co2 <-
  read.csv(
    "./data/yearly_co2_emissions_1000_tonnes.csv",
    header = TRUE,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

global_temp <- select(global_temp, -c("LandAverageTemperatureUncertainty", "LandMaxTemperature",
                                      "LandMaxTemperatureUncertainty", "LandMinTemperature", 
                                      "LandMinTemperatureUncertainty", "LandAndOceanAverageTemperature",
                                      "LandAndOceanAverageTemperatureUncertainty"))
global_temp <- within(global_temp,
                      date <- ifelse(!is.na(as.Date(global_temp$dt, "%Y-%m-%d")),
                                     as.character(as.Date(global_temp$dt, "%Y-%m-%d")),
                                     as.character(as.Date(global_temp$dt, "%m/%d/%Y")))) 

global_temp <- na.omit(global_temp)

global_temp_year <- global_temp %>%
  mutate(year = substring(date, 1, 4)) %>%
  group_by(year, LandAverageTemperature)

drop <- c("dt")
global_temp_year <- global_temp_year[!(names(global_temp_year) %in% drop)]
global_temp_year <- aggregate(global_temp_year$LandAverageTemperature, 
                              by=list(year=global_temp_year$year), 
                              FUN=mean, na.action = na.omit)
global_temp_year <- global_temp_year %>%
  mutate(LandAverageTemperature = x * 1.8 + 32)


drop <- c("x")
global_temp_year <- global_temp_year[!(names(global_temp_year) %in% drop)]
global_temp_year <- global_temp_year %>%
  filter(year >= 1850)

yearly_co2$continent <- countrycode(sourcevar = yearly_co2[, "country"],
                                    origin = "country.name",
                                    destination = "continent")
df_co2 <- yearly_co2 %>%
  pivot_longer(c('1850':'2012'), names_to = "year", 
               values_to = "co2_emissions") %>%
  select(country, continent, year, co2_emissions)
df_co2 <- na.omit(df_co2, cols=c("co2_emissions"))

global_co2 <- aggregate(df_co2$co2_emissions, 
                        by=list(year=df_co2$year), 
                        FUN=sum)
global_co2 <- global_co2 %>% mutate(co2_emissions = x)
drop <- c("x")
global_co2 <- global_co2[!(names(global_co2) %in% drop)]


global_temp_year
global_co2


co2_temp <- merge(global_temp_year, global_co2, by="year", all=TRUE)
co2_temp


gg <- ggplot(co2_temp, 
             aes(x=co2_emissions, y = LandAverageTemperature)) +
  geom_line() + 
  geom_point(aes(ids = year)) + 
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
  ) 
ggplotly(gg)

