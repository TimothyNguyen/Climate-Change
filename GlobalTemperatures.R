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

global_temp <- select(global_temp, -c("LandAverageTemperatureUncertainty", "LandMaxTemperature",
                          "LandMaxTemperatureUncertainty", "LandMinTemperature", 
                         "LandMinTemperatureUncertainty", "LandAndOceanAverageTemperature",
                         "LandAndOceanAverageTemperatureUncertainty"))
#head(global_temp)
global_temp <- within(global_temp,
                    date <- ifelse(!is.na(as.Date(global_temp$dt, "%Y-%m-%d")),
                                   as.character(as.Date(global_temp$dt, "%Y-%m-%d")),
                                   as.character(as.Date(global_temp$dt, "%m/%d/%Y")))) 
#head(global_temp)
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

global_temp_year

ggplot(global_temp_year, 
       aes(x=year, y = LandAverageTemperature, group=1)) +
       geom_line() + 
       geom_point() + 
       theme(
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
       ) + 
      scale_x_discrete(breaks=seq(1850, 2015, 10))




