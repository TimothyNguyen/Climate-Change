rm(list=ls())
library(ggplot2) 
library(plotly)
library(tidyr)
library(dplyr)
library(countrycode)
library(readr)
library(tidyverse)
library(highcharter) 

yearly_co2 <-
  read.csv(
    "./data/yearly_co2_emissions_1000_tonnes.csv",
    header = TRUE,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
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


ggplot(global_co2, 
       aes(x=year, y = co2_emissions, group=1)) +
  geom_line() + 
  geom_point() + 
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
  ) + 
  scale_x_discrete(breaks=seq(1850, 2015, 10))

