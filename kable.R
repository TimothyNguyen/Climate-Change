rm(list=ls())
# install.packages("choroplethrMaps")
#install.packages("magick")
#install.packages("webshot")
# webshot::install_phantomjs()
library(shiny)
library(ggplot2) 
library(plotly)
library(tidyr)
library(dplyr)
library(countrycode)
library(choroplethr)
library(readr)
# install.packages("plyr")
library(plyr)

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

flood_death<-read.csv("./data/flood_deaths_annual_number.csv")
drought_death<-read.csv("./data/drought_deaths_annual_number.csv")
extreme_temp_death<-read.csv("./data/extreme_temperature_deaths_annual_number.csv")


total_flood_death<-colSums(flood_death[-1],na.rm=TRUE)
total_drought_death<-colSums(drought_death[-1],na.rm=TRUE)
total_extreme_temp_death<-colSums(extreme_temp_death[-1],na.rm=TRUE)


data_total_flood_death <- ldply (total_flood_death, data.frame)
data_total_drought_death <- ldply (total_drought_death, data.frame)
data_total_extreme_temp_death <- ldply (total_extreme_temp_death, data.frame)
names(data_total_flood_death)<-c("Year","total_flood_death")
names(data_total_drought_death)<-c("Year","total_drought_death")
names(data_total_extreme_temp_death)<-c("Year","total_extreme_temp_death")
data_total_flood_death$Year<-substr(data_total_flood_death$Year,2,5)
data_total_drought_death$Year<-substr(data_total_drought_death$Year,2,5)
data_total_extreme_temp_death$Year<-substr(data_total_extreme_temp_death$Year,2,5)


data_total_drought_death<-data_total_drought_death[-1,]
data_total_flood_death<-data_total_flood_death[-1,]


global_warming_consequences_death <- 
  left_join(data_total_drought_death, data_total_flood_death, by = "Year") %>%
  left_join(data_total_extreme_temp_death, by = "Year")

'
subplot(
  plot_ly(global_warming_consequences_death, x=~total_drought_death, y=~Year, name="drought"),
  plot_ly(global_warming_consequences_death, x=~total_flood_death, y=~Year, name="flood"),
  plot_ly(global_warming_consequences_death, x=~total_extreme_temp_death, y=~Year, name="extreme_temperature")
)
'

flood_death<-flood_death[-2]
drought_death<-drought_death[-2]


#number_countries_flood<-data.frame(Year=c(1971:2008),number_of_countries_flood=numeric(38))
#number_countries_drought<-data.frame(Year=c(1971:2008),number_of_countries_flood=numeric(38))
#number_countries_extreme_temp<-data.frame(Year=c(1971:2008),number_of_countries_flood=numeric(38))


flood_death<-flood_death[-1]
drought_death<-drought_death[-1]
extreme_temp_death<-extreme_temp_death[-1]


flood_death[flood_death!=0]<-1
drought_death[drought_death!=0]<-1
extreme_temp_death[extreme_temp_death!=0]<-1


times_flood_death<-colSums(flood_death,na.rm=TRUE)
times_drought_death<-colSums(drought_death,na.rm=TRUE)
times_extreme_temp_death<-colSums(extreme_temp_death,na.rm=TRUE)


times_flood_death <- ldply (times_flood_death, data.frame)
times_drought_death <- ldply (times_drought_death, data.frame)
times_extreme_temp_death <- ldply (times_extreme_temp_death, data.frame)
names(times_flood_death)<-c("Year","times_flood_death")
names(times_drought_death)<-c("Year","times_drought_death")
names(times_extreme_temp_death)<-c("Year","times_extreme_temp_death")


times_flood_death$Year<-substr(times_flood_death$Year,2,5)
times_drought_death$Year<-substr(times_drought_death$Year,2,5)
times_extreme_temp_death$Year<-substr(times_extreme_temp_death$Year,2,5)


number_of_countries_disaster <- 
  left_join(times_flood_death, times_drought_death, by = "Year") %>%
  left_join(times_extreme_temp_death, by = "Year")

names(number_of_countries_disaster)[1] <- "year"

annual_co2_emissions <- read.csv(
  "./data/co-emissions-by-sector.csv", 
  header = TRUE, 
  stringsAsFactors = FALSE,
  check.names = FALSE
)

world <- annual_co2_emissions%>%
  filter(annual_co2_emissions$Entity == "World")%>%
  subset(select = -c(Entity, Code))%>%
  pivot_longer(!Year, names_to = "Categories", values_to = "amount")

library(kableExtra)

names(world)[1] <- "year"
# world

table_merge <- merge(global_temp_year, global_co2, by="year", all=TRUE) %>%
               merge(number_of_countries_disaster, by="year", all=TRUE)
  # merge(world, by="year", all=TRUE)
       
 
head(table_merge, 20) %>%
  kbl() %>%
  kable_styling() %>% 
  save_kable("combined_tables.png")
