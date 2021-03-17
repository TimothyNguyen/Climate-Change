library(dplyr)
library(tidyverse)
library(plyr)
library(plotly)


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


subplot(
  plot_ly(global_warming_consequences_death, x=~total_drought_death, y=~Year, name="drought"),
  plot_ly(global_warming_consequences_death, x=~total_flood_death, y=~Year, name="flood"),
  plot_ly(global_warming_consequences_death, x=~total_extreme_temp_death, y=~Year, name="extreme_temperature")
)


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

number_of_countries_disaster

subplot(
  plot_ly(number_of_countries_disaster, x=~times_drought_death, y=~Year, name="drought"),
  plot_ly(number_of_countries_disaster, x=~times_flood_death, y=~Year, name="flood"),
  plot_ly(number_of_countries_disaster, x=~times_extreme_temp_death, y=~Year, name="extreme_temperature")
)

