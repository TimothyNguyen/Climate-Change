rm(list=ls())
# install.packages("choroplethrMaps")
library(ggplot2) 
library(shiny)
# require(shinydashboard)
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

con <- factor(c('Asia','Africa', 'Americas', 'Europe', 'Oceania'))
ui <- fluidPage(
  titlePanel("Looking at Carbon Emissions per tonnes"),
  
  sidebarLayout(
    sidebarPanel(
      
      checkboxGroupInput("continent", 
                         "Choose a continent", 
                         choices = levels(con),
                         selected = levels(con)),
      
      sliderInput("year", "Year",
                  min = range(as.numeric(df_co2$year))[1],
                  max = range(as.numeric(df_co2$year))[2],
                  value = range(as.numeric(df_co2$year))[1],
                  sep = "",
                  step = 1,
                  animate = animationOptions(interval = 300)
      )
    ),
    mainPanel(
      highchartOutput("co2_plot"),
      htmlOutput("total_co2_text")
    )
  )
)

server <- function(input, output) {
  
  output$co2_plot <- renderHighchart ({ 
    data <- df_co2 %>%
      filter(year %in% input$year) %>%
      filter(continent %in% input$continent) %>%
      hchart(
        "treemap", 
        hcaes(x = country, value = co2_emissions, color = co2_emissions)
      )
  })
  
  output$total_co2_text <- renderUI({
    data <- df_co2 %>%
      filter(year %in% input$year) %>%
      filter(continent %in% input$continent)
    sum_co2 = sum(data$co2_emissions)
    
    co2 = data[order(-data$co2_emissions),]
    Names <- character(0)
    i = 1
    while(i <= 10) {
      Names[i] <- paste(i, ":", co2$country[i], "with emissions of",
                        co2$co2_emissions[i], "</br>")
      i = i + 1
    }
    
    Names[i] = (paste("The yearly co2 emissions per 1000 tonnes with the selected continents are:", sum_co2, "</br>"))
    HTML(Names)
  })
}

shinyApp(ui, server)