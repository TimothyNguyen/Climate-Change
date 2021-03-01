rm(list=ls())
library(tidyverse)
library(plotly)
library(countrycode)
gdp_per_cap <- 
  read.csv(
    "./data/income_per_person_gdppercapita_ppp_inflation_adjusted.csv", 
    header = TRUE, 
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
life_exp <- 
  read.csv(
    "./data/life_expectancy_years.csv",
    header = TRUE,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
pop <-
  read.csv(
    "./data/population_total.csv",
    header = TRUE,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

energy_use_pp <- 
  read.csv(
    "./data/energy_use_per_person.csv",
    header = TRUE,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

co2_em_pp <-
  read.csv(
    "./data/co2_emissions_tonnes_per_person.csv",
    header = TRUE,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

elect_use_pp <-
  read.csv("./data/electricity_use_per_person.csv",
           header = TRUE,
           stringsAsFactors = FALSE,
           check.names = FALSE)

coal_use_pp <- 
  read.csv("./data/coal_consumption_per_cap.csv",
           header = TRUE,
           stringsAsFactors = FALSE,
           check.names = FALSE)

oil_use_pp <-
  read.csv("./data/oil_consumption_per_cap.csv",
           header = TRUE,
           stringsAsFactors = FALSE,
           check.names = FALSE)



gdp_per_cap_long <- gdp_per_cap%>%
  pivot_longer(c('1965':'2014'), names_to = "year", values_to = "gdpPercap") %>%
  select(country, year, gdpPercap)

life_exp_long <- life_exp%>%
  pivot_longer(c('1965':'2014'), names_to = "year", values_to = "lifeExp") %>%
  select(country, year, lifeExp)


pop_long <- pop%>%
  pivot_longer(c('1965':'2014'), names_to = "year", values_to = "pop") %>%
  select(country, year, pop)


energy_use_pp_long <- energy_use_pp%>%
  pivot_longer(c('1965':'2014'), names_to = "year", values_to = "EnergyUse_pp") %>%
  select(country, year, EnergyUse_pp)


co2_em_pp_long <- co2_em_pp%>%
  pivot_longer(c('1965':'2014'), names_to = "year", values_to = "CO2_Emissions_pp") %>%
  select(country, year, CO2_Emissions_pp)

elect_use_pp_long <- elect_use_pp%>%
  pivot_longer(c('1965':'2014'), names_to = "year", values_to = "ElectUse_pp") %>%
  select(country, year, ElectUse_pp)

coal_use_pp_long <- coal_use_pp%>%
  pivot_longer(c('1965':'2014'), names_to = "year", values_to = "CoalUse_pp") %>%
  select(country, year, CoalUse_pp)

oil_use_pp_long <- oil_use_pp%>%
  pivot_longer(c('1965':'2014'), names_to = "year", values_to = "OilUse_pp") %>%
  select(country, year, OilUse_pp)


df <- 
  left_join(energy_use_pp_long, co2_em_pp_long, by = c("country", "year")) %>%
  left_join(elect_use_pp_long, by = c("country", "year")) %>%
  left_join( coal_use_pp_long, by = c("country", "year"))%>%
  left_join(oil_use_pp_long, by = c("country", "year")) %>%
  left_join(gdp_per_cap_long, by = c("country", "year")) %>%
  left_join(pop_long, by = c("country", "year"))%>%
  drop_na()%>%
  mutate(continent = as.factor(countrycode(sourcevar = country, origin = "country.name", destination = "continent")))%>%
  mutate(year = as.integer(year)) %>%
  mutate(country = as.factor(country)) %>%
  mutate(gdpPercap = as.double(gdpPercap))

library(shiny)
library(ggplot2)
library(dplyr)
# Shiny app
ui <- 
  fluidPage(
    # App title
    titlePanel("Energy vs. CO2"),
    
    # sidebar layout with input and output definitions
    sidebarLayout(
      # Sidebar panel for inputs
      sidebarPanel(
        helpText("Interactive plotting of gapminder data using R shiny"),
        checkboxGroupInput("continent",
                           "Choose a continent",
                           choices = levels(df$continent),
                           selected = levels(df$continent)),
        sliderInput("range",
                    label = "Percentiles of CO2 Emissions /person",
                    min = 0,
                    max = 100,
                    value = c(0, 100)),
        selectInput("diffEnergy", 
                    "Energy Type", 
                    choices = list("Coal" = 1, "Electricity" = 2, "Oil" = 3),
                    selected = 1),
        sliderInput("year",
                    "Year",
                    min = min(df$year),
                    max = max(df$year),
                    value = min(df$year),
                    sep = "",
                    step = 5,
                    animate = animationOptions(interval = 500))
      ),
      
      # Main panel for displaying outputs
      mainPanel(tabsetPanel(type="tab",
                            tabPanel("Total Energy Plot", plotOutput("energy_plot")),
                            tabPanel("Different Energy Plot", plotOutput("diffEnergy_plot")))
                
      )
    )
  )


server <- function(input, output) {
  output$energy_plot <- 
    renderPlot({
      df %>%
        filter(year == input$year)%>%
        filter(continent %in% input$continent)%>%
        filter(between (
          CO2_Emissions_pp,
          quantile(CO2_Emissions_pp, prob = input$range[1] / 100),
          quantile(CO2_Emissions_pp, prob = input$range[2] / 100)
        )
        )%>%
        ggplot(aes(y = EnergyUse_pp, x = CO2_Emissions_pp, color = continent)) +
        scale_x_sqrt(limits = range(df$CO2_Emissions_pp))+
        geom_point(aes(size = gdpPercap))+
        scale_y_log10(limits=range(df$EnergyUse_pp))+
        theme(legend.title = element_blank()) 
      
    })
  
  
  output$diffEnergy_plot <-
    renderPlot({
      if(input$diffEnergy == 1){
        df %>%
          filter(year == input$year)%>%
          filter(continent %in% input$continent)%>%
          filter(between (
            CO2_Emissions_pp,
            quantile(CO2_Emissions_pp, prob = input$range[1] / 100),
            quantile(CO2_Emissions_pp, prob = input$range[2] / 100)
          )
          )%>%
          ggplot(aes(y = CoalUse_pp, x = EnergyUse_pp, color = continent)) +
          scale_x_sqrt(limits = range(df$EnergyUse_pp))+
          geom_point(aes(size = gdpPercap))+
          scale_y_sqrt(limits=range(df$CoalUse_pp))+
          theme(legend.title = element_blank())  
      }
      
      else if(input$diffEnergy == 2){
        df %>%
          filter(year == input$year)%>%
          filter(continent %in% input$continent)%>%
          filter(between (
            CO2_Emissions_pp,
            quantile(CO2_Emissions_pp, prob = input$range[1] / 100),
            quantile(CO2_Emissions_pp, prob = input$range[2] / 100)
          )
          )%>%
          ggplot(aes(y = ElectUse_pp, x = EnergyUse_pp, color = continent)) +
          scale_x_sqrt(limits = range(df$EnergyUse_pp))+
          geom_point(aes(size = gdpPercap))+
          scale_y_sqrt(limits=range(df$ElectUse_pp))+
          theme(legend.title = element_blank()) 
      }
      
      else if(input$diffEnergy == 3){
        df %>%
          filter(year == input$year)%>%
          filter(continent %in% input$continent)%>%
          filter(between (
            CO2_Emissions_pp,
            quantile(CO2_Emissions_pp, prob = input$range[1] / 100),
            quantile(CO2_Emissions_pp, prob = input$range[2] / 100)
          )
          )%>%
          ggplot(aes(y = OilUse_pp, x = EnergyUse_pp, color = continent)) +
          scale_x_sqrt(limits = range(df$EnergyUse_pp))+
          geom_point(aes(size = gdpPercap))+
          scale_y_sqrt(limits=range(df$OilUse_pp))+
          theme(legend.title = element_blank())
      }
      
    })
  
  
}

shinyApp(ui, server)
