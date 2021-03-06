rm(list=ls())
library(shiny)
#runExample("06_tabsets")

library(ggplot2)
library(dplyr)
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

coal<- read.csv("./data/coal_consumption_total.csv")

coal_long <-
  coal %>%
  pivot_longer(setdiff(colnames(coal), "country"), names_to = "Year", values_to = "Coal_consumption")

gdp_per_cap_long <- gdp_per_cap%>%
  pivot_longer(c('1960':'2015'), names_to = "year", values_to = "gdpPercap") %>%
  select(country, year, gdpPercap)

life_exp_long <- life_exp%>%
  pivot_longer(c('1960':'2015'), names_to = "year", values_to = "lifeExp") %>%
  select(country, year, lifeExp)


pop_long <- pop%>%
  pivot_longer(c('1960':'2015'), names_to = "year", values_to = "pop") %>%
  select(country, year, pop)


energy_use_pp_long <- energy_use_pp%>%
  pivot_longer(c('1960':'2015'), names_to = "year", values_to = "EnergyUse_pp") %>%
  select(country, year, EnergyUse_pp)


co2_em_pp_long <- co2_em_pp%>%
  pivot_longer(c('1960':'2015'), names_to = "year", values_to = "CO2_Emissions_pp") %>%
  select(country, year, CO2_Emissions_pp)

names(coal_long)[names(coal_long)=="Year"]="year"
df <- 
  left_join(energy_use_pp_long, co2_em_pp_long, by = c("country", "year")) %>%
  left_join(gdp_per_cap_long, by = c("country", "year")) %>%
  left_join(pop_long, by = c("country", "year"))%>%
  left_join(coal_long, by = c("country", "year"))%>%
  drop_na()%>%
  mutate(continent = as.factor(countrycode(sourcevar = country, origin = "country.name", destination = "continent")))%>%
  mutate(year = as.integer(year)) %>%
  mutate(country = as.factor(country)) %>%
  mutate(gdpPercap = as.double(gdpPercap))

df

# Define UI for random distribution app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Energy and GDP"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select the random distribution type ----
      checkboxGroupInput("continent", 
                         "Choose a continent", 
                         choices = levels(df$continent),
                         selected = levels(df$continent)),
      
      # br() element to introduce extra vertical spacing ----
      br(),
      
      # Input: Slider for the number of observations to generate ----
      sliderInput("year", "Year",
                  min = min(df$year), max = max(df$year),
                  value = min(df$year), step = 5,
                  sep = "",
                  animate =
                    animationOptions(interval = 500)),
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("CoalPlot", plotOutput("coal_plot")),
                  tabPanel("Co2Plot", plotOutput("co2_plot")),
                  tabPanel("EnergyPlot", plotOutput("energy_plot"))
      )
      
    )
  )
)

# Define server logic for random distribution app ----
server <- function(input, output) {
  
  # Reactive expression to generate the requested distribution ----
  # This is called whenever the inputs change. The output functions
  # defined below then use the value computed from this expression
  # d <- reactive({
  #   continent <- switch(input$continent,
  #                  norm = rnorm,
  #                  unif = runif,
  #                  lnorm = rlnorm,
  #                  exp = rexp,
  #                  rnorm)
  # 
  #   dist(input$n)
  # })
  
  # Generate a plot of the data ----
  # Also uses the inputs to build the plot label. Note that the
  # dependencies on the inputs and the data reactive expression are
  # both tracked, and all expressions are called in the sequence
  # implied by the dependency graph.
  output$coal_plot <- renderPlot({
    df %>%
      filter(year == input$year) %>%
      filter(continent%in%input$continent)%>%
      ggplot() +
      #scale_x_log10(limits = range(df$GDP_per_cap))+
      #ylim(range(df$Coal_consumption))+
      #scale_y_log10(limits= range(df$Coal_consumption))+
      geom_point(aes(x =gdpPercap, y = Coal_consumption, color=continent, size= pop))
    
  })
  
  # Generate a summary of the data ----
  output$energy_plot <- 
    renderPlot({
      df %>%
        filter(year == input$year)%>%
        filter(continent %in% input$continent)%>%
        # filter(
        #   between(df$EnergyUse_pp, 
        #           quantile(df$EnergyUse_pp, input$percentile[1]/100),
        #           quantile(df$EnergyUse_pp, input$percentile[2]/100))
        #   )%>%
        ggplot() +
        geom_point(mapping = aes(x = gdpPercap, y = EnergyUse_pp, color = continent))+
        theme(legend.title = element_blank())
      
    })
  
  
  # Generate an HTML table view of the data ----
  output$co2_plot <- renderPlot({
    df %>%
      filter(year == input$year)%>%
      filter(continent %in% input$continent)%>%
      # filter(
      #   between(df$EnergyUse_pp, 
      #           quantile(df$EnergyUse_pp, input$percentile[1]/100),
      #           quantile(df$EnergyUse_pp, input$percentile[2]/100))
      #   )%>%
      ggplot() +
      geom_point(mapping = aes(x = gdpPercap, y = CO2_Emissions_pp, color = continent))+
      theme(legend.title = element_blank())
    
    
  })
}



# Create Shiny app ----
shinyApp(ui, server)