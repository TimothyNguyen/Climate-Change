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

gdp_per_cap <- 
  read.csv(
    "./data/income_per_person_gdppercapita_ppp_inflation_adjusted.csv", 
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
yearly_co2 <-
  read.csv(
    "./data/yearly_co2_emissions_1000_tonnes.csv",
    header = TRUE,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
land_temp <-
  read.csv(
    "./data/GlobalLandTemperaturesByCountry.csv",
    header = TRUE,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

gdp_per_cap$continent <- countrycode(sourcevar = gdp_per_cap[, "country"],
                                     origin = "country.name",
                                     destination = "continent")
pop$continent <- countrycode(sourcevar = pop[, "country"],
                             origin = "country.name",
                             destination = "continent")
land_temp$continent <- countrycode(sourcevar = land_temp[, "country"],
                                   origin = "country.name",
                                   destination = "continent")
yearly_co2$continent <- countrycode(sourcevar = yearly_co2[, "country"],
                                    origin = "country.name",
                                    destination = "continent")

land_temp
# Clean the data
land_temp <- land_temp %>% drop_na("continent")
drop <- c("AverageTemperatureUncertainty")
land_temp <- land_temp[!(names(land_temp) %in% drop)]
land_temp <- within(land_temp,
                    date <- ifelse(!is.na(as.Date(land_temp$dt, "%Y-%m-%d")),
                                   as.character(as.Date(land_temp$dt, "%Y-%m-%d")),
                                   as.character(as.Date(land_temp$dt, "%m/%d/%Y")))) 
land_temp <- land_temp[!(names(land_temp) %in% drop)]
land_temp <- na.omit(land_temp)

land_df <- land_temp %>%
  mutate(country, year = year(date)) %>%
  group_by(country, year, continent)
drop <- c("dt")
land_df <- land_df[!(names(land_df) %in% drop)]
land_df <- aggregate(land_df$AverageTemperature, 
                     by=list(year=land_df$year, 
                             country=land_df$country,
                             continent=land_df$continent), 
                     FUN=mean, na.action = na.omit)
land_df <- land_df %>%
  mutate(AverageTemperature = x * 1.8 + 32)
drop <- c("x")
land_df <- land_df[!(names(land_df) %in% drop)]
names(land_df)[4] <- "AverageTemperature"

df_co2 <- yearly_co2%>%
  pivot_longer(c('1850':'2012'), names_to = "year", 
               values_to = "co2_emissions") %>%
  select(country, year, co2_emissions)
df_co2 <- na.omit(df_co2, cols=c("co2_emissions"))
df_gdp <- gdp_per_cap%>%
  pivot_longer(c('1850':'2012'), names_to = "year", values_to = "gdpPercap") %>%
  select(country, year, gdpPercap)
df_pop <- pop%>%
  pivot_longer(c('1850':'2012'), names_to = "year", values_to = "pop") %>%
  select(country, year, pop)
df_land <- filter(land_df, year >= 1850) %>% filter(year <= 2012)
df_land <- df_land %>% mutate(year = as.character(year))

first_graph <- left_join(df_pop, df_co2) %>%
  merge(df_land)
first_graph <- na.omit(first_graph, cols=c("co2_emissions"))
first_graph$CODE <- countrycode(first_graph$country, origin = 'country.name', destination = 'genc3c')

con <- factor(c('Asia','Africa', 'Americas', 'Europe', 'Oceania'))

ui <- fluidPage(
  titlePanel("Analyze Relationship between "),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Interavtive plotting of data usng R shiny"),
      
      checkboxGroupInput("continent", 
                         "Choose a continent", 
                         choices = levels(con),
                         selected = levels(con)),
      
      sliderInput("quantiles", "Quantiles of interest",
                  min = 0,
                  max = 100,
                  value = c(0, 100),
                  sep = ""),
      
      sliderInput("year", "Year",
                  min = range(as.numeric(first_graph$year))[1],
                  max = range(as.numeric(first_graph$year))[2],
                  value = range(as.numeric(first_graph$year))[1],
                  sep = "",
                  step = 1,
                  animate = animationOptions(interval = 300)
      )
    ),
    tabsetPanel(type = "tabs",
                tabPanel("Population vs CO2", plotOutput("co2_plot")),
                tabPanel("CO2 Vs Land Temperature", plotOutput("temp_plot")))
  )
)


max_b <- max(first_graph$co2_emissions)
min_b <- min(first_graph$co2_emissions)
max_a <- max(first_graph$pop)
min_a <- min(first_graph$pop)

server <- function(input, output) {
  max_x <- max(first_graph$co2_emissions)
  min_x <- min(first_graph$co2_emissions)
  max_y <- max(first_graph$AverageTemperature)
  min_y <- min(first_graph$AverageTemperature)
  
  output$co2_plot <- renderPlot({ 
    df <- first_graph %>%
      filter(year == input$year) %>%
      filter(continent %in% input$continent)
    
    filter(df, pop <= quantile(df$pop, 
                               probs = (max(input$quantiles)/100),
                               na.rm = TRUE)) %>%
      filter(pop >= quantile(df$pop, 
                             probs = (min(input$quantiles)/100),
                             na.rm = TRUE)) %>%
      ggplot(aes(x = pop, 
                 y = co2_emissions, color=continent)) +
      geom_point(aes(frame = year, ids = country)) + 
      scale_x_log10(limits = c(min_a + 0.1, max_a)) + 
      scale_y_log10(limits = c(min_b + 0.1, max_b)) + 
      theme(legend.title = element_blank())
  })
  
  output$temp_plot <- renderPlot({ 
    df <- first_graph %>%
      filter(year == input$year) %>%
      filter(continent %in% input$continent)
    
    filter(df, co2_emissions <= quantile(df$co2_emissions, 
                                         probs = (max(input$quantiles)/100),
                                         na.rm = TRUE)) %>%
      filter(co2_emissions >= quantile(df$co2_emissions, 
                                       probs = (min(input$quantiles)/100),
                                       na.rm = TRUE)) %>%
      ggplot(aes(x = co2_emissions, 
                 y = AverageTemperature, color=continent)) +
      geom_point(aes(size = pop, frame = year, ids = country)) + 
      scale_x_log10(limits = c(min_x + 0.1, max_x)) + 
      ylim(min_y, max_y) + 
      theme(legend.title = element_blank())
  })
}
shinyApp(ui = ui, server = server)

