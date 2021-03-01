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
library(lubridate)

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
print(levels(con))
ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      helpText("Interactive plotting on Land Temperatures"),
      
      sliderInput("year", "Year (Land Temperatures)",
                  min = range(as.numeric( first_graph$year))[1],
                  max = range(as.numeric( first_graph$year))[2],
                  value = range(as.numeric( first_graph$year))[1],
                  sep = "",
                  step = 1,
                  animate = animationOptions(interval = 500)
      ),
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("AverageTemperature", plotOutput("temp_plot")),
                  tabPanel("CO2 Emissions", plotOutput("co2_plot"))))
  )
)


server <- function(input, output) {
  
  output$temp_plot <- renderPlot({ 
    df <- first_graph %>%
      filter(year == input$year) %>%
      rename(region = country, value = AverageTemperature) %>%
      mutate(region = tolower(region)) %>%
      mutate(region = recode(region,
                             "united states"    = "united states of america",
                             "congo, dem. rep." = "democratic republic of the congo",
                             "congo, rep."      = "republic of congo",
                             "korea, dem. rep." = "south korea",
                             "korea. rep."      = "north korea",
                             "tanzania"         = "united republic of tanzania",
                             "serbia"           = "republic of serbia",
                             "slovak republic"  = "slovakia",
                             "yemen, rep."      = "yemen"))
    country_choropleth(df, num_colors=5) + 
      scale_fill_brewer(palette="OrRd")
    
  })
  
  output$co2_plot <- renderPlot({ 
    df <- first_graph %>%
      filter(year == input$year) %>%
      rename(region = country, value = co2_emissions) %>%
      mutate(region = tolower(region)) %>%
      mutate(region = recode(region,
                             "united states"    = "united states of america",
                             "congo, dem. rep." = "democratic republic of the congo",
                             "congo, rep."      = "republic of congo",
                             "korea, dem. rep." = "south korea",
                             "korea. rep."      = "north korea",
                             "tanzania"         = "united republic of tanzania",
                             "serbia"           = "republic of serbia",
                             "slovak republic"  = "slovakia",
                             "yemen, rep."      = "yemen"))
    country_choropleth(df, num_colors=5) + 
      scale_fill_brewer(palette="OrRd")
    
  })
}

shinyApp(ui = ui, server = server)

