library(dplyr)
library(tidyr)

coal<- read.csv("coal_consumption_total.csv")

gdp_per_cap <- 
  read.csv(
    "income_per_person_gdppercapita_ppp_inflation_adjusted.csv", 
    header = TRUE, 
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
pop <-
  read.csv(
    "population_total.csv",
    header = TRUE,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

gdp_per_cap_long <- 
  gdp_per_cap %>% 
  pivot_longer(setdiff(colnames(gdp_per_cap), "country"), names_to = "Year", values_to = "GDP_per_cap")

pop_long <-
  pop %>%
  pivot_longer(setdiff(colnames(pop), "country"), names_to = "Year", values_to = "Population")

coal_long <-
  coal %>%
  pivot_longer(setdiff(colnames(coal), "country"), names_to = "Year", values_to = "Coal_consumption")

#delete x before year
coal_long$Year<-substr(coal_long$Year,2,5)

#combine all data
df<-inner_join(gdp_per_cap_long, pop_long, by = c("country", "Year")) %>%
  inner_join(coal_long, by = c("country", "Year")) %>%
  drop_na() 

#add continent
library(countrycode)
df$country<-as.factor(df$country)
df$Year<-as.numeric(df$Year)
names(df)[names(df)=="Year"]="year"
df$continent <-countrycode(sourcevar = df$country, origin = "country.name",destination = "continent")
df$continent<-as.factor(df$continent)

#shiny app
library(dplyr)
library(ggplot2)
library(shiny)
library(ggrepel)
library(plotly)
ui <- fluidPage(
  titlePanel("Coal consumption and GDP"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Interactive plotting of data using R shiny"),
      
      checkboxGroupInput("continent", 
                         "Choose a continent", 
                         choices = levels(df$continent),
                         selected = levels(df$continent)),
      
      
      sliderInput("year", "Year",
                  min = min(df$year), max = max(df$year),
                  value = min(df$year), step = 5,
                  sep = "",
                  animate =
                    animationOptions(interval = 500)),
    ),
    
    mainPanel(plotOutput("df_plot"))
  )
)

server <- function(input, output) {
  
  output$df_plot <- renderPlot({ 
    
    df %>%
      filter(year == input$year) %>%
      filter(continent%in%input$continent)%>%
      ggplot() +
      #scale_x_log10(limits = range(df$GDP_per_cap))+
      ylim(range(df$Coal_consumption))+
      #scale_y_log10(limits= range(df$Coal_consumption))+
      geom_point(aes(x =GDP_per_cap, y = Coal_consumption, color=continent, size= Population))
    
  })
  
}

shinyApp(ui, server)