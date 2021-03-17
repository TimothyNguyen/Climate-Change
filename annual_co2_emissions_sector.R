library(tidyverse)
library(shiny)
library(ggplot2)
library(dplyr)


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

ui <-
  fluidPage(
    # App title
    titlePanel("Annual CO_2 emissions by sector"),
    
    # sidebar layout with input and output definitions
    sidebarLayout(
      # Sidebar panel for inputs
      sidebarPanel(
        sliderInput("year",
                    "Year",
                    min = min(world$Year),
                    max = max(world$Year),
                    value = min(world$Year),
                    sep = "",
                    step = 1,
                    animate = animationOptions(interval = 250))
      ),
      
      # Main panel for displaying outputs
      mainPanel(
        plotOutput(outputId = "CO2_emissions")
      )
    ))


server <- function(input, output) {
  output$CO2_emissions <-
    renderPlot({
      world %>%
        filter(Year == input$year)%>%
        mutate(Categories = fct_reorder(Categories, amount))%>%
        ggplot(aes(x = Categories, y = amount, fill = Categories)) +
        # geom_text(aes(y = 0, label = paste(Categories, " ")), vjust = 0.2, hjust = 1, size = 3) +
        geom_text(aes(y = amount, label = amount, hjust=0), size = 3)+
        geom_bar(stat = "identity")+
        coord_flip()+
        labs(x = "", y = "")+
        ggtitle(paste("CO2 emissions by sector, World, ", input$year, "unit in (million tonnes)"))+
        theme(legend.title = element_blank())
    })
}

shinyApp(ui, server)






