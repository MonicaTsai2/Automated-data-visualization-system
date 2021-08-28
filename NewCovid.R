setwd("~/Desktop/App/Data ")

# Loading library
library(shiny)
library(shinydashboard)
library(shinythemes)
library(leaflet)
library(plotly)
library(maps)
library(mapproj)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(sf) 
library(DT)
library(rgdal)
library(zoo)
library(xts)
library(dygraphs)

# Source helper functions
#source("helpers.R")
#source("plotlyGraphWidget.R")

#Loading data
map <- readOGR("cb_2018_us_county_5m/cb_2018_us_county_5m.shp")
data <- read.csv("covid.csv")
data = data %>% mutate(date = as.Date(date))
data$ratio <- data$deaths/data$cases
str(data)


# user interface
ui <- fluidPage(
  navbarPage("Covid-19 tracker in the USA", theme = shinytheme("flatly"),
             
             tabPanel("Map",
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          helpText("The map visualizes the information from covid-19 data."),
                          
                          selectInput("variableselected", 
                                      label = "Choose a variable to display:",
                                      choices = c("Cases"="cases","Deaths"="deaths", "Ratio"="ratio"),
                                      selected = "Cases"),
                          
                          sliderInput("dateselected", label= "Select Date:",
                                      as.Date("2020-01-21"),
                                      as.Date("2020-12-05 "),
                                      value = as.Date("2020-01-21"),
                                      step = 1),
                     
                          submitButton("Submit"),
                          
                          helpText("Note: The dataset is scraped from Kaggle(https://www.kaggle.com/sudalairajkumar/covid19-in-usa)"),
                        ),
                      
                        mainPanel(
                          leafletOutput("covidmap"),
                          p(),
                          DTOutput(outputId = "table")
                        ))),
             
             tabPanel("Region(Daily)", 
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          helpText("Built plots to present Daily New Cases/New Deaths in selected country."),
                          selectizeInput("counties",
                                         label = "Country Names(s) of Interest",
                                         choices = unique(data$county),
                                         multiple = T,
                                         selected = "Snohomish"
                          ),
                          
                          submitButton("Submit")
                        ),
                        
                        mainPanel(
                          plotlyOutput(outputId = "p"),
                          plotlyOutput(outputId = "d")
                        )))
  )
)

# server
server <- function(input, output, ...){
  output$table <- renderDT(data)
  
  output$covidmap <- renderLeaflet({
    datafiltered <- data[which(data$date == input$dateselected), ]
    ordercounties <- match(map@data$NAME, datafiltered$county)
    map@data <- datafiltered[ordercounties,]
    
    map$variableplot <- as.numeric(
      map@data[, input$variableselected]
    )
    
    pal <- colorBin("YlOrRd", domain = map$variableplot, bins = 5)
    
    labels <- sprintf("%s: %g", map$county, map$variableplot) %>%
      lapply(htmltools::HTML)
    
    l <- leaflet(map) %>%
      addTiles() %>%
      setView(-100, 42, zoom = 4)%>% 
      addPolygons(
        fillColor = ~ pal(variableplot),
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        label = labels
      ) %>%
      leaflet::addLegend(
        pal = pal, values = ~variableplot,
        opacity = 0.7, title = NULL
      )

  })
  
  output$p <- renderPlotly({
    plot_ly(data, x= ~date, y = ~cases, color = ~county) %>%
      filter(county %in% input$counties) %>%
      group_by(county) %>%
      add_trace()
  })
  
  output$d <- renderPlotly({
    plot_ly(data, x= ~date, y = ~deaths, color = ~county) %>%
      filter(county %in% input$counties) %>%
      group_by(county) %>%
      add_trace()
  })
}

shinyApp(ui = ui, server = server)
