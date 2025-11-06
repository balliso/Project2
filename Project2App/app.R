library(shiny)
library(ggplot2)
library(tidyverse)
library(DT)

# Read in Seoul Bike Data
bike_data <- read.csv("SeoulBikeData.csv", header = TRUE, fileEncoding = "latin1")
# Tidy up column names
colnames(bike_data) <- (c("Date", "Rented_Bike_Count", "Hour", "Temperature_C", "Humidity_%", "Wind_speed_m/s", "Visibility_10m", "Dew_point_temp_C", "Solar_Radiation_MJ/m2", "Rainfall_mm", "Snowfall_cm", "Seasons", "Holiday", "Functioning_Day"))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # App title
    titlePanel("Seoul Bike Data Explorer"),

    # Sidebar 
    sidebarLayout(
      sidebarPanel(
        # Categorical Variables
        checkboxGroupInput("season", "Select Season(s)",
                           choices = unique(bike_data$Seasons),
                           selected = NULL),
        checkboxGroupInput("holiday", "Select Holiday Type(s)",
                           choices = unique(bike_data$Holiday),
                           selected = NULL),
        # Numeric variables
        selectInput("numVar", "Choose a Numeric Variable:", choices = c("Temperature_C", "Rainfall_mm")),
        uiOutput("numSlider"),
        # Action button
        actionButton("applyFilter", "Apply Filters", class = "btn-primary")
      ),

        mainPanel(
          tabsetPanel(
            tabPanel("About", 
                     img(src = "datasetcover.jpg", height = "100%", width = "100%"),
                     h1("About Seoul Bike Data Explorer"), 
                     h3("Purpose of App"), 
                     p("the purpose of this app is to..."),
                     tags$ul(
                       tags$li("do this thing"),
                       tags$li("do this thing")
                     ),
                     h3("Purpose of sidebar and tabs"), 
                     p("these tabs do this..."),
                     tags$ul(
                       tags$li("do this thing"),
                       tags$li("do this thing")
                     ),
                     h3("Dataset"),
                     img(src = "kagglelogo.png", height = "25%", width = "25%"),
                     p("this data is about..."),
                     tags$a(href="https://www.kaggle.com/datasets/saurabhshahane/seoul-bike-sharing-demand-prediction", "Click here for more information!")),
            tabPanel("Data Table", 
                     downloadButton(outputId = "downloadData", 
                                    label = "Click here to download table!", 
                                    class = NULL),
                     DT::dataTableOutput("filteredTable"))
            ,
            tabPanel("Data Exploration", h3("This is the Plots tab")))
        )
    )
)


# Server
server <- function(input, output) {
  # Store filtered data
  filtered_data <- reactiveVal(bike_data)
  
  # For categorical variables
  observeEvent(input$applyFilter, {
    df <- bike_data
    # Filter categorical variables
    if (!is.null(input$season) && length(input$season) > 0) {
      df <- df |> 
        filter(Seasons %in% input$season)
    }
    if (!is.null(input$holiday) && length(input$holiday) > 0) {
      df <- df |> 
        filter(Holiday %in% input$holiday)
    }
    # Filter numeric variables
    var <- input$numVar
    df <- df |>
      filter(
        get(var) >= input$numRange[1],
        get(var) <= input$numRange[2]
      )
  # Update dataset
  filtered_data(df)
  })
  
  # Numeric variable slider
  output$numSlider <- renderUI({
    var <- input$numVar
    
    if (var == "Temperature_C") {
      sliderInput("numRange", "Select Temperature (°C):",
                  min = min(bike_data$Temperature_C),
                  max = max(bike_data$Temperature_C),
                  value = c(min(bike_data$Temperature_C), max(bike_data$Temperature_C)))
    } else if (var == "Rainfall_mm") {
      sliderInput("numRange", "Select Rainfall (mm):",
                  min = min(bike_data$Rainfall_mm),
                  max = max(bike_data$Rainfall_mm),
                  value = c(min(bike_data$Rainfall_mm), max(bike_data$Rainfall_mm)))
    }
  })
  output$filteredTable <- DT::renderDataTable({
    filtered_data()
  })
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("bikedatatable_", Sys.Date(), ".csv")
    },
    content = function(file) {
      data_to_download <- filtered_data()
      write.csv(data_to_download, file, row.names = FALSE)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
