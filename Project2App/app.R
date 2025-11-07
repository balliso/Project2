library(shiny)
library(ggplot2)
library(tidyverse)
library(DT)

# Read in Seoul Bike Data
bike_data <- read.csv("SeoulBikeData.csv", header = TRUE, fileEncoding = "latin1")
# Tidy up column names
colnames(bike_data) <- (c("Date", "Rented_Bike_Count", "Hour", "Temperature_C", "Humidity_%", "Wind_speed_m/s", "Visibility_10m", "Dew_point_temp_C", "Solar_Radiation_MJ/m2", "Rainfall_mm", "Snowfall_cm", "Seasons", "Holiday", "Functioning_Day"))

# UI
ui <- fluidPage(

    # App title
    titlePanel("Seoul Bike Sharing Data Explorer"),

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
                     img(src = "seoulbike3.png", height = "100%", width = "100%"),
                     h1("About Seoul Bike Sharing Data Explorer"), 
                     h2("Purpose of this App"), 
                     p("The purpose of this app is to view data from the Seoul Bike Sharing datasheet, to discover relationships in the data. "),
                     br(),
                     p("The tabs along the top of the app can be clicked, showing the following:"),
                     tags$ul(
                       tags$li("The 'About' section describes the app and how to use it, and gives some background on the dataset."),
                       tags$li("The 'Data Download' section allows the user to view parts of the datasheet based on inputs from the sidebar, and download them to a computer. "),
                       tags$li("The 'Data Exploration' tab allows the user to obtain numeric and graphical summaries from the data, based on inputs on the sidebar.")
                     ),
                     br(),
                     p("The sidebar along the lefthand edge of the app is where a user can select numeric and categorical variables to view in the dataset. A user can:"),
                     tags$ul(
                       tags$li("Select up to four different seasons to view data from."),
                       tags$li("Select to view data from holidays vs nonholidays."),
                       tags$li("Choose to view one of two numeric variables: temperature or rainfall."),
                       tags$li("Use the slider to determine a range of temperature or rainfall to view."),
                       br(),
                       img(src = "seoulbike2.png", height = "100%", width = "100%")),
                     h2("About the data"),
                     p("The Seoul Bike Sharing dataset contains counts of public bicycles rented per hour in the Seoul Bike Sharing System. Corresponding weather and holiday information was also recorded. The dataset can be used to predict how atmospheric conditions affect the number of bikes rented."),
                     p("Please click the link below for more information."),
                     br(),
                     tags$a(href="https://www.kaggle.com/datasets/saurabhshahane/seoul-bike-sharing-demand-prediction", "Click here for more information!"),
                     br(), br(), br(),
                     img(src = "seoulbike.jpeg", height = "100%", width = "100%")),
            tabPanel("Data Table", br(), br(),
                     downloadButton(outputId = "downloadData", 
                                    label = "Click here to download table!", 
                                    class = NULL),
                     br(), br(), br(),
                     DT::dataTableOutput("filteredTable"))
            ,
            tabPanel("Data Exploration", h3("This is the plots tab")))
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
