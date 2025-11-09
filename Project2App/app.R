library(shiny)
library(ggplot2)
library(tidyverse)
library(DT)

# Read in Seoul Bike Data
bike_data <- read.csv("SeoulBikeData.csv", header = TRUE, fileEncoding = "latin1")
# Fix column names
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
        actionButton("applyFilter", "Apply Filters", class = "btn-primary")),
    # Main Panel
        mainPanel(
          tabsetPanel(
            tabPanel("About", 
                     img(src = "seoulbike3.png", height = "100%", width = "100%"),
                     h1("About: Seoul Bike Sharing Data Explorer"), 
                     h2("Purpose of the App"), 
                     p("The Seoul Bike Sharing Data Explorer is a tool to help users explore patterns in bike rentals across different weather and seasonal conditions. By selecting specific criteria in the sidebar, you can observe how seasons, holidays, temperature, and rainfall influence bike rentals in Seoul."),
                     h2("How to Use This App"),
                     p("Use the sidebar on the left to select numeric and categorical variables to filter the dataset. A user can:"),
                     tags$ul(
                       tags$li("Select up to four different seasons."),
                       tags$li("Choose to view data from holidays, nonholidays, or both."),
                       tags$li("Select one of two numeric variables, temperature or rainfall, and use the slider to adjust its range."),
                       tags$li("Click 'Apply Filters' to update the data and plots.")),
                       br(),
                     p("The app is organized into tabs across the top, showing the following:"),
                     tags$ul(
                       tags$li("The 'About' tab (this page) describes the app and how to use it, and gives some background on the dataset."),
                       tags$li("The 'Data Download' tab displays a filtered version of the dataset, which you can download to your computer."),
                       tags$li("The 'Data Exploration' tab provides summary statistics and visualizations to help identify trends and relationships in the data.")),
                     br(),
                       img(src = "seoulbike2.png", height = "100%", width = "100%"),
                     h2("About the Dataset"),
                     p("The data used in this app, the Seoul Bike Sharing dataset, contains counts of public bicycles rented per hour from the Seoul Bike Sharing System, along with corresponding weather and holiday information. It can be used to understand how atmospheric conditions affect the number of bikes rented, and make predictions for future bike rentals."),
                     br(),
                     p("The original dataset is available on Kaggle. Please click the link below for more information."),
                     tags$a(href="https://www.kaggle.com/datasets/saurabhshahane/seoul-bike-sharing-demand-prediction", "Click here for more information!"),
                     br(), br(), br(),
                     img(src = "seoulbike.jpeg", height = "100%", width = "100%")),
            
            tabPanel("Data Download", 
                     h3("See Table and Download Below"),
                     br(),
                     downloadButton(outputId = "downloadData", 
                                    label = "Click here to download table!", 
                                    class = NULL),
                     br(), br(), br(),
                     DT::dataTableOutput("filteredTable")),
            
            tabPanel("Data Exploration",
                     h3("Explore the Subsetted Data"),
                     selectInput("summaryType", "Choose a summary type:",
                                 choices = c("Categorical Summaries", "Numeric Summaries", "Graphs")),
                     # One-way Contingency
                     conditionalPanel(
                       condition = "input.summaryType == 'Categorical Summaries'",
                       br(),
                       h4("One-way Contingency Table"),
                       selectInput("catVar1", "Choose a variable",
                                   choices = c("Holiday", "Functioning_Day", "Seasons"),
                                   selected = "Holiday"),
                       tableOutput("catSummary1")),
                     br(),
                     # Two-way Contingency
                     conditionalPanel(
                       condition = "input.summaryType == 'Categorical Summaries'",
                       h4("Two-way Contingency Table"),
                       selectInput("catVar2_row", "Row variable:",
                                   choices = c("Holiday", "Functioning_Day", "Seasons")),
                       selectInput("catVar2_col", "Column variable:",
                                   choices = c("Holiday", "Functioning_Day", "Seasons"),
                                   selected = "Holiday"),
                       tableOutput("catSummary2")),
                     # Numeric summaries
                     conditionalPanel(
                       condition = "input.summaryType == 'Numeric Summaries'",
                       selectInput("numSummaryVar", "Numeric variable:",
                                   choices = c("Rented_Bike_Count","Temperature_C","Humidity_%","Wind_speed_m/s",
                                               "Visibility_10m","Dew_point_temp_C","Solar_Radiation_MJ/m2",
                                               "Rainfall_mm","Snowfall_cm")),
                       tableOutput("numSummary")),
                     # Graphs
                     conditionalPanel(
                       condition = "input.summaryType == 'Graphs'",
                       selectInput("plotType", "Choose a plot type:",
                                   choices = c("Bar Plot","Scatterplot","Boxplot","Line Plot","Density Plot","Heatmap")),
                       plotOutput("explorePlot")))
            )
        )
    )
)


# Server
server <- function(input, output) {
  # Store filtered data
  filtered_data <- reactiveVal(bike_data)
  
  # Sidebar 
  
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
  
  # Data Download Tab
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
    })
  
  # Data Exploration Tab

  # Contingency tables
  # One-way
  output$catSummary1 <- renderTable({
    df <- filtered_data()
    req(nrow(df) > 0)
    v1 <- input$catVar1
    req(v1)
    if (!(v1 %in% names(df))) {
      return(data.frame(Error = paste("Variable", v1, "not found in dataset.")))
    }
    tbl <- table(df[[v1]], useNA = "ifany")
    df_out <- data.frame(matrix(tbl, nrow = 1))
    colnames(df_out) <- names(tbl)
    df_out
  }, rownames = FALSE)
  # Two-way
  output$catSummary2 <- renderTable({
    df <- filtered_data()
    req(nrow(df) > 0)
    v_row <- input$catVar2_row
    v_col <- input$catVar2_col
    if (!(v_row %in% names(df))) {
      return(data.frame(Error = paste("Variable", v_row, "not found in dataset.")))
    }
    if (!(v_col %in% names(df))) {
      return(data.frame(Error = paste("Variable", v_col, "not found in dataset.")))
    }
      as.data.frame.matrix(table(df[[v_row]], df[[v_col]]))
    }, rownames = TRUE)
  
  # Numeric Summaries
  output$numSummary <- renderTable({
    df <- filtered_data()
    req(nrow(df) > 0)
    
    v <- input$numSummaryVar
    tibble::tibble(
      Mean   = mean(df[[v]], na.rm = TRUE),
      Median = median(df[[v]], na.rm = TRUE),
      SD     = sd(df[[v]], na.rm = TRUE),
      IQR    = IQR(df[[v]], na.rm = TRUE)
    )
  })
  # Plots
  output$explorePlot <- renderPlot({
    req(input$summaryType == "Graphs")
    
    df <- filtered_data()
    req(nrow(df) > 0)
    
    if (input$plotType == "Bar Plot") {
      ggplot(df, aes(x = Holiday)) +
        geom_bar(fill = "darkseagreen") +
        labs(x = "Holiday", y = "Count",
             title = "Bar Plot of Holiday vs No Holiday Bike Rentals")
      
    } else if (input$plotType == "Scatterplot") {
      ggplot(df, aes(x = Temperature_C, y = Rented_Bike_Count, color = Seasons)) +
        geom_point() +
        labs(x = "Temperature (°C)", y = "Rented Bike Count",
             title = "Scatterplot of Temperature vs Rented Bike Count by Season") +
        scale_color_manual(values = c("Winter" = "cadetblue1", "Spring" = "lightpink", "Summer" = "cornflowerblue", "Autumn" = "darkolivegreen"))
      
    } else if (input$plotType == "Boxplot") {
      ggplot(df, aes(x = Seasons, y = Rented_Bike_Count, fill = Functioning_Day)) +
        geom_boxplot() +
        labs(x = "Season", y = "Rented Bike Count",
             title = "Boxplot of Rented Bike Count by Season and Functioning Day") +
        scale_fill_manual(values = c("No" = "darkgoldenrod", "Yes" = "darkslategray3"))
      
    } else if (input$plotType == "Line Plot") {
      ggplot(df, aes(x = Hour, y = Rented_Bike_Count, color = Seasons)) +
        geom_line() +
        labs(x = "Hour Rented", y = "Rented Bike Count",
             title = "Lineplot of Hour Rented vs Rented Bike Count by Seasons")
      
    } else if (input$plotType == "Density Plot") {
      ggplot(df, aes(x = Temperature_C)) +
        geom_density(fill = "salmon", alpha = 0.7) +
        facet_wrap(~ Seasons) +
        labs(x = "Temperature (°C)", y = "Density",
             title = "Temperature Distribution per Season")
      
    } else if (input$plotType == "Heatmap") {
      ggplot(df, aes(x = Hour, y = Seasons, fill = Rented_Bike_Count)) +
        geom_tile(color = "white") +
        scale_fill_gradient(low = "yellow", high = "red") +
        labs(x = "Hour of Rental", y = "Season", fill = "Rented Bike Count",
             title = "Heatmap of Hour vs Season by Rented Bike Count")
    }
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
