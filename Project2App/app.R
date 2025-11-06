library(shiny)
library(ggplot2)

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
        selectInput("season", "Select Season",
                    choices = unique(bike_data$Seasons),
                    selected = unique(bike_data$Seasons),
                    multiple = TRUE),
        selectInput("holiday", "Select Holiday Type",
                    choices = unique(bike_data$Holiday),
                    selected = unique(bike_data$Holiday),
                    multiple = TRUE),
        # Numeric Variables
        sliderInput("rainfall", "Select Rainfall (mm)",
                    min = min(bike_data$Rainfall_mm),
                    max = max(bike_data$Rainfall_mm),
                    value = c(min(bike_data$Rainfall_mm), max(bike_data$Rainfall_mm))),
        sliderInput("temperature", "Select Temperature (°C)",
                    min = min(bike_data$Temperature_C),
                    max = max(bike_data$Temperature_C),
                    value = c(min(bike_data$Temperature_C), max(bike_data$Temperature_C))),
        # Action button
        actionButton("applyFilter", "Apply Filters", class = "btn-primary")
      ),

        mainPanel(
          tabsetPanel(
            tabPanel("About", h3("This is the About tab")),
            tabPanel("Data", h3("This is the Data tab")),
            tabPanel("Plots", h3("This is the Plots tab")))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
