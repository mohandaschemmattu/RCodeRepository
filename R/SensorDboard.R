
# Load packages -----------------------------------------------------
library(shiny)
library(highcharter)
library(dplyr)
library(tidyr)

# Load data ---------------------------------------------------------
births <- read.csv("Data/births.csv")

CascadeData <- read.csv("Data/StairsDirectionPlot220317.csv")

# Determine years in data -------------------------------------------
years <- unique(births$year)

Dates <- unique(CascadeData$Sday)

# UI ----------------------------------------------------------------
ui <- fluidPage(

  # App title -------------------------------------------------------
  titlePanel("Stair usage - Month to Date"),

  # Sidebar layout with a input and output definitions --------------
  sidebarLayout(

    # Inputs --------------------------------------------------------
    sidebarPanel(



      sliderInput("year",
                  label = "Day",
                  min = min(Dates),
                  max = max(Dates),
                  step = 1,
                  sep = "",
                  value = range(Dates)),


      selectInput("Direction",
                  label = "Direction",
                  choices = c("Downward" = "Downward",
                              "Upward" = "Upward")),

      selectInput("plot_type",
                  label = "Plot type",
                  choices = c("Scatter" = "scatter",
                              "Bar" = "column",
                              "Line" = "line")),
      selectInput("theme",
                  label = "Theme",
                  choices = c("No theme",
                              "Chalk" = "chalk",
                              "Dark Unica" = "darkunica",
                              "Economist" = "economist",
                              "FiveThirtyEight" = "fivethirtyeight",
                              "Gridlight" = "gridlight",
                              "Handdrawn" = "handdrawn",
                              "Sandsignika" = "sandsignika"))
    ),

    # Output --------------------------------------------------------
    mainPanel(
      highchartOutput("hcontainer", height = "500px")
    )

  )
)


# SERVER ------------------------------------------------------------
server = function(input, output) {

  # Calculate differences between 13th and avg of 6th and 20th ------

  CascadeDataFil <- reactive({
    #CascadeData <- read.csv("StairsDirectionPlot.csv")%>%
    CascadeData%>%
      filter(Direction == input$Direction) %>%
      filter(between(Sday, input$year[1], input$year[2]))
  })

  # Text string of selected years for plot subtitle -----------------
  selected_years_to_print <- reactive({
    #invalidateLater(5000)
    if(input$year[1] == input$year[2]) {
      as.character(input$year[1])
    } else {
      paste(input$year[1], " - ", input$year[2])
    }
  })

  # Highchart -------------------------------------------------------
  output$hcontainer <- renderHighchart({
    hc <- highchart() %>%
      hc_add_series(data = CascadeDataFil()$Duration,
                    type = input$plot_type,
                    name = "Time takn in Seconds",
                    showInLegend = FALSE) %>%


      hc_yAxis(title = list(text = "Time taken in Seconds"),
               allowDecimals = TRUE) %>%


      hc_xAxis(categories =CascadeData$StartTime,
               tickmarkPlacement = "on",
               opposite = TRUE) %>%


      hc_title(text = "Stair Usage",
               style = list(fontWeight = "bold")) %>%


      hc_subtitle(text = paste("Time taken to use stairs,",
                               selected_years_to_print())) %>%


      hc_tooltip(valueDecimals = 4,
                 pointFormat = "Datetime: {point.x} <br> Time taken: {point.y}") %>%


      hc_credits(enabled = TRUE,
                 text = "Sources: Cascde Connectedcare",
                 style = list(fontSize = "10px"))

    # Determine theme and apply to highchart ------------------------
    if (input$theme != "No theme") {
      theme <- switch(input$theme,
                      chalk = hc_theme_chalk(),
                      darkunica = hc_theme_darkunica(),
                      fivethirtyeight = hc_theme_538(),
                      gridlight = hc_theme_gridlight(),
                      handdrawn = hc_theme_handdrawn(),
                      economist = hc_theme_economist(),
                      sandsignika = hc_theme_sandsignika()
      )
      hc <- hc %>%
        hc_add_theme(theme)
    }



    # Print highchart -----------------------------------------------
    hc
  })

}



# Run app -----------------------------------------------------------
shinyApp(ui = ui, server = server)
