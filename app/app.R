library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)

body <- dashboardBody(
    tabBox(
        title = "Sample tabBox",
        id = "tabset1", height = "600px", width = "500px",
        tabPanel("Tab1", "First Tab"),
        tabPanel("Tab2", "Second Tab"),
        ),
  )

ui <- dashboardPage(
  header = dashboardHeader(),
  sidebar = dashboardSidebar(),
  body = body,
  skin = "purple",
)

server <- function(input, output) { }

shinyApp(ui, server)