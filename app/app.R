library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)

header <- dashboardHeader(title = "Dashboard CIAM")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Tablero de Canalización", icon = icon("dashboard"), startExpanded = TRUE, 
      menuSubItem("Contacto", tabName = "contacto"),
      menuSubItem("Canalización anterior", tabName = "canalizacion"),
      menuSubItem("Calificaciones instancias", tabName = "calificaciones")),

    menuItem("Registro de Violencia", icon = icon("bar-chart-o"), startExpanded = TRUE,
      menuSubItem("Violencia experimentada", tabName = "violencia"),
      menuSubItem("Tipos de violencia actuales", tabName = "vactuales"),
      menuSubItem("Datos demográficos", tabName = "demograficos"),
      menuSubItem("Datos del agresor", tabName = "agresor"))
  )
)
   

body <- dashboardBody(
   tabItems(
     tabItem("contacto", h2("Medios de contacto")),
     tabItem("violencia", h2("Registro de Violencia"))

   )
  )



ui <- dashboardPage(
  header,
  sidebar,
  body,
  skin = "purple"
)

server <- function(input, output) { }

shinyApp(ui, server)