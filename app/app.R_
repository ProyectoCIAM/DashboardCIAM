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
      menuSubItem("Calificaciones instancias", tabName = "instancias")),

    menuItem("Registro de Violencia", icon = icon("bar-chart-o"), startExpanded = TRUE,
      menuSubItem("Violencia experimentada", tabName = "violencia"),
      menuSubItem("Tipos de violencia actuales", tabName = "vactuales"),
      menuSubItem("Datos demográficos", tabName = "demograficos"),
      menuSubItem("Datos del agresor", tabName = "agresor")),
    
    menuItem("Estadísticas de Satisfacción", icon = icon("bar-chart-o"), startExpanded = TRUE,
      menuSubItem("Calificación instalaciones", tabName = "instalaciones"),
      menuSubItem("Calificación de servicio", tabName = "servicios"),
      menuSubItem("Utilidad canalización", tabName = "ucanalizacion"),
      menuSubItem("Calificación servicio externo", tabName = "sexterno"),
      menuSubItem("Sesiones", tabName = "sesiones"),
      menuSubItem("Servicio oportuno e importante", tabName = "soportuno"),
      menuSubItem("Atención brindada", tabName = "atencion"))
  )
)
   

body <- dashboardBody(
   tabItems(
     tabItem("contacto", h2("Medios de contacto")),
     tabItem("canalizacion", h2("Canalización anterior a otra instancia")),
     tabItem("instancias", h2("Calificaciones por instancia")),
     tabItem("violencia", h2("Tipos de violencia experimentada")),
     tabItem("vactuales", h2("Tipos de violenccia actuales")),
     tabItem("demograficos", h2("Datos demográficos")),
     tabItem("agresor", h2("Datos del agresor")),
     tabItem("instalaciones", h2("Calificación de las instalaciones")),
     tabItem("servicios", h2("Calificación de los servicios")),
     tabItem("ucanalizacion", h2("Utilidad de la canalización")),
     tabItem("sexterno", h2("Calificación del servicio externo")),
     tabItem("sesiones", h2("Número de sesiones por edad y sexo")),
     tabItem("soportuno", h2("Servicio oportuno e importante")),
     tabItem("atencion", h2("Calificaciones de atención brindada"))
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