library(plotly)
library(shiny)
library(shinydashboard)

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
    ############   TABS  DASHBOARD 1
    tabItem("contacto", h2("Medios de contacto"),  
      fluidRow(
        box(
          title = "Medio de Contacto", width = 6, solidHeader = TRUE,
          plotOutput("contacto")
        ),
        box(
          title = "¿La persona que contacta es quien requiere el servicio?", width = 6, solidHeader = TRUE,
          plotlyOutput("persona_servicio")
        ),
        box(
          title = "¿Cómo se enteró de los servicos de acompañamiento emocional?", width = 12, solidHeader = TRUE,
          plotlyOutput("serv_acomp_emocional")
        )
      ),
    ),
    tabItem("canalizacion", h2("Canalización anterior a otra instancia"),
      fluidRow(
        tabBox(
          tabPanel("Sección A", "¿Ha sido canalizada/o a otras instancias con anterioridad?", plotlyOutput("canalizacionxAnterior")),

          tabPanel("Sección B", "¿Ha tenido seguimiento?", plotlyOutput("canalizacionxSeguimiento"))
        ),

        tabBox(
          tabPanel("Sección C", "¿Considera que el medio de contacto fue el adecuado?", plotlyOutput("canalizacionxAdecuado")),

          tabPanel("Sección D", "¿Considera que el servicio fue oportuno (en el momento adecuado)?", plotlyOutput("canalizacionxOportuno")) 
        ),

        tabBox(
         width = 12,
         tabPanel("Sección E", "¿Recibió atención con prontitud?", plotlyOutput("canalizacionxProntitud")),

         tabPanel("Sección F", "¿Se sintió en confianza y seguro durante la atención?", plotlyOutput("canalizacionxConfianza")),

         tabPanel("Sección G", "¿Sintió en todo momento que su caso fue tratado con respeto?", plotlyOutput("canalizacionxRespeto"))
      ),


      ),
    ),

     tabItem("instancias", h2("Calificaciones por instancia"),
      mainPanel(
        plotlyOutput("calificacionesxinstancia")
        )),
    ############   TABS  DASHBOARD 2
     tabItem("violencia", h2("Tipos de violencia experimentada"),
      fluidRow(
        box(
          title = "Tipos de Violencia Experimentada ANTERIORMENTE", width = 6, solidHeader = TRUE,
          plotlyOutput("hist_tipo_violencia_anterior")
        ),
        box(
          title = "Modalidad Experimentada ANTERIORMENTE", width = 6, solidHeader = TRUE,
          plotlyOutput("hist_modalidad_anterior")
        ),
        box(
          title = "Tipos de Violencia Experimentada VS. Modalidad ANTERIORMENTE", width = 12, solidHeader = TRUE,
          plotlyOutput("hist_tipo_vs_modalidad_anterior")
        )
      )),
     tabItem("vactuales", h2("Tipos de violencia actuales"),
      fluidRow(
        box(
          title = "Tipos de Violencia Experimentada ACTUAL", width = 6, solidHeader = TRUE,
          plotlyOutput("hist_tipo_violencia_actual")
        ),
        box(
          title = "Modalidad Experimentada ACTUAL", width = 6, solidHeader = TRUE,
          plotlyOutput("hist_modalidad_actual")
        ),
        box(
          title = "Tipos de Violencia Experimentada VS. Modalidad ACTUAL", width = 12, solidHeader = TRUE,
          plotlyOutput("hist_tipo_vs_modalidad_actual")
        )
      )),
     tabItem("demograficos", h2("Datos demográficos de la víctima"),
      fluidRow(
        box(
          title = "Personas Edades", width = 6, solidHeader = FALSE,
          plotlyOutput("personasxEdad")
        ),
        box(
          title = "Personas LGBT", width = 6, solidHeader = FALSE,
          plotlyOutput("personasxLGBT")
        ),
        box(
          title = "¿Cuenta con alguna discapacidad?", width = 6, solidHeader = FALSE,
          plotlyOutput("personasxDiscapacidad")
        ),
        box(
          title = "Residencias", width = 12, solidHeader = FALSE,
          tabBox(
            width = 12,
            tabPanel("Localidad", "", plotlyOutput("personasxLocalidad")),

            tabPanel("Estado", "", plotlyOutput("personasxEstado")),

            tabPanel("Pais", "", plotlyOutput("personasxPais"))
          ),
        ),
        box(
          title = "¿Pertenece a pueblos originarios?", width = 6, solidHeader = FALSE,
          plotlyOutput("personasxPueblos")
        ),
        box(
          title = "¿Habla alguna lengua indígena?", width = 6, solidHeader = FALSE,
          plotlyOutput("personasxLengua_Indigena")
        ),
        box(
          title = "Tipos de servicio", width = 6, solidHeader = FALSE,
          plotlyOutput("personasxServicio")
        )
      )
    ),
     tabItem("agresor", h2("Datos del agresor"),
      fluidRow(
        box(
          title = "Edad del agresor", width = 6, solidHeader = TRUE,
          plotlyOutput("edades_agresor_grf")
        ),
        box(
          title = "Sexo del agresor", width = 6, solidHeader = TRUE,
          plotlyOutput("sexo_agresor_grf")
        ),
        box(
          title = "La dirección del agresor es la misma que de quien solicita el servico", width = 6, solidHeader = TRUE,
          plotlyOutput("misma_dir_agresor_victima")
        ),
        box(title = "¿La persona agresora cuenta con red de apoyo?", width = 6, solidHeader = TRUE,
        plotlyOutput("red_apoyo_agresor"))
      ),
     ),
    ############   TABS  DASHBOARD 3
     tabItem("instalaciones", h2("Calificación de las instalaciones")),
     tabItem("servicios", h2("Calificación de los servicios")),
     tabItem("ucanalizacion", h2("Utilidad de la canalización")),
     tabItem("sexterno", h2("Calificación del servicio externo")),
     tabItem("sesiones", h2("Número de sesiones por edad y sexo")),
     tabItem("soportuno", h2("Servicio oportuno e importante"),   fluidRow(
       tabBox(
         tabPanel("Sección F p.1","¿Recibió el servicio de Acompañamiento Emocional oportunamente (en el momento indicado) y de manera pronta?", plotlyOutput("si_servicio")),
         
         tabPanel("Sección F p.2", "¿Qué tan importante y necesario fue para usted recibir el Servicio de Acompañamiento Emocional?")
      ),
     ),
    ),
     tabItem("atencion", h2("Calificaciones de atención brindada"))
   )
  )



dashboardPage(
  header,
  sidebar,
  body,
  skin = "purple"
)