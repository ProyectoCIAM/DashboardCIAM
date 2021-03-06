library(plotly)
library(shiny)
library(shinydashboard)

header <- dashboardHeader(title = "Dashboard CIAM")

sidebar <- dashboardSidebar(
  sidebarMenu(
    dateRangeInput('dateRange', label = 'Filtrar por fechas:',
      start = Sys.Date() - 365, end = Sys.Date()
    ),
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
          title = "Medio de Contacto", width = 4, height="70vh", solidHeader = TRUE,
          plotlyOutput("contacto")
        ),
        box(
          title = "¿La persona que contacta es quien requiere el servicio?", width = 2, height="70vh", solidHeader = TRUE,
          plotlyOutput("persona_servicio")
        ),
        box(
          title = "¿Cómo se enteró de los servicos de acompañamiento emocional?", width = 6, height="70vh", solidHeader = TRUE,
          plotlyOutput("serv_acomp_emocional")
        )
      ),
    ),
    tabItem("canalizacion", h2("Canalización anterior a otra instancia"),
      fluidRow(
        tabBox(
          width = 4,
          height="65vh",
          tabPanel("Sección A", "¿Ha sido canalizada/o a otras instancias con anterioridad?", plotlyOutput("canalizacionxAnterior")),

          tabPanel("Sección B", "¿Ha tenido seguimiento?", plotlyOutput("canalizacionxSeguimiento"))
        ),

        tabBox(
          width = 4,
          height="65vh",
          tabPanel("Sección C", "¿Considera que el medio de contacto fue el adecuado?", plotlyOutput("canalizacionxAdecuado")),

          tabPanel("Sección D", "¿Considera que el servicio fue oportuno (en el momento adecuado)?", plotlyOutput("canalizacionxOportuno")) 
        ),

        tabBox(
         width = 4,
         height="65vh",
         tabPanel("Sección E", "¿Recibió atención con prontitud?", plotlyOutput("canalizacionxProntitud")),

         tabPanel("Sección F", "¿Se sintió en confianza y seguro durante la atención?", plotlyOutput("canalizacionxConfianza")),

         tabPanel("Sección G", "¿Sintió en todo momento que su caso fue tratado con respeto?", plotlyOutput("canalizacionxRespeto"))
      ),


      ),
    ),

     tabItem("instancias", h2("Calificaciones por instancia"),
      mainPanel(
        "En general, ¿cómo calificaría la calidad de la atención recibida en esa instancia, del 1 al 10, siendo 1 una calificación muy mala y 10 muy buena?", plotlyOutput("calificacionesxinstancia")
        )),
    ############   TABS  DASHBOARD 2
     tabItem("violencia", h2("Tipos de violencia experimentada"),
      fluidRow(
        box(
          title = "¿Qué tipos de violencia ha experimentado?", width = 6, solidHeader = TRUE,
          plotlyOutput("hist_tipo_violencia_anterior")
        ),
        box(
          title = "Modalidad", width = 6, solidHeader = TRUE,
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
          title = "¿Qué tipos de violencia experimenta actualmente?", width = 6, height = "300px", solidHeader = TRUE,
          plotlyOutput("hist_tipo_violencia_actual", height = '220px')
        ),
        box(
          title = "Modalidad", width = 6, height = "300px", solidHeader = TRUE,
          plotlyOutput("hist_modalidad_actual",height = '220px', width = 'auto')
        ),
        box(
          title = "Tipos de Violencia Experimentada VS. Modalidad ACTUAL", width = 6, height = "300px", solidHeader = TRUE,
          plotlyOutput("hist_tipo_vs_modalidad_actual", height = '220px',)
        )
      )),
     tabItem("demograficos", h2("Datos demográficos de la víctima"),
      fluidRow(
        box(
          title = "Edades", width = 6, height = '300px', solidHeader = FALSE,
          plotlyOutput("personasxEdad",height = '220px')
        ),
        box(
          title = "Tipos de servicio", width = 6, height = '300px', solidHeader = FALSE,
          plotlyOutput("personasxServicio",height = '220px')
        ),
        box(
          title = "Datos demográficos", width = 6, height = '550px', solidHeader = FALSE,
          tabBox(
            width = 12,
            height = '400px',
            tabPanel("Sección A", "¿Pertenece a la comunidad LGBTTTQA+?", plotlyOutput("personasxLGBT",height = '400px')),

            tabPanel("Sección B", "Identidad sexogenerica", plotlyOutput("personasxSexo",height = '400px')),

            tabPanel("Sección C", "¿Cuenta con alguna discapacidad?", plotlyOutput("personasxDiscapacidad",height = '400px')),

            tabPanel("Sección D", "¿Pertenece a pueblos originarios?", plotlyOutput("personasxPueblos",height = '400px')),

            tabPanel("Sección E", "¿Habla alguna lengua indígena?", plotlyOutput("personasxLengua_Indigena",height = '400px'))
          ),
        ),
        box(
          title = "Residencias", width = 6, solidHeader = FALSE,
          tabBox(
            width = 12,
            tabPanel("Localidad", "", plotlyOutput("personasxLocalidad")),

            tabPanel("Estado", "", plotlyOutput("personasxEstado")),

            tabPanel("Pais", "", plotlyOutput("personasxPais"))
          ),
        )
      )
    ),
     tabItem("agresor", h2("Datos del agresor"),
      fluidRow(
        box(
          title = "Edad del agresor", width = 6, height = "300px", solidHeader = TRUE,
          plotlyOutput("edades_agresor_grf",height = "220px")
        ),
        box(
          title = "Sexo del agresor", width = 6, height = "300px", solidHeader = TRUE,
          plotlyOutput("sexo_agresor_grf",height = "220px")
        ),
        box(
          title = "La dirección del agresor es la misma que de quien solicita el servico", width = 6, height = "300px", solidHeader = TRUE,
          plotlyOutput("misma_dir_agresor_victima",height = "220px")
        ),
        box(
          title = "¿La persona agresora cuenta con red de apoyo?", width = 6, height = "300px", solidHeader = TRUE,
          plotlyOutput("red_apoyo_agresor",height = "220px")
        )
      ),
     ),
    ############   TABS  DASHBOARD 3
     tabItem("instalaciones", h2("Calificación de las instalaciones"),
      fluidRow(
        box(
          title = "¿El servicio de Acompañamiento Emocional lo recibió de forma presencial (en las instalaciones de CIAM Cancún)?", width = 4, height = "450px", solidHeader = TRUE, 
          plotlyOutput("presencial", height = "370px")
        ),
        box(
          title = "Califique por favor las instalaciones de CIAM Cancún", width = 8, height = "450px", solidHeader = TRUE, 
          plotlyOutput("califInstalaciones", height = "370px")
        )
      )),
     tabItem("servicios", h2("Calificación de los servicios"),
      fluidRow(
        box(
          title = "¿Logró con el Acompañamiento Emocional los cambios que esperaba?", width = 6, height = "300px", header = TRUE, 
          plotlyOutput("cambios", height = "220px")
        ),
        box(
          title = "¿Recurriría a nuestros servicios de acompañamiento emocional de nuevo?", width = 6, height = "300px", header = TRUE, 
          plotlyOutput("serviciosDeNuevo", height = "220px")
        ),
        box(
          title = "¿Si requiriera de nueva cuenta los servicios de Acompañamiento Emocional de CIAM, le gustaría que le atendiera el mismo/a psicoterapeuta?", width = 6, height = "300px", header = TRUE, 
          plotlyOutput("mismoPsico", height = "220px")
        ),
        box(
          title = "¿Recomendaría nuestros servicios a otras personas?", width = 6, height = "300px", header = TRUE, 
          plotlyOutput("recomendacion", height = "220px")
        )
      )
     ),
     tabItem("ucanalizacion", h2("Utilidad de la canalización"),
      fluidRow(
        box(title = "¿Fue canalizada/o por parte de CIAM a alguna otra institución?", width = 6, height = "400px", header = TRUE, plotlyOutput("canalizado", height = "320px")),
        tabBox(
          width = 6, height = "400px", 
          tabPanel("Edad", "Edades de canalizadas/os", plotlyOutput("edadesCanalizados", height = "320px")),
          tabPanel("Sexo", "Identidad sexogenérica de canalizadas/os", plotlyOutput("sexoCanalizados", height = "320px"))
        ),
        box(title = "¿A cuál institución?", width = 12, header = TRUE, plotlyOutput("frecInstanciasCanalizadas"))
      )),
     tabItem("sexterno", h2("Calificación del servicio externo"),
      fluidRow(
        box(
          title = "¿Cómo califica el servicio que le dieron las autoridades en la instancia a la que fue canalizada/o?", width = 8, height = "500px", solidHeader = TRUE,
          # tabBox(
          #   width = 12,
          #   tabPanel("¿Cómo califica el servicio que le dieron las autoridades en la instancia a la que fue canalizada/o?", "", plotlyOutput("satisfaccionxServicio")),

          #   tabPanel("¿La información que se le brindó en CIAM para poder acudir a esa instancia le fue útil?", "", plotlyOutput("satisfaccionxUtil")),
          # ),
          plotlyOutput("satisfaccionxServicio", height = "420px")
        ),
        box(
          title = "¿La información que se le brindó en CIAM para poder acudir a esa instancia le fue útil?", width = 4, height = "500px", solidHeader = TRUE,
          plotlyOutput("satisfaccionxUtil", height = "420px")
        ),
      )
     ),
    tabItem("sesiones", h2("Número de sesiones por edad y sexo"),
    fluidRow(
       box(title = "¿Cuántas sesiones de acompañamiento emocional recibió por parte de CIAM Cancún A.C.? (Por rangos de edad)", width = 6, solidHeader = TRUE,
       plotlyOutput("sesionesxEdades")),
       box(title = "¿Cuántas sesiones de acompañamiento emocional recibió por parte de CIAM Cancún A.C.? (Por identidad sexogenérica)", width = 6, solidHeader = TRUE,
       plotlyOutput("sesionesxSexo"))
     ),
    ),
    tabItem("soportuno", h2("Servicio oportuno e importante"),   fluidRow(
       box(title = "¿Recibió el servicio de Acompañamiento Emocional oportunamente (en el momento indicado) y de manera pronta?", width = 6, solidHeader = TRUE,
       plotlyOutput("si_servicio")),

       box(title = "¿Qué tan importante y necesario fue para usted recibir el Servicio de Acompañamiento Emocional?", width = 6, solidHeader = TRUE,
       plotlyOutput("servicio_importante")),
       box(title = "Oportuno y pronto vs. Importante", width = 6, solidHeader = TRUE,
       plotlyOutput("importantexOportuno"))
     ),
    ),
     tabItem("atencion", h2("Calificaciones de atención brindada"),
     fluidRow(
        box(
         title = "¿Cómo califica la vía de atención, se adaptó a sus necesidades?", width = 6, height = "300px", solidHeader = TRUE, 
         plotlyOutput("via_atencion", height = "220px")
        ),

        box(
         title = "¿Cómo califica la confianza y seguridad que le hicieron sentir durante la atención?", width = 6, height = "300px", solidHeader = TRUE, 
         plotlyOutput("calificacion_confianza_seguridad", height = "220px")
        ),

        box(
         title = "Califique por favor el respeto con el que sintió que fue tratada/o durante el Acompañamiento Emocional", width = 6, height = "300px", solidHeader = TRUE, 
         plotlyOutput("respeto_sesiones", height = "220px")
        ),

        box(
         title = "¿Qué tan satisfactorio fue el trato que le brindó el equipo de Acompañamiento Emocional de CIAM?", width = 6, height = "300px", solidHeader = TRUE, 
         plotlyOutput("serv_satisfactorio", height = "220px")
        ),
     ),
     )
   )
  )



dashboardPage(
  header,
  sidebar,
  body,
  skin = 'purple'
)