library(shiny)
library(dplyr)
library(ggplot2)
#library(plotly)
library(jsonlite)
library(httr)

    

server <- function(input, output, session) {
    base_url <- "http://127.0.0.1:8080/api/"
    #infor_url <- "folio"
    #full_url <- base::paste0(base_url, infor_url)
    #api_call <- httr::GET(full_url) 
  
    #retrieving json files
    #folios_json <- jsonlite::fromJSON(full_url)

    #personas_json <- jsonlite::fromJSON("http://http://127.0.0.1:8080/api/persona")


    #retrieving api's response leaving the status out
    #folios <- folios_json$response

    #folios

    #personas <- personas_json$response

    # D1 p3
    # traemos la informacion
    full_url <- base::paste0(base_url, "canalizacion_seguimiento")
    canalizacion_seguimiento_json <- jsonlite::fromJSON(full_url)

    full_url <- base::paste0(base_url, "instancia")
    instancia_json <- jsonlite::fromJSON(full_url)

    # separamos el dataframe
    canalizacion_seguimiento <- canalizacion_seguimiento_json$response
    instancia <- instancia_json$response

    output$calificacionesxinstancia <- renderPlotly({
        colnames(canalizacion_seguimiento)[17] <- "id_instancia"
        # seleccionamos las columnas de interes
        califxinstancia <- canalizacion_seguimiento %>%
        select("id_canalizacion_seguimiento", "calificacion", "id_instancia")
        colnames(instancia)[2] <- "instancia"
        # hacemos el merge a traves de id_instancia
        califxinstancia <- merge(califxinstancia, instancia)
        # graficamos
        ggplotly(ggplot(califxinstancia, aes(x = calificacion, fill = instancia)) +
            geom_bar())
    })
}
