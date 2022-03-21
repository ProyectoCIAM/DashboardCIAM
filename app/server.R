library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(jsonlite)
library(httr)

    

server <- function(input, output, session) {
    #base_url <- "http://127.0.0.1:8080/api/"
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
    canalizacion_seguimiento_json <- jsonlite::fromJSON("http://127.0.0.1:8080/api/canalizacion_seguimiento")
    instancia_json <- jsonlite::fromJSON("http://127.0.0.1:8080/api/instancia")

    # separamos el dataframe
    canalizacion_seguimiento <- canalizacion_seguimiento_json$response
    # este es el catalogo de instancias
    instancia <- instancia_json$response

    # seleccionamos las columnas de interes
    calificacionxinstancia <- data.frame("id_canalizacion_seguimiento"=canalizacion_seguimiento$id_canalizacion_seguimiento,
                                        "calificacion"=canalizacion_seguimiento$calificacion,
                                        "id_instancia"=canalizacion_seguimiento$instancia)

    # hacemos el merge a traves de id_instancia
    calificacionxinstancia <- merge(calificacionxinstancia,instancia)

    # creamos histograma
    output$calificacionesxinstancia <- renderPlotly({
        ggplotly(ggplot(calificacionxinstancia, aes(x=calificacion, fill=nombre)) +
            geom_bar())
    })
}
