library(shiny)
library(dplyr)
library(ggplot2)
#library(plotly)
library(jsonlite)
library(httr)

    

server <- function(input, output, session) {

    ########################        DASHBOARD 1  PART 1
    base_url <- "http://127.0.0.1:8080/api/"


    ###     MEDIO DE CONTACTO    ###

    full_url_folio <- base::paste0(base_url, "folio")


    #api call
    api_call_folio <- httr::GET(full_url_folio)
  

    #retrieving json file
    folios_json <- jsonlite::fromJSON(full_url_folio)


    #retrieving api's response leaving the status out
    folios <- folios_json$response


    #counting id's from folios' column id_contacto_catalogo
    num_presencial <- sum(ifelse(folios$id_contacto_catalogo == "1", 1, 0))

    num_telefonico <- sum(ifelse(folios$id_contacto_catalogo == "2", 1, 0))

    num_redes_sociales <- sum(ifelse(folios$id_contacto_catalogo == "3", 1, 0))

    num_otro <- sum(ifelse(folios$id_contacto_catalogo == "4", 1, 0))


    folio_contacto <- data.frame(
    category = c("Presencial","Telefónico","Redes Sociales","Otro"), 
    value = c(num_presencial, num_presencial, num_redes_sociales, num_otro)
    )


    output$contacto <- renderPlot({
        # Compute percentages
        folio_contacto$fraction <- folio_contacto$value / sum(folio_contacto$value)

        # Compute the cumulative percentages (top of each rectangle)
        folio_contacto$ymax <- cumsum(folio_contacto$fraction)

        # Compute the bottom of each rectangle
        folio_contacto$ymin <- c(0, head(folio_contacto$ymax, n=-1))

        # Compute label position
        folio_contacto$labelPosition <- (folio_contacto$ymax + folio_contacto$ymin) / 2

        # Compute a good label
        folio_contacto$label <- paste0(folio_contacto$category, "\n valor: ", folio_contacto$value)

        # Make the plot
        ggplot(folio_contacto, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
        geom_rect() +
        geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
        scale_fill_brewer(palette=3) +
        coord_polar(theta="y") +
        xlim(c(2, 4)) +
        theme_void() +
        theme(legend.position = "none")
    })


    ###     ¿La persona que contacta es quien requiere el servicio?    ###













    ###     ¿Cómo se enteró de los servicios de acompañamiento emocional?    ###

    full_url_persona <- base::paste0(base_url, "persona")

    #api call
    api_call_persona <- httr::GET(full_url_persona)

    #retrieving json file
    personas_json <- jsonlite::fromJSON(full_url_persona)

    #retrieving api's response leaving the status out
    personas <- personas_json$response


    #counting ids from personas' column idComoSeEntero
    cont_redes_sociales <- sum(ifelse(personas$idComoSeEntero == "1", 1, 0))

    cont_recomendacion_familiar <- sum(ifelse(personas$idComoSeEntero == "2", 1, 0))

    cont_recomendacion_colab <- sum(ifelse(personas$idComoSeEntero == "3", 1, 0))

    cont_instancia <- sum(ifelse(personas$idComoSeEntero == "4", 1, 0))

    cont_asociacion <- sum(ifelse(personas$idComoSeEntero == "5", 1, 0))

    cont_otro <- sum(ifelse(personas$idComoSeEntero == "6", 1, 0))



    servicio_acomp_emocional_data <- data.frame(
        category = c("Redes Sociales","Recomendación de algún familiar","Recomendación de colaborador CIAM","Instancia gubernamental","Asociación civil","Otra"),
        value = c(cont_redes_sociales, cont_recomendacion_familiar, cont_recomendacion_colab, cont_instancia, cont_asociacion, cont_otro)
    )


    output$serv_acomp_emocional <- renderPlot({
       ggplot(servicio_acomp_emocional_data, aes(x=category, y=value)) + 
       geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) + 
       xlab("Categoría") +
       ylab("Frecuencia")
    })




    ########################        DASHBOARD 1  PART 2

























    ########################        DASHBOARD 1  PART 3
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
