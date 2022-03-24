library(shiny)
library(dplyr)
library(ggplot2)
#library(plotly)
library(jsonlite)
library(httr)

    

server <- function(input, output, session) {
    # llamada tablas
    # base_url <- "http://127.0.0.1:8000/api/" # url raiz
    base_url <- "http://127.0.0.1:8080/api/" # url raiz

    # urls
    full_url_folio <- base::paste0(base_url, "folio")
    full_url_medio_contacto <- base::paste0(base_url, "medio_contacto")
    full_url_persona <- base::paste0(base_url, "persona")
    full_url_como_se_entero_cat <- base::paste0(base_url, "como_se_entero")
    full_url_canalizacion_seguimiento <- base::paste0(base_url, "canalizacion_seguimiento")
    full_url_instancia <- base::paste0(base_url, "instancia")

    #api call
    api_call_folio <- httr::GET(full_url_folio)
    api_call_medio_contacto <- httr::GET(full_url_medio_contacto)
    api_call_persona <- httr::GET(full_url_persona)
    api_call_como_se_entero_cat <- httr::GET(full_url_como_se_entero_cat)

    #retrieving json file
    folios_json <- jsonlite::fromJSON(full_url_folio)
    medio_contacto_json <- jsonlite::fromJSON(full_url_medio_contacto)
    personas_json <- jsonlite::fromJSON(full_url_persona)
    como_se_entero_catalogo_json <- jsonlite::fromJSON(full_url_como_se_entero_cat)
    canalizacion_seguimiento_json <- jsonlite::fromJSON(full_url_canalizacion_seguimiento)
    instancia_json <- jsonlite::fromJSON(full_url_instancia)

    #retrieving api's response leaving the status out
    folios <- folios_json$response
    medio_contacto <- medio_contacto_json$response
    personas <- personas_json$response
    como_se_entero_catalogo <- como_se_entero_catalogo_json$response
    canalizacion_seguimiento <- canalizacion_seguimiento_json$response
    instancia <- instancia_json$response




    ########################        DASHBOARD 1  PART 1
    ###     MEDIO DE CONTACTO    ###

    # cambio de id para poder hacer merge con catalogo
    colnames(folios)[3] <- "id_medio_contacto"
    # solo por cambiar título de columna nombra a medio contacto
    colnames(medio_contacto)[2] <- "medio_contacto"
    # hacemos merge
    foliosxmedioc <- merge(folios,medio_contacto)
    # count por tipo de medio contacto
    folio_contacto <- foliosxmedioc %>% count(medio_contacto)

    output$contacto <- renderPlot({
        # Compute percentages
        folio_contacto$fraction <- folio_contacto$n / sum(folio_contacto$n)

        # Compute the cumulative percentages (top of each rectangle)
        folio_contacto$ymax <- cumsum(folio_contacto$fraction)

        # Compute the bottom of each rectangle
        folio_contacto$ymin <- c(0, head(folio_contacto$ymax, n=-1))

        # Compute label position
        folio_contacto$labelPosition <- (folio_contacto$ymax + folio_contacto$ymin) / 2

        # Compute a good label
        folio_contacto$label <- paste0(folio_contacto$medio_contacto, "\n valor: ", folio_contacto$n)

        # Make the plot
        ggplot(folio_contacto, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=medio_contacto)) +
        geom_rect() +
        geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
        scale_fill_brewer(palette=3) +
        coord_polar(theta="y") +
        xlim(c(2, 4)) +
        theme_void() +
        theme(legend.position = "none")
    })


    ###     ¿La persona que contacta es quien requiere el servicio?    ###
    #counting id's from folios' column id_contacto_catalogo

    persona_requiere_servicio_count <- sum(!is.na(folios$filtro_si))
    persona_otra_requiere_servicio_count <- sum(!is.na(folios$filtro_no))

    persona_solicita_servicio <- data.frame(category = c("Si", "No"), value = c(persona_requiere_servicio_count, persona_otra_requiere_servicio_count))

    output$persona_servicio <- renderPlotly({ggplotly(
        ggplot(persona_solicita_servicio, aes(x=category, y=value, fill = category)) +
        geom_bar(stat="identity") + 
            theme(axis.text.x = element_text(angle = 60, hjust=1)) +
            xlab("Categoría") +
            ylab("Frecuencia"))

    })



    ###     ¿Cómo se enteró de los servicios de acompañamiento emocional?    ###
    #counting ids from personas' column idComoSeEntero
    colnames(personas)[16] <- "id_como_se_entero"
    colnames(como_se_entero_catalogo)[2] <- "como_se_entero"

    servicio_acomp_emocional_data <- merge(personas,como_se_entero_catalogo)
    servicio_acomp_emocional_data <- servicio_acomp_emocional_data %>% count(como_se_entero)


    output$serv_acomp_emocional <- renderPlotly({
    ggplotly(
        ggplot(servicio_acomp_emocional_data, aes(x=como_se_entero, y=n, fill = como_se_entero)) + 
        geom_bar(stat="identity", alpha=.6, width=.4) + 
        theme(axis.text.x = element_text(angle = 60, hjust=1)) +
        xlab("Categoría") +
        ylab("Frecuencia"))
    })




    ########################        DASHBOARD 1  PART 2
    output$canalizacionxAnterior <- renderPlotly({
        canalizacion_anterior_si_count <- sum(!is.na(canalizacion_seguimiento$siCanalizacion))
        canalizacion_anterior_no_count <- sum(!is.na(canalizacion_seguimiento$noCanalizacion))

        respuestas_anterior <- data.frame(Anterior = c("Si", "No"), value = c(canalizacion_anterior_si_count, canalizacion_anterior_no_count))

        ggplotly(
        ggplot(respuestas_anterior, aes(x=Anterior, y=value, fill = Anterior)) +
        geom_bar(stat="identity") + 
            theme(axis.text.x = element_text(angle = 0, hjust=1)) +
            xlab("Respuesta")+
            scale_fill_manual(values=c('#56267d', '#2AB7CD')))
    })

    output$canalizacionxSeguimiento <- renderPlotly({
        canalizacion_Seguimiento_si_count <- sum(!is.na(canalizacion_seguimiento$siSeguimiento))
        canalizacion_Seguimiento_no_count <- sum(!is.na(canalizacion_seguimiento$noSeguimiento))

        respuestas_Seguimiento <- data.frame(Seguimiento = c("Si", "No"), value = c(canalizacion_Seguimiento_si_count, canalizacion_Seguimiento_no_count))

        ggplotly(
        ggplot(respuestas_Seguimiento, aes(x=Seguimiento, y=value, fill = Seguimiento)) +
        geom_bar(stat="identity") + 
            theme(axis.text.x = element_text(angle = 0, hjust=1)) +
            xlab("Respuesta")+
            scale_fill_manual(values=c('#56267d', '#2AB7CD')))
    })

    output$canalizacionxAdecuado <- renderPlotly({
        canalizacion_Adecuado_si_count <- sum(!is.na(canalizacion_seguimiento$siAdecuado))
        canalizacion_Adecuado_no_count <- sum(!is.na(canalizacion_seguimiento$noAdecuado))

        respuestas_Adecuado <- data.frame(Adecuado = c("Si", "No"), value = c(canalizacion_Adecuado_si_count, canalizacion_Adecuado_no_count))

        ggplotly(
        ggplot(respuestas_Adecuado, aes(x=Adecuado, y=value, fill = Adecuado)) +
        geom_bar(stat="identity") + 
            theme(axis.text.x = element_text(angle = 0, hjust=1)) +
            xlab("Respuesta")+
            scale_fill_manual(values=c('#56267d', '#2AB7CD')))
    })

    output$canalizacionxOportuno <- renderPlotly({
        canalizacion_Oportuno_si_count <- sum(!is.na(canalizacion_seguimiento$siOportuno))
        canalizacion_Oportuno_no_count <- sum(!is.na(canalizacion_seguimiento$noOportuno))

        respuestas_Oportuno <- data.frame(Oportuno = c("Si", "No"), value = c(canalizacion_Oportuno_si_count, canalizacion_Oportuno_no_count))

        ggplotly(
        ggplot(respuestas_Oportuno, aes(x=Oportuno, y=value, fill = Oportuno)) +
        geom_bar(stat="identity") + 
            theme(axis.text.x = element_text(angle = 0, hjust=1)) +
            xlab("Respuesta")+
            scale_fill_manual(values=c('#56267d', '#2AB7CD')))
    })

    output$canalizacionxProntitud <- renderPlotly({
        canalizacion_Prontitud_si_count <- sum(!is.na(canalizacion_seguimiento$siProntitud))
        canalizacion_Prontitud_no_count <- sum(!is.na(canalizacion_seguimiento$noProntitud))

        respuestas_Prontitud <- data.frame(Prontitud = c("Si", "No"), value = c(canalizacion_Prontitud_si_count, canalizacion_Prontitud_no_count))

        ggplotly(
        ggplot(respuestas_Prontitud, aes(x=Prontitud, y=value, fill = Prontitud)) +
        geom_bar(stat="identity") + 
            theme(axis.text.x = element_text(angle = 0, hjust=1)) +
            xlab("Respuesta")+
            scale_fill_manual(values=c('#56267d', '#2AB7CD')))
    })

    output$canalizacionxConfianza <- renderPlotly({
        canalizacion_Confianza_si_count <- sum(!is.na(canalizacion_seguimiento$siConfianza))
        canalizacion_Confianza_no_count <- sum(!is.na(canalizacion_seguimiento$noConfianza))

        respuestas_Confianza <- data.frame(Confianza = c("Si", "No"), value = c(canalizacion_Confianza_si_count, canalizacion_Confianza_no_count))

        ggplotly(
        ggplot(respuestas_Confianza, aes(x=Confianza, y=value, fill = Confianza)) +
        geom_bar(stat="identity") + 
            theme(axis.text.x = element_text(angle = 0, hjust=1)) +
            xlab("Respuesta")+
            scale_fill_manual(values=c('#56267d', '#2AB7CD')))
    })

    output$canalizacionxRespeto <- renderPlotly({
        canalizacion_Respeto_si_count <- sum(!is.na(canalizacion_seguimiento$siRespeto))
        canalizacion_Respeto_no_count <- sum(!is.na(canalizacion_seguimiento$noRespeto))

        respuestas_Respeto <- data.frame(Respeto = c("Si", "No"), value = c(canalizacion_Respeto_si_count, canalizacion_Respeto_no_count))

        ggplotly(
        ggplot(respuestas_Respeto, aes(x=Respeto, y=value, fill = Respeto)) +
        geom_bar(stat="identity") + 
            theme(axis.text.x = element_text(angle = 0, hjust=1)) +
            xlab("Respuesta")+
            scale_fill_manual(values=c('#56267d', '#2AB7CD')))
    })



    ########################        DASHBOARD 1  PART 3
    # traemos la informacion
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
