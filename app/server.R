library(shiny)
library(dplyr)
library(ggplot2)
library(hrbrthemes)
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
    full_url_riesgo <- base::paste0(base_url, "riesgo")
    full_url_sexos <- base::paste0(base_url, "sexo")
    full_url_tipo_violencia <- base::paste0(base_url, "tipo_violencia")
    full_url_modalidad <- base::paste0(base_url, "modalidad")
    full_url_encuesta_satisfaccion <- base::paste0(base_url, "satisfaccion")

    #api call
    api_call_folio <- httr::GET(full_url_folio)
    api_call_medio_contacto <- httr::GET(full_url_medio_contacto)
    api_call_persona <- httr::GET(full_url_persona)
    api_call_como_se_entero_cat <- httr::GET(full_url_como_se_entero_cat)
    api_call_riesgo <- httr::GET(full_url_riesgo)
    api_call_sexos <- httr::GET(full_url_sexos)
    api_call_encuesta_satisfaccion <- httr::GET(full_url_encuesta_satisfaccion)


    #retrieving json file
    folios_json <- jsonlite::fromJSON(full_url_folio)
    medio_contacto_json <- jsonlite::fromJSON(full_url_medio_contacto)
    personas_json <- jsonlite::fromJSON(full_url_persona)
    como_se_entero_catalogo_json <- jsonlite::fromJSON(full_url_como_se_entero_cat)
    canalizacion_seguimiento_json <- jsonlite::fromJSON(full_url_canalizacion_seguimiento)
    instancia_json <- jsonlite::fromJSON(full_url_instancia)
    riesgo_json <- jsonlite::fromJSON(full_url_riesgo)
    sexos_json <- jsonlite::fromJSON(full_url_sexos)
    tipo_violencia_json <- jsonlite::fromJSON(full_url_tipo_violencia)
    modalidad_json <- jsonlite::fromJSON(full_url_modalidad)
    encuesta_satisfaccion_json <- jsonlite::fromJSON(full_url_encuesta_satisfaccion)


    #retrieving api's response leaving the status out
    #folios <- folios_json$response
    medio_contacto <- medio_contacto_json$response
    #personas <- personas_json$response
    como_se_entero_catalogo <- como_se_entero_catalogo_json$response
    #canalizacion_seguimiento <- canalizacion_seguimiento_json$response
    instancia <- instancia_json$response
    #riesgos <- riesgo_json$response
    sexos <- sexos_json$response
    tipo_violencia <- tipo_violencia_json$response
    modalidad <- modalidad_json$response
    #encuesta_satisfaccion <- encuesta_satisfaccion_json$response

    #######################     Filtro de fechas ####################
    foliosF <- reactive({
        folios_json$response %>% 
            filter(between(as.Date(fecha), input$dateRange[1], input$dateRange[2]))
    })
    personasF <- reactive({
        personas_json$response %>% filter(id_folio %in% foliosF()$id_folio)
    })
    canalizacion_seguimientoF <- reactive({
        canalizacion_seguimiento_json$response %>% 
            filter(id_folio %in% foliosF()$id_folio)
    })
    riesgosF <- reactive({
        riesgo_json$response %>% 
            filter(id_folio %in% foliosF()$id_folio)
    })
    encuesta_satisfaccionF <- reactive({
        encuesta_satisfaccion_json$response %>% 
            filter(id_folio %in% foliosF()$id_folio)
    })

    ########################        DASHBOARD 1  PART 1
    ###     MEDIO DE CONTACTO    ###

    output$contacto <- renderPlot({
        folios <- foliosF()
        # cambio de id para poder hacer merge con catalogo
        colnames(folios)[3] <- "id_medio_contacto"
        # solo por cambiar título de columna nombra a medio contacto
        colnames(medio_contacto)[2] <- "medio_contacto"
        # hacemos merge
        foliosxmedioc <- merge(folios,medio_contacto)
        # count por tipo de medio contacto
        folio_contacto <- foliosxmedioc %>% count(medio_contacto)

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
    output$persona_servicio <- renderPlotly({
        folios <- foliosF()
        persona_requiere_servicio_count <- sum(!is.na(folios$filtro_si))
        persona_otra_requiere_servicio_count <- sum(!is.na(folios$filtro_no))

        persona_solicita_servicio <- data.frame(category = c("Si", "No"), value = c(persona_requiere_servicio_count, persona_otra_requiere_servicio_count))

        ggplotly(
        ggplot(persona_solicita_servicio, aes(x=category, y=value, fill = category)) +
        geom_bar(stat="identity") + 
            theme(axis.text.x = element_text(angle = 60, hjust=1)) +
            xlab("Categoría") +
            ylab("Frecuencia") +
            theme(legend.position = "none"), tooltip = "text")
    })



    ###     ¿Cómo se enteró de los servicios de acompañamiento emocional?    ###
    #counting ids from personas' column idComoSeEntero

    output$serv_acomp_emocional <- renderPlotly({
        personas <- personasF()
        colnames(personas)[16] <- "id_como_se_entero"
        colnames(como_se_entero_catalogo)[2] <- "como_se_entero"

        servicio_acomp_emocional_data <- merge(personas, como_se_entero_catalogo)
        servicio_acomp_emocional_data <- servicio_acomp_emocional_data %>% count(como_se_entero)
    ggplotly(
        ggplot(servicio_acomp_emocional_data, aes(x=como_se_entero, y=n, fill = como_se_entero)) + 
        geom_bar(stat="identity", alpha=.6, width=.4) + 
        xlab("Categoría") +
        ylab("Frecuencia") +
        theme(axis.ticks.x = element_blank(),
            axis.text.x = element_blank()) +
            labs(fill = ""))
    })




    ########################        DASHBOARD 1  PART 2
    output$canalizacionxAnterior <- renderPlotly({
        canalizacion_seguimiento <- canalizacion_seguimientoF()
        canalizacion_anterior_si_count <- sum(!is.na(canalizacion_seguimiento$siCanalizacion))
        canalizacion_anterior_no_count <- sum(!is.na(canalizacion_seguimiento$noCanalizacion))

        respuestas_anterior <- data.frame(Anterior = c("Si", "No"), value = c(canalizacion_anterior_si_count, canalizacion_anterior_no_count))

        ggplotly(
        ggplot(respuestas_anterior, aes(x=Anterior, y=value, fill = Anterior)) +
        geom_bar(stat="identity") + 
            xlab("Respuesta")+
            scale_fill_manual(values=c('#56267d', '#2AB7CD')) + 
            theme(axis.ticks.x = element_blank(),
            axis.text.x = element_blank()) +
            labs(fill = ""))
    })

    output$canalizacionxSeguimiento <- renderPlotly({
        canalizacion_seguimiento <- canalizacion_seguimientoF()
        canalizacion_Seguimiento_si_count <- sum(!is.na(canalizacion_seguimiento$siSeguimiento))
        canalizacion_Seguimiento_no_count <- sum(!is.na(canalizacion_seguimiento$noSeguimiento))

        respuestas_Seguimiento <- data.frame(Seguimiento = c("Si", "No"), value = c(canalizacion_Seguimiento_si_count, canalizacion_Seguimiento_no_count))

        ggplotly(
        ggplot(respuestas_Seguimiento, aes(x=Seguimiento, y=value, fill = Seguimiento)) +
        geom_bar(stat="identity") + 
            xlab("Respuesta")+
            scale_fill_manual(values=c('#56267d', '#2AB7CD')) +
            theme(axis.ticks.x = element_blank(),
            axis.text.x = element_blank()) +
            labs(fill = ""))
    })

    output$canalizacionxAdecuado <- renderPlotly({
        canalizacion_seguimiento <- canalizacion_seguimientoF()
        canalizacion_Adecuado_si_count <- sum(!is.na(canalizacion_seguimiento$siAdecuado))
        canalizacion_Adecuado_no_count <- sum(!is.na(canalizacion_seguimiento$noAdecuado))

        respuestas_Adecuado <- data.frame(Adecuado = c("Si", "No"), value = c(canalizacion_Adecuado_si_count, canalizacion_Adecuado_no_count))

        ggplotly(
        ggplot(respuestas_Adecuado, aes(x=Adecuado, y=value, fill = Adecuado)) +
        geom_bar(stat="identity") + 
            xlab("Respuesta")+
            scale_fill_manual(values=c('#56267d', '#2AB7CD')) +
            theme(axis.ticks.x = element_blank(),
            axis.text.x = element_blank()) +
            labs(fill = ""))
    })

    output$canalizacionxOportuno <- renderPlotly({
        canalizacion_seguimiento <- canalizacion_seguimientoF()
        canalizacion_Oportuno_si_count <- sum(!is.na(canalizacion_seguimiento$siOportuno))
        canalizacion_Oportuno_no_count <- sum(!is.na(canalizacion_seguimiento$noOportuno))

        respuestas_Oportuno <- data.frame(Oportuno = c("Si", "No"), value = c(canalizacion_Oportuno_si_count, canalizacion_Oportuno_no_count))

        ggplotly(
        ggplot(respuestas_Oportuno, aes(x=Oportuno, y=value, fill = Oportuno)) +
        geom_bar(stat="identity") + 
            xlab("Respuesta")+
            scale_fill_manual(values=c('#56267d', '#2AB7CD')) +
            theme(axis.ticks.x = element_blank(),
            axis.text.x = element_blank()) +
            labs(fill = ""))
    })

    output$canalizacionxProntitud <- renderPlotly({
        canalizacion_seguimiento <- canalizacion_seguimientoF()
        canalizacion_Prontitud_si_count <- sum(!is.na(canalizacion_seguimiento$siProntitud))
        canalizacion_Prontitud_no_count <- sum(!is.na(canalizacion_seguimiento$noProntitud))

        respuestas_Prontitud <- data.frame(Prontitud = c("Si", "No"), value = c(canalizacion_Prontitud_si_count, canalizacion_Prontitud_no_count))

        ggplotly(
        ggplot(respuestas_Prontitud, aes(x=Prontitud, y=value, fill = Prontitud)) +
        geom_bar(stat="identity") + 
            xlab("Respuesta")+
            scale_fill_manual(values=c('#56267d', '#2AB7CD')) +
            theme(axis.ticks.x = element_blank(),
            axis.text.x = element_blank()) +
            labs(fill = ""))
    })

    output$canalizacionxConfianza <- renderPlotly({
        canalizacion_seguimiento <- canalizacion_seguimientoF()
        canalizacion_Confianza_si_count <- sum(!is.na(canalizacion_seguimiento$siConfianza))
        canalizacion_Confianza_no_count <- sum(!is.na(canalizacion_seguimiento$noConfianza))

        respuestas_Confianza <- data.frame(Confianza = c("Si", "No"), value = c(canalizacion_Confianza_si_count, canalizacion_Confianza_no_count))

        ggplotly(
        ggplot(respuestas_Confianza, aes(x=Confianza, y=value, fill = Confianza)) +
        geom_bar(stat="identity") + 
            xlab("Respuesta")+
            scale_fill_manual(values=c('#56267d', '#2AB7CD')) +
            theme(axis.ticks.x = element_blank(),
            axis.text.x = element_blank()) +
            labs(fill = ""))
    })

    output$canalizacionxRespeto <- renderPlotly({
        canalizacion_seguimiento <- canalizacion_seguimientoF()
        canalizacion_Respeto_si_count <- sum(!is.na(canalizacion_seguimiento$siRespeto))
        canalizacion_Respeto_no_count <- sum(!is.na(canalizacion_seguimiento$noRespeto))

        respuestas_Respeto <- data.frame(Respeto = c("Si", "No"), value = c(canalizacion_Respeto_si_count, canalizacion_Respeto_no_count))

        ggplotly(
        ggplot(respuestas_Respeto, aes(x=Respeto, y=value, fill = Respeto)) +
        geom_bar(stat="identity") + 
            xlab("Respuesta")+
            scale_fill_manual(values=c('#56267d', '#2AB7CD')) +
            theme(axis.ticks.x = element_blank(),
            axis.text.x = element_blank()) +
            labs(fill = ""))
    })



    ########################        DASHBOARD 1  PART 3
    # traemos la informacion
    output$calificacionesxinstancia <- renderPlotly({
        canalizacion_seguimiento <- canalizacion_seguimientoF()

        colnames(canalizacion_seguimiento)[17] <- "id_instancia"
        # seleccionamos las columnas de interes
        califxinstancia <- canalizacion_seguimiento %>%
        select("id_canalizacion_seguimiento", "calificacion", "id_instancia")
        colnames(instancia)[2] <- "Instancia"
        # hacemos el merge a traves de id_instancia
        califxinstancia <- merge(califxinstancia, instancia)
        califxinstancia$calificacion <- as.numeric(califxinstancia$calificacion)
        
        califpromxinstancia <- califxinstancia %>% group_by(Instancia) %>%
            summarize(Promedio = round(mean(calificacion), 2), Conteo = n(), Min = min(calificacion), Max = max(calificacion)) %>%
            arrange(desc(Promedio)) %>%
            mutate(Instancia = factor(Instancia, levels = Instancia))

        todasInstanciasCalif <- left_join(instancia,califpromxinstancia)
        todasInstanciasCalif[is.na(todasInstanciasCalif)] <- 0
        
        # graficamos
        ggplotly(ggplot(todasInstanciasCalif, aes(x = reorder(Instancia,-Promedio), y = Promedio, fill = Instancia)) +
            geom_bar(stat = "identity", aes(text=sprintf("Instancia: %s<br>Promedio: %s<br>Conteo: %s<br>Calificación mínima: %s<br>Calificación máxima: %s", Instancia, Promedio, Conteo, Min, Max))) +
            theme(axis.title.x = element_blank(),
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank()), tooltip = "text")
    })



    ########################        DASHBOARD 2

    ## SECTION A 
    # Violencia experimentada
    colnames(tipo_violencia)[2] <- "tipo_violencia"
    colnames(modalidad)[2] <- "modalidad"

    # histgrama de tipos de violencia
    output$hist_tipo_violencia_anterior <- renderPlotly({
        violencia_experimentada <- personasF()[,c("id_folio","tipoViolenciaPersona")]
        colnames(violencia_experimentada)[2] <- "id_tipo_violencia"

        tipo_violencia_experimentada <- violencia_experimentada %>% count(id_tipo_violencia)
        tipo_violencia_experimentada <- merge(tipo_violencia_experimentada,tipo_violencia)
        tipo_violencia_experimentada <- right_join(tipo_violencia_experimentada,tipo_violencia)
        tipo_violencia_experimentada[is.na(tipo_violencia_experimentada)] <- 0

        ggplotly(
            ggplot(tipo_violencia_experimentada, aes(x = reorder(tipo_violencia,-n) ,y = n)) +
            geom_bar(stat="identity", color = '#56267d', fill = '#56267d', aes(text=sprintf("Tipo de violencia: %s<br>Frecuencia: %s", tipo_violencia, n))) + 
            xlab("Tipos de Violencia") +
            ylab("Frecuencia") +
            labs(fill = ""), tooltip = "text")
    })

    # histograma de modalidad
    output$hist_modalidad_anterior <- renderPlotly({
        violencia_experimentada <- personasF()[,c("id_folio","modalidadPersona")]
        colnames(violencia_experimentada)[2] <- "id_modalidad"

        modalidad_experimentada <- violencia_experimentada %>% count(id_modalidad)
        modalidad_experimentada <- merge(modalidad_experimentada,modalidad)
        modalidad_experimentada <- right_join(modalidad_experimentada,modalidad)
        modalidad_experimentada[is.na(modalidad_experimentada)] <- 0

        ggplotly(
            ggplot(modalidad_experimentada, aes(x = reorder(modalidad,-n) ,y = n)) +
            geom_bar(stat="identity", color = '#56267d', fill = '#56267d', aes(text=sprintf("Modalidad: %s<br>Frecuencia: %s", modalidad, n))) +
            xlab("Modalidad") +
            ylab("Frecuencia") +
            labs(fill = ""), tooltip = "text")
    })

    # histograma combinado violencia vs modalidad
    output$hist_tipo_vs_modalidad_anterior <- renderPlotly({
        violencia_experimentada <- personasF()[,c("id_folio","tipoViolenciaPersona","modalidadPersona")]
        colnames(violencia_experimentada)[2] <- "id_tipo_violencia"
        colnames(violencia_experimentada)[3] <- "id_modalidad"

        count_tv_vs_m <- violencia_experimentada %>% group_by(id_tipo_violencia,id_modalidad) %>% 
            mutate(n = n())

        tv_vs_m <- merge(count_tv_vs_m, tipo_violencia)
        tv_vs_m <- merge(tv_vs_m, modalidad)
        
        ggplotly(
            ggplot(tv_vs_m, aes(x = tipo_violencia, fill = modalidad)) +
            geom_bar(position="dodge", aes(text=sprintf("Tipo de violencia: %s<br>Modalidad: %s<br>Frecuencia: %s", tipo_violencia, modalidad, n))) + 
            xlab("Tipos de Violencia") +
            ylab("Frecuencia") +
            theme(axis.ticks.x = element_blank()), tooltip = "text")
    })

    ## SECTION B
    # Violencia actual

    # histgrama de tipos de violencia
    output$hist_tipo_violencia_actual <- renderPlotly({
        violencia_actual <- riesgosF()[,c("id_folio","violenciaRiesgo")]
        colnames(violencia_actual)[2] <- "id_tipo_violencia"

        tipo_violencia_actual <- violencia_actual %>% count(id_tipo_violencia)
        tipo_violencia_actual <- merge(tipo_violencia_actual,tipo_violencia)
        tipo_violencia_actual <- right_join(tipo_violencia_actual,tipo_violencia)
        tipo_violencia_actual[is.na(tipo_violencia_actual)] <- 0

        ggplotly(
            ggplot(tipo_violencia_actual, aes(x = reorder(tipo_violencia,-n) ,y = n)) +
            geom_bar(stat="identity", color = '#56267d', fill = '#56267d', aes(text=sprintf("Tipo de violencia: %s<br>Frecuencia: %s", tipo_violencia, n))) + 
            xlab("Tipos de Violencia") +
            ylab("Frecuencia") +
            labs(fill = ""), tooltip = "text")
    })

    # histograma de modalidad
    output$hist_modalidad_actual <- renderPlotly({
        violencia_actual <- riesgosF()[,c("id_folio","modalidadRiesgo")]
        colnames(violencia_actual)[2] <- "id_modalidad"

        modalidad_actual <- violencia_actual %>% count(id_modalidad)
        modalidad_actual <- merge(modalidad_actual,modalidad)
        modalidad_actual <- right_join(modalidad_actual,modalidad)
        modalidad_actual[is.na(modalidad_actual)] <- 0

        ggplotly(
            ggplot(modalidad_actual, aes(x = reorder(modalidad,-n) ,y = n)) +
            geom_bar(stat="identity", color = '#56267d', fill = '#56267d', aes(text=sprintf("Modalidad: %s<br>Frecuencia: %s", modalidad, n))) +
            xlab("Modalidad") +
            ylab("Frecuencia") +
            labs(fill = ""), tooltip = "text")
    })

    # histograma combinado violencia vs modalidad
    output$hist_tipo_vs_modalidad_actual <- renderPlotly({
        violencia_actual <- riesgosF()[,c("id_folio","violenciaRiesgo","modalidadRiesgo")]
        colnames(violencia_actual)[2] <- "id_tipo_violencia"
        colnames(violencia_actual)[3] <- "id_modalidad"

        count_tv_vs_m_actual <- violencia_actual %>% group_by(id_tipo_violencia,id_modalidad) %>%
            mutate(n = n())

        tv_vs_m_actual <- merge(count_tv_vs_m_actual,tipo_violencia)
        tv_vs_m_actual <- merge(tv_vs_m_actual,modalidad)

        ggplotly(
            ggplot(tv_vs_m_actual, aes(x = tipo_violencia, fill = modalidad)) +
            geom_bar(position="dodge", aes(text=sprintf("Tipo de violencia: %s<br>Modalidad: %s<br>Frecuencia: %s", tipo_violencia, modalidad, n))) + 
            xlab("Tipos de Violencia") +
            ylab("Frecuencia") +
            theme(axis.ticks.x = element_blank()), tooltip = "text")
    })

    ## SECTION C
    output$personasxEdad <- renderPlotly({
        personas <- personasF()
        countEdad <- personas %>% count(personas$edadPersona)
        colnames(countEdad)[1] <- "edad"
        respuestas_Edad <- data.frame(Edad = c(countEdad$edad), value = c(countEdad$n))

        ggplotly(
        ggplot(respuestas_Edad, aes(x=Edad, y=value, fill = Edad)) +
        geom_bar(stat="identity") + 
            theme(axis.text.x = element_text(angle = 0, hjust=1)) +
            xlab("Edades")+
            coord_flip()
        )
    })

    output$personasxLGBT <- renderPlotly({
        personas <- personasF()
        personas_LGBT_si <- sum(!is.na(personas$siLGBPersona))
        personas_LGBT_no <- sum(!is.na(personas$noLGBPersona))

        respuestas_LGBT <- data.frame(LGBT = c("Si", "No"), value = c(personas_LGBT_si, personas_LGBT_no))

        ggplotly(
        ggplot(respuestas_LGBT, aes(x=LGBT, y=value, fill = LGBT)) +
        geom_bar(stat="identity") + 
            xlab("Respuesta")+
            scale_fill_manual(values=c('#56267d', '#2AB7CD')) +
            theme(axis.ticks.x = element_blank(),
            axis.text.x = element_blank()) +
            labs(fill = ""))
    })

    output$personasxSexo <- renderPlotly({
        personas <- personasF()
        colnames(sexos)[2] <- "sexoPersona"
        colnames(personas)[3] <- "id_sexo"
        sexo <- sexos %>% group_by(id_sexo) %>% slice(1)
        sexos_data <- merge(personas, sexo, by="id_sexo")

        countSexo <- sexos_data %>% count(sexoPersona)
        colnames(countSexo)[1] <- "sexo"

        respuestas_Sexo <- data.frame(Sexo = c(countSexo$sexo), value = c(countSexo$n))

        ggplotly(
        ggplot(respuestas_Sexo, aes(x=Sexo, y=value, fill = Sexo)) +
        geom_bar(stat="identity") + 
            xlab("Identidad") +
            theme(axis.ticks.x = element_blank(),
            axis.text.x = element_blank()) +
            labs(fill = "")
        )
    })

    output$personasxLocalidad <- renderPlotly({
        personas <- personasF()
        countLoc <- personas %>% count(personas$localidadResidenciaPersona)
        colnames(countLoc)[1] <- "locations"

        respuestas_Localidad <- data.frame(Localidad = c(countLoc$locations), value = c(countLoc$n))

        ggplotly(
        ggplot(respuestas_Localidad, aes(x=Localidad, y=value, fill = Localidad)) +
        geom_bar(stat="identity") + 
            xlab("Localidades") +
            coord_flip() +
            theme(axis.ticks.x = element_blank(),
            axis.text.x = element_blank()) +
            labs(fill = "")
        )
    })

    output$personasxEstado <- renderPlotly({
        personas <- personasF()
        countEst <- personas %>% count(personas$estadoResidenciaPersona)
        colnames(countEst)[1] <- "locations"

        respuestas_Estado <- data.frame(Estado = c(countEst$locations), value = c(countEst$n))

        ggplotly(
        ggplot(respuestas_Estado, aes(x=Estado, y=value, fill = Estado)) +
        geom_bar(stat="identity") + 
            xlab("Estados")+
            coord_flip() +
            theme(axis.ticks.x = element_blank(),
            axis.text.x = element_blank()) +
            labs(fill = "")
        )
    })

    output$personasxPais <- renderPlotly({
        personas <- personasF()
        countPais <- personas %>% count(personas$paisResidenciaPersona)
        colnames(countPais)[1] <- "locations"

        respuestas_Pais <- data.frame(Pais = c(countPais$locations), value = c(countPais$n))

        ggplotly(
        ggplot(respuestas_Pais, aes(x=Pais, y=value, fill = Pais)) +
        geom_bar(stat="identity") + 
            xlab("Localidades")+
            coord_flip() +
            theme(axis.ticks.x = element_blank(),
            axis.text.x = element_blank()) +
            labs(fill = "")
        )
    })

    output$personasxDiscapacidad <- renderPlotly({
        personas <- personasF()
        personas_Discapacidad_si <- sum(!is.na(personas$siDiscapacidadPersona))
        personas_Discapacidad_no <- sum(!is.na(personas$noDiscapacidadPersona))

        respuestas_Discapacidad <- data.frame(Discapacidad = c("Si", "No"), value = c(personas_Discapacidad_si, personas_Discapacidad_no))

        ggplotly(
        ggplot(respuestas_Discapacidad, aes(x=Discapacidad, y=value, fill = Discapacidad)) +
        geom_bar(stat="identity") + 
            theme(axis.text.x = element_text(angle = 0, hjust=1)) +
            xlab("Respuesta")+
            scale_fill_manual(values=c('#56267d', '#2AB7CD')))
    })

    output$personasxPueblos <- renderPlotly({
        personas <- personasF()
        personas_Pueblos_si <- sum(!is.na(personas$siPueblosPersona))
        personas_Pueblos_no <- sum(!is.na(personas$noPueblosPersona))

        respuestas_Pueblos <- data.frame(Pueblos = c("Si", "No"), value = c(personas_Pueblos_si, personas_Pueblos_no))

        ggplotly(
        ggplot(respuestas_Pueblos, aes(x=Pueblos, y=value, fill = Pueblos)) +
        geom_bar(stat="identity") + 
            theme(axis.text.x = element_text(angle = 0, hjust=1)) +
            xlab("Respuesta")+
            scale_fill_manual(values=c('#56267d', '#2AB7CD')))
    })

    output$personasxLengua_Indigena <- renderPlotly({
        personas <- personasF()
        personas_Lengua_Indigena_si <- sum(!is.na(personas$siLenguaIndigenaPersona))
        personas_Lengua_Indigena_no <- sum(!is.na(personas$noLenguaIndigenaPersona))

        respuestas_Lengua_Indigena <- data.frame(Lengua_Indigena = c("Si", "No"), value = c(personas_Lengua_Indigena_si, personas_Lengua_Indigena_no))

        ggplotly(
        ggplot(respuestas_Lengua_Indigena, aes(x=Lengua_Indigena, y=value, fill = Lengua_Indigena)) +
        geom_bar(stat="identity") + 
            theme(axis.text.x = element_text(angle = 0, hjust=1)) +
            xlab("Respuesta")+
            scale_fill_manual(values=c('#56267d', '#2AB7CD')))
    })

    output$personasxServicio <- renderPlotly({
        personas <- personasF()
        primerServ <- count(filter(personas, servicioPersona == 1))
        segundoServ <- count(filter(personas, servicioPersona == 2))
        tercerServ <- count(filter(personas, servicioPersona == 3))
        otrosServ <- count(filter(personas, servicioPersona > 3))
        
        respuestas_Tipo_Servicio <- data.frame(Tipo_Servicio = c("Atención en crisis", "Asesoría psicológica", "Canalización", "Otros"), value = c(primerServ$n,segundoServ$n,tercerServ$n,otrosServ$n))
        
        ggplotly(
            ggplot(respuestas_Tipo_Servicio, aes(x=Tipo_Servicio, y=value, fill = Tipo_Servicio)) +
            geom_bar(stat="identity") + 
            ylab("Personas") +
            xlab("Servicios") +
            theme(axis.ticks.x = element_blank(),
            axis.text.x = element_blank()) +
            labs(fill = "")
        )
    })
    
    ## SECTION D
    ### gráfica de edad del agresor
    output$edades_agresor_grf <- renderPlotly({
        riesgos <- riesgosF()
        ## cambio de id y renomabriemnto de columna
        colnames(riesgos)[4] <- "edad_agresor"

        ## obtener rangos de edades de los agresores
        riesgos$edad_agresor <- as.numeric(riesgos$edad_agresor)

        labels_rangos <- data.frame("rango_edades" = c("0 a 5","6 a 12","13 a 18","19 a 25","26 a 29","30 a 59","más de 60"))
        rango_edades_agresor <- cut(riesgos$edad_agresor, breaks = c(-1,5,12,18,25,29,59,Inf), labels = labels_rangos$rango_edades)

        ## añadir los rangos de edades de los agresores a la tabla riesgos
        riesgos$rango_edades_agresor <- rango_edades_agresor

        #filtrado de NAs
        edadesAgresor <- riesgos %>% select(rango_edades_agresor) %>%
        filter(!is.na(rango_edades_agresor))

        rangos_edades_agresor <- edadesAgresor %>% group_by(rango_edades_agresor) %>% summarise(n = n())

        ggplotly(
            ggplot(rangos_edades_agresor, aes(x = rango_edades_agresor, y = n, fill = rango_edades_agresor)) +
            geom_bar(stat = "identity") +
            xlab("Edad Agresor") +
            ylab("Frecuencia") +
            theme(axis.ticks.x = element_blank(),
            axis.text.x = element_blank()) +
            labs(fill = ""))
    })

    ### gráfica de sexo del agresor
    output$sexo_agresor_grf <- renderPlotly({
        riesgos <- riesgosF()
        colnames(riesgos)[5] <- "id_sexo"
        colnames(sexos)[2] <- "sexos_agresor_catalogo"

        ### conteo de los datos de la columna id_sexo
        conteo_sexos_agresor <- riesgos %>% count(id_sexo)

        ### unión de las tablas riesgos y sexos por el campo id_sexo
        sexos_agresor_data <- merge(conteo_sexos_agresor, sexos)

        ggplotly(
            ggplot(sexos_agresor_data, aes(x = sexos_agresor_catalogo, y = n, fill = sexos_agresor_catalogo)) +
            geom_bar(stat = "identity") + 
            xlab("Sexo del Agresor") +
            ylab("Frecuencia") +
            theme(axis.ticks.x = element_blank(),
            axis.text.x = element_blank()) +
            labs(fill = "")
        )
    })

    ### gráfica: ¿La dirección del agresor/a es la misma de quien solicita el servicio?

    output$misma_dir_agresor_victima <- renderPlotly({
        riesgos <- riesgosF()
        dir <- riesgos[c("siDireccionAgresor","noDireccionAgresor")]

        count_si_dirs <- dir %>% filter(!is.na(siDireccionAgresor)) %>% count(siDireccionAgresor)
        count_no_dirs <- dir %>% filter(!is.na(noDireccionAgresor)) %>% count(noDireccionAgresor)

        count_total <- data.frame("Misma_direccion" = c("Si","No"),
                                "n" = c(count_si_dirs$n,count_no_dirs$n))

        ggplotly(
            ggplot(count_total,aes(x=Misma_direccion, y=n, fill=Misma_direccion)) +
                geom_bar(stat = "identity") +
                xlab("Misma dirección del agresor de quien solicita el servicio") +
                ylab("Frecuencia") + 
                theme(axis.ticks.x = element_blank(),
                axis.text.x = element_blank()) +
                labs(fill = "")
        )
    })



    ### ¿La persona agresora cuenta con una red de apoyo?
    
    output$red_apoyo_agresor <- renderPlotly({
        riesgos <- riesgosF()
        red_apoyo <- riesgos[c("siRedApoyo","noRedApoyo")]
        count_si_redAp <- red_apoyo %>% filter(!is.na(siRedApoyo)) %>% count(siRedApoyo)
        count_no_redAp <- red_apoyo %>% filter(!is.na(noRedApoyo)) %>% count(noRedApoyo)

        total_red_apoyo <- data.frame("Red_apoyo" = c("Si","No"),
                            "n" = c(count_si_redAp$n, count_no_redAp$n))

        ggplotly(
            ggplot(total_red_apoyo, aes(x=Red_apoyo, y = n, fill=Red_apoyo)) +
            geom_bar(stat = "identity") +
            xlab("Red apoyo agresor") +
            ylab("frecuencia") +
            theme(axis.ticks.x = element_blank(),
            axis.text.x = element_blank()) +
            labs(fill = "")
        )
    })



    ########################        DASHBOARD 3

    ##SECTION A

    output$presencial <- renderPlotly({
        # Presencial?
        encuesta_satisfaccion <- encuesta_satisfaccionF()
        sipresencial <- sum(!is.na(encuesta_satisfaccion$sipresencial))
        nopresencial <- sum(!is.na(encuesta_satisfaccion$nopresencial))

        presencial <- data.frame(category = c("Si", "No"), value = c(sipresencial, nopresencial))
        ggplotly(
            ggplot(presencial, aes(x=category, y=value)) +
                geom_bar(stat="identity", color = '#56267d', fill = '#56267d', aes(text=sprintf("Presencial: %s<br>Frecuencia: %s", category, value))) + 
                xlab("") +
                ylab("Frecuencia") +
                theme(legend.position = "none"), tooltip = "text")
    })
    
    # calificacion instalaciones
    califs <- data.frame("instalaciones" = c("1","2","3","4","5"))

    output$califInstalaciones<- renderPlotly({
        encuesta_satisfaccion <- encuesta_satisfaccionF()

        calif_instalaciones <- encuesta_satisfaccion %>% group_by(instalaciones) %>%
            filter(!is.na(instalaciones)) %>% summarize(n=n())

        calif_instalaciones <- left_join(califs,calif_instalaciones)
        calif_instalaciones[is.na(calif_instalaciones)] <- 0

        ggplotly(
            ggplot(calif_instalaciones, aes(x = instalaciones, y = n)) +
                geom_bar(stat = "identity", color = '#56267d', fill = '#56267d', aes(text=sprintf("Calificación: %s<br>Frecuencia: %s", instalaciones, n))) +
                xlab("") +
                ylab("Frecuencia") +
                theme(legend.position = "none"), tooltip = "text")
    })

    ##SECTION B
    output$cambios <- renderPlotly({
        servicio <- encuesta_satisfaccionF()[,c("sicambios","nocambios")]
        # cambios
        sicambios <- servicio %>% select(sicambios) %>% 
            filter(!is.na(sicambios)) %>%
            summarise(si = n())

        nocambios <- servicio %>% select(nocambios) %>% 
            filter(!is.na(nocambios)) %>%
            summarise(no = n())

        cambios <- t(bind_cols(sicambios,nocambios))
        colnames(cambios) <- "n"
        cambios <- cbind("Cambios"=rownames(cambios), data.frame(cambios, row.names=NULL))

        ggplotly(
            ggplot(cambios, aes(x = Cambios, y = n)) +
                geom_bar(stat = "identity", color = '#56267d', fill = '#56267d', aes(text=sprintf("Cambios: %s<br>Frecuencia: %s", Cambios, n))) +
                theme(legend.position = "none") +
                xlab("") +
                ylab("Frecuencia"), tooltip = "text")
    }) 

    # recurriria de nuevo a nuestros servicios?
    output$serviciosDeNuevo <- renderPlotly({
        servicio <- encuesta_satisfaccionF()[,c("siservicios","noservicios")]
        siservicios <- servicio %>% select(siservicios) %>% 
            filter(!is.na(siservicios)) %>%
            summarise(si = n())

        noservicios <- servicio %>% select(noservicios) %>% 
            filter(!is.na(noservicios)) %>%
            summarise(no = n())

        recur_serv <- t(bind_cols(siservicios,noservicios))
        colnames(recur_serv) <- "n"
        recur_serv <- cbind("Recurriria"=rownames(recur_serv), data.frame(recur_serv, row.names=NULL))

        ggplotly(
            ggplot(recur_serv, aes(x = Recurriria, y = n)) +
                geom_bar(stat = "identity", color = '#56267d', fill = '#56267d', aes(text=sprintf("Recurriría: %s<br>Frecuencia: %s", Recurriria, n))) +
                theme(legend.position = "none") +
                xlab("") +
                ylab("Frecuencia"), tooltip = "text")
    })
    
    # mismo psico de nuevo?
    output$mismoPsico <- renderPlotly({
        servicio <- encuesta_satisfaccionF()[,c("siMismoPsico","noMismoPsico")]
        siMismoPsico <- servicio %>% select(siMismoPsico) %>% 
            filter(!is.na(siMismoPsico)) %>%
            summarise(si = n())

        noMismoPsico <- servicio %>% select(noMismoPsico) %>% 
            filter(!is.na(noMismoPsico)) %>%
            summarise(no = n())

        mismoPsico <- t(bind_cols(siMismoPsico,noMismoPsico))
        colnames(mismoPsico) <- "n"
        mismoPsico <- cbind("Mismo"=rownames(mismoPsico), data.frame(mismoPsico, row.names=NULL))

        ggplotly(
            ggplot(mismoPsico, aes(x = Mismo, y = n)) +
                geom_bar(stat = "identity", color = '#56267d', fill = '#56267d', aes(text=sprintf("Mimso Psicoterapeuta: %s<br>Frecuencia: %s", Mismo, n))) +
                theme(legend.position = "none") +
                xlab("") +
                ylab("Frecuencia"), tooltip = "text")
    })

    # recomendaria servicios
    output$recomendacion <- renderPlotly({
        servicio <- encuesta_satisfaccionF()[,c("sirecomendacion","norecomendacion")]

        sirecomendacion <- servicio %>% select(sirecomendacion) %>% 
            filter(!is.na(sirecomendacion)) %>%
            summarise(si = n())

        norecomendacion <- servicio %>% select(norecomendacion) %>% 
            filter(!is.na(norecomendacion)) %>%
            summarise(no = n())

        recomendacion <- t(bind_cols(sirecomendacion,norecomendacion))
        colnames(recomendacion) <- "n"
        recomendacion <- cbind("Recomendaria"=rownames(recomendacion), data.frame(recomendacion, row.names=NULL))

        ggplotly(
            ggplot(recomendacion, aes(x = Recomendaria, y = n)) +
                geom_bar(stat = "identity", color = '#56267d', fill = '#56267d', aes(text=sprintf("Recomendaría: %s<br>Frecuencia: %s",Recomendaria,n))) +
                theme(legend.position = "none") +
                xlab("") +
                ylab("Frecuencia"), tooltip = "text")
    })
    
    ##SECTION C

    # fue canalizado? Edad, sexo, a que institucion

    output$canalizado <- renderPlotly({
        canalizaciones <- encuesta_satisfaccionF()[,c("sicanalizado","nocanalizado","institucion")]

        #fue canalizado?
        sicanalizado <- canalizaciones %>% select(sicanalizado) %>% 
            filter(!is.na(sicanalizado)) %>% summarise(si=n())
        nocanalizado <- canalizaciones %>% select(nocanalizado) %>%
            filter(!is.na(nocanalizado)) %>% summarise(no=n())

        canalizado <- t(bind_cols(sicanalizado,nocanalizado))
        colnames(canalizado) <- "n"
        canalizado <- cbind("Canalizado"=rownames(canalizado), data.frame(canalizado, row.names=NULL))

        ggplotly(
            ggplot(canalizado, aes(x = Canalizado, y = n)) +
                geom_bar(stat = "identity", color = '#56267d', fill = '#56267d', aes(text=sprintf("Canalizado: %s<br>Frecuencia: %s",Canalizado,n))) +
                theme(legend.position = "none") +
                xlab("") +
                ylab("Frecuencia"), tooltip = "text")
    })

    # edad
    output$edadesCanalizados <- renderPlotly({
        encuesta_satisfaccion <- encuesta_satisfaccionF()
        #convertimos a enteros
        encuesta_satisfaccion$edad <- as.numeric(encuesta_satisfaccion$edad)

        #obtenemos rangos
        labels_rangos <- data.frame("rango_edades" = c("0 a 5","6 a 12","13 a 18","19 a 25","26 a 29","30 a 59","más de 60"))
        rango_edades <- cut(encuesta_satisfaccion$edad, breaks = c(-1,5,12,18,25,29,59,Inf), labels = labels_rangos$rango_edades)
        encuesta_satisfaccion$rango_edades <- rango_edades
        # filtramos los que fueron canalizados
        personasCanalizadas <- encuesta_satisfaccion %>% filter(sicanalizado==1)
        # filtramos nas
        edadesPersonas <- personasCanalizadas %>% select(rango_edades) %>% filter(!is.na(rango_edades))

        rangos_edades <- edadesPersonas %>% group_by(rango_edades) %>% summarise(n = n())
        rangos_edades <- left_join(labels_rangos,rangos_edades)
        rangos_edades[is.na(rangos_edades)] <- 0

        ggplotly(
            ggplot(rangos_edades, aes(x = factor(rango_edades, level = labels_rangos$rango_edades), y = n)) + 
                geom_bar(stat = "identity", color = '#56267d', fill = '#56267d', aes(text=sprintf("Edad: %s<br>Frecuencia: %s",rango_edades,n))) +
                theme(legend.position = "none") +
                xlab("") +
                ylab("Frecuencia") +
                labs(fill = "Rango edades"), tooltip = "text")
    })

    # sexo
    output$sexoCanalizados <- renderPlotly({
        encuesta_satisfaccion <- encuesta_satisfaccionF()
        sexoPersonas <- personasF()[,c("id_folio","sexoPersona")]
        encuesta_satisfaccion <- left_join(x = encuesta_satisfaccion, y = sexoPersonas, by = "id_folio")
        # filtramos los que fueron canalizados
        personasCanalizadas <- encuesta_satisfaccion %>% filter(sicanalizado==1)
        sexosPersonaS <- personasCanalizadas %>% group_by(sexoPersona) %>% filter(!is.na(sexoPersona)) %>%
            summarise(n = n())
        colnames(sexosPersonaS)[1] <- "id_sexo"
        colnames(sexos)[2] <- "Sexo"
        sexosPersonaS <- right_join(sexosPersonaS,sexos, by = "id_sexo")
        sexosPersonaS[is.na(sexosPersonaS)] <- 0

        ggplotly(
            ggplot(sexosPersonaS, aes(x = Sexo, y = n)) +
                geom_bar(stat = "identity", color = '#56267d', fill = '#56267d', aes(text=sprintf("Sexo: %s<br>Frecuencia: %s",Sexo,n))) +
                theme(legend.position = "none") +
                xlab("") +
                ylab("Frecuencia"), tooltip = "text")
    })

    
    # frecuencia de canalizacion a instancias
    output$frecInstanciasCanalizadas <- renderPlotly({
        encuesta_satisfaccion <- encuesta_satisfaccionF()
        # filtramos los que fueron canalizados
        personasCanalizadas <- encuesta_satisfaccion %>% filter(sicanalizado==1)
        count_intancias <- personasCanalizadas %>% group_by(institucion) %>% summarise(n = n())
        colnames(count_intancias)[1] <- "id_instancia"

        count_instancias <- right_join(count_intancias,instancia)
        count_instancias[is.na(count_instancias)] <- 0
        colnames(count_instancias)[3] <- "nomInstancia"

        ggplotly(
            ggplot(count_instancias, aes(x = reorder(nomInstancia,-n), y = n, fill = nomInstancia)) +
                geom_bar(stat = "identity", aes(text=sprintf("Instancia: %s<br>Frecuencia: %s",nomInstancia,n))) +
                theme(axis.text.x = element_blank()) +
                xlab(element_blank()) +
                ylab("Frecuencia") +
                labs(fill = "Instancia"), tooltip = "text")
    })

    ##SECCION D

    output$satisfaccionxServicio <- renderPlotly({
        encuesta_satisfaccion <- encuesta_satisfaccionF()
        colnames(instancia)[2] <- "institucion"
        colnames(encuesta_satisfaccion)[24] <- "id_instancia"
        
        instancia_new <- instancia %>% group_by(id_instancia) %>% slice(1)
        
        satisfaccion_data <- merge(encuesta_satisfaccion, instancia_new, by="id_instancia")
        califxServicio <- satisfaccion_data %>% group_by(institucion) %>%
            summarize(promedio = round(mean(as.numeric(calificacionInstancia)), 2)) %>%
            arrange(desc(promedio)) %>%
            mutate(institucion = factor(institucion, levels = institucion))

        colnames(califxServicio)[1] <- "institucion"

        respuestas_Institucion <- data.frame(Institucion = c(califxServicio$institucion), value = c(califxServicio$promedio))

        ggplotly(
        ggplot(respuestas_Institucion, aes(x=Institucion, y=value, fill = Institucion)) +
        geom_bar(stat="identity") + 
        theme(axis.text.x = element_text(angle = 0, hjust=1)) +
        xlab("Instituciones")+
        ylab("Promedio de calificación")+
        coord_flip()
        )
    })

    output$satisfaccionxUtil <- renderPlotly({
        encuesta_satisfaccion <- encuesta_satisfaccionF()
        encuesta_satisfaccion_Util_si <- sum(!is.na(encuesta_satisfaccion$siutil))
        encuesta_satisfaccion_Util_no <- sum(!is.na(encuesta_satisfaccion$noutil))

        respuestas_Util <- data.frame(Util = c("Si", "No"), value = c(encuesta_satisfaccion_Util_si, encuesta_satisfaccion_Util_no))

        ggplotly(
        ggplot(respuestas_Util, aes(x=Util, y=value, fill = Util)) +
        geom_bar(stat="identity") + 
            theme(axis.text.x = element_text(angle = 0, hjust=1)) +
            xlab("Respuesta")+
            scale_fill_manual(values=c('#56267d', '#2AB7CD')))
    })

    ##SECTION E
    output$sesionesxEdades <- renderPlotly({
        encuesta_satisfaccion <- encuesta_satisfaccionF()

        labels_rangos <- data.frame("rango_edades" = c("0 a 5","6 a 12","13 a 18","19 a 25","26 a 29","30 a 59","más de 60"))
        rango_edades <- cut(encuesta_satisfaccion$edad, breaks = c(-1,5,12,18,25,29,59,Inf), labels = labels_rangos$rango_edades)
        encuesta_satisfaccion$rango_edades <- rango_edades

        enc_age <- encuesta_satisfaccion %>% group_by(id_cantidad_sesiones,rango_edades) %>%
            mutate(n = n())
        ggplotly(
            ggplot(enc_age, aes(x = id_cantidad_sesiones, y = rango_edades)) +
            geom_bar(position="dodge", aes(text=sprintf("Cantidad de sesiones: %s<br>Rango de Edad: %s<br>Cantidad: %s", id_cantidad_sesiones, rango_edades, n))) + 
            theme(legend.position = "none") +
            xlab("") +
            ylab("Cantidad") +
            theme(axis.ticks.x = element_blank()), tooltip = "text")
    })

    output$sesionesxSexo <- renderPlotly({
        encuesta_satisfaccion <- encuesta_satisfaccionF()
        print(length(colnames(encuesta_satisfaccion)))

        colnames(encuesta_satisfaccion)[30] <- "id_sexo"
        colnames(sexos)[2] <- "sexoPersona"
        
        sexo <- sexos %>% group_by(id_sexo) %>% slice(1)
        encuenta_new_data <- merge(encuesta_satisfaccion, sexo, by="id_sexo")
        encuenta_new_data <- merge(encuesta_satisfaccion, sexo, by="id_sexo")

        enc_actual <- encuenta_new_data %>% group_by(id_cantidad_sesiones,sexoPersona) %>%
            mutate(n = n())

        ggplotly(
            ggplot(enc_actual, aes(x = id_cantidad_sesiones, fill = sexoPersona)) +
            geom_bar(position="dodge", aes(text=sprintf("Cantidad de sesiones: %s<br>Rango de Edad: %s<br>Cantidad: %s", id_cantidad_sesiones, sexoPersona, n))) + 
            theme(legend.position = "none") +
            xlab("") +
            ylab("Cantidad") +
            theme(axis.ticks.x = element_blank()), tooltip = "text")
    })
    ##SECTION F

    #Principal: 6. ¿Recibió el servicio de Acompañamiento Emocional oportunamente (en el momento indicado) y de manera pronta?

    

    output$si_servicio <- renderPlotly({
        conteo_si_servicio <- encuesta_satisfaccionF() %>% count(siservicios)

        ggplotly(
        ggplot(conteo_si_servicio, aes(x = siservicios, y = n, fill = siservicios)) +
            geom_bar(stat = "identity") +
            xlab("Recibieron servicio de acompañamiento emocional") +
            ylab("Frecuencia") + 
            theme(axis.ticks.x = element_blank(),
            axis.text.x = element_blank()) +
            labs(fill = ""))

    })

    #12. ¿Qué tan importante y necesario fue para usted recibir el Servicio de Acompañamiento Emocional?

    ##SECTION G

    output$servicio_importante <- renderPlotly({
        ggplotly(
            ggplot(encuesta_satisfaccionF(), aes(x = importante, y = oportunoPronto)) +
            geom_line( color="grey") +
            geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
            theme_ipsum()
        )
    })

    ##SECTION F

    #7. ¿Cómo califica la vía de atención, se adaptó a sus necesidades? (La vía de atención se da por llamadas telefónicas, mensajes de texto y WhatsApp, presencial, etc.)

    output$via_atencion <- renderPlotly({
        conteo_via_atencion <- encuesta_satisfaccionF() %>% count(viaAdapto)

        ggplotly(
            ggplot(conteo_via_atencion, aes(x = viaAdapto, y = n, fill = viaAdapto)) + 
            geom_bar(stat = "identity") +
            xlab("Calificación de la vía de atención") +
            ylab("Frecuencia") +
             theme(axis.ticks.x = element_blank(),
            axis.text.x = element_blank()) +
            labs(fill = "")
        )
    })


    #8. ¿Cómo califica la confianza y seguridad que le hicieron sentir durante la atención?
    output$calificacion_confianza_seguridad <- renderPlotly({
        conteo_confianza_seguridad <- encuesta_satisfaccionF() %>% count(confianzaSeguridad)

        ggplotly(
            ggplot(conteo_confianza_seguridad, aes(x = confianzaSeguridad, y = n, fill = confianzaSeguridad)) +
            geom_bar(stat = "identity") +
            xlab("Calificación confianza y seguridad") +
            ylab("Frecuencia") +
             theme(axis.ticks.x = element_blank(),
            axis.text.x = element_blank()) +
            labs(fill = "")
        )
    })

    #9. Califique por favor el respeto con el que sintió que fue tratada/o durante el Acompañamiento Emocional:
    output$respeto_sesiones <- renderPlotly({
        conteo_respeto <- encuesta_satisfaccionF() %>% count(respeto) 

        ggplotly(
            ggplot(conteo_respeto, aes(x = respeto, y = n, fill = respeto)) +
            geom_bar(stat = "identity") +
            xlab("Respeto sentido en las sesiones de Acompañamiento Emocional") +
            ylab("Frecuencia") +
             theme(axis.ticks.x = element_blank(),
            axis.text.x = element_blank()) +
            labs(fill = "")
        )
    })

    #13. ¿Qué tan satisfactorio fue el trato que le brindó el equipo de Acompañamiento Emocional de CIAM?
    output$serv_satisfactorio <- renderPlotly({
        conteo_serv_satisfactorio <- encuesta_satisfaccionF() %>% count(satisfactorio)

        ggplotly(
            ggplot(conteo_serv_satisfactorio, aes(x = satisfactorio, y = n, fill = satisfactorio)) +
            geom_bar(stat = "identity") +
            xlab("Trato satisfactorio brindado") +
            ylab("Frecuencia") +
            theme(axis.ticks.x = element_blank(),
            axis.text.x = element_blank()) +
            labs(fill = "")
        )
    })


}
   







