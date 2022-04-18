library(shiny)
library(dplyr)
library(ggplot2)
#library(plotly)
library(jsonlite)
library(httr)

    

server <- function(input, output, session) {
    # llamada tablas
    base_url <- "http://127.0.0.1:8000/api/" # url raiz
    # base_url <- "http://127.0.0.1:8080/api/" # url raiz

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

    #api call
    api_call_folio <- httr::GET(full_url_folio)
    api_call_medio_contacto <- httr::GET(full_url_medio_contacto)
    api_call_persona <- httr::GET(full_url_persona)
    api_call_como_se_entero_cat <- httr::GET(full_url_como_se_entero_cat)
    api_call_riesgo <- httr::GET(full_url_riesgo)
    api_call_sexos <- httr::GET(full_url_sexos)

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

    #retrieving api's response leaving the status out
    folios <- folios_json$response
    medio_contacto <- medio_contacto_json$response
    personas <- personas_json$response
    como_se_entero_catalogo <- como_se_entero_catalogo_json$response
    canalizacion_seguimiento <- canalizacion_seguimiento_json$response
    instancia <- instancia_json$response
    riesgos <- riesgo_json$response
    sexos <- sexos_json$response
    tipo_violencia <- tipo_violencia_json$response
    modalidad <- modalidad_json$response




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
        califxinstancia$calificacion <- as.numeric(califxinstancia$calificacion)
        
        califpromxinstancia <- califxinstancia %>% group_by(instancia) %>%
            summarize(promedio = round(mean(calificacion), 2)) %>%
            arrange(desc(promedio)) %>%
            mutate(instancia = factor(instancia, levels = instancia))
        # graficamos
        ggplotly(ggplot(califpromxinstancia, aes(x = instancia, y = promedio, fill = instancia)) +
            geom_bar(stat = "identity", aes(text=sprintf("Instancia: %s<br>Promedio: %s", instancia, promedio))) +
            theme(axis.title.x = element_blank(),
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank()),tooltip="text")
    })



    ########################        DASHBOARD 2

    ## SECTION A 
    # Violencia experimentada

    violencia_experimentada <- personas[,c("id_folio","tipoViolenciaPersona","modalidadPersona")]
    colnames(violencia_experimentada)[2] <- "id_tipo_violencia"
    colnames(violencia_experimentada)[3] <- "id_modalidad"

    colnames(tipo_violencia)[2] <- "tipo_violencia"
    colnames(modalidad)[2] <- "modalidad"

    tipo_violencia_experimentada <- violencia_experimentada %>% count(id_tipo_violencia)
    modalidad_experimentada <- violencia_experimentada %>% count(id_modalidad)

    tipo_violencia_experimentada <- merge(tipo_violencia_experimentada,tipo_violencia)
    modalidad_experimentada <- merge(modalidad_experimentada,modalidad)

    count_tv_vs_m <- violencia_experimentada %>% group_by(id_tipo_violencia,id_modalidad) %>%
        summarize(n = n())

    tv_vs_m <- merge(count_tv_vs_m,tipo_violencia)
    tv_vs_m <- merge(tv_vs_m,modalidad)

    # histgrama de tipos de violencia
    output$hist_tipo_violencia_anterior <- renderPlotly({
        ggplotly(
            ggplot(tipo_violencia_experimentada, aes(x = tipo_violencia ,y = n, fill = tipo_violencia)) +
            geom_bar(stat="identity", aes(text=sprintf("Tipo de violencia: %s<br>Frecuencia: %s", tipo_violencia, n))) + 
            xlab("Tipos de Violencia") +
            ylab("Frecuencia"),tooltip="text")
    })

    # histograma de modalidad
    output$hist_modalidad_anterior <- renderPlotly({
        ggplotly(
            ggplot(modalidad_experimentada, aes(x = modalidad ,y = n, fill = modalidad)) +
            geom_bar(stat="identity", aes(text=sprintf("Modalidad: %s<br>Frecuencia: %s", modalidad, n))) +
            xlab("Modalidad") +
            ylab("Frecuencia"), tooltip="text")
    })

    # histograma combinado violencia vs modalidad
    output$hist_tipo_vs_modalidad_anterior <- renderPlotly({
        ggplotly(
            ggplot(tv_vs_m, aes(x = tipo_violencia, y = n, fill = modalidad)) +
            geom_bar(stat="identity", aes(text=sprintf("Tipo de violencia: %s<br>Modalidad: %s<br>Frecuencia: %s", tipo_violencia, modalidad, n))) + 
            xlab("Tipos de Violencia") +
            ylab("Frecuencia"), tooltip="text")
    })

    ## SECTION B
    # Violencia actual

    violencia_actual <- riesgos[,c("id_folio","violenciaRiesgo","modalidadRiesgo")]
    colnames(violencia_actual)[2] <- "id_tipo_violencia"
    colnames(violencia_actual)[3] <- "id_modalidad"
    
    tipo_violencia_actual <- violencia_actual %>% count(id_tipo_violencia)
    modalidad_actual <- violencia_actual %>% count(id_modalidad)

    tipo_violencia_actual <- merge(tipo_violencia_actual,tipo_violencia)
    modalidad_actual <- merge(modalidad_actual,modalidad)

    count_tv_vs_m_actual <- violencia_actual %>% group_by(id_tipo_violencia,id_modalidad) %>%
        summarize(n = n())

    tv_vs_m_actual <- merge(count_tv_vs_m_actual,tipo_violencia)
    tv_vs_m_actual <- merge(tv_vs_m_actual,modalidad)


    # histgrama de tipos de violencia
    output$hist_tipo_violencia_actual <- renderPlotly({
        ggplotly(
            ggplot(tipo_violencia_actual, aes(x = tipo_violencia ,y = n, fill = tipo_violencia)) +
            geom_bar(stat="identity", aes(text=sprintf("Tipo de violencia: %s<br>Frecuencia: %s", tipo_violencia, n))) + 
            xlab("Tipos de Violencia") +
            ylab("Frecuencia"),tooltip="text")
    })

    # histograma de modalidad
    output$hist_modalidad_actual <- renderPlotly({
        ggplotly(
            ggplot(modalidad_actual, aes(x = modalidad ,y = n, fill = modalidad)) +
            geom_bar(stat="identity", aes(text=sprintf("Modalidad: %s<br>Frecuencia: %s", modalidad, n))) +
            xlab("Modalidad") +
            ylab("Frecuencia"), tooltip="text")
    })

    # histograma combinado violencia vs modalidad
    output$hist_tipo_vs_modalidad_actual <- renderPlotly({
        ggplotly(
            ggplot(tv_vs_m_actual, aes(x = tipo_violencia, y = n, fill = modalidad)) +
            geom_bar(stat="identity", aes(text=sprintf("Tipo de violencia: %s<br>Modalidad: %s<br>Frecuencia: %s", tipo_violencia, modalidad, n))) + 
            xlab("Tipos de Violencia") +
            ylab("Frecuencia"), tooltip="text")
    })

    ## SECTION C
    output$personasxEdad <- renderPlotly({
        countEdad <- personas %>% count(personas$edadPersona)
        colnames(countEdad)[1] <- "edad"
        # print(countEdad)
        # rangeOne <- count(between(personas$edadPersona, 0, 10) == TRUEy)
        # rangeTwo <- between(personas$edadPersona, 11, 20)
        # rangeThre <- between(personas$edadPersona, 21, 30)
        # print(rangeOne)
        # 0,10
        # 11,20
        # 21,30
        # 31,40
        # 41,50
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
        personas_LGBT_si <- sum(!is.na(personas$siLGBPersona))
        personas_LGBT_no <- sum(!is.na(personas$noLGBPersona))

        respuestas_LGBT <- data.frame(LGBT = c("Si", "No"), value = c(personas_LGBT_si, personas_LGBT_no))

        ggplotly(
        ggplot(respuestas_LGBT, aes(x=LGBT, y=value, fill = LGBT)) +
        geom_bar(stat="identity") + 
            theme(axis.text.x = element_text(angle = 0, hjust=1)) +
            xlab("Respuesta")+
            scale_fill_manual(values=c('#56267d', '#2AB7CD')))
    })

    output$personasxLocalidad <- renderPlotly({
        countLoc <- personas %>% count(personas$localidadResidenciaPersona)
        colnames(countLoc)[1] <- "locations"

        respuestas_Localidad <- data.frame(Localidad = c(countLoc$locations), value = c(countLoc$n))

        ggplotly(
        ggplot(respuestas_Localidad, aes(x=Localidad, y=value, fill = Localidad)) +
        geom_bar(stat="identity") + 
            theme(axis.text.x = element_text(angle = 0, hjust=1)) +
            xlab("Localidades")+
            coord_flip()
        )
    })

    output$personasxEstado <- renderPlotly({
        countEst <- personas %>% count(personas$estadoResidenciaPersona)
        colnames(countEst)[1] <- "locations"

        respuestas_Estado <- data.frame(Estado = c(countEst$locations), value = c(countEst$n))

        ggplotly(
        ggplot(respuestas_Estado, aes(x=Estado, y=value, fill = Estado)) +
        geom_bar(stat="identity") + 
            theme(axis.text.x = element_text(angle = 0, hjust=1)) +
            xlab("Estados")+
            coord_flip()
        )
    })

    output$personasxPais <- renderPlotly({
        countPais <- personas %>% count(personas$paisResidenciaPersona)
        colnames(countPais)[1] <- "locations"

        respuestas_Pais <- data.frame(Pais = c(countPais$locations), value = c(countPais$n))

        ggplotly(
        ggplot(respuestas_Pais, aes(x=Pais, y=value, fill = Pais)) +
        geom_bar(stat="identity") + 
            theme(axis.text.x = element_text(angle = 0, hjust=1)) +
            xlab("Localidades")+
            coord_flip()
        )
    })

    output$personasxDiscapacidad <- renderPlotly({
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
        primerServ <- count(filter(personas, servicioPersona == 1))
        segundoServ <- count(filter(personas, servicioPersona == 2))
        tercerServ <- count(filter(personas, servicioPersona == 3))
        otrosServ <- count(filter(personas, servicioPersona > 3))
        
        respuestas_Tipo_Servicio <- data.frame(Tipo_Servicio = c("Atención en crisis", "Asesoría psicológica", "Canalización", "Otros"), value = c(primerServ$n,segundoServ$n,tercerServ$n,otrosServ$n))
        
        ggplotly(
            ggplot(respuestas_Tipo_Servicio, aes(x=Tipo_Servicio, y=value, fill = Tipo_Servicio)) +
            geom_bar(stat="identity") + 
            theme(axis.text.x = element_text(angle = 60, hjust=1)) +
            ylab("Personas")+
            xlab("Servicios")
        )
    })
    
    ## SECTION D

    ## cambio de id y renomabriemnto de columna
    colnames(riesgos)[4] <- "edad_agresor"
    colnames(riesgos)[5] <- "id_sexo"

    colnames(sexos)[2] <- "sexos_agresor_catalogo"

    ### conteo de los datos de las columnas edad_agresor y id_sexo
    conteo_edades_agresor <- riesgos %>% count(edad_agresor)

    conteo_sexos_agresor <- riesgos %>% count(id_sexo)

    ### unión de las tablas riesgos y sexos por el campo id_sexo
    sexos_agresor_data <- merge(conteo_sexos_agresor, sexos)

    ### gráfica de edad del agresor
    output$edades_agresor_grf <- renderPlotly({
        ggplotly(
            ggplot(conteo_edades_agresor, aes(x = edad_agresor, y = n, fill = edad_agresor)) +
            geom_bar(stat = "identity") +
            xlab("Edad Agresor") +
            ylab("Frecuencia"))
    })

    ### gráfica de sexo del agresor
    output$sexo_agresor_grf <- renderPlotly({
        ggplotly(
            ggplot(sexos_agresor_data, aes(x = sexos_agresor_catalogo, y = n, fill = sexos_agresor_catalogo)) +
            geom_bar(stat = "identity") + 
            xlab("Sexo del Agresor") +
            ylab("Frecuencia")
        )
    })

### gráfica: ¿La dirección del agresor/a es la misma de quien solicita el servicio?

dir <- riesgos[c("siDireccionAgresor","noDireccionAgresor")]

count_si_dirs <- dir %>% filter(!is.na(siDireccionAgresor)) %>% count(siDireccionAgresor)
count_no_dirs <- dir %>% filter(!is.na(noDireccionAgresor)) %>% count(noDireccionAgresor)

count_total <- data.frame("Misma_direccion" = c("Si","No"),
                          "n" = c(count_si_dirs$n,count_no_dirs$n))


output$misma_dir_agresor_victima <- renderPlotly({
    ggplotly(
        ggplot(count_total,aes(x=Misma_direccion, y=n, fill=Misma_direccion)) +
            geom_bar(stat = "identity") +
            xlab("Misma dirección del agresor de quien solicita el servicio") +
            ylab("Frecuencia")
    )
})



    ### ¿La persona agresora cuenta con una red de apoyo?
    red_apoyo <- riesgos[c("siRedApoyo","noRedApoyo")]
    count_si_redAp <- red_apoyo %>% filter(!is.na(siRedApoyo)) %>% count(siRedApoyo)
    count_no_redAp <- red_apoyo %>% filter(!is.na(noRedApoyo)) %>% count(noRedApoyo)

    total_red_apoyo <- data.frame("Red_apoyo" = c("Si","No"),
                          "n" = c(count_si_redAp$n, count_no_redAp$n))

    output$red_apoyo_agresor <- renderPlotly({
        ggplotly(
            ggplot(total_red_apoyo, aes(x=Red_apoyo, y = n, fill=Red_apoyo)) +
            geom_bar(stat = "identity") +
            xlab("Red apoyo agresor") +
            ylab("frecuencia")
        )
    })

 }
