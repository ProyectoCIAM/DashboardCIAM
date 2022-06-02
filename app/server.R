library(shiny)
library(dplyr)
library(ggplot2)
library(hrbrthemes)
#library(plotly)
library(jsonlite)
library(ggsankey)
#library(writexl)
library(grid)
library(gridExtra)
library(shadowtext)
library(tidyr)
    

server <- function(input, output, session) { 
    # llamada tablas
    #base_url <- "http://127.0.0.1:8000/api/" # url raiz
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

    ########################### paleta de colores ###########################

    paleta <- c("#56267d","#2AB7CD","#A19EA3","#ffda9e","#d8f79a","#fdf9c4","#fabfb7","#e2504c") #rev(RColorBrewer::brewer.pal(8, "BuPu")[2:8])

    ####################### KPIS #############################
    # kpi 1: mean(calificacionInstancia)
    output$kpi1 <- renderText({
        encuesta_satisfaccion <- encuesta_satisfaccionF()
        promedio_califInstancias <- round(mean(as.integer(encuesta_satisfaccion$calificacionInstancia)),2)
        prettyNum(promedio_califInstancias)
        #paste0(promedio_confianzaSeguridad, " / 5") 
    })

    # kpi 2: si cambios * 100 / n de encuenstas
    output$kpi2 <- renderUI({
        sicambios <- encuesta_satisfaccionF() %>% filter(!is.na(sicambios)) %>% count(sicambios)
        nencuestas <- nrow(encuesta_satisfaccionF())
        porcentaje_de_cambio <- round(sicambios$n * 100 / nencuestas, 2)
        paste0(porcentaje_de_cambio, "%")
    })

    # kpi 3: mean(confianzaSeguridad)
    output$kpi3 <- renderText({
        encuesta_satisfaccion <- encuesta_satisfaccionF()
        promedio_confianzaSeguridad <- round(mean(as.integer(encuesta_satisfaccion$confianzaSeguridad)),2)
        prettyNum(promedio_confianzaSeguridad)
        #paste0(promedio_confianzaSeguridad, " / 5") 
    })

    #kpi 4: mean(respeto)
    output$kpi4 <- renderText({
        encuesta_satisfaccion <- encuesta_satisfaccionF()
        promedio_respeto <- round(mean(as.integer(encuesta_satisfaccion$respeto)),2)
        prettyNum(promedio_respeto)
        #paste0(promedio_confianzaSeguridad, " / 5") 
    })

    #kpi 5: mean(satisfactorio)
    output$kpi5 <- renderText({
        encuesta_satisfaccion <- encuesta_satisfaccionF()
        promedio_satisfactorio <- round(mean(as.integer(encuesta_satisfaccion$satisfactorio)),2)
        prettyNum(promedio_satisfactorio)
        #paste0(promedio_confianzaSeguridad, " / 5") 
    })

    ########################        DASHBOARD 1  PART 1
    ###     MEDIO DE CONTACTO    ###

    output$contacto <- renderPlotly({
        folios <- foliosF()
        # cambio de id para poder hacer merge con catalogo
        colnames(folios)[3] <- "id_medio_contacto"
        # solo por cambiar título de columna nombra a medio contacto
        colnames(medio_contacto)[2] <- "medio_contacto"
        count_contactos <- folios %>% count(id_medio_contacto)
        # hacemos merge
        foliosxmedioc <- right_join(count_contactos,medio_contacto)
        foliosxmedioc$n[is.na(foliosxmedioc$n)] <- 0

        foliosxmedioc$p <- paste0(round(foliosxmedioc$n * 100 / sum(foliosxmedioc$n),1), "%")

        ggplotly(
            ggplot(foliosxmedioc, aes(x = reorder(medio_contacto, -n), y = n, fill = factor(medio_contacto, level = medio_contacto))) +
                geom_bar(stat="identity", aes(text=sprintf("Categoría: %s<br>Frecuencia: %s", medio_contacto,n))) + 
                #geom_shadowtext(aes(label = p, color = p)) +
                geom_text(aes(label = p), color = "white", size = 4,face = "bold", vjust = 1) +
                geom_text(aes(label = p), color = "black", size = 3, alpha=1, vjust = 1) +
                #geom_label(aes(label = p)) +
                xlab("Categoría") +
                ylab("Frecuencia") +
                theme(axis.text.x = element_blank(), 
                    panel.background = element_blank()) +
                scale_fill_manual(values = paleta) +
                labs(fill = ""), tooltip = "text"
        )
    })


    ###     ¿La persona que contacta es quien requiere el servicio?    ###
    #counting id's from folios' column id_contacto_catalogo
    output$persona_servicio <- renderPlotly({
        folios <- foliosF()
        persona_requiere_servicio_count <- sum(!is.na(folios$filtro_si))
        persona_otra_requiere_servicio_count <- sum(!is.na(folios$filtro_no))

        persona_solicita_servicio <- data.frame(category = c("Si", "No"), value = c(persona_requiere_servicio_count, persona_otra_requiere_servicio_count))

        persona_solicita_servicio$p <- paste0(round(persona_solicita_servicio$value * 100 / sum(persona_solicita_servicio$value),1), "%")

        ggplotly(
        ggplot(persona_solicita_servicio, aes(x=category, y=value, fill=category)) +
        geom_bar(stat="identity", aes(text=sprintf("Categoría: %s<br>Frecuencia: %s", category,value))) +
        geom_text(aes(label = p), vjust = 1) +
            xlab("") +
            ylab("Frecuencia") +
            scale_fill_manual(values=c('#56267d', '#2AB7CD')) + 
            theme(legend.position = "none", panel.background = element_blank()), tooltip = "text")
    })



    ###     ¿Cómo se enteró de los servicios de acompañamiento emocional?    ###
    #counting ids from personas' column idComoSeEntero

    output$serv_acomp_emocional <- renderPlotly({
        personas <- personasF()
        colnames(personas)[16] <- "id_como_se_entero"
        colnames(como_se_entero_catalogo)[2] <- "como_se_entero"

        count_comose <- personas %>% count(id_como_se_entero)
        servicio_acomp_emocional_data <- right_join(count_comose, como_se_entero_catalogo)
        servicio_acomp_emocional_data$n[is.na(servicio_acomp_emocional_data$n)] <- 0

        servicio_acomp_emocional_data$p <- paste0(round(servicio_acomp_emocional_data$n * 100 / sum(servicio_acomp_emocional_data$n),1), "%")
        
    ggplotly(
        ggplot(servicio_acomp_emocional_data, aes(x=reorder(como_se_entero, -n), y=n, fill = factor(como_se_entero, level = como_se_entero_catalogo$como_se_entero))) + 
        geom_bar(stat="identity", aes(text=sprintf("Categoría: %s<br>Frecuencia: %s", como_se_entero,n))) + 
        xlab("Categoría") +
        geom_text(aes(label = p), vjust = 1) +
        ylab("Frecuencia") +
        theme(axis.text.x = element_blank(), panel.background = element_blank()) +
            scale_fill_manual(values = paleta) +
            labs(fill = ""), tooltip = "text")
    })

    ########################        DASHBOARD 1  PART 2
    output$canalizacionxAnterior <- renderPlotly({
        canalizacion_seguimiento <- canalizacion_seguimientoF()
        canalizacion_anterior_si_count <- sum(!is.na(canalizacion_seguimiento$siCanalizacion))
        canalizacion_anterior_no_count <- sum(!is.na(canalizacion_seguimiento$noCanalizacion))

        respuestas_anterior <- data.frame(Anterior = c("Si", "No"), value = c(canalizacion_anterior_si_count, canalizacion_anterior_no_count))

        respuestas_anterior$p <- paste0(round(respuestas_anterior$value * 100 / sum(respuestas_anterior$value),1), "%")

        ggplotly(
        ggplot(respuestas_anterior, aes(x=Anterior, y=value, fill = Anterior)) +
        geom_bar(stat="identity", aes(text=sprintf("Canalizado anteriormente: %s<br>Frecuencia: %s", Anterior,value))) + 
            xlab("") +
            geom_text(aes(label = p), vjust = 1) +
            ylab("Frecuencia") +
            scale_fill_manual(values=c('#56267d', '#2AB7CD')) +
            theme(legend.position = "none", panel.background = element_blank()), tooltip = "text")
    })

    output$canalizacionxSeguimiento <- renderPlotly({
        canalizacion_seguimiento <- canalizacion_seguimientoF()
        canalizacion_Seguimiento_si_count <- sum(!is.na(canalizacion_seguimiento$siSeguimiento))
        canalizacion_Seguimiento_no_count <- sum(!is.na(canalizacion_seguimiento$noSeguimiento))

        respuestas_Seguimiento <- data.frame(Seguimiento = c("Si", "No"), value = c(canalizacion_Seguimiento_si_count, canalizacion_Seguimiento_no_count))

        respuestas_Seguimiento$p <- paste0(round(respuestas_Seguimiento$value * 100 / sum(respuestas_Seguimiento$value),1), "%")

        ggplotly(
        ggplot(respuestas_Seguimiento, aes(x=Seguimiento, y=value, fill = Seguimiento)) +
        geom_bar(stat="identity", aes(text=sprintf("Seguimiento: %s<br>Frecuencia: %s", Seguimiento,value))) + 
            xlab("") +
            geom_text(aes(label = p), vjust = 1) +
            ylab("Frecuencia") +
            scale_fill_manual(values=c('#56267d', '#2AB7CD')) +
            theme(legend.position = "none", panel.background = element_blank()), tooltip = "text")
    })

    output$canalizacionxAdecuado <- renderPlotly({
        canalizacion_seguimiento <- canalizacion_seguimientoF()
        canalizacion_Adecuado_si_count <- sum(!is.na(canalizacion_seguimiento$siAdecuado))
        canalizacion_Adecuado_no_count <- sum(!is.na(canalizacion_seguimiento$noAdecuado))

        respuestas_Adecuado <- data.frame(Adecuado = c("Si", "No"), value = c(canalizacion_Adecuado_si_count, canalizacion_Adecuado_no_count))
        
        respuestas_Adecuado$p <- paste0(round(respuestas_Adecuado$value * 100 / sum(respuestas_Adecuado$value),1), "%")

        ggplotly(
        ggplot(respuestas_Adecuado, aes(x=Adecuado, y=value, fill = Adecuado)) +
        geom_bar(stat="identity", aes(text=sprintf("Adecuado: %s<br>Frecuencia: %s", Adecuado, value))) + 
            xlab("") +
            geom_text(aes(label = p), vjust = 1) +
            ylab("Frecuencia") +
            scale_fill_manual(values=c('#56267d', '#2AB7CD')) +
            theme(legend.position = "none", panel.background = element_blank()), tooltip = "text")
    })

    output$canalizacionxOportuno <- renderPlotly({
        canalizacion_seguimiento <- canalizacion_seguimientoF()
        canalizacion_Oportuno_si_count <- sum(!is.na(canalizacion_seguimiento$siOportuno))
        canalizacion_Oportuno_no_count <- sum(!is.na(canalizacion_seguimiento$noOportuno))

        respuestas_Oportuno <- data.frame(Oportuno = c("Si", "No"), value = c(canalizacion_Oportuno_si_count, canalizacion_Oportuno_no_count))

        respuestas_Oportuno$p <- paste0(round(respuestas_Oportuno$value * 100 / sum(respuestas_Oportuno$value),1), "%")

        ggplotly(
        ggplot(respuestas_Oportuno, aes(x=Oportuno, y=value, fill = Oportuno)) +
        geom_bar(stat="identity", aes(text=sprintf("Oportuno: %s<br>Frecuencia: %s", Oportuno, value))) + 
            xlab("") +
            geom_text(aes(label = p), vjust = 1) +
            ylab("Frecuencia") +
            scale_fill_manual(values=c('#56267d', '#2AB7CD')) +
            theme(legend.position = "none", panel.background = element_blank()), tooltip = "text")
    })

    output$canalizacionxProntitud <- renderPlotly({
        canalizacion_seguimiento <- canalizacion_seguimientoF()
        canalizacion_Prontitud_si_count <- sum(!is.na(canalizacion_seguimiento$siProntitud))
        canalizacion_Prontitud_no_count <- sum(!is.na(canalizacion_seguimiento$noProntitud))

        respuestas_Prontitud <- data.frame(Prontitud = c("Si", "No"), value = c(canalizacion_Prontitud_si_count, canalizacion_Prontitud_no_count))

        respuestas_Prontitud$p <- paste0(round(respuestas_Prontitud$value * 100 / sum(respuestas_Prontitud$value),1), "%")

        ggplotly(
        ggplot(respuestas_Prontitud, aes(x=Prontitud, y=value, fill = Prontitud)) +
        geom_bar(stat="identity", aes(text=sprintf("Prontitud: %s<br>Frecuencia: %s", Prontitud, value))) + 
            xlab("") +
            geom_text(aes(label = p), vjust = 1) +
            ylab("Frecuencia") +
            scale_fill_manual(values=c('#56267d', '#2AB7CD')) +
            theme(legend.position = "none", panel.background = element_blank()), tooltip = "text")
    })

    output$canalizacionxConfianza <- renderPlotly({
        canalizacion_seguimiento <- canalizacion_seguimientoF()
        canalizacion_Confianza_si_count <- sum(!is.na(canalizacion_seguimiento$siConfianza))
        canalizacion_Confianza_no_count <- sum(!is.na(canalizacion_seguimiento$noConfianza))

        respuestas_Confianza <- data.frame(Confianza = c("Si", "No"), value = c(canalizacion_Confianza_si_count, canalizacion_Confianza_no_count))
        
        respuestas_Confianza$p <- paste0(round(respuestas_Confianza$value * 100 / sum(respuestas_Confianza$value),1), "%")

        ggplotly(
        ggplot(respuestas_Confianza, aes(x=Confianza, y=value, fill = Confianza)) +
        geom_bar(stat="identity", aes(text=sprintf("Confianza: %s<br>Frecuencia: %s", Confianza, value))) + 
            xlab("") +
            geom_text(aes(label = p), vjust = 1) +
            ylab("Frecuencia") +
            scale_fill_manual(values=c('#56267d', '#2AB7CD')) +
            theme(legend.position = "none", panel.background = element_blank()), tooltip = "text")
    })

    output$canalizacionxRespeto <- renderPlotly({
        canalizacion_seguimiento <- canalizacion_seguimientoF()
        canalizacion_Respeto_si_count <- sum(!is.na(canalizacion_seguimiento$siRespeto))
        canalizacion_Respeto_no_count <- sum(!is.na(canalizacion_seguimiento$noRespeto))

        respuestas_Respeto <- data.frame(Respeto = c("Si", "No"), value = c(canalizacion_Respeto_si_count, canalizacion_Respeto_no_count))

        respuestas_Respeto$p <- paste0(round(respuestas_Respeto$value * 100 / sum(respuestas_Respeto$value),1), "%")

        ggplotly(
        ggplot(respuestas_Respeto, aes(x=Respeto, y=value, fill = Respeto)) +
        geom_bar(stat="identity", aes(text=sprintf("Respeto: %s<br>Frecuencia: %s", Respeto, value))) + 
            xlab("") +
            geom_text(aes(label = p), vjust = 1) +
            ylab("Frecuencia") +
            scale_fill_manual(values=c('#56267d', '#2AB7CD')) +
            theme(legend.position = "none", panel.background = element_blank()), tooltip = "text")
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

        todasInstanciasCalif$p <- paste0(round(todasInstanciasCalif$Conteo * 100 / sum(todasInstanciasCalif$Conteo),1), "%")
        
        # graficamos
        ggplotly(ggplot(todasInstanciasCalif, aes(x = reorder(Instancia,-Promedio), y = Promedio, fill = Instancia)) +
            geom_bar(stat = "identity", aes(text=sprintf("Instancia: %s<br>Promedio: %s<br>Conteo: %s<br>Calificación mínima: %s<br>Calificación máxima: %s", Instancia, Promedio, Conteo, Min, Max))) +
            theme(axis.title.x = element_blank(),
                axis.text.x = element_blank(),
                panel.background = element_blank()) +
                geom_text(aes(label = p), vjust = 1) +
                scale_fill_manual(values = paleta), tooltip = "text")
    })

    ########################        DASHBOARD 2

    ## SECTION A 
    # Violencia Datos persona
    colnames(tipo_violencia)[2] <- "tipo_violencia"
    colnames(modalidad)[2] <- "modalidad"

    # histgrama de tipos de violencia
    output$hist_tipo_violencia_anterior <- renderPlotly({
        violencia_experimentada <- personasF()[,c("id_folio","tipoViolenciaPersona","sexoPersona")]
        colnames(violencia_experimentada)[2] <- "id_tipo_violencia"
        colnames(violencia_experimentada)[3] <- "id_sexo"
        colnames(sexos)[2] <- "Sexo"

        tipo_violencia_experimentada <- violencia_experimentada %>% filter(!is.na(id_folio)) %>%
            count(id_tipo_violencia)
        tipo_violencia_experimentada <- right_join(tipo_violencia_experimentada,tipo_violencia)
        tipo_violencia_experimentada$n[is.na(tipo_violencia_experimentada$n)] <- 0

        tipo_violencia_experimentadaxsex <- violencia_experimentada %>% filter(!is.na(id_folio)) %>%
            count(id_tipo_violencia, id_sexo) #%>%
            #mutate(n_sex=n())
        tipo_violencia_sexos <- merge(tipo_violencia,sexos)
        tipo_violencia_experimentadaxsex <- right_join(tipo_violencia_experimentadaxsex,tipo_violencia_sexos)
        tipo_violencia_experimentadaxsex$n[is.na(tipo_violencia_experimentadaxsex$n)] <- 0

        tipo_violencia_experimentada$p <- paste0(round(tipo_violencia_experimentada$n * 100 / sum(tipo_violencia_experimentada$n),1), "%")

        #tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
        #tbl <- tableGrob(tabla_resumen, rows=NULL, theme=tt)
        #gr <- grid.arrange(g, tbl, nrow=2, as.table=TRUE)

        ggplotly(
            ggplot(data = tipo_violencia_experimentada, aes(x = tipo_violencia, y = n)) +
                geom_bar(aes(text=sprintf("Tipo de violencia: %s<br>Frecuencia: %s", tipo_violencia, n)), stat="identity", color = '#56267d', fill = '#56267d') + 
                xlab("") +
                ylab("Frecuencia") +
                #labs(fill = "") +
                geom_text(aes(label = p), vjust = -1) +
                geom_line(data = tipo_violencia_experimentadaxsex, aes(x = tipo_violencia, y = n, group = Sexo, colour = Sexo)) +
                scale_color_manual(values = paleta[-1]) +
                theme(panel.background = element_blank()), tooltip = "text")
    })

    tableTipoVDP <- reactive({
        violencia_experimentada <- personasF()[,c("id_folio","tipoViolenciaPersona","sexoPersona")]
        colnames(violencia_experimentada)[2] <- "id_tipo_violencia"
        colnames(violencia_experimentada)[3] <- "id_sexo"
        colnames(sexos)[2] <- "Sexo"

         tipo_violencia_experimentadaxsex <- violencia_experimentada %>% filter(!is.na(id_folio)) %>%
            count(id_tipo_violencia, id_sexo) #%>%
            #mutate(n_sex=n())
        tipo_violencia_sexos <- merge(tipo_violencia,sexos)
        tipo_violencia_experimentadaxsex <- right_join(tipo_violencia_experimentadaxsex,tipo_violencia_sexos)
        tipo_violencia_experimentadaxsex$n[is.na(tipo_violencia_experimentadaxsex$n)] <- 0       

        tabla_resumen <- tipo_violencia_experimentadaxsex[,c("Sexo","tipo_violencia","n")]
        tabla_resumen <- pivot_wider(tabla_resumen, names_from = tipo_violencia, values_from = n)

        samp2 <- tabla_resumen[,-1]
        samp2 <- samp2[,order(names(samp2))]
        
        final <- cbind(samp2,tabla_resumen$Sexo)
        colnames(final)[ncol(final)] <- "Sexo"

        final
    })

    output$tablaTiposViolenciaDP <- renderTable({
        tableTipoVDP()
    })

    # histograma de modalidad
    output$hist_modalidad_anterior <- renderPlotly({
        violencia_experimentada <- personasF()[,c("id_folio","modalidadPersona")]
        colnames(violencia_experimentada)[2] <- "id_modalidad"

        modalidad_experimentada <- violencia_experimentada %>% count(id_modalidad)
        modalidad_experimentada <- merge(modalidad_experimentada,modalidad)
        modalidad_experimentada <- right_join(modalidad_experimentada,modalidad)
        modalidad_experimentada[is.na(modalidad_experimentada)] <- 0

        modalidad_experimentada$p <- paste0(round(modalidad_experimentada$n * 100 / sum(modalidad_experimentada$n),1), "%")

        ggplotly(
            ggplot(modalidad_experimentada, aes(x = reorder(modalidad,-n) ,y = n)) +
            geom_bar(stat="identity", color = '#56267d', fill = '#56267d', aes(text=sprintf("Ámbito: %s<br>Frecuencia: %s", modalidad, n))) +
            xlab("") +
            ylab("Frecuencia") +
            labs(fill = "") +
            geom_text(aes(label = p), vjust = -1) +
            theme(panel.background = element_blank()), tooltip = "text")
    })

    # histograma combinado violencia vs modalidad
    output$hist_tipo_vs_modalidad_anterior <- renderPlotly({
        violencia_experimentada <- personasF()[,c("id_folio","tipoViolenciaPersona","modalidadPersona")]
        colnames(violencia_experimentada)[2] <- "id_tipo_violencia"
        colnames(violencia_experimentada)[3] <- "id_modalidad"

        count_tv_vs_m <- violencia_experimentada %>% group_by(id_tipo_violencia,id_modalidad) %>% 
            mutate(n = n())

        v_y_m <-merge(tipo_violencia, modalidad)

        tv_vs_m <- left_join(v_y_m, count_tv_vs_m, by = c("id_tipo_violencia","id_modalidad"))
        tv_vs_m$n[is.na(tv_vs_m$n)] <- 0

        tv_vs_m$p <- paste0(round(tv_vs_m$n * 100 * tv_vs_m$n / sum(tv_vs_m$n),1), "%")
        #print(tv_vs_m)
        
        ggplotly(
            ggplot(tv_vs_m, aes(x = tipo_violencia, y = n, fill = modalidad)) +
            geom_bar(stat = "identity", position = position_dodge(), aes(text=sprintf("Tipo de violencia: %s<br>Ámbito: %s<br>Frecuencia: %s", tipo_violencia, modalidad, n))) + 
            xlab("Tipos de Violencia") +
            ylab("Frecuencia") +
            labs(fill = "Ámbito") +
            scale_fill_manual(values = paleta) +
            geom_text(aes(label = p), vjust = 1, position = position_dodge(width = .9)) +
            theme(panel.background = element_blank()), tooltip = "text")
    })

    ## SECTION B
    # Violencia Riesgo

    # histgrama de tipos de violencia
    output$hist_tipo_violencia_actual <- renderPlotly({
        violencia_actual <- riesgosF()[,c("id_folio","violenciaRiesgo")]
        colnames(violencia_actual)[2] <- "id_tipo_violencia"

        tipo_violencia_actual <- violencia_actual %>% count(id_tipo_violencia)
        tipo_violencia_actual <- merge(tipo_violencia_actual,tipo_violencia)
        tipo_violencia_actual <- right_join(tipo_violencia_actual,tipo_violencia)
        tipo_violencia_actual[is.na(tipo_violencia_actual)] <- 0

        tipo_violencia_actual$p <- paste0(round(tipo_violencia_actual$n * 100 / sum(tipo_violencia_actual$n),1), "%")

        ggplotly(
            ggplot(tipo_violencia_actual, aes(x = reorder(tipo_violencia,-n) ,y = n)) +
            geom_bar(stat="identity", color = '#56267d', fill = '#56267d', aes(text=sprintf("Tipo de violencia: %s<br>Frecuencia: %s", tipo_violencia, n))) + 
            xlab("") +
            ylab("Frecuencia") +
            labs(fill = "") +
            geom_text(aes(label = p), vjust = 1) +
            theme(panel.background = element_blank()), tooltip = "text")
    })

    # histograma de modalidad
    output$hist_modalidad_actual <- renderPlotly({
        violencia_actual <- riesgosF()[,c("id_folio","modalidadRiesgo")]
        colnames(violencia_actual)[2] <- "id_modalidad"

        modalidad_actual <- violencia_actual %>% count(id_modalidad)
        modalidad_actual <- merge(modalidad_actual,modalidad)
        modalidad_actual <- right_join(modalidad_actual,modalidad)
        modalidad_actual[is.na(modalidad_actual)] <- 0

        modalidad_actual$p <- paste0(round(modalidad_actual$n * 100 / sum(modalidad_actual$n),1), "%")

        ggplotly(
            ggplot(modalidad_actual, aes(x = reorder(modalidad,-n) ,y = n)) +
            geom_bar(stat="identity", color = '#56267d', fill = '#56267d', aes(text=sprintf("Ámbito: %s<br>Frecuencia: %s", modalidad, n))) +
            xlab("") +
            ylab("Frecuencia") +
            labs(fill = "") +
            geom_text(aes(label = p), vjust = 1) +
            theme(panel.background = element_blank()), tooltip = "text")
    })

    # histograma combinado violencia vs modalidad
    output$hist_tipo_vs_modalidad_actual <- renderPlotly({
        violencia_actual <- riesgosF()[,c("id_folio","violenciaRiesgo","modalidadRiesgo")]
        colnames(violencia_actual)[2] <- "id_tipo_violencia"
        colnames(violencia_actual)[3] <- "id_modalidad"

        count_tv_vs_m_actual <- violencia_actual %>% group_by(id_tipo_violencia,id_modalidad) %>%
            mutate(n = n())

        v_y_m <-merge(tipo_violencia, modalidad)

        tv_vs_m_actual <- left_join(v_y_m, count_tv_vs_m_actual, by = c("id_tipo_violencia","id_modalidad"))
        tv_vs_m_actual$n[is.na(tv_vs_m_actual$n)] <- 0

        tv_vs_m_actual$p <- paste0(round(tv_vs_m_actual$n * 100 * tv_vs_m_actual$n / sum(tv_vs_m_actual$n),1), "%")

        ggplotly(
            ggplot(tv_vs_m_actual, aes(x = tipo_violencia, y = n, fill = modalidad)) +
            geom_bar(stat = "identity", position = position_dodge(), aes(text=sprintf("Tipo de violencia: %s<br>Ámbito: %s<br>Frecuencia: %s", tipo_violencia, modalidad, n))) + 
            xlab("Tipos de Violencia") +
            ylab("Frecuencia") +
            labs(fill = "Ámbito") +
            scale_fill_manual(values = paleta) +
            geom_text(aes(label = p), vjust = 1, position = position_dodge(width = .9)) +
            theme(panel.background = element_blank()), tooltip = "text")
    })

    ## SECTION A+B
    output$violencethenvsnow <- renderPlotly({
        violencia_experimentada <- personasF()[,c("id_folio","tipoViolenciaPersona")]
        colnames(violencia_experimentada)[2] <- "id_tipo_violencia"

        colnames(tipo_violencia)[2] <- "tipo_violencia"

        violencia_actual <- riesgosF()[,c("id_folio","violenciaRiesgo")]
        colnames(violencia_actual)[2] <- "id_tipo_violencia"

        tipo_experimentada <- merge(tipo_violencia, violencia_experimentada)
        tipo_experimentada <- tipo_experimentada %>% select(tipo_violencia, id_folio)
        colnames(tipo_experimentada)[1] <- "Persona"

        tipo_actual <- merge(tipo_violencia, violencia_actual)
        tipo_actual <- tipo_actual %>% select(tipo_violencia, id_folio)
        colnames(tipo_actual)[1] <- "Riesgo"

        tiposthennow <- merge(tipo_experimentada, tipo_actual, all=TRUE)
        test_tipo <- tiposthennow %>% make_long(Persona, Riesgo) 

        ggplotly(ggplot(test_tipo, aes(x = x, 
                next_x = next_x, 
                node = node, 
                next_node = next_node,
                fill = factor(node))) +
            geom_sankey(aes(text=sprintf("Tipo violencia: %s", node))) +
            theme_sankey(base_size = 16) +
            scale_fill_manual(values = paleta) +
            labs(fill = "") +
            xlab(""), tooltip = "text")
    })

    output$modalidadthenvsnow <- renderPlotly({
        violencia_experimentada <- personasF()[,c("id_folio","modalidadPersona")]
        colnames(violencia_experimentada)[2] <- "id_modalidad"
        colnames(modalidad)[2] <- "modalidad"

        violencia_actual <- riesgosF()[,c("id_folio","modalidadRiesgo")]
        colnames(violencia_actual)[2] <- "id_modalidad"

        modalidad_experimentada <- merge(modalidad, violencia_experimentada)
        modalidad_experimentada <- modalidad_experimentada %>% select(modalidad, id_folio)
        colnames(modalidad_experimentada)[1] <- "Persona"

        modalidad_actual <- merge(modalidad, violencia_actual)
        modalidad_actual <- modalidad_actual %>% select(modalidad, id_folio)
        colnames(modalidad_actual)[1] <- "Riesgo"

        modalidadesthennow <- merge(modalidad_experimentada, modalidad_actual, all=TRUE)
        test <- modalidadesthennow %>% make_long(Persona, Riesgo) 

        ggplotly(ggplot(test, aes(x = x, 
                next_x = next_x, 
                node = node, 
                next_node = next_node,
                fill = factor(node))) +
            geom_sankey(aes(text=sprintf("Ámbito: %s", node))) +
            theme_sankey(base_size = 16) +
            scale_fill_manual(values = paleta) +
            labs(fill = "") +
            xlab(""), tooltip = "text")
    })

    ## SECTION C
    output$personasxEdad <- renderPlotly({
        personas <- personasF()
        edad_rangos <- data.frame("rango_edades" = c("0 a 5","6 a 12","13 a 18","19 a 25","26 a 29","30 a 59","más de 60"))
        rango_edades <- cut(as.numeric(personas$edadPersona), breaks = c(-1,5,12,18,25,29,59,Inf), labels = edad_rangos$rango_edades)
        personas$rango_edades <- rango_edades
        countEdad <- personas %>% count(rango_edades)
        
        countEdad <- left_join(edad_rangos,countEdad, by="rango_edades")
        countEdad$n[is.na(countEdad$n)] <- 0
        colnames(countEdad)[1] <- "edad"

        countEdad$p <- paste0(round(countEdad$n * 100 / sum(countEdad$n),1), "%")

        ggplotly(
        ggplot(countEdad, aes(x = factor(edad, level = edad_rangos$rango_edades), y = n)) + 
                geom_bar(stat = "identity", width = 1, color = '#56267d', fill = '#56267d', aes(text=sprintf("Edad: %s<br>Frecuencia: %s",edad,n))) +
                theme(legend.position = "none", panel.background = element_blank()) +
                xlab("Años") +
                geom_text(aes(label = p), vjust = 1) +
                ylab("Frecuencia") +
                labs(fill = "Rango edades"), tooltip = "text")
    })

    output$personasxLGBT <- renderPlotly({
        personas <- personasF()
        personas_LGBT_si <- sum(!is.na(personas$siLGBPersona))
        personas_LGBT_no <- sum(!is.na(personas$noLGBPersona))

        respuestas_LGBT <- data.frame(LGBT = c("Si", "No"), value = c(personas_LGBT_si, personas_LGBT_no))

        respuestas_LGBT$p <- paste0(round(respuestas_LGBT$value * 100 / sum(respuestas_LGBT$value),1), "%")

        ggplotly(
        ggplot(respuestas_LGBT, aes(x=LGBT, y=value, fill = LGBT)) +
        geom_bar(stat="identity", aes(text=sprintf("LGBT: %s<br>Frecuencia: %s", LGBT, value))) + 
            xlab("") +
            ylab("Frecuencia") +
            geom_text(aes(label = p), vjust = 1) +
            scale_fill_manual(values=c('#56267d', '#2AB7CD')) +
            theme(legend.position = "none", panel.background = element_blank()), tooltip = "text")
    })

    output$personasxSexo <- renderPlotly({
        personas <- personasF()
        colnames(sexos)[2] <- "sexoPersona"
        colnames(personas)[3] <- "id_sexo"

        countSexo <- personas %>% count(id_sexo)
        countSexo <- right_join(countSexo, sexos, by="id_sexo")
        countSexo$n[is.na(countSexo$n)] <- 0

        respuestas_Sexo <- data.frame(Sexo = c(countSexo$sexoPersona), value = c(countSexo$n))

        respuestas_Sexo$p <- paste0(round(respuestas_Sexo$value * 100 / sum(respuestas_Sexo$value),1), "%")

        ggplotly(
        ggplot(respuestas_Sexo, aes(x=Sexo, y=value, fill=Sexo)) +
            geom_bar(stat="identity", aes(text=sprintf("Categoría: %s<br>Frecuencia: %s", Sexo,value))) + 
            xlab("Categoría") +
            ylab("Frecuencia") +
            theme(axis.text.x = element_blank(), panel.background = element_blank()) +
            scale_fill_manual(values = paleta) +
            geom_text(aes(label = p), vjust = 1) +
            labs(fill = ""), tooltip = "text")
    })

    tableLocalidad <- reactive({
        personas <- personasF()
        countLoc <- personas %>% count(localidadResidenciaPersona) %>%
            mutate(freq = paste((n / sum(n))*100, "%", sep="")) %>% 
            arrange(desc(freq))
        colnames(countLoc)[1] <- "locations"

        data.frame(Localidad = c(countLoc$locations), Conteo = c(countLoc$n), Porcentaje = c(countLoc$freq))
    })

    output$personasxLocalidad <- renderTable({
        tableLocalidad()
    })

    output$downloadXLSXLocalidad <- downloadHandler(
        filename = function() {
            paste("localidad-", Sys.Date(), ".xlsx", sep="")
        },
        content = function(file) {
            data <- tableLocalidad()
            write_xlsx(data, path = file)
        }
    )

    imageLocalidad <- reactive({
        df <- tableLocalidad()
        myTable <- tableGrob(
            df, 
            rows = NULL, 
            theme = ttheme_default(core = list(bg_params = list(fill = "grey99")))
        )
        img <- grid.draw(myTable)
        img
    })

    output$downloadImageLocalidad <- downloadHandler(
        filename = function() {
            paste("localidad-", Sys.Date(), ".jpeg", sep="")
        },
        contentType = "image/jpeg",
        content = function(file) {
            png(file)
            imageLocalidad()
            dev.off()
        }
    )

    tableEstado <- reactive({
        personas <- personasF()
        countEst <- personas %>% count(personas$estadoResidenciaPersona) %>%
            mutate(freq = paste((n / sum(n))*100, "%", sep="")) %>% 
            arrange(desc(freq))
        colnames(countEst)[1] <- "locations"

        data.frame(Estado = c(countEst$locations), Conteo = c(countEst$n), Porcentaje = c(countEst$freq))
    })

    output$personasxEstado <- renderTable({
        tableEstado()
    })

    output$downloadXLSXEstado <- downloadHandler(
        filename = function() {
            paste("estado-", Sys.Date(), ".xlsx", sep="")
        },
        content = function(file) {
            data <- tableEstado()
            write_xlsx(data, path = file)
        }
    )

    imageEstado <- reactive({
        df <- tableEstado()
        myTable <- tableGrob(
            df, 
            rows = NULL, 
            theme = ttheme_default(core = list(bg_params = list(fill = "grey99")))
        )
        img <- grid.draw(myTable)
        img
    })

    output$downloadImageEstado <- downloadHandler(
        filename = function() {
            paste("estado-", Sys.Date(), ".jpeg", sep="")
        },
        contentType = "image/jpeg",
        content = function(file) {
            png(file)
            imageEstado()
            dev.off()
        }
    )

    tablePais <- reactive({
        personas <- personasF()
        countPais <- personas %>% count(personas$paisResidenciaPersona) %>%
            mutate(freq = paste((n / sum(n))*100, "%", sep="")) %>% 
            arrange(desc(freq))
        colnames(countPais)[1] <- "locations"

        data.frame(Pais = c(countPais$locations), Conteo = c(countPais$n), Porcentaje = c(countPais$freq))
    })

    output$personasxPais <- renderTable({
        tablePais()
    })

    output$downloadXLSXPais <- downloadHandler(
        filename = function() {
            paste("pais-", Sys.Date(), ".xlsx", sep="")
        },
        content = function(file) {
            data <- tablePais()
            write_xlsx(data, path = file)
        }
    )

    imagePais <- reactive({
        df <- tablePais()
        myTable <- tableGrob(
            df, 
            rows = NULL, 
            theme = ttheme_default(core = list(bg_params = list(fill = "grey99")))
        )
        img <- grid.draw(myTable)
        img
    })

    output$downloadImagePais <- downloadHandler(
        filename = function() {
            paste("pais-", Sys.Date(), ".jpeg", sep="")
        },
        contentType = "image/jpeg",
        content = function(file) {
            png(file)
            imagePais()
            dev.off()
        }
    )

    output$personasxDiscapacidad <- renderPlotly({
        personas <- personasF()
        personas_Discapacidad_si <- sum(!is.na(personas$siDiscapacidadPersona))
        personas_Discapacidad_no <- sum(!is.na(personas$noDiscapacidadPersona))

        respuestas_Discapacidad <- data.frame(Discapacidad = c("Si", "No"), value = c(personas_Discapacidad_si, personas_Discapacidad_no))

        respuestas_Discapacidad$p <- paste0(round(respuestas_Discapacidad$value * 100 / sum(respuestas_Discapacidad$value),1), "%")

        ggplotly(
        ggplot(respuestas_Discapacidad, aes(x=Discapacidad, y=value, fill = Discapacidad)) +
        geom_bar(stat="identity", aes(text=sprintf("Discapacidad: %s<br>Frecuencia: %s", Discapacidad, value))) + 
            xlab("") +
            ylab("Frecuencia") +
            scale_fill_manual(values=c('#56267d', '#2AB7CD')) +
            geom_text(aes(label = p), vjust = 1) +
            theme(legend.position = "none", panel.background = element_blank()), tooltip = "text")
    })

    output$personasxPueblos <- renderPlotly({
        personas <- personasF()
        personas_Pueblos_si <- sum(!is.na(personas$siPueblosPersona))
        personas_Pueblos_no <- sum(!is.na(personas$noPueblosPersona))

        respuestas_Pueblos <- data.frame(Pueblos = c("Si", "No"), value = c(personas_Pueblos_si, personas_Pueblos_no))
        
        respuestas_Pueblos$p <- paste0(round(respuestas_Pueblos$value * 100 / sum(respuestas_Pueblos$value),1), "%")

        ggplotly(
        ggplot(respuestas_Pueblos, aes(x=Pueblos, y=value, fill = Pueblos)) +
        geom_bar(stat="identity", aes(text=sprintf("Pueblos: %s<br>Frecuencia: %s", Pueblos, value))) + 
            xlab("") +
            ylab("Frecuencia") +
            scale_fill_manual(values=c('#56267d', '#2AB7CD')) +
            geom_text(aes(label = p), vjust = 1) +
            theme(legend.position = "none", panel.background = element_blank()), tooltip = "text")
    })

    output$personasxLengua_Indigena <- renderPlotly({
        personas <- personasF()
        personas_Lengua_Indigena_si <- sum(!is.na(personas$siLenguaIndigenaPersona))
        personas_Lengua_Indigena_no <- sum(!is.na(personas$noLenguaIndigenaPersona))

        respuestas_Lengua_Indigena <- data.frame(Lengua_Indigena = c("Si", "No"), value = c(personas_Lengua_Indigena_si, personas_Lengua_Indigena_no))

        respuestas_Lengua_Indigena$p <- paste0(round(respuestas_Lengua_Indigena$value * 100 / sum(respuestas_Lengua_Indigena$value),1), "%")

        ggplotly(
        ggplot(respuestas_Lengua_Indigena, aes(x=Lengua_Indigena, y=value, fill = Lengua_Indigena)) +
        geom_bar(stat="identity", aes(text=sprintf("Lengua Indigena: %s<br>Frecuencia: %s", Lengua_Indigena, value))) + 
            xlab("") +
            ylab("Frecuencia") +
            geom_text(aes(label = p), vjust = 1) +
            scale_fill_manual(values=c('#56267d', '#2AB7CD')) +
            theme(legend.position = "none", panel.background = element_blank()), tooltip = "text")
    })

    output$personasxServicio <- renderPlotly({
        personas <- personasF()
        primerServ <- count(filter(personas, servicioPersona == 1))
        segundoServ <- count(filter(personas, servicioPersona == 2))
        tercerServ <- count(filter(personas, servicioPersona == 3))
        otrosServ <- count(filter(personas, servicioPersona > 3))
        
        respuestas_Tipo_Servicio <- data.frame(Tipo_Servicio = c("Atención en crisis", "Asesoría psicológica", "Canalización", "Otros"), value = c(primerServ$n,segundoServ$n,tercerServ$n,otrosServ$n))

        respuestas_Tipo_Servicio$p <- paste0(round(respuestas_Tipo_Servicio$value * 100 / sum(respuestas_Tipo_Servicio$value),1), "%")
        
        ggplotly(
            ggplot(respuestas_Tipo_Servicio, aes(x= reorder(Tipo_Servicio, -value), y=value, fill = Tipo_Servicio)) +
            geom_bar(stat="identity", aes(text=sprintf("Tipo de servicio: %s<br>Frecuencia: %s", Tipo_Servicio, value))) + 
            ylab("Frecuencia") +
            xlab("Servicios") +
            geom_text(aes(label = p), vjust = 1) +
            scale_fill_manual(values = paleta) +
            theme(axis.text.x = element_blank(),
            panel.background = element_blank()) +
            labs(fill = ""), tooltip = "text")
    })
    
    ## SECTION D
    ### gráfica de edad del agresor
    output$edades_agresor_grf <- renderPlotly({
        riesgos <- riesgosF()
        ## cambio de id y renomabriemnto de columna
        colnames(riesgos)[4] <- "edad_agresor"

        ## obtener rangos de edades de los agresores
        riesgos$edad_agresor <- as.numeric(riesgos$edad_agresor)

        labels_rangos <- data.frame("rango_edades_agresor" = c("0 a 5","6 a 12","13 a 18","19 a 25","26 a 29","30 a 59","más de 60"))
        rango_edades_agresor <- cut(riesgos$edad_agresor, breaks = c(-1,5,12,18,25,29,59,Inf), labels = labels_rangos$rango_edades_agresor)
        ## añadir los rangos de edades de los agresores a la tabla riesgos
        riesgos$rango_edades_agresor <- rango_edades_agresor
        rangos_edades_agresor <- riesgos %>% count(rango_edades_agresor)

        rangos_edades_agresor <- left_join(labels_rangos,rangos_edades_agresor, by="rango_edades_agresor")
        rangos_edades_agresor$n[is.na(rangos_edades_agresor$n)] <- 0

        rangos_edades_agresor$p <- paste0(round(rangos_edades_agresor$n * 100 / sum(rangos_edades_agresor$n),1), "%")

        ggplotly(
            ggplot(rangos_edades_agresor, aes(x = factor(rango_edades_agresor, level = labels_rangos$rango_edades_agresor), y = n)) +
            geom_bar(stat = "identity", width = 1, color = '#56267d', fill = '#56267d', aes(text=sprintf("Edad: %s<br>Frecuencia: %s",rango_edades_agresor,n))) +
            theme(legend.position = "none", panel.background = element_blank()) +
            xlab("Años") +
            geom_text(aes(label = p), vjust = 1) +
            ylab("Frecuencia"), tooltip = "text")
    })

    ### gráfica de sexo del agresor
    output$sexo_agresor_grf <- renderPlotly({
        riesgos <- riesgosF()
        colnames(riesgos)[5] <- "id_sexo"
        colnames(sexos)[2] <- "sexos_agresor_catalogo"

        ### conteo de los datos de la columna id_sexo
        conteo_sexos_agresor <- riesgos %>% count(id_sexo)

        ### unión de las tablas riesgos y sexos por el campo id_sexo
        sexos_agresor_data <- right_join(conteo_sexos_agresor, sexos)
        sexos_agresor_data$n[is.na(sexos_agresor_data$n)] <- 0

        sexos_agresor_data$p <- paste0(round(sexos_agresor_data$n * 100 / sum(sexos_agresor_data$n),1), "%")

        ggplotly(
            ggplot(sexos_agresor_data, aes(x = sexos_agresor_catalogo, y = n, fill = sexos_agresor_catalogo)) +
            geom_bar(stat="identity", aes(text=sprintf("Categoría: %s<br>Frecuencia: %s", sexos_agresor_catalogo,n))) + 
            xlab("Categoría") +
            ylab("Frecuencia") +
            geom_text(aes(label = p), vjust = 1) +
            theme(axis.text.x = element_blank(), panel.background = element_blank()) +
            scale_fill_manual(values = paleta) +
            labs(fill = ""), tooltip = "text")
    })

    ### gráfica: ¿La dirección del agresor/a es la misma de quien solicita el servicio?

    output$misma_dir_agresor_victima <- renderPlotly({
        riesgos <- riesgosF()
        dir <- riesgos[c("siDireccionAgresor","noDireccionAgresor")]

        count_si_dirs <- dir %>% filter(!is.na(siDireccionAgresor)) %>% count(siDireccionAgresor)
        count_no_dirs <- dir %>% filter(!is.na(noDireccionAgresor)) %>% count(noDireccionAgresor)

        count_total <- data.frame("Misma_direccion" = c("Si","No"),
                                "n" = c(count_si_dirs$n,count_no_dirs$n))

        count_total$p <- paste0(round(count_total$n * 100 / sum(count_total$n),1), "%")

        ggplotly(
            ggplot(count_total,aes(x=Misma_direccion, y=n, fill=Misma_direccion)) +
                geom_bar(stat="identity", aes(text=sprintf("Misma dirección: %s<br>Frecuencia: %s", Misma_direccion, n))) + 
                xlab("") +
                geom_text(aes(label = p), vjust = 1) +
                ylab("Frecuencia") +
                scale_fill_manual(values=c('#56267d', '#2AB7CD')) +
                theme(legend.position = "none", panel.background = element_blank()), tooltip = "text")
    })



    ### ¿La persona agresora cuenta con una red de apoyo?
    
    output$red_apoyo_agresor <- renderPlotly({
        riesgos <- riesgosF()
        red_apoyo <- riesgos[c("siRedApoyo","noRedApoyo")]
        count_si_redAp <- red_apoyo %>% filter(!is.na(siRedApoyo)) %>% count(siRedApoyo)
        count_no_redAp <- red_apoyo %>% filter(!is.na(noRedApoyo)) %>% count(noRedApoyo)

        total_red_apoyo <- data.frame("Red_apoyo" = c("Si","No"),
                            "n" = c(count_si_redAp$n, count_no_redAp$n))

        total_red_apoyo$p <- paste0(round(total_red_apoyo$n * 100 / sum(total_red_apoyo$n),1), "%")

        ggplotly(
            ggplot(total_red_apoyo, aes(x=Red_apoyo, y = n, fill=Red_apoyo)) +
            geom_bar(stat="identity", aes(text=sprintf("Red de apoyo: %s<br>Frecuencia: %s", Red_apoyo, n))) + 
                xlab("") +
                geom_text(aes(label = p), vjust = 1) +
                ylab("Frecuencia") +
                scale_fill_manual(values=c('#56267d', '#2AB7CD')) +
                theme(legend.position = "none", panel.background = element_blank()), tooltip = "text")
    })



    ########################        DASHBOARD 3

    ##SECTION A

    output$presencial <- renderPlotly({
        # Presencial?
        encuesta_satisfaccion <- encuesta_satisfaccionF()
        sipresencial <- sum(!is.na(encuesta_satisfaccion$sipresencial))
        nopresencial <- sum(!is.na(encuesta_satisfaccion$nopresencial))

        presencial <- data.frame(category = c("Si", "No"), value = c(sipresencial, nopresencial))

        presencial$p <- paste0(round(presencial$value * 100 / sum(presencial$value),1), "%")

        ggplotly(
            ggplot(presencial, aes(x=category, y=value, fill = category)) +
                geom_bar(stat="identity", aes(text=sprintf("Presencial: %s<br>Frecuencia: %s", category, value))) + 
                xlab("") +
                geom_text(aes(label = p), vjust = 1) +
                ylab("Frecuencia") +
                theme(legend.position = "none", panel.background = element_blank()) +
                scale_fill_manual(values=c('#56267d', '#2AB7CD')), tooltip = "text")
    })
    
    # calificacion instalaciones
    output$califInstalaciones<- renderPlotly({
        encuesta_satisfaccion <- encuesta_satisfaccionF()

        calif_instalaciones <- encuesta_satisfaccion %>% group_by(instalaciones) %>%
            filter(!is.na(instalaciones)) %>% summarize(n=n())
        
        califs <- data.frame("instalaciones" = c("1","2","3","4","5"))

        calif_instalaciones <- left_join(califs,calif_instalaciones)
        calif_instalaciones[is.na(calif_instalaciones)] <- 0

        calif_instalaciones$p <- paste0(round(calif_instalaciones$n * 100 / sum(calif_instalaciones$n),1), "%")

        ggplotly(
            ggplot(calif_instalaciones, aes(x = instalaciones, y = n)) +
                geom_bar(stat = "identity", width = 1, color = '#56267d', fill = '#56267d', aes(text=sprintf("Calificación: %s<br>Frecuencia: %s", instalaciones, n))) +
                xlab("Calificación") +
                ylab("Frecuencia") +
                geom_text(aes(label = p), vjust = 1) +
                theme(legend.position = "none", panel.background = element_blank()), tooltip = "text")
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

        cambios$p <- paste0(round(cambios$n * 100 / sum(cambios$n),1), "%")

        ggplotly(
            ggplot(cambios, aes(x = Cambios, y = n, fill = Cambios)) +
                geom_bar(stat = "identity", aes(text=sprintf("Cambios: %s<br>Frecuencia: %s", Cambios, n))) +
                theme(legend.position = "none", panel.background = element_blank()) +
                xlab("") +
                geom_text(aes(label = p), vjust = 1) +
                ylab("Frecuencia") +
                scale_fill_manual(values=c('#56267d', '#2AB7CD')), tooltip = "text")
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

        recur_serv$p <- paste0(round(recur_serv$n * 100 / sum(recur_serv$n),1), "%")

        ggplotly(
            ggplot(recur_serv, aes(x = Recurriria, y = n, fill = Recurriria)) +
                geom_bar(stat = "identity", aes(text=sprintf("Recurriría: %s<br>Frecuencia: %s", Recurriria, n))) +
                theme(legend.position = "none", panel.background = element_blank()) +
                xlab("") +
                ylab("Frecuencia") +
                geom_text(aes(label = p), vjust = 1) +
                scale_fill_manual(values=c('#56267d', '#2AB7CD')), tooltip = "text")
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

        mismoPsico$p <- paste0(round(mismoPsico$n * 100 / sum(mismoPsico$n),1), "%")

        ggplotly(
            ggplot(mismoPsico, aes(x = Mismo, y = n, fill = Mismo)) +
                geom_bar(stat = "identity", aes(text=sprintf("Mimso Psicoterapeuta: %s<br>Frecuencia: %s", Mismo, n))) +
                theme(legend.position = "none", panel.background = element_blank()) +
                xlab("") +
                ylab("Frecuencia") +
                geom_text(aes(label = p), vjust = 1) +
                scale_fill_manual(values=c('#56267d', '#2AB7CD')), tooltip = "text")
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

        recomendacion$p <- paste0(round(recomendacion$n * 100 / sum(recomendacion$n),1), "%")

        ggplotly(
            ggplot(recomendacion, aes(x = Recomendaria, y = n, fill = Recomendaria)) +
                geom_bar(stat = "identity", aes(text=sprintf("Recomendaría: %s<br>Frecuencia: %s",Recomendaria,n))) +
                theme(legend.position = "none", panel.background = element_blank()) +
                xlab("") +
                ylab("Frecuencia") +
                geom_text(aes(label = p), vjust = 1) +
                scale_fill_manual(values=c('#56267d', '#2AB7CD')), tooltip = "text")
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

        canalizado$p <- paste0(round(canalizado$n * 100 / sum(canalizado$n),1), "%")

        ggplotly(
            ggplot(canalizado, aes(x = Canalizado, y = n, fill = Canalizado)) +
                geom_bar(stat = "identity", aes(text=sprintf("Canalizado: %s<br>Frecuencia: %s",Canalizado,n))) +
                theme(legend.position = "none", panel.background = element_blank()) +
                xlab("") +
                ylab("Frecuencia") +
                geom_text(aes(label = p), vjust = 1) +
                scale_fill_manual(values=c('#56267d', '#2AB7CD')), tooltip = "text")
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

        rangos_edades$p <- paste0(round(rangos_edades$n * 100 / sum(rangos_edades$n),1), "%")

        ggplotly(
            ggplot(rangos_edades, aes(x = factor(rango_edades, level = labels_rangos$rango_edades), y = n)) + 
                geom_bar(stat = "identity", width = 1, color = '#56267d', fill = '#56267d', aes(text=sprintf("Edad: %s<br>Frecuencia: %s",rango_edades,n))) +
                theme(legend.position = "none", panel.background = element_blank()) +
                xlab("Años") +
                ylab("Frecuencia") +
                geom_text(aes(label = p), vjust = 1) +
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

        sexosPersonaS$p <- paste0(round(sexosPersonaS$n * 100 / sum(sexosPersonaS$n),1), "%")

        ggplotly(
            ggplot(sexosPersonaS, aes(x = Sexo, y = n, fill = Sexo)) +
                geom_bar(stat = "identity", aes(text=sprintf("Instancia: %s<br>Frecuencia: %s",Sexo,n))) +
                theme(axis.text.x = element_blank(), panel.background = element_blank()) +
                xlab(element_blank()) +
                ylab("Frecuencia") +
                geom_text(aes(label = p), vjust = 1) +
                scale_fill_manual(values = paleta) +
                labs(fill = "Instancia"), tooltip = "text")
    })

    
    # frecuencia de canalizacion a instancias
    output$frecInstanciasCanalizadas <- renderPlotly({
        encuesta_satisfaccion <- encuesta_satisfaccionF()
        # filtramos los que fueron canalizados
        personasCanalizadas <- encuesta_satisfaccion %>% filter(sicanalizado==1)
        count_intancias <- personasCanalizadas %>% group_by(institucion) %>% summarise(n = n())
        colnames(count_intancias)[1] <- "id_instancia"

        instancia <- instancia[!instancia$nombre == "CIAM",]

        count_instancias <- right_join(count_intancias,instancia)
        count_instancias[is.na(count_instancias)] <- 0
        colnames(count_instancias)[3] <- "nomInstancia"

        count_instancias$p <- paste0(round(count_instancias$n * 100 / sum(count_instancias$n),1), "%")

        ggplotly(
            ggplot(count_instancias, aes(x = reorder(nomInstancia,-n), y = n, fill = nomInstancia)) +
                geom_bar(stat = "identity", aes(text=sprintf("Instancia: %s<br>Frecuencia: %s",nomInstancia,n))) +
                theme(axis.text.x = element_blank(), panel.background = element_blank()) +
                xlab(element_blank()) +
                ylab("Frecuencia") +
                scale_fill_manual(values = paleta) +
                geom_text(aes(label = p), vjust = 1) +
                labs(fill = "Instancia"), tooltip = "text")
    })

    ##SECCION D

    output$satisfaccionxServicio <- renderPlotly({
        encuesta_satisfaccion <- encuesta_satisfaccionF()
        colnames(instancia)[2] <- "institucion"
        colnames(encuesta_satisfaccion)[23] <- "id_instancia"

        instancia <- instancia[!instancia$institucion == "CIAM",]
        
        #instancia_new <- instancia %>% group_by(id_instancia) %>% slice(1)
        
        satisfaccion_data <- merge(encuesta_satisfaccion, instancia, by="id_instancia")
        califxServicio <- satisfaccion_data %>% group_by(institucion) %>%
            summarize(promedio = round(mean(as.numeric(calificacionInstancia)), 2)) %>%
            arrange(desc(promedio)) %>%
            mutate(institucion = factor(institucion, levels = institucion))

        #print("2\n", califxServicio)

        #colnames(califxServicio)[1] <- "institucion"

        todasInstanciasCalif <- left_join(instancia,califxServicio)
        todasInstanciasCalif$promedio[is.na(todasInstanciasCalif$promedio)] <- 0

        #print("3\n", todasInstanciasCalif)

        respuestas_Institucion <- data.frame(Institucion = c(todasInstanciasCalif$institucion), value = c(todasInstanciasCalif$promedio))

        respuestas_Institucion$p <- paste0(round(respuestas_Institucion$value * 100 / sum(respuestas_Institucion$value),1), "%")

        #print("4\n", respuestas_Institucion)

        ggplotly(
            ggplot(respuestas_Institucion, aes(x= reorder(Institucion, value), y=value, fill = Institucion)) +
                geom_bar(stat = "identity", aes(text=sprintf("Institucion: %s<br>Frecuencia: %s",Institucion,value))) +
                theme(axis.text.y = element_blank()) +
                theme(legend.position = "bottom") +
                theme(legend.title = element_blank(), panel.background = element_blank()) +
                xlab("") +
                ylab("Promedio de calificación")+
                scale_fill_manual(values = paleta) +
                geom_text(aes(label = p), vjust = 1) +
                coord_flip(), tooltip = "text")
    })

    output$satisfaccionxUtil <- renderPlotly({
        encuesta_satisfaccion <- encuesta_satisfaccionF()
        encuesta_satisfaccion_Util_si <- sum(!is.na(encuesta_satisfaccion$siutil))
        encuesta_satisfaccion_Util_no <- sum(!is.na(encuesta_satisfaccion$noutil))

        respuestas_Util <- data.frame(Util = c("Si", "No"), value = c(encuesta_satisfaccion_Util_si, encuesta_satisfaccion_Util_no))

        respuestas_Util$p <- paste0(round(respuestas_Util$value * 100 / sum(respuestas_Util$value),1), "%")

        ggplotly(
        ggplot(respuestas_Util, aes(x=Util, y=value, fill = Util)) +
        geom_bar(stat = "identity", aes(text=sprintf("Util: %s<br>Frecuencia: %s",Util,value))) +
            theme(legend.position="none", panel.background = element_blank()) +
            xlab("")+
            ylab("Frecuencia")+
            geom_text(aes(label = p), vjust = 1) +
            scale_fill_manual(values=c('#56267d', '#2AB7CD')), tooltip = "text")
    })

    ##SECTION E
    output$sesiones <- renderPlotly({
        encuesta_satisfaccion <- encuesta_satisfaccionF()
        hist_sesiones <- encuesta_satisfaccion %>% count(cantidad_sesiones)
        # creamos rango
        mini <- min(hist_sesiones$cantidad_sesiones)
        maxi <- max(hist_sesiones$cantidad_sesiones)
        cantidad_sesiones <- data.frame("cantidad_sesiones"=as.character(c(mini:maxi)))

        hist_sesiones <- left_join(cantidad_sesiones,hist_sesiones)
        hist_sesiones$n[is.na(hist_sesiones$n)] <- 0

        hist_sesiones$p <- paste0(round(hist_sesiones$n * 100 / sum(hist_sesiones$n),1), "%")

        ggplotly(
            ggplot(hist_sesiones, aes(x = cantidad_sesiones, y = n)) +
            geom_bar(stat = "identity", color = "#56267d", fill = "#56267d", width = 1, aes(text = sprintf("Cantidad sesiones: %s<br>Frecuencia: %s", cantidad_sesiones, n))) +
            xlab("Cantidad de sesiones") +
            ylab("Frecuencia") +
            geom_text(aes(label = p), vjust = 1) +
            theme(panel.background = element_blank()), tooltip = "text"
        )
    })

    output$sesionesxEdades <- renderPlotly({
        encuesta_satisfaccion <- encuesta_satisfaccionF()

        labels_rangos <- data.frame("rango_edades" = c("0 a 5","6 a 12","13 a 18","19 a 25","26 a 29","30 a 59","más de 60"))
        encuesta_satisfaccion$edad <- as.numeric(encuesta_satisfaccion$edad)

        rango_edades <- cut(encuesta_satisfaccion$edad, breaks = c(-1,5,12,18,25,29,59,Inf), labels = labels_rangos$rango_edades)
        encuesta_satisfaccion$rango_edades <- rango_edades

        # creamos rango
        mini <- min(encuesta_satisfaccion$cantidad_sesiones)
        maxi <- max(encuesta_satisfaccion$cantidad_sesiones)
        cantidad_sesiones <- data.frame("cantidad_sesiones"=as.character(c(mini:maxi)))

        count_sesiones_edades <- encuesta_satisfaccion %>% select(rango_edades, cantidad_sesiones) %>%
            group_by(rango_edades,cantidad_sesiones) %>%
            mutate(n = n())

        edad_cs <-merge(labels_rangos, cantidad_sesiones)

        edad_cs_count <- left_join(edad_cs, count_sesiones_edades, by = c("rango_edades","cantidad_sesiones"))
        edad_cs_count$n[is.na(edad_cs_count$n)] <- 0

        edad_cs_count$p <- paste0(round(edad_cs_count$n * 100 * edad_cs_count$n / sum(edad_cs_count$n),0), "%")

        ggplotly(
            ggplot(edad_cs_count, aes(x = factor(rango_edades, level = labels_rangos$rango_edades), y = n, fill = cantidad_sesiones)) +
            geom_bar(stat = "identity", position=position_dodge(), aes(text=sprintf("Cantidad de sesiones: %s<br>Rango de Edad: %s<br>Cantidad: %s", cantidad_sesiones, rango_edades, n))) + 
            theme(panel.background = element_blank()) +
            xlab("") +
            ylab("Frecuencia") +
            labs(fill = "Sesiones") +
            scale_fill_manual(values = paleta) +
            geom_text(aes(label = p), vjust = 1, position = position_dodge(width = .9), size = 3), tooltip = "text")
    })

    output$sesionesxSexo <- renderPlotly({
        encuesta_satisfaccioni <- encuesta_satisfaccionF()

        personas_copy <- personasF()[,c("id_folio","sexoPersona")]
        #personas_copy <- personas[,c("id_folio","sexoPersona")]
        encuesta_satisfaccion <- left_join(x = encuesta_satisfaccioni, y = personas_copy, by = "id_folio") %>% group_by(id_folio) %>% slice(1)
        colnames(encuesta_satisfaccion)[28] <- "id_sexo"
        colnames(sexos)[2] <- "sexoPersona"
        
        # creamos rango
        mini <- min(encuesta_satisfaccion$cantidad_sesiones)
        maxi <- max(encuesta_satisfaccion$cantidad_sesiones)
        cantidad_sesiones <- data.frame("cantidad_sesiones"=as.character(c(mini:maxi)))

        count_sesiones_sexo <- encuesta_satisfaccion %>% select(id_folio, id_sexo,cantidad_sesiones) %>%
            group_by(id_sexo,cantidad_sesiones) %>%
            mutate(n = n())

        sex_cs <-merge(sexos, cantidad_sesiones)

        sex_cs_count <- left_join(sex_cs, count_sesiones_sexo, by = c("id_sexo","cantidad_sesiones"))
        sex_cs_count$n[is.na(sex_cs_count$n)] <- 0

        sex_cs_count$p <- paste0(round(sex_cs_count$n * 100 * sex_cs_count$n / sum(sex_cs_count$n),1), "%")

        ggplotly(
            ggplot(sex_cs_count, aes(x = sexoPersona, y = n, fill = cantidad_sesiones)) +
            geom_bar(stat = "identity", position=position_dodge(), aes(text=sprintf("Cantidad de sesiones: %s<br>Sexo: %s<br>Cantidad: %s", cantidad_sesiones, sexoPersona, n))) + 
            theme(panel.background = element_blank()) +
            xlab("") +
            ylab("Frecuencia") +
            labs(fill = "Sesiones") +
            scale_fill_manual(values = paleta) +
            geom_text(aes(label = p), vjust = 1, position = position_dodge(width = .9)), tooltip = "text")
    })
    ##SECTION F

    #Principal: 6. ¿Recibió el servicio de Acompañamiento Emocional oportunamente (en el momento indicado) y de manera pronta?
    output$si_servicio <- renderPlotly({
        encuesta_satisfaccion <- encuesta_satisfaccionF()

        calif_oportunoPront <- encuesta_satisfaccion %>% group_by(oportunoPronto) %>%
            filter(!is.na(oportunoPronto)) %>% summarize(n=n())

        califs <- data.frame("oportunoPronto" = c("1","2","3","4","5"))

        calif_oportunoPront <- left_join(califs,calif_oportunoPront)
        calif_oportunoPront[is.na(calif_oportunoPront)] <- 0

        calif_oportunoPront$p <- paste0(round(calif_oportunoPront$n * 100 / sum(calif_oportunoPront$n),1), "%")

        ggplotly(
            ggplot(calif_oportunoPront, aes(x = oportunoPronto, y = n)) +
                geom_bar(stat = "identity", width = 1, color = '#56267d', fill = '#56267d', aes(text=sprintf("Calificación: %s<br>Frecuencia: %s", oportunoPronto, n))) +
                xlab("Calificación") +
                ylab("Frecuencia") +
                geom_text(aes(label = p), vjust = 1) +
                theme(legend.position = "none", panel.background = element_blank()), tooltip = "text")
    })

    #12. ¿Qué tan importante y necesario fue para usted recibir el Servicio de Acompañamiento Emocional?

    ##SECTION G

    output$servicio_importante <- renderPlotly({
        encuesta_satisfaccion <- encuesta_satisfaccionF()

        calif_importante <- encuesta_satisfaccion %>% group_by(importante) %>%
            filter(!is.na(importante)) %>% summarize(n=n())

        califs <- data.frame("importante" = c("1","2","3","4","5"))

        calif_importante <- left_join(califs,calif_importante)
        calif_importante[is.na(calif_importante)] <- 0

        calif_importante$p <- paste0(round(calif_importante$n * 100 / sum(calif_importante$n),1), "%")

        ggplotly(
            ggplot(calif_importante, aes(x = importante, y = n)) +
                geom_bar(stat = "identity", width = 1, color = '#56267d', fill = '#56267d', aes(text=sprintf("Calificación: %s<br>Frecuencia: %s", importante, n))) +
                xlab("Calificación") +
                ylab("Frecuencia") +
                geom_text(aes(label = p), vjust = 1) +
                theme(legend.position = "none", panel.background = element_blank()), tooltip = "text")
    })

    output$importantexOportuno <- renderPlotly({
        encuesta_satisfaccion <- encuesta_satisfaccionF()
        encuesta_satisfaccion <- encuesta_satisfaccion %>% group_by(importante, oportunoPronto) %>%
            mutate(n = n())
        imp <- data.frame("importante"=c("1","2","3","4","5"))
        opoP <- data.frame("oportunoPronto"=c("1","2","3","4","5"))

        impOpop <- merge(imp,opoP)

        enc_s <- left_join(impOpop,encuesta_satisfaccion, by=c("importante","oportunoPronto"))
        enc_s$n[is.na(enc_s$n)] <- 0
        
        cols <- RColorBrewer::brewer.pal(3,'Purples')[c(1,3)]

        enc_s$p <- paste0(round(enc_s$n * 100 / sum(enc_s$n),1), "%")

        ggplotly(
            ggplot(enc_s, aes(x = importante, y = oportunoPronto, fill = n)) +
            geom_tile(aes(text=sprintf("Respuestas: %s",n))) +
            xlab("Importante") +
            ylab("Oportuno y Pronto") +
            labs(fill = "Cantidad respuestas") +
            theme(panel.background = element_blank()) +
            scale_fill_gradient(low=cols[1],high=cols[2]), tooltip = "text"
        )
    })

    ##SECTION F

    #7. ¿Cómo califica la vía de atención, se adaptó a sus necesidades? (La vía de atención se da por llamadas telefónicas, mensajes de texto y WhatsApp, presencial, etc.)

    output$via_atencion <- renderPlotly({
        conteo_via_atencion <- encuesta_satisfaccionF() %>% count(viaAdapto)

        califs <- data.frame("viaAdapto" = c("1","2","3","4","5"))

        conteo_via_atencion <- left_join(califs,conteo_via_atencion)
        conteo_via_atencion[is.na(conteo_via_atencion)] <- 0

        conteo_via_atencion$p <- paste0(round(conteo_via_atencion$n * 100 / sum(conteo_via_atencion$n),1), "%")

        ggplotly(
            ggplot(conteo_via_atencion, aes(x = viaAdapto, y = n)) + 
            geom_bar(stat = "identity", width = 1, color = '#56267d', fill = '#56267d', aes(text=sprintf("Calificación: %s<br>Frecuencia: %s", viaAdapto, n))) +
                xlab("Calificación") +
                ylab("Frecuencia") +
                geom_text(aes(label = p), vjust = 1) +
                theme(legend.position = "none", panel.background = element_blank()), tooltip = "text")
    })


    #8. ¿Cómo califica la confianza y seguridad que le hicieron sentir durante la atención?
    output$calificacion_confianza_seguridad <- renderPlotly({
        conteo_confianza_seguridad <- encuesta_satisfaccionF() %>% count(confianzaSeguridad)

        califs <- data.frame("confianzaSeguridad" = c("1","2","3","4","5"))

        conteo_confianza_seguridad <- left_join(califs,conteo_confianza_seguridad)
        conteo_confianza_seguridad[is.na(conteo_confianza_seguridad)] <- 0

        conteo_confianza_seguridad$p <- paste0(round(conteo_confianza_seguridad$n * 100 / sum(conteo_confianza_seguridad$n),1), "%")

        ggplotly(
            ggplot(conteo_confianza_seguridad, aes(x = confianzaSeguridad, y = n, fill = confianzaSeguridad)) +
            geom_bar(stat = "identity", width = 1, color = '#56267d', fill = '#56267d', aes(text=sprintf("Calificación: %s<br>Frecuencia: %s", confianzaSeguridad, n))) +
                xlab("Calificación") +
                ylab("Frecuencia") +
                geom_text(aes(label = p), vjust = 1) +
                theme(legend.position = "none", panel.background = element_blank()), tooltip = "text")
    })

    #9. Califique por favor el respeto con el que sintió que fue tratada/o durante el Acompañamiento Emocional:
    output$respeto_sesiones <- renderPlotly({
        conteo_respeto <- encuesta_satisfaccionF() %>% count(respeto) 

        califs <- data.frame("respeto" = c("1","2","3","4","5"))

        conteo_respeto <- left_join(califs,conteo_respeto)
        conteo_respeto[is.na(conteo_respeto)] <- 0

        conteo_respeto$p <- paste0(round(conteo_respeto$n * 100 / sum(conteo_respeto$n),1), "%")

        ggplotly(
            ggplot(conteo_respeto, aes(x = respeto, y = n)) +
            geom_bar(stat = "identity", width = 1, color = '#56267d', fill = '#56267d', aes(text=sprintf("Calificación: %s<br>Frecuencia: %s", respeto, n))) +
                xlab("Calificación") +
                ylab("Frecuencia") +
                geom_text(aes(label = p), vjust = 1) +
                theme(legend.position = "none", panel.background = element_blank()), tooltip = "text")
    })

    #13. ¿Qué tan satisfactorio fue el trato que le brindó el equipo de Acompañamiento Emocional de CIAM?
    output$serv_satisfactorio <- renderPlotly({
        conteo_serv_satisfactorio <- encuesta_satisfaccionF() %>% count(satisfactorio)

        califs <- data.frame("satisfactorio" = c("1","2","3","4","5"))

        conteo_serv_satisfactorio <- left_join(califs,conteo_serv_satisfactorio)
        conteo_serv_satisfactorio[is.na(conteo_serv_satisfactorio)] <- 0

        conteo_serv_satisfactorio$p <- paste0(round(conteo_serv_satisfactorio$n * 100 / sum(conteo_serv_satisfactorio$n),1), "%")

        ggplotly(
            ggplot(conteo_serv_satisfactorio, aes(x = satisfactorio, y = n, fill = satisfactorio)) +
            geom_bar(stat = "identity", width = 1, color = '#56267d', fill = '#56267d', aes(text=sprintf("Calificación: %s<br>Frecuencia: %s", satisfactorio, n))) +
                xlab("Calificación") +
                ylab("Frecuencia") +
                geom_text(aes(label = p), vjust = 1) +
                theme(legend.position = "none", panel.background = element_blank()), tooltip = "text")
    })
}