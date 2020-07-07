library(shiny)
library(extrafont)
library(tidyverse)
library(viridis)
library(lubridate)
library(ggplot2)
library(shinythemes)
library(RColorBrewer)

casos_es <- read_csv("casos_es.csv")
source("ggplot_themes.R")
 
ui <- navbarPage(theme = shinytheme("cerulean"),
                 title = "Covid-19, España",
    sliderInput("valor_fecha",label = "Días", timeFormat = "%Y-%m-%d",
                min = ymd("2020-02-20"),
                max = ymd("2020-04-22"),
                value = ymd("2020-02-20"),
                width = "100%",
               animate = animationOptions(playButton = "Play", interval = 2*1000)),
    sidebarLayout(
        sidebarPanel(width = 2,
                     checkboxGroupInput("comunidId", label = "Comunidades Autónomas", 
                                        choices = unique(casos_es$Comunidades), 
                                        width = "100%", inline = FALSE,
                                        selected = c("Madrid", "Nacional", "Cataluña", "Canarias"))
                     ),
        mainPanel(width = 10,
                  tabsetPanel(
                  tabPanel("Evolucion de la infección nacional",
                           fluidRow(plotOutput("inf_area_nac", height = "300px")),
                           fluidRow(
                               column(plotOutput("casos_nuevos_nac", height = "220px"), width = 6),
                               column(plotOutput("casos_nuevos_pnac",height = "220px"), width = 6)
                           )),
                  tabPanel("Hopitalizados, cuidados intensivos y fallecidos",
                           fluidRow(plotOutput("fall_hosp_uci",height = "300px")),
                           fluidRow(
                               column(plotOutput("text_fallecidos", height = "220px"), width = 2),
                               column(plotOutput("por_text_letalidad", height = "220px"), width = 2),
                               column(plotOutput("text_hospital", height = "220px"), width = 2),
                               column(plotOutput("por_text_hospital", height = "220px"), width = 2),
                               column(plotOutput("uso_uci", height = "220px"), width = 2),
                               column(plotOutput("por_uso_uci", height = "220px"), width = 2)
                           )),
                  tabPanel("Evolucion de la infección por comunidad",
                           fluidRow(plotOutput("inf_lineal",height = "300px")),
                           fluidRow(
                               column(plotOutput("inf_comuna", height = "220px"), width = 6),
                               column(plotOutput("casos_nuevos",height = "220px"), width = 6)
                           )),
                  tabPanel("Fallecidos por comunidad",
                           fluidRow(
                               column(plotOutput("prop_fallecidos_com",height = "500px"),width = 6),
                               column(plotOutput("prop_mort_reloj",height = "500px"),width = 6),
                               fluidRow(
                                   column(plotOutput("mort_bar_comunidad",height = "200px"),width =12))
                               )
                    )  
                 
               )
            )
         )
    )


server <- function(input, output) {
    #filtro para gráfico de barras
    casos_es_dia <- reactive({
        casos_es %>% filter(Fecha == input$valor_fecha & Comunidades %in% input$comunidId)
    })
    casos_es_dia2 <- reactive({
        casos_es %>% filter(Fecha == input$valor_fecha)
    })
    casos_es_acum <- reactive({
        casos_es %>% filter(Fecha <= input$valor_fecha & Comunidades %in% input$comunidId)
    })
    casos_es_acum2 <- reactive({
        casos_es %>% filter(Fecha <= input$valor_fecha)
    })
    
    # TAB = "Evolucion de la infección nacional" #
    output$inf_area_nac <- renderPlot({
        casos_es_acum() %>% filter(Comunidades == "Nacional")%>%
            ggplot(aes(Fecha, Casos, fill = Comunidades))+
            geom_area(show.legend = T, alpha = 0.8)+
            scale_colour_viridis_d("Comunidades", option = "D",aesthetics = "fill")+
            scale_x_date(breaks = seq(min(casos_es$Fecha), max(casos_es$Fecha),by=7))+
            ggtitle("Evolución de casos, por día")+
            scale_y_continuous(position = "right")+
            sb_sober_theme_line+
            theme(axis.text.x = element_text(angle = 0))
    })
    output$casos_nuevos_nac <- renderPlot({
        casos_es_acum() %>%
            filter(Comunidades == "Nacional")%>%
            ggplot(aes(Fecha,nuevos_casos, colour = nuevos_casos) )+
            geom_line(show.legend = F, size = 1.1)+
            scale_colour_viridis_c("nuevos_casos", option = "A",aesthetics = "colour")+
            scale_x_date(breaks = seq(min(casos_es$Fecha), max(casos_es$Fecha),by=7))+
            ggtitle("Casos nuevos, por día")+
            scale_y_continuous(position = "right")+
            sb_sober_theme_line+
            theme(axis.text.x = element_text(angle = 0))
    })
    output$casos_nuevos_pnac <- renderPlot({
        casos_es_acum() %>% filter(Comunidades == "Nacional") %>%
            ggplot(aes(Fecha, y= (nuevos_casos/Casos)*100, fill = (nuevos_casos/Casos)*100))+
            geom_col(show.legend = F, size = 1)+
            scale_colour_distiller(palette = "Reds", aesthetics = "fill", direction = 1)+
            scale_x_date(breaks = seq(min(casos_es$Fecha), max(casos_es$Fecha),by=7))+
            ggtitle("Cambios porcentuales en infecciones por día")+
            sb_sober_theme_line+
            theme(axis.text.x = element_text(angle = 0))
    })
    
    #################################################
    
    # TAB = "Hopitalizados, cuidados intensivos y fallecidos" #
    output$fall_hosp_uci <- renderPlot({
        casos_es_acum2() %>% 
            filter(Comunidades == "Nacional") %>%
            ggplot(aes(Fecha, Hospitalizados))+
            geom_area(fill = brewer.pal(3,"Set1")[1], alpha = 0.6, colour = "white")+
            geom_area(aes(Fecha, Fallecidos), 
                      fill = brewer.pal(3,"Set1")[2], alpha = 0.6, colour = "white")+
            geom_area(aes(Fecha, UCI), 
                      fill = brewer.pal(3,"Set1")[3], alpha = 0.5, colour = "white")+
            scale_y_continuous(position = "right")+
            ggtitle("Hospitalizados, Cuidados Intensivos y Fallecidos")+
            sb_sober_theme_col
    })
    
    output$text_fallecidos <- renderPlot({
        casos_es_dia2() %>% 
            filter(Comunidades == "Nacional") %>%
            ggplot(aes(Fallecidos, y = ""))+
            geom_text(aes(label = Fallecidos), 
                      colour = brewer.pal(3,"Set1")[2], size = 16,
                      family = "Lato")+
            ggtitle("N° de Fallecidos")+
            sb_sober_theme_text
    })
    
    output$por_text_letalidad <- renderPlot({
        casos_es_dia2() %>%
            filter(Comunidades == "Nacional") %>%
            ggplot(aes(Fallecidos, y = ""))+
            geom_text(aes(label = paste0(round((Fallecidos/Casos)*100,2),"%")), 
                      colour = brewer.pal(3,"Set1")[2], size = 16,
                      family = "Lato")+
            ggtitle("Tasa de letalidad", subtitle = "Fallecidos/Casos")+
            sb_sober_theme_text
    })
    output$text_hospital <- renderPlot({
        casos_es_dia2() %>%
            filter(Comunidades == "Nacional") %>%
            ggplot(aes(Hospitalizados, y = ""))+
            geom_text(aes(label = Hospitalizados), 
                      colour = brewer.pal(3,"Set1")[1], size = 16,
                      family = "Lato")+
            ggtitle("N° de Hospitalizados")+
            sb_sober_theme_text
    })
    output$por_text_hospital <-renderPlot({
        casos_es_dia2() %>%
            filter(Comunidades == "Nacional") %>%
            ggplot(aes(Hospitalizados, y = ""))+
            geom_text(aes(label = paste0(round(Hospitalizados/Casos*100,2),"%")), 
                      colour = brewer.pal(3,"Set1")[1], size = 16,
                      family = "Lato")+
            ggtitle("% de Hospitalizados", subtitle = "Hospitalizados/Casos")+
            sb_sober_theme_text
    })
    output$uso_uci <- renderPlot({
        casos_es_dia2() %>%
            filter(Comunidades == "Nacional") %>%
            ggplot(aes(UCI, y = ""))+
            geom_text(aes(label = UCI), 
                      colour = brewer.pal(3,"Set1")[3], size = 16,
                      family = "Lato")+
            ggtitle("N° de camas usadas en UCI")+
            sb_sober_theme_text
    })
    output$por_uso_uci <- renderPlot({
        casos_es_dia2() %>%
            filter(Comunidades == "Nacional") %>%
            ggplot(aes(UCI, y = ""))+
            geom_text(aes(label = paste0(round(UCI/Hospitalizados*100,2),"%")), 
                      colour = brewer.pal(3,"Set1")[3], size = 16,
                      family = "Lato")+
            ggtitle("% de pacientes en UCI", "UCI/Hospitalizados")+
            sb_sober_theme_text
    })
        
    ################################################
    
    #GRAFICOS POR COMUNIDADES
    output$inf_lineal <- renderPlot({
        casos_es_acum() %>%
            ggplot(aes(Fecha, Casos))+
            geom_line(aes(color = Comunidades), size = 1.2, show.legend = T)+
            scale_colour_viridis_d("Comunidades", option = "D",aesthetics = "color")+
            scale_x_date(breaks = seq(min(casos_es$Fecha), max(casos_es$Fecha),by=7))+
            ggtitle("Evolución de casos, por día")+
            scale_y_continuous(position = "right")+
            sb_sober_theme_line+
            theme(axis.text.y.right = element_text())
    })
    
    output$inf_comuna <- renderPlot({
        casos_es_dia() %>%
            ggplot(aes(reorder(CCAA, -Casos),Casos))+
            geom_col(aes(fill = Comunidades), show.legend = F)+
            scale_colour_viridis_d("Comunidades", option = "D", aesthetics =  "fill")+
            geom_text(aes(label = Casos),
                      nudge_y = 4000, 
                      family = "Lato", 
                      fontface = "bold")+
            ggtitle("Casos por fecha segun la comunidad Autónoma")+
            ylab("Casos")+
            xlab("Comunidades Autónomas")+
            sb_sober_theme_col+
            theme(axis.text.x = element_text(angle = 0))
    })
    output$casos_nuevos <- renderPlot({
        casos_es_dia() %>%
            ggplot(aes(reorder(CCAA,-nuevos_casos),nuevos_casos))+
            geom_col(aes(fill = Comunidades), show.legend = F)+
            scale_colour_viridis_d("Comunidades", option = "D", aesthetics =  "fill")+
            geom_text(aes(label = nuevos_casos), 
                      nudge_y = 450, 
                      family = "Lato", 
                      fontface = "bold")+
            ggtitle("Nuevos casos por fecha segun la comunidad Autónoma")+
            ylab("Nuevos Casos diarios")+
            xlab("Comunidades Autónomas")+
            sb_sober_theme_col+
            theme(axis.text.x = element_text(angle = 0))
    })
    
    #GRAFICOS DE MORTALIDAD y letalidad
    output$prop_mort_reloj <- renderPlot({
        casos_es_acum2() %>%  filter(Comunidades != "Nacional") %>%
            ggplot(aes(reorder(Comunidades, letalidad) ,letalidad, color = -letalidad))+
            geom_point(show.legend = F, size = 4)+
            scale_colour_viridis_c("Comunidades", option = "D",aesthetics = "colour")+
            coord_polar("y")+
            ggtitle("Letalidad de la infeccion por día")+
            sb_sober_theme_pie+
            theme(axis.text.x = element_text(family = "Lato",
                                             size = 10,
                                             face = "bold"))
    })
    output$prop_fallecidos_com <- renderPlot({
        casos_es_dia() %>%
        ggplot(aes(reorder(Comunidades, -Fallecidos),Fallecidos , fill = Comunidades))+
            geom_col(width = 1, colour = "white", show.legend = T)+
            coord_polar("y")+
            geom_text(aes(label = Fallecidos), 
                      nudge_y = 0.5, 
                      family = "Lato", 
                      fontface = "bold",
                      size = 4)+
            geom_text(aes(label = paste0(round((Fallecidos/max(Fallecidos))*100,2),"%")), 
                      nudge_y = 0.5,
                      nudge_x = 0.6,
                      family = "Lato", 
                      fontface = "bold",
                      size = 4)+
            scale_colour_viridis_d("Comunidades",aesthetics = "fill", option = "D")+
            ggtitle("Proporcion de Fallecidos, por Día")+
            sb_sober_theme_pie+
            theme(axis.text.y = element_blank(),
                  axis.ticks.y = element_blank())
    })
    output$mort_bar_comunidad <- renderPlot({
        casos_es_dia2() %>%
            ggplot(aes(reorder(Comunidades, -mortalidad),mortalidad, fill = Comunidades))+
            geom_col(show.legend = F)+
            geom_text(aes(label = mortalidad), 
                      nudge_y = 6, 
                      family = "Lato", 
                      fontface = "bold",
                      size = 3)+
            scale_colour_viridis_d("Comunidades", option = "D",aesthetics = "fill")+
            ggtitle("Mortalidad de la infección por cada 100K habitantes")+
            ylab("Tasa de Mortalidad (100k hab)")+
            sb_sober_theme_col+
            theme(axis.title = element_blank(),
                  axis.text = element_text(angle = 30))
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
