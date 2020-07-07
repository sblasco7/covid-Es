library(extrafont)
library(tidyverse)
library(viridis)
library(lubridate)
library(RColorBrewer)

loadfonts(device = "win")

#casos nacionales totales
casos_es %>%
  filter(Comunidades == "Nacional")%>%
  ggplot(aes(Fecha, Casos))+
  geom_line(size = 1.2, show.legend = F)+
  scale_colour_viridis_d("Comunidades", option = "D",aesthetics = "color")+
  scale_x_date(breaks = seq(min(casos_es$Fecha), max(casos_es$Fecha),by=7))+
  scale_y_continuous(position = "right")+
  ggtitle("Evolución de casos, por día")+
  sb_sober_theme_line

casos_es %>% filter(Comunidades == "Nacional")%>%
  ggplot(aes(Fecha, Casos))+
  geom_line(size = 1, show.legend = F)+
  geom_line(aes(Fecha,nuevos_casos))+
  scale_colour_viridis_d("Comunidades", option = "D",aesthetics = "color")+
  scale_x_date(breaks = seq(min(casos_es$Fecha), max(casos_es$Fecha),by=7))+
  ggtitle("Evolución de casos, por día")+
  sb_sober_theme_line

#casos nuevos nacionales por fecha 
casos_es %>% filter(Comunidades == "Nacional")%>%
  ggplot(aes(Fecha,nuevos_casos, colour = nuevos_casos) )+
  geom_line(show.legend = F, size = 1.1)+
  scale_colour_viridis_c("nuevos_casos", option = "A",aesthetics = "colour")+
  scale_x_date(breaks = seq(min(casos_es$Fecha), max(casos_es$Fecha),by=7))+
  ggtitle("Casos nuevos, por día")+
  sb_sober_theme_line

#cambio porcentual de casos
casos_es %>% filter(Comunidades == "Nacional") %>%
  ggplot(aes(Fecha, y= (nuevos_casos/Casos)*100, fill = (nuevos_casos/Casos)*100))+
  geom_col(show.legend = F, size = 1)+
  scale_colour_distiller(palette = "Reds", aesthetics = "fill", direction = 1)+
  scale_x_date(breaks = seq(min(casos_es$Fecha), max(casos_es$Fecha),by=7))+
  ggtitle("Cambios porcentuales en infecciones por día")+
  sb_sober_theme_line

#letalidad reloj
casos_es %>%  filter(Comunidades != "Nacional") %>%
  ggplot(aes(reorder(Comunidades, letalidad) ,letalidad, color = -letalidad))+
  geom_point(show.legend = F)+
  scale_colour_viridis_c("Comunidades", option = "D",aesthetics = "colour")+
  coord_polar("y")+
  sb_sober_theme_pie+
  theme(axis.text.x = element_text(family = "Lato",
                                    size = 10,
                                    face = "bold"))

#mortalidad reloj
casos_es %>%  filter(Comunidades != "Nacional") %>%
  ggplot(aes(reorder(Comunidades, mortalidad) ,mortalidad, color = -mortalidad))+
  geom_point(show.legend = F)+
  scale_colour_viridis_c("mortalidad", option = "D",aesthetics = "colour")+
  coord_polar("y")+
  sb_sober_theme_pie+
  theme(axis.text.x = element_text(family = "Lato",
                                   size = 10,
                                   face = "bold"))
max(casos_es$mortalidad)
casos_es %>% group_by(Comunidades) %>% summarize(x = max(mortalidad))
casos_es %>% filter(Comunidades == "Madrid") %>% nrow()

# COMUNIDADES 
casos_es %>% filter(Fecha == "2020-03-30", Comunidades %in% c("Nacional","Madrid", "País Vasco", "Cataluña", "Canarias")) %>%
  ggplot(aes(reorder(Comunidades, -Fallecidos),Fallecidos , fill = Comunidades))+
  geom_col(width = 1, colour = "white", )+
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

# violin casos
casos_es %>% filter(Fecha <= "2020-03-19") %>%
  ggplot(aes(CCAA, letalidad, colour = CCAA))+
  geom_point(aes(size = letalidad), show.legend = F)+ 
#  stat_summary(fun=mean, geom="point", shape=23, size=3,fill = "white")+
  scale_colour_viridis_d("Comunidades", option = "D",aesthetics = "colour")+
  ggtitle("Letalidad de la infección")+
  sb_sober_theme_col

#Mortalidad en %
casos_es %>% filter(Fecha == "2020-03-19") %>%
  ggplot(aes(reorder(CCAA, -mortalidad),mortalidad, fill = CCAA))+
  geom_col(show.legend = F)+
  geom_text(aes(label = paste0(mortalidad,"%")), 
            nudge_y = 0.5, 
            family = "Lato", 
            fontface = "bold",
            size = 3)+
  scale_colour_viridis_d("Comunidades", option = "D",aesthetics = "fill")+
  ggtitle("Mortalidad de la infección en %")+
  ylab("Tasa de Mortalidad (%)")+
  xlab("Comunidades")+
  sb_sober_theme_col

# letalidad en % 

casos_es %>% 
  filter(Comunidades %in% c("Madrid", "Cataluña")) %>% 
  ggplot(aes(Casos, mortalidad, color = Comunidades))+
  geom_point(aes(size = 2), show.legend = F)+
  facet_grid(.~ Comunidades)

#casos por comunidad, totales por fecha
casos_es %>% 
  filter(Fecha == "2020-04-22") %>%
  ggplot(aes(reorder(CCAA, -Casos),Casos))+
  geom_col(aes(fill = Comunidades), show.legend = F)+
  scale_colour_viridis_d("Comunidades", option = "D")+
  geom_text(aes(label = Casos), 
            nudge_y = 2500, 
            family = "Lato", 
            fontface = "bold", 
            angle = 45)+
  ggtitle("Casos por fecha segun la comunidad Autónoma")+
  ylab("Casos")+
  xlab("Comunidades Autónomas")+
  sb_sober_theme_col+
  theme(axis.text.x = element_text(angle = 0))


#casos acumulados en el período
lin_comunidades_es <- casos_es  %>% 
  filter(Comunidades %in% c("Madrid", "Cataluña", "Castilla-La Mancha")) %>%
  ggplot(aes(Fecha, Casos, color = Comunidades))+
  geom_line( size = 1.2)+
  scale_colour_viridis_d("Comunidades",aesthetics = "color", option = "D")+
  scale_x_date(breaks = seq(min(casos_es$Fecha), max(casos_es$Fecha),by=7))+
  ggtitle("Evolución de casos, por día")+
  sb_sober_theme_line


#casos acumulados por fecha logaritmo base 2
lin_comunidades_es_log2 <- casos_es  %>% 
  filter(Comunidades %in% c("Madrid", "Cataluña", "Castilla-La Mancha")) %>%
  ggplot(aes(Fecha, Casos, colour= Comunidades))+
  geom_line(size = 1.2)+
  scale_colour_viridis_d("Comunidades",aesthetics = "colour", option = "E")+
  scale_y_continuous(trans = "log2")+
  scale_x_date(breaks = seq(min(casos_es$Fecha), max(casos_es$Fecha),by=7))+
  ggtitle("Evolución de casos, por día (log2)")+
  sb_sober_theme_line


casos_es %>% filter(Comunidades == "Nacional") %>%
  ggplot(aes(x = Fecha,Fallecidos))+
  geom_col(show.legend = F)+
  coord_polar("x")+
#  scale_colour_viridis_d("Comunidades", option = "D",aesthetics = "fill")+
  ggtitle("Fallecidos por día")+
  ylab("Tasa de Mortalidad (100k hab)")+
  sb_sober_theme_col+
  theme(axis.title = element_blank())


#fallecidos acumuladas por fecha
casos_es %>% 
  filter(Fecha == "2020-04-21") %>%
  ggplot(aes(reorder(CCAA,-Fallecidos),Fallecidos))+
  geom_col(aes(fill = Comunidades), show.legend = F)+
  scale_colour_viridis_d("Comunidades", option = "D")+
  geom_text(aes(label = Fallecidos), 
            nudge_y = 240, 
            family = "Lato", 
            fontface = "bold")+
  ggtitle("Fallecidos por fecha segun la comunidad Autónoma")+
  ylab("Fallecidos")+
  xlab("Comunidades Autónomas")+
  sb_sober_theme_col+
  theme(axis.text.x = element_text(angle = 0))

#nuevos casos por comuna por día
casos_es %>% 
  filter(Fecha == "2020-04-15") %>%
  ggplot(aes(reorder(CCAA,-nuevos_casos),nuevos_casos))+
  geom_col(aes(fill = Comunidades), show.legend = F)+
  scale_colour_viridis_d("Comunidades", option = "D")+
  geom_text(aes(label = nuevos_casos), 
            nudge_y = 50, 
            family = "Lato", 
            fontface = "bold")+
  ggtitle("Nuevos casos por fecha segun la comunidad Autónoma")+
  ylab("Nuevos Casos diarios")+
  xlab("Comunidades Autónomas")+
  sb_sober_theme_col+
  theme(axis.text.x = element_text(angle = 0))

#Hospitalizados, Cuidados intensivos y fallecidos

casos_es %>% filter(Comunidades == "Nacional") %>%
  ggplot(aes(Fecha, Hospitalizados))+
  geom_area(fill = brewer.pal(3,"Set1")[1], alpha = 0.6, colour = "white")+
  geom_area(aes(Fecha, Fallecidos), fill = brewer.pal(3,"Set1")[2], alpha = 0.6, colour = "white")+
  geom_area(aes(Fecha, UCI), fill = brewer.pal(3,"Set1")[3], alpha = 0.5, colour = "white")+
  sb_sober_theme_col

# texto de fallecidos

casos_es %>% filter(Comunidades == "Nacional", Fecha == "2020-03-15") %>%
  ggplot(aes(Fallecidos, y = ""))+
  geom_text(aes(label = Fallecidos), 
            colour = brewer.pal(3,"Set1")[2], size = 20,
            family = "Lato")+
  ggtitle("N° de Hospitalizados")+
  sb_sober_theme_text

# % fallecidos de la infección
casos_es %>% filter(Comunidades == "Nacional", Fecha == "2020-03-15") %>%
  ggplot(aes(Fallecidos, y = ""))+
  geom_text(aes(label = paste0(round(Fallecidos/Casos*100,2),"%")), 
            colour = brewer.pal(3,"Set1")[1], size = 20,
            family = "Lato")+
  ggtitle("% de infectados que fueron hospitalizados")+
  sb_sober_theme_text

# texto de Hospitalizados
casos_es %>% filter(Comunidades == "Nacional", Fecha == "2020-03-15") %>%
  ggplot(aes(Hospitalizados, y = ""))+
  geom_text(aes(label = Hospitalizados), 
            colour = brewer.pal(3,"Set1")[1], size = 20,
            family = "Lato")+
  ggtitle("N° de Hospitalizados")+
  sb_sober_theme_text

# % de casos hospitalizados
casos_es %>% filter(Comunidades == "Nacional", Fecha == "2020-03-15") %>%
  ggplot(aes(Hospitalizados, y = ""))+
  geom_text(aes(label = paste0(round(Hospitalizados/Casos*100,2),"%")), 
            colour = brewer.pal(3,"Set1")[1], size = 20,
            family = "Lato")+
  ggtitle("% de infectados que fueron hospitalizados")+
  sb_sober_theme_text

# texto de Cuidados Intensivos

casos_es %>% filter(Comunidades == "Nacional", Fecha == "2020-03-15") %>%
  ggplot(aes(UCI, y = ""))+
  geom_text(aes(label = UCI), 
            colour = brewer.pal(3,"Set1")[3], size = 20,
            family = "Lato")+
  ggtitle("N° de camas usadas en cuidados intensivos")+
  sb_sober_theme_text
# % de hospitalizado en UCI
casos_es %>% filter(Comunidades == "Nacional", Fecha == "2020-03-15") %>%
  ggplot(aes(UCI, y = ""))+
  geom_text(aes(label = paste0(round(UCI/Hospitalizados*100,2),"%")), 
            colour = brewer.pal(3,"Set1")[3], size = 20,
            family = "Lato")+
  ggtitle("% de pacientes en UCI")+
  sb_sober_theme_text


#dias transcurridos desde la primera muerte

plot(log10(casos_es %>% select(Fecha, Comunidades, Fallecidos)%>% 
  filter(Comunidades == "Nacional") %>% .$Fallecidos))


casos_es %>% select(Fecha, Comunidades, nuevos_casos) %>% filter(Comunidades == "Madrid")%>%
  ggplot(aes(Fecha, nuevos_casos))+
  geom_line()

plot(comuniddade)
lin_comunidades_es  
ymd(seq(min(casos_es$Fecha), max(casos_es$Fecha),by=7), truncated = 400)

