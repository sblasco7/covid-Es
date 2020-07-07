library(tidyverse)
library(lubridate)
library(stringr)

dat<-read_csv("data/casos_es_2.csv")
dat2 <- read_csv2("data/Censo_España.csv")
dat <- dat[-c(1198:1202),]

#Change the names of the comunities
comunidades <- str_replace_all(dat$CCAA,c("AN"= "Andalucía", 
                                          "AR"= "Aragon",
                                          "AS"= "Asturias",
                                          "IB"= "Islas Baleares",
                                          "CN"= "Canarias",
                                          "CB"= "Cantabria",
                                          "CM"= "Castilla-La Mancha",
                                          "CL"= "Castilla y León",
                                          "CT" = "Cataluña",
                                          "VC" = "C. Valenciana",
                                          "CE" = "Ceuta",
                                          "EX" = "Extremadura",
                                          "GA" = "Galicia",
                                          "MD"= "Madrid",
                                          "ML"= "Melilla",
                                          "NC"= "Navarra",
                                          "PV"= "País Vasco",
                                          "RI"= "La Rioja",
                                          "MC" = "Murcia"))


# replace NAs, format dates and names
casos_es <- dat %>% mutate(Comunidades =as.factor(comunidades),
                           CASOS = as.numeric(str_replace_na(CASOS,0)),
                           Hospitalizados = as.numeric(str_replace_na(Hospitalizados,0)),
                           UCI = as.numeric(str_replace_na(UCI,0)),
                           Fallecidos = as.numeric(str_replace_na(Fallecidos,0)),
                           Recuperados = as.numeric(str_replace_na(Recuperados,0)),
                           FECHA = dmy(FECHA)) %>%
                    rename("Fecha" = "FECHA", "Casos" = "CASOS")

#reorder columns to make df readable
casos_es <- casos_es[,c(1,8,2:7)]

#nuevos casos nacionales por fecha y nuevos casos diarios 
calc_nacional <- casos_es  %>% group_by(Fecha)%>% 
  summarize(CCAA = "ES", 
            Comunidades = "Nacional",
            Casos = sum(Casos), 
            Fallecidos = sum(Fallecidos),
            Hospitalizados = sum(Hospitalizados),
            UCI = sum(UCI),
            Recuperados = sum(Recuperados))%>% 
  mutate(nuevos_casos = c(0,diff(.$Casos)))

# agregar casos nacionales y calcular aumento de casos por día 
calc_comunidad <-casos_es %>% select(Casos,Fecha,Comunidades) %>%
  spread(Comunidades, Casos) %>%
  mutate(AN = c(0,diff(.$Andalucía)),
         AR = c(0,diff(.$Aragon)),
         AS = c(0,diff(.$Asturias)),
         IB = c(0,diff(.$"Islas Baleares")),
         CN = c(0,diff(.$Canarias)),
         CB = c(0,diff(.$Cantabria)),
         CM = c(0,diff(.$"Castilla-La Mancha")),
         CL = c(0,diff(.$"Castilla y León")),
         CT = c(0,diff(.$Cataluña)),
         VC = c(0,diff(.$"C. Valenciana")),
         CE = c(0,diff(.$Ceuta)),
         EX = c(0,diff(.$Extremadura)),
         GA = c(0,diff(.$Galicia)),
         MD = c(0,diff(.$Madrid)),
         ML = c(0,diff(.$Melilla)),
         NC = c(0,diff(.$Navarra)),
         PV = c(0,diff(.$"País Vasco")),
         RI = c(0,diff(.$"La Rioja")),
         MC = c(0,diff(.$Murcia))) %>% 
  select(c(1,21:39))%>%
 pivot_longer(-Fecha, names_to = c("CCAA"), values_to = "nuevos_casos")

# juntar las tablas

casos_es <-inner_join(casos_es,calc_comunidad)
casos_es <-bind_rows(casos_es,calc_nacional)

#census, demographic data 2019 by comunidad 

censo_es <- dat2 %>% filter(Periodo == "1 de julio de 2019") %>% 
  mutate(Comunidades = as_factor(`Comunidades y ciudades autónomas`))%>%
  select(-`Comunidades y ciudades autónomas`)%>%
  rename("Edades" = `Edad simple`) %>% 
  filter(Edades == "Total" & Sexo == 'Ambos sexos') %>% 
  rename("Poblacion" = "Total")

comunidades2 <- str_replace_all(censo_es$Comunidades,c("01 Andalucía"= "Andalucía",
                                                "02 Aragón"= "Aragon",
                                                "03 Asturias, Principado de"= "Asturias",
                                                "04 Balears, Illes"= "Islas Baleares",
                                                "05 Canarias"= "Canarias",
                                                "06 Cantabria"= "Cantabria",
                                                "08 Castilla - La Mancha"= "Castilla-La Mancha",
                                                "07 Castilla y León"= "Castilla y León",
                                                "09 Cataluña" = "Cataluña",
                                                "10 Comunitat Valenciana" = "C. Valenciana",
                                                "18 Ceuta" = "Ceuta",
                                                "11 Extremadura" = "Extremadura",
                                                "12 Galicia" = "Galicia",
                                                "13 Madrid, Comunidad de"= "Madrid",
                                                "19 Melilla"= "Melilla",
                                                "15 Navarra, Comunidad Foral de" = "Navarra",
                                                "16 País Vasco"= "País Vasco",
                                                "17 Rioja, La"= "La Rioja",
                                                "14 Murcia, Región de" = "Murcia",
                                                "Total Nacional" = "Nacional"))


censo_es <- censo_es %>% mutate(Comunidades = as.factor(comunidades2)) %>% select(Poblacion, Comunidades)

casos_es<-inner_join(casos_es,censo_es) %>% mutate(letalidad = replace_na(round((Fallecidos/Casos)*100,2),0),
                                                   mortalidad = round((Fallecidos/Poblacion)*10^5,2),
                                                   Comunidades = as.factor(Comunidades),
                                                   tasa_hospitalización = replace_na(round((Hospitalizados/Casos)*100,2),0))

# escribir archivo principal
write_csv(casos_es,"C:/Users/rstudio/Desktop/Datos/COVID_19/Estudio de casos España/Covid_ES/casos_es.csv")
