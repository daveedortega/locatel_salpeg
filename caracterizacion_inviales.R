####################################################.
## Analisis inicial de Incidentes Viales
## DAOA - 10/10/22
####################################################.

## Preparar Espacio ----
pacman::p_load(tidyverse,sf,janitor,scales)
rm(list=ls())
dev.off()

## Cargar Bases ----
inviales_15 <- read_csv("input/inViales_2014_2015.csv")
inviales_18 <- read_csv("input/inViales_2016_2018.csv")
inviales_21 <- read_csv("input/inViales_2019_2021.csv")
inviales_22 <- read_csv("input/inViales_2022_jul.csv")
# Mismos nombres
colnames(inviales_15) == colnames(inviales_18)
colnames(inviales_18) == colnames(inviales_21)
colnames(inviales_18) == colnames(inviales_22)

# Pegamos en una sola base ----

inviales_18_22 <- rbind(inviales_15,inviales_18,inviales_21,inviales_22)
inviales_18_22 # 1.73 millones de observaciones

# Análisis distribución temporal ----

inviales_18_22 %>% mutate(ano = format(fecha_creacion,"%Y")) %>% group_by(ano) %>% summarise(incidentes_creacion = n()) %>% 
  ggplot(aes(ano,incidentes_creacion, fill = ano))+
  geom_col(color = "black")+
  geom_label(aes(label = comma(incidentes_creacion)))+
  labs(x="", y = "Incidentes", title = "Incidentes Viales Reportados al C5", 
       subtitle = "Por inicio de reporte en la CDMX, entre 2014 - 2022/07", 
       caption = "Fuente: Portal de Datos Abiertos CDMX: Incidentes Viales C5")+
  theme(plot.title = element_text(size = 22, face = "bold", color = "#9f2241"),
        plot.subtitle = element_text(size = 18, face = "bold"),
        legend.position = "none")

inviales_18_22 %>% mutate(ano = format(fecha_cierre,"%Y")) %>% group_by(ano) %>% summarise(incidentes_cierre = n()) %>% 
  ggplot(aes(ano,incidentes_cierre, fill = ano))+
  geom_col(color = "black")+
  geom_label(aes(label = comma(incidentes_cierre)))+
  labs(x="", y = "Incidentes", title = "Incidentes Viales Reportados al C5", 
       subtitle = "Por cierre de reporte en la CDMX, entre 2014 - 2022/07", 
       caption = "Fuente: Portal de Datos Abiertos CDMX: Incidentes Viales C5")+
  theme(plot.title = element_text(size = 22, face = "bold", color = "#9f2241"),
        plot.subtitle = element_text(size = 18, face = "bold"),
        legend.position = "none")

# Se aprecia caida por Covid, importa para análisis ya que cablebus se inauguró en 2021

# Análisis distribución temporal ----

inviales_18_22 %>% mutate(ano = format(fecha_creacion,"%Y")) %>%  group_by(tipo_entrada, ano) %>% 
  summarise(incidentes = n()) %>% 
  ggplot(aes(ano,incidentes, fill = tipo_entrada))+
  geom_col()+
  geom_label(aes(label = comma(incidentes)))+
  labs(x="", y = "Incidentes", title = "Incidentes Viales Reportados al C5",
       subtitle = "Por tipo de entrada de reporte en la CDMX, entre 2014 - 2022/07",
       caption = "Fuente: Portal de Datos Abiertos CDMX: Incidentes Viales C5")+
  theme(plot.title = element_text(size = 22, face = "bold", color = "#9f2241"),
        plot.subtitle = element_text(size = 18, face = "bold"),
        legend.position = "none")+
  facet_wrap(~tipo_entrada, scales = "free_y")

# Buena distribución temporal de Botón de auxilio y demás.


inviales_18_22  %>%  group_by(tipo_entrada) %>% 
  summarise(incidentes = n()) %>% 
  ggplot(aes(reorder(tipo_entrada,incidentes),incidentes, fill = tipo_entrada))+
  geom_col(color = "black")+
  geom_label(aes(label = comma(incidentes)))+
  labs(x="", y = "Incidentes", title = "Incidentes Viales Reportados al C5",
       subtitle = "Por tipo de entrada de reporte en la CDMX, entre 2014 - 2022/07",
       caption = "Fuente: Portal de Datos Abiertos CDMX: Incidentes Viales C5")+
  theme(plot.title = element_text(size = 22, face = "bold", color = "#9f2241"),
        plot.subtitle = element_text(size = 18, face = "bold"),
        legend.position = "none")+
  scale_y_sqrt()

# Bien, radio se refiere a Radio de Policía, entonces quizá contrastarlo por cuadrante y número de policía por coso

inviales_18_22 %>% glimpse()

# Por incidente

inviales_18_22 %>% count(incidente_c4) %>% 
  ggplot(aes(reorder(incidente_c4,n),n,fill = incidente_c4))+
  geom_col(color = "black")+
  geom_label(aes(label = comma(n)))+
  labs(x="", y="Incidentes", title = "Incidentes Viales Reportados al C5", 
       subtitle = "Por tipo de Incidente de reporte en la CDMX, entre 2014 - 2022/07",
       caption = "Fuente: Portal de Datos Abiertos CDMX: Incidentes Viales C5")+
  theme(plot.title = element_text(size = 22, face = "bold", color = "#9f2241"),
        plot.subtitle = element_text(size = 18, face = "bold"),
        legend.position = "none")+
  scale_y_sqrt()+ 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

# Por incidente y reporte radio y botón

inviales_18_22 %>% count(incidente_c4, tipo_entrada) %>% filter(tipo_entrada %in% c("BOTÓN DE AUXILIO", "RADIO")) %>% 
  ggplot(aes(reorder(incidente_c4,n),n,fill = tipo_entrada))+
           geom_col(color = "black")+
           geom_label(aes(label = comma(n)))+
           labs(x="", y="Incidentes", title = "Incidentes Viales Reportados al C5", 
                subtitle = "Por tipo de Incidente de reporte en la CDMX, entre 2014 - 2022/07",
                caption = "Fuente: Portal de Datos Abiertos CDMX: Incidentes Viales C5")+
           theme(plot.title = element_text(size = 22, face = "bold", color = "#9f2241"),
                 plot.subtitle = element_text(size = 18, face = "bold"),
                 legend.position = "none")+
           scale_y_sqrt()+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  facet_wrap(~tipo_entrada)

# Heatmap de puntos
# Quitamos los NAs
inviales_18_22p <- inviales_18_22 %>% na.omit()

inviales_18_22p <- inviales_18_22p %>% st_as_sf(coords = c("longitud", "latitud"), 
                            crs = 4326)

inviales_18_22p %>% ggplot(aes())+
  geom_sf()


















