####################################################.
## Código Final para generar bdd de Carpetas de Investigación
## DAOA - 11/12/22
####################################################.

## Preparar Espacio ----
pacman::p_load(tidyverse,sf,janitor,scales)
rm(list=ls())
dev.off()

## Cargar Bases ----
# CARPETAS
carpetas_18 <- read_csv("input/carpetas_fgj/carpetas_2016-2018.csv")
carpetas_21 <- read_csv("input/carpetas_fgj/carpetas_2019-2021.csv")
carpetas_22 <- read_csv("input/carpetas_fgj/carpetas_2022.csv")
# Pegamos
carpetas_completa <-rbind(carpetas_18,carpetas_21,carpetas_22)
rm(carpetas_18,carpetas_21,carpetas_22)

glimpse(carpetas_completa)


# Estadísticas Generales ----
# Línea de tiempo
carpetas_completa %>% mutate(fecha = as.Date(paste0(format(fecha_inicio,format = "%Y-%m"), "-01"))) %>% count(fecha) %>% 
  ggplot(aes(fecha,n))+
  geom_line(color = "#9f2441", size = 1)+
  geom_smooth()+
  labs(x="", y="Carpetas de Investigación", title = "Carpetas de Investigación iniciadas al mes", 
       subtitle = " entre 01/2016 y 09/2022 reportados por la FGJ", fill ="", caption = "Fuente: Datos Abiertos - GCDMX")+
  theme(plot.title = element_text(size = 28, face = "bold", color = "#23433A"),
        plot.subtitle = element_text(size = 22, face="bold"))

# Tipos de Delito
carpetas_completa %>% count(categoria_delito) %>% ggplot(aes(reorder(categoria_delito, n),n, fill = categoria_delito))+
  geom_col(color = "black", size = 0.1)+
  geom_label(aes(label = comma(n)))+
  labs(x="", y="Carpetas de Investigación", title = "Carpetas de Investigación por Categoría de Delito", 
       subtitle = " entre 01/2016 y 09/2022 reportados por la FGJ", fill ="", caption = "Fuente: Datos Abiertos - GCDMX")+
  theme(plot.title = element_text(size = 28, face = "bold", color = "#9f2441"),
        plot.subtitle = element_text(size = 22, face="bold"), 
        legend.position = "none", 
        axis.text.y = element_text(color = "black", size = 10))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  scale_y_sqrt()
