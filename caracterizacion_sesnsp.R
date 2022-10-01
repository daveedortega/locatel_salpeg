####################################################.
## Analisis inicial de SESNEP
## DAOA - 01/10/22
####################################################.

## Preparar Espacio ----
pacman::p_load(tidyverse,sf,janitor,scales)
rm(list=ls())
dev.off()

## Cargar Bases ----
incidencia_municipal <- read_csv("input/victimas_completa_junio_2022.csv") %>% clean_names()

incidencia_municipal %>% glimpse()

## Debería haber 2,469 municipios ----
incidencia_municipal %>% group_by(alcaldia_hechos) %>% summarise(incidencia = n()) %>% arrange(incidencia) # No los hay, hay 476

incidencia_municipal %>% group_by(alcaldia_hechos) %>% summarise(incidencia = n()) %>% arrange(incidencia) %>% 
  filter(incidencia<10) # 376 tienen menos de 10 incidencias en 3 años

# otros tienen cientos de miles
incidencia_municipal %>% group_by(alcaldia_hechos) %>% summarise(incidencia = n()) %>% arrange(desc(incidencia)) 

incidencia_municipal %>% group_by(alcaldia_hechos) %>% summarise(incidencia = n()) %>% arrange(desc(incidencia)) %>% 
  filter(incidencia>=1000) # PFFF, los únicos mayores a mil TODOS son CDMX


# Densidad
densidad <- incidencia_municipal %>% group_by(alcaldia_hechos) %>% summarise(incidencia = n()) %>% arrange(incidencia)
plot(density(densidad$incidencia),main = "Distribución incidencia municipal", sub ="SESNSP - Incidencia Delictiva Nivel Municipal",
     xlab = "x",ylab = "px")
# PFFF no mamard; estaría bien 1) pesarlo por población
# Saber dónde se puede denunciar, i.e.; si sólo se puede denunciar en ciertos municipios


incidencia_municipal %>% group_by(alcaldia_hechos,ano_inicio) %>% summarise(incidencia = n()) %>% ungroup() %>% 
  mutate(porcentaje = round(incidencia/sum(incidencia)*100,4)) %>% group_by(ano_inicio) %>% 
  slice_max(order_by = porcentaje, n = 10) %>% filter(ano_inicio!=2018) %>% filter(!is.na(ano_inicio)) %>% 
  ggplot(aes(reorder(alcaldia_hechos,porcentaje),porcentaje,fill = alcaldia_hechos))+
  geom_col(color = "black")+
  geom_label(aes(label = comma(porcentaje,prefix = "%")))+
  labs(x="",y="%", title = "10 Municipios con mayor incidencia delictiva total", subtitle ="Por año, entre 2019,Agosto 2022",
                         caption = "Fuente: SESNSP - Incidencia Delictiva a Nivel Municipal")+
  theme(plot.title = element_text(face = "bold", size = 28, color = "#9F2441"),
        plot.subtitle = element_text(face = "bold",size = 22),
        axis.title.y = element_text(face = "bold"), 
        legend.position = "none",
        axis.text.x = element_text(angle = 90))+
  facet_wrap(~ano_inicio)
  


# Empieza la nueva metodología en 2019 ----
incidencia_municipal %>% count(ano_inicio)
labs <- incidencia_municipal %>% mutate(fecha_inicio = as.Date(fecha_inicio,tryFormats = "%d/%m/%Y")) %>% 
  group_by(fecha_inicio) %>% summarise(incidencia_total = n()) %>% slice_max(order_by = incidencia_total,n = 10)

incidencia_municipal %>% mutate(fecha_inicio = as.Date(fecha_inicio,tryFormats = "%d/%m/%Y")) %>% 
  group_by(fecha_inicio) %>% summarise(incidencia_total = n()) %>% 
  ggplot(aes(fecha_inicio,incidencia_total)) +
  geom_line(color = "#bc955c")+
  geom_smooth()+
  geom_label(data = labs,aes(label = comma(incidencia_total)))+
  labs(x="", y ="Carpetas de Investigación Iniciadas", title = "Carpetas de Investigación Iniciadas en México entre 2019 - 2022",
       subtitle = "De acuerdo al SESNEP, a Agosto 2022", caption = "Fuente: SESNESP - Incidencia Delictiva a Nivel Municipal")+
  theme(plot.title = element_text(face = "bold", size = 28, color = "#BC955C"),
        plot.subtitle = element_text(face = "bold",size = 22, color = "#9F2441"),
        axis.title.y = element_text(face = "bold"), 
        legend.position = "none")
  
# Cuál es la diferencia con la vieja metodología? Cuán comparables son?
# Capuda al inicio de la pandemia; se podrá hacer trends y "desestacionalizar", para revisar si es que ha caído en verdad
# la delictividad en su trend y no es un efecto estacional?












