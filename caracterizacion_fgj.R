####################################################.
## Analisis inicial de Victimas en Carpetas FGJ
## DAOA - 25/09/22
####################################################.

## Preparar Espacio ----
pacman::p_load(tidyverse,sf,janitor,scales)
rm(list=ls())
dev.off()

## Cargar Bases ----
victimas_fgj <- read_csv("input/victimas_completa_junio_2022.csv") %>% clean_names()

victimas_fgj %>% glimpse()

## Delitos ----
victimas_fgj %>% count(delito) %>% as.data.frame() %>% arrange(desc(n)) %>% slice_max(n=5, order_by = n)

## Feminicidios CDMX ----

labs <- victimas_fgj %>% filter(str_detect(delito,"FEMINICIDIO")) %>% group_by(ano_inicio,mes_inicio,delito) %>% 
  summarise(incidencia = n()) %>% 
  mutate(mes = case_when(mes_inicio == "Enero" ~ "01",
                         mes_inicio == "Febrero" ~ "02",
                         mes_inicio == "Marzo" ~ "03",
                         mes_inicio == "Abril" ~ "04",
                         mes_inicio == "Mayo" ~ "05",
                         mes_inicio == "Junio" ~ "06",
                         mes_inicio == "Julio" ~ "07",
                         mes_inicio == "Agosto" ~ "08",
                         mes_inicio == "Septiembre" ~ "09",
                         mes_inicio == "Octubre" ~ "10",
                         mes_inicio == "Noviembre" ~ "11",
                         mes_inicio == "Diciembre" ~ "12")) %>% 
  mutate(fecha = as.Date(paste0(ano_inicio,"-",mes,"-01"))) %>% group_by(delito) %>% slice_max(order_by = incidencia,n=5)

graph_feminicidios_1 <- victimas_fgj %>% filter(str_detect(delito,"FEMINICIDIO")) %>% group_by(ano_inicio,mes_inicio,delito) %>% 
  summarise(incidencia = n()) %>% 
  mutate(mes = case_when(mes_inicio == "Enero" ~ "01",
                         mes_inicio == "Febrero" ~ "02",
                         mes_inicio == "Marzo" ~ "03",
                         mes_inicio == "Abril" ~ "04",
                         mes_inicio == "Mayo" ~ "05",
                         mes_inicio == "Junio" ~ "06",
                         mes_inicio == "Julio" ~ "07",
                         mes_inicio == "Agosto" ~ "08",
                         mes_inicio == "Septiembre" ~ "09",
                         mes_inicio == "Octubre" ~ "10",
                         mes_inicio == "Noviembre" ~ "11",
                         mes_inicio == "Diciembre" ~ "12")) %>% 
  mutate(fecha = as.Date(paste0(ano_inicio,"-",mes,"-01"))) %>% 
  ggplot(aes(fecha,incidencia,fill = delito))+
  geom_col(color = "black", size = 0.5)+
  geom_label(data = labs,aes(label = comma(incidencia)),size = 8)+
  labs(x="",y="Carpetas de Investigación Iniciadas",title = "Carpetas de Investigación iniciadas por la FGJCDMX entre 2019 - 2022",
       subtitle = "Por delitos relacionados con el feminicidio", 
       caption = "Fuente: Datos Abiertos CDMX - Victimas en Carpetas de Investigación")+
  theme(plot.title = element_text(size = 24,color ="#9f2441", face = "bold"),
        plot.subtitle = element_text(size = 20,face = "bold"),
        legend.position = "bottom")+
  facet_wrap(~delito,scales = "free_y")

ggsave("./output/feminicidios_1.png",width = 12,height = 9)

## Grafica Bonita Feminicidios vs - Tentativa de Feminicidio ----

labs <- victimas_fgj %>% filter(str_detect(delito,"FEMINICIDIO")) %>% group_by(ano_inicio,mes_inicio,delito) %>% 
  summarise(incidencia = n()) %>% 
  mutate(feminicidio = ifelse(str_detect(delito,"TENTATIVA"),"TENTATIVA DE FEMINICIDIO","FEMINICIDIO")) %>% 
  ungroup() %>% group_by(feminicidio,mes_inicio,ano_inicio) %>% summarise(incidencia = sum(incidencia)) %>% 
  mutate(mes = case_when(mes_inicio == "Enero" ~ "01",
                         mes_inicio == "Febrero" ~ "02",
                         mes_inicio == "Marzo" ~ "03",
                         mes_inicio == "Abril" ~ "04",
                         mes_inicio == "Mayo" ~ "05",
                         mes_inicio == "Junio" ~ "06",
                         mes_inicio == "Julio" ~ "07",
                         mes_inicio == "Agosto" ~ "08",
                         mes_inicio == "Septiembre" ~ "09",
                         mes_inicio == "Octubre" ~ "10",
                         mes_inicio == "Noviembre" ~ "11",
                         mes_inicio == "Diciembre" ~ "12")) %>% 
  mutate(fecha = as.Date(paste0(ano_inicio,"-",mes,"-01"))) %>% ungroup(mes_inicio) %>% 
  slice_max(order_by = incidencia,n=3)

victimas_fgj %>% filter(str_detect(delito,"FEMINICIDIO")) %>% group_by(ano_inicio,mes_inicio,delito) %>% 
  summarise(incidencia = n()) %>% 
  mutate(feminicidio = ifelse(str_detect(delito,"TENTATIVA"),"TENTATIVA DE FEMINICIDIO","FEMINICIDIO")) %>% 
  ungroup() %>% group_by(feminicidio,mes_inicio,ano_inicio) %>% summarise(incidencia = sum(incidencia)) %>% 
  mutate(mes = case_when(mes_inicio == "Enero" ~ "01",
                         mes_inicio == "Febrero" ~ "02",
                         mes_inicio == "Marzo" ~ "03",
                         mes_inicio == "Abril" ~ "04",
                         mes_inicio == "Mayo" ~ "05",
                         mes_inicio == "Junio" ~ "06",
                         mes_inicio == "Julio" ~ "07",
                         mes_inicio == "Agosto" ~ "08",
                         mes_inicio == "Septiembre" ~ "09",
                         mes_inicio == "Octubre" ~ "10",
                         mes_inicio == "Noviembre" ~ "11",
                         mes_inicio == "Diciembre" ~ "12")) %>% 
  mutate(fecha = as.Date(paste0(ano_inicio,"-",mes,"-01"))) %>% 
  ggplot(aes(fecha,incidencia, color = feminicidio))+
  geom_line()+
  geom_smooth()+
  geom_label(data = labs,aes(label = comma(incidencia, prefix = paste0(mes_inicio," ",ano_inicio,": "))),size = 6)+
  labs(x="",y="Carpetas de Investigación Iniciada", 
       title = "Carpetas de Investigación Mensuales iniciadas por la FGJCDMX ",
       subtitle = "Por delitos relacionados con el feminicidio entre 2019 - 2022", 
       caption = "Fuente: Datos Abiertos CDMX - Victimas en Carpetas de Investigación",
       color = "Tipo de Delito:")+
  theme(plot.title = element_text(size = 24,color ="#235b4e", face = "bold"),
        plot.subtitle = element_text(size = 20,face = "bold", color = "#10312b"),
        legend.position = "bottom",
        legend.text = element_text(size = 12,face = "bold"),
        legend.title = element_text(size = 16,face = "bold"))+
        scale_color_manual(values = c("#9f2441","#BC955C"))
ggsave("./output/grafica_feminicidios_2.png",width = 16,height = 9)
















  
