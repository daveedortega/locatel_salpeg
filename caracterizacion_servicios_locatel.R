####################################################.
## Analisis inicial de Bases de Datos de Locatel
## DAOA - 25/09/22
####################################################.

## Preparar Espacio ----
pacman::p_load(tidyverse,sf,janitor,scales)
rm(list=ls())
dev.off()

## Cargar Bases ----
locatel_18 <- read_csv("input/servicios_integrales_2016-2018.csv")
locatel_21 <- read_csv("input/servicios_integrales_2019-2021.csv")
locatel_22 <- read_csv("input/servicios_integrales_20220921.csv")


glimpse(locatel_18)
glimpse(locatel_21)
glimpse(locatel_22)

# Todas tienen la misma estructura 
colnames(locatel_18) == colnames(locatel_21) 
colnames(locatel_21) == colnames(locatel_22)

# Pegamos
locatel_16_22 <- rbind(locatel_18,locatel_21,locatel_22) %>% clean_names()

# Revisamos Origen de las Llamadas ----
locatel_16_22 %>% glimpse()

origen_colgraph <- locatel_16_22 %>% count(origen) %>% 
  ggplot(aes(reorder(origen,n),n,fill = origen)) + 
  geom_col(color = "black") +
  geom_label(aes(label = comma(n)),size = 10)+
  labs(x="", y ="",title = "Origen de Atenciones provistas por Locatel entre 2016 - 2022", subtitle = "A septiembre 2022",
       caption = "Portal de Datos Abiertos: Servicios a la Población en General", fill = "Origen:")+
  scale_y_sqrt()+
  theme(plot.title = element_text(size = 24,color ="#9f2441", face = "bold"),
        plot.subtitle = element_text(size = 20,face = "bold"),
        legend.position = "bottom")

origen_colgraph

# Pregunta, ¿ Cuál es el origen de Asesoría? ¿Cuál la de atención Inmediata?

# Revisamos temporalidad por mes ----
labs <- locatel_16_22 %>% count(mes_alta,ano_alta,origen) %>% mutate(mes = case_when(mes_alta == "Enero" ~ "01",
                                                                                     mes_alta == "Febrero" ~ "02",
                                                                                     mes_alta == "Marzo" ~ "03",
                                                                                     mes_alta == "Abril" ~ "04",
                                                                                     mes_alta == "Mayo" ~ "05",
                                                                                     mes_alta == "Junio" ~ "06",
                                                                                     mes_alta == "Julio" ~ "07",
                                                                                     mes_alta == "Agosto" ~ "08",
                                                                                     mes_alta == "Septiembre" ~ "09",
                                                                                     mes_alta == "Octubre" ~ "10",
                                                                                     mes_alta == "Noviembre" ~ "11",
                                                                                     mes_alta == "Diciembre" ~ "12")) %>% 
  mutate(date = as.Date(paste0(ano_alta,"-",mes,"-01"))) %>% 
  group_by(origen) %>% 
  slice_max(n = 5,order_by = n)

densidad_ldt <- locatel_16_22 %>% count(mes_alta,ano_alta,origen) %>% mutate(mes = case_when(mes_alta == "Enero" ~ "01",
                                                                      mes_alta == "Febrero" ~ "02",
                                                                      mes_alta == "Marzo" ~ "03",
                                                                      mes_alta == "Abril" ~ "04",
                                                                      mes_alta == "Mayo" ~ "05",
                                                                      mes_alta == "Junio" ~ "06",
                                                                      mes_alta == "Julio" ~ "07",
                                                                      mes_alta == "Agosto" ~ "08",
                                                                      mes_alta == "Septiembre" ~ "09",
                                                                      mes_alta == "Octubre" ~ "10",
                                                                      mes_alta == "Noviembre" ~ "11",
                                                                      mes_alta == "Diciembre" ~ "12")) %>% 
  mutate(date = as.Date(paste0(ano_alta,"-",mes,"-01"))) %>% 
  ggplot(aes(date,n,group = origen,color = origen))+
  geom_line(size = 0.5 )+
  geom_label(data = labs,aes(label = comma(n)))+
  geom_smooth()+
  scale_y_sqrt()+
  labs(x="",y = "Número de Atenciones", title = "Número de Atenciones por Servicio de LOCATEL", 
       subtitle = "Entre Enero 2016 - Septiembre 2022", color = "Origen",
       caption = "Fuente:Portal de Datos Abiertos: Servicios a la Población en General")+
  theme(plot.title = element_text(size = 24,color ="#9f2441", face = "bold"),
        plot.subtitle = element_text(size = 20,face = "bold"),
        legend.position = "bottom")


densidad_fldt <- locatel_16_22 %>% count(mes_alta,ano_alta,origen) %>% mutate(mes = case_when(mes_alta == "Enero" ~ "01",
                                                                             mes_alta == "Febrero" ~ "02",
                                                                             mes_alta == "Marzo" ~ "03",
                                                                             mes_alta == "Abril" ~ "04",
                                                                             mes_alta == "Mayo" ~ "05",
                                                                             mes_alta == "Junio" ~ "06",
                                                                             mes_alta == "Julio" ~ "07",
                                                                             mes_alta == "Agosto" ~ "08",
                                                                             mes_alta == "Septiembre" ~ "09",
                                                                             mes_alta == "Octubre" ~ "10",
                                                                             mes_alta == "Noviembre" ~ "11",
                                                                             mes_alta == "Diciembre" ~ "12")) %>% 
  mutate(date = as.Date(paste0(ano_alta,"-",mes,"-01"))) %>% 
  ggplot(aes(date,n,group = origen,color = origen))+
  geom_line(size = 0.5 )+
  geom_smooth()+
  geom_label(data = labs,aes(label = comma(n)))+
  facet_wrap(~origen,scales = "free_y")+
  labs(x="",y = "Número de Atenciones", title = "Número de Atenciones por Servicio de LOCATEL", 
       caption = "Fuente: Portal de Datos Abiertos: Servicios a la Población en General",
       subtitle = "Mensuales entre Enero 2016 - Septiembre 2022", color = "Origen:")+
  theme(plot.title = element_text(size = 24,color ="#9f2441", face = "bold"),
        plot.subtitle = element_text(size = 20,face = "bold"),
        legend.position = "bottom")

# Por qué hay un pico muy fuerte por ahí de 2020; seguramente atención a pandemia pero igual vale la pena revisar

locatel_16_22 %>% filter(origen == "BOTÓN DE APOYO") %>% count(tematica_1) %>% 
  ggplot(aes(reorder(tematica_1,n),n,fill = tematica_1))+
  geom_col()+
  labs(x="",y="Número de Atenciones", 
       title = "Temática principal de las atenciones provistas a través de Botón de Apoyo por LOCATEL",
       subtitle = "Entre 2016 - 09/2022", caption = "Fuente: Portal de Datos Abiertos: Servicios a la Población en General")+
  geom_label(aes(label = comma(n)))+
  theme(plot.title = element_text(size = 24,color ="#9f2441", face = "bold"),
        plot.subtitle = element_text(size = 20,face = "bold"),
        legend.position = "none", 
        axis.text.x = element_text(angle = 90))




