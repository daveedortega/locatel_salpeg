####################################################.
## Analisis inicial Censo Nacional de Seguridad Pública Estatal INEGI
## DAOA - 04/10/22
####################################################.

## Preparar Espacio ----
pacman::p_load(tidyverse,sf,janitor,scales)
rm(list=ls())
dev.off()

## Cargar Bases ----
# Vamos a Comparar Dos Censos, 2021 y 2011, primero y último levantamiento
censo_2021_sp <- read_csv("input/censo_seguridad21/conjunto_de_datos/m1s1p9_cnspe2021.csv")
censo_2011_sp <- read_csv("/Users/dortega/Downloads/sp_recursosh_cngspspe2011_csv/conjunto_de_datos/rec_h_sp.csv")
indice <- read_csv("input/censo_gspsp11/diccionario_de_datos/diccionario_datos_recursos_humanos_cngspspe2011.csv",
                   locale = locale(encoding = "ISO-8859-2")) %>% clean_names()

# Las encuestas, aunque en general tienen mediciones similares Y la descripción dice que son comparables, sólo se ha mejorado
# Y se han agregado mediciones a estos datos, se ven muy distintas las encuestas. Entre 2011 y 2019 se levantaba el
# Censo Nacional de Gobierno, Seguridad Pública y Sistema Penitenciario Estatales 
# Posteriormente, se dividio en 3 encuestas, la de Gobiernos Estatales, Seguridad Pública y Sistema Penitenciario

# Nos queremos concentrar en Seguridad Pública:

# Los Apartados de Seguridad Pública incluyen:

# 2011 - 
#1)        Ejercicio de la función, 
#2)        Infraestructura, 
#3)        Recursos Humanos
# 2012 - 
#1)        Ejercicio de la función, 
#2)        Infraestructura, 
#3)        Recursos Humanos
# 2013 - 
#1)        Ejercicio de la función, 
#2)        Infraestructura, 
#3)        Recursos Humanos
# 2014 - 
#1)        Ejercicio de la función, 
#2)        Infraestructura, 
#3)        Mando único policial, 
#4)        Recursos Humanos
# 2015 - 
#1)        Ejercicio de la función, 
#2)        Infraestructura para el ejercicio de la función pública, 
#3)        Mando único policial, 
#4)        Personal destinado a funciones de seguridad pública
# 2016 - 
#1)        Ejercicio de la función de seguridad pública, 
#2)        Ejercicio de la función Especifica, 
#3)        Infraestructura para el ejercicio de la función pública, 
#4)        Mando único policial, 
#5)        Personal destinado a funciones de seguridad pública
# 2017 - 
#1)        Ejercicio de funciones específicas, 
#2)        Infraestructura e información estadística para la función de seguridad pública,
#3)        Recursos humanos y presupuestales para la función de seguridad pública, 
#4)        Intervenciones de la policía, 
#6)        Presuntas infracciones y delitos y víctimas registrados
# 2018 - 
#1)        Recursos humanos y presupuestales para la función de seguridad pública, 
#2)        Infraestructura para la función de seguridad pública,
#3)        Generación de información estadística sobre la función de seguridad pública,
#4)        Infraestructura para la generación y análisis de información sobre la función de seguridad pública, 
#5)        Empresas de seguridad privada, 
#6)        Intervenciones de la policía, presuntas infracciones y delitos y víctimas registrados, 
#7)        Probables infractores y responsables registrados y mando único policial, 
#8)        Ejercicio de funciones específicas, 
#9)        Fallecimientos en cumplimiento de la función policial
# 2019 - 
#1)          Recursos humanos y presupuestales para la función de seguridad pública, 
#2)        Infraestructura para la función de seguridad pública,
#3)        Generación de información estadística sobre la función de seguridad pública,
#4)        Infraestructura para la generación y análisis de información sobre la función de seguridad pública, 
#5)        Empresas de seguridad privada, 
#6)        Intervenciones de la policía, presuntas infracciones y delitos y víctimas registrados, 
#7)        Probables infractores y responsables registrados y mando único policial, 
#8)        Fallecimientos en cumplimiento de la función policial
# 2020 - 
#1) Recursos humanos y presupuestales para la función de seguridad pública
#2) Infraestructura para la seguridad pública
#3) Actividades estadísticas y geográficas
#4) Actividades de análisis de información
#5) Régimen disciplinario
#6) Empresas de seguridad privada
#7) Intervenciones policiales, tránsito y vialidad
#8) Presuntas faltas cívicas registradas en las intervenciones policiales
#9) Presuntos delitos registrados en las intervenciones policiales
#10) Probables víctimas registradas en las intervenciones policiales
#11) Probables infractores y responsables registrados en las intervenciones policiales
#12) Asociación interinstitucional para la función de seguridad pública
#13) Fallecimientos en cumplimiento de la función policial
#14) Enfrentamientos
#15) Aseguramiento de armas, narcóticos y vehículos

# Para la nueva encuesta contiene:


# Los datos en formato de Datos Abiertos del INEGI para esta encuesta descargan un ZIP:
# El ZIP contiene: 

# 1) Conjunto de Datos: 
# 2) Diccionario de Datos
# 3) Catálogos
# 4) Metadatos
# 5) Modelo Entidad-Relación

# Analisis Recursos Humanos ----

censo_2011_sp %>% glimpse()

indice











