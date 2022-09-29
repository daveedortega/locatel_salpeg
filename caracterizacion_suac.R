####################################################.
## Analisis inicial de Victimas en Carpetas FGJ
## DAOA - 28/09/22
####################################################.

## Preparar Espacio ----
pacman::p_load(tidyverse,sf,janitor,scales)
rm(list=ls())
dev.off()

## Cargar Bases ----
tablero_locatel <- read_csv("input/SUAC-csvErs5zD.csv") %>% clean_names()

tablero_locatel %>% count(tipo_de_entrada)




