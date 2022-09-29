####################################################.
## Analisis inicial de Defunciones
## DAOA - 27/09/22
####################################################.

## Preparar Espacio ----
pacman::p_load(tidyverse,sf,janitor,scales)
rm(list=ls())
dev.off()

## Cargar Bases ----
defunciones <- read_csv("input/defunciones_corte_220722_limpia.csv") %>% clean_names()

defunciones %>% glimpse()

## Causa vs Causa Registro
## Causa sólo involucra covid vs. no covid

defunciones %>% count(causa_registro) %>% arrange(desc(n))



defunciones %>% count(lugarmuerte)












