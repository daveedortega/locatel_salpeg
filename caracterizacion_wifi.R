####################################################.
## Analisis inicial de Postes Wi-Fi
## DAOA - 25/09/22
####################################################.

## Preparar Espacio ----
pacman::p_load(tidyverse,sf,janitor,scales)
rm(list=ls())
dev.off()

## Cargar Bases ----
postes_wifi <- read_csv("input/puntos_wifi_gratuito.csv") %>% clean_names()

postes_wifi %>% summarise(sum(puntos_de_acceso))

## Por alcaldia ----

postes_wifi %>% group_by(alcaldia) %>% summarise(puntos_de_acceso = sum(puntos_de_acceso)) %>% 
  ggplot(aes(reorder(alcaldia,puntos_de_acceso),puntos_de_acceso,fill = alcaldia))+
  geom_col(color = "black")+
  geom_label(aes(label = comma(puntos_de_acceso)),size = 10)+
  labs(x = "", y = "Núimero de puntos de acceso por alcaldía", 
        title = "Número de puntos de acceso Wi-Fi por alcaldía", fill = "Alcaldía: ",
        subtitle = "A 28 de Septiembre de 2022", caption = "Fuente: Datos Abiertos - Puntos Wi-Fi")+
  theme(plot.title = element_text(face = "bold", size = 28, color = "#BC955C"),
        plot.subtitle = element_text(face = "bold",size = 22, color = "#9F2441"),
        axis.title.y = element_text(face = "bold"), 
        legend.position = "none")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

# ¿Por cuadrante? ¿Pesarlo por población?

## Cargamos Mapa e Intersecamos por cuadrante ----

cuadrantes_cdmx <- read_sf("~/Desktop/Mapas/cuadrantes/cuadrantes.shp")
cuadrantes_cdmx <- cuadrantes_cdmx %>% st_make_valid()

dummy <- cuadrantes_cdmx %>% st_is_valid()==F 
sum(dummy)


# Latitud Longitud a Geometría Puntos
postes_wifi <- st_as_sf(postes_wifi,coords = c("longitud", "latitud"), 
         crs = 4326)

dummy = postes_wifi %>% st_is_valid()==F 
sum(dummy)

# INTERSECTS != INTERSECTION, returns vector with summary of points over each polygon, lengths counts elements per line

cuadrantes_cdmx$puntos_cuad <-  lengths(st_intersects(cuadrantes_cdmx$geometry,postes_wifi$geometry))

#
cuadrantes_cdmx %>% ggplot(aes(fill = puntos_cuad))+
  geom_sf()

# Recordamos como hacer leaflet ----

pacman::p_load(leaflet,htmltools)

# 10 Bins
bins <- cuadrantes_cdmx$puntos_cuad %>% quantile(probs = seq(0,1,0.1)) %>% as.data.frame() %>% rename(bins = ".")
bins <- bins$bins
bins<- c(bins,Inf)

# less bins, lower bounds

bins <- c(15,25,35,50,Inf)

# Damos dos valores entre los cuales hará cortes con el número de bins generrado previamente sobre el domuni dado
pal <- colorNumeric(c("#ddc9a3", "#9f2441"),domain = NULL)
pal <- colorNumeric(c("Blues"),domain = cuadrantes_cdmx$puntos_cuad)
# labs

labs <- sprintf(
  "<strong>Puntos en %s %s</strong>: %d ",
  cuadrantes_cdmx$sector, cuadrantes_cdmx$no_cdrn, cuadrantes_cdmx$puntos_cuad
) %>% lapply(htmltools::HTML)


leaflet(cuadrantes_cdmx) %>% 
  addTiles() %>% 
  addPolygons(
    fillColor = ~pal(puntos_cuad),
    weight = 0.5,
    opacity = 1,
    color = "black",
    dashArray = "1",
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 2,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labs,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")
  )

## Distribucion de Postes
cuadrantes_cdmx$puntos_cuad %>% summary()

plot(density(cuadrantes_cdmx$puntos_cuad),main = "Distribución de postes por cuadrante", sub ="Portal DatosAbiertos CDMX",
     xlab = "x",ylab = "px")

