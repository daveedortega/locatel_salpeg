####################################################.
## Código Final para generar bdd de Tesis
## DAOA - 10/10/22
####################################################.

## Preparar Espacio ----
pacman::p_load(tidyverse,sf,janitor,scales)
rm(list=ls())
dev.off()

## Cargar Bases ----
# Incidentes Viales
inviales_15 <- read_csv("input/incidentes_viales/inViales_2014_2015.csv")
inviales_18 <- read_csv("input/incidentes_viales/inViales_2016_2018.csv")
inviales_21 <- read_csv("input/incidentes_viales/inViales_2019_2021.csv")
inviales_22 <- read_csv("input/incidentes_viales/inViales_2022_10.csv") # Actualizada pero seguir actualizando, DICE ALCALDÍA NO DELEGACION
inviales_22 <- inviales_22 %>% rename(delegacion_inicio=alcaldia_inicio,delegacion_cierre=alcaldia_cierre) %>% select(!colonia)
# Juntar Incidentes Viales en una sola BdD
inviales_18_22 <- rbind(inviales_15,inviales_18,inviales_21,inviales_22)
rm(list=setdiff(ls(), "inviales_18_22")) # Eliminamos cosas que no necesitamos

# Afluencia STE y L12 ----

# Por ahora NO, de acuerdo a Antonella

# afluencia_cb <- read_csv("input/afluencia_desglosada_cb_08_2022.csv")
# afluencia_tb <- read_csv("input/afluencia_desglosada_trolebus_08_2022.csv")
# afluencia_tb %>% filter(linea == "Linea 10") # Línea 10 es TB Elevado, revisar por base más reciente porque ahora está vacía
# afluencia_metro <-  read_csv("input/afluenciastc_desglosado_08_2022.csv")
# afluencia_l12_l1 <- afluencia_metro %>% filter(linea %in% c("Linea 12", "Linea 1"))
# rm(afluencia_metro) # Quitamos lo del metro


## Cargar Mapas ----
colonias_cdmx <- read_sf("input/mapas/colonias_iecm_2019/mgpc_2019.shp")
colonias_cdmx <- colonias_cdmx %>% st_make_valid() # Hacemos válidos los polígonos inválidos
cablebus_1 <- read_sf("input/mapas/Cablebús Línea 1.kml")
cablebus_2 <- read_sf("input/mapas/Cablebús Linea 2.kml")
trole_l9 <- read_sf("input/mapas/ste_trolebus_shp/t2.shp") %>% filter(LINEA == "9")
trole_elevado <- read_sf("input/mapas/Trolebús Elevado.kml")
metro_mapa <- read_sf("input/mapas/stcmetro_shp/STC_Metro_lineas_utm14n.shp")
l12 <- metro_mapa %>% filter(LINEA=="12")
l1 <-  metro_mapa %>% filter(LINEA=="1")
intersecciones_seguras <- read_sf("input/mapas/intersecciones_seguras/intersecciones_seguras.shp")

# Arreglamos la base de colonias, tiene NOMBRES duplicados  ----

colonias_cdmx$NOMUT[colonias_cdmx$NOMUT %>% duplicated()]
colonias_cdmx$CVEUT[colonias_cdmx$CVEUT %>% duplicated()] 
# cve NO está duplicada, podemos mejor usar eso, aunque es menos intuitivo


# Intersectamos Vialidades con mapa de colonias para saber cuáles intersectan ----
colonias_cdmx <- st_transform(colonias_cdmx,st_crs(cablebus_1)) # Cambiamos mapa a mismo CRS de otros
colonias_cdmx <- colonias_cdmx %>% st_make_valid()
cb_1 <- lengths(st_intersects(colonias_cdmx,cablebus_1))
cb_2 <- lengths(st_intersects(colonias_cdmx,cablebus_2))
tr_e <- lengths(st_intersects(colonias_cdmx,trole_elevado))
trole_l9 <- st_transform(trole_l9,st_crs(colonias_cdmx))
tr_9 <- lengths(st_intersects(colonias_cdmx,trole_l9))
l1 <- st_transform(l1,st_crs(colonias_cdmx))
l_1 <- lengths(st_intersects(colonias_cdmx,l1))
l12 <- st_transform(l12,st_crs(colonias_cdmx))
l_12 <- lengths(st_intersects(colonias_cdmx,l12))
intersecciones_seguras <- st_transform(intersecciones_seguras,st_crs(colonias_cdmx))
int_s <- lengths(st_intersects(colonias_cdmx,intersecciones_seguras))

# Homologamos a 0,1, i.e., presencia? O que sea intensidad del tratamiento?

colonias_cdmx <- colonias_cdmx %>% cbind(cb_1,cb_2,tr_9,tr_e,int_s,l_1,l_12) %>% mutate(cb_1 = ifelse(cb_1>0,1,0)) %>% 
  mutate(cb_2 = ifelse(cb_2>0,1,0)) %>% mutate(tr_e = ifelse(tr_e>0,1,0)) %>% mutate(int_s = ifelse(int_s>0,1,0)) %>% 
  mutate(l_12 = ifelse(l_12>0,1,0)) %>% mutate(l_1 = ifelse(l_1>0,1,0))%>% mutate(tr_9 = ifelse(tr_9>0,1,0))

rm(l_1,l_12,cb_1,cb_2,tr_e,tr_9,int_s) # Eliminamos lo que no necesitamos

# Agrupamos incidentes viales por mes ----

inviales_18_22 %>% glimpse()
# Tomamos fecha de creación, no tomamos alcaldia porque lo vamos a hacer por georeferencia 
inviales_18_22 <- inviales_18_22 %>% mutate(ano_mes = format(fecha_creacion,format = "%Y-%m")) %>% 
  select(ano_mes,tipo_incidente_c4,incidente_c4,latitud,longitud,tipo_entrada)

# Revisamos, debremos de separar por tipo de incidente?  me imagino que sí ----
inviales_18_22 %>% count(incidente_c4,tipo_incidente_c4) %>% 
  ggplot(aes(reorder(tipo_incidente_c4,n),n,group = incidente_c4, fill = incidente_c4))+
  geom_col(color = "black", position = "stack")+
  coord_flip()+
  labs(x="Incidente", y="Número de Reportes al C5", title = "Reportes al C5 por tipo de incidente",
       subtitle = "Por tipo de incidente en la CDMX, ENTRE 2014 - Octubre 2022", fill = "Tipo de Incidente", 
       caption = "Fuente: Datos Abiertos - Incidentes Viales Reportados al C5")+
  theme(plot.title = element_text(size = 22, face = "bold", color = "#9f2241"),
        plot.subtitle = element_text(size = 18, face = "bold"),
        legend.position = "bottom")+
  scale_y_sqrt()

# Invertimos relación -----
inviales_18_22 %>% count(incidente_c4,tipo_incidente_c4) %>% 
  ggplot(aes(reorder(incidente_c4,n),n,group = tipo_incidente_c4, fill = tipo_incidente_c4))+
  geom_col(color = "black", position = "stack")+
  coord_flip()+
  labs(x="Incidente", y="Número de Reportes al C5", title = "Reportes al C5 por tipo de incidente",
       subtitle = "Por tipo de incidente en la CDMX, ENTRE 2014 - Octubre 2022", fill = "Tipo de Incidente", 
       caption = "Fuente: Datos Abiertos - Incidentes Viales Reportados al C5")+
  theme(plot.title = element_text(size = 22, face = "bold", color = "#9f2241"),
        plot.subtitle = element_text(size = 18, face = "bold"),
        legend.position = "bottom")+
  scale_y_sqrt()
# Apagar
dev.off()
# Selección 
# Seccionamos por tipo de accidente, Atropellados, Choques con y sin Lesionados, Motociclista, Ciclissta y así 
# Pensar si reclasifico por motociclista, ciclista, persona atropellada, etc.

# Primero convertimos en punto geográffico para que luego no haya problemas -----

inviales_18_22 <- inviales_18_22 %>% na.omit() %>% # No acepta cosas sin valores, los quitamos
  st_as_sf(coords = c("longitud", "latitud"),crs = 4326)

# Lista con 17 tipos de Incidentes Viales -----
# Lista de 17 DFs
incidentes_separados<- inviales_18_22 %>% group_by(incidente_c4) %>% group_split()
accidente_auto <- incidentes_separados[1] %>% as.data.frame()
atropellado <- incidentes_separados[2] %>% as.data.frame()
choque_cl <- incidentes_separados[3] %>% as.data.frame()
choque_cp <- incidentes_separados[4] %>% as.data.frame()
choque_sl <- incidentes_separados[5] %>% as.data.frame()
ciclista <- incidentes_separados[6] %>% as.data.frame()
ferroviario <- incidentes_separados[7] %>% as.data.frame()
incidente_transito <- incidentes_separados[8] %>% as.data.frame() 
monopatin <- incidentes_separados[9] %>% as.data.frame()
motos <- incidentes_separados[10] %>% as.data.frame()
otros <- incidentes_separados[11] %>% as.data.frame()
persona_atrapada <- incidentes_separados[12] %>% as.data.frame()
persona_atropellada <- incidentes_separados[13] %>% as.data.frame() # pocos registros, tiene q ver con sismo en tipo de incidente
vehiculo_atrapado <- incidentes_separados[14] %>% as.data.frame()
vehiculo_varado <- incidentes_separados[15] %>% as.data.frame()
vehiculo_desbarrancado <- incidentes_separados[16] %>% as.data.frame()
volcadura <- incidentes_separados[17] %>% as.data.frame()
rm(incidentes_separados) # Clear unused stuff

# Convertimos cada una de las 17 listas en una lista por mes -----
accidente_auto_fechas <- accidente_auto %>% group_by(ano_mes) %>% group_split() 
# PFF, son 103 dfs, pero creo que está bien, lo que luego voy a necesitar
# va a ser iterar sobre las listas particulares
atropellado_fechas <- atropellado %>% group_by(ano_mes) %>% group_split()
choque_cl_fechas <- choque_cl %>% group_by(ano_mes) %>% group_split()
choque_cp_fechas <- choque_cp %>% group_by(ano_mes) %>% group_split()
choque_sl_fechas <- choque_sl %>% group_by(ano_mes) %>% group_split()
ciclista_fechas <- ciclista %>% group_by(ano_mes) %>% group_split()
ferroviario_fechas <- ferroviario %>% group_by(ano_mes) %>% group_split()
incidente_transito_fechas <- incidente_transito %>% group_by(ano_mes) %>% group_split()
monopatin_fechas <- monopatin %>% group_by(ano_mes) %>% group_split()
motos_fechas <- motos %>% group_by(ano_mes) %>% group_split()
otros_fechas <- otros %>% group_by(ano_mes) %>% group_split()
persona_atrapada_fechas <- persona_atrapada %>% group_by(ano_mes) %>% group_split()
persona_atropellada_fechas <- persona_atropellada %>% group_by(ano_mes) %>% group_split()
vehiculo_atrapado_fechas <- vehiculo_atrapado %>% group_by(ano_mes) %>% group_split()
vehiculo_varado_fechas <- vehiculo_varado %>% group_by(ano_mes) %>% group_split()
vehiculo_desbarrancado_fechas <- vehiculo_desbarrancado %>% group_by(ano_mes) %>% group_split()
volcadura_fechas <- volcadura %>% group_by(ano_mes) %>% group_split()
rm(atropellado,choque_cl,choque_cp,choque_sl,ciclista,ferroviario,incidente_transito, monopatin,motos, 
   otros,persona_atrapada,persona_atropellada,vehiculo_atrapado,vehiculo_varado, vehiculo_desbarrancado,volcadura)

# Ahora sí, tenemos que iterar un lengths, pegar a un df como columna para tener incidentes por mes por colonia ----
test <- data.frame(id = 1:length(colonias_cdmx$geometry))# La función necesita saber cuántas tiene
colonias_cdmx <- st_transform(colonias_cdmx,st_crs(accidente_auto_fechas[[1]]$geometry))
colonias_cdmx <- colonias_cdmx %>% st_make_valid()
# Funcion que itera ----
iterated_intersection <- function(x,y,z){
  for (i in 1:length(x)){
    df <- x[[i]] # ith data frame
    fetch_date <- df[1,1] #get date, always in first object in df
    fetch_what <- paste(df[1,2],df[1,3]) # get what it is
    isects <- lengths(st_intersects(z,df$geometry)) # vector of intersections
    y[,ncol(y)+1] <- isects # después lo puedo colapsar como columna de atributos con pivot wider
    colnames(y)[ncol(y)] <- paste(fetch_what,fetch_date) # rename w date
    print(paste(i, "intersected data frames out of:", length(x)))
  }
  return(y)
}


# Aplicamos función a todos -----
tok <- Sys.time()
accidente_auto_fechas_r<- iterated_intersection(accidente_auto_fechas,test,colonias_cdmx)
atropellado_fechas_r <- iterated_intersection(atropellado_fechas,test,colonias_cdmx)
choque_cl_fechas_r <- iterated_intersection(choque_cl_fechas,test,colonias_cdmx)
choque_cp_fechas_r <- iterated_intersection(choque_cp_fechas,test,colonias_cdmx)
choque_sl_fechas_r <- iterated_intersection(choque_sl_fechas,test,colonias_cdmx)
ciclista_fechas_r <- iterated_intersection(ciclista_fechas,test,colonias_cdmx)
ferroviario_fechas_r <- iterated_intersection(ferroviario_fechas,test,colonias_cdmx)
incidente_transito_fechas_r <- iterated_intersection(incidente_transito_fechas,test,colonias_cdmx)
monopatin_fechas_r <- iterated_intersection(monopatin_fechas,test,colonias_cdmx)
motos_fechas_r <- iterated_intersection(motos_fechas,test,colonias_cdmx)
otros_fechas_r <- iterated_intersection(otros_fechas,test,colonias_cdmx)
persona_atrapada_fechas_r <- iterated_intersection(persona_atrapada_fechas,test,colonias_cdmx)
persona_atropellada_fechas_r <- iterated_intersection(persona_atropellada_fechas,test,colonias_cdmx)
vehiculo_atrapado_fechas_r <- iterated_intersection(vehiculo_atrapado_fechas,test,colonias_cdmx)
vehiculo_varado_fechas_r <- iterated_intersection(vehiculo_varado_fechas,test,colonias_cdmx)
vehiculo_desbarrancado_fechas_r <- iterated_intersection(vehiculo_desbarrancado_fechas,test,colonias_cdmx)
volcadura_fechas_r <- iterated_intersection(volcadura_fechas,test,colonias_cdmx)
tik <- Sys.time()
print(tik-tok) # Toma 3 mins todo el chiste
# Liberamos lo que sobra
rm(accidente_auto_fechas,atropellado_fechas,choque_cl_fechas,choque_cp_fechas, choque_sl_fechas, ciclista_fechas, 
   ferroviario_fechas, incidente_transito_fechas, monopatin_fechas, motos_fechas, otros_fechas, persona_atrapada_fechas, 
   vehiculo_atrapado_fechas, vehiculo_varado_fechas, vehiculo_desbarrancado_fechas, volcadura_fechas,tik,tok,
   persona_atropellada_fechas_r)

## Deben ser 107 columnas por cada uno, por lo que vamos a usar uno completo y luego pegar los demas ----
a_0 <- cbind(colonia = colonias_cdmx$CVEUT,choque_cl_fechas_r)
a_0 <- a_0 %>% select(!id)
a_0 <- a_0 %>% pivot_longer(!colonia,names_to = "incidente_fecha",values_to = "incidentes")
# Long Format
# Separar en dos fecha y accidente
incidentes_wf <- a_0 %>% separate(incidente_fecha,into = c("accidente","ano_mes"), sep = "(?<=[a-zA-Z])\\s*(?=[0-9])")
incidentes_wf <- incidentes_wf %>% pivot_wider(names_from = accidente,values_from = incidentes) %>% clean_names()

# Realizamos el mismo procedimiento con cada una y luego sólo hacemos left_join con el df original

a_1 <- cbind(colonia = colonias_cdmx$CVEUT,atropellado_fechas_r) %>% select(!id)%>% 
  pivot_longer(!colonia,names_to = "incidente_fecha",values_to = "incidentes") %>% 
  separate(incidente_fecha,into = c("accidente","ano_mes"), sep = "(?<=[a-zA-Z])\\s*(?=[0-9])") %>% 
  pivot_wider(names_from = accidente,values_from = incidentes)%>% clean_names()
a_2 <- cbind(colonia = colonias_cdmx$CVEUT,choque_cp_fechas_r) %>% select(!id) %>% 
  pivot_longer(!colonia,names_to = "incidente_fecha",values_to = "incidentes") %>% 
  separate(incidente_fecha,into = c("accidente","ano_mes"), sep = "(?<=[a-zA-Z])\\s*(?=[0-9])")%>% 
  pivot_wider(names_from = accidente,values_from = incidentes) %>% clean_names()
a_3 <- cbind(colonia = colonias_cdmx$CVEUT,choque_sl_fechas_r)%>% select(!id) %>% 
  pivot_longer(!colonia,names_to = "incidente_fecha",values_to = "incidentes") %>% 
  separate(incidente_fecha,into = c("accidente","ano_mes"), sep = "(?<=[a-zA-Z])\\s*(?=[0-9])")%>% 
  pivot_wider(names_from = accidente,values_from = incidentes) %>% clean_names()
a_4 <- cbind(colonia = colonias_cdmx$CVEUT,ciclista_fechas_r)%>% select(!id) %>% 
  pivot_longer(!colonia,names_to = "incidente_fecha",values_to = "incidentes") %>% 
  separate(incidente_fecha,into = c("accidente","ano_mes"), sep = "(?<=[a-zA-Z])\\s*(?=[0-9])")%>% 
  pivot_wider(names_from = accidente,values_from = incidentes) %>% clean_names()
a_5 <- cbind(colonia = colonias_cdmx$CVEUT,ferroviario_fechas_r)%>% select(!id)%>% 
  pivot_longer(!colonia,names_to = "incidente_fecha",values_to = "incidentes")  %>% 
  separate(incidente_fecha,into = c("accidente","ano_mes"), sep = "(?<=[a-zA-Z])\\s*(?=[0-9])")%>% 
  pivot_wider(names_from = accidente,values_from = incidentes) %>% clean_names()
a_6 <- cbind(colonia = colonias_cdmx$CVEUT,incidente_transito_fechas_r)%>% select(!id) %>% 
  pivot_longer(!colonia,names_to = "incidente_fecha",values_to = "incidentes") %>% 
  separate(incidente_fecha,into = c("accidente","ano_mes"), sep = "(?<=[a-zA-Z])\\s*(?=[0-9])")%>% 
  pivot_wider(names_from = accidente,values_from = incidentes) %>% clean_names()
a_7 <- cbind(colonia = colonias_cdmx$CVEUT,monopatin_fechas_r)%>% select(!id) %>% 
  pivot_longer(!colonia,names_to = "incidente_fecha",values_to = "incidentes") %>% 
  separate(incidente_fecha,into = c("accidente","ano_mes"), sep = "(?<=[a-zA-Z])\\s*(?=[0-9])")%>% 
  pivot_wider(names_from = accidente,values_from = incidentes) %>% clean_names()
a_8 <- cbind(colonia = colonias_cdmx$CVEUT,motos_fechas_r)%>% select(!id) %>% 
  pivot_longer(!colonia,names_to = "incidente_fecha",values_to = "incidentes") %>% 
  separate(incidente_fecha,into = c("accidente","ano_mes"), sep = "(?<=[a-zA-Z])\\s*(?=[0-9])")%>% 
  pivot_wider(names_from = accidente,values_from = incidentes) %>% clean_names()
a_9 <- cbind(colonia = colonias_cdmx$CVEUT,otros_fechas_r)%>% select(!id) %>% 
  pivot_longer(!colonia,names_to = "incidente_fecha",values_to = "incidentes") %>% 
  separate(incidente_fecha,into = c("accidente","ano_mes"), sep = "(?<=[a-zA-Z])\\s*(?=[0-9])")%>% 
  pivot_wider(names_from = accidente,values_from = incidentes) %>% clean_names()
a_10 <- cbind(colonia = colonias_cdmx$CVEUT,persona_atrapada_fechas_r)%>% select(!id) %>% 
  pivot_longer(!colonia,names_to = "incidente_fecha",values_to = "incidentes") %>% 
  separate(incidente_fecha,into = c("accidente","ano_mes"), sep = "(?<=[a-zA-Z])\\s*(?=[0-9])")%>% 
  pivot_wider(names_from = accidente,values_from = incidentes) %>% clean_names()
a_11 <- cbind(colonia = colonias_cdmx$CVEUT,persona_atropellada_fechas_r)%>% select(!id) %>% 
  pivot_longer(!colonia,names_to = "incidente_fecha",values_to = "incidentes") %>% 
  separate(incidente_fecha,into = c("accidente","ano_mes"), sep = "(?<=[a-zA-Z])\\s*(?=[0-9])")%>% 
  pivot_wider(names_from = accidente,values_from = incidentes) %>% clean_names()
a_12 <- cbind(colonia = colonias_cdmx$CVEUT,volcadura_fechas_r)%>% select(!id) %>% 
  pivot_longer(!colonia,names_to = "incidente_fecha",values_to = "incidentes")  %>% 
  separate(incidente_fecha,into = c("accidente","ano_mes"), sep = "(?<=[a-zA-Z])\\s*(?=[0-9])")%>% 
  pivot_wider(names_from = accidente,values_from = incidentes) %>% clean_names()
a_13 <- cbind(colonia = colonias_cdmx$CVEUT,vehiculo_varado_fechas_r)%>% select(!id) %>% 
  pivot_longer(!colonia,names_to = "incidente_fecha",values_to = "incidentes") %>% 
  separate(incidente_fecha,into = c("accidente","ano_mes"), sep = "(?<=[a-zA-Z])\\s*(?=[0-9])")%>% 
  pivot_wider(names_from = accidente,values_from = incidentes) %>% clean_names()
a_14 <- cbind(colonia = colonias_cdmx$CVEUT,vehiculo_desbarrancado_fechas_r)%>% select(!id) %>% 
  pivot_longer(!colonia,names_to = "incidente_fecha",values_to = "incidentes") %>% 
  separate(incidente_fecha,into = c("accidente","ano_mes"), sep = "(?<=[a-zA-Z])\\s*(?=[0-9])")%>% 
  pivot_wider(names_from = accidente,values_from = incidentes) %>% clean_names()

# leftjoin into final df ----
incidentes_wf <- incidentes_wf %>% left_join(a_1,by = c("colonia","ano_mes"))
incidentes_wf <- incidentes_wf %>% left_join(a_2,by = c("colonia","ano_mes"))
incidentes_wf <- incidentes_wf %>% left_join(a_3,by = c("colonia","ano_mes"))
incidentes_wf <- incidentes_wf %>% left_join(a_4,by = c("colonia","ano_mes"))
incidentes_wf <- incidentes_wf %>% left_join(a_5,by = c("colonia","ano_mes"))
incidentes_wf <- incidentes_wf %>% left_join(a_6,by = c("colonia","ano_mes"))
incidentes_wf <- incidentes_wf %>% left_join(a_7,by = c("colonia","ano_mes"))
incidentes_wf <- incidentes_wf %>% left_join(a_8,by = c("colonia","ano_mes"))
incidentes_wf <- incidentes_wf %>% left_join(a_9,by = c("colonia","ano_mes"))
incidentes_wf <- incidentes_wf %>% left_join(a_10,by = c("colonia","ano_mes"))
incidentes_wf <- incidentes_wf %>% left_join(a_11,by = c("colonia","ano_mes"))
incidentes_wf <- incidentes_wf %>% left_join(a_12,by = c("colonia","ano_mes"))
incidentes_wf <- incidentes_wf %>% left_join(a_13,by = c("colonia","ano_mes"))
incidentes_wf <- incidentes_wf %>% left_join(a_14,by = c("colonia","ano_mes"))
# Hacemos NA 0
incidentes_wf <- incidentes_wf %>% replace(is.na(.), 0)
incidentes_wf <- incidentes_wf %>% rename(CVEUT = colonia)
# Tenemos formato ancho, conservamos pero queremos en realidad sólo formato largo
incidentes_lf <- incidentes_wf %>% pivot_longer(!c(CVEUT,ano_mes),names_to = "accidente",values_to = "incidencia")
# Pegamos mapa
incidentes_lf <- incidentes_lf %>%  left_join(colonias_cdmx, by = "CVEUT")
## limpiar espacio 
rm(accidente_auto_fechas_r,atropellado_fechas_r,choque_cl_fechas_r,choque_cp_fechas_r, choque_sl_fechas_r, ciclista_fechas_r, 
   ferroviario_fechas_r, incidente_transito_fechas_r, monopatin_fechas_r, motos_fechas_r, otros_fechas_r, persona_atrapada_fechas_r, 
   vehiculo_atrapado_fechas_r, vehiculo_varado_fechas_r, vehiculo_desbarrancado_fechas_r, volcadura_fechas_r, 
   a_0,a_1,a_2,a_3,a_4,a_5,a_6,a_7,a_8,a_9,a_10,a_11,a_12,a_13,a_14)

# Data frame final 
incidentes_lf <- incidentes_lf %>% mutate(fecha = as.Date(paste0(ano_mes,"-01"))) 

# Estadísticas Generales ----

# Total de incidentes ----
incidentes_lf %>% colnames()
fechas_importantes <- data.frame(evento = c("Inicia Cuarentena por Covid-19", "Inicia Cablebus L1", 
                                            "Inicia Cablebus L2", "Tragedia L12", "Inicia Trolebus Elevado", "Reabre L9 Trolebus"),
                                 fecha = c(as.Date("2020-04-01"),as.Date("2021-07-11"),as.Date("2021-08-08"), 
                                           as.Date("2021-05-03"), as.Date("2022-10-29"), as.Date("2021-01-30")), 
                                 incidentes = c(15000,11000,14000,18000,15000,15000))
colorcitos <- c("#C94F4B","#69C1CB","#69C1CB","#B1994E", "#2C63AC","#2C63AC")

incidentes_lf %>% group_by(fecha) %>% summarise(incidentes = sum(incidencia)) %>% 
  ggplot(aes(fecha,incidentes))+
  geom_line(color = "#9f2441")+
  geom_smooth()+
  labs(x="", y="Incidentes Viales", title = "Incidentes Viales Totales en CDMX", 
       subtitle = "entre 2014 - 10/2022 Reportados al C5", caption = "Fuente: Portal de Datos Abiertos - CDMX")+
  theme(plot.title = element_text(size = 28, face = "bold", color = "#9f2441"),
        plot.subtitle = element_text(size = 22, face="bold"))+
  geom_point(data = fechas_importantes)+
  geom_label(data = fechas_importantes, aes(label = str_wrap(evento,width = 10)),color = colorcitos)
  

## Mayores colonias con incidentes por año ----

incidentes_lf %>% separate(ano_mes, into = c("ano", "mes"), sep = "-") %>% group_by(colonia,ano) %>% 
  summarise(incidentes = sum(incidentes)) %>%  ungroup() %>% 
  slice_max(order_by = incidentes,n=100) %>% 
  ggplot(aes(reorder(colonia,incidentes),incidentes,group = ano,fill = ano))+
  geom_col(color = "black")+
  labs(x="", y="Incidentes Viales", title = "Colonias con Más incidentes Viales por año", fill = "Año de inicio",
       subtitle = "entre 2014 - 10/2022 Reportados al C5", caption = "Fuente: Portal de Datos Abiertos - CDMX")+
  theme(plot.title = element_text(size = 28, face = "bold", color = "#9f2441"),
        plot.subtitle = element_text(size = 22, face="bold"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  coord_flip()
  
# Total de Incidentes NO disminuye sustancialmente: Incidentes Viales tiene  entradas, 
# La suma de las interacciones con los polígonos tiene 1,732,154, se pierden 5,000 incidentes, pero en general
# SÍ CAEN EN LOS POLÍGONOS DE COLONIAS QUE TENEMOS
# Actualización a base de 10/22: 1,802,170 incidentes en base original, 1,796,780 en intersecciones

# Incidentes más presentes:

incidentes_lf %>% group_by(accidente) %>% summarise(n = sum(incidentes)) %>% 
  ggplot(aes(reorder(accidente,n),n, fill = accidente))+
  geom_col(color = "black")+
  labs(x="", y="Incidentes Viales", title = "Incidentes Viales entre 2014 - 10/2022", fill = "Tipo de Incidente",
       subtitle = "Por tipo de Incidente entre 2014 - 10/2022 Reportados al C5",
       caption = "Fuente: Portal de Datos Abiertos - CDMX")+
  theme(plot.title = element_text(size = 28, face = "bold", color = "#9f2441"),
        plot.subtitle = element_text(size = 22, face="bold"),
        legend.position = "bottom",
        axis.text.x = element_blank())

# 10 colonias por año con más incidentes ----
# Medio difícil de ver, luego lo arreglo
incidentes_lf %>% separate(ano_mes, into = c("ano", "mes"), sep = "-") %>% group_by(colonia,ano) %>% 
  summarise(incidentes = sum(incidencia)) %>%  ungroup() %>% group_by(ano) %>% 
  slice_max(order_by = incidentes,n=10) %>% 
  ggplot(aes(reorder(colonia, incidentes),incidentes, fill = ano))+
  geom_col(color = "black")+
  labs(x="", y="Incidentes Viales", title = "10 Colonias con Más incidentes Viales por año", fill = "Año de inicio",
       subtitle = "entre 2014 - 10/2022 Reportados al C5", caption = "Fuente: Portal de Datos Abiertos - CDMX")+
  facet_wrap(~ano, scales = "free")+
  theme(plot.title = element_text(size = 28, face = "bold", color = "#9f2441"),
        plot.subtitle = element_text(size = 22, face="bold"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  coord_flip()

# Linea de tiempo por grupos en colonias tocadas por Cablebus L1-----

colonias_cb1_1 <- colonias_cdmx %>% filter(cb_1>0) %>% select(CVEUT) 
colonias_cb1_1 <- colonias_cb1_1$CVEUT

incidentes_lf %>% filter(CVEUT %in% colonias_cb1_1) %>% group_by(fecha) %>% summarise(incidentes = sum(incidencia)) %>% 
  ggplot(aes(fecha,incidentes))+
  geom_line(color = "#69C1CB",size = 1)+
  geom_smooth()+
  labs(x="",y="Incidentes Viales", title="Incidentes Víales Totales en la Región cubierta por CableBus Línea 1",
       subtitle = "entre 2014 - 10/2022 Reportados al C5", caption = "Fuente: Portal de Datos Abiertos - CDMX")+
  theme(plot.title = element_text(size = 28, face = "bold", color = "#9f2441"),
        plot.subtitle = element_text(size = 22, face="bold"))
# Linea de tiempo por grupos en colonias tocadas por Cablebus L2-----

colonias_cb1_2 <- colonias_cdmx %>% filter(cb_2>0) %>% select(CVEUT) 
colonias_cb1_2 <- colonias_cb1_2$CVEUT

incidentes_lf %>% filter(CVEUT %in% colonias_cb1_2) %>% group_by(fecha) %>% summarise(incidentes = sum(incidencia)) %>% 
  ggplot(aes(fecha,incidentes))+
  geom_line(color = "#69C1CB",size = 1)+
  geom_smooth()+
  labs(x="",y="Incidentes Viales", title="Incidentes Víales Totales en la Región cubierta por CableBus Línea 2",
       subtitle = "entre 2014 - 10/2022 Reportados al C5", caption = "Fuente: Portal de Datos Abiertos - CDMX")+
  theme(plot.title = element_text(size = 28, face = "bold", color = "#9f2441"),
        plot.subtitle = element_text(size = 22, face="bold"))


# Linea de tiempo por grupos en colonias tocadas por L12 -----

colonias_l12 <- colonias_cdmx %>% filter(l_12>0) %>% select(CVEUT) 
colonias_l12 <- colonias_l12$CVEUT

incidentes_lf %>% filter(CVEUT %in% colonias_l12) %>% group_by(fecha) %>% summarise(incidentes = sum(incidencia)) %>% 
  ggplot(aes(fecha,incidentes))+
  geom_line(color = "#B1994E",size = 1)+
  geom_smooth()+
  labs(x="",y="Incidentes Viales", title="Incidentes Víales Totales en la Región cubierta por la línea 12 del Metro",
       subtitle = "entre 2014 - 10/2022 Reportados al C5", caption = "Fuente: Portal de Datos Abiertos - CDMX")+
  theme(plot.title = element_text(size = 28, face = "bold", color = "#9f2441"),
        plot.subtitle = element_text(size = 22, face="bold"))

# Linea de tiempo por grupos en colonias tocadas por L1 -----

colonias_l1 <- colonias_cdmx %>% filter(l_1>0) %>% select(CVEUT) 
colonias_l1 <- colonias_l1$CVEUT

incidentes_lf %>% filter(CVEUT %in% colonias_l1) %>% group_by(fecha) %>% summarise(incidentes = sum(incidencia)) %>% 
  ggplot(aes(fecha,incidentes))+
  geom_line(color = "#D3548E",size = 1)+
  geom_smooth()+
  labs(x="",y="Incidentes Viales", title="Incidentes Víales Totales en la Región cubierta por la línea 1 del Metro",
       subtitle = "entre 2014 - 10/2022 Reportados al C5", caption = "Fuente: Portal de Datos Abiertos - CDMX")+
  theme(plot.title = element_text(size = 28, face = "bold", color = "#D3548E"),
        plot.subtitle = element_text(size = 22, face="bold"))

# Linea de tiempo por grupos en colonias tocadas por  Trole Elevado-----

colonias_te <- colonias_cdmx %>% filter(tr_e>0) %>% select(CVEUT)  
colonias_te <- colonias_te$CVEUT

incidentes_lf %>% filter(CVEUT %in% colonias_te) %>% group_by(fecha) %>% summarise(incidentes = sum(incidencia)) %>% 
  ggplot(aes(fecha,incidentes))+
  geom_line(color = "#2C63AC",size = 1)+
  geom_smooth(color = "green")+
  labs(x="",y="Incidentes Viales", title="Incidentes Víales Totales en la Región cubierta por el Trolebus Elevado",
       subtitle = "entre 2014 - 10/2022 Reportados al C5", caption = "Fuente: Portal de Datos Abiertos - CDMX")+
  theme(plot.title = element_text(size = 28, face = "bold", color = "#9f2441"),
        plot.subtitle = element_text(size = 22, face="bold"))

# Linea de tiempo por grupos en colonias tocadas por Trole L9 -----

colonias_t9 <- colonias_cdmx %>% filter(tr_9>0) %>% select(CVEUT)  
colonias_t9 <- colonias_t9$CVEUT

incidentes_lf %>% filter(CVEUT %in% colonias_t9) %>% group_by(fecha) %>% summarise(incidentes = sum(incidencia)) %>% 
  ggplot(aes(fecha,incidentes))+
  geom_line(color = "#2C63AC",size = 1)+
  geom_smooth(color = "green")+
  labs(x="",y="Incidentes Viales", title="Incidentes Víales Totales en la Región cubierta por el Trolebus L9",
       subtitle = "entre 2014 - 10/2022 Reportados al C5", caption = "Fuente: Portal de Datos Abiertos - CDMX")+
  theme(plot.title = element_text(size = 28, face = "bold", color = "#9f2441"),
        plot.subtitle = element_text(size = 22, face="bold"))



