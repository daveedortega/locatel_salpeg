####################################################.
## Código con baase de colonias CONAPO 2020 para generar bdd de Incidentes Viales
## DAOA - 01/03/23
####################################################.

## Preparar Espacio ----
dev.off()
pacman::p_load(tidyverse,sf,janitor,scales)
rm(list=ls())

## Cargar Bases ----
# Incidentes Viales
inviales_15 <- read_csv("input/incidentes_viales/inViales_2014_2015.csv")
inviales_18 <- read_csv("input/incidentes_viales/inViales_2016_2018.csv")
inviales_21 <- read_csv("input/incidentes_viales/inViales_2019_2021.csv")
# Actualizado hasta enero 2023
inviales_22 <- read_csv("input/incidentes_viales/inViales_2022_2023.csv") # Además dice alcaldía no colonia
inviales_22 <- inviales_22 %>% rename(delegacion_inicio=alcaldia_inicio,delegacion_cierre=alcaldia_cierre) %>% 
  select(!c(colonia))
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
colonias_cdmx <- read_sf("~/Desktop/Mapas/colonias_indicemarginacionurbana_2020/cdmx_imu_2020/cdmx_imu2020.shp")
colonias_cdmx <- colonias_cdmx %>% st_make_valid() # Hacemos válidos los polígonos inválidos
# Cambiamos viejos mapas por buffers
cablebus_1 <- read_sf("~/Desktop/Mapas/cb_l1_e/cb_l1_b500.shp")
cablebus_2 <- read_sf("~/Desktop/Mapas/cb_l2_shp/cb_l2_b500.shp")
trole_l9 <- read_sf("~/Desktop/Mapas/trolebus/trolebus_l9_b500.shp")
trole_elevado <- read_sf("~/Desktop/Mapas/tr_e_shp/tr_e_b500_v.shp")
l12 <- read_sf("~/Desktop/Mapas/STC_shp/stc_l12_b500.shp") # Toda la línea
el12 <- read_sf("~/Desktop/Mapas/STC_shp/stc_l12_eb500_feb.shp") # Estaciones cerradas desde feb 2023, i.e. las demás se abrieron
l1 <-  read_sf("~/Desktop/Mapas/STC_shp/stc_l1_ec500m.shp") # Estaciones Cerradas
# intersecciones_seguras <- read_sf("input/mapas/intersecciones_seguras/intersecciones_seguras.shp")

# Intersectamos Vialidades con mapa de colonias para saber cuáles intersectan ----
colonias_cdmx <- st_transform(colonias_cdmx,st_crs(cablebus_1)) # Cambiamos mapa a mismo CRS de otros, ahora todos tienen el mismo
colonias_cdmx <- colonias_cdmx %>% st_make_valid()
cb_1 <- lengths(st_intersects(colonias_cdmx,cablebus_1))
cb_2 <- lengths(st_intersects(colonias_cdmx,cablebus_2))
tr_e <- lengths(st_intersects(colonias_cdmx,trole_elevado))
tr_9 <- lengths(st_intersects(colonias_cdmx,trole_l9))
l_1 <- lengths(st_intersects(colonias_cdmx,l1))
l_12 <- lengths(st_intersects(colonias_cdmx,l12))
# intersecciones_seguras <- st_transform(intersecciones_seguras,st_crs(colonias_cdmx))
# int_s <- lengths(st_intersects(colonias_cdmx,intersecciones_seguras))

# Homologamos a 0,1, para presencia o no del tratamiento

colonias_cdmx <- colonias_cdmx %>% cbind(cb_1,cb_2,tr_9,tr_e,l_1,l_12) %>% mutate(cb_1 = ifelse(cb_1>0,1,0)) %>% 
  mutate(cb_2 = ifelse(cb_2>0,1,0)) %>% mutate(tr_e = ifelse(tr_e>0,1,0)) %>% 
  mutate(l_12 = ifelse(l_12>0,1,0)) %>% mutate(l_1 = ifelse(l_1>0,1,0))%>% mutate(tr_9 = ifelse(tr_9>0,1,0))

rm(l_1,l_12,cb_1,cb_2,tr_e,tr_9) # Eliminamos lo que no necesitamos

# Escribimos para seleccionar manualmente las colonias en QGIS aquellas colonias aledañoas ----
# st_write(colonias_cdmx, "./output/colonias_cdmx_int.shp")
# Ahora reescribimos con buffer
# st_write(colonias_cdmx, "./output/colonias_cdmx_int_buffer.shp")


# Agrupamos incidentes viales por mes ----

inviales_18_22 %>% glimpse()
# Tomamos fecha de creación, no tomamos alcaldia porque lo vamos a hacer por georeferencia 
inviales_18_22 <- inviales_18_22 %>% mutate(ano_mes = format(fecha_creacion,format = "%Y-%m")) %>% 
  select(ano_mes,tipo_incidente_c4,incidente_c4,latitud,longitud,tipo_entrada)



# Primero convertimos en punto geográffico para que luego no haya problemas -----

inviales_18_22 <- inviales_18_22 %>% na.omit() %>% # No acepta cosas sin valores, los quitamos
  st_as_sf(coords = c("longitud", "latitud"),crs = 4326)

## Separar base de incidentes viales por 1/17 categorías? ----
test_17_split <- inviales_18_22 %>% mutate(incidente_tipificado = paste(incidente_c4,tipo_incidente_c4)) %>% # Conjuntamos incidente y tipo
  select(ano_mes,incidente_tipificado,geometry) %>% group_by(incidente_tipificado) %>% group_split() # Se generan 25 categorías
# Genera un split de 25 delitos categorizados

# accidentes automovilisticos ----
accidente_auto_cadaver <- test_17_split[[1]] # 1,527 obs
accidente_auto_detencion <- test_17_split[[2]] # 273 obs
accidente_auto_lesionado <- test_17_split[[3]] # 6,054 obs
# atropellados ----
atropellado_cadaver <- test_17_split[[4]] # 2,312 obs
atropellado_detencion <- test_17_split[[5]] # 901 obs
atropellado_lesionado <- test_17_split[[6]] # ------------------ 236,739 Done
atropellado_sismo <- test_17_split[[21]] # 47 obs
# choque con lesionados ----
choque_cl_accidente <- test_17_split[[7]] # -------------------- 431,755 Done
choque_cl_sismo <- test_17_split[[8]] # 531 obs
# choque con prensados ----
choque_cp_accidente <- test_17_split[[9]] # 3,286 obs
choque_cp_sismo <- test_17_split[[10]] # 2 observaciones 
# choques sin lesionados ----
choque_sl_accidente <- test_17_split[[11]] # -------------------- 955,391 Done
choque_sl_sismo <- test_17_split[[12]] # 540 obs
# ciclista ----
ciclista_accidente <- test_17_split[[13]] # 8,455 obs
# ferroviario ----
ferroviario_accidente <- test_17_split[[14]] # 187 obs
# Incidentes de transito ----
incidente_transito_micalle <- test_17_split[[15]] # 727 obs
incidente_transito_mitaxi <- test_17_split[[16]] # 68 obs
# Monopatin ----
monopatin_accidente <- test_17_split[[17]] # 77 obs
# Motociclista ----
moto_accidente <- test_17_split[[18]] # --------------------------- 96,541 Done
# Otros ----
otros_accidente <- test_17_split[[19]] # 2,713 obs
# Persona atrapada_desbarrancada ----
atrapada_accidente <- test_17_split[[20]] # 7,789 obs
# vehiculo atrapado ----
vehiculo_atrapado <- test_17_split[[22]] # 1,121 obs
vehiculo_atrapado_varado_accidente <- test_17_split[[23]] # 2,676 obs
vehiculo_desbarrancado_accidene <- test_17_split[[24]] # 776 obs
volcadura_accidente <- test_17_split[[25]]

## Generar base con colonias cdmx y fechas completas -----

dates_complete <- seq(as.Date("2013-12-01"),as.Date("2023-01-01"),by = "month") %>% rep(2243) # each county has full dates, 2243 colonias
colonias_key <- colonias_cdmx$CVE_COL %>% rep(110) %>% sort() # Must be same length as the sequence of dates

dummy_df <- tibble(dates_complete,CVE_COL = colonias_key)
# dummy_df %>% count(dates_complete) %>% View()# testing

complete_df <- dummy_df %>% left_join(colonias_cdmx,by = "CVE_COL")

# Now split by dates ----

choque_sl_accidente <- choque_sl_accidente %>% group_split(ano_mes)
choque_cl_accidente <- choque_cl_accidente %>% group_split(ano_mes)
atropellado_lesionado <- atropellado_lesionado %>% group_split(ano_mes)
moto_accidente <- moto_accidente %>% group_split(ano_mes)
# Añadimos total
total_incidentes <- inviales_18_22 %>% group_by(ano_mes) %>% group_split()

# Old code chunk to do so
test <- data.frame(id = 1:length(colonias_cdmx$geometry))# La función necesita saber cuántas tiene
colonias_cdmx <- st_transform(colonias_cdmx,st_crs(accidente_auto_cadaver$geometry))
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


# Creamos bases individuales -----

# Intersectamos 
choque_sl_accidente_final<- iterated_intersection(choque_sl_accidente,test,colonias_cdmx)
choque_cl_accidente_final <- iterated_intersection(choque_cl_accidente,test,colonias_cdmx)
atropellado_lesionado_final<- iterated_intersection(atropellado_lesionado,test,colonias_cdmx)
moto_accidente_final <-  iterated_intersection(moto_accidente,test,colonias_cdmx)
total_intersected<- iterated_intersection(total_incidentes,test,colonias_cdmx)

# pegamos nombres
choque_sl_accidente_final <- cbind(CVE_COL = colonias_cdmx$CVE_COL,choque_sl_accidente_final) %>% select(!id)
choque_cl_accidente_final <- cbind(CVE_COL = colonias_cdmx$CVE_COL,choque_cl_accidente_final) %>% select(!id)
atropellado_lesionado_final <- cbind(CVE_COL = colonias_cdmx$CVE_COL,atropellado_lesionado_final) %>% select(!id)
moto_accidente_final <- cbind(CVE_COL = colonias_cdmx$CVE_COL,moto_accidente_final) %>% select(!id)
total_intersected <- cbind(CVE_COL = colonias_cdmx$CVE_COL,total_intersected) %>% select(!id)

# pivoteamos
choque_sl_accidente_final <- choque_sl_accidente_final %>% pivot_longer(!CVE_COL,names_to = "incidente_fecha",values_to = "incidentes") 
choque_cl_accidente_final <- choque_cl_accidente_final %>% pivot_longer(!CVE_COL,names_to = "incidente_fecha",values_to = "incidentes") 
atropellado_lesionado_final <- atropellado_lesionado_final %>% pivot_longer(!CVE_COL,names_to = "incidente_fecha",values_to = "incidentes") 
moto_accidente_final <- moto_accidente_final %>% pivot_longer(!CVE_COL,names_to = "incidente_fecha",values_to = "incidentes") 
total_intersected <- total_intersected %>% pivot_longer(!CVE_COL,names_to = "incidente_fecha",values_to = "incidentes") 

# borramos list
str_remove(choque_sl_accidente_final$incidente_fecha, "list\\(c\\(.*\\s")#Remove everything starting with list(c( and until a whitespace (\s)
choque_sl_accidente_final$incidente_fecha <- str_remove(choque_sl_accidente_final$incidente_fecha, "list\\(c\\(.*\\s")
choque_cl_accidente_final$incidente_fecha <- str_remove(choque_cl_accidente_final$incidente_fecha, "list\\(c\\(.*\\s")
atropellado_lesionado_final$incidente_fecha <- str_remove(atropellado_lesionado_final$incidente_fecha, "list\\(c\\(.*\\s")
moto_accidente_final$incidente_fecha <- str_remove(moto_accidente_final$incidente_fecha, "list\\(c\\(.*\\s")
# No sale en total intersected, habrá sido bug de una vez?

# separamos las fechas en otra columna
choque_sl_accidente_final <- choque_sl_accidente_final %>% separate(incidente_fecha,into = c("incidente", "ano_mes"), sep = "(?<=Accidente )")# Look around for separator
choque_cl_accidente_final <- choque_cl_accidente_final %>% separate(incidente_fecha,into = c("incidente", "ano_mes"), sep = "(?<=Accidente )")
atropellado_lesionado_final <- atropellado_lesionado_final %>% separate(incidente_fecha,into = c("incidente", "ano_mes"), sep = "(?<=Lesionado )")
moto_accidente_final <- moto_accidente_final %>% separate(incidente_fecha,into = c("incidente", "ano_mes"), sep = "(?<=Accidente )")
# New sep
total_intersected <- total_intersected %>% separate(incidente_fecha,into = c("incidente", "ano_mes"), sep = "(?<=[a-zA-Z])\\s*(?=[0-9])")
# mutamos para agregar jic
total_intersected <- total_intersected %>% mutate(incidente = "Total") %>% group_by(CVE_COL,incidente,ano_mes) %>% 
  summarise(incidentes = sum(incidentes))


# Quitamos unused whitespace
choque_sl_accidente_final$incidente <- choque_sl_accidente_final$incidente %>% str_squish()
choque_cl_accidente_final$incidente <- choque_cl_accidente_final$incidente %>% str_squish()
atropellado_lesionado_final$incidente <- atropellado_lesionado_final$incidente %>% str_squish()
moto_accidente_final$incidente <- moto_accidente_final$incidente %>% str_squish()

# Hacemos fechas fechas y ordenamos
choque_sl_accidente_final <- choque_sl_accidente_final %>% mutate(dates_complete = as.Date(paste0(ano_mes,"-01"))) %>% 
  select(dates_complete,CVE_COL,incidente,incidencia = incidentes) 
choque_cl_accidente_final <- choque_cl_accidente_final %>% mutate(dates_complete = as.Date(paste0(ano_mes,"-01"))) %>% 
  select(dates_complete,CVE_COL,incidente,incidencia = incidentes) 
atropellado_lesionado_final <- atropellado_lesionado_final %>% mutate(dates_complete = as.Date(paste0(ano_mes,"-01"))) %>% 
  select(dates_complete,CVE_COL,incidente,incidencia = incidentes) 
moto_accidente_final <- moto_accidente_final %>% mutate(dates_complete = as.Date(paste0(ano_mes,"-01"))) %>% 
  select(dates_complete,CVE_COL,incidente,incidencia = incidentes) 
total_intersected <- total_intersected %>% mutate(dates_complete = as.Date(paste0(ano_mes,"-01"))) %>% 
  select(dates_complete,CVE_COL,incidente,incidencia = incidentes) 


## Pegamos a base original -----

complete_choque_sl_accidente <- complete_df %>% left_join(choque_sl_accidente_final, by = c("CVE_COL","dates_complete"))
complete_choque_cl_accidente <- complete_df %>% left_join(choque_cl_accidente_final, by = c("CVE_COL","dates_complete"))
complete_atropellado_lesionado <- complete_df %>% left_join(atropellado_lesionado_final, by = c("CVE_COL","dates_complete"))
complete_moto_accidente <- complete_df %>% left_join(moto_accidente_final, by = c("CVE_COL","dates_complete"))
complete_total<- complete_df %>% left_join(total_intersected, by = c("CVE_COL","dates_complete"))

# Mismo número de incidencias
complete_choque_sl_accidente$incidencia %>% sum()
choque_sl_accidente_final$incidencia %>% sum()

complete_choque_cl_accidente$incidencia %>% sum()
choque_cl_accidente_final$incidencia %>% sum()

complete_atropellado_lesionado$incidencia %>% sum()
atropellado_lesionado_final$incidencia %>% sum()

complete_moto_accidente$incidencia %>% sum(na.rm = T)
moto_accidente_final$incidencia %>% sum()

total_intersected$incidencia %>% sum(na.rm = T)
complete_total$incidencia %>% sum()

# Inicio de proyectos ----
# Creamos columnas para los respectivos dummies de inicios y ordenamos base
colonias_cdmx %>% glimpse()

complete_choque_sl_accidente <- complete_choque_sl_accidente %>% 
  mutate(inicio_cb1 = ifelse(dates_complete > as.Date("2021-07-11"),1,0)) %>%
  mutate(inicio_cb2 = ifelse(dates_complete > as.Date("2021-08-08"),1,0)) %>% 
  mutate(inicio_te = ifelse(dates_complete > as.Date("2022-10-29"),1,0)) %>% 
  mutate(inicio_l9 = ifelse(dates_complete > as.Date("2021-01-30"),1,0)) %>% 
  mutate(para_l12 = ifelse(dates_complete > as.Date("2021-05-03"),1,0)) %>% 
  mutate(para_l1 = ifelse(dates_complete > as.Date("2022-07-11"),1,0)) %>% 
  select(dates_complete,CVE_COL,COLONIA,NOM_LOC,POBTOT,IM_2020, IMN_2020,
         incidente,incidencia,cb_1,cb_2,tr_e,tr_9,l_1,l_12,inicio_cb1,inicio_cb2,inicio_te,
         inicio_l9,para_l1,para_l12,geometry)

complete_choque_cl_accidente <- complete_choque_cl_accidente %>% 
  mutate(inicio_cb1 = ifelse(dates_complete > as.Date("2021-07-11"),1,0)) %>%
  mutate(inicio_cb2 = ifelse(dates_complete > as.Date("2021-08-08"),1,0)) %>% 
  mutate(inicio_te = ifelse(dates_complete > as.Date("2022-10-29"),1,0)) %>% 
  mutate(inicio_l9 = ifelse(dates_complete > as.Date("2021-01-30"),1,0)) %>% 
  mutate(para_l12 = ifelse(dates_complete > as.Date("2021-05-03"),1,0)) %>% 
  mutate(para_l1 = ifelse(dates_complete > as.Date("2022-07-11"),1,0)) %>% 
  select(dates_complete,CVE_COL,COLONIA,NOM_LOC,POBTOT,IM_2020, IMN_2020,
         incidente,incidencia,cb_1,cb_2,tr_e,tr_9,l_1,l_12,inicio_cb1,inicio_cb2,inicio_te,
         inicio_l9,para_l1,para_l12,geometry)

complete_atropellado_lesionado <- complete_atropellado_lesionado %>% 
  mutate(inicio_cb1 = ifelse(dates_complete > as.Date("2021-07-11"),1,0)) %>%
  mutate(inicio_cb2 = ifelse(dates_complete > as.Date("2021-08-08"),1,0)) %>% 
  mutate(inicio_te = ifelse(dates_complete > as.Date("2022-10-29"),1,0)) %>% 
  mutate(inicio_l9 = ifelse(dates_complete > as.Date("2021-01-30"),1,0)) %>% 
  mutate(para_l12 = ifelse(dates_complete > as.Date("2021-05-03"),1,0)) %>% 
  mutate(para_l1 = ifelse(dates_complete > as.Date("2022-07-11"),1,0)) %>% 
  select(dates_complete,CVE_COL,COLONIA,NOM_LOC,POBTOT,IM_2020, IMN_2020,
         incidente,incidencia,cb_1,cb_2,tr_e,tr_9,l_1,l_12,inicio_cb1,inicio_cb2,inicio_te,
         inicio_l9,para_l1,para_l12,geometry)

complete_moto_accidente <- complete_moto_accidente %>% 
  mutate(inicio_cb1 = ifelse(dates_complete > as.Date("2021-07-11"),1,0)) %>%
  mutate(inicio_cb2 = ifelse(dates_complete > as.Date("2021-08-08"),1,0)) %>% 
  mutate(inicio_te = ifelse(dates_complete > as.Date("2022-10-29"),1,0)) %>% 
  mutate(inicio_l9 = ifelse(dates_complete > as.Date("2021-01-30"),1,0)) %>% 
  mutate(para_l12 = ifelse(dates_complete > as.Date("2021-05-03"),1,0)) %>% 
  mutate(para_l1 = ifelse(dates_complete > as.Date("2022-07-11"),1,0)) %>% 
  select(dates_complete,CVE_COL,COLONIA,NOM_LOC,POBTOT,IM_2020, IMN_2020,
         incidente,incidencia,cb_1,cb_2,tr_e,tr_9,l_1,l_12,inicio_cb1,inicio_cb2,inicio_te,
         inicio_l9,para_l1,para_l12,geometry)

complete_total <- complete_total %>% 
  mutate(inicio_cb1 = ifelse(dates_complete > as.Date("2021-07-11"),1,0)) %>%
  mutate(inicio_cb2 = ifelse(dates_complete > as.Date("2021-08-08"),1,0)) %>% 
  mutate(inicio_te = ifelse(dates_complete > as.Date("2022-10-29"),1,0)) %>% 
  mutate(inicio_l9 = ifelse(dates_complete > as.Date("2021-01-30"),1,0)) %>% 
  mutate(para_l12 = ifelse(dates_complete > as.Date("2021-05-03"),1,0)) %>% 
  mutate(para_l1 = ifelse(dates_complete > as.Date("2022-07-11"),1,0)) %>% 
  select(dates_complete,CVE_COL,COLONIA,NOM_LOC,POBTOT,IM_2020, IMN_2020,
         incidente,incidencia,cb_1,cb_2,tr_e,tr_9,l_1,l_12,inicio_cb1,inicio_cb2,inicio_te,
         inicio_l9,para_l1,para_l12,geometry)


# IM e IMN son LD, entonces sólo usar una, la que sea

# LISTOOOO!!!!
# complete_choque_sl_accidente %>% View()

# 

# Fechas ----
fechas_importantes <- data.frame(evento = c("Inicia Cuarentena por Covid-19", "Inicia Cablebus L1", 
                                            "Inicia Cablebus L2", "Tragedia L12", "Inicia Trolebus Elevado", "Reabre L9 Trolebus", 
                                            "Reparaciones L1", "Reapertura Mitad L12"),
                                 fecha = c(as.Date("2020-04-01"),as.Date("2021-07-11"),as.Date("2021-08-08"), 
                                           as.Date("2021-05-03"), as.Date("2022-10-29"), as.Date("2021-01-30"),as.Date("2022-07-11"), 
                                           as.Date("2023-01-15")), 
                                 incidentes = c(15000,11000,14000,18000,15000,15000,10000,10000))









