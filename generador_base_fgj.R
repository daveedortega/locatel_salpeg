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
# Quitamos lo que no ocupamos
glimpse(carpetas_completa)
# Mapa de Colonias
colonias_cdmx <- read_sf("input/mapas/colonias_iecm_2019/mgpc_2019.shp")
colonias_cdmx <- colonias_cdmx %>% st_make_valid() # Hacemos válidos los polígonos inválidos
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


## Selección de Delitos y Variables ----
carpetas_completa %>% glimpse()
puntos_carpetas <- carpetas_completa %>% select(fecha_inicio, categoria_delito,delito, longitud,latitud) %>% 
  na.omit() %>% # No acepta cosas sin valores, los quitamos
  st_as_sf(coords = c("longitud", "latitud"),crs = 4326)
# Hacemos mismo crs colonias y lo otro
colonias_cdmx <- st_transform(colonias_cdmx,st_crs(puntos_carpetas))

## Selección de Delitos ----
puntos_carpetas %>% count(delito) %>% View()

# Por Categorías ----
puntos_carpetas %>% count(categoria_delito)
#Todo parece interesante, sólo quitamows hechos no delictivos, 61,929 observaciones
puntos_carpetas %>%  filter(categoria_delito != "HECHO NO DELICTIVO") %>% count(delito) %>% filter(!delito %in% todos) %>% View()
# 322 categorías, tenemos que quitar un montón 
# total
robos <- c("ROBO DE OBJETOS", 
           "ROBO A TRANSEUNTE EN VIA PUBLICA CON VIOLENCIA", 
           "ROBO A NEGOCIO SIN VIOLENCIA", 
           "ROBO DE ACCESORIOS DE AUTO", 
           "ROBO DE OBJETOS DEL INTERIOR DE UN VEHICULO", 
           "ROBO DE VEHICULO DE SERVICIO PARTICULAR SIN VIOLENCIA", 
           "ROBO A CASA HABITACION SIN VIOLENCIA", 
           "ROBO A NEGOCIO CON VIOLENCIA", 
           "ROBO A PASAJERO / CONDUCTOR DE VEHICULO CON VIOLENCIA", 
           "ROBO DE VEHICULO DE SERVICIO PARTICULAR CON VIOLENCIA", 
           "ROBO A NEGOCIO SIN VIOLENCIA POR FARDEROS (TIENDAS DE AUTOSERVICIO)", 
           "ROBO A TRANSEUNTE DE CELULAR CON VIOLENCIA", 
           "ROBO A TRANSEUNTE DE CELULAR SIN VIOLENCIA", 
           "ROBO A PASAJERO A BORDO DE METRO SIN VIOLENCIA", 
           "ROBO A TRANSEUNTE EN VIA PUBLICA SIN VIOLENCIA", 
           "ROBO A REPARTIDOR CON VIOLENCIA", 
           "ROBO DE VEHICULO DE PEDALES", 
           "ROBO DE DINERO", 
           "ROBO DE MOTOCICLETA SIN VIOLENCIA", 
           "ROBO A PASAJERO A BORDO DE TRANSPORTE PÚBLICO CON VIOLENCIA",
           "ROBO DE DOCUMENTOS", 
           "ROBO DE PLACA DE AUTOMOVIL", 
           "ROBO DE MOTOCICLETA CON VIOLENCIA", 
           "ROBO A PASAJERO A BORDO DE PESERO COLECTIVO CON VIOLENCIA", 
           "ROBO DE VEHICULO DE SERVICIO PÚBLICO CON VIOLENCIA", 
           "ROBO DE VEHICULO DE SERVICIO PÚBLICO SIN VIOLENCIA", 
           "ROBO A PASAJERO A BORDO DE METROBUS SIN VIOLENCIA", 
           "ROBO A TRANSEUNTE EN PARQUES Y MERCADOS CON VIOLENCIA", 
           "ROBO A CASA HABITACION CON VIOLENCIA", 
           "ROBO A REPARTIDOR SIN VIOLENCIA", 
           "ROBO A TRANSEUNTE EN NEGOCIO CON VIOLENCIA", 
           "ROBO S/V DENTRO DE NEGOCIOS, AUTOSERVICIOS, CONVENIENCIA", 
           "ROBO DE OBJETOS A ESCUELA", 
           "ROBO A PASAJERO A BORDO DE TRANSPORTE PÚBLICO SIN VIOLENCIA",
           "ROBO A NEGOCIO SIN VIOLENCIA POR FARDEROS", 
           "ROBO A TRANSEUNTE SALIENDO DEL BANCO CON VIOLENCIA", 
           "TENTATIVA DE ROBO", 
           "ROBO A NEGOCIO SIN VIOLENCIA POR FARDEROS (TIENDAS DE CONVENIENCIA)", 
           "ROBO A NEGOCIO CON VIOLENCIA POR FARDEROS (TIENDAS DE CONVENIENCIA)", 
           "ROBO A TRANSEUNTE CONDUCTOR DE TAXI PUBLICO Y PRIVADO CON VIOLENCIA", 
           "ROBO A PASAJERO A BORDO DE METRO CON VIOLENCIA", 
           "ROBO A TRANSEUNTE A BORDO DE TAXI PUBLICO Y PRIVADO SIN VIOLENCIA", 
           "ROBO A TRANSEUNTE A BORDO DE TAXI PÚBLICO Y PRIVADO CON VIOLENCIA", 
           "ROBO A OFICINA PÚBLICA SIN VIOLENCIA", 
           "POSESION DE VEHICULO ROBADO", 
           "ROBO A PASAJERO A BORDO DE PESERO COLECTIVO SIN VIOLENCIA", 
           "ROBO DE ANIMALES", 
           "ROBO A REPARTIDOR Y VEHICULO CON VIOLENCIA", 
           "ROBO DE ARMA",
           "ROBO A PASAJERO / CONDUCTOR DE TAXI CON VIOLENCIA", 
           "ROBO EN EVENTOS MASIVOS (DEPORTIVOS, CULTURALES, RELIGIOSOS Y ARTISTICOS) S/V", 
           "ROBO A TRANSPORTISTA Y VEHICULO PESADO CON VIOLENCIA", 
           "ROBO A TRANSEUNTE EN RESTAURANT CON VIOLENCIA", 
           "ROBO A PASAJERO EN TROLEBUS SIN VIOLENCIA", 
           "ROBO A PASAJERO A BORDO DE TAXI CON VIOLENCIA", 
           "ROBO A TRANSEUNTE SALIENDO DEL CAJERO CON VIOLENCIA", 
           "ROBO DE FLUIDOS", 
           "TENTATIVA DE ROBO DE VEHICULO", 
           "ROBO DE ALHAJAS", 
           "ROBO A CASA HABITACION Y VEHICULO SIN VIOLENCIA", 
           "ROBO A PASAJERO A BORDO DE TAXI SIN VIOLENCIA", 
           "ROBO DE VEHICULO DE SERVICIO DE TRANSPORTE SIN VIOLENCIA", 
           "ROBO A CASA HABITACION Y VEHICULO CON VIOLENCIA", 
           "ROBO A PASAJERO A BORDO DE METROBUS CON VIOLENCIA",
           "ROBO A PASAJERO EN TREN LIGERO SIN VIOLENCIA",
           "ROBO A LOCALES SEMIFIJOS (PUESTOS DE ALIMENTOS,BEBIDAS, ENSERES, PERIODICOS,LOTERIA, OTROS)", 
           "ROBO A PASAJERO EN RTP CON VIOLENCIA", 
           "ROBO DE VEHICULO DE SERVICIO OFICIAL SIN VIOLENCIA", 
           "ROBO A NEGOCIO CON VIOLENCIA POR FARDEROS (TIENDAS DE AUTOSERVICIO)", 
           "ROBO A NEGOCIO Y VEHICULO CON VIOLENCIA", 
           "DAÑO EN PROPIEDAD AJENA CULPOSA POR TRÁNSITO VEHICULAR A VIAS DE COMUNICACION", 
           "ROBO DE VEHICULO DE SERVICIO DE TRANSPORTE CON VIOLENCIA", 
           "ROBO A NEGOCIO Y VEHICULO SIN VIOLENCIA", 
           "ROBO A REPARTIDOR Y VEHICULO SIN VIOLENCIA", 
           "ROBO DE MAQUINARIA CON VIOLENCIA", 
           "ROBO A PASAJERO EN TROLEBUS CON VIOLENCIA",
           "ROBO A PASAJERO EN RTP SIN VIOLENCIA", 
           "ROBO A TRANSPORTISTA Y VEHICULO PESADO SIN VIOLENCIA", 
           "ROBO A PASAJERO EN AUTOBÚS FORÁNEO CON VIOLENCIA", 
           "ROBO A TRANSEUNTE EN HOTEL CON VIOLENCIA", 
           "ROBO A PASAJERO EN TREN SUBURBANO SIN VIOLENCIA", 
           "ROBO DE CONTENEDORES DE TRAILERS S/V", 
           "ROBO A OFICINA PÚBLICA CON VIOLENCIA", 
           "ROBO A SUCURSAL BANCARIA CON VIOLENCIA", 
           "ROBO DE MERCANCIA EN CONTENEDEROS EN ÁREAS FEDERALES", 
           "ROBO DE VEHICULO EN PENSION, TALLER Y AGENCIAS S/V", 
           "ROBO EN INTERIOR DE EMPRESA (NOMINA) SIN VIOLENCIA", 
           "ROBO DE VEHICULO DE SERVICIO OFICIAL CON VIOLENCIA", 
           "ROBO DE MAQUINARIA SIN VIOLENCIA", 
           "ROBO A SUCURSAL BANCARIA (SUPERMERCADO) SIN VIOLENCIA", 
           "ROBO A PASAJERO EN ECOBUS CON VIOLENCIA", 
           "ROBO A SUCURSAL BANCARIA (ASALTO BANCARIO) CON VIOLENCIA", 
           "ROBO A TRANSEUNTE EN VIA PUBLICA (NOMINA) CON VIOLENCIA", 
           "ROBO DE VEHICULO EN PENSION, TALLER Y AGENCIAS C/V", 
           "ROBO DE MERCANCIA A TRANSPORTISTA C/V", 
           "ROBO A PASAJERO EN AUTOBUS FORANEO SIN VIOLENCIA", 
           "ROBO A PASAJERO EN ECOBUS SIN VIOLENCIA", 
           "ROBO A SUCURSAL BANCARIA SIN VIOLENCIA", 
           "ROBO A TRANSEUNTE EN TERMINAL DE PASAJEROS CON VIOLENCIA", 
           "ROBO A TRANSEUNTE Y VEHICULO CON VIOLENCIA", 
           "ROBO DE VEHICULO ELECTRICO MOTOPATIN", 
           "ROBO EN INTERIOR DE EMPRESA (NOMINA) CON VIOLENCIA", 
           "ROBO A PASAJERO EN TREN LIGERO CON VIOLENCIA", 
           "ROBO A TRANSEUNTE EN CINE CON VIOLENCIA", 
           "ROBO DURANTE TRASLADO DE VALORES (NOMINA) CON VIOLENCIA", 
           "ROBO A PASAJERO A BORDO DE PESERO Y VEHICULO CON VIOLENCIA", 
           "ROBO A SUCURSAL BANCARIA DENTRO DE  TIENDAS DE AUTOSERVICIO CON VIOLENCIA", 
           "ROBO A SUCURSAL BANCARIA (SUPERMERCADO) CON VIOLENCIA", 
           "ROBO DURANTE TRASLADO DE VALORES (NOMINA) SIN VIOLENCIA", 
           "ROBO A PASAJERO EN TREN SUBURBANO CON VIOLENCIA", 
           "ROBO A SUCURSAL BANCARIA DENTRO DE  TIENDAS DE AUTOSERVICIO S/V", 
           "ROBO A PASAJERO A BORDO DE CABLEBUS CON VIOLENCIA", 
           "ROBO A PASAJERO A BORDO DE CABLEBUS SIN VIOLENCIA", 
           "ROBO A TRANSEUNTE EN VIA PUBLICA (NOMINA) SIN VIOLENCIA",
           "ROBO DE VEHICULO Y NOMINA CON VIOLENCIA")

delitos_sexuales <- c("VIOLACION EQUIPARADA",
               "ABUSO SEXUAL", 
               "VIOLACION",
               "ACOSO SEXUAL", 
               "VIOLACION DE CORRESPONDENCIA", 
               "VIOLACION TUMULTUARIA", 
               "ESTUPRO", 
               "VIOLACION EQUIPARADA POR CONOCIDO", 
               "VIOLACION TUMULTUARIA EQUIPARADA", 
               "TENTATIVA DE VIOLACION", 
               "VIOLACION TUMULTUARIA EQUIPARADA POR CONOCIDO", 
               "ACOSO SEXUAL AGRAVADO EN CONTRA DE MENORES",
               "VIOLACION EQUIPARADA Y ROBO DE VEHICULO", 
               "VIOLACION Y ROBO DE VEHICULO")

plagios <- c("SUSTRACCION DE MENORES",
               "PRIVACION DE LA LIBERTAD PERSONAL", 
               "SUSTRACCIÓN DE MENORES", 
               "SECUESTRO EXPRESS (PARA COMETER ROBO O EXTORSIÓN)", 
               "TRATA DE PERSONAS",
               "RETENCIÓN O SUSTRACCIÓN DE MENORES INCAPACES", 
               "PLAGIO O SECUESTRO", 
               "PRIV. ILEGAL DE LA LIB. Y ROBO DE VEHICULO", 
               "ROBO DE INFANTE", 
               "TRAFICO DE INFANTES",
               "SECUESTRO")

lesiones <- c("LESIONES INTENCIONALES POR GOLPES", 
               "LESIONES CULPOSAS POR TRANSITO VEHICULAR EN COLISION", 
               "LESIONES INTENCIONALES POR ARMA DE FUEGO", 
               "LESIONES INTENCIONALES POR ARMA BLANCA",
               "LESIONES CULPOSAS POR TRANSITO VEHICULAR", 
               "LESIONES CULPOSAS", 
               "LESIONES INTENCIONALES",
               "LESIONES CULPOSAS POR CAIDA", 
               "LESIONES CULPOSAS POR CAIDA DE VEHÍCULO EN MOVIMIENTO", 
               "LESIONES CULPOSAS POR QUEMADURAS",
               "LESIONES DOLOSAS POR QUEMADURAS", 
               "LESIONES CULPOSAS ACCIDENTE LABORAL", 
               "LESIONES CULPOSAS CON EXCLUYENTES DE RESPONSABILIDAD", 
               "LESIONES INTENCIONALES Y ROBO DE VEHICULO")

asesinatos <- c("TENTATIVA DE HOMICIDIO", 
               "HOMICIDIO POR ARMA DE FUEGO", 
               "HOMICIDIO CULPOSO POR TRÁNSITO VEHICULAR (ATROPELLADO)", 
               "HOMICIDIO CULPOSO POR TRÁNSITO VEHICULAR (COLISION)", 
               "HOMICIDIO POR ARMA BLANCA", 
               "HOMICIDIO POR GOLPES", 
               "HOMICIDIOS INTENCIONALES (OTROS)", 
               "HOMICIDIO CULPOSO POR TRÁNSITO VEHICULAR", 
               "HOMICIDIO CULPOSO POR TRÁNSITO VEHICULAR (CAIDA)", 
               "HOMICIDIO CULPOSO",
               "HOMICIDIO CULPOSO POR ARMA DE FUEGO",
               "HOMICIDIO CULPOSO CON EXCLUYENTES DE RESPONSABILIDAD", 
               "HOMICIDIO POR AHORCAMIENTO", 
               "HOMICIDIO CULPOSO POR INSTRUMENTO PUNZO CORTANTE", 
               "HOMICIDIO DOLOSO", 
               "HOMICIDIO INTENCIONAL Y ROBO DE VEHICULO", 
               "HOMICIDIO POR INMERSION",
               "TENTATIVA DE FEMINICIDIO",
               "FEMINICIDIO", 
               "FEMINICIDIO POR ARMA BLANCA", 
               "FEMINICIDIO POR DISPARO DE ARMA DE FUEGO", 
               "FEMINICIDIO POR GOLPES",
               "DESAPARICION FORZADA DE PERSONAS")

danio <- c("DAÑO EN PROPIEDAD AJENA CULPOSA POR TRÁNSITO VEHICULAR A AUTOMOVIL", 
           "DAÑO EN PROPIEDAD AJENA INTENCIONAL", 
           "DAÑO EN PROPIEDAD AJENA CULPOSA", 
           "DAÑO EN PROPIEDAD AJENA INTENCIONAL A AUTOMOVIL",
           "DAÑO EN PROPIEDAD AJENA INTENCIONAL A BIENES INMUEBLES", 
           "DAÑO EN PROPIEDAD AJENA INTENCIONAL A CASA HABITACION", 
           "DAÑO EN PROPIEDAD AJENA CULPOSA POR TRÁNSITO VEHICULAR A BIENES INMUEBLES",
           "DAÑO EN PROPIEDAD AJENA INTENCIONAL A NEGOCIO", 
           "DAÑO EN PROPIEDAD AJENA INTENCIONAL A VIAS DE COMUNICACION")

otros <- c("VIOLENCIA FAMILIAR",
           "NARCOMENUDEO POSESION SIMPLE", 
           "TORTURA",
           "PERSONAS EXTRAVIADAS Y AUSENTES", 
           "ABANDONO DE PERSONA",
           "EXTORSION", 
           "TENTATIVA DE EXTORSION", 
           "NARCOMENUDEO POSESIÓN CON FINES DE VENTA, COMERCIO Y SUMINISTRO", 
           "ALLANAMIENTO DE MORADA, DESPACHO, OFICINA O ESTABLECIMIENTO MERCANTIL", 
           "ALLANAMIENTO DE MORADA", 
           "ATAQUE A LAS VIAS GENERALES DE COMUNICACIÓN", 
           "PORTACION DE ARMA DE FUEGO", 
           "PORTACION ARMA/PROHIB.", 
           "DISPAROS DE ARMA DE FUEGO", 
           "PORTACIÓN, FABRICACIÓN E IMPORTACIÓN DE OBJETOS APTOS PARA AGREDIR", 
           "ATAQUE A LAS VIAS DE COMUNICACION (DAÑO A VIAS O MEDIOS DE TRANSPORTE)", 
           "TENTATIVA DE SUICIDIO",
           "PANDILLA, ASOCIACIÓN DELICTUOSA Y DELINCUENCIA ORGANIZADA", 
           "SABOTAJE")
todos <- c(asesinatos,delitos_sexuales,lesiones,otros,plagios, danio,robos)

# Empezamos con robos ----
# 642,126 observaciones desde 2016
# Primero por fecha y luego por delito, genera un montón de dfs
fecha_robos <- puntos_carpetas %>% filter(delito %in% robos) %>%  
  mutate(ano_mes = format(fecha_inicio,format = "%Y-%m")) %>% group_split(ano_mes,delito)

# Funcion que itera ----
iterated_intersection <- function(x,y,z){
  for (i in 1:length(x)){
    df <- x[[i]] # ith data frame
    fetch_date <- df[1,5] #get date, 5th column
    fetch_what <- paste(df[1,2],df[1,3]) # categoria - delito
    isects <- lengths(st_intersects(z,df$geometry)) # vector of intersections
    y[,ncol(y)+1] <- isects # después lo puedo colapsar como columna de atributos con pivot wider
    colnames(y)[ncol(y)] <- paste(fetch_what,fetch_date) # rename w date
    print(paste(i, "intersected data frames out of:", length(x)))
  }
  return(y)
}
test <- data.frame(id = 1:length(colonias_cdmx$geometry))# La función necesita saber cuántas tiene

# Aplicamos función a todos -----
tok <- Sys.time()
robos_fechas_intersecciones<- iterated_intersection(fecha_robos,test,colonias_cdmx) # masa enorme de columnas
tik <- Sys.time()
print(tik-tok)

## Partimos puntos por meses ----

a_0 <- cbind(colonia = colonias_cdmx$CVEUT,robos_fechas_intersecciones) #pegamos clave unica
a_0 <- a_0 %>% select(!id)
a_0 <- a_0 %>% pivot_longer(!colonia,names_to = "incidente_fecha",values_to = "incidentes") # pivoteamos y luego separamos
# Separamos fechas
carpetas_wf <- a_0 %>% separate(incidente_fecha,into = c("accidente","ano_mes"), sep = "(?<=[a-zA-Z])\\s*(?=[0-9])")
# 10 millones de observaciones 
# Falta rellenar las fehcas - delito faltante
# son 115 robos x 1815 colonias x 82 meses más o menos deberían ser 17,115,450
carpetas_wf %>% count(ano_mes) 





