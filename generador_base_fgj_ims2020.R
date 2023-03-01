####################################################.
## Código Final para generar bdd de Carpetas de Investigación
## Ahora con IMS CONAPO 2020
## DAOA - 01/03/23
####################################################.

## Preparar Espacio ----
pacman::p_load(tidyverse,sf,janitor,scales)
rm(list=ls())
dev.off()

## Cargar Bases ----
# CARPETAS
carpetas_18 <- read_csv("input/carpetas_fgj/carpetas_2016-2018.csv")
carpetas_21 <- read_csv("input/carpetas_fgj/carpetas_2019-2021.csv")
carpetas_22 <- read_csv("input/carpetas_fgj/carpetas_2022_2023.csv") # A diciembre 2022
# Eliminamos columnas adicionales
carpetas_22 %>% glimpse()
colnames(carpetas_22)[!colnames(carpetas_22) %in% colnames(carpetas_21)]
carpetas_22 <- carpetas_22 %>% select(!c(temporal_fecha_hechos,temporal_fecha_inicio,..anio_fecha_hechos,..anio_fecha_inicio))
# Pegamos
carpetas_completa <-rbind(carpetas_18,carpetas_21,carpetas_22)
rm(carpetas_18,carpetas_21,carpetas_22)
# Quitamos lo que no ocupamos
glimpse(carpetas_completa)
# Mapa de Colonias
colonias_cdmx <- read_sf("~/Desktop/Mapas/colonias_indicemarginacionurbana_2020/cdmx_imu_2020/cdmx_imu2020.shp")
colonias_cdmx <- colonias_cdmx %>% st_make_valid() # Hacemos válidos los polígonos inválidos
# Mapas Adicionales
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
# Homologamos a 0,1, para presencia o no del tratamiento

colonias_cdmx <- colonias_cdmx %>% cbind(cb_1,cb_2,tr_9,tr_e,l_1,l_12) %>% mutate(cb_1 = ifelse(cb_1>0,1,0)) %>% 
  mutate(cb_2 = ifelse(cb_2>0,1,0)) %>% mutate(tr_e = ifelse(tr_e>0,1,0)) %>% 
  mutate(l_12 = ifelse(l_12>0,1,0)) %>% mutate(l_1 = ifelse(l_1>0,1,0))%>% mutate(tr_9 = ifelse(tr_9>0,1,0))

rm(l_1,l_12,cb_1,cb_2,tr_e,tr_9) # Eliminamos lo que no necesitamos


# Estadísticas Generales ----
# Línea de tiempo
carpetas_completa %>% mutate(fecha = as.Date(paste0(format(fecha_inicio,format = "%Y-%m"), "-01"))) %>% count(fecha) %>% 
  ggplot(aes(fecha,n))+
  geom_line(color = "#9f2441", size = 1)+
  geom_smooth()+
  labs(x="", y="Carpetas de Investigación", title = "Carpetas de Investigación iniciadas al mes", 
       subtitle = " entre 01/2016 y 12/2022 reportados por la FGJ", fill ="", caption = "Fuente: Datos Abiertos - GCDMX")+
  theme(plot.title = element_text(size = 28, face = "bold", color = "#23433A"),
        plot.subtitle = element_text(size = 22, face="bold"))

# Tipos de Delito
carpetas_completa %>% count(categoria_delito) %>% ggplot(aes(reorder(categoria_delito, n),n, fill = categoria_delito))+
  geom_col(color = "black", size = 0.1)+
  geom_label(aes(label = comma(n)))+
  labs(x="", y="Carpetas de Investigación", title = "Carpetas de Investigación por Categoría de Delito", 
       subtitle = " entre 01/2016 y 12/2022 reportados por la FGJ", fill ="", caption = "Fuente: Datos Abiertos - GCDMX")+
  theme(plot.title = element_text(size = 28, face = "bold", color = "#9f2441"),
        plot.subtitle = element_text(size = 22, face="bold"), 
        legend.position = "none", 
        axis.text.y = element_text(color = "black", size = 10))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  scale_y_sqrt()

# Vemos qué es Delito de Bajo Impacto

carpetas_completa %>% filter(categoria_delito == "DELITO DE BAJO IMPACTO") %>% count(delito,sort = T) %>% 
  ggplot(aes(reorder(delito,n),n, fill = delito))+
  geom_col()+
  geom_label(aes(label = comma(n)))+
  theme(legend.position = "none")

## Selección de Delitos y Variables ----
carpetas_completa %>% glimpse()
puntos_carpetas <- carpetas_completa %>% select(fecha_inicio, categoria_delito,delito, longitud,latitud) %>% 
  na.omit() %>% # No acepta cosas sin valores, los quitamos
  st_as_sf(coords = c("longitud", "latitud"),crs = 4326)
# Hacemos mismo crs colonias y lo otro
colonias_cdmx <- st_transform(colonias_cdmx,st_crs(puntos_carpetas))

## Selección de Delitos ----
# puntos_carpetas %>% count(delito) %>% View()

# Por Categorías ----
puntos_carpetas %>% count(categoria_delito)
#Todo parece interesante, sólo quitamows hechos no delictivos, 61,929 observaciones
# carpetas_completa %>%  filter(categoria_delito != "HECHO NO DELICTIVO") %>% count(delito) %>% filter(!delito %in% todos) %>% View()
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
                      "VIOLACION Y ROBO DE VEHICULO",
                      "CONTRA LA INTIMIDAD SEXUAL",
                      "CORRUPCION DE MENORES E INCAPACES",
                      "LENOCINIO")

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
             "DESAPARICION FORZADA DE PERSONAS",
             "RETENCIÓN DE MENORES",
             "SECUESTRO",
             "PORNOGRAFIA INFANTIL",
             "PORNOGRAFÍA")

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
                "HOMICIDIO CULPOSO FUERA DEL D.F (ATROPELLADO)",
                "HOMICIDIO CULPOSO FUERA DEL D.F (COLISION)")

danio <- c("DAÑO EN PROPIEDAD AJENA CULPOSA POR TRÁNSITO VEHICULAR A AUTOMOVIL", 
           "DAÑO EN PROPIEDAD AJENA INTENCIONAL", 
           "DAÑO EN PROPIEDAD AJENA CULPOSA", 
           "DAÑO EN PROPIEDAD AJENA INTENCIONAL A AUTOMOVIL",
           "DAÑO EN PROPIEDAD AJENA INTENCIONAL A BIENES INMUEBLES", 
           "DAÑO EN PROPIEDAD AJENA INTENCIONAL A CASA HABITACION", 
           "DAÑO EN PROPIEDAD AJENA CULPOSA POR TRÁNSITO VEHICULAR A BIENES INMUEBLES",
           "DAÑO EN PROPIEDAD AJENA INTENCIONAL A NEGOCIO", 
           "DAÑO EN PROPIEDAD AJENA INTENCIONAL A VIAS DE COMUNICACION",
           "DAÑO SUELO (ACTIVIDAD, INVASIÓN O EXTRACCIÓN)")

de_servidores_publicos <- c("LA ADMINISTRACION DE JUSTICIA",
                            "ABUSO DE AUTORIDAD Y USO ILEGAL DE LA FUERZA PUBLICA",
                            "NEGACION DEL SERVICIO PUBLICO",
                            "ABUSO DE AUTORIDAD",
                            "COHECHO",
                            "EJERCICIO ILEGAL Y ABANDONO DEL SERVICIO PUBLICO",
                            "CONTRA FUNCIONARIOS PUBLICOS",
                            "USO INDEBIDO DE ATRIBUCIONES Y FACULTADES",
                            "EJERCICIO INDEBIDO DEL SERVIDOR PUBLICO",
                            "USURPACION DE FUNCIONES PUBLICAS",
                            "PECULADO",
                            "OPERACIONES CON RECURSOS DE PROCEDENCIA ILICITA",
                            "TRAFICO DE INFLUENCIA",
                            "USURPACION DE FUNCIONES",
                            "ENRIQUECIMIENTO ILICITO",
                            "COALICIÓN DE SERVIDORES PÚBLICOS",
                            "OPERACIONES CON RECURSOS DE PROCEDENCIA ILEGAL",
                            "COACCION DE SERVIDORES PUBLICOS",
                            "EJERCICIO ABUSIVO DE FUNCIONES")

otros <- c("VIOLENCIA FAMILIAR",
           "FRAUDE",
           "AMENAZAS",
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
           "SABOTAJE", 
           "ABUSO DE CONFIANZA")
todos <- c(asesinatos,delitos_sexuales,lesiones,otros,plagios, danio,robos, de_servidores_publicos)

# Vemos robos ----

carpetas_completa %>% filter(delito %in% robos) %>% count(categoria_delito,delito) %>% 
  ggplot(aes(reorder(categoria_delito,n),n,fill = delito))+
  geom_col(color = "black")+
  labs(x="", y="Número de Carpetas", title = "Robos Categorizados en Carpetas FGJ", 
       caption = "Elaboración Propia, Datos Abiertos, Carpetas FGJCDMX", subtitle = "Coso")+
  theme(plot.title = element_text(size = 28, face = "bold", color = "#9f2441"),
        plot.subtitle = element_text(size = 22, face="bold"), 
        legend.position = "none", 
        axis.text.y = element_text(color = "black", size = 10))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  scale_y_sqrt()

# LDT de robos ----
# Se ve MUY bien
carpetas_completa %>% filter(delito %in% robos) %>% 
  mutate(ano_mes = format(fecha_inicio, format = "%Y-%m")) %>% count(ano_mes) %>% 
  mutate(fecha = as.Date(paste0(ano_mes,"-01"))) %>% 
  ggplot(aes(fecha,n))+
  geom_line(color = "#9f2241")+
  geom_smooth(color = "blue")+
  labs(x="", y = "Robos totales*", title = "Número de Robos Totales* en CDMX al Mes", 
       subtitle = "De acuerdo a la FGJCDMX entre 01/2016 y 12/2022",
       caption = "De acuerdo a la Lista de Robos*, Fuente: Carpetas de Investigación - FGJCDMX, Datos Abiertos")+
  theme(plot.title = element_text(size = 28, face = "bold", color = "#9f2441"),
        plot.subtitle = element_text(size = 22, face="bold"))
# LDT de asesinatos ----
carpetas_completa %>% filter(delito %in% asesinatos) %>% 
  mutate(ano_mes = format(fecha_inicio, format = "%Y-%m")) %>% count(ano_mes) %>% 
  mutate(fecha = as.Date(paste0(ano_mes,"-01"))) %>% 
  ggplot(aes(fecha,n))+
  geom_line(color = "#9f2241")+
  geom_smooth(color = "blue")+
  labs(x="", y = "Asesinatos totales*", title = "Número de Asesinatos Totales* en CDMX al Mes", 
       subtitle = "De acuerdo a la FGJCDMX entre 01/2016 y 12/2022",
       caption = "De acuerdo a la Lista de Asesinatos*, Fuente: Carpetas de Investigación - FGJCDMX, Datos Abiertos")+
  theme(plot.title = element_text(size = 28, face = "bold", color = "#9f2441"),
        plot.subtitle = element_text(size = 22, face="bold"))

# LDT Delitos Sexuales ----
carpetas_completa %>% filter(delito %in% delitos_sexuales) %>% 
  mutate(ano_mes = format(fecha_inicio, format = "%Y-%m")) %>% count(ano_mes) %>% 
  mutate(fecha = as.Date(paste0(ano_mes,"-01"))) %>% 
  ggplot(aes(fecha,n))+
  geom_line(color = "#9f2241")+
  geom_smooth(color = "blue")+
  labs(x="", y = "Delitos Sexuales totales*", title = "Número de Delitos Sexuales Totales* en CDMX al Mes", 
       subtitle = "De acuerdo a la FGJCDMX entre 01/2016 y 12/2022",
       caption = "De acuerdo a la Lista de Delitos Sexuales*, Fuente: Carpetas de Investigación - FGJCDMX, Datos Abiertos")+
  theme(plot.title = element_text(size = 28, face = "bold", color = "#9f2441"),
        plot.subtitle = element_text(size = 22, face="bold"))



## Coso ----
carpetas_completa %>% filter(categoria_delito == "DELITO DE BAJO IMPACTO")%>% 
  filter(delito %in% robos) %>% count(delito) %>% filter(n>1000) %>% 
  ggplot(aes(reorder(delito,n),n,fill = delito))+
  geom_col(color = "black")+
  geom_label(aes(label = comma(n)))+
  labs(x="", y="Número de Carpetas", title = "Robos Categorizados en Carpetas FGJ", 
       caption = "Elaboración Propia, Datos Abiertos, Carpetas FGJCDMX", 
       subtitle = "Para la categoría Delitos de Bajo Impacto con más de 1,000 observaciones")+
  theme(plot.title = element_text(size = 28, face = "bold", color = "#9f2441"),
        plot.subtitle = element_text(size = 22, face="bold"), 
        legend.position = "none", 
        axis.text.y = element_text(color = "black", size = 10))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  scale_y_sqrt()


# Empezamos A Seccionar ----
carpetas_completa %>% filter(delito %in% robos) %>% count()
# 662,583 observaciones desde 2016
# Agrupamos robos en general --
fecha_robos <- puntos_carpetas %>% filter(delito %in% robos) %>%  mutate(c_delito = "ROBOS") %>% 
  mutate(ano_mes = format(fecha_inicio,format = "%Y-%m")) %>% select(ano_mes,c_delito,geometry) %>% 
  group_split(ano_mes, c_delito)
#Asesinatos --
carpetas_completa %>% filter(delito %in% asesinatos) %>% count()
# 14,207 obs
fecha_asesinatos <- puntos_carpetas %>% filter(delito %in% asesinatos) %>%  mutate(c_delito = "ASESINATOS") %>% 
  mutate(ano_mes = format(fecha_inicio,format = "%Y-%m"))%>% select(ano_mes,c_delito,geometry) %>% 
  group_split(ano_mes, c_delito)
# Delitos en general --
fechas_delitos <- puntos_carpetas %>% mutate(c_delito = "TODOS") %>%
  mutate(ano_mes = format(fecha_inicio,format = "%Y-%m")) %>% select(ano_mes,c_delito,geometry) %>% 
  group_split(ano_mes, c_delito)

# Funcion que itera ----
iterated_intersection <- function(x,y,z){
  for (i in 1:length(x)){
    df <- x[[i]] # ith data frame
    fetch_date <- df[1,1] #get date, 1st column
    fetch_what <- paste(df[1,2]) # categoria - delito
    isects <- lengths(st_intersects(z,df$geometry)) # vector of intersections
    y[,ncol(y)+1] <- isects # después lo puedo colapsar como columna de atributos con pivot wider
    colnames(y)[ncol(y)] <- paste(fetch_what,fetch_date) # rename w date
    print(paste(i, "intersected data frames out of:", length(x)))
  }
  return(y)
}
test <- data.frame(id = 1:length(colonias_cdmx$geometry))# La función necesita saber cuántas tiene
colonias_cdmx <- st_transform(colonias_cdmx,st_crs(fecha_robos[[1]]$geometry))
colonias_cdmx <- colonias_cdmx %>% st_make_valid()
# Aplicamos función a todos -----
tok <- Sys.time()
robos_fechas_intersecciones<- iterated_intersection(fecha_robos,test,colonias_cdmx) # 84 dfs
asesinatos_fechas_intersecciones<- iterated_intersection(fecha_asesinatos,test,colonias_cdmx) # 84 dfs
delitos_general <- iterated_intersection(fechas_delitos,test,colonias_cdmx) # 84 dfs
tik <- Sys.time()
print(tik-tok)


## Generar base con colonias cdmx y fechas completas -----

dates_complete <- seq(as.Date("2016-01-01"),as.Date("2022-12-01"),by = "month") %>% rep(2243) # each county has full dates
colonias_key <- colonias_cdmx$CVE_COL %>% rep(84) %>% sort() # Must be same length as the sequence of dates
dummy_df <- tibble(dates_complete,CVE_COL = colonias_key)
complete_df <- dummy_df %>% left_join(colonias_cdmx,by = "CVE_COL")

#Proceso acortado de generar base ----
#Robos
robos_final <- robos_fechas_intersecciones %>% cbind(CVE_COL = colonias_cdmx$CVE_COL) %>%  select(!id) %>% 
  pivot_longer(!CVE_COL,names_to = "delito_fecha",values_to = "delitos") %>% 
  separate(delito_fecha, into = c("delito", "ano_mes"),sep = " ") %>% mutate(dates_complete = as.Date(paste0(ano_mes, "-01")))
#Asesinatos
asesinatos_final <- asesinatos_fechas_intersecciones %>% cbind(CVE_COL = colonias_cdmx$CVE_COL) %>%  select(!id) %>% 
  pivot_longer(!CVE_COL,names_to = "delito_fecha",values_to = "delitos") %>% 
  separate(delito_fecha, into = c("delito", "ano_mes"),sep = " ") %>% mutate(dates_complete = as.Date(paste0(ano_mes, "-01")))
#Delitos Totales
delitos_totales_final <- delitos_general %>% cbind(CVE_COL = colonias_cdmx$CVE_COL) %>%  select(!id) %>% 
  pivot_longer(!CVE_COL,names_to = "delito_fecha",values_to = "delitos") %>% 
  separate(delito_fecha, into = c("delito", "ano_mes"),sep = " ") %>% mutate(dates_complete = as.Date(paste0(ano_mes, "-01")))


# Pegamos a base final ----

complete_robos <- complete_df %>% left_join(robos_final, by = c("CVE_COL","dates_complete"))
complete_asesinatos <- complete_df %>% left_join(asesinatos_final, by = c("CVE_COL","dates_complete"))
complete_delitos <- complete_df %>% left_join(delitos_totales_final, by = c("CVE_COL","dates_complete"))

# Inicio de proyectos ----
# Creamos columnas para los respectivos dummies de inicios y ordenamos base
# Robos
complete_robos <- complete_robos %>% 
  mutate(inicio_cb1 = ifelse(dates_complete > as.Date("2021-07-11"),1,0)) %>%
  mutate(inicio_cb2 = ifelse(dates_complete > as.Date("2021-08-08"),1,0)) %>% 
  mutate(inicio_te = ifelse(dates_complete > as.Date("2022-10-29"),1,0)) %>% 
  mutate(inicio_l9 = ifelse(dates_complete > as.Date("2021-01-30"),1,0)) %>% 
  mutate(para_l12 = ifelse(dates_complete > as.Date("2021-05-03"),1,0)) %>% 
  mutate(para_l1 = ifelse(dates_complete > as.Date("2022-07-11"),1,0)) %>% 
  select(dates_complete,CVE_COL,NOM_MUN,NOM_ENT,POBTOT,IM_2020,
         delito,delitos,cb_1,cb_2,tr_e,tr_9,l_1,l_12,inicio_cb1,inicio_cb2,inicio_te,
         inicio_l9,para_l1,para_l12,geometry)
# Asesinatos 
complete_asesinatos <- complete_asesinatos %>% 
  mutate(inicio_cb1 = ifelse(dates_complete > as.Date("2021-07-11"),1,0)) %>%
  mutate(inicio_cb2 = ifelse(dates_complete > as.Date("2021-08-08"),1,0)) %>% 
  mutate(inicio_te = ifelse(dates_complete > as.Date("2022-10-29"),1,0)) %>% 
  mutate(inicio_l9 = ifelse(dates_complete > as.Date("2021-01-30"),1,0)) %>% 
  mutate(para_l12 = ifelse(dates_complete > as.Date("2021-05-03"),1,0)) %>% 
  mutate(para_l1 = ifelse(dates_complete > as.Date("2022-07-11"),1,0)) %>% 
  select(dates_complete,CVE_COL,NOM_MUN,NOM_ENT,POBTOT,IM_2020,
         delito,delitos,cb_1,cb_2,tr_e,tr_9,l_1,l_12,inicio_cb1,inicio_cb2,inicio_te,
         inicio_l9,para_l1,para_l12,geometry)
# Totales 
# Asesinatos 
complete_delitos <- complete_delitos %>% 
  mutate(inicio_cb1 = ifelse(dates_complete > as.Date("2021-07-11"),1,0)) %>%
  mutate(inicio_cb2 = ifelse(dates_complete > as.Date("2021-08-08"),1,0)) %>% 
  mutate(inicio_te = ifelse(dates_complete > as.Date("2022-10-29"),1,0)) %>% 
  mutate(inicio_l9 = ifelse(dates_complete > as.Date("2021-01-30"),1,0)) %>% 
  mutate(para_l12 = ifelse(dates_complete > as.Date("2021-05-03"),1,0)) %>% 
  mutate(para_l1 = ifelse(dates_complete > as.Date("2022-07-11"),1,0)) %>% 
  select(dates_complete,CVE_COL,NOM_MUN,NOM_ENT,POBTOT,IM_2020,
         delito,delitos,cb_1,cb_2,tr_e,tr_9,l_1,l_12,inicio_cb1,inicio_cb2,inicio_te,
         inicio_l9,para_l1,para_l12,geometry)

## LISTOOO!!!
