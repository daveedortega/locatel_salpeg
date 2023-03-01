## Trying for models w IMG CONAPO 2020 ----
pacman::p_load(estimatr,haven,stargazer)

# CB1 ----
# DID Incidentes Viales
dd_1 <- complete_choque_sl_accidente %>% lm(incidencia ~ cb_1+inicio_cb1+cb_1*inicio_cb1 + POBTOT + IM_2020 ,.)
dd_2 <- complete_choque_cl_accidente %>% lm(incidencia ~ cb_1+inicio_cb1+cb_1*inicio_cb1 + POBTOT + IM_2020 ,.)
dd_3 <- complete_atropellado_lesionado %>% lm(incidencia ~ cb_1+inicio_cb1+cb_1*inicio_cb1 + POBTOT + IM_2020 ,.)
dd_4 <- complete_moto_accidente %>% lm(incidencia ~ cb_1+inicio_cb1+cb_1*inicio_cb1 + POBTOT + IM_2020,.)
dd_5 <- complete_total %>% lm(incidencia ~ cb_1+inicio_cb1+cb_1*inicio_cb1 + POBTOT + IM_2020,.)
# Tabla
stargazer(dd_1,dd_2,dd_3,dd_4,dd_5, type = "text", digits = 2, covariate.labels = c("CB L1", "Start L1","2020 Population", "Margination Index 2020", "CB1 X Start"),
          dep.var.labels = "|1 = Choque Sin lesionados | 2 = Choque con Lesionados | 3  = Atropellado Lesionado | 4 = Accidente de Moto | 5 = Total Incidentes |", 
          title = "Cablebus L1 Effects on Incidentes Viales")
# Carpetas de Investigacion 
dd_1 <- complete_robos %>% lm(delitos ~ cb_1+inicio_cb1+cb_1*inicio_cb1 + POBTOT + IM_2020,.)
dd_2 <- complete_asesinatos %>% lm(delitos ~ cb_1+inicio_cb1+cb_1*inicio_cb1 + POBTOT + IM_2020,.)
dd_3 <- complete_delitos %>% lm(delitos ~ cb_1+inicio_cb1+cb_1*inicio_cb1 + POBTOT + IM_2020,.)
# Tabla
stargazer(dd_1,dd_2,dd_3, type = "text", digits = 2, covariate.labels = c("CB L1","Start L1","Population 2020", "Margination Index 2020","L1 X Start"),
          dep.var.labels = "|1 = ROBOS | 2 = ASESINATOS | 3  = Delitos Totales |", 
          title = "Cablebus L1 Effects on Carpetas de Investigacion")

# CB2 ----
# Incidentes Viales
dd_1 <- complete_choque_sl_accidente %>% lm(incidencia ~ cb_2+inicio_cb2+cb_2*inicio_cb2 + POBTOT + IM_2020,.)
dd_2 <- complete_choque_cl_accidente %>% lm(incidencia ~ cb_2+inicio_cb2+cb_2*inicio_cb2 + POBTOT + IM_2020,.)
dd_3 <- complete_atropellado_lesionado %>% lm(incidencia ~ cb_2+inicio_cb2+cb_2*inicio_cb2 + POBTOT + IM_2020,.)
dd_4 <- complete_moto_accidente %>% lm(incidencia ~ cb_2+inicio_cb2+cb_2*inicio_cb2 + POBTOT + IM_2020,.)
dd_5 <- complete_total %>% lm(incidencia ~ cb_2+inicio_cb2+cb_2*inicio_cb2 + POBTOT + IM_2020,.)

# Tabla
stargazer(dd_1,dd_2,dd_3,dd_4,dd_5, type = "text", digits = 2, covariate.labels = c("CB L2","Start L2","2020 Population", "Margination Index 2020","L2 X Start"),
          dep.var.labels = "|1 = Choque Sin lesionados | 2 = Choque con Lesionados | 3  = Atropellado Lesionado | 4 = Accidente de Moto | 5 = Total Incidentes |", 
          title = "Cablebus L2 Effects on Incidentes Viales")

# Carpetas de Investigacion
dd_1 <- complete_robos %>% lm(delitos ~ cb_2+inicio_cb2+cb_2*inicio_cb2 + POBTOT + IM_2020,.)
dd_2 <- complete_asesinatos %>% lm(delitos ~ cb_2+inicio_cb2+cb_2*inicio_cb2 + POBTOT + IM_2020,.)
dd_3 <- complete_delitos %>% lm(delitos ~ cb_2+inicio_cb2+cb_2*inicio_cb2 + POBTOT + IM_2020,.)
# Tabla
stargazer(dd_1,dd_2,dd_3, type = "text", digits = 2, covariate.labels = c("CB L2","Start L2","Population 2020", "Margination Index 2020","L2 X Start"),
          dep.var.labels = "|1 = ROBOS | 2 = ASESINATOS | 3  = Delitos Totales |", 
          title = "Cablebus L2 Effects on Carpetas de Investigacion")

# TR 9 ----
# Incidentes Viales
dd_1 <- complete_choque_sl_accidente %>% lm(incidencia ~ tr_9+inicio_l9+tr_9*inicio_l9 + POBTOT + IM_2020,.)
dd_2 <- complete_choque_cl_accidente %>% lm(incidencia ~ tr_9+inicio_l9+tr_9*inicio_l9 + POBTOT + IM_2020,.)
dd_3 <- complete_atropellado_lesionado %>% lm(incidencia ~ tr_9+inicio_l9+tr_9*inicio_l9 + POBTOT + IM_2020,.)
dd_4 <- complete_moto_accidente %>% lm(incidencia ~ tr_9+inicio_l9+tr_9*inicio_l9 + POBTOT + IM_2020,.)
dd_5 <- complete_total %>% lm(incidencia ~ tr_9+inicio_l9+tr_9*inicio_l9 + POBTOT + IM_2020,.)

# Tabla
stargazer(dd_1,dd_2,dd_3,dd_4,dd_5, type = "text", digits = 2, covariate.labels = c("Trolebus L9","Start L9","Population 2020", "Margination Index 2020","L9 X Start"),
          dep.var.labels = "|1 = Choque Sin lesionados | 2 = Choque con Lesionados | 3  = Atropellado Lesionado | 4 = Accidente de Moto | 5 = Total Incidentes", 
          title = "Trolebus L9 Effects on Incidentes Viales")
# Carpetas de Investigacion
dd_1 <- complete_robos %>% lm(delitos ~ tr_9+inicio_l9+tr_9*inicio_l9 + POBTOT + IM_2020,.)
dd_2 <- complete_asesinatos %>% lm(delitos ~ tr_9+inicio_l9+tr_9*inicio_l9 + POBTOT + IM_2020,.)
dd_3 <- complete_delitos %>% lm(delitos ~ tr_9+inicio_l9+tr_9*inicio_l9 + POBTOT + IM_2020,.)
# Tabla
stargazer(dd_1,dd_2,dd_3, type = "text", digits = 2, covariate.labels = c("Trolebus L9","Start L9","Population 2020", "Margination Index 2020","L9 X Start"),
          dep.var.labels = "|1 = ROBOS | 2 = ASESINATOS | 3  = Delitos Totales |",
          title = "Trolebus L9 Effects on Incidentes Viales")

# TR E ----
# Incidentes Viales
dd_1 <- complete_choque_sl_accidente %>% lm(incidencia ~ tr_e+inicio_te+tr_e*inicio_te +  POBTOT + IM_2020,.)
dd_2 <- complete_choque_cl_accidente %>% lm(incidencia ~ tr_e+inicio_te+tr_e*inicio_te +  POBTOT + IM_2020,.)
dd_3 <- complete_atropellado_lesionado %>% lm(incidencia ~ tr_e+inicio_te+tr_e*inicio_te +  POBTOT + IM_2020,.)
dd_4 <- complete_moto_accidente %>% lm(incidencia ~ tr_e+inicio_te+tr_e*inicio_te +  POBTOT + IM_2020,.)
dd_5 <- complete_total %>% lm(incidencia ~ tr_e+inicio_te+tr_e*inicio_te +  POBTOT + IM_2020,.)

# Tabla
stargazer(dd_1,dd_2,dd_3,dd_4,dd_5, type = "text", digits = 2, covariate.labels = c("Trolebus Elevado","Start","Population 2020", "Margination Index 2020","TRE X Start"),
          dep.var.labels = "|1 = Choque Sin lesionados | 2 = Choque con Lesionados | 3  = Atropellado Lesionado | 4 = Accidente de Moto | 5 = Incidentes Totales |", 
          title = "Trolebus Elevado Effects on Incidentes Viales")
# Carpetas de Investigacion
dd_1 <- complete_robos %>% lm(delitos ~ tr_e+inicio_te+tr_e*inicio_te + POBTOT + IM_2020,.)
dd_2 <- complete_asesinatos %>% lm(delitos ~  tr_e+inicio_te+tr_e*inicio_te + POBTOT + IM_2020,.)
dd_3 <- complete_delitos %>% lm(delitos ~  tr_e+inicio_te+tr_e*inicio_te + POBTOT + IM_2020,.)
# Tabla
stargazer(dd_1,dd_2,dd_3, type = "text", digits = 2, covariate.labels = c("Trolebus Elevado","Start","Population 2020","Margination Index 2020","TRE X Start"),
          dep.var.labels = "|1 = ROBOS | 2 = ASESINATOS | 3  = Delitos Totales |",
          title = "Trolebus Elevado Effects on Carpetas de Investigación")

# L1 ----
# Efectos fijos poligono y fecha(año) ?
# Incidentes Viales
dd_1 <- complete_choque_sl_accidente %>% lm(incidencia ~ l_1+para_l1+l_1*para_l1 +  POBTOT + IM_2020,.)
dd_2 <- complete_choque_cl_accidente %>% lm(incidencia ~ l_1+para_l1+l_1*para_l1 +  POBTOT + IM_2020,.)
dd_3 <- complete_atropellado_lesionado %>% lm(incidencia ~ l_1+para_l1+l_1*para_l1 +  POBTOT + IM_2020,.)
dd_4 <- complete_moto_accidente %>% lm(incidencia ~ l_1+para_l1+l_1*para_l1 +  POBTOT + IM_2020,.)
dd_5 <- complete_total %>% lm(incidencia ~ l_1+para_l1+l_1*para_l1 +  POBTOT + IM_2020,.)
# Tabla
stargazer(dd_1,dd_2,dd_3,dd_4,dd_5, type = "text", digits = 2, covariate.labels = c("STC L1","Stop L1","Population 2020", "Margination Idnex 2020","L1 X Stop"),
          dep.var.labels = "|1 = Choque Sin lesionados | 2 = Choque con Lesionados | 3  = Atropellado Lesionado | 4 = Accidente de Moto | 5 = Incidentes Totales |", 
          title = "STC L1 Effects on Incidentes Viales")
# Carpetas de Investigacion
dd_1 <- complete_robos %>% lm(delitos ~  l_1+para_l1+l_1*para_l1 + POBTOT + IM_2020,.)
dd_2 <- complete_asesinatos %>% lm(delitos ~   l_1+para_l1+l_1*para_l1 + POBTOT + IM_2020,.)
dd_3 <- complete_delitos %>% lm(delitos ~   l_1+para_l1+l_1*para_l1 + POBTOT + IM_2020,.)
# Tabla
stargazer(dd_1,dd_2,dd_3, type = "text", digits = 2, covariate.labels = c("STC L1","Stop L1","Population 2020", "Margination Index 2020","L1 X Stop"),
          dep.var.labels = "|1 = ROBOS | 2 = ASESINATOS | 3  = Delitos Totales |",
          title = "STC L1 Effects on Carpetas de Investigación")

# L12 ----
# Efectos fijos poligono y fecha(año)
# Incidentes Viales
dd_1 <- complete_choque_sl_accidente %>% lm(incidencia ~ l_12+para_l12+l_12*para_l12 + POBTOT + IM_2020,.)
dd_2 <- complete_choque_cl_accidente %>% lm(incidencia ~ l_12+para_l12+l_12*para_l12 + POBTOT + IM_2020,.)
dd_3 <- complete_atropellado_lesionado %>% lm(incidencia ~ l_12+para_l12+l_12*para_l12 + POBTOT + IM_2020,.)
dd_4 <- complete_moto_accidente %>% lm(incidencia ~ l_12+para_l12+l_12*para_l12 + POBTOT + IM_2020,.)
dd_5 <- complete_total %>% lm(incidencia ~ l_12+para_l12+l_12*para_l12 + POBTOT + IM_2020,.)
# Tabla
stargazer(dd_1,dd_2,dd_3,dd_4,dd_5, type = "text", digits = 2, covariate.labels = c("STC L12","Stop L12","Population 2020", "Margination Index 2020","L12 X Stop"),
          dep.var.labels = "|1 = Choque Sin lesionados | 2 = Choque con Lesionados | 3  = Atropellado Lesionado | 4 = Accidente de Moto | 5 = Incidentes Totales |", 
          title = "STC L12 Effects on Incidentes Viales")

# Carpetas de Investigacion
dd_1 <- complete_robos %>% lm(delitos ~ l_12+para_l12+l_12*para_l12 +  POBTOT + IM_2020,.)
dd_2 <- complete_asesinatos %>% lm(delitos ~ l_12+para_l12+l_12*para_l12 +  POBTOT + IM_2020,.)
dd_3 <- complete_delitos %>% lm(delitos ~  l_12+para_l12+l_12*para_l12 +  POBTOT + IM_2020,.)
# Tabla
stargazer(dd_1,dd_2,dd_3, type = "text", digits = 2, covariate.labels = c("STC L12","Stop L12","Population 2020", "Margination Index 2020","L12 X Stop"),
          dep.var.labels = "|1 = ROBOS | 2 = ASESINATOS | 3  = Delitos Totales |",
          title = "STC L12 Effects on Carpetas de Investigación")







