## Trying for models ----
pacman::p_load(estimatr,haven,stargazer)
# RDD for treated colonies before and after thee introduction of CB in Choques sin Lesionados ----


# CB1
dd_1 <- complete_choque_sl_accidente %>% lm(incidencia ~ cb_1+inicio_cb1+cb_1*inicio_cb1 + POB2010,.)
dd_2 <- complete_choque_cl_accidente %>% lm(incidencia ~ cb_1+inicio_cb1+cb_1*inicio_cb1 + POB2010,.)
dd_3 <- complete_atropellado_lesionado %>% lm(incidencia ~ cb_1+inicio_cb1+cb_1*inicio_cb1 + POB2010,.)
dd_4 <- complete_moto_accidente %>% lm(incidencia ~ cb_1+inicio_cb1+cb_1*inicio_cb1 + POB2010,.)

stargazer(dd_1,dd_2,dd_3,dd_4, type = "text", digits = 2, covariate.labels = c("CB L1","Start L1","Population 2010","L1 X Start"),
          dep.var.labels = "|1 = Choque Sin lesionados | 2 = Choque con Lesionados | 3  = Atropellado Lesionado | 4 = Accidente de Moto |", 
          title = "Cablebus L1 Effects on Incidentes Viales")

# CB2
dd_1 <- complete_choque_sl_accidente %>% lm(incidencia ~ cb_2+inicio_cb2+cb_2*inicio_cb2 + POB2010,.)
dd_2 <- complete_choque_cl_accidente %>% lm(incidencia ~ cb_2+inicio_cb2+cb_2*inicio_cb2 + POB2010,.)
dd_3 <- complete_atropellado_lesionado %>% lm(incidencia ~ cb_2+inicio_cb2+cb_2*inicio_cb2 + POB2010,.)
dd_4 <- complete_moto_accidente %>% lm(incidencia ~ cb_2+inicio_cb2+cb_2*inicio_cb2 + POB2010,.)

stargazer(dd_1,dd_2,dd_3,dd_4, type = "text", digits = 2, covariate.labels = c("CB L2","Start L2","Population 2010","L2 X Start"),
          dep.var.labels = "|1 = Choque Sin lesionados | 2 = Choque con Lesionados | 3  = Atropellado Lesionado | 4 = Accidente de Moto |", 
          title = "Cablebus L2 Effects on Incidentes Viales")

# TR 9
dd_1 <- complete_choque_sl_accidente %>% lm(incidencia ~ tr_9+inicio_l9+tr_9*inicio_l9 + POB2010,.)
dd_2 <- complete_choque_cl_accidente %>% lm(incidencia ~ tr_9+inicio_l9+tr_9*inicio_l9 + POB2010,.)
dd_3 <- complete_atropellado_lesionado %>% lm(incidencia ~ tr_9+inicio_l9+tr_9*inicio_l9 + POB2010,.)
dd_4 <- complete_moto_accidente %>% lm(incidencia ~ tr_9+inicio_l9+tr_9*inicio_l9 + POB2010,.)

stargazer(dd_1,dd_2,dd_3,dd_4, type = "text", digits = 2, covariate.labels = c("Trolebus L9","Start L9","Population 2010","L9 X Start"),
          dep.var.labels = "|1 = Choque Sin lesionados | 2 = Choque con Lesionados | 3  = Atropellado Lesionado | 4 = Accidente de Moto |", 
          title = "Trolebus L9 Effects on Incidentes Viales")

# TR E
dd_1 <- complete_choque_sl_accidente %>% lm(incidencia ~ tr_e+inicio_te+tr_e*inicio_te + POB2010,.)
dd_2 <- complete_choque_cl_accidente %>% lm(incidencia ~ tr_e+inicio_te+tr_e*inicio_te + POB2010,.)
dd_3 <- complete_atropellado_lesionado %>% lm(incidencia ~ tr_e+inicio_te+tr_e*inicio_te + POB2010,.)
dd_4 <- complete_moto_accidente %>% lm(incidencia ~ tr_e+inicio_te+tr_e*inicio_te + POB2010,.)

stargazer(dd_1,dd_2,dd_3,dd_4, type = "text", digits = 2, covariate.labels = c("Trolebus Elevado","Start","Population 2010","TR_E X Start"),
          dep.var.labels = "|1 = Choque Sin lesionados | 2 = Choque con Lesionados | 3  = Atropellado Lesionado | 4 = Accidente de Moto |", 
          title = "Trolebus Elevado Effects on Incidentes Viales")

# L1
# Efectos fijos poligono y fecha(año)

dd_1 <- complete_choque_sl_accidente %>% lm(incidencia ~ l_1+para_l1+l_1*para_l1 + POB2010,.)
dd_2 <- complete_choque_cl_accidente %>% lm(incidencia ~ l_1+para_l1+l_1*para_l1 + POB2010,.)
dd_3 <- complete_atropellado_lesionado %>% lm(incidencia ~ l_1+para_l1+l_1*para_l1 + POB2010,.)
dd_4 <- complete_moto_accidente %>% lm(incidencia ~ l_1+para_l1+l_1*para_l1 + POB2010,.)

stargazer(dd_1,dd_2,dd_3,dd_4, type = "text", digits = 2, covariate.labels = c("STC L1","Stop L1","Population 2010","L1 X Stop"),
          dep.var.labels = "|1 = Choque Sin lesionados | 2 = Choque con Lesionados | 3  = Atropellado Lesionado | 4 = Accidente de Moto |", 
          title = "STC L1 Effects on Incidentes Viales")

# L12
# Efectos fijos poligono y fecha(año)

dd_1 <- complete_choque_sl_accidente %>% lm(incidencia ~ l_12+para_l12+l_12*para_l12 + POB2010,.)
dd_2 <- complete_choque_cl_accidente %>% lm(incidencia ~ l_12+para_l12+l_12*para_l12 + POB2010,.)
dd_3 <- complete_atropellado_lesionado %>% lm(incidencia ~ l_12+para_l12+l_12*para_l12 + POB2010,.)
dd_4 <- complete_moto_accidente %>% lm(incidencia ~ l_12+para_l12+l_12*para_l12 + POB2010,.)

stargazer(dd_1,dd_2,dd_3,dd_4, type = "text", digits = 2, covariate.labels = c("STC L12","Stop L12","Population 2010","L12 X Stop"),
          dep.var.labels = "|1 = Choque Sin lesionados | 2 = Choque con Lesionados | 3  = Atropellado Lesionado | 4 = Accidente de Moto |", 
          title = "STC L12 Effects on Incidentes Viales")

