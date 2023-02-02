## Trying for models ----
pacman::p_load(estimatr,haven,stargazer)
# RDD for treated colonies before and after thee introduction of CB in Choques sin Lesionados ----

complete_choque_sl_accidente %>% filter(cb_1 ==1) %>% lm(incidencia ~ inicio_cb1*dates_complete,.)

# DD?

# CB1
dd_1 <- complete_choque_sl_accidente %>% lm(incidencia ~ cb_1+inicio_cb1+cb_1*inicio_cb1 + POB2010,.)
# Diferencias con Robust LM?
complete_choque_sl_accidente %>% lm_robust(incidencia ~ cb_1+inicio_cb1+cb_1*inicio_cb1 + POB2010,.)
#
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

# L12
dd_1 <- complete_choque_sl_accidente %>% lm(incidencia ~ l_12+para_l12+l_12*para_l12 + POB2010,.)
dd_2 <- complete_choque_cl_accidente %>% lm(incidencia ~ l_12+para_l12+l_12*para_l12 + POB2010,.)
dd_3 <- complete_atropellado_lesionado %>% lm(incidencia ~ l_12+para_l12+l_12*para_l12 + POB2010,.)
dd_4 <- complete_moto_accidente %>% lm(incidencia ~ l_12+para_l12+l_12*para_l12 + POB2010,.)

stargazer(dd_1,dd_2,dd_3,dd_4, type = "text", digits = 2, covariate.labels = c("STC L12","Stop L12","Population 2010","L12 X Stop"),
          dep.var.labels = "|1 = Choque Sin lesionados | 2 = Choque con Lesionados | 3  = Atropellado Lesionado | 4 = Accidente de Moto |", 
          title = "STC L12 Effects on Incidentes Viales")

