## Trying for models ----
pacman::p_load(estimatr,haven)
# RDD for treated colonies before and after thee introduction of CB in Choques sin Lesionados ----

complete_choque_sl_accidente %>% filter(cb_1 ==1) %>% lm(incidencia ~ inicio_cb1*dates_complete,.)

# DD?

complete_choque_sl_accidente %>% lm_robust(incidencia ~ cb_1+inicio_cb1+cb_1*inicio_cb1,.)

complete_choque_sl_accidente %>% lm_robust(incidencia ~ l_12+para_l12+l_12*para_l12,.)


