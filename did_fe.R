## Adding fixed effects 
# David A. Ortega
# 07/03/2023

# Load packages ----
pacman::p_load(estimatr,haven,stargazer,lfe,plm,lmtest)

# Aadding fixed effects per colonia and month ----
# Month effects take up inicio cb_1

## Cablebus L1 ------------------------------------------------------
# choques sin lesionados Sin BUFFER ----
ddfe_c_sl_1 <- plm(incidencia ~ cb_1+inicio_cb1+cb_1*inicio_cb1 + IM_2020 + POBTOT, 
              data = complete_choque_sl_accidente,
              index = c("dates_complete", "CVE_COL"))
# wo pop
ddfe_c_sl_2 <- plm(incidencia ~ cb_1+inicio_cb1+cb_1*inicio_cb1 + IM_2020, 
                   data = complete_choque_sl_accidente,
                   index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_c_sl_3 <- plm(incidencia ~ cb_1+inicio_cb1+cb_1*inicio_cb1, 
                   data = complete_choque_sl_accidente,
                   index = c("dates_complete", "CVE_COL"))

# Latex
stargazer(ddfe_c_sl_1,ddfe_c_sl_2,ddfe_c_sl_3,type = "latex",title="Efectos Cablebus Línea 1 sin Buffer", 
          dep.var.labels = "Choques sin Lesionados Mensuales por Colinia",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("CB L1", "Margination Index 2020", "2020 Population", "CB1 X Start"),
          out = "output/resultados/cbl1/choques_sl_sb_cbl1.tex")

# choques sin lesionados CON BUFFER ----
ddfe_c_sl_1.1 <- plm(incidencia ~ cb_1b+inicio_cb1+cb_1b*inicio_cb1 + IM_2020 + POBTOT, 
                      data = complete_choque_sl_accidente,
                      index = c("dates_complete", "CVE_COL"))
# Not statistically significant the interection, hmm :(
# wo pop
ddfe_c_sl_2.1 <- plm(incidencia ~ cb_1b+inicio_cb1+cb_1b*inicio_cb1 + IM_2020, 
                      data = complete_choque_sl_accidente,
                      index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_c_sl_3.1 <- plm(incidencia ~ cb_1b+inicio_cb1+cb_1b*inicio_cb1, 
                      data = complete_choque_sl_accidente,
                      index = c("dates_complete", "CVE_COL"))
# Latex
stargazer(ddfe_c_sl_1.1,ddfe_c_sl_2.1,ddfe_c_sl_3.1,type = "latex", title="Efectos Cablebus Línea 1 con Buffer", 
          dep.var.labels = "Choques sin Lesionados Mensuales por Colinia",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("CB L1", "Margination Index 2020", "2020 Population", "CB1 X Start"),
          out = "output/resultados/cbl1/choques_sl_cb_cbl1.tex")

# choques con lesionados Sin Buffer----
ddfe_c_cl_1 <- plm(incidencia ~ cb_1+inicio_cb1+cb_1*inicio_cb1 + IM_2020 + POBTOT, 
                   data = complete_choque_cl_accidente,
                   index = c("dates_complete", "CVE_COL"))
#wo pop
ddfe_c_cl_2 <- plm(incidencia ~ cb_1+inicio_cb1+cb_1*inicio_cb1 + IM_2020, 
                   data = complete_choque_cl_accidente,
                   index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_c_cl_3 <- plm(incidencia ~ cb_1+inicio_cb1+cb_1*inicio_cb1, 
                   data = complete_choque_cl_accidente,
                   index = c("dates_complete", "CVE_COL"))
# Latex
stargazer(ddfe_c_cl_1,ddfe_c_cl_2,ddfe_c_cl_3,type = "latex", title="Efectos Cablebus Línea 1 sin Buffer", 
          dep.var.labels = "Choques con Lesionados Mensuales por Colinia",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("CB L1", "Margination Index 2020", "2020 Population", "CB1 X Start"),
          out = "output/resultados/cbl1/choques_cl_sb_cbl1.tex")

# Choques con lesionados con buffer ----
ddfe_c_cl_1.1 <- plm(incidencia ~ cb_1b+inicio_cb1+cb_1b*inicio_cb1 + IM_2020 + POBTOT, 
                   data = complete_choque_cl_accidente,
                   index = c("dates_complete", "CVE_COL"))
#wo pop
ddfe_c_cl_2.1 <- plm(incidencia ~ cb_1b+inicio_cb1+cb_1b*inicio_cb1 + IM_2020, 
                   data = complete_choque_cl_accidente,
                   index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_c_cl_3.1 <- plm(incidencia ~ cb_1b+inicio_cb1+cb_1b*inicio_cb1, 
                   data = complete_choque_cl_accidente,
                   index = c("dates_complete", "CVE_COL"))
# latex
stargazer(ddfe_c_cl_1.1,ddfe_c_cl_2.1,ddfe_c_cl_3.1,type = "latex", title="Efectos Cablebus Línea 1 con Buffer", 
          dep.var.labels = "Choques con Lesionados Mensualmente por Colonia",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("CB L1", "Margination Index 2020", "2020 Population", "CB1 X Start"),
          out = "output/resultados/cbl1/choques_cl_cb_cbl1.tex")
# Atropellados con lesionados sin buffer ----
ddfe_a_1 <- plm(incidencia ~ cb_1+inicio_cb1+cb_1*inicio_cb1 + IM_2020 + POBTOT, 
                     data = complete_atropellado_lesionado,
                     index = c("dates_complete", "CVE_COL"))
#wo population
ddfe_a_2 <- plm(incidencia ~ cb_1+inicio_cb1+cb_1*inicio_cb1 + IM_2020, 
                     data = complete_atropellado_lesionado,
                     index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_a_3 <- plm(incidencia ~ cb_1+inicio_cb1+cb_1*inicio_cb1, 
                     data = complete_atropellado_lesionado,
                     index = c("dates_complete", "CVE_COL"))
# latex
stargazer(ddfe_a_1,ddfe_a_2,ddfe_a_3,type = "latex", title="Efectos Cablebus Línea 1 sin Buffer", 
          dep.var.labels = "Atropellados Lesionados por Colinia Mensualmente",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("CB L1", "Margination Index 2020", "2020 Population", "CB1 X Start"),
          out = "output/resultados/cbl1/atropellados_sb_cbl1.tex")

# Atropellados con lesionados con buffer ----
ddfe_a_1.1 <- plm(incidencia ~ cb_1b+inicio_cb1+cb_1b*inicio_cb1 + IM_2020 + POBTOT, 
                data = complete_atropellado_lesionado,
                index = c("dates_complete", "CVE_COL"))
#wo pop
ddfe_a_2.1 <- plm(incidencia ~ cb_1b+inicio_cb1+cb_1b*inicio_cb1 + IM_2020, 
                data = complete_atropellado_lesionado,
                index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_a_3.1 <- plm(incidencia ~ cb_1b+inicio_cb1+cb_1b*inicio_cb1, 
                data = complete_atropellado_lesionado,
                index = c("dates_complete", "CVE_COL"))
# latex
stargazer(ddfe_a_1.1,ddfe_a_2.1,ddfe_a_3.1,type = "latex",title="Efectos Cablebus Línea 1 con Buffer", 
          dep.var.labels = "Atropellados Lesionados por Colinia Mensualmente",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("CB L1", "Margination Index 2020", "2020 Population", "CB1 X Start"),
          out = "output/resultados/cbl1/atropellados_cb_cbl1.tex")
# Accidentes en Moto sin buffer ----
ddfe_m_1 <- plm(incidencia ~ cb_1+inicio_cb1+cb_1*inicio_cb1 + IM_2020 + POBTOT, 
                data = complete_moto_accidente,
                index = c("dates_complete", "CVE_COL"))
#wo population
ddfe_m_2 <- plm(incidencia ~ cb_1+inicio_cb1+cb_1*inicio_cb1 + IM_2020, 
                data = complete_moto_accidente,
                index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_m_3 <- plm(incidencia ~ cb_1+inicio_cb1+cb_1*inicio_cb1, 
                data = complete_moto_accidente,
                index = c("dates_complete", "CVE_COL"))
# latex
stargazer(ddfe_m_1,ddfe_m_2,ddfe_m_3,type = "latex", title="Efectos Cablebus Línea 1 sin Buffer", 
          dep.var.labels = "Accidentes en Motocicleta Mensuales por Colinia Mensualmente",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("CB L1", "Margination Index 2020", "2020 Population", "CB1 X Start"),
          out = "output/resultados/cbl1/moto_sb_cbl1.tex")
# Accidentes en Moto con buffer ----
ddfe_m_1.1 <- plm(incidencia ~ cb_1b+inicio_cb1+cb_1b*inicio_cb1 + IM_2020 + POBTOT, 
                data = complete_moto_accidente,
                index = c("dates_complete", "CVE_COL"))
#wo population
ddfe_m_2.1 <- plm(incidencia ~ cb_1b+inicio_cb1+cb_1b*inicio_cb1 + IM_2020, 
                data = complete_moto_accidente,
                index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_m_3.1 <- plm(incidencia ~ cb_1b+inicio_cb1+cb_1b*inicio_cb1, 
                data = complete_moto_accidente,
                index = c("dates_complete", "CVE_COL"))
# latex
stargazer(ddfe_m_1.1,ddfe_m_2.1,ddfe_m_3.1,type = "latex", title="Efectos Cablebus Línea 1 con Buffer", 
          dep.var.labels = "Accidentes en Motocicleta Mensuales por Colinia Mensualmente",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("CB L1", "Margination Index 2020", "2020 Population", "CB1 X Start"),
          out = "output/resultados/cbl1/moto_cb_cbl1.tex")
# Complete InViales sin buffer ----
ddfe_tot_1 <- plm(incidencia ~ cb_1+inicio_cb1+cb_1*inicio_cb1 + IM_2020 + POBTOT, 
                  data = complete_total,
                  index = c("dates_complete", "CVE_COL"))
#wo population
ddfe_tot_2 <- plm(incidencia ~ cb_1+inicio_cb1+cb_1*inicio_cb1 + IM_2020, 
                  data = complete_total,
                  index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_tot_3 <- plm(incidencia ~ cb_1+inicio_cb1+cb_1*inicio_cb1, 
                  data = complete_total,
                  index = c("dates_complete", "CVE_COL"))
# latex
stargazer(ddfe_tot_1,ddfe_tot_2,ddfe_tot_3,type = "latex",title="Efectos Cablebus Línea 1 sin Buffer", 
          dep.var.labels = "Total de Incidentes Viales Mensuales por Colinia Mensualmente",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("CB L2", "Margination Index 2020", "2020 Population", "cb1 X Start"),
          out = "output/resultados/cbl1/totinvi_sb_cbl1.tex")

# Complete InViales con buffer ----
ddfe_tot_1.1 <- plm(incidencia ~ cb_1b+inicio_cb1+cb_1b*inicio_cb1 + IM_2020 + POBTOT, 
                  data = complete_total,
                  index = c("dates_complete", "CVE_COL"))
#wo population
ddfe_tot_2.1 <- plm(incidencia ~ cb_1b+inicio_cb1+cb_1b*inicio_cb1 + IM_2020, 
                  data = complete_total,
                  index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_tot_3.1 <- plm(incidencia ~ cb_1b+inicio_cb1+cb_1b*inicio_cb1, 
                  data = complete_total,
                  index = c("dates_complete", "CVE_COL"))
# latex
stargazer(ddfe_tot_1.1,ddfe_tot_2.1,ddfe_tot_3.1,type = "latex", title="Efectos Cablebus Línea 1 con Buffer", 
          dep.var.labels = "Total de Incidentes Viales Mensuales por Colinia Mensualmente",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("CB L2", "Margination Index 2020", "2020 Population", "cb1 X Start"),
          out = "output/resultados/cbl1/totinvi_cb_cbl1.tex")

## Cablebus L2 ----------------------------------------------------
# choques sin lesionados Sin BUFFER ----
ddfe_c_sl_1 <- plm(incidencia ~ cb_2+inicio_cb2+cb_2*inicio_cb2 + IM_2020 + POBTOT, 
                   data = complete_choque_sl_accidente,
                   index = c("dates_complete", "CVE_COL"))
# wo pop
ddfe_c_sl_2 <- plm(incidencia ~ cb_2+inicio_cb2+cb_2*inicio_cb2 + IM_2020, 
                   data = complete_choque_sl_accidente,
                   index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_c_sl_3 <- plm(incidencia ~ cb_2+inicio_cb2+cb_2*inicio_cb2, 
                   data = complete_choque_sl_accidente,
                   index = c("dates_complete", "CVE_COL"))

# Latex
stargazer(ddfe_c_sl_1,ddfe_c_sl_2,ddfe_c_sl_3,type = "latex",title="Efectos Cablebus Línea 2 sin Buffer", 
          dep.var.labels = "Choques sin Lesionados Mensuales por Colinia",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("CB L2", "Margination Index 2020", "2020 Population", "CB2 X Start"),
          out = "output/resultados/cbl2/choques_sl_sb_cbl2.tex")

# choques sin lesionados CON BUFFER ----
ddfe_c_sl_1.1 <- plm(incidencia ~ cb_2b+inicio_cb2+cb_2b*inicio_cb2 + IM_2020 + POBTOT, 
                     data = complete_choque_sl_accidente,
                     index = c("dates_complete", "CVE_COL"))
# Not statistically significant the interection, hmm :(
# wo pop
ddfe_c_sl_2.1 <- plm(incidencia ~ cb_2b+inicio_cb2+cb_2b*inicio_cb2 + IM_2020, 
                     data = complete_choque_sl_accidente,
                     index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_c_sl_3.1 <- plm(incidencia ~ cb_2b+inicio_cb2+cb_2b*inicio_cb2, 
                     data = complete_choque_sl_accidente,
                     index = c("dates_complete", "CVE_COL"))
# Latex
stargazer(ddfe_c_sl_1.1,ddfe_c_sl_2.1,ddfe_c_sl_3.1,type = "latex", title="Efectos Cablebus Línea 2 con Buffer", 
          dep.var.labels = "Choques sin Lesionados Mensuales por Colinia",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("CB L2", "Margination Index 2020", "2020 Population", "CB2 X Start"),
          out = "output/resultados/cbl2/choques_sl_cb_cbl2.tex")

# choques con lesionados Sin Buffer----
ddfe_c_cl_1 <- plm(incidencia ~ cb_2+inicio_cb2+cb_2*inicio_cb2 + IM_2020 + POBTOT, 
                   data = complete_choque_cl_accidente,
                   index = c("dates_complete", "CVE_COL"))
#wo pop
ddfe_c_cl_2 <- plm(incidencia ~ cb_2+inicio_cb2+cb_2*inicio_cb2 + IM_2020, 
                   data = complete_choque_cl_accidente,
                   index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_c_cl_3 <- plm(incidencia ~ cb_2+inicio_cb2+cb_2*inicio_cb2, 
                   data = complete_choque_cl_accidente,
                   index = c("dates_complete", "CVE_COL"))
# Latex
stargazer(ddfe_c_cl_1,ddfe_c_cl_2,ddfe_c_cl_3,type = "latex", title="Efectos Cablebus Línea 2 sin Buffer", 
          dep.var.labels = "Choques con Lesionados Mensuales por Colinia",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("CB L2", "Margination Index 2020", "2020 Population", "CB2 X Start"),
          out = "output/resultados/cbl2/choques_cl_sb_cbl2.tex")

# Choques con lesionados con buffer ----
ddfe_c_cl_1.1 <- plm(incidencia ~ cb_2b+inicio_cb2+cb_2b*inicio_cb2 + IM_2020 + POBTOT, 
                     data = complete_choque_cl_accidente,
                     index = c("dates_complete", "CVE_COL"))
#wo pop
ddfe_c_cl_2.1 <- plm(incidencia ~ cb_2b+inicio_cb2+cb_2b*inicio_cb2 + IM_2020, 
                     data = complete_choque_cl_accidente,
                     index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_c_cl_3.1 <- plm(incidencia ~ cb_2b+inicio_cb2+cb_2b*inicio_cb2, 
                     data = complete_choque_cl_accidente,
                     index = c("dates_complete", "CVE_COL"))
# latex
stargazer(ddfe_c_cl_1.1,ddfe_c_cl_2.1,ddfe_c_cl_3.1,type = "latex", title="Efectos Cablebus Línea 2 con Buffer", 
          dep.var.labels = "Choques con Lesionados por Colinia Mensualmente",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("CB L2", "Margination Index 2020", "2020 Population", "CB2 X Start"),
          out = "output/resultados/cbl2/choques_cl_cb_cbl2.tex")
# Atropellados con lesionados sin buffer ----
ddfe_a_1 <- plm(incidencia ~ cb_2+inicio_cb2+cb_2*inicio_cb2 + IM_2020 + POBTOT, 
                data = complete_atropellado_lesionado,
                index = c("dates_complete", "CVE_COL"))
#wo population
ddfe_a_2 <- plm(incidencia ~ cb_2+inicio_cb2+cb_2*inicio_cb2 + IM_2020, 
                data = complete_atropellado_lesionado,
                index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_a_3 <- plm(incidencia ~ cb_2+inicio_cb2+cb_2*inicio_cb2, 
                data = complete_atropellado_lesionado,
                index = c("dates_complete", "CVE_COL"))
# latex
stargazer(ddfe_a_1,ddfe_a_2,ddfe_a_3,type = "latex", title="Efectos Cablebus Línea 2 sin Buffer", 
          dep.var.labels = "Atropellados Lesionados por Colinia Mensualmente",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("CB L2", "Margination Index 2020", "2020 Population", "CB2 X Start"),
          out = "output/resultados/cbl2/atropellados_sb_cbl2.tex")

# Atropellados con lesionados con buffer ----
ddfe_a_1.1 <- plm(incidencia ~ cb_2b+inicio_cb2+cb_2b*inicio_cb2 + IM_2020 + POBTOT, 
                  data = complete_atropellado_lesionado,
                  index = c("dates_complete", "CVE_COL"))
#wo pop
ddfe_a_2.1 <- plm(incidencia ~ cb_2b+inicio_cb2+cb_2b*inicio_cb2 + IM_2020, 
                  data = complete_atropellado_lesionado,
                  index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_a_3.1 <- plm(incidencia ~ cb_2b+inicio_cb2+cb_2b*inicio_cb2, 
                  data = complete_atropellado_lesionado,
                  index = c("dates_complete", "CVE_COL"))
# latex
stargazer(ddfe_a_1.1,ddfe_a_2.1,ddfe_a_3.1,type = "latex",title="Efectos Cablebus Línea 2 con Buffer", 
          dep.var.labels = "Atropellados Lesionados por Colinia Mensualmente",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("CB L2", "Margination Index 2020", "2020 Population", "CB2 X Start"),
          out = "output/resultados/cbl2/atropellados_cb_cbl2.tex")
# Accidentes en Moto sin buffer ----
ddfe_m_1 <- plm(incidencia ~ cb_2+inicio_cb2+cb_2*inicio_cb2 + IM_2020 + POBTOT, 
                data = complete_moto_accidente,
                index = c("dates_complete", "CVE_COL"))
#wo population
ddfe_m_2 <- plm(incidencia ~ cb_2+inicio_cb2+cb_2*inicio_cb2 + IM_2020, 
                data = complete_moto_accidente,
                index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_m_3 <- plm(incidencia ~ cb_2+inicio_cb2+cb_2*inicio_cb2, 
                data = complete_moto_accidente,
                index = c("dates_complete", "CVE_COL"))
# latex
stargazer(ddfe_m_1,ddfe_m_2,ddfe_m_3,type = "latex", title="Efectos Cablebus Línea 2 sin Buffer", 
          dep.var.labels = "Accidentes en Motocicleta Mensuales por Colinia Mensualmente",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("CB L2", "Margination Index 2020", "2020 Population", "CB2 X Start"),
          out = "output/resultados/cbl2/moto_sb_cbl2.tex")
# Accidentes en Moto con buffer ----
ddfe_m_1.1 <- plm(incidencia ~ cb_2b+inicio_cb2+cb_2b*inicio_cb2 + IM_2020 + POBTOT, 
                  data = complete_moto_accidente,
                  index = c("dates_complete", "CVE_COL"))
#wo population
ddfe_m_2.1 <- plm(incidencia ~ cb_2b+inicio_cb2+cb_2b*inicio_cb2 + IM_2020, 
                  data = complete_moto_accidente,
                  index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_m_3.1 <- plm(incidencia ~ cb_2b+inicio_cb2+cb_2b*inicio_cb2, 
                  data = complete_moto_accidente,
                  index = c("dates_complete", "CVE_COL"))
# latex
stargazer(ddfe_m_1.1,ddfe_m_2.1,ddfe_m_3.1,type = "latex", title="Efectos Cablebus Línea 2 con Buffer", 
          dep.var.labels = "Accidentes en Motocicleta Mensuales por Colinia Mensualmente",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("CB L2", "Margination Index 2020", "2020 Population", "CB2 X Start"),
          out = "output/resultados/cbl2/moto_cb_cbl2.tex")
# Complete InViales sin buffer ----
ddfe_tot_1 <- plm(incidencia ~ cb_2+inicio_cb2+cb_2*inicio_cb2 + IM_2020 + POBTOT, 
                  data = complete_total,
                  index = c("dates_complete", "CVE_COL"))
#wo population
ddfe_tot_2 <- plm(incidencia ~ cb_2+inicio_cb2+cb_2*inicio_cb2 + IM_2020, 
                  data = complete_total,
                  index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_tot_3 <- plm(incidencia ~ cb_2+inicio_cb2+cb_2*inicio_cb2, 
                  data = complete_total,
                  index = c("dates_complete", "CVE_COL"))
# latex
stargazer(ddfe_tot_1,ddfe_tot_2,ddfe_tot_3,type = "latex",title="Efectos Cablebus Línea 2 sin Buffer", 
          dep.var.labels = "Total de Incidentes Viales Mensuales por Colinia Mensualmente",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("CB L2", "Margination Index 2020", "2020 Population", "CB2 X Start"),
          out = "output/resultados/cbl2/totinvi_sb_cbl2.tex")

# Complete InViales con buffer ----
ddfe_tot_1.1 <- plm(incidencia ~ cb_2b+inicio_cb2+cb_2b*inicio_cb2 + IM_2020 + POBTOT, 
                    data = complete_total,
                    index = c("dates_complete", "CVE_COL"))
#wo population
ddfe_tot_2.1 <- plm(incidencia ~ cb_2b+inicio_cb2+cb_2b*inicio_cb2 + IM_2020, 
                    data = complete_total,
                    index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_tot_3.1 <- plm(incidencia ~ cb_2b+inicio_cb2+cb_2b*inicio_cb2, 
                    data = complete_total,
                    index = c("dates_complete", "CVE_COL"))
# latex
stargazer(ddfe_tot_1.1,ddfe_tot_2.1,ddfe_tot_3.1,type = "latex", title="Efectos Cablebus Línea 2 con Buffer", 
          dep.var.labels = "Total de Incidentes Viales Mensuales por Colinia Mensualmente",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("CB L2", "Margination Index 2020", "2020 Population", "CB2 X Start"),
          out = "output/resultados/cbl2/totinvi_cb_cbl2.tex")
## Trolebus L9 ----------------------------------------------------
# choques sin lesionados Sin BUFFER ----
ddfe_c_sl_1 <- plm(incidencia ~ tr_9+inicio_l9+tr_9*inicio_l9 + IM_2020 + POBTOT, 
                   data = complete_choque_sl_accidente,
                   index = c("dates_complete", "CVE_COL"))
# wo pop
ddfe_c_sl_2 <- plm(incidencia ~ tr_9+inicio_l9+tr_9*inicio_l9 + IM_2020, 
                   data = complete_choque_sl_accidente,
                   index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_c_sl_3 <- plm(incidencia ~ tr_9+inicio_l9+tr_9*inicio_l9, 
                   data = complete_choque_sl_accidente,
                   index = c("dates_complete", "CVE_COL"))

# Latex
stargazer(ddfe_c_sl_1,ddfe_c_sl_2,ddfe_c_sl_3,type = "latex",title="Efectos Trolebús Línea 9 sin Buffer", 
          dep.var.labels = "Choques sin Lesionados Mensuales por Colinia",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("TR L9", "Margination Index 2020", "2020 Population", "TRL9 X Start"),
          out = "output/resultados/tr9/choques_sl_sb_trl9.tex")

# choques sin lesionados CON BUFFER ----
ddfe_c_sl_1.1 <- plm(incidencia ~ tr_9b+inicio_l9+tr_9b*inicio_l9 + IM_2020 + POBTOT, 
                     data = complete_choque_sl_accidente,
                     index = c("dates_complete", "CVE_COL"))
# Not statistically significant the interection, hmm :(
# wo pop
ddfe_c_sl_2.1 <- plm(incidencia ~ tr_9b+inicio_l9+tr_9b*inicio_l9 + IM_2020, 
                     data = complete_choque_sl_accidente,
                     index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_c_sl_3.1 <- plm(incidencia ~ tr_9b+inicio_l9+tr_9b*inicio_l9, 
                     data = complete_choque_sl_accidente,
                     index = c("dates_complete", "CVE_COL"))
# Latex
stargazer(ddfe_c_sl_1.1,ddfe_c_sl_2.1,ddfe_c_sl_3.1,type = "latex", title="Efectos Trolebús Línea 9 con Buffer", 
          dep.var.labels = "Choques sin Lesionados Mensuales por Colinia",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("TR L9", "Margination Index 2020", "2020 Population", "TRL9 X Start"),
          out = "output/resultados/tr9/choques_sl_cb_trl9.tex")

# choques con lesionados Sin Buffer----
ddfe_c_cl_1 <- plm(incidencia ~ tr_9+inicio_l9+tr_9*inicio_l9 + IM_2020 + POBTOT, 
                   data = complete_choque_cl_accidente,
                   index = c("dates_complete", "CVE_COL"))
#wo pop
ddfe_c_cl_2 <- plm(incidencia ~ tr_9+inicio_l9+tr_9*inicio_l9 + IM_2020, 
                   data = complete_choque_cl_accidente,
                   index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_c_cl_3 <- plm(incidencia ~ tr_9+inicio_l9+tr_9*inicio_l9, 
                   data = complete_choque_cl_accidente,
                   index = c("dates_complete", "CVE_COL"))
# Latex
stargazer(ddfe_c_cl_1,ddfe_c_cl_2,ddfe_c_cl_3,type = "latex", title="Efectos Trolebús Línea 9 sin Buffer", 
          dep.var.labels = "Choques con Lesionados Mensuales por Colinia",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("TR L9", "Margination Index 2020", "2020 Population", "TRL9 X Start"),
          out = "output/resultados/tr9/choques_cl_sb_trl9.tex")

# Choques con lesionados con buffer ----
ddfe_c_cl_1.1 <- plm(incidencia ~ tr_9b+inicio_l9+tr_9b*inicio_l9 + IM_2020 + POBTOT, 
                     data = complete_choque_cl_accidente,
                     index = c("dates_complete", "CVE_COL"))
#wo pop
ddfe_c_cl_2.1 <- plm(incidencia ~ tr_9b+inicio_l9+tr_9b*inicio_l9 + IM_2020, 
                     data = complete_choque_cl_accidente,
                     index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_c_cl_3.1 <- plm(incidencia ~ tr_9b+inicio_l9+tr_9b*inicio_l9, 
                     data = complete_choque_cl_accidente,
                     index = c("dates_complete", "CVE_COL"))
# latex
stargazer(ddfe_c_cl_1.1,ddfe_c_cl_2.1,ddfe_c_cl_3.1,type = "latex", title="Efectos Trolebús Línea 9 con Buffer", 
          dep.var.labels = "Choques con Lesionados por Colinia Mensualmente",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("TR L9", "Margination Index 2020", "2020 Population", "TRL9 X Start"),
          out = "output/resultados/tr9/choques_cl_cb_trl9.tex")
# Atropellados con lesionados sin buffer ----
ddfe_a_1 <- plm(incidencia ~ tr_9+inicio_l9+tr_9*inicio_l9 + IM_2020 + POBTOT, 
                data = complete_atropellado_lesionado,
                index = c("dates_complete", "CVE_COL"))
#wo population
ddfe_a_2 <- plm(incidencia ~ tr_9+inicio_l9+tr_9*inicio_l9 + IM_2020, 
                data = complete_atropellado_lesionado,
                index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_a_3 <- plm(incidencia ~ tr_9+inicio_l9+tr_9*inicio_l9, 
                data = complete_atropellado_lesionado,
                index = c("dates_complete", "CVE_COL"))
# latex
stargazer(ddfe_a_1,ddfe_a_2,ddfe_a_3,type = "latex", title="Efectos Trolebús Línea 9 sin Buffer", 
          dep.var.labels = "Atropellados Lesionados por Colinia Mensualmente",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("TR L9", "Margination Index 2020", "2020 Population", "TRL9 X Start"),
          out = "output/resultados/tr9/atropellados_sb_trl9.tex")

# Atropellados con lesionados con buffer ----
ddfe_a_1.1 <- plm(incidencia ~ tr_9b+inicio_l9+tr_9b*inicio_l9 + IM_2020 + POBTOT, 
                  data = complete_atropellado_lesionado,
                  index = c("dates_complete", "CVE_COL"))
#wo pop
ddfe_a_2.1 <- plm(incidencia ~ tr_9b+inicio_l9+tr_9b*inicio_l9 + IM_2020, 
                  data = complete_atropellado_lesionado,
                  index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_a_3.1 <- plm(incidencia ~ tr_9b+inicio_l9+tr_9b*inicio_l9, 
                  data = complete_atropellado_lesionado,
                  index = c("dates_complete", "CVE_COL"))
# latex
stargazer(ddfe_a_1.1,ddfe_a_2.1,ddfe_a_3.1,type = "latex",title="Efectos Trolebús Línea 9 con Buffer", 
          dep.var.labels = "Atropellados Lesionados por Colinia Mensualmente",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("TR L9", "Margination Index 2020", "2020 Population", "TRL9 X Start"),
          out = "output/resultados/tr9/atropellados_cb_trl9.tex")
# Accidentes en Moto sin buffer ----
ddfe_m_1 <- plm(incidencia ~ tr_9+inicio_l9+tr_9*inicio_l9 + IM_2020 + POBTOT, 
                data = complete_moto_accidente,
                index = c("dates_complete", "CVE_COL"))
#wo population
ddfe_m_2 <- plm(incidencia ~ tr_9+inicio_l9+tr_9*inicio_l9 + IM_2020, 
                data = complete_moto_accidente,
                index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_m_3 <- plm(incidencia ~ tr_9+inicio_l9+tr_9*inicio_l9, 
                data = complete_moto_accidente,
                index = c("dates_complete", "CVE_COL"))
# latex
stargazer(ddfe_m_1,ddfe_m_2,ddfe_m_3,type = "latex", title="Efectos Trolebús Línea 9 sin Buffer", 
          dep.var.labels = "Accidentes en Motocicleta Mensuales por Colinia Mensualmente",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("TR L9", "Margination Index 2020", "2020 Population", "TRL9 X Start"),
          out = "output/resultados/tr9/moto_sb_trl9.tex")
# Accidentes en Moto con buffer ----
ddfe_m_1.1 <- plm(incidencia ~ tr_9b+inicio_l9+tr_9b*inicio_l9 + IM_2020 + POBTOT, 
                  data = complete_moto_accidente,
                  index = c("dates_complete", "CVE_COL"))
#wo population
ddfe_m_2.1 <- plm(incidencia ~ tr_9b+inicio_l9+tr_9b*inicio_l9 + IM_2020, 
                  data = complete_moto_accidente,
                  index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_m_3.1 <- plm(incidencia ~ tr_9b+inicio_l9+tr_9b*inicio_l9, 
                  data = complete_moto_accidente,
                  index = c("dates_complete", "CVE_COL"))
# latex
stargazer(ddfe_m_1.1,ddfe_m_2.1,ddfe_m_3.1,type = "latex", title="Efectos Trolebús Línea 9 con Buffer", 
          dep.var.labels = "Accidentes en Motocicleta Mensuales por Colinia Mensualmente",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("TR L9", "Margination Index 2020", "2020 Population", "TRL9 X Start"),
          out = "output/resultados/tr9/moto_cb_trl9.tex")
# Complete InViales sin buffer ----
ddfe_tot_1 <- plm(incidencia ~ tr_9+inicio_l9+tr_9*inicio_l9 + IM_2020 + POBTOT, 
                  data = complete_total,
                  index = c("dates_complete", "CVE_COL"))
#wo population
ddfe_tot_2 <- plm(incidencia ~ tr_9+inicio_l9+tr_9*inicio_l9 + IM_2020, 
                  data = complete_total,
                  index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_tot_3 <- plm(incidencia ~ tr_9+inicio_l9+tr_9*inicio_l9, 
                  data = complete_total,
                  index = c("dates_complete", "CVE_COL"))
# latex
stargazer(ddfe_tot_1,ddfe_tot_2,ddfe_tot_3,type = "latex",title="Efectos Trolebús Línea 9 sin Buffer", 
          dep.var.labels = "Total de Incidentes Viales Mensuales por Colinia Mensualmente",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("TR L9", "Margination Index 2020", "2020 Population", "TRL9 X Start"),
          out = "output/resultados/tr9/totinvi_sb_trl9.tex")

# Complete InViales con buffer ----
ddfe_tot_1.1 <- plm(incidencia ~ tr_9b+inicio_l9+tr_9b*inicio_l9 + IM_2020 + POBTOT, 
                    data = complete_total,
                    index = c("dates_complete", "CVE_COL"))
#wo population
ddfe_tot_2.1 <- plm(incidencia ~ tr_9b+inicio_l9+tr_9b*inicio_l9 + IM_2020, 
                    data = complete_total,
                    index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_tot_3.1 <- plm(incidencia ~ tr_9b+inicio_l9+tr_9b*inicio_l9, 
                    data = complete_total,
                    index = c("dates_complete", "CVE_COL"))
# latex
stargazer(ddfe_tot_1.1,ddfe_tot_2.1,ddfe_tot_3.1,type = "latex", title="Efectos Trolebús Línea 9 con Buffer", 
          dep.var.labels = "Total de Incidentes Viales Mensuales por Colinia Mensualmente",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("TR L9", "Margination Index 2020", "2020 Population", "TRL9 X Start"),
          out = "output/resultados/tr9/totinvi_cb_trl9.tex")
## Trolebus Elevado ----------------------------------------------------
# choques sin lesionados Sin BUFFER ----
ddfe_c_sl_1 <- plm(incidencia ~ tr_e+inicio_te+tr_e*inicio_te + IM_2020 + POBTOT, 
                   data = complete_choque_sl_accidente,
                   index = c("dates_complete", "CVE_COL"))
# wo pop
ddfe_c_sl_2 <- plm(incidencia ~ tr_e+inicio_te+tr_e*inicio_te + IM_2020, 
                   data = complete_choque_sl_accidente,
                   index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_c_sl_3 <- plm(incidencia ~ tr_e+inicio_te+tr_e*inicio_te, 
                   data = complete_choque_sl_accidente,
                   index = c("dates_complete", "CVE_COL"))

# Latex
stargazer(ddfe_c_sl_1,ddfe_c_sl_2,ddfe_c_sl_3,type = "latex",title="Efectos Trolebús Elevado sin Buffer", 
          dep.var.labels = "Choques sin Lesionados Mensuales por Colinia",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("TR E", "Margination Index 2020", "2020 Population", "TRE X Start"),
          out = "output/resultados/tre/choques_sl_sb_tre.tex")

# choques sin lesionados CON BUFFER ----
ddfe_c_sl_1.1 <- plm(incidencia ~ tr_eb+inicio_te+tr_eb*inicio_te + IM_2020 + POBTOT, 
                     data = complete_choque_sl_accidente,
                     index = c("dates_complete", "CVE_COL"))
# Not statistically significant the interection, hmm :(
# wo pop
ddfe_c_sl_2.1 <- plm(incidencia ~ tr_eb+inicio_te+tr_eb*inicio_te + IM_2020, 
                     data = complete_choque_sl_accidente,
                     index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_c_sl_3.1 <- plm(incidencia ~ tr_eb+inicio_te+tr_eb*inicio_te, 
                     data = complete_choque_sl_accidente,
                     index = c("dates_complete", "CVE_COL"))
# Latex
stargazer(ddfe_c_sl_1.1,ddfe_c_sl_2.1,ddfe_c_sl_3.1,type = "latex", title="Efectos Trolebús Elevado con Buffer", 
          dep.var.labels = "Choques sin Lesionados Mensuales por Colinia",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("TR E", "Margination Index 2020", "2020 Population", "TRE X Start"),
          out = "output/resultados/tre/choques_sl_cb_tre.tex")

# choques con lesionados Sin Buffer----
ddfe_c_cl_1 <- plm(incidencia ~ tr_e+inicio_te+tr_e*inicio_te + IM_2020 + POBTOT, 
                   data = complete_choque_cl_accidente,
                   index = c("dates_complete", "CVE_COL"))
#wo pop
ddfe_c_cl_2 <- plm(incidencia ~ tr_e+inicio_te+tr_e*inicio_te + IM_2020, 
                   data = complete_choque_cl_accidente,
                   index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_c_cl_3 <- plm(incidencia ~ tr_e+inicio_te+tr_e*inicio_te, 
                   data = complete_choque_cl_accidente,
                   index = c("dates_complete", "CVE_COL"))
# Latex
stargazer(ddfe_c_cl_1,ddfe_c_cl_2,ddfe_c_cl_3,type = "latex", title="Efectos Trolebús Elevado sin Buffer", 
          dep.var.labels = "Choques con Lesionados Mensuales por Colinia",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("TR E", "Margination Index 2020", "2020 Population", "TRE X Start"),
          out = "output/resultados/tre/choques_cl_sb_tre.tex")

# Choques con lesionados con buffer ----
ddfe_c_cl_1.1 <- plm(incidencia ~ tr_eb+inicio_te+tr_eb*inicio_te + IM_2020 + POBTOT, 
                     data = complete_choque_cl_accidente,
                     index = c("dates_complete", "CVE_COL"))
#wo pop
ddfe_c_cl_2.1 <- plm(incidencia ~ tr_eb+inicio_te+tr_eb*inicio_te + IM_2020, 
                     data = complete_choque_cl_accidente,
                     index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_c_cl_3.1 <- plm(incidencia ~ tr_eb+inicio_te+tr_eb*inicio_te, 
                     data = complete_choque_cl_accidente,
                     index = c("dates_complete", "CVE_COL"))
# latex
stargazer(ddfe_c_cl_1.1,ddfe_c_cl_2.1,ddfe_c_cl_3.1,type = "latex", title="Efectos Trolebús Elevado con Buffer", 
          dep.var.labels = "Choques con Lesionados por Colinia Mensualmente",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("TR E", "Margination Index 2020", "2020 Population", "TRE X Start"),
          out = "output/resultados/tre/choques_cl_cb_tre.tex")
# Atropellados con lesionados sin buffer ----
ddfe_a_1 <- plm(incidencia ~ tr_e+inicio_te+tr_e*inicio_te + IM_2020 + POBTOT, 
                data = complete_atropellado_lesionado,
                index = c("dates_complete", "CVE_COL"))
#wo population
ddfe_a_2 <- plm(incidencia ~ tr_e+inicio_te+tr_e*inicio_te + IM_2020, 
                data = complete_atropellado_lesionado,
                index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_a_3 <- plm(incidencia ~ tr_e+inicio_te+tr_e*inicio_te, 
                data = complete_atropellado_lesionado,
                index = c("dates_complete", "CVE_COL"))
# latex
stargazer(ddfe_a_1,ddfe_a_2,ddfe_a_3,type = "latex", title="Efectos Trolebús Elevado sin Buffer", 
          dep.var.labels = "Atropellados Lesionados por Colinia Mensualmente",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("TR E", "Margination Index 2020", "2020 Population", "TRE X Start"),
          out = "output/resultados/tre/atropellados_sb_tre.tex")

# Atropellados con lesionados con buffer ----
ddfe_a_1.1 <- plm(incidencia ~ tr_eb+inicio_te+tr_eb*inicio_te + IM_2020 + POBTOT, 
                  data = complete_atropellado_lesionado,
                  index = c("dates_complete", "CVE_COL"))
#wo pop
ddfe_a_2.1 <- plm(incidencia ~ tr_eb+inicio_te+tr_eb*inicio_te + IM_2020, 
                  data = complete_atropellado_lesionado,
                  index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_a_3.1 <- plm(incidencia ~ tr_eb+inicio_te+tr_eb*inicio_te, 
                  data = complete_atropellado_lesionado,
                  index = c("dates_complete", "CVE_COL"))
# latex
stargazer(ddfe_a_1.1,ddfe_a_2.1,ddfe_a_3.1,type = "latex",title="Efectos Trolebús Elevado con Buffer", 
          dep.var.labels = "Atropellados Lesionados por Colinia Mensualmente",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("TR E", "Margination Index 2020", "2020 Population", "TRE X Start"),
          out = "output/resultados/tre/atropellados_cb_tre.tex")
# Accidentes en Moto sin buffer ----
ddfe_m_1 <- plm(incidencia ~ tr_e+inicio_te+tr_e*inicio_te + IM_2020 + POBTOT, 
                data = complete_moto_accidente,
                index = c("dates_complete", "CVE_COL"))
#wo population
ddfe_m_2 <- plm(incidencia ~ tr_e+inicio_te+tr_e*inicio_te + IM_2020, 
                data = complete_moto_accidente,
                index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_m_3 <- plm(incidencia ~ tr_e+inicio_te+tr_e*inicio_te, 
                data = complete_moto_accidente,
                index = c("dates_complete", "CVE_COL"))
# latex
stargazer(ddfe_m_1,ddfe_m_2,ddfe_m_3,type = "latex", title="Efectos Trolebús Elevado sin Buffer", 
          dep.var.labels = "Accidentes en Motocicleta Mensuales por Colinia Mensualmente",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("TR E", "Margination Index 2020", "2020 Population", "TRE X Start"),
          out = "output/resultados/tre/moto_sb_tre.tex")
# Accidentes en Moto con buffer ----
ddfe_m_1.1 <- plm(incidencia ~ tr_eb+inicio_te+tr_eb*inicio_te + IM_2020 + POBTOT, 
                  data = complete_moto_accidente,
                  index = c("dates_complete", "CVE_COL"))
#wo population
ddfe_m_2.1 <- plm(incidencia ~ tr_eb+inicio_te+tr_eb*inicio_te + IM_2020, 
                  data = complete_moto_accidente,
                  index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_m_3.1 <- plm(incidencia ~ tr_eb+inicio_te+tr_eb*inicio_te, 
                  data = complete_moto_accidente,
                  index = c("dates_complete", "CVE_COL"))
# latex
stargazer(ddfe_m_1.1,ddfe_m_2.1,ddfe_m_3.1,type = "latex", title="Efectos Trolebús Elevado con Buffer", 
          dep.var.labels = "Accidentes en Motocicleta Mensuales por Colinia Mensualmente",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("TR E", "Margination Index 2020", "2020 Population", "TRE X Start"),
          out = "output/resultados/tre/moto_cb_tre.tex")
# Complete InViales sin buffer ----
ddfe_tot_1 <- plm(incidencia ~ tr_e+inicio_te+tr_e*inicio_te + IM_2020 + POBTOT, 
                  data = complete_total,
                  index = c("dates_complete", "CVE_COL"))
#wo population
ddfe_tot_2 <- plm(incidencia ~ tr_e+inicio_te+tr_e*inicio_te + IM_2020, 
                  data = complete_total,
                  index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_tot_3 <- plm(incidencia ~ tr_e+inicio_te+tr_e*inicio_te, 
                  data = complete_total,
                  index = c("dates_complete", "CVE_COL"))
# latex
stargazer(ddfe_tot_1,ddfe_tot_2,ddfe_tot_3,type = "latex",title="Efectos Trolebús Elevado sin Buffer", 
          dep.var.labels = "Total de Incidentes Viales Mensuales por Colinia Mensualmente",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("TR E", "Margination Index 2020", "2020 Population", "TRE X Start"),
          out = "output/resultados/tre/totinvi_sb_tre.tex")

# Complete InViales con buffer ----
ddfe_tot_1.1 <- plm(incidencia ~ tr_eb+inicio_te+tr_eb*inicio_te + IM_2020 + POBTOT, 
                    data = complete_total,
                    index = c("dates_complete", "CVE_COL"))
#wo population
ddfe_tot_2.1 <- plm(incidencia ~ tr_eb+inicio_te+tr_eb*inicio_te + IM_2020, 
                    data = complete_total,
                    index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_tot_3.1 <- plm(incidencia ~ tr_eb+inicio_te+tr_eb*inicio_te, 
                    data = complete_total,
                    index = c("dates_complete", "CVE_COL"))
# latex
stargazer(ddfe_tot_1.1,ddfe_tot_2.1,ddfe_tot_3.1,type = "latex", title="Efectos Trolebús Elevado con Buffer", 
          dep.var.labels = "Total de Incidentes Viales Mensuales por Colinia Mensualmente",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("TR E", "Margination Index 2020", "2020 Population", "TRE X Start"),
          out = "output/resultados/tre/totinvi_cb_tre.tex")
## Línea 1 ----------------------------------------------------
# choques sin lesionados Sin BUFFER ----
ddfe_c_sl_1 <- plm(incidencia ~ l_1+para_l1+l_1*para_l1 + IM_2020 + POBTOT, 
                   data = complete_choque_sl_accidente,
                   index = c("dates_complete", "CVE_COL"))
# wo pop
ddfe_c_sl_2 <- plm(incidencia ~ l_1+para_l1+l_1*para_l1 + IM_2020, 
                   data = complete_choque_sl_accidente,
                   index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_c_sl_3 <- plm(incidencia ~ l_1+para_l1+l_1*para_l1, 
                   data = complete_choque_sl_accidente,
                   index = c("dates_complete", "CVE_COL"))

# Latex
stargazer(ddfe_c_sl_1,ddfe_c_sl_2,ddfe_c_sl_3,type = "latex",title="Efectos STC Línea 1 sin Buffer", 
          dep.var.labels = "Choques sin Lesionados Mensuales por Colinia",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("STC L1", "Margination Index 2020", "2020 Population", "L1 X Stop"),
          out = "output/resultados/stc_l1/choques_sl_sb_stc_l1.tex")

# choques sin lesionados CON BUFFER ----
ddfe_c_sl_1.1 <- plm(incidencia ~ l_1b+para_l1+l_1b*para_l1 + IM_2020 + POBTOT, 
                     data = complete_choque_sl_accidente,
                     index = c("dates_complete", "CVE_COL"))
# Not statistically significant the interection, hmm :(
# wo pop
ddfe_c_sl_2.1 <- plm(incidencia ~ l_1b+para_l1+l_1b*para_l1 + IM_2020, 
                     data = complete_choque_sl_accidente,
                     index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_c_sl_3.1 <- plm(incidencia ~ l_1b+para_l1+l_1b*para_l1, 
                     data = complete_choque_sl_accidente,
                     index = c("dates_complete", "CVE_COL"))
# Latex
stargazer(ddfe_c_sl_1.1,ddfe_c_sl_2.1,ddfe_c_sl_3.1,type = "latex", title="Efectos STC Línea 1 con Buffer", 
          dep.var.labels = "Choques sin Lesionados Mensuales por Colinia",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("STC L1", "Margination Index 2020", "2020 Population", "L1 X Stop"),
          out = "output/resultados/stc_l1/choques_sl_cb_stc_l1.tex")

# choques con lesionados Sin Buffer----
ddfe_c_cl_1 <- plm(incidencia ~ l_1+para_l1+l_1*para_l1 + IM_2020 + POBTOT, 
                   data = complete_choque_cl_accidente,
                   index = c("dates_complete", "CVE_COL"))
#wo pop
ddfe_c_cl_2 <- plm(incidencia ~ l_1+para_l1+l_1*para_l1 + IM_2020, 
                   data = complete_choque_cl_accidente,
                   index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_c_cl_3 <- plm(incidencia ~ l_1+para_l1+l_1*para_l1, 
                   data = complete_choque_cl_accidente,
                   index = c("dates_complete", "CVE_COL"))
# Latex
stargazer(ddfe_c_cl_1,ddfe_c_cl_2,ddfe_c_cl_3,type = "latex", title="Efectos STC Línea 1 sin Buffer", 
          dep.var.labels = "Choques con Lesionados Mensuales por Colinia",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("STC L1", "Margination Index 2020", "2020 Population", "L1 X Stop"),
          out = "output/resultados/stc_l1/choques_cl_sb_stc_l1.tex")

# Choques con lesionados con buffer ----
ddfe_c_cl_1.1 <- plm(incidencia ~ l_1b+para_l1+l_1b*para_l1 + IM_2020 + POBTOT, 
                     data = complete_choque_cl_accidente,
                     index = c("dates_complete", "CVE_COL"))
#wo pop
ddfe_c_cl_2.1 <- plm(incidencia ~ l_1b+para_l1+l_1b*para_l1 + IM_2020, 
                     data = complete_choque_cl_accidente,
                     index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_c_cl_3.1 <- plm(incidencia ~ l_1b+para_l1+l_1b*para_l1, 
                     data = complete_choque_cl_accidente,
                     index = c("dates_complete", "CVE_COL"))
# latex
stargazer(ddfe_c_cl_1.1,ddfe_c_cl_2.1,ddfe_c_cl_3.1,type = "latex", title="Efectos STC Línea 1 con Buffer", 
          dep.var.labels = "Choques con Lesionados por Colinia Mensualmente",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("STC L1", "Margination Index 2020", "2020 Population", "L1 X Stop"),
          out = "output/resultados/stc_l1/choques_cl_cb_stc_l1.tex")
# Atropellados con lesionados sin buffer ----
ddfe_a_1 <- plm(incidencia ~ l_1+para_l1+l_1*para_l1 + IM_2020 + POBTOT, 
                data = complete_atropellado_lesionado,
                index = c("dates_complete", "CVE_COL"))
#wo population
ddfe_a_2 <- plm(incidencia ~ l_1+para_l1+l_1*para_l1 + IM_2020, 
                data = complete_atropellado_lesionado,
                index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_a_3 <- plm(incidencia ~ l_1+para_l1+l_1*para_l1, 
                data = complete_atropellado_lesionado,
                index = c("dates_complete", "CVE_COL"))
# latex
stargazer(ddfe_a_1,ddfe_a_2,ddfe_a_3,type = "latex", title="Efectos STC Línea 1 sin Buffer", 
          dep.var.labels = "Atropellados Lesionados por Colinia Mensualmente",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("STC L1", "Margination Index 2020", "2020 Population", "L1 X Stop"),
          out = "output/resultados/stc_l1/atropellados_sb_stc_l1.tex")

# Atropellados con lesionados con buffer ----
ddfe_a_1.1 <- plm(incidencia ~ l_1b+para_l1+l_1b*para_l1 + IM_2020 + POBTOT, 
                  data = complete_atropellado_lesionado,
                  index = c("dates_complete", "CVE_COL"))
#wo pop
ddfe_a_2.1 <- plm(incidencia ~ l_1b+para_l1+l_1b*para_l1 + IM_2020, 
                  data = complete_atropellado_lesionado,
                  index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_a_3.1 <- plm(incidencia ~ l_1b+para_l1+l_1b*para_l1, 
                  data = complete_atropellado_lesionado,
                  index = c("dates_complete", "CVE_COL"))
# latex
stargazer(ddfe_a_1.1,ddfe_a_2.1,ddfe_a_3.1,type = "latex",title="Efectos STC Línea 1 con Buffer", 
          dep.var.labels = "Atropellados Lesionados por Colinia Mensualmente",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("STC L1", "Margination Index 2020", "2020 Population", "L1 X Stop"),
          out = "output/resultados/stc_l1/atropellados_cb_stc_l1.tex")
# Accidentes en Moto sin buffer ----
ddfe_m_1 <- plm(incidencia ~ l_1+para_l1+l_1*para_l1 + IM_2020 + POBTOT, 
                data = complete_moto_accidente,
                index = c("dates_complete", "CVE_COL"))
#wo population
ddfe_m_2 <- plm(incidencia ~ l_1+para_l1+l_1*para_l1 + IM_2020, 
                data = complete_moto_accidente,
                index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_m_3 <- plm(incidencia ~ l_1+para_l1+l_1*para_l1, 
                data = complete_moto_accidente,
                index = c("dates_complete", "CVE_COL"))
# latex
stargazer(ddfe_m_1,ddfe_m_2,ddfe_m_3,type = "latex", title="Efectos STC Línea 1 sin Buffer", 
          dep.var.labels = "Accidentes en Motocicleta Mensuales por Colinia Mensualmente",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("STC L1", "Margination Index 2020", "2020 Population", "L1 X Stop"),
          out = "output/resultados/stc_l1/moto_sb_stc_l1.tex")
# Accidentes en Moto con buffer ----
ddfe_m_1.1 <- plm(incidencia ~ l_1b+para_l1+l_1b*para_l1 + IM_2020 + POBTOT, 
                  data = complete_moto_accidente,
                  index = c("dates_complete", "CVE_COL"))
#wo population
ddfe_m_2.1 <- plm(incidencia ~ l_1b+para_l1+l_1b*para_l1 + IM_2020, 
                  data = complete_moto_accidente,
                  index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_m_3.1 <- plm(incidencia ~ l_1b+para_l1+l_1b*para_l1, 
                  data = complete_moto_accidente,
                  index = c("dates_complete", "CVE_COL"))
# latex
stargazer(ddfe_m_1.1,ddfe_m_2.1,ddfe_m_3.1,type = "latex", title="Efectos STC Línea 1 con Buffer", 
          dep.var.labels = "Accidentes en Motocicleta Mensuales por Colinia Mensualmente",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("STC L1", "Margination Index 2020", "2020 Population", "L1 X Stop"),
          out = "output/resultados/stc_l1/moto_cb_stc_l1.tex")
# Complete InViales sin buffer ----
ddfe_tot_1 <- plm(incidencia ~ l_1+para_l1+l_1*para_l1 + IM_2020 + POBTOT, 
                  data = complete_total,
                  index = c("dates_complete", "CVE_COL"))
#wo population
ddfe_tot_2 <- plm(incidencia ~ l_1+para_l1+l_1*para_l1 + IM_2020, 
                  data = complete_total,
                  index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_tot_3 <- plm(incidencia ~ l_1+para_l1+l_1*para_l1, 
                  data = complete_total,
                  index = c("dates_complete", "CVE_COL"))
# latex
stargazer(ddfe_tot_1,ddfe_tot_2,ddfe_tot_3,type = "latex",title="Efectos STC Línea 1 sin Buffer", 
          dep.var.labels = "Total de Incidentes Viales Mensuales por Colinia Mensualmente",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("STC L1", "Margination Index 2020", "2020 Population", "L1 X Stop"),
          out = "output/resultados/stc_l1/totinvi_sb_stc_l1.tex")

# Complete InViales con buffer ----
ddfe_tot_1.1 <- plm(incidencia ~ l_1b+para_l1+l_1b*para_l1 + IM_2020 + POBTOT, 
                    data = complete_total,
                    index = c("dates_complete", "CVE_COL"))
#wo population
ddfe_tot_2.1 <- plm(incidencia ~ l_1b+para_l1+l_1b*para_l1 + IM_2020, 
                    data = complete_total,
                    index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_tot_3.1 <- plm(incidencia ~ l_1b+para_l1+l_1b*para_l1, 
                    data = complete_total,
                    index = c("dates_complete", "CVE_COL"))
# latex
stargazer(ddfe_tot_1.1,ddfe_tot_2.1,ddfe_tot_3.1,type = "latex", title="Efectos STC Línea 1 con Buffer", 
          dep.var.labels = "Total de Incidentes Viales Mensuales por Colinia Mensualmente",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("STC L1", "Margination Index 2020", "2020 Population", "L1 X Stop"),
          out = "output/resultados/stc_l1/totinvi_cb_stc_l1.tex")

## Línea 12 ----------------------------------------------------
# choques sin lesionados Sin BUFFER ----
ddfe_c_sl_1 <- plm(incidencia ~ l_12+para_l12+l_12*para_l12 + IM_2020 + POBTOT, 
                   data = complete_choque_sl_accidente,
                   index = c("dates_complete", "CVE_COL"))
# wo pop
ddfe_c_sl_2 <- plm(incidencia ~ l_12+para_l12+l_12*para_l12 + IM_2020, 
                   data = complete_choque_sl_accidente,
                   index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_c_sl_3 <- plm(incidencia ~ l_12+para_l12+l_12*para_l12, 
                   data = complete_choque_sl_accidente,
                   index = c("dates_complete", "CVE_COL"))

# Latex
stargazer(ddfe_c_sl_1,ddfe_c_sl_2,ddfe_c_sl_3,type = "latex",title="Efectos STC Línea 12 sin Buffer", 
          dep.var.labels = "Choques sin Lesionados Mensuales por Colinia",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("STC L12", "Margination Index 2020", "2020 Population", "L12 X Stop"),
          out = "output/resultados/stc_l12/choques_sl_sb_stc_l12.tex")

# choques sin lesionados CON BUFFER ----
ddfe_c_sl_1.1 <- plm(incidencia ~ l_12b+para_l12+l_12b*para_l12 + IM_2020 + POBTOT, 
                     data = complete_choque_sl_accidente,
                     index = c("dates_complete", "CVE_COL"))
# Not statistically significant the interection, hmm :(
# wo pop
ddfe_c_sl_2.1 <- plm(incidencia ~ l_12b+para_l12+l_12b*para_l12 + IM_2020, 
                     data = complete_choque_sl_accidente,
                     index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_c_sl_3.1 <- plm(incidencia ~ l_12b+para_l12+l_12b*para_l12, 
                     data = complete_choque_sl_accidente,
                     index = c("dates_complete", "CVE_COL"))
# Latex
stargazer(ddfe_c_sl_1.1,ddfe_c_sl_2.1,ddfe_c_sl_3.1,type = "latex", title="Efectos STC Línea 12 con Buffer", 
          dep.var.labels = "Choques sin Lesionados Mensuales por Colinia",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("STC L12", "Margination Index 2020", "2020 Population", "L12 X Stop"),
          out = "output/resultados/stc_l12/choques_sl_cb_stc_l12.tex")

# choques con lesionados Sin Buffer----
ddfe_c_cl_1 <- plm(incidencia ~ l_12+para_l12+l_12*para_l12 + IM_2020 + POBTOT, 
                   data = complete_choque_cl_accidente,
                   index = c("dates_complete", "CVE_COL"))
#wo pop
ddfe_c_cl_2 <- plm(incidencia ~ l_12+para_l12+l_12*para_l12 + IM_2020, 
                   data = complete_choque_cl_accidente,
                   index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_c_cl_3 <- plm(incidencia ~ l_12+para_l12+l_12*para_l12, 
                   data = complete_choque_cl_accidente,
                   index = c("dates_complete", "CVE_COL"))
# Latex
stargazer(ddfe_c_cl_1,ddfe_c_cl_2,ddfe_c_cl_3,type = "latex", title="Efectos STC Línea 12 sin Buffer", 
          dep.var.labels = "Choques con Lesionados Mensuales por Colinia",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("STC L12", "Margination Index 2020", "2020 Population", "L12 X Stop"),
          out = "output/resultados/stc_l12/choques_cl_sb_stc_l12.tex")

# Choques con lesionados con buffer ----
ddfe_c_cl_1.1 <- plm(incidencia ~ l_12b+para_l12+l_12b*para_l12 + IM_2020 + POBTOT, 
                     data = complete_choque_cl_accidente,
                     index = c("dates_complete", "CVE_COL"))
#wo pop
ddfe_c_cl_2.1 <- plm(incidencia ~ l_12b+para_l12+l_12b*para_l12 + IM_2020, 
                     data = complete_choque_cl_accidente,
                     index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_c_cl_3.1 <- plm(incidencia ~ l_12b+para_l12+l_12b*para_l12, 
                     data = complete_choque_cl_accidente,
                     index = c("dates_complete", "CVE_COL"))
# latex
stargazer(ddfe_c_cl_1.1,ddfe_c_cl_2.1,ddfe_c_cl_3.1,type = "latex", title="Efectos STC Línea 12 con Buffer", 
          dep.var.labels = "Choques con Lesionados por Colinia Mensualmente",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("STC L12", "Margination Index 2020", "2020 Population", "L12 X Stop"),
          out = "output/resultados/stc_l12/choques_cl_cb_stc_l12.tex")
# Atropellados con lesionados sin buffer ----
ddfe_a_1 <- plm(incidencia ~ l_12+para_l12+l_12*para_l12 + IM_2020 + POBTOT, 
                data = complete_atropellado_lesionado,
                index = c("dates_complete", "CVE_COL"))
#wo population
ddfe_a_2 <- plm(incidencia ~ l_12+para_l12+l_12*para_l12 + IM_2020, 
                data = complete_atropellado_lesionado,
                index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_a_3 <- plm(incidencia ~ l_12+para_l12+l_12*para_l12, 
                data = complete_atropellado_lesionado,
                index = c("dates_complete", "CVE_COL"))
# latex
stargazer(ddfe_a_1,ddfe_a_2,ddfe_a_3,type = "latex", title="Efectos STC Línea 12 sin Buffer", 
          dep.var.labels = "Atropellados Lesionados por Colinia Mensualmente",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("STC L12", "Margination Index 2020", "2020 Population", "L12 X Stop"),
          out = "output/resultados/stc_l12/atropellados_sb_stc_l12.tex")

# Atropellados con lesionados con buffer ----
ddfe_a_1.1 <- plm(incidencia ~ l_12b+para_l12+l_12b*para_l12 + IM_2020 + POBTOT, 
                  data = complete_atropellado_lesionado,
                  index = c("dates_complete", "CVE_COL"))
#wo pop
ddfe_a_2.1 <- plm(incidencia ~ l_12b+para_l12+l_12b*para_l12 + IM_2020, 
                  data = complete_atropellado_lesionado,
                  index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_a_3.1 <- plm(incidencia ~ l_12b+para_l12+l_12b*para_l12, 
                  data = complete_atropellado_lesionado,
                  index = c("dates_complete", "CVE_COL"))
# latex
stargazer(ddfe_a_1.1,ddfe_a_2.1,ddfe_a_3.1,type = "latex",title="Efectos STC Línea 12 con Buffer", 
          dep.var.labels = "Atropellados Lesionados por Colinia Mensualmente",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("STC L12", "Margination Index 2020", "2020 Population", "L12 X Stop"),
          out = "output/resultados/stc_l12/atropellados_cb_stc_l12.tex")
# Accidentes en Moto sin buffer ----
ddfe_m_1 <- plm(incidencia ~ l_12+para_l12+l_12*para_l12 + IM_2020 + POBTOT, 
                data = complete_moto_accidente,
                index = c("dates_complete", "CVE_COL"))
#wo population
ddfe_m_2 <- plm(incidencia ~ l_12+para_l12+l_12*para_l12 + IM_2020, 
                data = complete_moto_accidente,
                index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_m_3 <- plm(incidencia ~ l_12+para_l12+l_12*para_l12, 
                data = complete_moto_accidente,
                index = c("dates_complete", "CVE_COL"))
# latex
stargazer(ddfe_m_1,ddfe_m_2,ddfe_m_3,type = "latex", title="Efectos STC Línea 12 sin Buffer", 
          dep.var.labels = "Accidentes en Motocicleta Mensuales por Colinia Mensualmente",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("STC L12", "Margination Index 2020", "2020 Population", "L12 X Stop"),
          out = "output/resultados/stc_l12/moto_sb_stc_l12.tex")
# Accidentes en Moto con buffer ----
ddfe_m_1.1 <- plm(incidencia ~ l_12b+para_l12+l_12b*para_l12 + IM_2020 + POBTOT, 
                  data = complete_moto_accidente,
                  index = c("dates_complete", "CVE_COL"))
#wo population
ddfe_m_2.1 <- plm(incidencia ~ l_12b+para_l12+l_12b*para_l12 + IM_2020, 
                  data = complete_moto_accidente,
                  index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_m_3.1 <- plm(incidencia ~ l_12b+para_l12+l_12b*para_l12, 
                  data = complete_moto_accidente,
                  index = c("dates_complete", "CVE_COL"))
# latex
stargazer(ddfe_m_1.1,ddfe_m_2.1,ddfe_m_3.1,type = "latex", title="Efectos STC Línea 12 con Buffer", 
          dep.var.labels = "Accidentes en Motocicleta Mensuales por Colinia Mensualmente",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("STC L12", "Margination Index 2020", "2020 Population", "L12 X Stop"),
          out = "output/resultados/stc_l12/moto_cb_stc_l12.tex")
# Complete InViales sin buffer ----
ddfe_tot_1 <- plm(incidencia ~ l_12+para_l12+l_12*para_l12 + IM_2020 + POBTOT, 
                  data = complete_total,
                  index = c("dates_complete", "CVE_COL"))
#wo population
ddfe_tot_2 <- plm(incidencia ~ l_12+para_l12+l_12*para_l12 + IM_2020, 
                  data = complete_total,
                  index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_tot_3 <- plm(incidencia ~ l_12+para_l12+l_12*para_l12, 
                  data = complete_total,
                  index = c("dates_complete", "CVE_COL"))
# latex
stargazer(ddfe_tot_1,ddfe_tot_2,ddfe_tot_3,type = "latex",title="Efectos STC Línea 12 sin Buffer", 
          dep.var.labels = "Total de Incidentes Viales Mensuales por Colinia Mensualmente",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("STC L12", "Margination Index 2020", "2020 Population", "L12 X Stop"),
          out = "output/resultados/stc_l12/totinvi_sb_stc_l12.tex")

# Complete InViales con buffer ----
ddfe_tot_1.1 <- plm(incidencia ~ l_12b+para_l12+l_12b*para_l12 + IM_2020 + POBTOT, 
                    data = complete_total,
                    index = c("dates_complete", "CVE_COL"))
#wo population
ddfe_tot_2.1 <- plm(incidencia ~ l_12b+para_l12+l_12b*para_l12 + IM_2020, 
                    data = complete_total,
                    index = c("dates_complete", "CVE_COL"))
# same without IM_2020
ddfe_tot_3.1 <- plm(incidencia ~ l_12b+para_l12+l_12b*para_l12, 
                    data = complete_total,
                    index = c("dates_complete", "CVE_COL"))
# latex
stargazer(ddfe_tot_1.1,ddfe_tot_2.1,ddfe_tot_3.1,type = "latex", title="Efectos STC Línea 12 con Buffer", 
          dep.var.labels = "Total de Incidentes Viales Mensuales por Colinia Mensualmente",
          column.labels = c("Complete","Without Populaton", "Without Covariates"),
          covariate.labels = c("STC L12", "Margination Index 2020", "2020 Population", "L12 X Stop"),
          out = "output/resultados/stc_l12/totinvi_cb_stc_l12.tex")

































