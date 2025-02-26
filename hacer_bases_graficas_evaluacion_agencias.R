
setwd("~/R/fiscalia/Evaluaciones_agencias")

iniciadas <- readxl::read_excel("Iniciadas por Territoriales_130225.xlsx") %>% clean_names()

personal <- readxl::read_excel("IniciadasporTerritoriales.xlsx") %>% clean_names() %>% 
  select(ctrluinv, fiscal)


iniciadas <- iniciadas %>% mutate(id_ap=as.integer(id_ap)) %>% 
  left_join(personal, "ctrluinv") #%>% 
  # left_join(incidencia$base %>% 
  #             select(id_ap, delito, modalidad_delito), 
  #           by="id_ap"
  # )

flagrancias <- incidencia$flagrancias


flagrancias <- flagrancias %>% 
  rename(ci_formato_siap_fsiap=ap_ci) %>% get_fiscalia()

flagrancias %>% 
  filter(year(fecha_inicio)==2025) %>% 
  filter(subprocuraduria=="COORDINACIÓN GENERAL DE INVESTIGACIÓN TERRITORIAL") %>% 
  tabyl(agencia_ini)

iniciadas <- iniciadas %>% 
  mutate(ct_inicio_ap=str_remove(ct_inicio_ap, "UAT-")) %>% 
  mutate(id_ap=as.integer(id_ap))

flagrancias <- flagrancias %>% 
  mutate(res_puestas=case_when(
    #motivo_de_libertad=="Artículo 140 CNPP (Libertad durante la investigación)" ~ "LIBERTAD MP",
    libertad==1 & observaciones_dgpec=="Ok" ~ "1. Libertad MP",
    observaciones_dgpec_2=="No califica de legal la detención (libertad)" & observaciones_dgpec=="Ok" ~ "2. Ilegal detención PJ",
    mecanismo_alternativo==1 ~ "3. Mecanismo alternativo",
    incompetencia==1 & observaciones_dgpec=="Ok" ~
      "8. Incompetencia",
    vinculacion_a_proceso_por_persona==1 & observaciones_dgpec=="Ok" & (prision_preventiva_justificada>0 | prision_preventiva_oficiosa>0) ~ "7. Prisión preventiva",
    vinculacion_a_proceso_por_persona==1 & suspension_condicional==1 & observaciones_dgpec=="Ok" ~ "6. Suspen. Condicional",
    vinculacion_a_proceso_por_persona==1 & observaciones_dgpec=="Ok" ~ "5. Vinculación simple",
    observaciones_dgpec=="Ok" &
      (observaciones_dgpec_2 %in% c("No vincula a proceso",
                                    "Perdón de la víctima",
                                    "Juez determina libertad",
                                    "Acuerdo Reparatorio")) ~
      "4. No vinculación",
    observaciones_dgpec!="Ok" ~ "9. Pendiente de estatus"
    
  ))


iniciadas <- iniciadas %>% 
  mutate(fiscalia=gsub("[0-9]|-", "", ct_inicio_ap), 
         fiscalia = case_when(
           fiscalia=="CJ" ~ "CUJ", 
           ct_inicio_ap=="H3" ~ "BJ", 
           ct_inicio_ap %in% c("H1", "H2") ~ "MH", 
           T ~ fiscalia
         ), 
         grupo=case_when(
           fiscalia %in% c("IZP", "CUH", "GAM") ~ "Alta", 
           fiscalia %in% c("BJ", "COY", "MH", "TLP", "VC", 
                           "AO") ~"Media", 
           T ~ "Baja"
         )
  ) %>% drop_na(fiscal)


#info gráfica 1


data <- iniciadas %>% 
  filter(grepl(fiscalias_territoriales, fiscalia),
         !grepl("CJM|URI|AOP", fiscalia)) %>% 
  # filter(grepl(fiscalia_analisis, ct_inicio_ap)) %>% 
  mutate(fecha_inicio=dmy(fecha_de_inicio)) %>% 
  filter(fecha_inicio>="2025-01-15", 
         fecha_inicio<="2025-01-31"
  ) %>% 
  mutate(quincena=case_when(
    fecha_inicio>="2025-01-15" ~ "Actual",
    T ~ "Anterior"
  ), 
  detenido=case_when(
    id_ap %in% flagrancias$id_ap ~ "Con detenido", 
    T ~ "Sin detenido"
  )
  ) 

periodo <- as.integer(as_date("2025-01-31")-as_date("2025-01-15"))

data_1_sd <- data %>% 
  filter(quincena=="Actual", 
         detenido=="Sin detenido") %>% 
  group_by(fiscalia, detenido, 
           fecha_inicio, 
           fiscal, grupo
  ) %>% 
  summarise(media=n()/periodo) 


data_1_cd <- data %>% 
  filter(quincena=="Actual", 
         detenido=="Con detenido")%>% 
  group_by(fiscalia, detenido, 
           fecha_inicio, 
           fiscal, grupo
  ) %>% 
  summarise(media=n()/periodo) 


data_1 <- bind_rows(data_1_sd, data_1_cd) %>% 
  rename("id_mp"=fiscal)


write.csv(data_1, "./bases/data_gr1.csv", row.names = F)

#solo promedio diario sin mp

data <- iniciadas %>% 
  filter(grepl(fiscalias_territoriales, fiscalia),
         !grepl("CJM|URI|AOP", fiscalia)) %>% 
  # filter(grepl(fiscalia_analisis, ct_inicio_ap)) %>% 
  mutate(fecha_inicio=dmy(fecha_de_inicio)) %>% 
  filter(fecha_inicio>="2025-01-15", 
         fecha_inicio<="2025-01-31"
  ) %>% 
  mutate(quincena=case_when(
    fecha_inicio>="2025-01-15" ~ "Actual",
    T ~ "Anterior"
  ), 
  detenido=case_when(
    id_ap %in% flagrancias$id_ap ~ "Con detenido", 
    T ~ "Sin detenido"
  )
  ) 

periodo <- as.integer(as_date("2025-01-31")-as_date("2025-01-15"))

data_1_sd <- data %>% 
  filter(quincena=="Actual", 
         detenido=="Sin detenido") %>% 
  group_by(fiscalia, detenido, 
           # fecha_inicio, 
           # fiscal,
           grupo
  ) %>% 
  summarise(media=n()/periodo) 


data_1_cd <- data %>% 
  filter(quincena=="Actual", 
         detenido=="Con detenido")%>% 
  group_by(fiscalia, detenido, 
           # fecha_inicio, 
           # fiscal,
           grupo)
  summarise(media=n()/periodo) 


data_1 <- bind_rows(data_1_sd, data_1_cd) #%>% 
  # rename("id_mp"=fiscal)


write.csv(data_1, "H:/Mi unidad/Evaluacion_agencias/bases/data_gr1_sin_mp.csv", row.names = F)
#info gráfica 2
data <- juntas %>% 
  filter(grepl(fiscalias_territoriales, fiscalia),
         !grepl("CJM|URI|AOP", fiscalia), 
         fiscalia!="STCMH") %>% 
  # filter(grepl(fiscalia_analisis, ct_inicio_ap)) %>% 
  mutate(fecha_inicio=as_date(fecha_de_inicio)) %>% 
  filter(fecha_inicio>="2024-01-01", 
         fecha_inicio<="2025-01-15"
  ) %>% 
  mutate(quincena=case_when(
    fecha_inicio>="2025-01-15" ~ "Actual",
    T ~ "Anterior"
  ), 
  detenido=case_when(
    id_ap %in% flagrancias$id_ap ~ "Con detenido", 
    T ~ "Sin detenido"
  )
  ) 

periodo <- as.integer(as_date("2025-01-31")-as_date("2025-01-15"))

floor_15_days <- function(date) {
  # Calcula el número de días desde una fecha de referencia (por ejemplo, "2024-01-01")
  dias_desde_referencia <- as.numeric(date - ymd("2024-01-01"))
  
  # Calcula el intervalo de 15 días
  intervalo <- floor(dias_desde_referencia / 15) * 15
  
  # Devuelve la fecha correspondiente al inicio del intervalo
  return(ymd("2024-01-01") + days(intervalo))
}

data_1_sd <- data %>% 
  filter(grepl(fiscalias_territoriales, fiscalia),
         !grepl("CJM|URI|AOP", fiscalia)) %>%
  filter(#quincena=="Actual", 
        detenido=="Sin detenido") %>% 
  group_by(fiscalia, detenido, 
           fecha_inicio
  ) %>% 
  summarise(media=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date("2024-01-01"), 
                                 as_date(fecha_fin), "1 day"
                                 ), 
           fiscalia=c(.$fiscalia), 
           detenido="Sin detenido", 
           fill=list(media=0)
           ) %>% 
  group_by(fiscalia, fecha_inicio=floor_date(fecha_inicio, "1 month"), 
           detenido) %>% 
  summarise(media=mean(media))


data_1_cd <- data %>%   
  filter(grepl(fiscalias_territoriales, fiscalia),
                               !grepl("CJM|URI|AOP", fiscalia)) %>% 
  filter(#quincena=="Actual", 
         detenido=="Con detenido")%>% 
  group_by(fiscalia, detenido, 
           fecha_inicio
  ) %>% 
  summarise(media=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date("2024-01-01"), 
                                 as_date(fecha_fin), "1 day"
  ), 
  fiscalia=c(.$fiscalia), 
  detenido="Con detenido", 
  fill=list(media=0)
  ) %>% 
  group_by(fiscalia, fecha_inicio=floor_date(fecha_inicio, "1 month"), 
           detenido) %>% 
  summarise(media=mean(media))


data_2 <- bind_rows(data_1_sd, data_1_cd) %>% 
  filter(grepl(fiscalias_territoriales, fiscalia),
         !grepl("CJM|URI|AOP", fiscalia))
  # rename("id_mp"=fiscal)

gr_2_cd <- data_2 %>% 
  filter(detenido=="Con detenido") %>% 
  ggplot(aes(fecha_inicio, media)) +
  geom_point(alpha=1.5) +
  geom_line(color="grey") +
  geom_smooth(se=F, color=colores[2]) + theme_light() +
  tema_fgj + tema_ppp +
  scale_x_date(date_labels = "%b\n%Y") +
  facet_wrap(detenido~fiscalia, scales = "free_y") +
  geom_vline(xintercept=as_date("2024-10-05"), 
             linetype="dashed", size=1, color="#800040"
  ) +
  scale_color_manual(values = colores)

gr_2_sd <- data_2 %>% 
  filter(detenido=="Sin detenido", 
         fecha_inicio>="2024-01-02", 
         fecha_inicio<="2025-01-20") %>% 
  ggplot(aes(fecha_inicio, media)) +
  geom_point(alpha=1.5) +
  geom_line(color="grey") +
  geom_smooth(se=F, color=colores[2]) + theme_light() +
  tema_fgj + tema_ppp +
  scale_x_date(date_labels = "%b\n%Y") +
  facet_wrap(detenido~fiscalia, scales = "free_y") +
  geom_vline(xintercept=as_date("2024-10-05"), 
             linetype="dashed", size=1, color="#800040"
  ) +
  scale_color_manual(values = colores)



ggsave(plot = gr_2_sd, 
       "gr_2_sd_260225.svg", width = 16, height = 7
       )

ggsave(plot = gr_2_cd, 
       "gr_2_cd_260225.svg", width = 16, height = 7
)

write.csv(data_2, "./bases/data_gr2.csv", row.names = F)


#info gráfica 3
#metemos campap
# campap <- readRDS("H:/Mi unidad/bases/campap_territoriales.rds")

campap <- readRDS("H:/Mi unidad/bases/campap_terri.rds")
# campap <- read_rds("campap.rds")

base <- incidencia$base %>% 
  mutate(campap=ifelse(id_ap %in% campap$id_ap, 1, 0)) %>% 
  left_join(campap %>% 
              select(-ctrluinv)
  )

data <- base %>% 
  # filter(grepl(fiscalia_analisis, ct_inicio_ap)) %>% 
  mutate(fecha_inicio=dmy(fecha_de_inicio)) %>% 
  filter(fecha_inicio>="2020-01-01", 
         fecha_inicio<="2025-01-31"
  ) %>% 
  mutate(quincena=case_when(
    fecha_inicio>="2025-01-15" ~ "Actual",
    T ~ "Anterior"
  ), 
  detenido=case_when(
    id_ap %in% flagrancias$id_ap ~ "Con detenido", 
    T ~ "Sin detenido"
  )
  ) 


data_co <- flagrancias %>% 
  filter(libertad_homologada=="Criterio de oportunidad", 
         fecha_inicio>="2020-01-01", 
         fecha_inicio<="2025-02-15", 
         subprocuraduria=="COORDINACIÓN GENERAL DE INVESTIGACIÓN TERRITORIAL") %>% 
  pull(id_ap)

fiscalias_territoriales <- paste(sep = "|",  "AZ", "BJ", "IZP", "IZC", "MH", "MIL",
                                 "CUJ", "COY", "CUH", "GAM", "MC", "TLH", "TLP", "VC", "XO", "AO")
# iniciadas <- iniciadas %>% get_fiscalia()
# total_agencia <- iniciadas %>% 
#   filter(grepl(fiscalias_territoriales, agencia_ini),
#          !grepl("CJM|URI|UAT|ADD|AOP", agencia_ini),
#   ) %>% pull(agencia_ini) %>% unique()


campap_total <- campap_total %>% drop_na(propuesta) %>% 
  mutate(propuesta=case_when(
    grepl("Penal",propuesta) ~ "NEAP", 
    T ~ "Archivo temporal"
  ),
    propuesta2=paste0(trimws(propuesta), " ", trimws(estatus)))

ord_lib <- readxl::read_excel("2018-2025_02_13 ORDENES APREHENSION.xlsx", 
                              sheet = "LIBRADAS") %>% clean_names()
ord_cum <- incidencia$ordenes

id_ord_lib <- ord_lib %>% 
  mutate(id_ap=as.integer(id_ap_uet)) %>% drop_na(id_ap) %>% 
  pull(id_ap)

id_ord_cum <- ord_cum %>% 
  mutate(id_ap=as.integer(id_carpeta_uet)) %>% drop_na(id_ap) %>% 
  pull(id_ap)

id_sen <- incidencia$sentencias %>% 
  mutate(id_ap=as.integer(id_ap2)) %>% drop_na(id_ap) %>% 
  pull(id_ap)

id_flagr <- flagrancias %>% 
  filter(vinculacion_a_proceso_por_persona==1) %>% pull(id_ap)

id_campap <- campap_total %>% 
  filter(estatus %in% c("Aprobada     ", "Pendiente    ")) %>% 
  drop_na(propuesta) %>% pull(id_ap)




data_estatus <- incidencia$base %>% 
  filter(year(fecha_inicio)>2019) %>% 
  # filter(grepl(fiscalias_territoriales, ct_inicio_ap),
  #                  !grepl("CJM|AOP", ct_inicio_ap)) %>% 
  mutate(estatus=case_when(
    id_ap %in% id_sen ~ "Sentencias", 
    id_ap %in% id_ord_cum ~ "Ordenes cumplidas", 
    id_ap %in% id_ord_lib ~ "Ordenes libradas", 
    id_ap %in% id_flagr ~ "Vinculado por flagrancias", 
    id_ap %in% data_co ~ "Criterio de oportunidad", 
    id_ap %in% id_campap ~ "CAMPAP", 
    T ~ "Rezago"
  )) %>% 
  mutate(detenido=case_when(
    id_ap %in% flagrancias$id_ap ~ "Con detenido", 
    T ~ "Sin detenido"
  ), 
  territoriales=case_when(
    grepl(fiscalias_territoriales, ct_inicio_ap) &
    !grepl("CJM|AOP", ct_inicio_ap) ~ 1, T ~0
  ))


data_estatus %>% 
  select(-resumen_ap) %>% 
  write.csv("H:/Mi unidad/Evaluacion_agencias/bases/data_estatus_230225.csv")


data_estatus %>% 
  filter(territoriales==1) %>% 
  filter(fecha_inicio<="2025-02-15") %>% 
  get_fiscalia() %>% 
  mutate(fiscalia=gsub("[-0-9]|UAT|URI|ADD", "", ct_inicio_ap)) %>% 
  filter(fiscalia!="STCMH") %>% 
  group_by(fecha_inicio=floor_date(fecha_inicio, "1 month"), 
           fiscalia
           ) %>% 
  summarise(Total=n(), 
            incompetencias_interna=sum(delito_especializada, 
                                       na.rm = T)
            ) %>% 
  write.csv("H:/Mi unidad/Evaluacion_agencias/bases/incompetencias_internas_250225.csv", 
            row.names = F)
  
# data_campap <- incidencia$base %>%
#   filter(grepl(fiscalias_territoriales, ct_inicio_ap),
#          !grepl("CJM|URI|AOP|STCMH", ct_inicio_ap),
#   ) %>% 
#   filter(fecha_inicio>="2024-12-15", 
#          fecha_inicio<="2025-01-15"
#          ) %>% 
#   mutate(campap=ifelse(id_ap %in% campap$id_ap, 1, 0)) %>% 
#   left_join(campap %>% 
#               select(id_ap, propuesta2)
#   ) %>% 
#   mutate(fiscalia=gsub("URI-|ADD-|UAT-|-|[0-9]", "", ct_inicio_ap)) %>% 
#   replace_na(list(propuesta2="Rezago")) %>% 
#   filter(fiscalia!="") %>% 
#   group_by(fiscalia, propuesta=propuesta2) %>% 
#   summarise(Total=n()) 
#   
# 
# data_3 <- bind_rows(data_campap, data_co)


write.csv(data_3, "H:/Mi unidad/Evaluacion_agencias/bases/data_gr3.csv", row.names = F)

##gráfica 4
data_libertades <- flagrancias %>% 
  filter(fecha_inicio>="2025-01-15", 
         fecha_inicio<="2025-01-31",
         libertad==1, 
         subprocuraduria=="COORDINACIÓN GENERAL DE INVESTIGACIÓN TERRITORIAL"
         ) %>% 
  filter(res_puestas=="1. Libertad MP", 
         !libertad_homologada %in% c("No se formuló imputación", "Acuerdo reparatorio", 
                                     "Pendientes por identificar")
  ) %>% 
  mutate(tipo=case_when(
    libertad_homologada %in% c("Artículo 140 CNPP", "Personas mayores de 65 años")~ "Artículo 140", 
    libertad_homologada %in% c("Falta de querella") ~"Falta de querella", 
    libertad_homologada %in% "Perdón de la víctima u ofendido" ~"Perdón",
    T ~ "Artículo 149"
  )) %>% 
  group_by(fiscalia=siglas_fiscalia_inicio, 
           tipo
           ) %>% 
  summarise(Total=n())


write.csv(data_libertades, "H:/Mi unidad/Evaluacion_agencias/bases/data_gr4.csv", row.names = F)


#gráfica 5
data_libertades_tiempo <- flagrancias %>% 
  filter(fecha_inicio>="2024-01-01", 
         fecha_inicio<="2025-01-31",
         libertad==1, 
        subprocuraduria=="COORDINACIÓN GENERAL DE INVESTIGACIÓN TERRITORIAL") %>% 
  filter(res_puestas=="1. Libertad MP", 
         !libertad_homologada %in% c("No se formuló imputación", "Acuerdo reparatorio", 
                                     "Pendientes por identificar", "Criterio de oportunidad")
  ) %>% 
  mutate(tipo=case_when(
    libertad_homologada %in% c("Artículo 140 CNPP", "Personas mayores de 65 años")~ "Artículo 140", 
    libertad_homologada %in% c("Falta de querella") ~"Falta de querella", 
    libertad_homologada %in% "Perdón de la víctima u ofendido" ~"Perdón",
    T ~ "Artículo 149"
  )) %>% 
  group_by(fiscalia=siglas_fiscalia_inicio, 
           tipo
  ) %>% 
  summarise(Total=n())


write.csv(data_libertades_tiempo, "H:/Mi unidad/Evaluacion_agencias/bases/data_gr5.csv", row.names = F)

#gráfica 6
#metemos el catalogo de masc
cat_masc <- readxl::read_excel("Delito_MASC.xlsx", sheet = "cat_masc")
masc <- flagrancias %>% 
  filter(mecanismo_alternativo==1) %>% 
  pull(id_ap)

#comparación con la fiscalía sin contar agencia principal
fiscalias_territoriales <- paste(sep = "|",  "AZ", "BJ", "IZP", "IZC", "MH", "MIL",
                                 "CUJ", "COY", "CUH", "GAM", "MC", "TLH", "TLP", "VC", "XO", "AO")
# iniciadas <- iniciadas %>% get_fiscalia()
# total_agencia <- iniciadas %>% 
#   filter(grepl(fiscalias_territoriales, agencia_ini),
#          !grepl("CJM|URI|UAT|ADD|AOP", agencia_ini),
#   ) %>% pull(agencia_ini) %>% unique()


data_masc <- incidencia$base %>% 
  filter(fecha_inicio>="2025-01-15", 
         fecha_inicio<="2025-01-31") %>% 
  filter(modalidad_delito %in% cat_masc$modalidad_delito) %>% 
  get_fiscalia() %>%
  filter(grepl(fiscalias_territoriales, agencia_ini),
         !grepl("CJM|URI|AOP", agencia_ini),
  ) %>% 
  mutate(masc=ifelse(id_ap %in% masc, 1, 0)) %>%  
  group_by("fiscalia"=fiscalia_ini, masc) %>% 
  summarise(Total=n())

#gráfica 7
data_acuerdo <- flagrancias %>% 
  filter(fecha_inicio>="2024-01-15", 
         fecha_inicio<="2025-01-31", 
         delito %in% cat_masc$modalidad_delito, 
         libertad==1, 
         subprocuraduria=="COORDINACIÓN GENERAL DE INVESTIGACIÓN TERRITORIAL"
         ) %>% 
  mutate(acuerdo_reparatorio=case_when(
    libertad_homologada=="Acuerdo reparatorio"~ "Acuerdo reparatorio", 
    T ~"No hubo acuerdo reparatorio"
  )) %>% 
  group_by("fiscalia"=siglas_fiscalia_inicio, 
           acuerdo_reparatorio) %>% 
  summarise(Total=n()) %>% 
  spread(acuerdo_reparatorio, Total, fill=0)


write.csv(data_acuerdo, "H:/Mi unidad/Evaluacion_agencias/bases/data_gr7.csv", row.names = F)

#gráfica 8


quitar_lib <- c("No se formuló imputación", "Acuerdo reparatorio", 
                "Pendientes por identificar", 
                "Hechos no son constitutivos de delito", 
                "Criterio de oportunidad"
)
data_8 <- flagrancias %>% 
  filter(
    fecha_inicio>="2025-01-01", 
    fecha_inicio<="2025-01-15", 
    subprocuraduria=="COORDINACIÓN GENERAL DE INVESTIGACIÓN TERRITORIAL",
    siglas_fiscalia_inicio!="FIDAMPU",
    !(delito %in% cat_masc$modalidad_delito)
       
         ) %>% 
  filter(  !libertad_homologada %in% c("No se formuló imputación", "Acuerdo reparatorio", 
                                       "Pendientes por identificar", "Criterio de oportunidad")) %>% 
  filter(libertad==0) %>% 
  group_by(fiscalia=siglas_fiscalia_inicio, 
           legal=ifelse(legalidad_de_la_detencion==1, "Legal", "No legal")) %>% 
  summarise(Total=n()) %>% 
  spread(legal, Total, fill=0)


write.csv(data_8, "H:/Mi unidad/Evaluacion_agencias/bases/data_gr8.csv", row.names = F)

#gráfica 9
data_9 <- flagrancias %>% 
  filter(
    fecha_inicio>="2019-01-01", 
    fecha_inicio<="2025-01-31", 
    subprocuraduria=="COORDINACIÓN GENERAL DE INVESTIGACIÓN TERRITORIAL",
    siglas_fiscalia_inicio!="FIDAMPU",
    !(delito %in% cat_masc$modalidad_delito)
    
  ) %>% 
  filter(  !libertad_homologada %in% c("No se formuló imputación", "Acuerdo reparatorio", 
                                       "Pendientes por identificar", "Criterio de oportunidad")) %>% 
  filter(judicializacion==1) %>% 
  mutate(no_legal=ifelse(legalidad_de_la_detencion==1, 0, 1), 
         legal=ifelse(legalidad_de_la_detencion==1, 1, 0),
         vinculacion=case_when(
           legal==1 &
           vinculacion_a_proceso_por_persona==1 ~ 1,
           T ~0),
         no_vinculacion=case_when(
           legal==1 &
             vinculacion_a_proceso_por_persona==0 ~ 1,
           T ~0) 
         ) %>% 
  group_by(fecha_inicio=floor_date(fecha_inicio, "1 month"),
    fiscalia=siglas_fiscalia_inicio) %>% 
  summarise(
    puestas=n(),
    legal=sum(legal, na.rm = T), 
    no_legal=sum(no_legal, na.rm = T), 
    
    vinculacion=sum(vinculacion, na.rm = T),
    no_vinculacion=sum(no_vinculacion, na.rm = T)
            ) %>% ungroup() %>% 
  mutate(prop_legal=no_legal/puestas,
         prop_vinculacion=vinculacion/puestas
         ) %>% select(fecha_inicio, fiscalia, contains("prop")) %>%
  gather(tipo, Total, prop_legal:prop_vinculacion) %>%
  mutate(tipo=gsub("prop_", "", tipo),
         tipo=ifelse(tipo=="legal", "No legal", tipo),
         fiscalia=substr(fiscalia, 3,nchar(fiscalia))
         )


gr_9 <- data_9 %>%
  mutate(fiscalia=factor(fiscalia, 
                         levels=c("CUH", "CUJ", "IZC", "MC", "IZP", "VC",
                                  "GAM", "BJ", "MH", "TLH", "AO", "AZ", "MA","TLP","COY","XOC"))) %>% 
  # filter(detenido=="Sin detenido", 
  #        fecha_inicio>="2024-01-02", 
  #        fecha_inicio<="2025-01-20") %>% 
  ggplot(aes(fecha_inicio, Total, color=tipo)) +
  geom_point(alpha=.2) +
  # geom_line(color="grey") +
  geom_smooth(se=F) + theme_light() +
  tema_fgj + tema_ppp +
  geom_vline(xintercept=as_date("2024-10-05"), 
             linetype="dashed", size=1, color="#800040"
             ) +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y") +
  facet_wrap(.~fiscalia, scales = "free_y") +
  scale_color_manual(values = colores)



ggsave(plot = gr_9, 
       "gr_no_legal_vinc_facet_hist_250225.svg", width = 16, height = 7
)


  # summarise(legal=sum(leg))
  # spread(legal, Total, fill=0)

flagrancias %>% 
  filter(
    fecha_inicio>="2019-01-01", 
    fecha_inicio<="2025-01-31", 
    subprocuraduria=="COORDINACIÓN GENERAL DE INVESTIGACIÓN TERRITORIAL",
    siglas_fiscalia_inicio!="FIDAMPU",
    !(delito %in% cat_masc$modalidad_delito)
    
  ) %>% 
  filter(  !libertad_homologada %in% c("No se formuló imputación", "Acuerdo reparatorio", 
                                       "Pendientes por identificar", "Criterio de oportunidad")) %>% 
  filter(judicializacion==1) %>% 
  mutate(no_legal=ifelse(legalidad_de_la_detencion==1, 0, 1), 
         legal=ifelse(legalidad_de_la_detencion==1, 1, 0),
         vinculacion=case_when(
           legal==1 &
             vinculacion_a_proceso_por_persona==1 ~ 1,
           T ~0),
         no_vinculacion=case_when(
           legal==1 &
             vinculacion_a_proceso_por_persona==0 ~ 1,
           T ~0) 
  ) %>% 
  group_by(fecha_inicio=floor_date(fecha_inicio, "1 month"),
           fiscalia=siglas_fiscalia_inicio) %>% 
  summarise(
    puestas=n(),
    legal=sum(legal, na.rm = T), 
    no_legal=sum(no_legal, na.rm = T), 
    
    vinculacion=sum(vinculacion, na.rm = T),
    no_vinculacion=sum(no_vinculacion, na.rm = T)
  ) %>% ungroup() %>% 
  mutate(prop_legal=no_legal/puestas,
         prop_vinculacion=vinculacion/puestas
  ) %>% 
  write.csv("H:/Mi unidad/Evaluacion_agencias/bases/data_gr9_250225.csv",
            row.names = F)

#gráfica 10
data_10 <- flagrancias %>% 
  filter(
    fecha_inicio>="2024-01-01", 
    fecha_inicio<="2025-01-15", 
    subprocuraduria=="COORDINACIÓN GENERAL DE INVESTIGACIÓN TERRITORIAL",
    siglas_fiscalia_inicio!="FIDAMPU",
    !(delito %in% cat_masc$modalidad_delito)
    
  ) %>% 
  filter(  !libertad_homologada %in% c("No se formuló imputación", "Acuerdo reparatorio", 
                                       "Pendientes por identificar", "Criterio de oportunidad")) %>% 
  filter(legalidad_de_la_detencion==1) %>%
  mutate(periodo=case_when(
    fecha_inicio>="2025-01-01" ~ "Quincen actual", 
    T ~"Histórico"
  )) %>% 
  group_by(periodo, fiscalia=siglas_fiscalia_inicio, 
           vinculacion=ifelse(vinculacion_a_proceso_por_persona==1, "Vinculación", "No Vinculación")) %>% 
  summarise(Total=n()) %>% 
  spread(vinculacion, Total, fill=0)


write.csv(data_10, "H:/Mi unidad/Evaluacion_agencias/bases/data_gr10.csv", row.names = F)

# Gráfica 11
ord_lib <- readxl::read_excel("2018-2025_02_13 ORDENES APREHENSION.xlsx", 
                              sheet = "LIBRADAS") %>% clean_names()
ord_cum <- incidencia$ordenes

id_ord_lib <- ord_lib %>% 
  mutate(id_ap=as.integer(id_ap_uet)) %>% drop_na(id_ap) %>% 
  pull(id_ap)

id_ord_cum <- ord_cum %>% 
  mutate(id_ap=as.integer(id_carpeta_uet)) %>% drop_na(id_ap) %>% 
  pull(id_ap)

id_sen <- incidencia$sentencias %>% 
  mutate(id_ap=as.integer(id_ap2)) %>% drop_na(id_ap) %>% 
  pull(id_ap)

data_11 <- incidencia$base %>% 
  filter(fecha_inicio>="2019-01-01") %>% 
  filter(!id_ap %in% flagrancias$id_ap) %>% #get_fiscalia() %>% 
  filter(grepl(fiscalias_territoriales, ct_inicio_ap),
          !grepl("CJM|URI|AOP|AAE|STCMH", ct_inicio_ap))  %>% 
  mutate(estatus=case_when(
    id_ap %in% id_ord_lib~ "Orden librada",
    id_ap %in% id_ord_cum ~ "Orden cumplida", 
    id_ap %in% id_sen ~ "Sentencia",
    T ~ "Pendiente")) %>% 
  mutate(fiscalia=gsub("URI-|ADD-|UAT-|-|[0-9]", "", ct_inicio_ap)) %>% 
  # mutate(fiscalia=case_when(
  #   fiscalia=="ZP"~ "IZP",
  #   fiscalia=="ZC"~ "IZC",
  #   fiscalia=="TL"~ "TLH",
  #   fiscalia=="TP"~ "TLP",
  # ))
  group_by(estatus, fiscalia) %>% 
  summarise(Total=n()) %>% 
  spread(estatus, Total, fill=0)

write.csv(data_11, "H:/Mi unidad/Evaluacion_agencias/bases/data_gr11.csv", row.names = F)

#gráfica 12
#no tenemos info de citatorios

#gráfica 13
#no tenemos info de citatorios

#gráfica 14
data_14 <- flagrancias %>% 
  filter(vinculacion_a_proceso_por_persona==1, 
         subprocuraduria=="COORDINACIÓN GENERAL DE INVESTIGACIÓN TERRITORIAL",
         siglas_fiscalia_inicio!="FIDAMPU") %>% 
  group_by(fiscalia=siglas_fiscalia_inicio) %>% 
  summarise(acuerdo_reparatorio=sum(acuerdo_reparatorio, na.rm=T), 
            suspension_condicional=sum(suspension_condicional, na.rm = T)
            )

write.csv(data_14, "H:/Mi unidad/Evaluacion_agencias/bases/data_gr14.csv", row.names = F)

#gráfica 15

data_15 <- incidencia$base %>% 
  filter(fecha_inicio>="2018-01-01") %>% 
  filter(grepl(fiscalias_territoriales, ct_inicio_ap),
         !grepl("CJM|URI|AOP|AAE|STCMH", ct_inicio_ap))  %>% 
  mutate(estatus=case_when(
    id_ap %in% id_ord_lib~ "Orden librada",
    id_ap %in% id_ord_cum ~ "Orden cumplida", 
    id_ap %in% id_sen ~ "Sentencia",
    T ~ "Pendiente")) %>% 
  mutate(fiscalia=gsub("URI-|ADD-|UAT-|-|[0-9]", "", ct_inicio_ap)) %>% 
  # mutate(fiscalia=case_when(
  #   fiscalia=="ZP"~ "IZP",
  #   fiscalia=="ZC"~ "IZC",
  #   fiscalia=="TL"~ "TLH",
  #   fiscalia=="TP"~ "TLP",
  # ))
  group_by(estatus, fiscalia) %>% 
  summarise(Total=n()) %>% 
  spread(estatus, Total, fill=0)

write.csv(data_15, "H:/Mi unidad/Evaluacion_agencias/bases/data_gr15.csv", row.names = F)

#gráfica 16


nombres_desc <- paste("DESCONO", "ANONIM", "NNN", "IDENTID",
                      "MASCU", "FEMENI",
                      sep = "|")


punibles_territoriales <- readRDS("H:/Mi unidad/bases/punibles_territoriales.rds")
"\\"

nombres_desc <- paste("DESCONO", "ANONIM", "NNN", "IDENTID",
                      "MASCU", "FEMENI", "N.N.", '\\"N""N"',
                      sep = "|")

punibles_territoriales <- punibles_territoriales %>% 
  mutate(nombre_completo=paste0(nombre, apellidoPaterno, apellidoMaterno), 
         nombre_completo=gsub(" ", "", nombre_completo))

nombres_conocidos <- punibles_territoriales %>% 
  filter(!grepl(nombres_desc, nombre_completo))

id_conocidos <- nombres_conocidos %>% pull(idAveriguacionPrevia)

data_16 <- incidencia$base %>% 
  filter(fecha_inicio>="2018-01-01") %>% 
  filter(grepl(fiscalias_territoriales, ct_inicio_ap),
         !grepl("CJM|AOP|AAE|STCMH", ct_inicio_ap)
         )  %>% 
  mutate(detenido=case_when(
    id_ap %in% flagrancias$id_ap ~ "Con detenido", T ~ "Sin detenido"
  )) %>% 
  # filter(!id_ap %in% id_campap) %>%
  # filter(!id_ap %in% id_sen) %>% 
  filter(id_ap %in% id_conocidos) %>% 
  filter(detenido=="Sin detenido") %>% 
  # filter(!(delito %in% cat_masc$modalidad_delito)) %>% 
  mutate(robo_violencia=case_when(
    grepl("ROBO", modalidad_delito) & 
      grepl("CON VIOLENC|C/V", modalidad_delito) ~ 1, T ~0
  )) %>% 
  filter(grepl("LESIONES INTENCIO|LESIONES DOLOS|DESPOJ", modalidad_delito) |
           robo_violencia==1
           ) %>% 
  # filter(grepl("INTENCIO|DOLOS", modalidad_delito)) %>% 
  mutate(estatus=case_when(
    
    id_ap %in% id_ord_cum ~ "Orden cumplida",
    id_ap %in% id_ord_lib~ "Orden librada",
    id_ap %in% id_sen ~ "Sentencia",
    id_ap %in% id_campap ~ "Campap",
    T ~ "Pendiente")) %>% 
  filter(estatus %in% c("Orden cumplida", "Orden librada")) %>% 
  mutate(fiscalia=gsub("URI-|ADD-|UAT-|-|[0-9]", "", ct_inicio_ap)) #%>% 
  # mutate(fiscalia=case_when(
  #   fiscalia=="ZP"~ "IZP",
  #   fiscalia=="ZC"~ "IZC",
  #   fiscalia=="TL"~ "TLH",
  #   fiscalia=="TP"~ "TLP",
  # ))
  # group_by(estatus, fiscalia, 
  #          fecha_inicio=floor_date(fecha_inicio, "month")) %>% 
  # summarise(Total=n()) 

write.csv(data_16, "H:/Mi unidad/Evaluacion_agencias/bases/data_gr16.csv", row.names = F)


gr_16 <- data_16 %>% 
  group_by(año=year(fecha_inicio), 
           estatus, fiscalia
           ) %>% 
  summarise(Total=sum(Total), .groups = "drop") %>% 
  group_by(año, fiscalia) %>% mutate(Porcentaje=Total/sum(Total)) %>% ungroup() %>% 
  filter(estatus!="Pendiente") %>% 
  ggplot(aes(año, Porcentaje, fill=estatus)) +
 geom_col() + theme_light() +
  tema_fgj + tema_ppp +
  geom_text_repel(aes(label=percent(Porcentaje, .01)),
            position = position_stack(vjust = .8),
            size=4.5, fontface="bold", direction = "y"
            ) +
  facet_wrap(.~fiscalia) +
  scale_y_continuous(labels = percent) +
  # scale_x_date(date_labels = "%b\n%Y") +
  facet_wrap(.~fiscalia) +
  scale_fill_manual(values = colores[7:8])


ggsave(plot = gr_16, 
       "gr_ordenes_19225.svg", width = 18, height = 8
       )




id_data_16 <- incidencia$base %>% 
  filter(fecha_inicio>="2018-01-01") %>% 
  filter(grepl(fiscalias_territoriales, ct_inicio_ap),
         !grepl("CJM|AOP|AAE|STCMH", ct_inicio_ap)
  )  %>% 
  mutate(detenido=case_when(
    id_ap %in% flagrancias$id_ap ~ "Con detenido", T ~ "Sin detenido"
  )) %>% 
  # filter(!id_ap %in% id_campap) %>%
  # filter(!id_ap %in% id_sen) %>% 
  filter(id_ap %in% id_conocidos) %>% 
  filter(detenido=="Sin detenido") %>% 
  # filter(!(delito %in% cat_masc$modalidad_delito)) %>% 
  mutate(robo_violencia=case_when(
    grepl("ROBO", modalidad_delito) & 
      grepl("CON VIOLENC|C/V", modalidad_delito) ~ 1, T ~0
  )) %>% 
  filter(grepl("LESIONES INTENCIO|LESIONES DOLOS|DESPOJ", modalidad_delito) |
           robo_violencia==1
  ) %>% 
  # filter(grepl("INTENCIO|DOLOS", modalidad_delito)) %>% 
  mutate(estatus=case_when(
    
    id_ap %in% id_ord_cum ~ "Orden cumplida",
    id_ap %in% id_ord_lib~ "Orden librada",
    id_ap %in% id_sen ~ "Sentencia",
    id_ap %in% id_campap ~ "Campap",
    T ~ "Pendiente")) %>% 
  filter(estatus %in% c("Orden cumplida", "Orden librada", "Pendiente")) %>% 
  pull(id_ap)


ordenes <- incidencia$ordenes

ordenes <- ordenes %>% 
  mutate(id_ap=as.integer(id_carpeta_uet), 
         fecha=as_date(fecha_cumplida))
ord_cumplidas <- ordenes %>% 
  mutate(id_ap=as.integer(id_carpeta_uet), 
         fecha=as_date(fecha_cumplida)) %>% 
  left_join(data_16 %>% select(id_ap, fiscalia), by="id_ap") %>% 
  drop_na(fiscalia) %>% 
  group_by(fiscalia, año=year(fecha)) %>% 
  summarise("Ordenes cumplidas"=n())

ord_lib <- ord_lib %>% 
  mutate(id_ap=as.integer(id_ap_uet), 
         fecha=as_date(fecha_librada))

filtro_libradas <- ord_lib %>% 
  mutate(id_ap=as.integer(id_ap_uet), 
         fecha=as_date(fecha_librada)) %>% 
  filter(!id_ap %in% ordenes$id_ap[!is.na(ordenes$id_ap)]) %>% 
  left_join(data_16 %>% select(id_ap, fiscalia), by="id_ap")

ord_libradas <- ord_lib %>% 
  mutate(id_ap=as.integer(id_ap_uet), 
         fecha=as_date(fecha_librada)) %>% 
  filter(!id_ap %in% ordenes$id_ap[!is.na(ordenes$id_ap)]) %>% 
  left_join(data_16 %>% select(id_ap, fiscalia), by="id_ap") %>% 
  drop_na(fiscalia) %>% 
  group_by(fiscalia, año=year(fecha)) %>% 
  summarise("Ordenes libradas"=n())

total_carpetas_16 <- incidencia$base %>% 
  filter(fecha_inicio>="2018-01-01") %>% 
  filter(grepl(fiscalias_territoriales, ct_inicio_ap),
         !grepl("CJM|AOP|AAE|STCMH", ct_inicio_ap)
  )  %>% 
  mutate(detenido=case_when(
    id_ap %in% flagrancias$id_ap ~ "Con detenido", T ~ "Sin detenido"
  )) %>% 
  # filter(!id_ap %in% id_campap) %>%
  # filter(!id_ap %in% id_sen) %>% 
  filter(id_ap %in% id_conocidos) %>% 
  filter(detenido=="Sin detenido") %>% 
  # filter(!(delito %in% cat_masc$modalidad_delito)) %>% 
  mutate(robo_violencia=case_when(
    grepl("ROBO", modalidad_delito) & 
      grepl("CON VIOLENC|C/V", modalidad_delito) ~ 1, T ~0
  )) %>% 
  filter(grepl("LESIONES INTENCIO|LESIONES DOLOS|DESPOJ", modalidad_delito) |
           robo_violencia==1
  ) %>% 
  # filter(grepl("INTENCIO|DOLOS", modalidad_delito)) %>% 
  mutate(estatus=case_when(
    
    id_ap %in% id_ord_cum ~ "Orden cumplida",
    id_ap %in% id_ord_lib~ "Orden librada",
    id_ap %in% id_sen ~ "Sentencia",
    id_ap %in% id_campap ~ "Campap",
    T ~ "Pendiente")) %>% 
  filter(estatus %in% c("Orden cumplida", "Orden librada", "Pendiente")) %>% 
  mutate(fiscalia=gsub("URI-|ADD-|UAT-|-|[0-9]", "", ct_inicio_ap)) %>% 
  group_by(año=year(fecha_inicio), fiscalia) %>% 
  summarise(Carpetas=n())
  
total_ordenes <- total_carpetas_16 %>%
  ungroup() %>% 
  left_join(ord_cumplidas) %>% 
  left_join(ord_libradas) %>% 
  replace_na(list(`Ordenes cumplidas`=0, `Ordenes libradas`=0)) %>% 
  gather(estatus, Total, Carpetas:`Ordenes libradas`) %>% 
  group_by(año, fiscalia) %>% 
  mutate(Porcentaje=Total/sum(Total))


total_ordenes %>% 
  write.csv("tabla_carpetas_ordenes_200225.csv", row.names = F)

gr_16 <- total_ordenes %>% 
  filter(estatus!="Carpetas") %>% 
  ggplot(aes(año, Porcentaje, fill=estatus)) +
  geom_col() + theme_light() +
  tema_fgj + tema_ppp +
  geom_text_repel(data=. %>% 
                    filter(Total>0),
                    aes(label=percent(Porcentaje, .1)),
                  position = position_stack(vjust = .8),
                  size=4.5, fontface="bold", direction = "y"
  ) +
  facet_wrap(.~fiscalia) +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(breaks = c(2018:2025), labels = c(2018:2025)) +
  # scale_x_date(date_labels = "%b\n%Y") +
  facet_wrap(.~fiscalia) +
  scale_fill_manual(values = colores[7:8]) +
  theme(axis.text.x = element_text(size=14))


ggsave(plot = gr_16, 
       "gr_ordenes_200225.svg", width = 18, height = 8
)

