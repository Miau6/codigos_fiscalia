####
setwd("C:/Users/mauri/OneDrive/Documentos/R/fiscalia/plan_politica_criminal")
#plan de política criminal
incidencia <- read_rds("bases_uet_2025-01-30.rds")

base <- incidencia$base

tablas_especialies <- list()
grafica_tendencia <- function(datos=data, 
                              fecha_inicio_global=fecha_inicio_global,
                              fecha_lim=fecha_lim, 
                              delito=delito){
  gr_trend <- datos %>% 
    # filter(delito=="ROBO DE VEHÍCULO CON Y SIN VIOLENCIA", 
    #        #grepl("CON VIOLE|C/V", modalidad_delito),
    #        year(fecha_inicio) %in% c(2020:2025)) %>% 
    group_by(fecha_inicio) %>% 
    summarise(Total=n()) %>% ungroup() %>% 
    complete(fecha_inicio=seq.Date(as_date(fecha_inicio_global), 
                                   as_date(fecha_lim), "1 day"), 
             fill=list(Total=0)
    ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 month")) %>% 
    summarise(Total=mean(Total)) %>% 
    ggplot(aes(fecha_inicio, Total)) +
    geom_point(alpha=.7, color=colores[1]) +
    geom_smooth(se=F, color=colores[7]) + theme_light() + tema_fgj +
    # geom_smooth(se=F, color=colores[2], method = "lm") +
    scale_x_date(date_labels = "%b-%y", date_breaks = "3 month") +
    labs(x="Fecha de inicio", y="Promedio diario", 
         title = paste0("Promedio diario de carpetas por ", delito), 
         subtitle = paste0("Desde ",  format(as_date(fecha_inicio_global), "%B de %Y"), " a ",
                           format(as_date(fecha_lim), "%B de %Y"))) +
    theme(axis.text.x = element_text(angle = 90)) 
  
  return(gr_trend)
}

fecha_lim <- "2024-12-31"
fecha_inicio_global <- "2019-01-01"
data <- base %>% filter(fecha_inicio<=fecha_lim, fecha_inicio>=fecha_inicio_global)

#incidencia total


gr_total <- plot_anual(base = data, tipo = c("incidencia"), nombre = "Incidencia total", fecha_inicio_global, 
           fecha_lim, comparacion_mensual = 0)

ggsave(plot = gr_total, 
       "incidencia_total.svg", width = 12, height = 7
       )


#alto impacto
gr_alto_impacto <- grafica_tendencia(datos = data %>% filter(tipo_impacto=="ALTO IMPACTO"), 
                  fecha_inicio_global = fecha_inicio_global, fecha_lim = fecha_lim,
                  delito = "Alto impacto")

gr_bajo_impacto <- grafica_tendencia(datos = data %>% filter(tipo_impacto=="BAJO IMPACTO"), 
                                     fecha_inicio_global = fecha_inicio_global, fecha_lim = fecha_lim,
                                     delito = "Bajo impacto")

ggsave(plot = gr_alto_impacto, 
       "gr_alto_impacto.svg", width = 12, height = 6
       )

ggsave(plot = gr_bajo_impacto, 
       "gr_bajo_impacto.svg", width = 12, height = 6
)

#Homicidios dolosos de alto impacto
hom_contextual <- read.csv("homicidios_contextual.csv")
hom <- data %>% filter(delito=="HOMICIDIO DOLOSO", 
                       grepl("HOM", modalidad_delito), 
                       fecha_inicio>="2022-01-01"
                       ) %>% 
  left_join(hom_contextual %>% 
              select(-fecha_de_inicio), by="id_ap") 

gr_trend_hom <- hom %>% 
  # filter(delito=="ROBO DE VEHÍCULO CON Y SIN VIOLENCIA", 
  #        #grepl("CON VIOLE|C/V", modalidad_delito),
  #        year(fecha_inicio) %in% c(2020:2025)) %>% 
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date("2022-01-01"), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(hom$tipo),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 month"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  ggplot(aes(fecha_inicio, Total, color=tipo)) +
  geom_point(alpha=.7#, color=colores[1]
             ) +
  geom_smooth(se=F#, color=colores[7]
              ) + theme_light() + tema_fgj +
  # geom_smooth(se=F, color=colores[2], method = "lm") +
  scale_x_date(date_labels = "%b-%y", date_breaks = "3 month") +
  labs(x="Fecha de inicio", y="Promedio diario", 
       title = paste0("Promedio diario de carpetas por homicidio según clasificación"), 
       subtitle = paste0("Desde ",  format(as_date("2022-01-01"), "%B de %Y"), " a ",
                         format(as_date(fecha_lim), "%B de %Y"))) +
  scale_color_manual(values = colores[2:1]) +
  theme(axis.text.x = element_text(angle = 90), 
        legend.position = "bottom"
        )


ggsave(plot = gr_trend_hom, 
       "gr_hom_tipo.png", width = 12, height = 6
)

tablas_especialies[[1]] <-  hom %>% 
  # filter(delito=="ROBO DE VEHÍCULO CON Y SIN VIOLENCIA", 
  #        #grepl("CON VIOLE|C/V", modalidad_delito),
  #        year(fecha_inicio) %in% c(2020:2025)) %>% 
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date("2022-01-01"), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(hom$tipo),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 month"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  mutate(tipo_delito="Homicidio doloso")
#lesiones
gr_lesiones <- grafica_tendencia(datos = data %>% filter(delito=="LESIONES DOLOSAS POR DISPARO DE ARMA DE FUEGO"), 
                                     fecha_inicio_global = fecha_inicio_global, fecha_lim = fecha_lim,
                                     delito = "Lesiones dolosas por arma de fuego")

ggsave(plot = gr_lesiones, 
       "gr_lesiones.svg", width = 12, height = 6
)

#extorsiones
entrevistas <- readRDS("C:/Users/mauri/Downloads/entrevistas_extorsion.rds")

llamada <- paste(sep="|", "LLAMADA", "MENSAJE", "CELULAR", "TELEFONO", "T?LEFONO", "VOZ", 
                 "WHATSAPP", "WHATS", "REDES S", "MENSAJES", "CORREO", "MAIL", "EMAIL", 
                 "MESAJE", "LAMADAS", "TELÉFONO", "FACEBO", "FEISBU", "INSTAGR", "TINDER",
                 "GRINDER")

presencial <- paste(sep="|", "ACUDIERON", "INGRESARON A", "MI DOMICILIO", 
                    "LUGAR DE TRABAJO", "AMEDRENTA", "MI NEGOCIO", "SOBRE AMARRILLO", 
                    "CORTINA", "PORTABAN", "SE PRESENTARON", "UNA NOTA", "JEFE", 
                    "COMERCIANTES", "LLEGAN", "LOCAL", "DUEÑO"
                    
)
entrevistas <- entrevistas %>% 
  mutate(tipo=case_when(
    grepl(llamada, resumen_ap) ~ "Virtual", 
    grepl(presencial, resumen_ap) ~ "Presencial", T ~ "Otros"
  ))


entrevistas2 <- entrevistas2 %>% 
  mutate(tipo=case_when(
    grepl(llamada, declaracion) ~ "Virtual", 
    grepl(presencial, declaracion) ~ "Presencial", T ~ "Otros"
  ))

id_Presencial <- entrevistas2 %>% filter(tipo=="Presencial") %>% 
  pull(idAveriguacionPrevia)
id_virtual <- entrevistas2 %>% filter(tipo=="Virtual") %>% 
  pull(idAveriguacionPrevia)

entrevistas <- entrevistas %>% 
  mutate(tipo_extorsion=case_when(
    id_ap %in% id_Presencial ~ "Presencial", 
    tipo=="Presencial" ~ "Presencial",
    id_ap %in% id_virtual ~ "Virtual", 
    tipo=="Virtual" ~ "Virtual", 
    T ~ "Otros"
    
  ))

extorsion <- entrevistas %>% 
  filter(fecha_inicio>=fecha_inicio_global)


gr_trend_ext <- extorsion %>% 
  # filter(delito=="ROBO DE VEHÍCULO CON Y SIN VIOLENCIA", 
  #        #grepl("CON VIOLE|C/V", modalidad_delito),
  #        year(fecha_inicio) %in% c(2020:2025)) %>% 
  filter(fecha_inicio<=fecha_lim) %>% 
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date(fecha_inicio_global), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(extorsion$tipo_extorsion),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 month"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  ggplot(aes(fecha_inicio, Total, color=tipo)) +
  geom_point(alpha=.7#, color=colores[1]
  ) +
  geom_smooth(se=F#, color=colores[7]
  ) + theme_light() + tema_fgj +
  # geom_smooth(se=F, color=colores[2], method = "lm") +
  scale_x_date(date_labels = "%b-%y", date_breaks = "3 month") +
  labs(x="Fecha de inicio", y="Promedio diario", 
       title = paste0("Promedio diario de carpetas por extorsión según tipo"), 
       subtitle = paste0("Desde ",  format(as_date(fecha_inicio_global), "%B de %Y"), " a ",
                         format(as_date(fecha_lim), "%B de %Y"))) +
  scale_color_manual(values = colores[3:1]) +
  theme(axis.text.x = element_text(angle = 90), 
        legend.position = "bottom"
  )


ggsave(plot = gr_trend_ext, 
       "gr_trend_ext.svg", width = 12, height = 6
)

#tentativa de homicidio
gr_tenta_hom <- grafica_tendencia(datos = data %>% 
                                   filter(modalidad_delito %in% c( 
                                                                  "TENTATIVA DE HOMICIDIO"
                                                                  )), 
                                 fecha_inicio_global = fecha_inicio_global, fecha_lim = fecha_lim,
                                 delito = "Tentativa de homicidio")

ggsave(plot = gr_tenta_hom, 
       "gr_tenta_hom.svg", width = 12, height = 6
)
#robo de vehículo
gr_coche <- data %>% 
  filter(delito=="ROBO DE VEHÍCULO CON Y SIN VIOLENCIA") %>% 
  mutate(tipo=case_when(
    grepl("CON VIOLEN|C/V", modalidad_delito) ~ "Con violencia", T ~ "Sin violencia"
  )) %>% 
  # filter(delito=="ROBO DE VEHÍCULO CON Y SIN VIOLENCIA", 
  #        #grepl("CON VIOLE|C/V", modalidad_delito),
  #        year(fecha_inicio) %in% c(2020:2025)) %>% 
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date(fecha_inicio_global), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=c("Con violencia", "Sin violencia"),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 month"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  ggplot(aes(fecha_inicio, Total, color=tipo)) +
  geom_point(alpha=.7#, color=colores[1]
  ) +
  geom_smooth(se=F#, color=colores[7]
  ) + theme_light() + tema_fgj +
  # geom_smooth(se=F, color=colores[2], method = "lm") +
  scale_x_date(date_labels = "%b-%y", date_breaks = "3 month") +
  labs(x="Fecha de inicio", y="Promedio diario", 
       title = paste0("Promedio diario de carpetas por robo de vehículo por tipo"), 
       subtitle = paste0("Desde ",  format(as_date(fecha_inicio_global), "%B de %Y"), " a ",
                         format(as_date(fecha_lim), "%B de %Y"))) +
  scale_color_manual(values = colores[2:1]) +
  theme(axis.text.x = element_text(angle = 90), 
        legend.position = "bottom"
  )


ggsave(plot = gr_coche, 
       "./graficas_incidencia/especiales/gr_coche.png", width = 12, height = 6
)


tablas_especialies[[2]] <- data %>% 
  filter(delito=="ROBO DE VEHÍCULO CON Y SIN VIOLENCIA") %>% 
  mutate(tipo=case_when(
    grepl("CON VIOLEN|C/V", modalidad_delito) ~ "Con violencia", T ~ "Sin violencia"
  )) %>% 
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date("2022-01-01"), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(hom$tipo),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 month"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  mutate(tipo_delito="Robo de vehículo")
###robo a casa habitación
gr_casa_tipo <- data %>% 
  filter(grepl("ROBO A CASA", modalidad_delito)) %>% 
  mutate(tipo=case_when(
    grepl("CON VIOLEN|C/V", modalidad_delito) ~ "Con violencia", T ~ "Sin violencia"
  )) %>% 
  # filter(delito=="ROBO DE VEHÍCULO CON Y SIN VIOLENCIA", 
  #        #grepl("CON VIOLE|C/V", modalidad_delito),
  #        year(fecha_inicio) %in% c(2020:2025)) %>% 
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date(fecha_inicio_global), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=c("Con violencia", "Sin violencia"),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 month"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  ggplot(aes(fecha_inicio, Total, color=tipo)) +
  geom_point(alpha=.7#, color=colores[1]
  ) +
  geom_smooth(se=F#, color=colores[7]
  ) + theme_light() + tema_fgj +
  # geom_smooth(se=F, color=colores[2], method = "lm") +
  scale_x_date(date_labels = "%b-%y", date_breaks = "3 month") +
  labs(x="Fecha de inicio", y="Promedio diario", 
       title = paste0("Promedio diario de carpetas por robo de casa habitación por tipo"), 
       subtitle = paste0("Desde ",  format(as_date(fecha_inicio_global), "%B de %Y"), " a ",
                         format(as_date(fecha_lim), "%B de %Y"))) +
  scale_color_manual(values = colores[2:1]) +
  theme(axis.text.x = element_text(angle = 90), 
        legend.position = "bottom"
  )


ggsave(plot = gr_casa_tipo, 
       "./graficas_incidencia/especiales/gr_casa.png", width = 12, height = 6
)

tablas_especialies[[3]] <- data %>% 
  filter(grepl("ROBO A CASA", modalidad_delito)) %>% 
  mutate(tipo=case_when(
    grepl("CON VIOLEN|C/V", modalidad_delito) ~ "Con violencia", T ~ "Sin violencia"
  )) %>% 
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date("2022-01-01"), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(hom$tipo),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 month"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  mutate(tipo_delito="Robo a casa habitación")

### para screenshoot
ai <- data %>% 
  filter(tipo_impacto=="ALTO IMPACTO", 
         !delito %in% c("HOMICIDIO DOLOSO", "LESIONES DOLOSAS POR DISPARO DE ARMA DE FUEGO", 
                        "ROBO A CASA HABITACIÓN CON VIOLENCIA", 
                        "VIOLACIÓN", "ROBO DE VEHÍCULO CON Y SIN VIOLENCIA", 
                        "ROBO A REPARTIDOR CON Y SIN VIOLENCIA", 
                        "ROBO A TRANSEUNTE EN VÍA PÚBLICA CON Y SIN VIOLENCIA", "ROBO A CUENTAHABIENTE SALIENDO DEL CAJERO CON VIOLENCIA"
                        ), 
         !grepl("PASAJERO", modalidad_delito)
         )


gr_ai <- ai %>% 
  # filter(delito=="ROBO DE VEHÍCULO CON Y SIN VIOLENCIA", 
  #        #grepl("CON VIOLE|C/V", modalidad_delito),
  #        year(fecha_inicio) %in% c(2020:2025)) %>% 
  group_by(fecha_inicio, delito) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date(fecha_inicio_global), 
                                 as_date(fecha_lim), "1 day"), 
           delito=unique(ai$delito),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 month"), delito) %>% 
  mutate(delito=str_wrap(delito, 20)) %>% 
  summarise(Total=mean(Total)) %>% 
  ggplot(aes(fecha_inicio, Total)) +
  geom_point(alpha=.7#, color=colores[1]
  ) + facet_wrap(.~delito, scales = "free") +
  geom_smooth(se=F#, color=colores[7]
  ) + theme_light() + tema_fgj +
  # geom_smooth(se=F, color=colores[2], method = "lm") +
  scale_x_date(date_labels = "%b-%y", date_breaks = "3 month") +
  labs(x="Fecha de inicio", y="Promedio diario", 
       title = paste0("Promedio diario de carpetas por extorsión según tipo"), 
       subtitle = paste0("Desde ",  format(as_date(fecha_inicio_global), "%B de %Y"), " a ",
                         format(as_date(fecha_lim), "%B de %Y"))) +
  scale_color_manual(values = colores[3:1]) +
  theme(axis.text.x = element_text(angle = 90), 
        legend.position = "bottom"
  )

###
gr_casa <- grafica_tendencia(datos = data %>% 
                                     filter(delito %in% c("ROBO A CASA HABITACIÓN CON VIOLENCIA"
                                     )), 
                                   fecha_inicio_global = fecha_inicio_global, fecha_lim = fecha_lim,
                                   delito = "Robo a casa habitación")

ggsave(plot = gr_casa, 
       "gr_casa.svg", width = 12, height = 6
)

#####
#sección de género

#feminicido
gr_fem <- grafica_tendencia(datos = data %>% 
                               filter(delito %in% c("HOMICIDIO DOLOSO"), 
                                                    grepl("FEM", modalidad_delito)
                               ), 
                             fecha_inicio_global = fecha_inicio_global, fecha_lim = fecha_lim,
                             delito = "Feminicidio")

ggsave(plot = gr_fem, 
       "gr_fem.svg", width = 12, height = 6
)


#abuso
gr_abuso <- grafica_tendencia(datos = data %>% 
                              filter(modalidad_delito %in% c("ABUSO SEXUAL")
                              ), 
                            fecha_inicio_global = fecha_inicio_global, fecha_lim = fecha_lim,
                            delito = "Abuso sexual")

ggsave(plot = gr_abuso, 
       "gr_abuso.svg", width = 12, height = 6
)

#acoso
gr_acoso <- grafica_tendencia(datos = data %>% 
                                filter(modalidad_delito %in% c("ACOSO SEXUAL")
                                ), 
                              fecha_inicio_global = fecha_inicio_global, fecha_lim = fecha_lim,
                              delito = "Acoso sexual")

ggsave(plot = gr_acoso, 
       "gr_acoso.svg", width = 12, height = 6
)


#acoso
gr_CIS <- grafica_tendencia(datos = data %>% 
                                filter(modalidad_delito %in% c("CONTRA LA INTIMIDAD SEXUAL")
                                ), 
                              fecha_inicio_global = fecha_inicio_global, fecha_lim = fecha_lim,
                              delito = "Contra la intimidad sexual")

ggsave(plot = gr_CIS, 
       "gr_CIS.svg", width = 12, height = 6
)

#trata
gr_CIS <- grafica_tendencia(datos = data %>% 
                              filter(modalidad_delito %in% c("CONTRA LA INTIMIDAD SEXUAL")
                              ), 
                            fecha_inicio_global = fecha_inicio_global, fecha_lim = fecha_lim,
                            delito = "Contra la intimidad sexual")

ggsave(plot = gr_CIS, 
       "gr_CIS.svg", width = 12, height = 6
)


#tentativa de violación
gr_tenta_viol <- grafica_tendencia(datos = data %>% 
                              filter(modalidad_delito %in% c("TENTATIVA DE VIOLACION")
                              ), 
                            fecha_inicio_global = fecha_inicio_global, fecha_lim = fecha_lim,
                            delito = "Tentativa de violación")

ggsave(plot = gr_tenta_viol, 
       "gr_tentativa_violacion.svg", width = 12, height = 6
)

#trata de personas
gr_trata <- grafica_tendencia(datos = data %>% 
                                     filter(modalidad_delito %in% c("TRATA DE PERSONAS")
                                     ), 
                                   fecha_inicio_global = fecha_inicio_global, fecha_lim = fecha_lim,
                                   delito = "Trata de personas")

ggsave(plot = gr_trata, 
       "gr_trata.svg", width = 12, height = 6
)

#tentativa de feminicidio
gr_tenta_femi <- grafica_tendencia(datos = data %>% 
                                    filter(modalidad_delito %in% c( 
                                      "TENTATIVA DE FEMINICIDIO"
                                    )), 
                                  fecha_inicio_global = fecha_inicio_global, fecha_lim = fecha_lim,
                                  delito = "Tentativa de feminicidio")

ggsave(plot = gr_tenta_femi, 
       "gr_tenta_femi.svg", width = 12, height = 6
)

#violación
gr_violacion <- data %>% 
  filter(delito=="VIOLACIÓN") %>% 
  mutate(tipo=case_when(
    grepl("EQUIPAR", modalidad_delito) ~ "Equiparada", T ~ "Simple"
  )) %>% 
  # filter(delito=="ROBO DE VEHÍCULO CON Y SIN VIOLENCIA", 
  #        #grepl("CON VIOLE|C/V", modalidad_delito),
  #        year(fecha_inicio) %in% c(2020:2025)) %>% 
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date(fecha_inicio_global), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=c("Equiparada", "Simple"),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 month"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  ggplot(aes(fecha_inicio, Total, color=tipo)) +
  geom_point(alpha=.7#, color=colores[1]
  ) +
  geom_smooth(se=F#, color=colores[7]
  ) + theme_light() + tema_fgj +
  # geom_smooth(se=F, color=colores[2], method = "lm") +
  scale_x_date(date_labels = "%b-%y", date_breaks = "3 month") +
  labs(x="Fecha de inicio", y="Promedio diario", 
       title = paste0("Promedio diario de carpetas por violación según tipo"), 
       subtitle = paste0("Desde ",  format(as_date(fecha_inicio_global), "%B de %Y"), " a ",
                         format(as_date(fecha_lim), "%B de %Y"))) +
  scale_color_manual(values = colores[2:1]) +
  theme(axis.text.x = element_text(angle = 90), 
        legend.position = "bottom"
  )


ggsave(plot = gr_violacion, 
       "gr_violacion.svg", width = 12, height = 6
)

#violencia familiar 
fam_23 <- read_excel("01 VIOLENCIA FAMILIAR 2023_31dic2023.xlsx") %>% clean_names() %>% 
  select(id_ap, tipo_de_agresion_moral, tipo_de_agresion_fisica)
fam_24 <- read_excel("01 VIOLENCIA FAMILIAR 2024_31dic2024.xlsx")%>% clean_names() %>% 
  # mutate(tipo_de_agresion_moral=case_when(
  #   tipo_de_agresion_moral=="No" ~NA, T ~tipo_de_agresion_moral
  # ), tipo_de_agresion_fisica=case_when(
  #   tipo_de_agresion_fisica %in% c("No", "NO", "no") ~NA, T ~tipo_de_agresion_fisica
  # )) %>% 
  select(id_ap, tipo_de_agresion_moral, tipo_de_agresion_fisica)
fam_22 <- read_excel("04 VIOLENCIA FAMILIAR 2022_31dic2022.xlsx")%>% clean_names() %>%
  rename(tipo_de_agresion_moral=moral, tipo_de_agresion_fisica=fisica) %>% 
  mutate(tipo_de_agresion_moral=case_when(
    tipo_de_agresion_moral=="No" ~NA, T ~tipo_de_agresion_moral
  ), tipo_de_agresion_fisica=case_when(
    tipo_de_agresion_fisica %in% c("No", "NO", "no") ~NA, T ~tipo_de_agresion_fisica
  )) %>% 
  select(id_ap, tipo_de_agresion_moral, tipo_de_agresion_fisica)

violencia_familiar <- read_excel("violencia_familiar_variable.xlsx") %>% 
  clean_names() %>% drop_na(id_ap)
  
violencia_familiar <- violencia_familiar %>% 
  filter(!duplicated(id_ap))

familiar <- bind_rows(fam_22, fam_23) %>% bind_rows(fam_24) %>% 
  filter(!duplicated(id_ap)) %>% 
  mutate(tipo_agresion=case_when(
    !is.na(tipo_de_agresion_fisica) & 
      !is.na(tipo_de_agresion_moral) ~ "Física y moral", 
    !is.na(tipo_de_agresion_fisica) & 
      is.na(tipo_de_agresion_moral) ~ "Física", 
    is.na(tipo_de_agresion_fisica) & 
      !is.na(tipo_de_agresion_moral) ~ "Moral", 
    T ~ NA
  )) %>% drop_na(tipo_agresion)

familiar <- data %>%
  filter(fecha_inicio>="2022-01-01") %>% 
  filter(modalidad_delito=="VIOLENCIA FAMILIAR") %>% 
  left_join(violencia_familiar, by="id_ap")

viol_fam <- data %>% filter(modalidad_delito=="VIOLENCIA FAMILIAR") %>% 
  filter(fecha_inicio>="2022-01-01") %>% 
  left_join(familiar, by="id_ap") #%>% 
  # # rename(tipo_mau=tipo_agresion) %>% 
  # left_join(violencia_familiar, by="id_ap") %>% 
  # rename(tipo_javier=tipo_agresion)

gr_fam <- viol_fam %>% drop_na(tipo_agresion) %>% 
  # filter(delito=="VIOLACIÓN") %>% 
  # mutate(tipo=case_when(
  #   grepl("EQUIPAR", modalidad_delito) ~ "Equiparada", T ~ "Simple"
  # )) %>% 
  # filter(delito=="ROBO DE VEHÍCULO CON Y SIN VIOLENCIA", 
  #        #grepl("CON VIOLE|C/V", modalidad_delito),
  #        year(fecha_inicio) %in% c(2020:2025)) %>% 
  # mutate(tipo_agresion=case_when(
  #   tipo_agresion=="economica" ~ "Moral", 
  #   tipo_agresion=="moral" ~ "Moral", 
  #   tipo_agresion=="fisica" ~ "Física", 
  #   tipo_agresion=="fisica y moral" ~ "Física y Moral", 
  #   
  # )) %>% 
  group_by(fecha_inicio, tipo_agresion) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date("2022-01-01"), 
                                 as_date(fecha_lim), "1 day"), 
           tipo_agresion=c("Moral", "Física", "Física y moral"),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 month"), tipo_agresion) %>% 
  summarise(Total=mean(Total)) %>% 
  ggplot(aes(fecha_inicio, Total, color=tipo_agresion)) +
  geom_point(alpha=.7#, color=colores[1]
  ) +
  geom_smooth(se=F#, color=colores[7]
  ) + theme_light() + tema_fgj +
  #facet_wrap(.~tipo_agresion, scales = "free_y", nrow = 3) +
  # geom_smooth(se=F, color=colores[2], method = "lm") +
  scale_x_date(date_labels = "%b-%y", date_breaks = "3 month") +
  labs(x="Fecha de inicio", y="Promedio diario", 
       color="",
       title = paste0("Promedio diario de carpetas por violencia familiar según tipo"), 
       subtitle = paste0("Desde ",  format(as_date("2022-01-01"), "%B de %Y"), " a ",
                         format(as_date(fecha_lim), "%B de %Y"))) +
  scale_color_manual(values = colores[4:1]) +
  theme(axis.text.x = element_text(angle = 90), 
        legend.position = "bottom"
  )


ggsave(plot = gr_fam, 
       "./graficas_incidencia/especiales/gr_fam.png", width = 14, height = 7
)

tablas_especialies[[4]] <- viol_fam %>% drop_na(tipo_agresion) %>% 
  group_by(fecha_inicio, tipo=tipo_agresion) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date("2022-01-01"), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(hom$tipo),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 month"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  mutate(tipo_delito="Violencia familiar")
#####
#movilidad
#transeunte
gr_transeunte <- grafica_tendencia(datos = data %>% 
                                     filter(delito %in% c("ROBO A TRANSEUNTE EN VÍA PÚBLICA CON Y SIN VIOLENCIA", 
                                                          "ROBO A CUENTAHABIENTE SALIENDO DEL CAJERO CON VIOLENCIA"), 
                                     ), 
                                   fecha_inicio_global = fecha_inicio_global, fecha_lim = fecha_lim,
                                   delito = "Transeúnte y cuentahabiente")

ggsave(plot = gr_transeunte, 
       "gr_transeunte.svg", width = 12, height = 6
)
#repartidor

gr_repartidor <- data %>% 
  filter(delito=="ROBO A REPARTIDOR CON Y SIN VIOLENCIA") %>% 
  mutate(tipo=case_when(
    grepl("CON VIOLEN|C/V", modalidad_delito) ~ "Con violencia", T ~ "Sin violencia"
  )) %>% 
  # filter(delito=="ROBO DE VEHÍCULO CON Y SIN VIOLENCIA", 
  #        #grepl("CON VIOLE|C/V", modalidad_delito),
  #        year(fecha_inicio) %in% c(2020:2025)) %>% 
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date(fecha_inicio_global), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=c("Con violencia", "Sin violencia"),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 month"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  ggplot(aes(fecha_inicio, Total, color=tipo)) +
  geom_point(alpha=.7#, color=colores[1]
  ) +
  geom_smooth(se=F#, color=colores[7]
  ) + theme_light() + tema_fgj +
  # geom_smooth(se=F, color=colores[2], method = "lm") +
  scale_x_date(date_labels = "%b-%y", date_breaks = "3 month") +
  labs(x="Fecha de inicio", y="Promedio diario", 
       title = paste0("Promedio diario de carpetas por robo a repartidor por tipo"), 
       subtitle = paste0("Desde ",  format(as_date(fecha_inicio_global), "%B de %Y"), " a ",
                         format(as_date(fecha_lim), "%B de %Y"))) +
  scale_color_manual(values = colores[2:1]) +
  theme(axis.text.x = element_text(angle = 90), 
        legend.position = "bottom"
  )


ggsave(plot = gr_repartidor, 
       "gr_repartidor.svg", width = 12, height = 6
)
#pasajero

gr_pasajero <- grafica_tendencia(datos = data %>% 
                                     filter(grepl("PASAJERO", modalidad_delito)
                                     ), 
                                   fecha_inicio_global = fecha_inicio_global, fecha_lim = fecha_lim,
                                   delito = "Robo a pasajero")

ggsave(plot = gr_pasajero, 
       "gr_pasajero.svg", width = 12, height = 6
)

#transportista
gr_transportista <- grafica_tendencia(datos = data %>% 
                                   filter(delito=="ROBO A TRANSPORTISTA CON Y SIN VIOLENCIA"
                                   ), 
                                 fecha_inicio_global = fecha_inicio_global, fecha_lim = fecha_lim,
                                 delito = "Robo a transportista")

ggsave(plot = gr_transportista, 
       "gr_transportista.svg", width = 12, height = 6
)



#####
#delitos urbanos 
#despojo
gr_despojo <- grafica_tendencia(datos = data %>% 
                                        filter(modalidad_delito=="DESPOJO"
                                        ), 
                                      fecha_inicio_global = fecha_inicio_global, fecha_lim = fecha_lim,
                                      delito = "Despojo")

ggsave(plot = gr_despojo, 
       "gr_despojo.svg", width = 12, height = 6
)

#animales
gr_animales <- grafica_tendencia(datos = data %>% 
                                  filter(modalidad_delito %in% c("MALTRATO ANIMAL","ROBO DE ANIMALES")
                                  ), 
                                fecha_inicio_global = fecha_inicio_global, fecha_lim = fecha_lim,
                                delito = "robo de animales y maltrato animales")

ggsave(plot = gr_animales, 
       "gr_animales.svg", width = 12, height = 6
)


#ambientales
#animales
gr_ambientales <- grafica_tendencia(datos = data %>% 
                                   filter(modalidad_delito %in% c(grep("AMBI", unique(data$modalidad_delito), value = T), 
                                                                  "TALA")
                                   ), 
                                 fecha_inicio_global = fecha_inicio_global, fecha_lim = fecha_lim,
                                 delito = "delito ambientales")

ggsave(plot = gr_ambientales, 
       "gr_ambientales.svg", width = 12, height = 6
)

#####
#bonus pack
#fraude
gr_fraude <- grafica_tendencia(datos = data %>% 
                                      filter(modalidad_delito %in% c("FRAUDE")
                                      ), 
                                    fecha_inicio_global = fecha_inicio_global, fecha_lim = fecha_lim,
                                    delito = "Fraude")

ggsave(plot = gr_fraude, 
       "gr_fraude.svg", width = 12, height = 6
)
#amenazas
gr_amenazas <- grafica_tendencia(datos = data %>% 
                                 filter(modalidad_delito %in% c("AMENAZAS")
                                 ), 
                               fecha_inicio_global = fecha_inicio_global, fecha_lim = fecha_lim,
                               delito = "Amenazas")

ggsave(plot = gr_amenazas, 
       "gr_amenazas.svg", width = 12, height = 6
)



data %>% 
  filter(tipo_impacto=="BAJO IMPACTO"
         ) %>% 
  group_by(fecha_inicio=year(fecha_inicio), 
           modalidad_delito
           ) %>% 
  summarise(Total=n()) %>% 
  filter(fecha_inicio %in% c(2022, 2024)) %>% 
  spread(fecha_inicio, Total, fill=0) %>% 
  filter(`2024`>30) %>% 
  mutate(cambio=`2024`/`2022`-1) %>% 
  arrange(-cambio) %>% 
  head(15)

#de acuerdo con la tabla, metemos otros delitos
#doc falso
gr_doc_falso <- grafica_tendencia(datos = data %>% 
                                   filter(modalidad_delito %in% c("USO DE DOCUMENTO FALSO")
                                   ), 
                                 fecha_inicio_global = fecha_inicio_global, fecha_lim = fecha_lim,
                                 delito = "Uso de documento falso")

ggsave(plot = gr_doc_falso, 
       "gr_doc_falso.svg", width = 12, height = 6
)

#Narcomenudeo
#doc falso
gr_narco <- grafica_tendencia(datos = data %>% 
                                    filter(grepl("NARCO", modalidad_delito)
                                    ), 
                                  fecha_inicio_global = fecha_inicio_global, fecha_lim = fecha_lim,
                                  delito = "Narcomenudeo")

ggsave(plot = gr_narco, 
       "gr_narco.svg", width = 12, height = 6
)

#Usupación de identidad
gr_usurpacion <- grafica_tendencia(datos = data %>% 
                                filter(grepl("USURPACIÓN DE IDENTIDAD", modalidad_delito)
                                ), 
                              fecha_inicio_global = fecha_inicio_global, fecha_lim = fecha_lim,
                              delito = "Usurpación de identidad")

ggsave(plot = gr_usurpacion, 
       "gr_usurpacion.svg", width = 12, height = 6
)


#metemos clasificacion
clasificacion <- readxl::read_excel("20250131_Nueva categorización.xlsx", 
                                    sheet = "Hoja 1"
                                    ) %>% clean_names() %>% 
  filter(is.na(exclusion)) %>% 
  select(delito, modalidad_delito, 
         cat_mp2, del_mp
         )



####Generamos la base de datos

data_bruto <- data %>% 
  left_join(clasificacion, by=c("delito", "modalidad_delito"))
  

# 
# data_bruto <- data %>% 
#   # filter(delito %in% c("ROBO A TRANSEUNTE EN VÍA PÚBLICA CON Y SIN VIOLENCIA", 
#   #                      "ROBO A NEGOCIO CON VIOLENCIA", "HOMICIDIO DOLOSO", 
#   #                      "LESIONES DOLOSAS POR DISPARO DE ARMA DE FUEGO", 
#   #                      "ROBO DE VEHÍCULO CON Y SIN VIOLENCIA", "VIOLACIÓN", 
#   #                      "ROBO A REPARTIDOR CON Y SIN VIOLENCIA", 
#   #                      "ROBO A PASAJERO A BORDO DE MICROBUS CON Y SIN VIOLENCIA", "ROBO A CASA HABITACIÓN CON VIOLENCIA", 
#   #                      "ROBO A PASAJERO A BORDO DEL METRO CON Y SIN VIOLENCIA", 
#   #                      "ROBO A CUENTAHABIENTE SALIENDO DEL CAJERO CON VIOLENCIA", 
#   #                      "ROBO A PASAJERO A BORDO DE TAXI CON VIOLENCIA", 
#   #                      "ROBO A TRANSPORTISTA CON Y SIN VIOLENCIA"
#   #                      ) |
#   #          modalidad_delito %in% c("EXTORSION", "VIOLENCIA FAMILIAR", 
#   #                                  "DESPOJO", "MALTRATO ANIMAL","ROBO DE ANIMALES", 
#   #                                  grep("AMBI", unique(data$modalidad_delito), value = T), "TALA", 
#   #                                  "TENTATIVA DE FEMINICIDIO", "TENTATIVA DE HOMICIDIO", 
#   #                                  "FRAUDE", "AMENAZAS", "USO DE DOCUMENTO FALSO",
#   #                                  "USURPACIÓN DE IDENTIDAD", 
#   #                                  grep("NARCO", unique(data$modalidad_delito), value = T), 
#   #                                  "TRATA DE PERSONAS", 
#   #                                  "TENTATIVA DE VIOLACION", "ACOSO SEXUAL", "ABUSO SEXUAL", 
#   #                                  "CONTRA LA INTIMIDAD SEXUAL"
#   #                                  )
#   #        
#   #        
#   #        ) %>% 
#   mutate(tipo_delito=case_when(
#     grepl("PASAJERO", modalidad_delito) ~ "Robo a pasajero",
#     grepl("FEMINICI", modalidad_delito) & delito=="HOMICIDIO DOLOSO" ~ "Feminicidio",
#     grepl("LESIONES INTENCIO|LESIONES DOLOS", modalidad_delito) ~ "Lesiones dolosas",
#     grepl("ROBO A CASA HABITACION", modalidad_delito) ~ "Robo a casa habitación",
#     delito=="ROBO DE VEHÍCULO CON Y SIN VIOLENCIA" ~ "Robo de vehículo",
#     delito %in% c("ROBO A TRANSEUNTE EN VÍA PÚBLICA CON Y SIN VIOLENCIA", 
#                   "ROBO A NEGOCIO CON VIOLENCIA", "HOMICIDIO DOLOSO", 
#                   "LESIONES DOLOSAS POR DISPARO DE ARMA DE FUEGO", 
#                   "ROBO DE VEHÍCULO CON Y SIN VIOLENCIA", "VIOLACIÓN", 
#                   "ROBO A REPARTIDOR CON Y SIN VIOLENCIA", 
#                   # "ROBO A PASAJERO A BORDO DE MICROBUS CON Y SIN VIOLENCIA", "ROBO A CASA HABITACIÓN CON VIOLENCIA", 
#                   # "ROBO A PASAJERO A BORDO DEL METRO CON Y SIN VIOLENCIA", 
#                   "ROBO A CUENTAHABIENTE SALIENDO DEL CAJERO CON VIOLENCIA", 
#                   # "ROBO A PASAJERO A BORDO DE TAXI CON VIOLENCIA", 
#                   "ROBO A TRANSPORTISTA CON Y SIN VIOLENCIA"
#     ) ~ str_to_sentence(delito), 
#     modalidad_delito %in% c(grep("AMBI", unique(data$modalidad_delito), value = T), "TALA") ~ "Ambientales", 
#     modalidad_delito %in% grep("NARCO", unique(data$modalidad_delito), value = T) ~ "Narcomenudeo", 
#     grepl("ANIMAL", modalidad_delito) ~ "Delitos contra animales",
#     grepl("NARCO", modalidad_delito) ~ "Narcomenudeo",
#     grepl("PORNO", modalidad_delito) ~ "Pornografía infantil",
#     grepl("CORRUP", modalidad_delito) ~ "Corrupción de menores",
#     grepl("PRIVACI", modalidad_delito) | modalidad_delito=="PRIV. ILEGAL DE LA LIB. Y ROBO DE VEHICULO"~ "Privación ilegal de la libertad",
#     # modalidad_delito %in% c("TENTATIVA DE FEMINICIDIO", "TENTATIVA DE HOMICIDIO") ~ "Tentativa de homicidio y feminicidio", 
#     grepl("ROBO", modalidad_delito) & grepl("CON VIOLENCIA|C/V", modalidad_delito) ~ "Robo con violencia",
#     grepl("RETEN|SUSTR", modalidad_delito) ~ "Retención y sustracción de menores", 
#     grepl("DESAP", modalidad_delito) ~ "Desaparición forzada", 
#     grepl("RECURS", modalidad_delito) ~ "ORPI", 
#     modalidad_delito=="EXTORSION" ~ "Extorsión", 
#     grepl("SECUE", delito) ~ "Secuestro", 
#     modalidad_delito=="EVASION DE PRESOS" ~ "Evasión de presos",
#     T ~ str_to_sentence(modalidad_delito)
#     
#   ))
#####
#homicidio, hom

# extorsion,extorsion %>% select(-tipo)

# violencia_familiar, viol_fam 

library(openxlsx)
wb <- createWorkbook()


addWorksheet(wb, "data_bruta")
addWorksheet(wb, "homicidios")
addWorksheet(wb, "extorsion")
addWorksheet(wb, "violencia_familiar")



writeData(wb, "data_bruta", data_bruto)
writeData(wb, "homicidios", hom)
writeData(wb, "extorsion", extorsion %>% select(-tipo))
writeData(wb, "violencia_familiar", viol_fam)

saveWorkbook(wb, "datos_en_bruto.xlsx", overwrite = T)




#### Parte 2 (la revancha) ######



#graficas de eficiencia ministerial
ordenes <- incidencia$ordenes
flagrancias <- incidencia$flagrancias
sentencias <- incidencia$sentencias




fecha_inicio_global <- "2019-01-01"
fecha_lim <- "2024-12-31"

# delitos_ppp <- c("Homicidio doloso", 
#                  "Feminicidio", "Lesiones dolosas", "Robo de vehículo", 
#                  "Robo a casa habitación", "Robo con violencia", 
#                  "Secuestro", "Extorsión", "Narcomenudeo", 
#                  "Privación ilegal de la libertad","Violación", "Abuso sexual", 
#                  "Acoso sexual", "Contra la intimidad sexual", 
#                  "Estupro", "Pornografía infantil", "Trata de personas", 
#                  "Corrupción de menores", "Retención y sustracción de menores", 
#                  "Violencia familiar", "Desaparición forzada", 
#                  "Tortura", "ORPI", "Enriquecimiento ilícito", 
#                  "Evasión de presos", "Sabotaje"
#                  
#                  )

delitos_ppp <- grep("\\d", unique(data_bruto$cat_mp2), 
                    value = T
                    )
data_bruto <- data_bruto %>% 
  mutate(cat_mp2=substr(cat_mp2, 5,nchar(cat_mp2)))

delitos_ppp <- substr(delitos_ppp, 5,nchar(delitos_ppp))

gr <- list()
for (i in 1:length(delitos_ppp)) {
  gr[[i]] <- grafica_tendencia(
    datos=data_bruto %>% 
      filter(cat_mp2 %in% delitos_ppp[i]), 
    fecha_inicio_global = fecha_inicio_global, 
    fecha_lim = fecha_lim, 
    delito = delitos_ppp[[i]]
  )
  
  
  ggsave(plot = gr[[i]], 
         paste0("./graficas_incidencia_la_revancha/", delitos_ppp[i], ".png"), 
         width = 14, height = 7
         )
  
  
}

tablas <- list()

for (i in 1:length(delitos_ppp)) {
  tablas[[i]] <- data_bruto %>% 
      filter(cat_mp2 %in% delitos_ppp[i]) %>% 
  group_by(fecha_inicio) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date(fecha_inicio_global), 
                                 as_date(fecha_lim), "1 day"), 
           #tipo=c("Secuestro", "Secuestro express"),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 year")) %>% 
  summarise(Total_carpetas=sum(Total),
            promedio_diario=mean(Total)
            ) %>% 
    mutate(tipo_delito=delitos_ppp[i])
  
  

  
}

tablas_promedios <- bind_rows(tablas)
tablas_promedios %>% 
  write.csv("./graficas_incidencia_la_revancha/tablas_promedios_delitos.csv", row.names = F)

#gráficas de barras
delitos_ppp2 <- c( "ORPI", "Enriquecimiento ilicito", "Sabotaje", "Estupro")

gr <- list()

for (i in 1:length(delitos_ppp2)) {
  gr[[i]] <- data_bruto %>% 
    filter(tipo_delito %in% delitos_ppp2[i]) %>% 
    group_by(año=year(fecha_inicio)) %>% 
    summarise(Total=n()) %>% 
    ggplot(aes(factor(año), Total)) +
    geom_col(fill=colores[1]) + 
    geom_label(aes(label=comma(Total, 1)), 
               size=6, position = position_stack(vjust = .8)) + theme_light()+
    tema_fgj + 
    labs(x="Año de inicio", y="Total de carpetas", 
         title = paste0("Total de carpetas iniciadas por ", delitos_ppp2[i]), 
         subtitle = "Desde 1 de enero de 2019 hasta 31 de diciembre de 2024"
         )
  
  
  ggsave(plot = gr[[i]], 
         paste0("./graficas_incidencia/", delitos_ppp2[i], ".svg"), 
         width = 14, height = 7
  )
  
  
}

#graficas especiales 
gr_cis <- grafica_tendencia(
  datos=data_bruto %>% 
    filter(tipo_delito %in% "Contra la intimidad sexual") %>% 
    filter(fecha_inicio>="2020-01-01"), 
  fecha_inicio_global = "2020-01-01", 
  fecha_lim = fecha_lim, 
  delito = "Contra la intimidad sexual"
)


ggsave(plot = gr_cis, 
       paste0("./graficas_incidencia/", "Contra la intimidad sexual", ".svg"), 
       width = 14, height = 7
)



###secuestro
gr_secuestro <- data_bruto %>% 
  filter(grepl("Secue", tipo_delito)) %>% 
  mutate(tipo=case_when(
    grepl("PLAGI", modalidad_delito) ~ "Secuestro", T ~ "Secuestro express"
  )) %>% 
  # filter(delito=="ROBO DE VEHÍCULO CON Y SIN VIOLENCIA", 
  #        #grepl("CON VIOLE|C/V", modalidad_delito),
  #        year(fecha_inicio) %in% c(2020:2025)) %>% 
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date(fecha_inicio_global), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=c("Secuestro", "Secuestro express"),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 month"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  ggplot(aes(fecha_inicio, Total, color=tipo)) +
  geom_point(alpha=.7#, color=colores[1]
  ) +
  geom_smooth(se=F#, color=colores[7]
  ) + theme_light() + tema_fgj +
  # geom_smooth(se=F, color=colores[2], method = "lm") +
  scale_x_date(date_labels = "%b-%y", date_breaks = "3 month") +
  labs(x="Fecha de inicio", y="Promedio diario", 
       title = paste0("Promedio diario de carpetas por secuestro por tipo"), 
       subtitle = paste0("Desde ",  format(as_date(fecha_inicio_global), "%B de %Y"), " a ",
                         format(as_date(fecha_lim), "%B de %Y"))) +
  scale_color_manual(values = colores[2:1]) +
  theme(axis.text.x = element_text(angle = 90), 
        legend.position = "bottom"
  )


ggsave(plot = gr_secuestro, 
       "./graficas_incidencia/especiales/gr_secuestro.png", width = 12, height = 6
)

tablas_especialies[[5]] <- data_bruto %>% 
  filter(grepl("Secue", tipo_delito)) %>% 
  mutate(tipo=case_when(
    grepl("PLAGI", modalidad_delito) ~ "Secuestro", T ~ "Secuestro express"
  )) %>% 
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date("2022-01-01"), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(hom$tipo),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 month"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  mutate(tipo_delito="Secuestro")
###lesiones dolosas tipo

gr_lesiones <- data_bruto %>% 
  filter(tipo_delito=="Lesiones dolosas") %>% 
  mutate(tipo=case_when(
    grepl("BLANC", modalidad_delito) ~ "Arma blanca",
    grepl("FUEGO", modalidad_delito) ~ "Arma de fuego",
    grepl("GOLPES", modalidad_delito) ~ "Golpes",
    T ~ "Otros"
  )) %>% 
  # filter(delito=="ROBO DE VEHÍCULO CON Y SIN VIOLENCIA", 
  #        #grepl("CON VIOLE|C/V", modalidad_delito),
  #        year(fecha_inicio) %in% c(2020:2025)) %>% 
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date(fecha_inicio_global), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=c("Arma blanca", "Arma de fuego", "Golpes", "Otros"),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 month"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  ggplot(aes(fecha_inicio, Total, color=tipo)) +
  geom_point(alpha=.7#, color=colores[1]
  ) +
  geom_smooth(se=F#, color=colores[7]
  ) + theme_light() + tema_fgj +
  # geom_smooth(se=F, color=colores[2], method = "lm") +
  scale_x_date(date_labels = "%b-%y", date_breaks = "3 month") +
  labs(x="Fecha de inicio", y="Promedio diario", 
       title = paste0("Promedio diario de carpetas por lesiones por tipo"), 
       subtitle = paste0("Desde ",  format(as_date(fecha_inicio_global), "%B de %Y"), " a ",
                         format(as_date(fecha_lim), "%B de %Y"))) +
  scale_color_manual(values = colores[4:1]) +
  theme(axis.text.x = element_text(angle = 90), 
        legend.position = "bottom"
  )


ggsave(plot = gr_lesiones, 
       "./graficas_incidencia/especiales/gr_lesiones.svg", width = 12, height = 6
)

tablas_especialies[[6]] <-  data_bruto %>% 
  filter(tipo_delito=="Lesiones dolosas") %>% 
  mutate(tipo=case_when(
    grepl("BLANC", modalidad_delito) ~ "Arma blanca",
    grepl("FUEGO", modalidad_delito) ~ "Arma de fuego",
    grepl("GOLPES", modalidad_delito) ~ "Golpes",
    T ~ "Otros"
  )) %>% 
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date("2022-01-01"), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(hom$tipo),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 month"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  mutate(tipo_delito="Lesiones dolosas")

###narcomenudeo
gr_narco <- data_bruto %>% 
  filter(tipo_delito=="Narcomenudeo") %>% 
  mutate(tipo=case_when(
    grepl("VENTA", modalidad_delito) ~ "Con fines de venta",
    T ~ "Posesión simple"
  )) %>% 
  # filter(delito=="ROBO DE VEHÍCULO CON Y SIN VIOLENCIA", 
  #        #grepl("CON VIOLE|C/V", modalidad_delito),
  #        year(fecha_inicio) %in% c(2020:2025)) %>% 
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date(fecha_inicio_global), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=c("Con fines de venta", "Posesión simple"),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 month"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  ggplot(aes(fecha_inicio, Total, color=tipo)) +
  geom_point(alpha=.7#, color=colores[1]
  ) +
  geom_smooth(se=F#, color=colores[7]
  ) + theme_light() + tema_fgj +
  # geom_smooth(se=F, color=colores[2], method = "lm") +
  scale_x_date(date_labels = "%b-%y", date_breaks = "3 month") +
  labs(x="Fecha de inicio", y="Promedio diario", 
       title = paste0("Promedio diario de carpetas por narcomenudeo tipo"), 
       subtitle = paste0("Desde ",  format(as_date(fecha_inicio_global), "%B de %Y"), " a ",
                         format(as_date(fecha_lim), "%B de %Y"))) +
  scale_color_manual(values = colores[4:1]) +
  theme(axis.text.x = element_text(angle = 90), 
        legend.position = "bottom"
  )


ggsave(plot = gr_narco, 
       "./graficas_incidencia/especiales/gr_narco.svg", width = 12, height = 6
)

tablas_especialies[[7]] <-  data_bruto %>% 
  filter(tipo_delito=="Narcomenudeo") %>% 
  mutate(tipo=case_when(
    grepl("VENTA", modalidad_delito) ~ "Con fines de venta",
    T ~ "Posesión simple"
  )) %>% 
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date("2022-01-01"), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(hom$tipo),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 month"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  mutate(tipo_delito="Narcomenudeo")

id_menor <- incidencia$victimas %>% 
  filter(edad<18) %>% pull(id_ap)

#homicidio por tipo
gr_hom <- data_bruto %>% 
  filter(tipo_delito %in% c("Homicidio doloso", "Feminicidio")) %>% 
  mutate(tipo=case_when(
    grepl("FEM", modalidad_delito) ~ "Feminicidio",
    # grepl("AHORC", modalidad_delito) ~ "H. por ahorcamiento",
    # grepl("BLAN", modalidad_delito) ~ "H. por arma blanca",
    grepl("FUEGO", modalidad_delito) ~ "H. por arma de fuego",
    # grepl("GOLPES", modalidad_delito) ~ "H. por golpes",
    T ~ "Otros"
  )) %>% 
  # filter(delito=="ROBO DE VEHÍCULO CON Y SIN VIOLENCIA", 
  #        #grepl("CON VIOLE|C/V", modalidad_delito),
  #        year(fecha_inicio) %in% c(2020:2025)) %>% 
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date(fecha_inicio_global), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=c("Feminicidio", #"H. por arma blanca", 
                  "H. por arma de fuego", #"H. por golpes"
                  "Otros"),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 month"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  ggplot(aes(fecha_inicio, Total, color=tipo)) +
  geom_point(alpha=.7#, color=colores[1]
  ) +
  geom_smooth(se=F#, color=colores[7]
  ) + theme_light() + tema_fgj +
  # facet_wrap(.~tipo, scales = "free") +
  # geom_smooth(se=F, color=colores[2], method = "lm") +
  scale_x_date(date_labels = "%b-%y", date_breaks = "3 month") +
  labs(x="Fecha de inicio", y="Promedio diario", 
       title = paste0("Promedio diario de carpetas por femincidio y homicidio doloso por tipo"), 
       subtitle = paste0("Desde ",  format(as_date(fecha_inicio_global), "%B de %Y"), " a ",
                         format(as_date(fecha_lim), "%B de %Y"))) +
  scale_color_manual(values = colores[5:1]) +
  theme(axis.text.x = element_text(angle = 90), 
        legend.position = "bottom"
  )


ggsave(plot = gr_hom, 
       "./graficas_incidencia/especiales/gr_hom_tipo.svg", width = 12, height = 6
)

#abuso sexual

gr_abuso <- data_bruto %>% 
  filter(tipo_delito=="Abuso sexual") %>% 
  mutate(tipo=case_when(
    id_ap %in% id_menor ~ "Menores",
    T ~ "Mayores"
  )) %>% 
  # filter(delito=="ROBO DE VEHÍCULO CON Y SIN VIOLENCIA", 
  #        #grepl("CON VIOLE|C/V", modalidad_delito),
  #        year(fecha_inicio) %in% c(2020:2025)) %>% 
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date(fecha_inicio_global), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=c("Menores", "Mayores"),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 month"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  ggplot(aes(fecha_inicio, Total, color=tipo)) +
  geom_point(alpha=.7#, color=colores[1]
  ) +
  geom_smooth(se=F#, color=colores[7]
  ) + theme_light() + tema_fgj +
  # geom_smooth(se=F, color=colores[2], method = "lm") +
  scale_x_date(date_labels = "%b-%y", date_breaks = "3 month") +
  labs(x="Fecha de inicio", y="Promedio diario", 
       title = paste0("Promedio diario de carpetas por abuso sexual por tipo"), 
       subtitle = paste0("Desde ",  format(as_date(fecha_inicio_global), "%B de %Y"), " a ",
                         format(as_date(fecha_lim), "%B de %Y"))) +
  scale_color_manual(values = colores[4:1]) +
  theme(axis.text.x = element_text(angle = 90), 
        legend.position = "bottom"
  )


#violación 
gr_violacion_tipo <- data_bruto %>% 
  filter(tipo_delito=="Violación") %>% 
  mutate(tipo=case_when(
    grepl("EQUIPARADA", modalidad_delito) ~ "Equiparada",
    T ~ "Simple"
  )) %>% 
  # filter(delito=="ROBO DE VEHÍCULO CON Y SIN VIOLENCIA", 
  #        #grepl("CON VIOLE|C/V", modalidad_delito),
  #        year(fecha_inicio) %in% c(2020:2025)) %>% 
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date(fecha_inicio_global), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=c("Equiparada","Simple"),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 month"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  ggplot(aes(fecha_inicio, Total, color=tipo)) +
  geom_point(alpha=.7#, color=colores[1]
  ) +
  geom_smooth(se=F#, color=colores[7]
  ) + theme_light() + tema_fgj +
  # geom_smooth(se=F, color=colores[2], method = "lm") +
  scale_x_date(date_labels = "%b-%y", date_breaks = "3 month") +
  labs(x="Fecha de inicio", y="Promedio diario", 
       title = paste0("Promedio diario de carpetas por violación por tipo"), 
       subtitle = paste0("Desde ",  format(as_date(fecha_inicio_global), "%B de %Y"), " a ",
                         format(as_date(fecha_lim), "%B de %Y"))) +
  scale_color_manual(values = colores[2:1]) +
  theme(axis.text.x = element_text(angle = 90), 
        legend.position = "bottom"
  )


ggsave(plot = gr_violacion_tipo, 
       "./graficas_incidencia/especiales/gr_violacion_tipo.svg", width = 12, height = 6
)

tablas_especialies[[8]] <-  data_bruto %>% 
  filter(tipo_delito=="Violación") %>% 
  mutate(tipo=case_when(
    grepl("EQUIPARADA", modalidad_delito) ~ "Equiparada",
    T ~ "Simple"
  )) %>% 
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date("2022-01-01"), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(hom$tipo),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 month"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  mutate(tipo_delito="Violación")

####
#violación conocido

violacion_conocido <- read_excel("violacion_conocido.xlsx") %>% 
  clean_names() %>% 
  mutate(relacion_victima_victimario=str_to_sentence(relacion_victima_victimario))

gr_violacion_conocido <- data_bruto %>% 
  filter(tipo_delito=="Violación", 
         fecha_inicio>="2022-01-01"
         ) %>% 
  left_join(violacion_conocido, by="id_ap") %>% 
  mutate(tipo=case_when(
    relacion_victima_victimario=="Familiar y conocido" ~ "Familiar", 
    T ~relacion_victima_victimario
  )) %>% drop_na(relacion_victima_victimario) %>% 
  # filter(delito=="ROBO DE VEHÍCULO CON Y SIN VIOLENCIA", 
  #        #grepl("CON VIOLE|C/V", modalidad_delito),
  #        year(fecha_inicio) %in% c(2020:2025)) %>% 
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date("2022-01-01"), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=c("Familiar", "Conocido", "Desconocido"),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 month"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  ggplot(aes(fecha_inicio, Total, color=tipo)) +
  geom_point(alpha=.7#, color=colores[1]
  ) +
  geom_smooth(se=F#, color=colores[7]
  ) + theme_light() + tema_fgj +
  # geom_smooth(se=F, color=colores[2], method = "lm") +
  scale_x_date(date_labels = "%b-%y", date_breaks = "3 month") +
  labs(x="Fecha de inicio", y="Promedio diario", 
       title = paste0("Promedio diario de carpetas por violación por relación víctima-victimario"), 
       subtitle = paste0("Desde ",  format(as_date("2022-01-01"), "%B de %Y"), " a ",
                         format(as_date(fecha_lim), "%B de %Y"))) +
  scale_color_manual(values = colores[3:1]) +
  theme(axis.text.x = element_text(angle = 90), 
        legend.position = "bottom"
  )


ggsave(plot = gr_violacion_conocido, 
       "./graficas_incidencia/especiales/gr_violacion_conocido.svg", width = 12, height = 6
)

tablas_especialies[[9]] <-  data_bruto %>% 
  filter(tipo_delito=="Violación", 
         fecha_inicio>="2022-01-01"
  ) %>% 
  left_join(violacion_conocido, by="id_ap") %>% 
  mutate(tipo=case_when(
    relacion_victima_victimario=="Familiar y conocido" ~ "Familiar", 
    T ~relacion_victima_victimario
  )) %>% drop_na(relacion_victima_victimario) %>% 
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date("2022-01-01"), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(hom$tipo),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 month"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  mutate(tipo_delito="Violación (relación)")



tablas_promedio_especiales <- bind_rows(tablas_especialies)

write.csv(tablas_promedio_especiales, "tablas_promedio_especiales.csv")

###grafica de incidencia para todos los delitos
gr_inci_ppp <- grafica_tendencia(
  datos=data_bruto %>% 
    filter(tipo_delito %in% delitos_ppp), 
  fecha_inicio_global = fecha_inicio_global, 
  fecha_lim = fecha_lim, 
  delito = "delitos seleccionados para el plan de política criminal"
)


ggsave(plot = gr_inci_ppp, 
       paste0("./graficas_incidencia/", "delito_ppp", ".svg"), 
       width = 14, height = 7
)


#graficas de incidecia solo para femin, homicidio y desaparición
###grafica de incidencia para todos los delitos
gr_inci_alta <- grafica_tendencia(
  datos=data_bruto %>% 
    filter(tipo_delito %in% c("Feminicidio", "Homicidio doloso", 
                              "Desaparición forzada"
                              )), 
  fecha_inicio_global = fecha_inicio_global, 
  fecha_lim = fecha_lim, 
  delito = "Feminicidio, homicidio doloso y desaparición forzada"
)


ggsave(plot = gr_inci_alta, 
       paste0("./graficas_incidencia/", "incidencia_fem_hom_desap", ".svg"), 
       width = 14, height = 7
)


###grafica de incidencia homicidio y feminicio
gr_inci_hom_fem <- grafica_tendencia(
  datos=data_bruto %>% 
    filter(tipo_delito %in% c("Feminicidio", "Homicidio doloso"
    )), 
  fecha_inicio_global = fecha_inicio_global, 
  fecha_lim = fecha_lim, 
  delito = "Feminicidio y homicidio doloso"
)


ggsave(plot = gr_inci_hom_fem, 
       paste0("./graficas_incidencia/", "gr_inci_hom_fem", ".svg"), 
       width = 14, height = 7
)

ordenes <- incidencia$ordenes
sentencias <- incidencia$sentencias

# ordenes <- ordenes %>% 
#   mutate(tipo_delito=case_when(
#     grepl("FEMIN", delito_hom_uet) & !grepl("TENTA", delito_hom_uet) ~ "Feminicidio", 
#     delito_alto_impacto=="HOMICIDIO DOLOSO" ~ "Homicidio doloso", 
#     delito_hom_uet=="LESIONES DOLOSAS" ~ "Lesiones dolosas", 
#     grepl("CASA", delito_hom_uet) ~ "Robo a casa habitación", 
#     grepl("SECU", delito_hom_uet) ~ "Secuestro", 
#     grepl("NARCO", delito_hom_uet) ~ "Narcomenudeo", 
#     delito_hom_uet=="VIOLACIÓN" ~ "Violación", 
#     delito_hom_uet=="ACOSO SEXUAL" ~ "Acoso sexual", 
#     delito_hom_uet=="ESTUPRO" ~"Estupro", 
#     delito_hom_uet=="TRATA DE PERSONAS" ~"Trata de personas", 
#     delito_hom_uet %in% c( "RETENCIÓN DE MENORES", 
#                            "SUSTRACCIÓN DE MENORES", 
#                            "SUSTRACCIÓN DE MENORES O INCAPACES",
#                            "RETENCIÓN DE MENORES O INCAPACES" 
#     ) ~ "Retención y sustracción de menores",
#     delito_hom_uet %in% c("DESAPARICIÓN FORZADA DE PERSONAS",
#                           "DESAPARICIÓN COMETIDA POR PARTICULARES") ~ "Desaparición forzada", 
#     delito_hom_uet %in% "OPERACIONES CON RECURSOS DE PROCEDENCIA ILÍCITA" ~ "ORPI", 
#     delito_hom_uet %in% "EVASIÓN DE PRESOS" ~ "Evasión de presos", 
#     delito_alto_impacto %in% "ROBO DE VEHÍCULO CON Y SIN VIOLENCIA" ~ "Robo de vehículo", 
#     delito_hom_uet %in% "EXTORSIÓN" ~ "Extorsión", 
#     grepl("LIBERTAD", delito_hom_uet) & !grepl("TENTA", delito_hom_uet) ~ "Privación ilegal de la libertad", 
#     delito_hom_uet %in% "ABUSO SEXUAL" ~ "Abuso sexual", 
#     delito_hom_uet %in% "VIOLENCIA FAMILIAR" ~ "Violencia familiar",
#     delito_hom_uet %in% "CONTRA LA INTIMIDAD SEXUAL" ~ "Contra la intimidad sexual", 
#     delito_hom_uet %in% c("PORNOGRAFÍA INFANTIL", "PORNOGRAFÍA") ~ "Pornografía infantil",
#     grepl("CORRUPCI.N DE MENO", delito_hom_uet) ~ "Corrupción de menores", 
#     grepl("TORTUR", delito_hom_uet) ~ "Tortura",
#     grepl("ENRIQUE", delito_hom_uet) ~ "Enriquecimiento ilícito", 
#     grepl("SABOTAJE", delito_hom_uet) ~ "Sabotaje", 
#     grepl("ROBO", delito_hom_uet) & 
#       grepl("CON VIOLENCIA|C/V", delito_hom_uet) ~ "Robo de vehículo con violencia", 
#     
#     
#     
#     T ~ str_to_sentence(delito_hom_uet)
#   )
#   )

ordenes <- ordenes %>% 
  mutate(cat_mp2=case_when(
    # grepl("FEMIN", delito_hom_uet) & !grepl("TENTA", delito_hom_uet) ~ "Feminicidio", 
    delito_alto_impacto=="HOMICIDIO DOLOSO" ~ "Muertes violentas", 
    delito_hom_uet %in% c("ROBO A CASA HABITACIÓN","ROBO A CASA HABITACIÓN CON VIOLENCIA") ~ "Robo a casa habitación",
    delito_hom_uet %in% c("AMENAZAS", "ALLANAMIENTO DE MORADA", 
                          "DISCRIMINACIÓN")~ "Convivencia y tranquilidad", 
    delito_hom_uet %in% c("DESAPARICIÓN FORZADA DE PERSONAS",
                          "DESAPARICIÓN FORZADA DE PERSONAS",
                          "DESAPARICIÓN COMETIDA POR PARTICULARES") ~ "Desaparición de Personas", 
    delito_hom_uet=="DESPOJO" ~ "Despojo",
    delito_hom_uet %in% c("VIOLENCIA FAMILIAR",  "INCUMPLIMIENTO DE LA OBLIGACIÓN ALIMENTARIA", 
                          "EVASIÓN DE OBLIGACIONES ALIMENTARIAS",
                          "RETENCIÓN DE MENORES", 
                          "SUSTRACCIÓN DE MENORES", 
                          "SUSTRACCIÓN DE MENORES O INCAPACES",
                          "RETENCIÓN DE MENORES O INCAPACES", "OMISIÓN DE CUIDADO"
                          ) ~ "Violencia y conflictos familiares",
    delito_hom_uet %in% c("HOMICIDIO CULPOSO POR TRÁNSITO VEHICULAR", 
                          "ATAQUES A LAS VÍAS DE COMUNICACIÓN Y A LOS MEDIOS DE TRANSPORTE"
                          
                          ) ~"Tránsito vehicular",
    delito_hom_uet %in% "LESIONES DOLOSAS" ~"Violencia física intencional",
    delito_hom_uet=="LESIONES DOLOSAS" ~ "Lesiones dolosas", 
    delito_hom_uet %in% c("LESIONES CULPOSAS", 
                          "HOMICIDIO CULPOSO",
                          "DAÑOS A LA PROPIEDAD", "DAÑO EN LOS BIENES"
                          )~"Muertes, lesiones y daños no intencionales",
    
    delito_hom_uet %in% c("FRAUDE", 
                          "ABUSO DE CONFIANZA", "USURPACIÓN DE IDENTIDAD",
                          "UTILIZACIÓN INDEBIDA DE TARJETA", 
                          "USURPACIÓN DE PROFESIÓN") ~"Defraudación",
    delito_hom_uet=="EXTORSIÓN" ~ "Extorsión",
    grepl("SECU", delito_hom_uet) ~ "Secuestro", 
    grepl("NARCO", delito_hom_uet) ~ "Posesión y venta de drogas", 
    grepl("ABUSO SE|ACOSO|VIOLACI|ESTUPRO|CONTRA LA INTIMID", delito_hom_uet) &
      !grepl("TENTA|DEP", delito_hom_uet)~ "Violencia sexual", 
    grepl("TORTUR", delito_hom_uet) ~ "Tortura",
    grepl("COHECH|TR.FICO|PECUL|ENRI", delito_hom_uet) ~ "Corrupción",
    delito_hom_uet=="DELITOS ELECTORALES" ~"Delitos Electorales", 
    delito_hom_uet=="TRATA DE PERSONAS" ~"Trata de personas", 
    delito_hom_uet %in% c("DELITOS CONTRA EL AMBIENTE") ~ "Ambientales",
    delito_hom_uet %in% c("DELITOS COMETIDOS POR ACTOS DE MALTRATO O CRUELDAD EN CONTRA DE ANIMALES NO HUMANOS") ~ "Maltrato Animal",

    delito_alto_impacto %in% "ROBO DE VEHÍCULO CON Y SIN VIOLENCIA" |
     delito_hom_uet %in% c("ENCUBRIMIENTO POR RECEPTACIÓN","ENCUBRIMIENTO" ) ~ "Robo de vehículo", 
    delito_hom_uet %in% "EXTORSIÓN" ~ "Extorsión", 
    delito_hom_uet %in% "CONTRA LA INTIMIDAD SEXUAL" ~ "Contra la intimidad sexual", 
     
    grepl("CORRUPCI.N DE MENO", delito_hom_uet) |
      delito_hom_uet %in% c("PORNOGRAFÍA INFANTIL", "PORNOGRAFÍA") ~"Afectación a las infancias", 
    
    grepl("ROBO", delito_hom_uet) & 
      grepl("CON VIOLENCIA|C/V", delito_hom_uet) ~ "Robos con violencia", 
    grepl("ROBO", delito_hom_uet) & 
      grepl("SIN VIOLENCIA|S/V", delito_hom_uet) ~ "Robos sin violencia", 
    
    
    
    T ~ str_to_sentence(delito_hom_uet)
  )
  )

# sentencias <- sentencias %>% 
#   mutate(tipo_delito=case_when(
#     grepl("FEMIN", delito_hom_uet) & !grepl("TENTA", delito_hom_uet) ~ "Feminicidio", 
#     delitos_alto_impacto=="HOMICIDIO DOLOSO" ~ "Homicidio doloso", 
#     delito_hom_uet=="LESIONES DOLOSAS" ~ "Lesiones dolosas", 
#     grepl("CASA", delito_hom_uet) ~ "Robo a casa habitación", 
#     grepl("SECU", delito_hom_uet) ~ "Secuestro", 
#     grepl("NARCO", delito_hom_uet) ~ "Narcomenudeo", 
#     delito_hom_uet=="VIOLACIÓN" ~ "Violación", 
#     delito_hom_uet=="ACOSO SEXUAL" ~ "Acoso sexual", 
#     delito_hom_uet=="ESTUPRO" ~"Estupro", 
#     delito_hom_uet=="TRATA DE PERSONAS" ~"Trata de personas", 
#     delito_hom_uet %in% c( "RETENCIÓN DE MENORES", 
#                            "SUSTRACCIÓN DE MENORES", 
#                            "SUSTRACCIÓN DE MENORES O INCAPACES",
#                            "RETENCIÓN DE MENORES O INCAPACES" 
#                            ) ~ "Retención y sustracción de menores",
#     delito_hom_uet %in% c("DESAPARICIÓN FORZADA DE PERSONAS",
#                           "DESAPARICIÓN COMETIDA POR PARTICULARES") ~ "Desaparición forzada", 
#     delito_hom_uet %in% "OPERACIONES CON RECURSOS DE PROCEDENCIA ILÍCITA" ~ "ORPI", 
#     delito_hom_uet %in% "EVASIÓN DE PRESOS" ~ "Evasión de presos", 
#     delitos_alto_impacto %in% "ROBO DE VEHÍCULO CON Y SIN VIOLENCIA" ~ "Robo de vehículo", 
#     delito_hom_uet %in% "EXTORSIÓN" ~ "Extorsión", 
#     grepl("LIBERTAD", delito_hom_uet) & !grepl("TENTA", delito_hom_uet) ~ "Privación ilegal de la libertad", 
#     delito_hom_uet %in% "ABUSO SEXUAL" ~ "Abuso sexual", 
#     delito_hom_uet %in% "VIOLENCIA FAMILIAR" ~ "Violencia familiar",
#     delito_hom_uet %in% "CONTRA LA INTIMIDAD SEXUAL" ~ "Contra la intimidad sexual", 
#     delito_hom_uet %in% c("PORNOGRAFÍA INFANTIL", "PORNOGRAFÍA") ~ "Pornografía infantil",
#     grepl("CORRUPCI.N DE MENO", delito_hom_uet) ~ "Corrupción de menores", 
#     grepl("TORTUR", delito_hom_uet) ~ "Tortura",
#     grepl("ENRIQUE", delito_hom_uet) ~ "Enriquecimiento ilícito", 
#     grepl("SABOTAJE", delito_hom_uet) ~ "Sabotaje", 
#     grepl("ROBO", delito_hom_uet) & 
#       grepl("CON VIOLENCIA|C/V", delito_hom_uet) ~ "Robo de vehículo con violencia", 
#     
#     
#     
#     T ~ str_to_sentence(delito_hom_uet)
#   ))

graf_efi_ministerial <- function(
    datos=data, 
    fecha_inicio_global=fecha_inicio_global,
    fecha_lim=fecha_lim, 
    delito_elegido=delito 
    #tipo=c("Vinculaciones", "Sentencias")
){
  
  carpetas <- datos %>% 
    group_by(fecha_inicio=floor_date(fecha_inicio, "1 year")) %>% 
    summarise(Carpetas=n()) 
  
  data_id <- datos%>% drop_na(id_ap) %>% pull(id_ap)
  
  
  vinculaciones_flagr <- flagrancias %>% 
      filter(id_ap %in% data_id) %>% 
    group_by(fecha_inicio=floor_date(fecha_inicio, "1 year")) %>% 
    summarise(Vinculados=sum(vinculacion_a_proceso_por_persona, na.rm = T))
  
  
  vinculaciones_ord <- ordenes %>% 
    filter(cat_mp2 %in% delito_elegido) %>% 
    group_by(fecha_inicio=floor_date(fecha_cumplida, "1 year")) %>% 
    summarise(Vinculados=n())
  
  total_vinc <- bind_rows(vinculaciones_flagr, vinculaciones_ord) %>% 
    group_by(fecha_inicio) %>% 
    summarise(Vinculaciones=sum(Vinculados)) %>% 
    filter(fecha_inicio<=fecha_lim)
  
  # total_sen <- sentencias %>% 
  #   filter(tipo_delito %in% delito_elegido, 
  #          fallo_homologado=="CONDENATORIA"
  #          ) %>% 
  #   group_by(fecha_inicio=floor_date(fecha_de_fallo, "1 year")) %>% 
  #   summarise(Sentencias=n())
  
  
  gr_trend_vinc <- carpetas %>% 
    left_join(total_vinc, by="fecha_inicio") %>% 
    gather(tipo, Total, Carpetas:Vinculaciones) %>% 
    mutate(fecha_inicio=as_date(fecha_inicio)) %>% 
    complete(fecha_inicio=seq.Date(as_date(fecha_inicio_global), 
                                   as_date(fecha_lim), "1 year"), 
             tipo=c("Carpetas", "Vinculaciones"),
             fill=list(Total=0)
    ) %>% 
    ggplot(aes(fecha_inicio, Total, color=tipo)) +
    geom_point(alpha=.7) +
    geom_smooth(se=F, ) + theme_light() + tema_fgj +
    geom_label(aes(label=comma(Total)), 
               size=5, show.legend = F
               )+
    # geom_smooth(se=F, color=colores[2], method = "lm") +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
    labs(x="Fecha de inicio", y="Total", 
         title = paste0("Total de vinculaciones y carpetas por ", knitr::combine_words(delito_elegido, and = " y ")), 
         subtitle = paste0("Desde ",  format(as_date(fecha_inicio_global), "%B de %Y"), " a ",
                           format(as_date(fecha_lim), "%B de %Y"))) +
    theme(axis.text.x = element_text(angle = 90)) +
    tema_fgj + scale_y_continuous(labels = comma) +
    theme(legend.position = "bottom") +
    scale_color_manual(values = colores[7:9])
  
  
  # gr_trend_sen <- carpetas %>% 
  #   left_join(total_sen, by="fecha_inicio") %>% 
  #   gather(tipo, Total, Carpetas:Sentencias) %>% 
  #   mutate(fecha_inicio=as_date(fecha_inicio)) %>% 
  #   complete(fecha_inicio=seq.Date(as_date(fecha_inicio_global), 
  #                                  as_date(fecha_lim), "1 year"), 
  #            tipo=c("Carpetas", "Sentencias"),
  #            fill=list(Total=0)
  #   ) %>% 
  #   ggplot(aes(fecha_inicio, Total, color=tipo)) +
  #   geom_point(alpha=.7) +
  #   geom_smooth(se=F, ) + theme_light() + tema_fgj +
  #   # geom_smooth(se=F, color=colores[2], method = "lm") +
  #   scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  #   geom_label(aes(label=comma(Total)), 
  #              size=5, show.legend = F
  #   )+
  #   labs(x="Fecha", y="Total", 
  #        title = paste0("Total de sentencias y carpetas por ", knitr::combine_words(delito_elegido, and = " y ")), 
  #        subtitle = paste0("Desde ",  format(as_date(fecha_inicio_global), "%B de %Y"), " a ",
  #                          format(as_date(fecha_lim), "%B de %Y"))) +
  #   theme(axis.text.x = element_text(angle = 90)) +
  #   tema_fgj + 
  #   theme(legend.position = "bottom") +
  #   scale_color_manual(values = c(colores[7], colores[4]))
  # 
  # gr_trend <- cowplot::plot_grid(gr_trend_vinc, gr_trend_sen, nrow = 2
  #                    
  #                    )
  
  return(gr_trend_vinc)
  
  
}


graf_efi_ministerial_traza <- function(
    datos=data, 
    fecha_inicio_global=fecha_inicio_global,
    fecha_lim=fecha_lim, 
    delito_elegido=delito 
    #tipo=c("Vinculaciones", "Sentencias")
){
  
  carpetas <- datos %>% 
    group_by(fecha_inicio=floor_date(fecha_inicio, "1 year")) %>% 
    summarise(Carpetas=n()) 
  
  data_id <- datos%>% drop_na(id_ap) %>% pull(id_ap)
  
  
  vinculaciones_flagr <- flagrancias %>% 
    filter(id_ap %in% data_id) %>% 
    group_by(fecha_inicio=floor_date(fecha_inicio, "1 year")) %>% 
    summarise(Vinculados=sum(vinculacion_a_proceso_por_persona, na.rm = T))
  
  
  vinculaciones_ord <- ordenes %>% 
    mutate(id_ap=as.integer(id_carpeta_uet)) %>% drop_na(id_ap) %>% 
    filter(id_ap %in% data_id) %>% 
    group_by(fecha_inicio=floor_date(fecha_cumplida, "1 year")) %>% 
    summarise(Vinculados=n())
  
  total_vinc <- bind_rows(vinculaciones_flagr, vinculaciones_ord) %>% 
    group_by(fecha_inicio) %>% 
    summarise(Vinculaciones=sum(Vinculados)) %>% 
    filter(fecha_inicio<=fecha_lim)
  
  # total_sen <- sentencias %>% 
  #   filter(tipo_delito %in% delito_elegido, 
  #          fallo_homologado=="CONDENATORIA"
  #          ) %>% 
  #   group_by(fecha_inicio=floor_date(fecha_de_fallo, "1 year")) %>% 
  #   summarise(Sentencias=n())
  
  
  gr_trend_vinc <- carpetas %>% 
    left_join(total_vinc, by="fecha_inicio") %>% 
    gather(tipo, Total, Carpetas:Vinculaciones) %>% 
    mutate(fecha_inicio=as_date(fecha_inicio)) %>% 
    complete(fecha_inicio=seq.Date(as_date(fecha_inicio_global), 
                                   as_date(fecha_lim), "1 year"), 
             tipo=c("Carpetas", "Vinculaciones"),
             fill=list(Total=0)
    ) %>% 
    ggplot(aes(fecha_inicio, Total, color=tipo)) +
    geom_point(alpha=.7) +
    geom_smooth(se=F, ) + theme_light() + tema_fgj +
    geom_label(aes(label=comma(Total)), 
               size=5, show.legend = F
    )+
    # geom_smooth(se=F, color=colores[2], method = "lm") +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
    labs(x="Fecha de inicio", y="Total", 
         title = paste0("Total de vinculaciones y carpetas por ", knitr::combine_words(delito_elegido, and = " y ")), 
         subtitle = paste0("Desde ",  format(as_date(fecha_inicio_global), "%B de %Y"), " a ",
                           format(as_date(fecha_lim), "%B de %Y"))) +
    theme(axis.text.x = element_text(angle = 90)) +
    tema_fgj + scale_y_continuous(labels = comma) +
    theme(legend.position = "bottom") +
    scale_color_manual(values = colores[7:9])
  
  
  # gr_trend_sen <- carpetas %>% 
  #   left_join(total_sen, by="fecha_inicio") %>% 
  #   gather(tipo, Total, Carpetas:Sentencias) %>% 
  #   mutate(fecha_inicio=as_date(fecha_inicio)) %>% 
  #   complete(fecha_inicio=seq.Date(as_date(fecha_inicio_global), 
  #                                  as_date(fecha_lim), "1 year"), 
  #            tipo=c("Carpetas", "Sentencias"),
  #            fill=list(Total=0)
  #   ) %>% 
  #   ggplot(aes(fecha_inicio, Total, color=tipo)) +
  #   geom_point(alpha=.7) +
  #   geom_smooth(se=F, ) + theme_light() + tema_fgj +
  #   # geom_smooth(se=F, color=colores[2], method = "lm") +
  #   scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  #   geom_label(aes(label=comma(Total)), 
  #              size=5, show.legend = F
  #   )+
  #   labs(x="Fecha", y="Total", 
  #        title = paste0("Total de sentencias y carpetas por ", knitr::combine_words(delito_elegido, and = " y ")), 
  #        subtitle = paste0("Desde ",  format(as_date(fecha_inicio_global), "%B de %Y"), " a ",
  #                          format(as_date(fecha_lim), "%B de %Y"))) +
  #   theme(axis.text.x = element_text(angle = 90)) +
  #   tema_fgj + 
  #   theme(legend.position = "bottom") +
  #   scale_color_manual(values = c(colores[7], colores[4]))
  # 
  # gr_trend <- cowplot::plot_grid(gr_trend_vinc, gr_trend_sen, nrow = 2
  #                    
  #                    )
  
  return(gr_trend_vinc)
  
  
}




gr_efi_hom <- graf_efi_ministerial(datos = data_bruto %>% 
                       filter(cat_mp2=="Robo de vehículo"), 
                     fecha_inicio_global = fecha_inicio_global, 
                     fecha_lim = fecha_lim, 
                     delito ="Robo de vehículo"
                     
                       )

gr_efi_femi <- graf_efi_ministerial(datos = data_bruto %>% 
                       filter(tipo_delito=="Feminicidio"), 
                     fecha_inicio_global = fecha_inicio_global, 
                     fecha_lim = fecha_lim, 
                     delito ="Feminicidio"
                     
)


gr_efi_altos <- graf_efi_ministerial(datos = data_bruto %>% 
                                      filter(tipo_delito %in% c("Feminicidio", "Homicidio doloso", 
                                                                "Desaparición forzada"
                                                                )), 
                                    fecha_inicio_global = fecha_inicio_global, 
                                    fecha_lim = fecha_lim, 
                                    delito =c("Feminicidio", "Homicidio doloso", 
                                              "Desaparición forzada")
)

ggsave(plot = gr_efi_altos, 
       "./graficas_incidencia/eficiencia_min/gr_eficiencia_alta.svg", 
       width = 12, height = 6
       )


#eficiencia femin y hom doloso
gr_efi_fem_hom<- graf_efi_ministerial(datos = data_bruto %>% 
                                       filter(tipo_delito %in% c("Feminicidio", "Homicidio doloso"
                                       )), 
                                     fecha_inicio_global = fecha_inicio_global, 
                                     fecha_lim = fecha_lim, 
                                     delito =c("Feminicidio", "Homicidio doloso")
)

ggsave(plot = gr_efi_altos, 
       "./graficas_incidencia/eficiencia_min/gr_efi_fem_hom.svg", 
       width = 12, height = 6
)

gr_efi <- list()

for (i in 1:length(delitos_ppp)) {

  
  gr_efi[[i]] <- graf_efi_ministerial(datos = data_bruto %>% 
                         filter(cat_mp2==delitos_ppp[[i]]), 
                       fecha_inicio_global = fecha_inicio_global, 
                       fecha_lim = fecha_lim, 
                       delito =delitos_ppp[[i]]
                       
  )
  
  
  ggsave(plot = gr_efi[[i]], 
         paste0("./graficas_incidencia_la_revancha/eficiencia_min/", delitos_ppp[i], ".svg"), 
         width = 14, height = 7
  )
  
  
}



#grafíca de eficiencia ministerial todos los delitos de ppp
carpetas <- data_bruto %>% 
  filter(tipo_delito %in% delito_elegido) %>% 
  group_by(fecha_inicio=floor_date(fecha_inicio, "1 year")) %>% 
  summarise(Carpetas=n()) 

data_id <- data_bruto %>% 
  filter(tipo_delito %in% delito_elegido)%>% 
  drop_na(id_ap) %>% pull(id_ap)


vinculaciones_flagr <- flagrancias %>% 
  filter(id_ap %in% data_id) %>% 
  group_by(fecha_inicio=floor_date(fecha_inicio, "1 year")) %>% 
  summarise(Vinculados=sum(vinculacion_a_proceso_por_persona, na.rm = T))


vinculaciones_ord <- ordenes %>% 
  filter(tipo_delito %in% delito_elegido) %>% 
  group_by(fecha_inicio=floor_date(fecha_cumplida, "1 year")) %>% 
  summarise(Vinculados=n())

total_vinc <- bind_rows(vinculaciones_flagr, vinculaciones_ord) %>% 
  group_by(fecha_inicio) %>% 
  summarise(Vinculaciones=sum(Vinculados)) %>% 
  filter(fecha_inicio<=fecha_lim)

total_sen <- sentencias %>% 
  filter(tipo_delito %in% delito_elegido, 
         fallo_homologado=="CONDENATORIA"
  ) %>% 
  group_by(fecha_inicio=floor_date(fecha_de_fallo, "1 year")) %>% 
  summarise(Sentencias=n())


gr_trend_vinc <- carpetas %>% 
  left_join(total_vinc, by="fecha_inicio") %>% 
  gather(tipo, Total, Carpetas:Vinculaciones) %>% 
  mutate(fecha_inicio=as_date(fecha_inicio)) %>% 
  complete(fecha_inicio=seq.Date(as_date(fecha_inicio_global), 
                                 as_date(fecha_lim), "1 year"), 
           tipo=c("Carpetas", "Vinculaciones"),
           fill=list(Total=0)
  ) %>% 
  ggplot(aes(fecha_inicio, Total, color=tipo)) +
  geom_point(alpha=.7) +
  geom_smooth(se=F, ) + theme_light() + tema_fgj +
  geom_label(aes(label=comma(Total)), 
             size=5, show.legend = F
  )+
  # geom_smooth(se=F, color=colores[2], method = "lm") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(x="Fecha de inicio", y="Promedio diario", 
       title = paste0("Total de vinculaciones y carpetas por ", "delito del plan de política criminal"), 
       subtitle = paste0("Desde ",  format(as_date(fecha_inicio_global), "%B de %Y"), " a ",
                         format(as_date(fecha_lim), "%B de %Y"))) +
  theme(axis.text.x = element_text(angle = 90)) +
  tema_fgj + scale_y_continuous(labels = comma) +
  theme(legend.position = "bottom") +
  scale_color_manual(values = colores[7:9])


gr_trend_sen <- carpetas %>% 
  left_join(total_sen, by="fecha_inicio") %>% 
  gather(tipo, Total, Carpetas:Sentencias) %>% 
  mutate(fecha_inicio=as_date(fecha_inicio)) %>% 
  complete(fecha_inicio=seq.Date(as_date(fecha_inicio_global), 
                                 as_date(fecha_lim), "1 year"), 
           tipo=c("Carpetas", "Sentencias"),
           fill=list(Total=0)
  ) %>% 
  ggplot(aes(fecha_inicio, Total, color=tipo)) +
  geom_point(alpha=.7) +
  geom_smooth(se=F, ) + theme_light() + tema_fgj +
  # geom_smooth(se=F, color=colores[2], method = "lm") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  geom_label(aes(label=comma(Total)), 
             size=5, show.legend = F
  )+
  labs(x="Fecha", y="Total", 
       title = paste0("Total de sentencias y carpetas por ", "delitos del plan de política criminal"), 
       subtitle = paste0("Desde ",  format(as_date(fecha_inicio_global), "%B de %Y"), " a ",
                         format(as_date(fecha_lim), "%B de %Y"))) +
  theme(axis.text.x = element_text(angle = 90)) +
  tema_fgj + scale_y_continuous(labels = comma) +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c(colores[7], colores[4]))

gr_trend_ppp <- cowplot::plot_grid(gr_trend_vinc, gr_trend_sen, nrow = 2
                               
)


ggsave(plot = gr_trend_ppp, 
       paste0("./graficas_incidencia/eficiencia_min/", "delitos_ppp", ".svg"), 
       width = 14, height = 7
       )

#####
#datos de masc

masc <- readxl::read_excel("delito_masc.xlsx", sheet = "MASC") %>% 
  clean_names()



gr_trend_masc <- masc %>% 
  filter(masc=="Acuerdo reparatorio") %>% 
  mutate(
    mes_num=case_when(
      mes == "ene" ~ 1,
      mes == "feb" ~ 2,
      mes == "mar" ~ 3,
      mes == "abr" ~ 4,
      mes == "may" ~ 5,
      mes == "jun" ~ 6,
      mes == "jul" ~ 7,
      mes == "ago" ~ 8,
      mes == "sep" ~ 9,
      mes == "oct" ~ 10,
      mes == "nov" ~ 11,
      mes == "dic" ~ 12,
    ),
    fecha_inicio=ymd(paste0(ano, "-", mes_num, "-01"))) %>% 
  # filter(delito=="ROBO DE VEHÍCULO CON Y SIN VIOLENCIA", 
  #        #grepl("CON VIOLE|C/V", modalidad_delito),
  #        year(fecha_inicio) %in% c(2020:2025)) %>% 
  group_by(fecha_inicio) %>% 
  summarise(Total=n()) %>%
  mutate(dias=days_in_month(fecha_inicio), 
         media=Total/dias) %>% 
  ungroup() %>% 
  filter(fecha_inicio>=fecha_inicio_global) %>% 
  # complete(fecha_inicio=seq.Date(as_date(fecha_inicio_global), 
  #                                as_date(fecha_lim), "1 day"), 
  #          fill=list(Total=0)
  # ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 month")) %>% 
  # summarise(Total=mean(Total)) %>% 
  ggplot(aes(fecha_inicio, media)) +
  geom_point(alpha=.7, color=colores[1]) +
  geom_smooth(se=F, color=colores[7]) + theme_light() + tema_fgj +
  # geom_smooth(se=F, color=colores[2], method = "lm") +
  scale_x_date(date_labels = "%b-%y", date_breaks = "3 month") +
  labs(x="Fecha de inicio", y="Promedio diario", 
       title = paste0("Promedio diario de carpetas por acuerdos reparatorios"), 
       subtitle = paste0("Desde ",  format(as_date(fecha_inicio_global), "%B de %Y"), " a ",
                         format(as_date(fecha_lim), "%B de %Y"))) +
  theme(axis.text.x = element_text(angle = 90)) 


ggsave(plot = gr_trend_masc, 
       paste0("./graficas_incidencia/", "acuerdos_reparatorios", ".svg"), 
       width = 14, height = 7
)
#####

ordenes <- ordenes %>% 
  mutate(tipo_delito=case_when(
    grepl("FEM", delito_hom_uet) ~ "Feminicidio", 
    delito_hom_uet %in% c("HOMICIDIO DOLOSO") ~ "Homicidio doloso",
    delito_hom_uet %in% "ROBO DE VEHÍCULO CON VIOLENCIA" ~ "Robo de vehículo con violencia",
    delito_hom_uet %in% "ROBO DE VEHÍCULO SIN VIOLENCIA" ~ "Robo de vehículo sin violencia",
    delito_hom_uet %in% c("ENCUBRIMIENTO POR RECEPTACIÓN","ENCUBRIMIENTO" ) ~ "Encubrimiento",
    delito_hom_uet %in% "SECUESTRO" ~ "Secuestro", 
    delito_hom_uet %in% "SECUESTRO EXPRES" ~ "Secuestro express",
    delito_hom_uet %in% "LESIONES DOLOSAS" ~ "Lesiones dolosas",
    delito_hom_uet %in% "EXTORSIÓN" ~ "Extorsión",
    delito_hom_uet %in% "ROBO A CASA HABITACIÓN CON VIOLENCIA" ~ "Robo a casa habitación con violencia",
    delito_hom_uet %in% "ROBO A CASA HABITACIÓN" ~ "Robo a casa habitación sin violencia",
    delito_hom_uet %in% "NARCOMENUDEO POSESIÓN SIMPLE" ~ "Narcomenudeo simple",
    delito_hom_uet %in% "NARCOMENUDEO POSESIÓN CON FINES DE VENTA, COMERCIO Y SUMINISTRO" ~ "Narcomenudeo con fines de venta",
    delito_hom_uet %in% "DESPOJO" ~ "Despojo", 
    delito_hom_uet %in% "COHECHO" ~ "Cohecho",
    T ~ str_to_sentence(delito_hom_uet) 
  ))
#####
#vamos a hacer gráficas especiales de cada uno de las categorias
#graficas de "Muertes violentas"
#línea 1: incidencia de homicidio doloso (incluir homicidio doloso de mujeres)
# línea 2: incidencia de feminicidio
tabla_muertes_violentas <- list()

hom_fem <- data_bruto %>% 
  filter(cat_mp2=="Muertes violentas") %>% 
  mutate(tipo=case_when(
    grepl("FEM", modalidad_delito) ~ "Feminicidio", 
    T ~ "Homicidio doloso"
  )) %>% group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date(fecha_inicio_global), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(.$tipo),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 month"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  ggplot(aes(fecha_inicio, Total, color=tipo)) +
  geom_point(alpha=.7#, color=colores[1]
  ) +
  geom_smooth(se=F#, color=colores[7]
  ) + theme_light() + tema_fgj +
  # geom_smooth(se=F, color=colores[2], method = "lm") +
  scale_x_date(date_labels = "%b\n%y", date_breaks = "6 month") +
  labs(x="Fecha de inicio", y="Promedio diario", 
       color="",
       title = paste0("Promedio diario de carpetas por muertes violentas según delito"), 
       subtitle = paste0("Desde ",  format(as_date("2019-01-01"), "%B de %Y"), " a ",
                         format(as_date(fecha_lim), "%B de %Y"))) +
  scale_color_manual(values = colores[2:1]) +
  theme(#axis.text.x = element_text(angle = 90), 
        legend.position = "bottom"
  )


ggsave(plot = hom_fem, 
       "./graficas_incidencia_la_revancha/especiales/Muertes violentas/gr_muertes_violentas_tipo.svg", width = 12, height = 6
)

tabla_muertes_violentas[[1]] <- data_bruto %>% 
  filter(cat_mp2=="Muertes violentas") %>% 
  mutate(tipo=case_when(
    grepl("FEM", modalidad_delito) ~ "Feminicidio", 
    T ~ "Homicidio doloso"
  )) %>% group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date(fecha_inicio_global), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(.$tipo),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 year"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  mutate(nombre="comparacion homi vs femi")


#homicidios por tipo de arma (una línea por tipo. Dejar como categorías arma de fuego, 
#arma blanca y otros (agrupar en otros los demás instrumentos de comisión) .
#Esta gráfica no incluye feminicidios
hom_arma <- data_bruto %>% 
  filter(cat_mp2=="Muertes violentas", 
         del_mp=="Homicidio doloso") %>% 
  mutate(tipo=case_when(
    grepl("FUEGO", modalidad_delito) ~ "Arma de fuego", 
    grepl("BLANCA", modalidad_delito) ~ "Arma blanca", 
    T ~ "Otros"
  )) %>%
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date(fecha_inicio_global), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(.$tipo),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 month"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  ggplot(aes(fecha_inicio, Total, color=tipo)) +
  geom_point(alpha=.7#, color=colores[1]
  ) +
  geom_smooth(se=F#, color=colores[7]
  ) + theme_light() + tema_fgj +
  # geom_smooth(se=F, color=colores[2], method = "lm") +
  scale_x_date(date_labels = "%b\n%y", date_breaks = "6 month") +
  labs(x="Fecha de inicio", y="Promedio diario", 
       color="",
       title = paste0("Promedio diario de carpetas por homicidio según clasificación"), 
       subtitle = paste0("Desde ",  format(as_date("2019-01-01"), "%B de %Y"), " a ",
                         format(as_date(fecha_lim), "%B de %Y"))) +
  scale_color_manual(values = colores[3:1]) +
  theme(#axis.text.x = element_text(angle = 90), 
    legend.position = "bottom"
  )


ggsave(plot = hom_arma, 
       "./graficas_incidencia_la_revancha/especiales/Muertes violentas/gr_hom_arma.svg", width = 12, height = 6
)

tabla_muertes_violentas[[2]] <- data_bruto %>% 
  filter(cat_mp2=="Muertes violentas", 
         del_mp=="Homicidio doloso") %>% 
  mutate(tipo=case_when(
    grepl("FUEGO", modalidad_delito) ~ "Arma de fuego", 
    grepl("BLANCA", modalidad_delito) ~ "Arma blanca", 
    T ~ "Otros"
  )) %>%
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date(fecha_inicio_global), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(.$tipo),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 year"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  mutate(nombre="comparacion armas en homicidios")
###
# hom_contextual <- read.csv("homicidios_contextual.csv")

#línea 1: incidencia por "Homicidios de alto impacto" (ataque directo, robo, grupos delitctivos)
# línea 2: incidencia por "Homicidios aislados" (riña, personales, aparente violencia de género)
# Excluir los que no tenemos información del móvil. Gráfica desde 2022 porque antes no tenemos información
data_homicidios <- read_excel("~/R/fiscalia/bases/BASE HOMICIDIOS 06 DE ENERO 2025.xlsx")

data_homicidios <- data_homicidios %>% clean_names() %>% 
  mutate(tipo=case_when(
    clasificacion_contextual %in% c("Ejecución","Grupos delictivos" ,
                                    "Robo",
                                    "Ajuste de cuentas entre delincuentes") ~"Alto impacto", 
    clasificacion_contextual %in% "Desconocido" ~ "Desconocido",
    T ~ "Aislados"
  )) %>% 
  filter(tipo!="Desconocido")
hom <- data_bruto %>% filter(delito=="HOMICIDIO DOLOSO", 
                       grepl("HOM", modalidad_delito), 
                       fecha_inicio>="2022-01-01"
) %>% 
  left_join(data_homicidios %>% 
              select(-fecha_de_inicio), by="id_ap") 

gr_hom_tipo <- hom %>% 
  # filter(delito=="ROBO DE VEHÍCULO CON Y SIN VIOLENCIA", 
  #        #grepl("CON VIOLE|C/V", modalidad_delito),
  #        year(fecha_inicio) %in% c(2020:2025)) %>% 
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% drop_na(tipo) %>% 
  complete(fecha_inicio=seq.Date(as_date("2022-01-01"), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(.$tipo),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 month"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  ggplot(aes(fecha_inicio, Total, color=tipo)) +
  geom_point(alpha=.7#, color=colores[1]
  ) +
  geom_smooth(se=F#, color=colores[7]
  ) + theme_light() + tema_fgj +
  # geom_smooth(se=F, color=colores[2], method = "lm") +
  scale_x_date(date_labels = "%b\n%y", date_breaks = "6 month") +
  labs(x="Fecha de inicio", y="Promedio diario", 
       title = paste0("Promedio diario de carpetas por homicidio según clasificación"), 
       subtitle = paste0("Desde ",  format(as_date("2022-01-01"), "%B de %Y"), " a ",
                         format(as_date(fecha_lim), "%B de %Y"))) +
  scale_color_manual(values = colores[1:2]) +
  theme(axis.text.x = element_text(angle = 90), 
        legend.position = "bottom"
  )


ggsave(plot = gr_hom_tipo, 
       "./graficas_incidencia_la_revancha/especiales/Muertes violentas/gr_hom_clasificacion.svg", width = 12, height = 6
)

tabla_muertes_violentas[[3]] <- hom %>% 
  # filter(delito=="ROBO DE VEHÍCULO CON Y SIN VIOLENCIA", 
  #        #grepl("CON VIOLE|C/V", modalidad_delito),
  #        year(fecha_inicio) %in% c(2020:2025)) %>% 
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% drop_na(tipo) %>% 
  complete(fecha_inicio=seq.Date(as_date("2022-01-01"), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(.$tipo),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 year"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  mutate(nombre="comparacion clasificacion")

total_tablas_muertes_violentas <- bind_rows(tabla_muertes_violentas)

total_tablas_muertes_violentas %>% 
  write.csv("./graficas_incidencia_la_revancha/especiales/Muertes violentas/tablas_muertes_violentas.csv")
####
#eficiencia ministerial
#vamos a modificar la función para que filtre por tipo_delito en vez de cat_mp2
graf_efi_ministerial <- function(
    datos=data, 
    fecha_inicio_global=fecha_inicio_global,
    fecha_lim=fecha_lim, 
    delito_elegido=delito 
    #tipo=c("Vinculaciones", "Sentencias")
){
  
  carpetas <- datos %>% 
    group_by(fecha_inicio=floor_date(fecha_inicio, "1 year")) %>% 
    summarise(Carpetas=n()) 
  
  data_id <- datos%>% drop_na(id_ap) %>% pull(id_ap)
  
  
  vinculaciones_flagr <- flagrancias %>% 
    filter(id_ap %in% data_id) %>% 
    group_by(fecha_inicio=floor_date(fecha_inicio, "1 year")) %>% 
    summarise(Vinculados=sum(vinculacion_a_proceso_por_persona, na.rm = T))
  
  
  vinculaciones_ord <- ordenes %>% 
    filter(tipo_delito %in% delito_elegido) %>% 
    group_by(fecha_inicio=floor_date(fecha_cumplida, "1 year")) %>% 
    summarise(Vinculados=n())
  
  total_vinc <- bind_rows(vinculaciones_flagr, vinculaciones_ord) %>% 
    group_by(fecha_inicio) %>% 
    summarise(Vinculaciones=sum(Vinculados)) %>% 
    filter(fecha_inicio<=fecha_lim)
  
  # total_sen <- sentencias %>% 
  #   filter(tipo_delito %in% delito_elegido, 
  #          fallo_homologado=="CONDENATORIA"
  #          ) %>% 
  #   group_by(fecha_inicio=floor_date(fecha_de_fallo, "1 year")) %>% 
  #   summarise(Sentencias=n())
  
  
  gr_trend_vinc <- carpetas %>% 
    left_join(total_vinc, by="fecha_inicio") %>% 
    gather(tipo, Total, Carpetas:Vinculaciones) %>% 
    mutate(fecha_inicio=as_date(fecha_inicio)) %>% 
    complete(fecha_inicio=seq.Date(as_date(fecha_inicio_global), 
                                   as_date(fecha_lim), "1 year"), 
             tipo=c("Carpetas", "Vinculaciones"),
             fill=list(Total=0)
    ) %>% 
    ggplot(aes(fecha_inicio, Total, color=tipo)) +
    geom_point(alpha=.7) +
    geom_smooth(se=F, ) + theme_light() + tema_fgj +
    geom_label(aes(label=comma(Total)), 
               size=5, show.legend = F
    )+
    # geom_smooth(se=F, color=colores[2], method = "lm") +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
    labs(x="Fecha de inicio", y="Total", color="",
         title = paste0("Total de vinculaciones y carpetas por ", knitr::combine_words(delito_elegido, and = " y ")), 
         subtitle = paste0("Desde ",  format(as_date(fecha_inicio_global), "%B de %Y"), " a ",
                           format(as_date(fecha_lim), "%B de %Y"))) +
    theme(axis.text.x = element_text(angle = 90)) +
    tema_fgj + scale_y_continuous(labels = comma) +
    theme(legend.position = "bottom") +
    scale_color_manual(values = colores[7:9])
  
  
  # gr_trend_sen <- carpetas %>% 
  #   left_join(total_sen, by="fecha_inicio") %>% 
  #   gather(tipo, Total, Carpetas:Sentencias) %>% 
  #   mutate(fecha_inicio=as_date(fecha_inicio)) %>% 
  #   complete(fecha_inicio=seq.Date(as_date(fecha_inicio_global), 
  #                                  as_date(fecha_lim), "1 year"), 
  #            tipo=c("Carpetas", "Sentencias"),
  #            fill=list(Total=0)
  #   ) %>% 
  #   ggplot(aes(fecha_inicio, Total, color=tipo)) +
  #   geom_point(alpha=.7) +
  #   geom_smooth(se=F, ) + theme_light() + tema_fgj +
  #   # geom_smooth(se=F, color=colores[2], method = "lm") +
  #   scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  #   geom_label(aes(label=comma(Total)), 
  #              size=5, show.legend = F
  #   )+
  #   labs(x="Fecha", y="Total", 
  #        title = paste0("Total de sentencias y carpetas por ", knitr::combine_words(delito_elegido, and = " y ")), 
  #        subtitle = paste0("Desde ",  format(as_date(fecha_inicio_global), "%B de %Y"), " a ",
  #                          format(as_date(fecha_lim), "%B de %Y"))) +
  #   theme(axis.text.x = element_text(angle = 90)) +
  #   tema_fgj + 
  #   theme(legend.position = "bottom") +
  #   scale_color_manual(values = c(colores[7], colores[4]))
  # 
  # gr_trend <- cowplot::plot_grid(gr_trend_vinc, gr_trend_sen, nrow = 2
  #                    
  #                    )
  
  return(gr_trend_vinc)
  
  
}

gr_efi_hom <- graf_efi_ministerial(datos = data_bruto %>% 
                                     filter(cat_mp2=="Muertes violentas", 
                                            grepl("HOM", modalidad_delito)
                                            ), 
                                   fecha_inicio_global = fecha_inicio_global, 
                                   fecha_lim = fecha_lim, 
                                   delito ="Homicidio doloso"
                                   
)


ggsave(plot = gr_efi_hom, 
       "./graficas_incidencia_la_revancha/especiales/Muertes violentas/gr_efi_hom.svg", width = 12, height = 6
)

#eficiencia feminicidio
gr_efi_fem <- graf_efi_ministerial(datos = data_bruto %>% 
                                     filter(cat_mp2=="Muertes violentas", 
                                            grepl("FEM", modalidad_delito)
                                     ), 
                                   fecha_inicio_global = fecha_inicio_global, 
                                   fecha_lim = fecha_lim, 
                                   delito ="Feminicidio"
                                   
)


ggsave(plot = gr_efi_fem, 
       "./graficas_incidencia_la_revancha/especiales/Muertes violentas/gr_efi_fem.svg", width = 12, height = 6
)

#####
#Robo de vehículo
# línea 1: incidencia de robo de vehículo
# línea 2: incidencia de encubrimiento por receptación
veh_tipo <- data_bruto %>% 
  filter(cat_mp2=="Robo de vehículo") %>% 
  mutate(tipo=case_when(
    grepl("ENCU", modalidad_delito) ~ "Encubrimiento", 
    T ~ "Robo de vehículo"
  )) %>%
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date(fecha_inicio_global), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(.$tipo),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 month"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  ggplot(aes(fecha_inicio, Total, color=tipo)) +
  geom_point(alpha=.7#, color=colores[1]
  ) +
  geom_smooth(se=F#, color=colores[7]
  ) + theme_light() + tema_fgj +
  # geom_smooth(se=F, color=colores[2], method = "lm") +
  scale_x_date(date_labels = "%b\n%y", date_breaks = "6 month") +
  labs(x="Fecha de inicio", y="Promedio diario", 
       color="",
       title = paste0("Promedio diario de carpetas de robo de vehículo según delito"), 
       subtitle = paste0("Desde ",  format(as_date("2019-01-01"), "%B de %Y"), " a ",
                         format(as_date(fecha_lim), "%B de %Y"))) +
  scale_color_manual(values = colores[2:1]) +
  theme(#axis.text.x = element_text(angle = 90), 
    legend.position = "bottom"
  )


ggsave(plot = veh_tipo, 
       "./graficas_incidencia_la_revancha/especiales/robo de vehiculo/veh_tipo.svg", width = 12, height = 6
)

tabla_veh <- list()

tabla_veh[[1]] <-  data_bruto %>% 
  filter(cat_mp2=="Robo de vehículo") %>% 
  mutate(tipo=case_when(
    grepl("ENCU", modalidad_delito) ~ "Encubrimiento", 
    T ~ "Robo de vehículo"
  )) %>%
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date(fecha_inicio_global), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(.$tipo),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 year"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  mutate(nombre="comparacion robo veh vs encu")

#solo robo de vehículo 
#línea 1: incidencia de robo de vehículo cv
# línea 2: incidencia de robo de vehículo sv
veh_violencia <- data_bruto %>% 
  filter(cat_mp2=="Robo de vehículo",
         !grepl("ENCUB", modalidad_delito)
         ) %>% 
  mutate(tipo=case_when(
    grepl("CON VIOLE|C/V", modalidad_delito) ~ "Con violencia", 
    T ~ "Sin violencia"
  )) %>%
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date(fecha_inicio_global), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(.$tipo),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 month"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  ggplot(aes(fecha_inicio, Total, color=tipo)) +
  geom_point(alpha=.7#, color=colores[1]
  ) +
  geom_smooth(se=F#, color=colores[7]
  ) + theme_light() + tema_fgj +
  # geom_smooth(se=F, color=colores[2], method = "lm") +
  scale_x_date(date_labels = "%b\n%y", date_breaks = "6 month") +
  labs(x="Fecha de inicio", y="Promedio diario", 
       color="",
       title = paste0("Promedio diario de carpetas de robo de vehículo según tipo de violencia"), 
       subtitle = paste0("Desde ",  format(as_date("2019-01-01"), "%B de %Y"), " a ",
                         format(as_date(fecha_lim), "%B de %Y"))) +
  scale_color_manual(values = colores[2:1]) +
  theme(#axis.text.x = element_text(angle = 90), 
    legend.position = "bottom"
  )


ggsave(plot = veh_violencia, 
       "./graficas_incidencia_la_revancha/especiales/robo de vehiculo/veh_violencia.svg", width = 12, height = 6
)


tabla_veh[[2]] <-  data_bruto %>% 
  filter(cat_mp2=="Robo de vehículo",
         !grepl("ENCUB", modalidad_delito)
  ) %>% 
  mutate(tipo=case_when(
    grepl("CON VIOLE|C/V", modalidad_delito) ~ "Con violencia", 
    T ~ "Sin violencia"
  )) %>%
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date(fecha_inicio_global), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(.$tipo),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 month"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  mutate(nombre="robo de vehiculo con viol vs sin viol")

total_tabla_veh <- bind_rows(tabla_veh)
total_tabla_veh %>% 
  write.csv( "./graficas_incidencia_la_revancha/especiales/robo de vehiculo/tabla_vehiculos.csv")
##graficas de eficiencia ministerial
#eficiencia robo de veh con violencia
gr_efi_veh_con <- graf_efi_ministerial(datos = data_bruto %>% 
                                     filter(cat_mp2=="Robo de vehículo",
                                            !grepl("ENCUB", modalidad_delito),
                                            grepl("CON VIOLEN|C/V", modalidad_delito)
                                     ), 
                                   fecha_inicio_global = fecha_inicio_global, 
                                   fecha_lim = fecha_lim, 
                                   delito_elegido ="Robo de vehículo con violencia"
                                   
)


ggsave(plot = gr_efi_veh_con, 
       "./graficas_incidencia_la_revancha/especiales/robo de vehiculo/gr_efi_veh_con.svg", width = 12, height = 6
)


#eficiencia robo de veh sin violencia
gr_efi_veh_sin <- graf_efi_ministerial(datos = data_bruto %>% 
                                         filter(cat_mp2=="Robo de vehículo",
                                                !grepl("ENCUB", modalidad_delito),
                                                grepl("SIN VIOLEN|S/V", modalidad_delito)
                                         ), 
                                       fecha_inicio_global = fecha_inicio_global, 
                                       fecha_lim = fecha_lim, 
                                       delito_elegido ="Robo de vehículo sin violencia"
                                       
)


ggsave(plot = gr_efi_veh_sin, 
       "./graficas_incidencia_la_revancha/especiales/robo de vehiculo/gr_efi_veh_sin.svg", width = 12, height = 6
)

#encubrimiento
gr_efi_veh_encu <- graf_efi_ministerial(datos = data_bruto %>% 
                                         filter(cat_mp2=="Robo de vehículo",
                                                grepl("ENCUB", modalidad_delito)
                                         ), 
                                       fecha_inicio_global = fecha_inicio_global, 
                                       fecha_lim = fecha_lim, 
                                       delito_elegido ="Encubrimiento"
                                       
)


ggsave(plot = gr_efi_veh_encu, 
       "./graficas_incidencia_la_revancha/especiales/robo de vehiculo/gr_efi_veh_encu.svg", width = 12, height = 6
)

#####
#Secuestro
# línea 1: incidencia de secuestro
# línea 2: incidentia de secuestro exprés
#promedio
sec_tipo_prom <- data_bruto %>% 
  filter(cat_mp2=="Secuestro"
  ) %>% 
  mutate(tipo=case_when(
    grepl("EXPRE", modalidad_delito) ~ "Secuestro express", 
    T ~ "Secuestro"
  )) %>%
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date(fecha_inicio_global), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(.$tipo),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 month"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  ggplot(aes(fecha_inicio, Total, color=tipo)) +
  geom_point(alpha=.7#, color=colores[1]
  ) +
  geom_smooth(se=F#, color=colores[7]
  ) + theme_light() + tema_fgj +
  # geom_smooth(se=F, color=colores[2], method = "lm") +
  scale_x_date(date_labels = "%b\n%y", date_breaks = "6 month") +
  labs(x="Fecha de inicio", y="Promedio diario", 
       color="",
       title = paste0("Promedio diario de carpetas de secuestro por tipo"), 
       subtitle = paste0("Desde ",  format(as_date("2019-01-01"), "%B de %Y"), " a ",
                         format(as_date(fecha_lim), "%B de %Y"))) +
  scale_color_manual(values = colores[2:1]) +
  theme(#axis.text.x = element_text(angle = 90), 
    legend.position = "bottom"
  )


ggsave(plot = sec_tipo_prom, 
       "./graficas_incidencia_la_revancha/especiales/secuestro/sec_tipo_prom.svg", width = 12, height = 6
)

tabla_secuestro <- list()

tabla_secuestro[[1]] <- data_bruto %>% 
  filter(cat_mp2=="Secuestro"
  ) %>% 
  mutate(tipo=case_when(
    grepl("EXPRE", modalidad_delito) ~ "Secuestro express", 
    T ~ "Secuestro"
  )) %>%
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date(fecha_inicio_global), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(.$tipo),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 year"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  mutate(nombre="Comparación entre secuestros")

tabla_secuestro[[1]] %>% 
  write.csv("./graficas_incidencia_la_revancha/especiales/secuestro/tablas_secuestro.csv", 
            row.names=F)
#absolutos
sec_tipo_abs <- data_bruto %>% 
  filter(cat_mp2=="Secuestro"
  ) %>% 
  mutate(tipo=case_when(
    grepl("EXPRE", modalidad_delito) ~ "Secuestro express", 
    T ~ "Secuestro"
  )) %>%
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date(fecha_inicio_global), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(.$tipo),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 year"), tipo) %>% 
  summarise(Total=sum(Total)) %>% 
  ggplot(aes(fecha_inicio, Total, color=tipo)) +
  geom_point(alpha=.7#, color=colores[1]
  ) +
  geom_label_repel(aes(label=Total), 
                   direction = "y"
                   ) +
  geom_smooth(se=F#, color=colores[7]
  ) + theme_light() + tema_fgj +
  # geom_smooth(se=F, color=colores[2], method = "lm") +
  scale_x_date(date_labels = "%b\n%y", date_breaks = "6 month") +
  labs(x="Fecha de inicio", y="Total de carpetas", 
       color="",
       title = paste0("Total de carpetas de secuestro por tipo"), 
       subtitle = paste0("Desde ",  format(as_date("2019-01-01"), "%B de %Y"), " a ",
                         format(as_date(fecha_lim), "%B de %Y"))) +
  scale_color_manual(values = colores[2:1]) +
  theme(#axis.text.x = element_text(angle = 90), 
    legend.position = "bottom"
  )


ggsave(plot = sec_tipo_abs, 
       "./graficas_incidencia_la_revancha/especiales/secuestro/sec_tipo_abs.svg", width = 12, height = 6
)
#### Eficiencia ministerial
#Secuestro
gr_efi_sec<- graf_efi_ministerial(datos = data_bruto %>% 
                                         filter(cat_mp2=="Secuestro",
                                                del_mp=="Secuestro"
                                         ), 
                                       fecha_inicio_global = fecha_inicio_global, 
                                       fecha_lim = fecha_lim, 
                                       delito_elegido ="Secuestro" 
                                       
)


ggsave(plot = gr_efi_sec, 
       "./graficas_incidencia_la_revancha/especiales/secuestro/gr_efi_sec.svg", width = 12, height = 6
)

#secuestro express
gr_efi_sec_exp<- graf_efi_ministerial(datos = data_bruto %>% 
                                    filter(cat_mp2=="Secuestro",
                                           del_mp=="Secuestro express"
                                    ), 
                                  fecha_inicio_global = fecha_inicio_global, 
                                  fecha_lim = fecha_lim, 
                                  delito_elegido ="Secuestro express"
                                  
)


ggsave(plot = gr_efi_sec_exp, 
       "./graficas_incidencia_la_revancha/especiales/secuestro/gr_efi_sec_exp.svg", width = 12, height = 6
)



#####
#Violencia sexual
#Línea 1: violación simple
# Línea 2: violación equiparada

violacion_tipo <- data_bruto %>% 
  filter(cat_mp2=="Violencia sexual", 
         delito=="VIOLACIÓN"
  ) %>% 
  mutate(tipo=case_when(
    grepl("EQUIP", modalidad_delito) ~ "Equiparada", 
    T ~ "Simple"
  )) %>%
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date(fecha_inicio_global), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(.$tipo),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 month"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  ggplot(aes(fecha_inicio, Total, color=tipo)) +
  geom_point(alpha=.7#, color=colores[1]
  ) +
  geom_smooth(se=F#, color=colores[7]
  ) + theme_light() + tema_fgj +
  # geom_smooth(se=F, color=colores[2], method = "lm") +
  scale_x_date(date_labels = "%b\n%y", date_breaks = "6 month") +
  labs(x="Fecha de inicio", y="Promedio diario", 
       color="",
       title = paste0("Promedio diario de carpetas de violación por tipo"), 
       subtitle = paste0("Desde ",  format(as_date("2019-01-01"), "%B de %Y"), " a ",
                         format(as_date(fecha_lim), "%B de %Y"))) +
  scale_color_manual(values = colores[2:1]) +
  theme(#axis.text.x = element_text(angle = 90), 
    legend.position = "bottom"
  )

ggsave(plot = violacion_tipo, 
       "./graficas_incidencia_la_revancha/especiales/violacion/violacion_tipo.svg", width = 12, height = 6
)


tablas_sexuales <- list()


tablas_sexuales[[1]] <-  data_bruto %>% 
  filter(cat_mp2=="Violencia sexual", 
         delito=="VIOLACIÓN"
  ) %>% 
  mutate(tipo=case_when(
    grepl("EQUIP", modalidad_delito) ~ "Equiparada", 
    T ~ "Simple"
  )) %>%
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date(fecha_inicio_global), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(.$tipo),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 year"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  mutate(nombre="violación simple vs equiparada")

#violación por delito
violacion_delito <- data_bruto %>% 
  filter(cat_mp2=="Violencia sexual", 
         grepl("Viola|Abus|Acos", del_mp)
  ) %>% 
  mutate(tipo=case_when(
    grepl("Viola", del_mp) ~ "Violación", 
    T ~ del_mp
  )) %>%
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date(fecha_inicio_global), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(.$tipo),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 month"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  ggplot(aes(fecha_inicio, Total, color=tipo)) +
  geom_point(alpha=.7#, color=colores[1]
  ) +
  geom_smooth(se=F#, color=colores[7]
  ) + theme_light() + tema_fgj +
  # geom_smooth(se=F, color=colores[2], method = "lm") +
  scale_x_date(date_labels = "%b\n%y", date_breaks = "6 month") +
  labs(x="Fecha de inicio", y="Promedio diario", 
       color="",
       title = paste0("Promedio diario de carpetas de violencia sexual por delito"), 
       subtitle = paste0("Desde ",  format(as_date("2019-01-01"), "%B de %Y"), " a ",
                         format(as_date(fecha_lim), "%B de %Y"))) +
  scale_color_manual(values = colores[4:1]) +
  theme(#axis.text.x = element_text(angle = 90), 
    legend.position = "bottom"
  )

ggsave(plot = violacion_delito, 
       "./graficas_incidencia_la_revancha/especiales/violacion/violacion_delito.svg", width = 12, height = 6
)

tablas_sexuales[[2]] <- data_bruto %>% 
  filter(cat_mp2=="Violencia sexual", 
         grepl("Viola|Abus|Acos", del_mp)
  ) %>% 
  mutate(tipo=case_when(
    grepl("Viola", del_mp) ~ "Violación", 
    T ~ del_mp
  )) %>%
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date(fecha_inicio_global), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(.$tipo),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 month"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  mutate(nombre="violación delito")
#violación conocido

violacion_conocido <- read_excel("violacion_conocido.xlsx") %>% 
  clean_names() %>% 
  mutate(relacion_victima_victimario=str_to_sentence(relacion_victima_victimario)) %>% 
  mutate(relacion_victima_victimario=case_when(
    relacion_victima_victimario %in% c("Familiar", "Conocido", 
                                       "Familiar y conocido") ~ "Conocido", 
    T ~ "Desconocido"
  ))


gr_violacion_relacion <- data_bruto %>% 
  filter(cat_mp2=="Violencia sexual", 
         delito=="VIOLACIÓN", 
         year(fecha_inicio)>=2022
  ) %>% left_join(violacion_conocido, by="id_ap") %>% 
  mutate(tipo=relacion_victima_victimario) %>% drop_na(tipo) %>% 
  # mutate(tipo=case_when(
  #   relacion_victima_victimario ~ "Desconocido", 
  #   T ~ "Simple"
  # )) %>%
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date("2022-01-01"), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(.$tipo),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 month"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  ggplot(aes(fecha_inicio, Total, color=tipo)) +
  geom_point(alpha=.7#, color=colores[1]
  ) +
  geom_smooth(se=F#, color=colores[7]
  ) + theme_light() + tema_fgj +
  # geom_smooth(se=F, color=colores[2], method = "lm") +
  scale_x_date(date_labels = "%b\n%y", date_breaks = "6 month") +
  labs(x="Fecha de inicio", y="Promedio diario", 
       color="",
       title = paste0("Promedio diario de carpetas de violación por tipo"), 
       subtitle = paste0("Desde ",  format(as_date("2022-01-01"), "%B de %Y"), " a ",
                         format(as_date(fecha_lim), "%B de %Y"))) +
  scale_color_manual(values = colores[2:1]) +
  theme(#axis.text.x = element_text(angle = 90), 
    legend.position = "bottom"
  )

ggsave(plot = gr_violacion_relacion, 
       "./graficas_incidencia_la_revancha/especiales/violacion/gr_violacion_relacion.svg", width = 12, height = 6
)



tablas_sexuales[[3]] <- data_bruto %>% 
  filter(cat_mp2=="Violencia sexual", 
         delito=="VIOLACIÓN", 
         year(fecha_inicio)>=2022
  ) %>% left_join(violacion_conocido, by="id_ap") %>% 
  mutate(tipo=relacion_victima_victimario) %>% drop_na(tipo) %>% 
  # mutate(tipo=case_when(
  #   relacion_victima_victimario ~ "Desconocido", 
  #   T ~ "Simple"
  # )) %>%
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date("2022-01-01"), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(.$tipo),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 year"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  mutate(nombre="violacion conocido")

#violación mayor y menor de edad
# línea 1: incidencia de víctimas violación mayores de edad
# línea 2: incidencia de víctimas violación menores de edad
id_menor <- incidencia$victimas %>% 
  filter(edad<18) %>% pull(id_ap)
id_mayor <- incidencia$victimas %>% 
  filter(edad>=18) %>% pull(id_ap)



gr_violacion_edades <- data_bruto %>% 
  filter(cat_mp2=="Violencia sexual", 
         delito=="VIOLACIÓN", 
         #year(fecha_inicio)>=2022
  ) %>% 
  mutate(tipo=case_when(
    id_ap %in% id_menor ~ "Menores de edad", 
    id_ap %in% id_mayor ~ "Mayores de edad"
  )) %>%
  
  drop_na(tipo) %>% 
  # mutate(tipo=case_when(
  #   relacion_victima_victimario ~ "Desconocido", 
  #   T ~ "Simple"
  # )) %>%
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date("2019-01-01"), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(.$tipo),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 month"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  ggplot(aes(fecha_inicio, Total, color=tipo)) +
  geom_point(alpha=.7#, color=colores[1]
  ) +
  geom_smooth(se=F#, color=colores[7]
  ) + theme_light() + tema_fgj +
  # geom_smooth(se=F, color=colores[2], method = "lm") +
  scale_x_date(date_labels = "%b\n%y", date_breaks = "6 month") +
  labs(x="Fecha de inicio", y="Promedio diario", 
       color="",
       title = paste0("Promedio diario de carpetas de violación por tipo"), 
       subtitle = paste0("Desde ",  format(as_date("2019-01-01"), "%B de %Y"), " a ",
                         format(as_date(fecha_lim), "%B de %Y"))) +
  scale_color_manual(values = colores[2:1]) +
  theme(#axis.text.x = element_text(angle = 90), 
    legend.position = "bottom"
  )

ggsave(plot = gr_violacion_edades, 
       "./graficas_incidencia_la_revancha/especiales/violacion/gr_violacion_edades.svg", width = 12, height = 6
)


tablas_sexuales[[4]] <- data_bruto %>% 
  filter(cat_mp2=="Violencia sexual", 
         delito=="VIOLACIÓN", 
         #year(fecha_inicio)>=2022
  ) %>% 
  mutate(tipo=case_when(
    id_ap %in% id_menor ~ "Menores de edad", 
    id_ap %in% id_mayor ~ "Mayores de edad"
  )) %>%
  
  drop_na(tipo) %>% 
  # mutate(tipo=case_when(
  #   relacion_victima_victimario ~ "Desconocido", 
  #   T ~ "Simple"
  # )) %>%
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date("2019-01-01"), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(.$tipo),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 year"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  mutate(nombre="violacion edades")

# #violacion relacion y grupo de edad
# Facet 1: Barras de incidencia 2024 imputado conocido con edades 0-7, 8-12, 13-18, 19-30, 31-40,40+
# Facet 2: Barras de incidencia 2024 imputado desconocido con edades 0-7, 8-12, 13-18, 19-30, 31-40,40+
gr_violacion_relacion_edad <- incidencia$victimas %>% 
  filter(#cat_mp2=="Violencia sexual", 
         delito=="VIOLACIÓN", 
         year(fecha_inicio)>=2024
  ) %>% left_join(violacion_conocido, by="id_ap") %>% 
  mutate(tipo=relacion_victima_victimario) %>% drop_na(tipo) %>% 
  mutate(gr_edad=case_when(
    edad<=7 ~ "0-7", 
    edad>=8 & edad<=12 ~ "8-12",
    edad>=13 & edad<=18 ~ "13-18",
    edad>=19 & edad<=30 ~ "19-30",
    edad>=31 & edad<=40 ~ "31-40",
    T ~ "+40"
  )) %>%
  mutate(gr_edad=factor(gr_edad, 
                        levels=c("0-7", "8-12", "13-18", "19-30", 
                                 "31-40","+40"
                                 )
                        )) %>% 
  group_by(tipo, gr_edad) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(           tipo=unique(.$tipo),
                      gr_edad=unique(.$gr_edad),
           fill=list(Total=0)
  ) %>% #group_by(fecha_inicio=floor_date(fecha_inicio, "1 month"), tipo) %>% 
  # summarise(Total=mean(Total)) %>% 
  ungroup() %>% group_by(tipo) %>% 
  mutate(porcentaje=percent(Total/sum(Total), 1)) %>% 
  ggplot(aes(gr_edad, Total, fill=tipo)) +
  geom_col(alpha=.7#, color=colores[1]
  ) +
  facet_wrap(.~tipo, scales = "free") +
  geom_label(aes(label=paste0(Total, "\n(", porcentaje, ")")), show.legend = F, 
             fill="ghostwhite"
  ) + theme_light() + tema_fgj +
  # geom_smooth(se=F, color=colores[2], method = "lm") +
  # scale_x_date(date_labels = "%b\n%y", date_breaks = "6 month") +
  labs(x="Grupo de edad", y="víctimas", 
       fill="",
       title = paste0("Total de víctimas de violación por tipo de relación y grupo de edad"), 
       subtitle = paste0("Desde ",  format(as_date("2024-01-01"), "%B de %Y"), " a ",
                         format(as_date(fecha_lim), "%B de %Y"))) +
  scale_fill_manual(values = colores[7:8]) +
  theme(#axis.text.x = element_text(angle = 90), 
    legend.position = "bottom"
  )

ggsave(plot = gr_violacion_relacion_edad, 
       "./graficas_incidencia_la_revancha/especiales/violacion/gr_violacion_relacion_edad.svg", width = 12, height = 6
)


###eficiencia ministerial
#imputados conocido

id_conocido <- violacion_conocido %>% 
  filter(relacion_victima_victimario=="Conocido") %>% pull(id_ap)

gr_efi_viol_conocido <- graf_efi_ministerial_traza(datos = data_bruto %>% 
                                        filter(cat_mp2=="Violencia sexual",
                                               delito=="VIOLACIÓN", 
                                               id_ap %in% id_conocido
                                        ), 
                                      fecha_inicio_global = "2022-01-01", 
                                      fecha_lim = fecha_lim, 
                                      delito_elegido ="Violación imputado conocido"
                                      
)


ggsave(plot = gr_efi_viol_conocido, 
       "./graficas_incidencia_la_revancha/especiales/violacion/gr_efi_viol_conocido.svg", width = 12, height = 6
)

#id_conocido <- violacion_conocido %>% 
id_desconocido <- violacion_conocido %>% 
  filter(relacion_victima_victimario=="Desconocido") %>% pull(id_ap)

gr_efi_viol_desconocido <- graf_efi_ministerial_traza(datos = data_bruto %>% 
                                               filter(cat_mp2=="Violencia sexual",
                                                      delito=="VIOLACIÓN", 
                                                      id_ap %in% id_desconocido
                                               ), 
                                             fecha_inicio_global = "2022-01-01", 
                                             fecha_lim = fecha_lim, 
                                             delito_elegido ="Violación imputado desconocido"
                                             
)


ggsave(plot = gr_efi_viol_desconocido, 
       "./graficas_incidencia_la_revancha/especiales/violacion/gr_efi_viol_desconocido.svg", width = 12, height = 6
)

#menores de edad


gr_efi_viol_menor_edad <- graf_efi_ministerial_traza(datos = data_bruto %>% 
                                                  filter(cat_mp2=="Violencia sexual",
                                                         delito=="VIOLACIÓN", 
                                                         id_ap %in% id_menor
                                                  ), 
                                                fecha_inicio_global = "2022-01-01", 
                                                fecha_lim = fecha_lim, 
                                                delito_elegido ="Violación víctima menor de edad"
                                                
)


ggsave(plot = gr_efi_viol_menor_edad, 
       "./graficas_incidencia_la_revancha/especiales/violacion/gr_efi_viol_menor_edad.svg", width = 12, height = 6
)

total_tablas_violacion <- bind_rows(tabla_sexuales)
##abuso sexual
gr_abuso_edades <- data_bruto %>% 
  filter(cat_mp2=="Violencia sexual", 
         del_mp=="Abuso sexual", 
         #year(fecha_inicio)>=2022
  ) %>% 
  mutate(tipo=case_when(
    id_ap %in% id_menor ~ "Menores de edad", 
    id_ap %in% id_mayor ~ "Mayores de edad"
  )) %>%
  
  drop_na(tipo) %>% 
  # mutate(tipo=case_when(
  #   relacion_victima_victimario ~ "Desconocido", 
  #   T ~ "Simple"
  # )) %>%
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date("2019-01-01"), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(.$tipo),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 month"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  ggplot(aes(fecha_inicio, Total, color=tipo)) +
  geom_point(alpha=.7#, color=colores[1]
  ) +
  geom_smooth(se=F#, color=colores[7]
  ) + theme_light() + tema_fgj +
  # geom_smooth(se=F, color=colores[2], method = "lm") +
  scale_x_date(date_labels = "%b\n%y", date_breaks = "6 month") +
  labs(x="Fecha de inicio", y="Promedio diario", 
       color="",
       title = paste0("Promedio diario de carpetas de abuso sexual por tipo"), 
       subtitle = paste0("Desde ",  format(as_date("2019-01-01"), "%B de %Y"), " a ",
                         format(as_date(fecha_lim), "%B de %Y"))) +
  scale_color_manual(values = colores[2:1]) +
  theme(#axis.text.x = element_text(angle = 90), 
    legend.position = "bottom"
  )

ggsave(plot = gr_abuso_edades, 
       "./graficas_incidencia_la_revancha/especiales/violacion/gr_abuso_edades.svg", width = 12, height = 6
)

tablas_sexuales[[5]] <- data_bruto %>% 
  filter(cat_mp2=="Violencia sexual", 
         del_mp=="Abuso sexual", 
         #year(fecha_inicio)>=2022
  ) %>% 
  mutate(tipo=case_when(
    id_ap %in% id_menor ~ "Menores de edad", 
    id_ap %in% id_mayor ~ "Mayores de edad"
  )) %>%
  
  drop_na(tipo) %>% 
  # mutate(tipo=case_when(
  #   relacion_victima_victimario ~ "Desconocido", 
  #   T ~ "Simple"
  # )) %>%
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date("2019-01-01"), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(.$tipo),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 year"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  mutate(nombre="abuso edades")

###eficiencia ministerial
#menores de edad


gr_efi_abuso_menor_edad <- graf_efi_ministerial_traza(datos = data_bruto %>% 
                                                 filter(cat_mp2=="Violencia sexual",
                                                        del_mp=="Abuso sexual", 
                                                        id_ap %in% id_menor
                                                 ), 
                                               fecha_inicio_global = "2019-01-01", 
                                               fecha_lim = fecha_lim, 
                                               delito_elegido ="Abuso sexual víctima menor de edad"
                                               
)


ggsave(plot = gr_efi_abuso_menor_edad, 
       "./graficas_incidencia_la_revancha/especiales/violacion/gr_efi_abuso_menor_edad.svg", width = 12, height = 6
)

#mayores de edad


gr_efi_abuso_mayor_edad <- graf_efi_ministerial_traza(datos = data_bruto %>% 
                                                        filter(cat_mp2=="Violencia sexual",
                                                               del_mp=="Abuso sexual", 
                                                               id_ap %in% id_mayor
                                                        ), 
                                                      fecha_inicio_global = "2019-01-01", 
                                                      fecha_lim = fecha_lim, 
                                                      delito_elegido ="Abuso sexual víctima mayor de edad"
                                                      
)


ggsave(plot = gr_efi_abuso_mayor_edad, 
       "./graficas_incidencia_la_revancha/especiales/violacion/gr_efi_abuso_mayor_edad.svg", width = 12, height = 6
)


###
#Acoso sexual
gr_acoso_edades <- data_bruto %>% 
  filter(cat_mp2=="Violencia sexual", 
         del_mp=="Acoso sexual", 
         #year(fecha_inicio)>=2022
  ) %>% 
  mutate(tipo=case_when(
    id_ap %in% id_menor ~ "Menores de edad", 
    id_ap %in% id_mayor ~ "Mayores de edad"
  )) %>%
  
  drop_na(tipo) %>% 
  # mutate(tipo=case_when(
  #   relacion_victima_victimario ~ "Desconocido", 
  #   T ~ "Simple"
  # )) %>%
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date("2019-01-01"), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(.$tipo),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 month"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  ggplot(aes(fecha_inicio, Total, color=tipo)) +
  geom_point(alpha=.7#, color=colores[1]
  ) +
  geom_smooth(se=F#, color=colores[7]
  ) + theme_light() + tema_fgj +
  # geom_smooth(se=F, color=colores[2], method = "lm") +
  scale_x_date(date_labels = "%b\n%y", date_breaks = "6 month") +
  labs(x="Fecha de inicio", y="Promedio diario", 
       color="",
       title = paste0("Promedio diario de carpetas de acoso sexual por tipo"), 
       subtitle = paste0("Desde ",  format(as_date("2019-01-01"), "%B de %Y"), " a ",
                         format(as_date(fecha_lim), "%B de %Y"))) +
  scale_color_manual(values = colores[2:1]) +
  theme(#axis.text.x = element_text(angle = 90), 
    legend.position = "bottom"
  )

ggsave(plot = gr_acoso_edades, 
       "./graficas_incidencia_la_revancha/especiales/violacion/gr_acoso_edades.svg", width = 12, height = 6
)


tablas_sexuales[[6]] <- data_bruto %>% 
  filter(cat_mp2=="Violencia sexual", 
         del_mp=="Acoso sexual", 
         #year(fecha_inicio)>=2022
  ) %>% 
  mutate(tipo=case_when(
    id_ap %in% id_menor ~ "Menores de edad", 
    id_ap %in% id_mayor ~ "Mayores de edad"
  )) %>%
  
  drop_na(tipo) %>% 
  # mutate(tipo=case_when(
  #   relacion_victima_victimario ~ "Desconocido", 
  #   T ~ "Simple"
  # )) %>%
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date("2019-01-01"), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(.$tipo),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 year"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  mutate(nombre="acoso edades")

total_tablas_sexuales <- bind_rows(tablas_sexuales)

total_tablas_sexuales %>% 
  write.csv("./graficas_incidencia_la_revancha/especiales/violacion/tablas_sexuales.csv", 
            row.names = F
            )

###eficiencia ministerial
#menores de edad


gr_efi_acoso_menor_edad <- graf_efi_ministerial_traza(datos = data_bruto %>% 
                                                        filter(cat_mp2=="Violencia sexual",
                                                               del_mp=="Acoso sexual", 
                                                               id_ap %in% id_menor
                                                        ), 
                                                      fecha_inicio_global = "2019-01-01", 
                                                      fecha_lim = fecha_lim, 
                                                      delito_elegido ="Acoso sexual víctima menor de edad"
                                                      
)


ggsave(plot = gr_efi_acoso_menor_edad, 
       "./graficas_incidencia_la_revancha/especiales/violacion/gr_efi_acoso_menor_edad.svg", width = 12, height = 6
)

#mayores de edad


gr_efi_acoso_mayor_edad <- graf_efi_ministerial_traza(datos = data_bruto %>% 
                                                        filter(cat_mp2=="Violencia sexual",
                                                               del_mp=="Acoso sexual", 
                                                               id_ap %in% id_mayor
                                                        ), 
                                                      fecha_inicio_global = "2019-01-01", 
                                                      fecha_lim = fecha_lim, 
                                                      delito_elegido ="Acoso sexual víctima mayor de edad"
                                                      
)


ggsave(plot = gr_efi_acoso_mayor_edad, 
       "./graficas_incidencia_la_revancha/especiales/violacion/gr_efi_acoso_mayor_edad.svg", width = 12, height = 6
)



#####
#violencia fisicia intencional
# línea 1: incidencia lesiones dolosas
# línea 2: incidencia de lesiones por arma de fuego
gr_lesiones_tipo <- data_bruto %>% 
  filter(cat_mp2=="Violencia física intencional", 
         #del_mp=="Acoso sexual", 
         #year(fecha_inicio)>=2022
  ) %>% 
  mutate(tipo=del_mp) %>%
  
  # mutate(tipo=case_when(
  #   relacion_victima_victimario ~ "Desconocido", 
  #   T ~ "Simple"
  # )) %>%
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date("2019-01-01"), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(.$tipo),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 month"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  ggplot(aes(fecha_inicio, Total, color=tipo)) +
  geom_point(alpha=.7#, color=colores[1]
  ) +
  geom_smooth(se=F#, color=colores[7]
  ) + theme_light() + tema_fgj +
  # geom_smooth(se=F, color=colores[2], method = "lm") +
  scale_x_date(date_labels = "%b\n%y", date_breaks = "6 month") +
  labs(x="Fecha de inicio", y="Promedio diario", 
       color="",
       title = paste0("Promedio diario de carpetas lesiones por tipo"), 
       subtitle = paste0("Desde ",  format(as_date("2019-01-01"), "%B de %Y"), " a ",
                         format(as_date(fecha_lim), "%B de %Y"))) +
  scale_color_manual(values = colores[2:1]) +
  theme(#axis.text.x = element_text(angle = 90), 
    legend.position = "bottom"
  )

ggsave(plot = gr_lesiones_tipo, 
       "./graficas_incidencia_la_revancha/especiales/violencia_intencional/gr_lesiones_tipo.svg", width = 12, height = 6
)

tabla_violencia_fisica <- list()

tabla_violencia_fisica[[1]] <-  data_bruto %>% 
  filter(cat_mp2=="Violencia física intencional", 
         #del_mp=="Acoso sexual", 
         #year(fecha_inicio)>=2022
  ) %>% 
  mutate(tipo=del_mp) %>%
  
  # mutate(tipo=case_when(
  #   relacion_victima_victimario ~ "Desconocido", 
  #   T ~ "Simple"
  # )) %>%
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date("2019-01-01"), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(.$tipo),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 year"), tipo) %>% 
  summarise(Total=mean(Total))

tabla_violencia_fisica[[1]] %>% 
  write.csv("./graficas_incidencia_la_revancha/especiales/violencia_intencional/violencia_intencional.csv", 
            row.names=F
            )

#lesiones dolosas


gr_efi_lesiones <- graf_efi_ministerial(datos = data_bruto %>% 
                                                        filter(cat_mp2=="Violencia física intencional",
                                                               del_mp=="Lesiones dolosas"
                                                        ), 
                                                      fecha_inicio_global = "2019-01-01", 
                                                      fecha_lim = fecha_lim, 
                                                      delito_elegido ="Lesiones dolosas"
                                                      
)


ggsave(plot = gr_efi_lesiones, 
       "./graficas_incidencia_la_revancha/especiales/violencia_intencional/gr_efi_lesiones.svg", width = 12, height = 6
)

#lesiones dolosas por arma de guefo


gr_efi_lesiones_fuego <- graf_efi_ministerial_traza(datos = data_bruto %>% 
                                          filter(cat_mp2=="Violencia física intencional",
                                                 del_mp=="Lesiones por arma de fuego"
                                          ), 
                                        fecha_inicio_global = "2019-01-01", 
                                        fecha_lim = fecha_lim, 
                                        delito_elegido ="Lesiones por arma de fuego"
                                        
)


ggsave(plot = gr_efi_lesiones_fuego, 
       "./graficas_incidencia_la_revancha/especiales/violencia_intencional/gr_efi_lesiones_fuego.svg", width = 12, height = 6
)

#####
#Extorsión
data_extorsion <- readRDS("C:/Users/mauri/Downloads/entrevistas_extorsion.rds")

llamada <- paste(sep="|", "LLAMADA", "MENSAJE", "CELULAR", "TELEFONO", "T?LEFONO", "VOZ", 
                 "WHATSAPP", "WHATS", "REDES S", "MENSAJES", "CORREO", "MAIL", "EMAIL", 
                 "MESAJE", "LAMADAS", "TELÉFONO", "FACEBO", "FEISBU", "INSTAGR", "TINDER",
                 "GRINDER")

presencial <- paste(sep="|", "ACUDIERON", "INGRESARON A", "MI DOMICILIO", 
                    "LUGAR DE TRABAJO", "AMEDRENTA", "MI NEGOCIO", "SOBRE AMARRILLO", 
                    "CORTINA", "PORTABAN", "SE PRESENTARON", "UNA NOTA", "JEFE", 
                    "COMERCIANTES", "LLEGAN", "LOCAL", "DUEÑO"
                    
)

data_extorsion <- data_extorsion %>% 
  mutate(tipo=case_when(
    grepl(llamada, resumen_ap) ~ "Virtual", 
    grepl(presencial, resumen_ap) ~ "Presencial", T ~ "Otros"
  ))


entrevistas2 <- entrevistas2 %>% 
  mutate(tipo=case_when(
    grepl(llamada, declaracion) ~ "Virtual", 
    grepl(presencial, declaracion) ~ "Presencial", T ~ "Otros"
  ))

id_Presencial <- entrevistas2 %>% filter(tipo=="Presencial") %>% 
  pull(idAveriguacionPrevia)
id_virtual <- entrevistas2 %>% filter(tipo=="Virtual") %>% 
  pull(idAveriguacionPrevia)

data_extorsion <- data_extorsion %>% 
  mutate(tipo_extorsion=case_when(
    id_ap %in% id_Presencial ~ "Presencial", 
    tipo=="Presencial" ~ "Presencial",
    id_ap %in% id_virtual ~ "Virtual", 
    tipo=="Virtual" ~ "Virtual", 
    T ~ "Otros"
    
  ))

data_extorsion <- data_extorsion %>% 
  filter(fecha_inicio>=fecha_inicio_global)


gr_trend_ext <- data_extorsion %>% 
  # filter(delito=="ROBO DE VEHÍCULO CON Y SIN VIOLENCIA", 
  #        #grepl("CON VIOLE|C/V", modalidad_delito),
  #        year(fecha_inicio) %in% c(2020:2025)) %>% 
  filter(fecha_inicio<=fecha_lim) %>% 
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% drop_na(tipo) %>% 
  complete(fecha_inicio=seq.Date(as_date(fecha_inicio_global), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(.$tipo_extorsion),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 month"), tipo) %>% 
  summarise(Total=mean(Total)) %>% drop_na(tipo) %>% 
  ggplot(aes(fecha_inicio, Total, color=tipo)) +
  geom_point(alpha=.7#, color=colores[1]
  ) +
  geom_smooth(se=F#, color=colores[7]
  ) + theme_light() + tema_fgj +
  # geom_smooth(se=F, color=colores[2], method = "lm") +
  scale_x_date(date_labels = "%b-%y", date_breaks = "3 month") +
  labs(x="Fecha de inicio", y="Promedio diario", color="",
       title = paste0("Promedio diario de carpetas por extorsión según tipo"), 
       subtitle = paste0("Desde ",  format(as_date(fecha_inicio_global), "%B de %Y"), " a ",
                         format(as_date(fecha_lim), "%B de %Y"))) +
  scale_color_manual(values = colores[3:1]) +
  theme(axis.text.x = element_text(angle = 90), 
        legend.position = "bottom"
  )


ggsave(plot = gr_trend_ext, 
       "./graficas_incidencia_la_revancha/especiales/extorsion/gr_trend_ext.svg", width = 12, height = 6
)

tablas_extorsion <- list()


tablas_extorsion[[1]] <- data_extorsion %>% 
  # filter(delito=="ROBO DE VEHÍCULO CON Y SIN VIOLENCIA", 
  #        #grepl("CON VIOLE|C/V", modalidad_delito),
  #        year(fecha_inicio) %in% c(2020:2025)) %>% 
  filter(fecha_inicio<=fecha_lim) %>% 
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% drop_na(tipo) %>% 
  complete(fecha_inicio=seq.Date(as_date(fecha_inicio_global), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(.$tipo_extorsion),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 year"), tipo) %>% 
  summarise(Total=mean(Total)) %>% drop_na(tipo)

tablas_extorsion[[1]]  %>% 
  write.csv("./graficas_incidencia_la_revancha/especiales/extorsion/tablas_extorsion.csv", 
            row.names=F
            )


#####
#Robo a casa habitación
gr_casa_tipo <- data_bruto %>% 
  filter(cat_mp2=="Robo a casa habitación", 
         #del_mp=="Acoso sexual", 
         #year(fecha_inicio)>=2022
  ) %>% 
  mutate(tipo=case_when(
    modalidad_delito=="ROBO A CASA HABITACION CON VIOLENCIA" ~ "Con violencia", 
    T ~ "Sin violencia"
  )) %>%
  
  # mutate(tipo=case_when(
  #   relacion_victima_victimario ~ "Desconocido", 
  #   T ~ "Simple"
  # )) %>%
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date("2019-01-01"), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(.$tipo),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 month"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  ggplot(aes(fecha_inicio, Total, color=tipo)) +
  geom_point(alpha=.7#, color=colores[1]
  ) +
  geom_smooth(se=F#, color=colores[7]
  ) + theme_light() + tema_fgj +
  # geom_smooth(se=F, color=colores[2], method = "lm") +
  scale_x_date(date_labels = "%b\n%y", date_breaks = "6 month") +
  labs(x="Fecha de inicio", y="Promedio diario", 
       color="",
       title = paste0("Promedio diario de carpetas lesiones por tipo"), 
       subtitle = paste0("Desde ",  format(as_date("2019-01-01"), "%B de %Y"), " a ",
                         format(as_date(fecha_lim), "%B de %Y"))) +
  scale_color_manual(values = colores[2:1]) +
  theme(#axis.text.x = element_text(angle = 90), 
    legend.position = "bottom"
  )

ggsave(plot = gr_casa_tipo, 
       "./graficas_incidencia_la_revancha/especiales/robo_casa/gr_casa_tipo.svg", width = 12, height = 6
)


data_bruto %>% 
  filter(cat_mp2=="Robo a casa habitación", 
         #del_mp=="Acoso sexual", 
         #year(fecha_inicio)>=2022
  ) %>% 
  mutate(tipo=case_when(
    modalidad_delito=="ROBO A CASA HABITACION CON VIOLENCIA" ~ "Con violencia", 
    T ~ "Sin violencia"
  )) %>%
  
  # mutate(tipo=case_when(
  #   relacion_victima_victimario ~ "Desconocido", 
  #   T ~ "Simple"
  # )) %>%
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date("2019-01-01"), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(.$tipo),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 year"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  write.csv("./graficas_incidencia_la_revancha/especiales/robo_casa/tablas_robo_casa.csv", 
            row.names = F
            )
###eficiencia ministerial
#robo con violencia
gr_casa_con <- graf_efi_ministerial(datos = data_bruto %>% 
                                                      filter(cat_mp2=="Robo a casa habitación",
                                                             modalidad_delito=="ROBO A CASA HABITACION CON VIOLENCIA"
                                                      ), 
                                                    fecha_inicio_global = "2019-01-01", 
                                                    fecha_lim = fecha_lim, 
                                                    delito_elegido ="Robo a casa habitación con violencia"
                                                    
)


ggsave(plot = gr_casa_con, 
       "./graficas_incidencia_la_revancha/especiales/robo_casa/gr_casa_con.svg", width = 12, height = 6
)

#robo sin violencia



gr_casa_sin <- graf_efi_ministerial(datos = data_bruto %>% 
                                      filter(cat_mp2=="Robo a casa habitación",
                                             modalidad_delito=="ROBO A CASA HABITACION SIN VIOLENCIA"
                                      ), 
                                    fecha_inicio_global = "2019-01-01", 
                                    fecha_lim = fecha_lim, 
                                    delito_elegido ="Robo a casa habitación sin violencia"
                                    
)


ggsave(plot = gr_casa_sin, 
       "./graficas_incidencia_la_revancha/especiales/robo_casa/gr_casa_sin.svg", width = 12, height = 6
)


#####
#Robos con violencia
gr_robo_violencia_tipo <- data_bruto %>% 
  filter(cat_mp2=="Robos con violencia", 
         del_mp %in% c("Robo a transeunte c/v", 
                       "Robo a pasajero c/v", 
                       "Robo a transportista c/v"
                       ) 
         #year(fecha_inicio)>=2022
  ) %>% 
  mutate(tipo=case_when(
    del_mp=="Robo a transportista c/v" ~ "Robo a chofer c/V", 
    T ~ del_mp
  )) %>%
  
  # mutate(tipo=case_when(
  #   relacion_victima_victimario ~ "Desconocido", 
  #   T ~ "Simple"
  # )) %>%
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date("2019-01-01"), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(.$tipo),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 month"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  ggplot(aes(fecha_inicio, Total, color=tipo)) +
  geom_point(alpha=.7#, color=colores[1]
  ) +
  geom_smooth(se=F,#, color=colores[7]
              #span=.08
  ) + theme_light() + tema_fgj +
  # geom_smooth(se=F, color=colores[2], method = "lm") +
  scale_x_date(date_labels = "%b\n%y", date_breaks = "6 month") +
  labs(x="Fecha de inicio", y="Promedio diario", 
       color="",
       title = paste0("Promedio diario de carpetas robos con violencia por delito"), 
       subtitle = paste0("Desde ",  format(as_date("2019-01-01"), "%B de %Y"), " a ",
                         format(as_date(fecha_lim), "%B de %Y"))) +
  scale_color_manual(values = colores[3:1]) +
  theme(#axis.text.x = element_text(angle = 90), 
    legend.position = "bottom"
  )

ggsave(plot = gr_robo_violencia_tipo, 
       "./graficas_incidencia_la_revancha/especiales/robos_con_violencia/gr_robo_violencia_tipo.svg", width = 12, height = 6
)

data_bruto %>% 
  filter(cat_mp2=="Robos con violencia", 
         del_mp %in% c("Robo a transeunte c/v", 
                       "Robo a pasajero c/v", 
                       "Robo a transportista c/v"
         ) 
         #year(fecha_inicio)>=2022
  ) %>% 
  mutate(tipo=case_when(
    del_mp=="Robo a transportista c/v" ~ "Robo a chofer c/V", 
    T ~ del_mp
  )) %>%
  
  # mutate(tipo=case_when(
  #   relacion_victima_victimario ~ "Desconocido", 
  #   T ~ "Simple"
  # )) %>%
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date("2019-01-01"), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(.$tipo),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 year"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  write.csv("./graficas_incidencia_la_revancha/especiales/robos_con_violencia/tabla_robo_con_violencia.csv", 
            row.names = F
            )

###eficiencia ministerial
#robo con violencia
gr_efi_robo_con_violencia <- graf_efi_ministerial(datos = data_bruto %>% 
                                      filter(cat_mp2=="Robos con violencia",
                                             del_mp %in% c("Robo a transeunte c/v", 
                                                           "Robo a pasajero c/v", 
                                                           "Robo a transportista c/v"
                                             ) 
                                      ), 
                                    fecha_inicio_global = "2019-01-01", 
                                    fecha_lim = fecha_lim, 
                                    delito_elegido =c("Robo a transeúnte con violencia", 
                                                      "Robo a pasajero a bordo de transporte público con violencia")
                                    
)


ggsave(plot = gr_efi_robo_con_violencia, 
       "./graficas_incidencia_la_revancha/especiales/robos_con_violencia/gr_efi_robo_con_violencia.svg", width = 12, height = 6
)

#####
#robo sin violencia
#puestas y vinculados por robo sin violenciadata
id_robo_sin_viol <- data_bruto %>% 
  filter(cat_mp2=="Robos sin violencia") %>% pull(id_ap)
  
  gr_detenidos_robo_sin_viol <- flagrancias  %>% 
    filter(id_ap %in% id_robo_sin_viol) %>% 
    group_by(año=year(fecha_inicio)) %>% 
    summarise(Detenidos=n(),
              Vinculados=sum(vinculacion_a_proceso_por_persona, na.rm = T)
              ) %>% 
    gather(tipo, Total, Detenidos:Vinculados) %>% 
    ggplot(aes(factor(año), Total, fill=tipo, group=tipo))+
    geom_col(position = "dodge") +
    geom_label(aes(label=comma(Total), 
                   y=Total*.8), alpha=.75,
               position = position_dodge(1), show.legend = F
               ) + theme_light() +
    tema_fgj + scale_y_continuous(labels = comma) +
    labs(x="Fecha de inicio", y="Total", 
         fill="",
         title = paste0("Total de detenidos y vinculados por robos sin violencia por año"), 
         subtitle = paste0("Desde ",  format(as_date("2019-01-01"), "%B de %Y"), " a ",
                           format(as_date(fecha_lim), "%B de %Y"))) +
    scale_fill_manual(values = colores[7:8]) +
    theme(#axis.text.x = element_text(angle = 90), 
      legend.position = "bottom"
    )
    
  ggsave(plot = gr_detenidos_robo_sin_viol, 
         "./graficas_incidencia_la_revancha/especiales/robos_sin_violencia/gr_detenidos_robo_sin_viol.png", width = 12, height = 6
  )
#####
#Posesión y venta de drogas
# línea 1: incidencia de narco fines de venta
# línea 2: incidencia de narco posesión simple
gr_narco_tipo <- data_bruto %>% 
  filter(cat_mp2=="Posesión y venta de drogas" 
         #year(fecha_inicio)>=2022
  ) %>% 
  mutate(tipo=del_mp) %>%
  
  # mutate(tipo=case_when(
  #   relacion_victima_victimario ~ "Desconocido", 
  #   T ~ "Simple"
  # )) %>%
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date("2019-01-01"), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(.$tipo),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 month"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  ggplot(aes(fecha_inicio, Total, color=tipo)) +
  geom_point(alpha=.7#, color=colores[1]
  ) +
  geom_smooth(se=F#, color=colores[7]
  ) + theme_light() + tema_fgj +
  # geom_smooth(se=F, color=colores[2], method = "lm") +
  scale_x_date(date_labels = "%b\n%y", date_breaks = "6 month") +
  labs(x="Fecha de inicio", y="Promedio diario", 
       color="",
       title = paste0("Promedio diario de carpetas por narcomenudeo por tipo"), 
       subtitle = paste0("Desde ",  format(as_date("2019-01-01"), "%B de %Y"), " a ",
                         format(as_date(fecha_lim), "%B de %Y"))) +
  scale_color_manual(values = colores[2:1]) +
  theme(#axis.text.x = element_text(angle = 90), 
    legend.position = "bottom"
  )

ggsave(plot = gr_narco_tipo, 
       "./graficas_incidencia_la_revancha/especiales/narco/gr_narco_tipo.svg", width = 12, height = 6
)


data_bruto %>% 
  filter(cat_mp2=="Posesión y venta de drogas" 
         #year(fecha_inicio)>=2022
  ) %>% 
  mutate(tipo=del_mp) %>%
  
  # mutate(tipo=case_when(
  #   relacion_victima_victimario ~ "Desconocido", 
  #   T ~ "Simple"
  # )) %>%
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date("2019-01-01"), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(.$tipo),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 month"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  write.csv("./graficas_incidencia_la_revancha/especiales/narco/tablas_narco.csv", 
            row.names = F
            )
#nrco simple
gr_efi_narco_simple <- graf_efi_ministerial(datos = data_bruto %>% 
                                                    filter(cat_mp2=="Posesión y venta de drogas" ,
                                                           del_mp %in% c("Narcomenudeo posesión"
                                                           ) 
                                                    ), 
                                                  fecha_inicio_global = "2019-01-01", 
                                                  fecha_lim = fecha_lim, 
                                                  delito_elegido =c("Narcomenudeo simple")
                                                  
)


ggsave(plot = gr_efi_narco_simple, 
       "./graficas_incidencia_la_revancha/especiales/narco/gr_efi_narco_simple.svg", width = 12, height = 6
)
#nrco venta
gr_efi_narco_venta <- graf_efi_ministerial(datos = data_bruto %>% 
                                              filter(cat_mp2=="Posesión y venta de drogas" ,
                                                     del_mp %in% c("Narcomenudeo venta"
                                                     ) 
                                              ), 
                                            fecha_inicio_global = "2019-01-01", 
                                            fecha_lim = fecha_lim, 
                                            delito_elegido =c("Narcomenudeo venta")
                                            
)


ggsave(plot = gr_efi_narco_venta, 
       "./graficas_incidencia_la_revancha/especiales/narco/gr_efi_narco_venta.svg", width = 12, height = 6
)


#####
# Defraudación
gr_defraudacion_tipo <- data_bruto %>% 
  filter(cat_mp2=="Defraudación"
         #year(fecha_inicio)>=2022
  ) %>% 
  mutate(tipo=del_mp) %>%
  
  # mutate(tipo=case_when(
  #   relacion_victima_victimario ~ "Desconocido", 
  #   T ~ "Simple"
  # )) %>%
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date("2019-01-01"), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(.$tipo),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 month"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  ggplot(aes(fecha_inicio, Total, color=tipo)) +
  geom_point(alpha=.7#, color=colores[1]
  ) +
  geom_smooth(se=F#, color=colores[7]
  ) + theme_light() + tema_fgj +
  # geom_smooth(se=F, color=colores[2], method = "lm") +
  scale_x_date(date_labels = "%b\n%y", date_breaks = "6 month") +
  labs(x="Fecha de inicio", y="Promedio diario", 
       color="",
       title = paste0("Promedio diario de carpetas por defraudación por tipo"), 
       subtitle = paste0("Desde ",  format(as_date("2019-01-01"), "%B de %Y"), " a ",
                         format(as_date(fecha_lim), "%B de %Y"))) +
  scale_color_manual(values = colores[5:1]) +
  theme(#axis.text.x = element_text(angle = 90), 
    legend.position = "bottom"
  )

ggsave(plot = gr_defraudacion_tipo, 
       "./graficas_incidencia_la_revancha/especiales/defraudacion/gr_defraudacion_tipo.svg", width = 12, height = 6
)

tablas_defraudacion <- list()

tablas_defraudacion[[1]] <-  data_bruto %>% 
  filter(cat_mp2=="Defraudación"
         #year(fecha_inicio)>=2022
  ) %>% 
  mutate(tipo=del_mp) %>%
  
  # mutate(tipo=case_when(
  #   relacion_victima_victimario ~ "Desconocido", 
  #   T ~ "Simple"
  # )) %>%
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date("2019-01-01"), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(.$tipo),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 year"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  mutate(nombre="defraudacion delitos")


#metemos delito_ministerial de fraude
fraude_min <- read_rds("fraude.rds")
fraude_ministerial <- fraude_min$ministerial

fraude_ministerial <- fraude_ministerial %>% 
  filter(grepl("SALARIO", delitoSIAP)) %>% 
  mutate(tipo=case_when(
   grepl("50 SALARIOS|NO EXCEDA DE 300|EXCEDA DE 20 Y MENOS DE 300|MÁS DE 50 Y MENOS DE 500|NO EXCEDA DE 20 SALARIOS|MÁS 500 Y MENOS DE 5,000|MÁS DE 50 Y MENOS DE 500", delitoSIAP) ~ "Montos pequeños", 
   grepl("EXCEDA DE 300 Y MENOS DE 750|500 Y MENOS DE 10,000|MÁS 500 Y MENOS DE 5,000|MÁS 500 Y MENOS DE 5,000|MÁS 5,000 Y MENOS DE 10,000", delitoSIAP) ~ "Montos medianos",
   T ~ "Montos grandes"
   )) %>% filter(!duplicated(idAveriguacionPrevia)) %>% 
  rename(id_ap=idAveriguacionPrevia) %>% 
  select(id_ap, tipo)


gr_fraude_monto <- data_bruto %>% 
  filter(cat_mp2=="Defraudación", 
         del_mp=="Fraude" 
         #year(fecha_inicio)>=2022
  ) %>% 
  left_join(fraude_ministerial) %>% 
  drop_na(tipo) %>% 
  
  # mutate(tipo=case_when(
  #   relacion_victima_victimario ~ "Desconocido", 
  #   T ~ "Simple"
  # )) %>%
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date("2019-01-01"), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(.$tipo),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 month"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  ggplot(aes(fecha_inicio, Total, color=tipo)) +
  geom_point(alpha=.7#, color=colores[1]
  ) +
  geom_smooth(se=F#, color=colores[7]
  ) + theme_light() + tema_fgj +
  # geom_smooth(se=F, color=colores[2], method = "lm") +
  scale_x_date(date_labels = "%b\n%y", date_breaks = "6 month") +
  labs(x="Fecha de inicio", y="Promedio diario", 
       color="",
       title = paste0("Promedio diario de carpetas por defraudación por tipo"), 
       subtitle = paste0("Desde ",  format(as_date("2019-01-01"), "%B de %Y"), " a ",
                         format(as_date(fecha_lim), "%B de %Y"))) +
  scale_color_manual(values = colores[3:1]) +
  theme(#axis.text.x = element_text(angle = 90), 
    legend.position = "bottom"
  )

ggsave(plot = gr_fraude_monto, 
       "./graficas_incidencia_la_revancha/especiales/defraudacion/gr_fraude_monto.svg", width = 12, height = 6
)

tablas_defraudacion[[2]] <- data_bruto %>% 
  filter(cat_mp2=="Defraudación", 
         del_mp=="Fraude" 
         #year(fecha_inicio)>=2022
  ) %>% 
  left_join(fraude_ministerial) %>% 
  drop_na(tipo) %>% 
  
  # mutate(tipo=case_when(
  #   relacion_victima_victimario ~ "Desconocido", 
  #   T ~ "Simple"
  # )) %>%
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date("2019-01-01"), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(.$tipo),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 year"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  mutate(nombre="fraude montos")

bind_rows(tablas_defraudacion) %>% 
  write.csv("./graficas_incidencia_la_revancha/especiales/defraudacion/tablas_defraudacion.csv",
            row.names = F
            )
#eficiencia cantidades grandes
gr_efi_fraude_grande <- graf_efi_ministerial_traza(datos = data_bruto %>% 
                                                     filter(cat_mp2=="Defraudación", 
                                                            del_mp=="Fraude" 
                                                            #year(fecha_inicio)>=2022
                                                     ) %>% 
                                                     left_join(fraude_ministerial) %>% 
                                                     filter(tipo=="Montos grandes")
                                                     , 
                                           fecha_inicio_global = "2019-01-01", 
                                           fecha_lim = fecha_lim, 
                                           delito_elegido =c("Fraude montos grandes")
                                           
)


ggsave(plot = gr_efi_fraude_grande, 
       "./graficas_incidencia_la_revancha/especiales/defraudacion/gr_efi_fraude_grande.svg", width = 12, height = 6
)
#cobranza ilegítima
cobranza <- read_rds("cobranza_ilegitima.rds")



#####
#corrupción
gr_corrupcion_tipo <- data_bruto %>% 
  filter(cat_mp2=="Corrupción"
         #year(fecha_inicio)>=2022
  ) %>% 
  mutate(tipo=del_mp) %>%

  
  mutate(tipo=case_when(
    tipo=="Cohecho" ~ "Cohecho",
    T ~ "Otros"
  )) %>%
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date("2019-01-01"), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(.$tipo),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 month"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  ggplot(aes(fecha_inicio, Total, color=tipo)) +
  geom_point(alpha=.7#, color=colores[1]
  ) +
  geom_smooth(se=F#, color=colores[7]
  ) + theme_light() + tema_fgj +
  # geom_smooth(se=F, color=colores[2], method = "lm") +
  scale_x_date(date_labels = "%b\n%y", date_breaks = "6 month") +
  labs(x="Fecha de inicio", y="Promedio diario", 
       color="",
       title = paste0("Promedio diario de carpetas por corrupción por delito"), 
       subtitle = paste0("Desde ",  format(as_date("2019-01-01"), "%B de %Y"), " a ",
                         format(as_date(fecha_lim), "%B de %Y"))) +
  scale_color_manual(values = colores[2:1]) +
  theme(#axis.text.x = element_text(angle = 90), 
    legend.position = "bottom"
  )

ggsave(plot = gr_corrupcion_tipo, 
       "./graficas_incidencia_la_revancha/especiales/corrupcion/gr_corrupcion_tipo.svg", width = 12, height = 6
)
tablas_corrupcion <- list()

tablas_corrupcion[[1]] <- data_bruto %>% 
  filter(cat_mp2=="Corrupción"
         #year(fecha_inicio)>=2022
  ) %>% 
  mutate(tipo=del_mp) %>%
  
  
  mutate(tipo=case_when(
    tipo=="Cohecho" ~ "Cohecho",
    T ~ "Otros"
  )) %>%
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date("2019-01-01"), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(.$tipo),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 year"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  mutate(nombre="corrupcion tipo de delito")
##cohecho monto
cohecho <- read_rds("cohecho.rds")
cohecho_ministerial <- cohecho$ministerial

cohecho_ministerial <- cohecho_ministerial %>% 
  filter(grepl("SALARIO", delitoSIAP)) %>% 
  mutate(tipo=case_when(
    grepl("NO EXCEDA DE 20 SALARIOS MÍNIMOS|NO EXCEDA DE 300 SALARIOS MÍNIMOS", delitoSIAP) ~ "Montos pequeños", 
    grepl("EXCEDA DE 20 Y MENOS DE 300 SALARIOS MÍNIMOS|NO EXCEDE DE 500 SALARIOS MÍNIMOS|MÁS DE 300 Y MENOS DE 750", delitoSIAP) ~ "Montos medianos",
    T ~ "Montos grandes"
  )) %>% filter(!duplicated(idAveriguacionPrevia)) %>% 
  rename(id_ap=idAveriguacionPrevia) %>% 
  select(id_ap, tipo)


gr_cohecho_monto <- data_bruto %>% 
  filter(cat_mp2=="Corrupción", 
         del_mp=="Cohecho" 
         #year(fecha_inicio)>=2022
  ) %>% 
  left_join(cohecho_ministerial) %>% 
  drop_na(tipo) %>% 
  
  # mutate(tipo=case_when(
  #   relacion_victima_victimario ~ "Desconocido", 
  #   T ~ "Simple"
  # )) %>%
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date("2019-01-01"), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(.$tipo),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 year"), tipo) %>% 
  summarise(Total=sum(Total)) %>% 
  ggplot(aes(fecha_inicio, Total, color=tipo)) +
  geom_point(alpha=.7#, color=colores[1]
  ) +
  geom_label_repel(aes(label=Total), 
                   direction = "y", show.legend = F
                   ) +
  geom_smooth(se=F#, color=colores[7]
  ) + theme_light() + tema_fgj +
  # geom_smooth(se=F, color=colores[2], method = "lm") +
  scale_x_date(date_labels = "%b\n%y", date_breaks = "6 month") +
  labs(x="Fecha de inicio", y="Total", 
       color="",
       title = paste0("Total de carpetas por corrupción por monto"), 
       subtitle = paste0("Desde ",  format(as_date("2019-01-01"), "%B de %Y"), " a ",
                         format(as_date(fecha_lim), "%B de %Y"))) +
  scale_color_manual(values = colores[3:1]) +
  theme(#axis.text.x = element_text(angle = 90), 
    legend.position = "bottom"
  )


tablas_corrupcion[[2]] <-  data_bruto %>% 
  filter(cat_mp2=="Corrupción", 
         del_mp=="Cohecho" 
         #year(fecha_inicio)>=2022
  ) %>% 
  left_join(cohecho_ministerial) %>% 
  drop_na(tipo) %>% 
  
  # mutate(tipo=case_when(
  #   relacion_victima_victimario ~ "Desconocido", 
  #   T ~ "Simple"
  # )) %>%
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date("2019-01-01"), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(.$tipo),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 year"), tipo) %>% 
  summarise(Total=sum(Total)) %>% 
  mutate(nombre="cohecho montos")
ggsave(plot = gr_cohecho_monto, 
       "./graficas_incidencia_la_revancha/especiales/corrupcion/gr_cohecho_monto.svg", width = 12, height = 6
)

bind_rows(tablas_corrupcion) %>% 
  write.csv("./graficas_incidencia_la_revancha/especiales/corrupcion/tablas_corrupcion.csv", 
            row.names = F
            )
#eficiencia cantidades grandes
gr_efi_cohecho_grande <- graf_efi_ministerial_traza(datos = data_bruto %>% 
                                                     filter(cat_mp2=="Corrupción", 
                                                            del_mp=="Cohecho", 
                                                            
                                                            #year(fecha_inicio)>=2022
                                                     ) %>% 
                                                     left_join(cohecho_ministerial) %>% 
                                                     filter(tipo=="Montos grandes")
                                                   , 
                                                   fecha_inicio_global = "2019-01-01", 
                                                   fecha_lim = fecha_lim, 
                                                   delito_elegido =c("Cohecho montos grandes")
                                                   
)


ggsave(plot = gr_efi_cohecho_grande, 
       "./graficas_incidencia_la_revancha/especiales/corrupcion/gr_efi_cohecho_grande.svg", width = 12, height = 6
)

#####
#violencia y conflictos familiares
#violencia_delito
gr_violencia_tipo <- data_bruto %>% 
  filter(cat_mp2=="Violencia y conflictos familiares"
         #year(fecha_inicio)>=2022
  ) %>% 
  mutate(tipo=del_mp) %>%
  
  
  mutate(tipo=case_when(
    tipo=="Violencia familiar" ~ "Violencia familiar" ,
    T ~ "Otros"
  )) %>%
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date("2019-01-01"), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(.$tipo),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 month"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  ggplot(aes(fecha_inicio, Total, color=tipo)) +
  geom_point(alpha=.7#, color=colores[1]
  ) +
  geom_smooth(se=F#, color=colores[7]
  ) + theme_light() + tema_fgj +
  # geom_smooth(se=F, color=colores[2], method = "lm") +
  scale_x_date(date_labels = "%b\n%y", date_breaks = "6 month") +
  labs(x="Fecha de inicio", y="Promedio diario", 
       color="",
       title = paste0("Promedio diario de carpetas por violencia y conflictos familiares por delito"), 
       subtitle = paste0("Desde ",  format(as_date("2019-01-01"), "%B de %Y"), " a ",
                         format(as_date(fecha_lim), "%B de %Y"))) +
  scale_color_manual(values = colores[2:1]) +
  theme(#axis.text.x = element_text(angle = 90), 
    legend.position = "bottom"
  )

ggsave(plot = gr_violencia_tipo, 
       "./graficas_incidencia_la_revancha/especiales/violencia_familiar/gr_violencia_tipo.svg", width = 12, height = 6
)

tablas_violencia_conflictos <- list()

tablas_violencia_conflictos[[1]] <-  data_bruto %>% 
  filter(cat_mp2=="Violencia y conflictos familiares"
         #year(fecha_inicio)>=2022
  ) %>% 
  mutate(tipo=del_mp) %>%
  
  
  mutate(tipo=case_when(
    tipo=="Violencia familiar" ~ "Violencia familiar" ,
    T ~ "Otros"
  )) %>%
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date("2019-01-01"), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(.$tipo),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 year"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  mutate(nombre="violencia y conflictos por delito")
#violencia familiar tipo

fam_23 <- read_excel("01 VIOLENCIA FAMILIAR 2023_31dic2023.xlsx") %>% clean_names() %>% 
  select(id_ap, tipo_de_agresion_moral, tipo_de_agresion_fisica)
fam_24 <- read_excel("01 VIOLENCIA FAMILIAR 2024_31dic2024.xlsx")%>% clean_names() %>% 
  # mutate(tipo_de_agresion_moral=case_when(
  #   tipo_de_agresion_moral=="No" ~NA, T ~tipo_de_agresion_moral
  # ), tipo_de_agresion_fisica=case_when(
  #   tipo_de_agresion_fisica %in% c("No", "NO", "no") ~NA, T ~tipo_de_agresion_fisica
  # )) %>% 
  select(id_ap, tipo_de_agresion_moral, tipo_de_agresion_fisica)
fam_22 <- read_excel("04 VIOLENCIA FAMILIAR 2022_31dic2022.xlsx")%>% clean_names() %>%
  rename(tipo_de_agresion_moral=moral, tipo_de_agresion_fisica=fisica) %>% 
  mutate(tipo_de_agresion_moral=case_when(
    tipo_de_agresion_moral=="No" ~NA, T ~tipo_de_agresion_moral
  ), tipo_de_agresion_fisica=case_when(
    tipo_de_agresion_fisica %in% c("No", "NO", "no") ~NA, T ~tipo_de_agresion_fisica
  )) %>% 
  select(id_ap, tipo_de_agresion_moral, tipo_de_agresion_fisica)

# violencia_familiar <- read_excel("violencia_familiar_variable.xlsx") %>% 
#   clean_names() %>% drop_na(id_ap)
# 
# violencia_familiar <- violencia_familiar %>% 
#   filter(!duplicated(id_ap))

familiar <- bind_rows(fam_22, fam_23) %>% bind_rows(fam_24) %>% 
  filter(!duplicated(id_ap)) %>% 
  mutate(tipo_agresion=case_when(
    !is.na(tipo_de_agresion_fisica) & 
      !is.na(tipo_de_agresion_moral) ~ "Física y moral", 
    !is.na(tipo_de_agresion_fisica) & 
      is.na(tipo_de_agresion_moral) ~ "Física", 
    is.na(tipo_de_agresion_fisica) & 
      !is.na(tipo_de_agresion_moral) ~ "Moral", 
    T ~ NA
  )) %>% drop_na(tipo_agresion)

# familiar <- data %>%
#   filter(fecha_inicio>="2022-01-01") %>% 
#   filter(modalidad_delito=="VIOLENCIA FAMILIAR") %>% 
#   left_join(violencia_familiar, by="id_ap")

viol_fam <- data %>% filter(modalidad_delito=="VIOLENCIA FAMILIAR") %>% 
  filter(fecha_inicio>="2022-01-01") %>% 
  left_join(familiar, by="id_ap") #%>% 
# # rename(tipo_mau=tipo_agresion) %>% 
# left_join(violencia_familiar, by="id_ap") %>% 
# rename(tipo_javier=tipo_agresion)
id_violenca_fisica <- familiar %>% 
  filter(grepl("sica", tipo_agresion)) %>% pull(id_ap)

gr_fam <- viol_fam %>% drop_na(tipo_agresion) %>% 
  # filter(delito=="VIOLACIÓN") %>% 
  # mutate(tipo=case_when(
  #   grepl("EQUIPAR", modalidad_delito) ~ "Equiparada", T ~ "Simple"
  # )) %>% 
  # filter(delito=="ROBO DE VEHÍCULO CON Y SIN VIOLENCIA", 
  #        #grepl("CON VIOLE|C/V", modalidad_delito),
  #        year(fecha_inicio) %in% c(2020:2025)) %>% 
  # mutate(tipo_agresion=case_when(
  #   tipo_agresion=="economica" ~ "Moral", 
  #   tipo_agresion=="moral" ~ "Moral", 
  #   tipo_agresion=="fisica" ~ "Física", 
#   tipo_agresion=="fisica y moral" ~ "Física y Moral", 
#   
# )) %>% 
group_by(fecha_inicio, tipo_agresion) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date("2022-01-01"), 
                                 as_date(fecha_lim), "1 day"), 
           tipo_agresion=c("Moral", "Física", "Física y moral"),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 month"), tipo_agresion) %>% 
  summarise(Total=mean(Total)) %>% 
  ggplot(aes(fecha_inicio, Total, color=tipo_agresion)) +
  geom_point(alpha=.7#, color=colores[1]
  ) +
  geom_smooth(se=F#, color=colores[7]
  ) + theme_light() + tema_fgj +
  #facet_wrap(.~tipo_agresion, scales = "free_y", nrow = 3) +
  # geom_smooth(se=F, color=colores[2], method = "lm") +
  scale_x_date(date_labels = "%b-%y", date_breaks = "3 month") +
  labs(x="Fecha de inicio", y="Promedio diario", 
       color="",
       title = paste0("Promedio diario de carpetas por violencia familiar según tipo"), 
       subtitle = paste0("Desde ",  format(as_date("2022-01-01"), "%B de %Y"), " a ",
                         format(as_date(fecha_lim), "%B de %Y"))) +
  scale_color_manual(values = colores[4:1]) +
  theme(axis.text.x = element_text(angle = 90), 
        legend.position = "bottom"
  )


ggsave(plot = gr_fam, 
       "./graficas_incidencia_la_revancha/especiales/violencia_familiar/gr_fam_tipo.svg", width = 14, height = 7
)

tablas_violencia_conflictos[[2]] <- viol_fam %>% drop_na(tipo_agresion) %>% 
  
group_by(fecha_inicio, tipo_agresion) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date("2022-01-01"), 
                                 as_date(fecha_lim), "1 day"), 
           tipo_agresion=c("Moral", "Física", "Física y moral"),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 year"), tipo_agresion) %>% 
  summarise(Total=mean(Total)) %>% 
  mutate(nombre="Violencia familiar agresiones")

bind_rows(tablas_violencia_conflictos) %>% 
  write.csv("./graficas_incidencia_la_revancha/especiales/violencia_familiar/tablas_violencias_conflictos.csv", 
            row.names = F)

###eficiencia ministerial

gr_efi_viol_fisica <- graf_efi_ministerial_traza(datos = data_bruto %>% 
                                                      filter(cat_mp2=="Violencia y conflictos familiares", 
                                                             del_mp=="Violencia familiar", 
                                                             id_ap %in% id_violenca_fisica,
                                                             year(fecha_inicio)>=2022
                                                      ) 
                                                    , 
                                                    fecha_inicio_global = "2022-01-01", 
                                                    fecha_lim = fecha_lim, 
                                                    delito_elegido =c("violencia familiar con agresiones físicas")
                                                    
)


ggsave(plot = gr_efi_viol_fisica, 
       "./graficas_incidencia_la_revancha/especiales/violencia_familiar/gr_efi_viol_fisica.svg", width = 12, height = 6
)


#####
#Transito vehicular
transito_vehi <- read_rds("transito_vehi.rds")

id_alcohol <- transito_vehi$entrevistas %>% 
  filter(tipo=="Alcoholizado") %>% pull(id_ap)


id_alcohol <- transito_vehi$entrevistas2 %>% 
  filter(tipo=="Alcoholizado") %>% pull(id_ap)

gr_hom_alcohol <- data_bruto %>% 
  filter(cat_mp2=="Tránsito vehicular", 
         del_mp=="Homcidio culposo transito", 
         
         #year(fecha_inicio)>=2022
  ) %>% 
  mutate(tipo=case_when(
    id_ap %in% id_alcohol ~ "Alcohol", 
    T ~ "Otros"
  )) %>% 
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date("2019-01-01"), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(.$tipo),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 month"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  ggplot(aes(fecha_inicio, Total, color=tipo)) +
  geom_point(alpha=.7#, color=colores[1]
  ) +
  geom_smooth(se=F#, color=colores[7]
  ) + theme_light() + tema_fgj +
  # geom_smooth(se=F, color=colores[2], method = "lm") +
  scale_x_date(date_labels = "%b\n%y", date_breaks = "6 month") +
  labs(x="Fecha de inicio", y="Promedio diario", 
       color="",
       title = paste0("Promedio diario de carpetas por homicidio culposo por tránsito vehícular con alcohol"), 
       subtitle = paste0("Desde ",  format(as_date("2019-01-01"), "%B de %Y"), " a ",
                         format(as_date(fecha_lim), "%B de %Y"))) +
  scale_color_manual(values = colores[2:1]) +
  theme(#axis.text.x = element_text(angle = 90), 
    legend.position = "bottom"
  )

ggsave(plot = gr_hom_alcohol, 
       "./graficas_incidencia_la_revancha/especiales/transito_veh/gr_hom_alcohol.svg", width = 12, height = 6
)


data_bruto %>% 
  filter(cat_mp2=="Tránsito vehicular", 
         del_mp=="Homcidio culposo transito", 
         
         #year(fecha_inicio)>=2022
  ) %>% 
  mutate(tipo=case_when(
    id_ap %in% id_alcohol ~ "Alcohol", 
    T ~ "Otros"
  )) %>% 
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date("2019-01-01"), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(.$tipo),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 year"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  write.csv("./graficas_incidencia_la_revancha/especiales/transito_veh/tabla_transito.csv", 
            row.names = F
            )

###eficiencia ministerial

gr_efi_hom_alcohol <- graf_efi_ministerial_traza(datos = data_bruto %>% 
                                                   filter(cat_mp2=="Tránsito vehicular", 
                                                          del_mp=="Homcidio culposo transito", 
                                                          
                                                          #year(fecha_inicio)>=2022
                                                   )
                                                 , 
                                                 fecha_inicio_global = "2019-01-01", 
                                                 fecha_lim = fecha_lim, 
                                                 delito_elegido =c("Homicidio culposo por tránsito con alcohol")
                                                 
)


ggsave(plot = gr_efi_hom_alcohol, 
       "./graficas_incidencia_la_revancha/especiales/transito_veh/gr_efi_hom_alcohol.svg", width = 12, height = 6
)


#####
gr_convivencia_tipo <- data_bruto %>% 
  filter(cat_mp2=="Convivencia y tranquilidad" , 
         # del_mp=="Homcidio culposo transito", 
         
         #year(fecha_inicio)>=2022
  ) %>% 
  mutate(tipo=case_when(
    del_mp %in% "Amenazas" ~ "Amenazas", 
    T ~ "Otros"
  )) %>% 
  group_by(fecha_inicio, tipo) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date("2019-01-01"), 
                                 as_date(fecha_lim), "1 day"), 
           tipo=unique(.$tipo),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 month"), tipo) %>% 
  summarise(Total=mean(Total)) %>% 
  ggplot(aes(fecha_inicio, Total, color=tipo)) +
  geom_point(alpha=.7#, color=colores[1]
  ) +
  geom_smooth(se=F#, color=colores[7]
  ) + theme_light() + tema_fgj +
  # geom_smooth(se=F, color=colores[2], method = "lm") +
  scale_x_date(date_labels = "%b\n%y", date_breaks = "6 month") +
  labs(x="Fecha de inicio", y="Promedio diario", 
       color="",
       title = paste0("Promedio diario de carpetas por convivencia y tranquilidad por tipo"), 
       subtitle = paste0("Desde ",  format(as_date("2019-01-01"), "%B de %Y"), " a ",
                         format(as_date(fecha_lim), "%B de %Y"))) +
  scale_color_manual(values = colores[2:1]) +
  theme(#axis.text.x = element_text(angle = 90), 
    legend.position = "bottom"
  )


ggsave(plot = gr_convivencia_tipo, 
       "./graficas_incidencia_la_revancha/especiales/convivencia/gr_convivencia_tipo.svg", width = 12, height = 6
)
