####
setwd("C:/Users/mauri/OneDrive/Documentos/R/fiscalia/plan_politica_criminal")
#plan de política criminal


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




####Generamos la base de datos
data_bruto <- data %>% 
  # filter(delito %in% c("ROBO A TRANSEUNTE EN VÍA PÚBLICA CON Y SIN VIOLENCIA", 
  #                      "ROBO A NEGOCIO CON VIOLENCIA", "HOMICIDIO DOLOSO", 
  #                      "LESIONES DOLOSAS POR DISPARO DE ARMA DE FUEGO", 
  #                      "ROBO DE VEHÍCULO CON Y SIN VIOLENCIA", "VIOLACIÓN", 
  #                      "ROBO A REPARTIDOR CON Y SIN VIOLENCIA", 
  #                      "ROBO A PASAJERO A BORDO DE MICROBUS CON Y SIN VIOLENCIA", "ROBO A CASA HABITACIÓN CON VIOLENCIA", 
  #                      "ROBO A PASAJERO A BORDO DEL METRO CON Y SIN VIOLENCIA", 
  #                      "ROBO A CUENTAHABIENTE SALIENDO DEL CAJERO CON VIOLENCIA", 
  #                      "ROBO A PASAJERO A BORDO DE TAXI CON VIOLENCIA", 
  #                      "ROBO A TRANSPORTISTA CON Y SIN VIOLENCIA"
  #                      ) |
  #          modalidad_delito %in% c("EXTORSION", "VIOLENCIA FAMILIAR", 
  #                                  "DESPOJO", "MALTRATO ANIMAL","ROBO DE ANIMALES", 
  #                                  grep("AMBI", unique(data$modalidad_delito), value = T), "TALA", 
  #                                  "TENTATIVA DE FEMINICIDIO", "TENTATIVA DE HOMICIDIO", 
  #                                  "FRAUDE", "AMENAZAS", "USO DE DOCUMENTO FALSO",
  #                                  "USURPACIÓN DE IDENTIDAD", 
  #                                  grep("NARCO", unique(data$modalidad_delito), value = T), 
  #                                  "TRATA DE PERSONAS", 
  #                                  "TENTATIVA DE VIOLACION", "ACOSO SEXUAL", "ABUSO SEXUAL", 
  #                                  "CONTRA LA INTIMIDAD SEXUAL"
  #                                  )
  #        
  #        
  #        ) %>% 
  mutate(tipo_delito=case_when(
    grepl("PASAJERO", modalidad_delito) ~ "Robo a pasajero",
    grepl("FEMINICI", modalidad_delito) & delito=="HOMICIDIO DOLOSO" ~ "Feminicidio",
    grepl("LESIONES INTENCIO|LESIONES DOLOS", modalidad_delito) ~ "Lesiones dolosas",
    grepl("ROBO A CASA HABITACION", modalidad_delito) ~ "Robo a casa habitación",
    delito=="ROBO DE VEHÍCULO CON Y SIN VIOLENCIA" ~ "Robo de vehículo",
    delito %in% c("ROBO A TRANSEUNTE EN VÍA PÚBLICA CON Y SIN VIOLENCIA", 
                  "ROBO A NEGOCIO CON VIOLENCIA", "HOMICIDIO DOLOSO", 
                  "LESIONES DOLOSAS POR DISPARO DE ARMA DE FUEGO", 
                  "ROBO DE VEHÍCULO CON Y SIN VIOLENCIA", "VIOLACIÓN", 
                  "ROBO A REPARTIDOR CON Y SIN VIOLENCIA", 
                  # "ROBO A PASAJERO A BORDO DE MICROBUS CON Y SIN VIOLENCIA", "ROBO A CASA HABITACIÓN CON VIOLENCIA", 
                  # "ROBO A PASAJERO A BORDO DEL METRO CON Y SIN VIOLENCIA", 
                  "ROBO A CUENTAHABIENTE SALIENDO DEL CAJERO CON VIOLENCIA", 
                  # "ROBO A PASAJERO A BORDO DE TAXI CON VIOLENCIA", 
                  "ROBO A TRANSPORTISTA CON Y SIN VIOLENCIA"
    ) ~ str_to_sentence(delito), 
    modalidad_delito %in% c(grep("AMBI", unique(data$modalidad_delito), value = T), "TALA") ~ "Ambientales", 
    modalidad_delito %in% grep("NARCO", unique(data$modalidad_delito), value = T) ~ "Narcomenudeo", 
    grepl("ANIMAL", modalidad_delito) ~ "Delitos contra animales",
    grepl("NARCO", modalidad_delito) ~ "Narcomenudeo",
    grepl("PORNO", modalidad_delito) ~ "Pornografía infantil",
    grepl("CORRUP", modalidad_delito) ~ "Corrupción de menores",
    grepl("PRIVACI", modalidad_delito) | modalidad_delito=="PRIV. ILEGAL DE LA LIB. Y ROBO DE VEHICULO"~ "Privación ilegal de la libertad",
    # modalidad_delito %in% c("TENTATIVA DE FEMINICIDIO", "TENTATIVA DE HOMICIDIO") ~ "Tentativa de homicidio y feminicidio", 
    grepl("ROBO", modalidad_delito) & grepl("CON VIOLENCIA|C/V", modalidad_delito) ~ "Robo con violencia",
    grepl("RETEN|SUSTR", modalidad_delito) ~ "Retención y sustracción de menores", 
    grepl("DESAP", modalidad_delito) ~ "Desaparición forzada", 
    grepl("RECURS", modalidad_delito) ~ "ORPI", 
    modalidad_delito=="EXTORSION" ~ "Extorsión", 
    grepl("SECUE", delito) ~ "Secuestro", 
    modalidad_delito=="EVASION DE PRESOS" ~ "Evasión de presos",
    T ~ str_to_sentence(modalidad_delito)
    
  ))
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

delitos_ppp <- c("Homicidio doloso", 
                 "Feminicidio", "Lesiones dolosas", "Robo de vehículo", 
                 "Robo a casa habitación", "Robo con violencia", 
                 "Secuestro", "Extorsión", "Narcomenudeo", 
                 "Privación ilegal de la libertad","Violación", "Abuso sexual", 
                 "Acoso sexual", "Contra la intimidad sexual", 
                 "Estupro", "Pornografía infantil", "Trata de personas", 
                 "Corrupción de menores", "Retención y sustracción de menores", 
                 "Violencia familiar", "Desaparición forzada", 
                 "Tortura", "ORPI", "Enriquecimiento ilícito", 
                 "Evasión de presos", "Sabotaje"
                 
                 )



gr <- list()
for (i in 1:length(delitos_ppp)) {
  gr[[i]] <- grafica_tendencia(
    datos=data_bruto %>% 
      filter(tipo_delito %in% delitos_ppp[i]), 
    fecha_inicio_global = fecha_inicio_global, 
    fecha_lim = fecha_lim, 
    delito = delitos_ppp[[i]]
  )
  
  
  ggsave(plot = gr[[i]], 
         paste0("./graficas_incidencia/", delitos_ppp[i], ".svg"), 
         width = 14, height = 7
         )
  
  
}

tablas <- list()

for (i in 1:length(delitos_ppp)) {
  tablas[[i]] <- data_bruto %>% 
      filter(tipo_delito %in% delitos_ppp[i]) %>% 
  group_by(fecha_inicio) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date(fecha_inicio_global), 
                                 as_date(fecha_lim), "1 day"), 
           #tipo=c("Secuestro", "Secuestro express"),
           fill=list(Total=0)
  ) %>% group_by(fecha_inicio=floor_date(fecha_inicio, "1 year")) %>% 
  summarise(Total=mean(Total)) %>% 
    mutate(tipo_delito=delitos_ppp[i])
  
  

  
}

tablas_promedios <- bind_rows(tablas)
tablas_promedios %>% 
  write.csv("tablas_promedios_delitos.csv", row.names = F)

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

ordenes <- ordenes %>% 
  mutate(tipo_delito=case_when(
    grepl("FEMIN", delito_hom_uet) & !grepl("TENTA", delito_hom_uet) ~ "Feminicidio", 
    delito_alto_impacto=="HOMICIDIO DOLOSO" ~ "Homicidio doloso", 
    delito_hom_uet=="LESIONES DOLOSAS" ~ "Lesiones dolosas", 
    grepl("CASA", delito_hom_uet) ~ "Robo a casa habitación", 
    grepl("SECU", delito_hom_uet) ~ "Secuestro", 
    grepl("NARCO", delito_hom_uet) ~ "Narcomenudeo", 
    delito_hom_uet=="VIOLACIÓN" ~ "Violación", 
    delito_hom_uet=="ACOSO SEXUAL" ~ "Acoso sexual", 
    delito_hom_uet=="ESTUPRO" ~"Estupro", 
    delito_hom_uet=="TRATA DE PERSONAS" ~"Trata de personas", 
    delito_hom_uet %in% c( "RETENCIÓN DE MENORES", 
                           "SUSTRACCIÓN DE MENORES", 
                           "SUSTRACCIÓN DE MENORES O INCAPACES",
                           "RETENCIÓN DE MENORES O INCAPACES" 
    ) ~ "Retención y sustracción de menores",
    delito_hom_uet %in% c("DESAPARICIÓN FORZADA DE PERSONAS",
                          "DESAPARICIÓN COMETIDA POR PARTICULARES") ~ "Desaparición forzada", 
    delito_hom_uet %in% "OPERACIONES CON RECURSOS DE PROCEDENCIA ILÍCITA" ~ "ORPI", 
    delito_hom_uet %in% "EVASIÓN DE PRESOS" ~ "Evasión de presos", 
    delito_alto_impacto %in% "ROBO DE VEHÍCULO CON Y SIN VIOLENCIA" ~ "Robo de vehículo", 
    delito_hom_uet %in% "EXTORSIÓN" ~ "Extorsión", 
    grepl("LIBERTAD", delito_hom_uet) & !grepl("TENTA", delito_hom_uet) ~ "Privación ilegal de la libertad", 
    delito_hom_uet %in% "ABUSO SEXUAL" ~ "Abuso sexual", 
    delito_hom_uet %in% "VIOLENCIA FAMILIAR" ~ "Violencia familiar",
    delito_hom_uet %in% "CONTRA LA INTIMIDAD SEXUAL" ~ "Contra la intimidad sexual", 
    delito_hom_uet %in% c("PORNOGRAFÍA INFANTIL", "PORNOGRAFÍA") ~ "Pornografía infantil",
    grepl("CORRUPCI.N DE MENO", delito_hom_uet) ~ "Corrupción de menores", 
    grepl("TORTUR", delito_hom_uet) ~ "Tortura",
    grepl("ENRIQUE", delito_hom_uet) ~ "Enriquecimiento ilícito", 
    grepl("SABOTAJE", delito_hom_uet) ~ "Sabotaje", 
    grepl("ROBO", delito_hom_uet) & 
      grepl("CON VIOLENCIA|C/V", delito_hom_uet) ~ "Robo de vehículo con violencia", 
    
    
    
    T ~ str_to_sentence(delito_hom_uet)
  )
  )


sentencias <- sentencias %>% 
  mutate(tipo_delito=case_when(
    grepl("FEMIN", delito_hom_uet) & !grepl("TENTA", delito_hom_uet) ~ "Feminicidio", 
    delitos_alto_impacto=="HOMICIDIO DOLOSO" ~ "Homicidio doloso", 
    delito_hom_uet=="LESIONES DOLOSAS" ~ "Lesiones dolosas", 
    grepl("CASA", delito_hom_uet) ~ "Robo a casa habitación", 
    grepl("SECU", delito_hom_uet) ~ "Secuestro", 
    grepl("NARCO", delito_hom_uet) ~ "Narcomenudeo", 
    delito_hom_uet=="VIOLACIÓN" ~ "Violación", 
    delito_hom_uet=="ACOSO SEXUAL" ~ "Acoso sexual", 
    delito_hom_uet=="ESTUPRO" ~"Estupro", 
    delito_hom_uet=="TRATA DE PERSONAS" ~"Trata de personas", 
    delito_hom_uet %in% c( "RETENCIÓN DE MENORES", 
                           "SUSTRACCIÓN DE MENORES", 
                           "SUSTRACCIÓN DE MENORES O INCAPACES",
                           "RETENCIÓN DE MENORES O INCAPACES" 
                           ) ~ "Retención y sustracción de menores",
    delito_hom_uet %in% c("DESAPARICIÓN FORZADA DE PERSONAS",
                          "DESAPARICIÓN COMETIDA POR PARTICULARES") ~ "Desaparición forzada", 
    delito_hom_uet %in% "OPERACIONES CON RECURSOS DE PROCEDENCIA ILÍCITA" ~ "ORPI", 
    delito_hom_uet %in% "EVASIÓN DE PRESOS" ~ "Evasión de presos", 
    delitos_alto_impacto %in% "ROBO DE VEHÍCULO CON Y SIN VIOLENCIA" ~ "Robo de vehículo", 
    delito_hom_uet %in% "EXTORSIÓN" ~ "Extorsión", 
    grepl("LIBERTAD", delito_hom_uet) & !grepl("TENTA", delito_hom_uet) ~ "Privación ilegal de la libertad", 
    delito_hom_uet %in% "ABUSO SEXUAL" ~ "Abuso sexual", 
    delito_hom_uet %in% "VIOLENCIA FAMILIAR" ~ "Violencia familiar",
    delito_hom_uet %in% "CONTRA LA INTIMIDAD SEXUAL" ~ "Contra la intimidad sexual", 
    delito_hom_uet %in% c("PORNOGRAFÍA INFANTIL", "PORNOGRAFÍA") ~ "Pornografía infantil",
    grepl("CORRUPCI.N DE MENO", delito_hom_uet) ~ "Corrupción de menores", 
    grepl("TORTUR", delito_hom_uet) ~ "Tortura",
    grepl("ENRIQUE", delito_hom_uet) ~ "Enriquecimiento ilícito", 
    grepl("SABOTAJE", delito_hom_uet) ~ "Sabotaje", 
    grepl("ROBO", delito_hom_uet) & 
      grepl("CON VIOLENCIA|C/V", delito_hom_uet) ~ "Robo de vehículo con violencia", 
    
    
    
    T ~ str_to_sentence(delito_hom_uet)
  ))

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
    labs(x="Fecha de inicio", y="Total", 
         title = paste0("Total de vinculaciones y carpetas por ", knitr::combine_words(delito_elegido, and = " y ")), 
         subtitle = paste0("Desde ",  format(as_date(fecha_inicio_global), "%B de %Y"), " a ",
                           format(as_date(fecha_lim), "%B de %Y"))) +
    theme(axis.text.x = element_text(angle = 90)) +
    tema_fgj + 
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
         title = paste0("Total de sentencias y carpetas por ", knitr::combine_words(delito_elegido, and = " y ")), 
         subtitle = paste0("Desde ",  format(as_date(fecha_inicio_global), "%B de %Y"), " a ",
                           format(as_date(fecha_lim), "%B de %Y"))) +
    theme(axis.text.x = element_text(angle = 90)) +
    tema_fgj + 
    theme(legend.position = "bottom") +
    scale_color_manual(values = c(colores[7], colores[4]))
  
  gr_trend <- cowplot::plot_grid(gr_trend_vinc, gr_trend_sen, nrow = 2
                     
                     )
  
  return(gr_trend)
  
  
}





gr_efi_hom <- graf_efi_ministerial(datos = data_bruto %>% 
                       filter(tipo_delito=="Homicidio doloso"), 
                     fecha_inicio_global = fecha_inicio_global, 
                     fecha_lim = fecha_lim, 
                     delito ="Homicidio doloso"
                     
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
                         filter(tipo_delito==delitos_ppp[[i]]), 
                       fecha_inicio_global = fecha_inicio_global, 
                       fecha_lim = fecha_lim, 
                       delito =delitos_ppp[[i]]
                       
  )
  
  
  ggsave(plot = gr_efi[[i]], 
         paste0("./graficas_incidencia/eficiencia_min/", delitos_ppp[i], ".png"), 
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

####
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
