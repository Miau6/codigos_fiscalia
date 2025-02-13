####Script de evaluación de agencias####
setwd("~/R/fiscalia/Evaluaciones_agencias")
# iniciadas_uet <- read_rds()
iniciadas <- readxl::read_excel("Iniciadas por Territoriales.xlsx") %>% clean_names()


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

#filtramos por fiscalía territorial
grafica_cargas_trabajo <- function(
  datos=iniciadas, 
  fiscalia_analisis=siglas_fiscalia_analisis,
  fecha_comienzo=inicio_quincena, 
  fecha_fin=final_quincena, 
  tipo = c("total", "promedio")
){
  data <- datos %>% 
    filter(grepl(fiscalia_analisis, ct_inicio_ap)) %>% 
    mutate(fecha_inicio=dmy(fecha_de_inicio)) %>% 
    filter(fecha_inicio>=fecha_comienzo, 
           fecha_inicio<=fecha_fin
    ) %>% 
    mutate(quincena=case_when(
      fecha_inicio>=fecha_comienzo ~ "Actual",
      T ~ "Anterior"
    ), 
    detenido=case_when(
      id_ap %in% flagrancias$id_ap ~ "Con detenido", 
      T ~ "Sin detenido"
    )
    ) 
  
  if(tipo=="promedio"){
    data_1_sd <- data %>% 
      filter(quincena=="Actual", 
             detenido=="Sin detenido") %>% 
      group_by(ct_inicio_ap, #detenido, 
               fecha_inicio
      ) %>% 
      summarise(Total=n()) %>% ungroup() %>% 
      complete(fecha_inicio=seq.Date(as_date(fecha_comienzo),
                                     as_date(fecha_fin), 
                                     "1 day"
      ), 
      #detenido=c("Con detenido", "Sin detenido"), 
      ct_inicio_ap=unique(data$ct_inicio_ap), 
      fill=list(Total=0)
      
      ) %>% 
      group_by(ct_inicio_ap) %>% 
      summarise(media=round(mean(Total), 2)) 
    
    data_1_cd <- data %>% 
      filter(quincena=="Actual", 
             detenido=="Con detenido") %>% 
      group_by(ct_inicio_ap, #detenido, 
               fecha_inicio
      ) %>% 
      summarise(Total=n()) %>% ungroup() %>% 
      complete(fecha_inicio=seq.Date(as_date(fecha_comienzo),
                                     as_date(fecha_fin), 
                                     "1 day"
      ), 
      #detenido=c("Con detenido", "Sin detenido"), 
      ct_inicio_ap=unique(.$ct_inicio_ap), 
      fill=list(Total=0)
      
      )  %>% 
      group_by(ct_inicio_ap) %>% 
      summarise(media=round(mean(Total), 2)) %>% 
      mutate(detenido="Con detenido", 
             ct_inicio_ap=paste0(ct_inicio_ap, "\nCD")
      )
    
    prom_territoriales_sd <- iniciadas %>% 
      mutate(fecha_inicio=dmy(fecha_de_inicio)) %>% 
      filter(fecha_inicio>=fecha_comienzo, 
             fecha_inicio<=fecha_fin
      ) %>% 
      filter(!id_ap %in% data$id_ap ) %>% 
      mutate(detenido=case_when(
        id_ap %in% flagrancias$id_ap ~ "Con detenido", T ~ "Sin detenido" 
      )) %>% 
      filter(detenido=="Sin detenido") %>% 
      group_by(fecha_inicio, ct_inicio_ap) %>% 
      summarise(Total=n()) %>% ungroup() %>%  
      complete(
        fecha_inicio=seq.Date(as_date("2025-01-16"),
                              as_date("2025-01-31"),
                              "1 day"
        ),
        ct_inicio_ap=unique(.$ct_inicio_ap), 
        fill=list(Total=0)
        
      )  %>% 
      summarise(media=round(mean(Total), 2)) %>% pull(media)
    
    
    prom_territoriales_cd <- iniciadas %>% 
      mutate(fecha_inicio=dmy(fecha_de_inicio)) %>% 
      filter(fecha_inicio>=fecha_comienzo, 
             fecha_inicio<=fecha_fin
      ) %>% 
      filter(!id_ap %in% data$id_ap ) %>% 
      mutate(detenido=case_when(
        id_ap %in% flagrancias$id_ap ~ "Con detenido", T ~ "Sin detenido" 
      )) %>% 
      filter(detenido=="Con detenido") %>% 
      group_by(fecha_inicio, ct_inicio_ap) %>% 
      summarise(Total=n()) %>% ungroup() %>%  
      complete(
        fecha_inicio=seq.Date(as_date(fecha_comienzo),
                              as_date(fecha_fin),
                              "1 day"
        ),
        ct_inicio_ap=unique(.$ct_inicio_ap), 
        fill=list(Total=0)
        
      )  %>% 
      summarise(media=round(mean(Total), 2)) %>% pull(media)
    
    orden_data_1_sd <- data_1_sd %>% arrange(-media) %>% pull(ct_inicio_ap)
    orden_data_1_cd <- data_1_cd %>% arrange(-media) %>% pull(ct_inicio_ap)
    
    prom_fisc <- bind_rows(data_1_sd, data_1_cd) %>% 
      summarise(media=round(mean(media), 2)) %>% pull(media)
    
    gr_1 <- bind_rows(data_1_sd, data_1_cd) %>% 
      rename(Total=media) %>% 
      # data %>% 
      # filter(quincena=="Actual", ) %>% 
      # group_by(ct_inicio_ap, detenido#, 
      #          #fecha_inicio
      # ) %>% 
      # summarise(Total=n()) %>% 
      mutate(
        # ct_inicio_ap=ifelse(detenido=="Con detenido", 
        #                     paste0(ct_inicio_ap, "\nCD"), ct_inicio_ap
        # ),
        comparacion=ifelse(Total>=prom_fisc, 1, 0), 
        ct_inicio_ap=factor(ct_inicio_ap, 
                            levels=c(orden_data_1_sd,orden_data_1_cd)
        )) %>% 
      ggplot(aes(ct_inicio_ap, Total, fill=factor(comparacion))) +
      geom_col() +
      geom_hline(aes(yintercept=prom_territoriales_sd), 
                 linetype="dashed", size=1, color=colores[1]
      ) + theme_light() +
      geom_hline(aes(yintercept=prom_territoriales_cd), 
                 linetype="dashed", size=1, color=colores[1]
      ) +
      geom_text(aes(label=Total), 
                position = position_stack(vjust = 1.1), size=5.5
      ) +
      tema_fgj +
      geom_vline(aes(xintercept=length(orden_data_1_sd)+0.5), 
                 linetype="dashed", size=1.5, color="grey"
      ) +
      geom_label(x=length(orden_data_1_sd)*.75, size=6.5, 
                 y=prom_territoriales_sd*1.1, label=prom_territoriales_sd, 
                 fill=colores[1], color="ghostwhite"
      ) +
      geom_label(x=length(orden_data_1_sd)+0.75, size=6.5, 
                 y=prom_territoriales_cd*1.1, label=prom_territoriales_cd, 
                 fill="#e94f51", color="ghostwhite"
      ) +
      scale_fill_manual(values = c("#e94f51", "#a1c181")) +
      theme(legend.position = "none") +
      labs(x="Agencia de inicio", y="Promedio diario de carpetas") +
      tema_ppp
  } else {
    # data_1_sd <- data %>% 
    #   filter(quincena=="Actual", 
    #          detenido=="Sin detenido") %>% 
    #   group_by(ct_inicio_ap, #detenido, 
    #            
    #   ) %>% 
    #   summarise(Total=n()) 
    # 
    # data_1_cd <- data %>% 
    #   filter(quincena=="Actual", 
    #          detenido=="Con detenido") %>% 
    #   group_by(ct_inicio_ap, #detenido, 
    #            
    #   ) %>% 
    #   summarise(Total=n()) 
    
    prom_territoriales_sd <- iniciadas %>% 
      mutate(fecha_inicio=dmy(fecha_de_inicio)) %>% 
      filter(fecha_inicio>=fecha_comienzo, 
             fecha_inicio<=fecha_fin
      ) %>% 
      filter(!id_ap %in% data$id_ap ) %>% 
      mutate(detenido=case_when(
        id_ap %in% flagrancias$id_ap ~ "Con detenido", T ~ "Sin detenido" 
      )) %>% 
      filter(detenido=="Sin detenido") %>% 
      group_by(ct_inicio_ap) %>% 
      summarise(Total=n()) %>% 
      summarise(media=round(mean(Total), 2)) %>% pull(media)
    
    
    prom_territoriales_cd <- iniciadas %>% 
      mutate(fecha_inicio=dmy(fecha_de_inicio)) %>% 
      filter(fecha_inicio>=fecha_comienzo, 
             fecha_inicio<=fecha_fin
      ) %>% 
      filter(!id_ap %in% data$id_ap ) %>% 
      mutate(detenido=case_when(
        id_ap %in% flagrancias$id_ap ~ "Con detenido", T ~ "Sin detenido" 
      )) %>% 
      filter(detenido=="Con detenido") %>% 
      group_by(ct_inicio_ap) %>% 
      summarise(Total=n()) %>% 
      summarise(media=round(mean(Total), 2)) %>% pull(media)
    
    orden_data_1_sd <- data_1_sd %>% arrange(-media) %>% pull(ct_inicio_ap)
    orden_data_1_cd <- data_1_cd %>% arrange(-media) %>% pull(ct_inicio_ap)
    
    prom_fisc <- data %>% 
      filter(quincena=="Actual", ) %>% 
      group_by(ct_inicio_ap, detenido#, 
               #fecha_inicio
      ) %>% 
      summarise(Total=n()) %>% ungroup() %>% 
      summarise(media=mean(Total)) %>% pull(media)
    
    gr_1 <- #bind_rows(data_1_sd, data_1_cd) %>% 
      data %>% 
      filter(quincena=="Actual", ) %>% 
      group_by(ct_inicio_ap, detenido#, 
               #fecha_inicio
      ) %>% 
      summarise(Total=n()) %>% 
      mutate(comparacion=ifelse(Total>=prom_fisc, 1, 0)) %>% 
      mutate(
        ct_inicio_ap=ifelse(detenido=="Con detenido", 
                            paste0(ct_inicio_ap, "\nCD"), ct_inicio_ap
        ),
        ct_inicio_ap=factor(ct_inicio_ap, 
                            levels=c(orden_data_1_sd,orden_data_1_cd)
        )) %>% 
      ggplot(aes(ct_inicio_ap, Total, fill=factor(comparacion))) +
      geom_col() +
      geom_hline(aes(yintercept=prom_territoriales_sd), 
                 linetype="dashed", size=1, color=colores[1]
      ) + theme_light() +
      geom_hline(aes(yintercept=prom_territoriales_cd), 
                 linetype="dashed", size=1, color=colores[1]
      ) +
      geom_text(aes(label=Total), 
                position = position_stack(vjust = 1.1), size=5.5
      ) +
      tema_fgj +
      geom_vline(aes(xintercept=length(orden_data_1_sd)+0.5), 
                 linetype="dashed", size=1.5, color="grey"
      ) +
      geom_label(x=length(orden_data_1_sd)*.75, size=6.5, 
                 y=prom_territoriales_sd*1.1, label=prom_territoriales_sd, 
                 fill=colores[1], color="ghostwhite"
      ) +
      geom_label(x=length(orden_data_1_sd)+0.75, size=6.5, 
                 y=prom_territoriales_cd*1.1, label=prom_territoriales_cd, 
                 fill="#e94f51", color="ghostwhite"
      ) +
      scale_fill_manual(values = c("#e94f51", "#a1c181")) +
      theme(legend.position = "none") +
      labs(x="Agencia de inicio", y="Total de carpetas") +
      tema_ppp
  }
  
  
  
  return(gr_1)
  
  
  
}


gr_gam <- grafica_cargas_trabajo(datos = iniciadas, 
                                 fiscalia_analisis = "GAM", 
                                 fecha_comienzo = "2025-01-16", 
                                 fecha_fin = "2025-01-31", 
                                 tipo = "promedio"
                                 )

ggsave(plot = gr_gam, 
       "gam_120225.svg", width = 14, height = 6.5
       )


gr_izc <- grafica_cargas_trabajo(datos = iniciadas, 
                                 fiscalia_analisis = "IZC", 
                                 fecha_comienzo = "2025-01-16", 
                                 fecha_fin = "2025-01-31", 
                                 tipo = "promedio"
)


gr_cuh <- grafica_cargas_trabajo(datos = iniciadas, 
                                 fiscalia_analisis = "CUH", 
                                 fecha_comienzo = "2025-01-16", 
                                 fecha_fin = "2025-01-31", 
                                 tipo = "promedio"
)

gr_donde_determina <- function(
    fiscalia_analisis=siglas_fiscalia_analisis,
    fecha_comienzo=inicio_quincena, 
    fecha_fin=final_quincena
){
  flagr <- flagrancias %>% 
    filter(grepl(fiscalia_analisis, agencia_ini), 
           fecha_inicio>=fecha_comienzo, 
           fecha_inicio<=fecha_fin
           )
  
  flagr_terri <-  flagrancias %>%
    filter( fecha_inicio>=fecha_comienzo, 
            fecha_inicio<=fecha_fin) %>% 
    filter(res_puestas %in% c("1. Libertad MP", "2. Ilegal detención PJ", 
                                           "4. No vinculación", "5. Vinculación simple", 
                                           "7. Prisión preventiva"
  )) %>% 
    filter(!id_ap %in% flagr) %>% 
    mutate(res_puestas=case_when(
      res_puestas=="7. Prisión preventiva" ~ "5. Vinculación simple", 
      T ~ res_puestas
    )) %>% 
    mutate(res_puestas=substr(res_puestas, 4, nchar(res_puestas))) %>% 
    group_by(res_puestas) %>% 
    summarise(Total=n()) %>% 
    mutate(Porcentaje=Total/sum(Total)) %>% ungroup() %>% 
    mutate(res_puestas=factor(res_puestas, levels = c("Libertad MP", "Ilegal detención PJ", "No vinculación", 
                                                      "Vinculación simple"
    ))) %>% 
    mutate(agencia_ini="Territoriales")
  
  gr_det <- flagr %>% 
    filter(res_puestas %in% c("1. Libertad MP", "2. Ilegal detención PJ", 
                              "4. No vinculación", "5. Vinculación simple", 
                              "7. Prisión preventiva"
                              )) %>% 
    mutate(res_puestas=case_when(
      res_puestas=="7. Prisión preventiva" ~ "5. Vinculación simple", 
      T ~ res_puestas
    )) %>% 
    mutate(res_puestas=substr(res_puestas, 4, nchar(res_puestas))) %>% 
    group_by(agencia_ini, res_puestas) %>% 
    summarise(Total=n()) %>% ungroup() %>% 
    complete(agencia_ini=unique(flagr$agencia_ini), 
             res_puestas=unique(.$res_puestas), 
             fill = list(Total=0)
             ) %>% 
    group_by(agencia_ini) %>% 
    mutate(Porcentaje=Total/sum(Total)) %>% ungroup() %>% 
    mutate(res_puestas=factor(res_puestas, levels = c("Libertad MP", "Ilegal detención PJ", "No vinculación", 
                                         "Vinculación simple"
                                         ))) %>% 
    bind_rows(flagr_terri) %>% 
    ggplot(aes(agencia_ini, Porcentaje, fill=res_puestas, group=res_puestas)) +
    geom_col() + theme_light() + tema_fgj + tema_ppp +
    geom_text(aes(label=paste0(comma(Total), " (", percent(Porcentaje, .1), ")")), 
              color="black", size=6, 
              position = position_stack(vjust = 0.7)
              ) + 
    scale_y_continuous(labels = percent) +
    labs(x="Agencia de inicio", y="Total de carpetas", 
         fill=""
         ) +
    theme(legend.position = "bottom")+
    scale_fill_manual(values = colores[3:8])
  
  return(gr_det)
  
  
  
}


gr_determina_gam <- gr_donde_determina(
  fiscalia_analisis="GAM", 
  fecha_comienzo="2025-01-01", 
  fecha_fin="2025-01-15"
)


gr_determina_cuh <- gr_donde_determina(
  fiscalia_analisis="CUH", 
  fecha_comienzo="2025-01-01", 
  fecha_fin="2025-01-15"
)

ggsave(plot = gr_determina_gam, 
       "gr_determina_gam_120225.svg", width = 12, height = 6
       )



###tercer gráfica

tibble(tipo=c(
  "Artículo 140", "Artículo 140", "Perdón y falta de querella", "Perdón y falta de querella",
  "Criterio de oportunidad", 
), 
libertad_homologada=c("Artículo 140 CNPP", "Personas mayores de 65 años", "Falta de querella",
                      "Perdón de la víctima u ofendido", "Criterio de oportunidad", 
                      
                      )
)


#"No se formuló imputación", "Acuerdo reparatorio", "Pendientes por identificar"

library(wesanderson)

gr_libertad <- function(
    fiscalia_analisis=siglas_fiscalia_analisis,
    fecha_comienzo=inicio_quincena, 
    fecha_fin=final_quincena
) {
  flagr <- flagrancias %>% 
    filter(grepl(fiscalia_analisis, agencia_ini), 
           fecha_inicio>=fecha_comienzo, 
           fecha_inicio<=fecha_fin
    ) %>% 
    filter(res_puestas=="1. Libertad MP")
  
  flagr_terri <-  flagrancias %>%
    filter( fecha_inicio>=fecha_comienzo, 
            fecha_inicio<=fecha_fin) %>% 
    filter(res_puestas=="1. Libertad MP", 
           !libertad_homologada %in% c("No se formuló imputación", "Acuerdo reparatorio", 
                                       "Pendientes por identificar")
           ) %>% 
    filter(!id_ap %in% flagr) %>% 
    mutate(res_puestas=case_when(
      libertad_homologada %in% c("Artículo 140 CNPP", "Personas mayores de 65 años")~ "Artículo 140", 
      libertad_homologada %in% c("Falta de querella",
      "Perdón de la víctima u ofendido") ~"Perdón y falta de querella", 
      libertad_homologada %in% "Criterio de oportunidad" ~"Criterio de oportunidad",
      T ~ "Artículo 149"
    )) %>% 
    # mutate(res_puestas=substr(res_puestas, 4, nchar(res_puestas))) %>% 
    group_by(res_puestas) %>% 
    summarise(Total=n()) %>% 
    mutate(Porcentaje=Total/sum(Total)) %>% ungroup() %>% 
    mutate(res_puestas=factor(res_puestas, levels = c("Artículo 140", "Perdón y falta de querella", 
                                                      "Criterio de oportunidad", "Artículo 149"
    ))) %>% 
    mutate(agencia_ini="Territoriales")
  
  gr_det <- flagr %>% 
    filter(res_puestas=="1. Libertad MP", 
           !libertad_homologada %in% c("No se formuló imputación", "Acuerdo reparatorio", 
                                       "Pendientes por identificar")
    ) %>% 
    filter(!id_ap %in% flagr) %>% 
    mutate(res_puestas=case_when(
      libertad_homologada %in% c("Artículo 140 CNPP", "Personas mayores de 65 años")~ "Artículo 140", 
      libertad_homologada %in% c("Falta de querella",
                                 "Perdón de la víctima u ofendido") ~"Perdón y falta de querella", 
      libertad_homologada %in% "Criterio de oportunidad" ~"Criterio de oportunidad",
      T ~ "Artículo 149"
    )) %>% 
    # mutate(res_puestas=substr(res_puestas, 4, nchar(res_puestas))) %>% 
    group_by(agencia_ini, res_puestas) %>% 
    summarise(Total=n()) %>% 
    mutate(Porcentaje=Total/sum(Total)) %>% ungroup() %>% 
    complete(agencia_ini=unique(flagr$agencia_ini), 
             res_puestas=unique(.$res_puestas), 
             fill = list(Total=0, Porcentaje=0)
    ) %>% 
    mutate(res_puestas=factor(res_puestas, levels = c("Artículo 140", "Perdón y falta de querella", 
                                                      "Criterio de oportunidad", "Artículo 149"
    ))) %>% 
    bind_rows(flagr_terri) %>% 
    ggplot(aes(agencia_ini, Porcentaje, fill=res_puestas, group=res_puestas)) +
    geom_col() + theme_light() + tema_fgj + tema_ppp +
    geom_text(data=. %>% 
                filter(Total>0),
                aes(label=paste0(comma(Total), " (", percent(Porcentaje, .1), ")")), 
              color="black", size=6, 
              position = position_stack(vjust = 0.7)
    ) + 
    scale_y_continuous(labels = percent) +
    labs(x="Agencia de inicio", y="Total de carpetas", 
         fill=""
    ) +
    theme(legend.position = "bottom")+
    scale_fill_manual(values = c( "#85D4E3", "#F4B5BD", "#9C964A", "#CDC08C"))
  
  return(gr_det)
}

gr_libertad_gam <- gr_libertad(fiscalia_analisis = "GAM", 
                               fecha_comienzo = "2025-01-01", 
                               fecha_fin = "2025-01-15"
                               )

ggsave(plot = gr_libertad_gam, 
       "gr_libertad_gam_120225.svg", width = 12, height = 6
)

data <- iniciadas %>% 
  filter(grepl("GAM", ct_inicio_ap)) %>% 
  mutate(fecha_inicio=dmy(fecha_de_inicio)) %>% 
  filter(fecha_inicio>="2025-01-16", 
         fecha_inicio<="2025-01-31"
         ) %>% 
  mutate(quincena=case_when(
    fecha_inicio>="2025-01-01" ~ "Actual",
    T ~ "Anterior"
  ), 
  detenido=case_when(
    id_ap %in% flagrancias$id_ap ~ "Con detenido", 
    T ~ "Sin detenido"
  )
  ) 


data_1_sd <- data %>% 
  filter(quincena=="Actual", 
         detenido=="Sin detenido") %>% 
  group_by(ct_inicio_ap, #detenido, 
           fecha_inicio
           ) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date("2025-01-16"),
                                 as_date("2025-01-31"), 
                                 "1 day"
                                 ), 
           #detenido=c("Con detenido", "Sin detenido"), 
           ct_inicio_ap=unique(data$ct_inicio_ap), 
           fill=list(Total=0)
           
           ) %>% 
  group_by(ct_inicio_ap) %>% 
  summarise(media=round(mean(Total), 2)) %>% 
  mutate(detenido="Sin detenido")

data_1_cd <- data %>% 
  filter(quincena=="Actual", 
         detenido=="Con detenido") %>% 
  group_by(ct_inicio_ap, #detenido, 
           fecha_inicio
  ) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date("2025-01-15"),
                                 as_date("2025-01-31"), 
                                 "1 day"
  ), 
  #detenido=c("Con detenido", "Sin detenido"), 
  ct_inicio_ap=unique(.$ct_inicio_ap), 
  fill=list(Total=0)
  
  )  %>% 
  group_by(ct_inicio_ap) %>% 
  summarise(media=round(mean(Total), 2)) %>% 
  mutate(detenido="Con detenido", 
          ct_inicio_ap=paste0(ct_inicio_ap, "\nCD")
         )

prom_territoriales_sd <- iniciadas %>% 
  mutate(fecha_inicio=dmy(fecha_de_inicio)) %>% 
  filter(fecha_inicio>="2025-01-15", 
         fecha_inicio<="2025-01-31"
  ) %>% 
  filter(!id_ap %in% data$id_ap ) %>% 
  mutate(detenido=case_when(
    id_ap %in% flagrancias$id_ap ~ "Con detendido", T ~ "Sin detenido" 
  )) %>% 
  filter(detenido=="Sin detenido") %>% 
  group_by(fecha_inicio, ct_inicio_ap, detenido) %>% 
  summarise(Total=n()) %>% ungroup() %>%  
  complete(
    fecha_inicio=seq.Date(as_date("2025-01-16"),
                                 as_date("2025-01-31"),
                                 "1 day"
  ),
  ct_inicio_ap=unique(.$ct_inicio_ap), 
  fill=list(Total=0)
  
  )  %>% 
  summarise(media=round(mean(Total), 2)) %>% pull(media)

orden_data_1_sd <- data_1_sd %>% arrange(-media) %>% pull(ct_inicio_ap)
orden_data_1_cd <- data_1_cd %>% arrange(-media) %>% pull(ct_inicio_ap)

gr_1 <- #bind_rows(data_1_sd, data_1_cd) %>% 
  data %>% 
  filter(quincena=="Actual", ) %>% 
  group_by(ct_inicio_ap, detenido#, 
           #fecha_inicio
  ) %>% 
  summarise(Total=n()) %>% 
 mutate(
   ct_inicio_ap=ifelse(detenido=="Con detenido", 
                       paste0(ct_inicio_ap, "\nCD"), ct_inicio_ap
                       ),
   comparacion=ifelse(Total>=prom_territoriales, 1, 0), 
        ct_inicio_ap=factor(ct_inicio_ap, 
                            levels=c(orden_data_1_sd,orden_data_1_cd)
                            )) %>% 
  ggplot(aes(ct_inicio_ap, Total, fill=factor(comparacion))) +
  geom_col() +
  geom_hline(aes(yintercept=prom_territoriales), 
             linetype="dashed", size=1
             ) + theme_light() +
  geom_text(aes(label=Total), 
            position = position_stack(vjust = 1.1), size=5.5
            ) +
  tema_fgj +
  geom_vline(aes(xintercept=length(orden_data_1_sd)+0.5), 
             linetype="dashed", size=1.5, color="grey"
             ) +
  geom_label(x=length(orden_data_1_sd)*.75, size=6.5, 
            y=prom_territoriales*1.1, label=prom_territoriales, 
            fill=colores[1], color="ghostwhite"
  ) +
  geom_vline(aes(xintercept=length(orden_data_1_sd)+0.5), 
             linetype="dashed", size=1.5, color="grey"
  ) +
  geom_label(x=length(orden_data_1_sd)*.75, size=6.5, 
             y=prom_territoriales*1.1, label=prom_territoriales, 
             fill=colores[1], color="ghostwhite"
  ) +
  scale_fill_manual(values = c("#e94f51", "#a1c181")) +
  theme(legend.position = "none") +
  labs(x="Agencia de inicio", y="Total de carpetas")

ggsave(plot = gr_1, 
       "gr_1_absolutos.svg", width = 14, height = 7 
       )

##ct_inicio
data %>% 
  filter(quincena=="Anterior") %>% 
  group_by(ct_inicio_ap, detenido#, 
           #fecha_inicio
  ) %>% 
  summarise(Total=n())


#### graficas top
  
top_5_max <- iniciadas %>% 
  mutate(fecha_inicio=dmy(fecha_de_inicio)) %>% 
  filter(fecha_inicio>="2025-01-01", 
         fecha_inicio<="2025-01-15"
  ) %>% 
  # filter(!id_ap %in% data$id_ap ) %>% 
  group_by(ct_inicio_ap) %>% 
  summarise(Total=n()) %>% top_n(5) %>% 
  ggplot(aes(reorder(ct_inicio_ap, -Total), Total)) +
  geom_col(fill=colores[11]) +
 theme_light() +
  geom_label(aes(label=Total), 
            position = position_stack(vjust = .8), size=5.5
  ) +
  tema_fgj +
  labs(x="Agencia de inicio", y="Total de carpetas", 
       title = "Top 5 de agencias con más")
  
ggsave(plot = top_5_max, 
       "top_5_absolutos.svg", width = 5, height = 3
)

top_5_min <- iniciadas %>% 
  mutate(fecha_inicio=dmy(fecha_de_inicio)) %>% 
  filter(fecha_inicio>="2025-01-01", 
         fecha_inicio<="2025-01-15"
  ) %>% 
  # filter(!id_ap %in% data$id_ap ) %>% 
  group_by(ct_inicio_ap) %>% 
  summarise(Total=n()) %>% top_n(-5) %>% 
  ggplot(aes(reorder(ct_inicio_ap, -Total), Total)) +
  geom_col(fill=colores[13]) +
  theme_light() +
  geom_label(aes(label=Total), 
             position = position_stack(vjust = .8), size=5.5
  ) +
  tema_fgj +
  labs(x="Agencia de inicio", y="Total de carpetas", 
       title = "5 con agencias menos")  
  
ggsave(plot = top_5_min, 
       "top_5_min_absolutos.svg", width = 5, height = 3
)  


personal_fisc <- personal %>% 
  filter(grepl("IZP", agencia)) %>% 
  summarise(Total=sum(personal_ministerial)) %>% pull(Total)


data_prom <- bind_rows(data_1_sd, data_1_cd) %>% 
  left_join(personal %>% 
              select("ct_inicio_ap"=agencia, 
                     personal_ministerial
              ), by="ct_inicio_ap"
  ) %>% 
  mutate(ct_inicio_ap=ifelse(detenido=="Con detenido", 
                             paste0(ct_inicio_ap, "\nCD"), ct_inicio_ap
  ),
  media=round(media/personal_ministerial, 3),
  comparacion=ifelse(media>=prom_territoriales/(1372-personal_fisc), 1, 0), 
  # ct_inicio_ap=factor(ct_inicio_ap, 
  #                     levels=c(orden_data_1_sd,orden_data_1_cd)
  # )
  ) 


orden_data_1_sd_prom <- data_prom %>%
  filter(detenido=="Sin detenido") %>% 
  arrange(-media) %>% pull(ct_inicio_ap)

orden_data_1_cd_prom <- data_1_cd %>%
  filter(detenido=="Con detenido") %>% 
  arrange(-media) %>%
  mutate(ct_inicio_ap=paste0(ct_inicio_ap, "\nCD")) %>% 
  pull(ct_inicio_ap)

gr_1_prom <- bind_rows(data_1_sd, data_1_cd) %>% 
  left_join(personal %>% 
              select("ct_inicio_ap"=agencia, 
                     personal_ministerial
                     ), by="ct_inicio_ap"
              ) %>% 
  mutate(ct_inicio_ap=ifelse(detenido=="Con detenido", 
                                paste0(ct_inicio_ap, "\nCD"), ct_inicio_ap
  ),
    comparacion=ifelse(media>=prom_territoriales/(1372-personal_fisc), 1, 0), 
    ct_inicio_ap=factor(ct_inicio_ap, 
                        levels=c(orden_data_1_sd_prom,orden_data_1_cd_prom)
    )) %>% 
  mutate(media=round(media/personal_ministerial, 3)) %>% 
  ggplot(aes(ct_inicio_ap, media, fill=factor(comparacion))) +
  geom_col() +
  geom_hline(aes(yintercept=prom_territoriales/(1372-personal_fisc)), 
             linetype="dashed", size=1
  ) + theme_light() +
  geom_text(aes(label=media), 
            position = position_stack(vjust = 1.1), size=5.5
  ) +
  tema_fgj +
  geom_vline(aes(xintercept=10.5), 
             linetype="dashed", size=1.5, color="grey"
  ) +
  geom_label(x=1, size=6.5, 
             y=prom_territoriales/(1372-personal_fisc)*1.1, 
             label=round(prom_territoriales/(1372-personal_fisc), 3), 
             fill=colores[1], color="ghostwhite"
  ) +
  scale_fill_manual(values = c("#e94f51", "#a1c181")) +
  theme(legend.position = "none") +
  labs(x="Agencia de inicio", y="Promedio diario por MP")

ggsave(plot = gr_1_prom, 
       "gr_1_prom.svg", width = 14, height = 7 
)
  
  
  
  

