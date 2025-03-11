

setwd("I:/Mi unidad/18. Evaluacion_fiscalias_especiales/corte_022825")
library(readxl)

# iniciadas <- read_excel("H:/Mi unidad/peticiones_jaime/Iniciadas por MP - Extraida el 24022025/iniciadas por  MP - listado 24022025.xlsx") %>% 
#   clean_names()
iniciadas <- readRDS("I:/Mi unidad/0. Bases/data_mp_280225.rds") %>% 
  clean_names()

cat_fiscalias <- read_excel("H:/Mi unidad/bases/cat_fiscalias_agencias.xlsx") %>% 
  clean_names()

juntas <- iniciadas %>% 
  left_join(cat_fiscalias %>% 
              select(siglas_fiscalia, siglas_buena, area)
              )


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

juntas <- juntas %>% 
  mutate(fiscalia=gsub("[0-9]|-|UAT|URI|ADD", "", ct_inicio), 
         fiscalia = case_when(
           fiscalia=="CJ" ~ "CUJ", 
           ct_inicio=="H3" ~ "BJ", 
           ct_inicio %in% c("H1", "H2") ~ "MH", 
           T ~ fiscalia
         ), 
         grupo=case_when(
           fiscalia %in% c("IZP", "CUH", "GAM") ~ "Alta", 
           fiscalia %in% c("BJ", "COY", "MH", "TLP", "VC", 
                           "AO") ~"Media", 
           T ~ "Baja"
         )
  )

juntas <- juntas %>% 
  rename(id_ap=id_ci, 
         fecha_de_inicio=fecha_inicio, 
         fiscal=mp
  )


juntas <- juntas %>% 
  mutate(ct_inicio_ap=gsub("UAT-|URI-|ADD-", "", ct_inicio))


grafica_cargas_trabajo2 <- function(
    datos=iniciadas, 
    fiscalia_analisis=siglas_fiscalia_analisis,
    fecha_comienzo=inicio_quincena, 
    fecha_fin=final_quincena, 
    tipo = c("total", "promedio", "fiscalia_sin", "fiscalia_con", "agencia")
){
  data <- datos %>% 
    # filter(grepl(fiscalia_analisis, ct_inicio_ap)) %>% 
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
    ) %>% drop_na(fiscal)
  
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
  }
  else if(tipo=="total") {
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
  else if (tipo=="fiscalia_sin") {
    
    periodo <-  as.integer(as_date(fecha_fin)-as_date(fecha_comienzo))+1
    
    data_1_sd_mp <- data %>%
      filter(quincena=="Actual",
             detenido=="Sin detenido") %>%
      group_by(fiscalia=siglas_buena, #detenido,
               fecha_inicio,
               fiscal
      ) %>%
      summarise(Total=n()) %>% 
      group_by(fiscalia, fiscal) %>% 
      summarise(Total=mean(Total))
    
    data_1_sd <- data %>%
      filter(quincena=="Actual",
             detenido=="Sin detenido") %>%
      group_by(fiscalia=siglas_buena, #detenido,
               fecha_inicio,
               fiscal
      ) %>%
      summarise(Total=n()) %>% ungroup() %>%
      group_by(fiscalia, fecha_inicio) %>%
      summarise(fiscales=n(),
                Total=sum(Total, na.rm = T)
      ) %>%
      mutate(media=Total/fiscales)
    
    data_1_cd_mp <- data %>%
      filter(quincena=="Actual",
             detenido=="Con detenido") %>%
      group_by(fiscalia=siglas_buena, #detenido,
               fecha_inicio,
               fiscal
      ) %>%
      summarise(Total=n()) %>% 
      group_by(fiscalia, fiscal) %>% 
      summarise(Total=mean(Total))
    
    
    
    data_1_cd <- data %>%
      filter(quincena=="Actual",
             detenido=="Con detenido") %>%
      group_by(fiscalia=siglas_buena, #detenido,
               fecha_inicio,
               fiscal
      ) %>%
      summarise(Total=n()) %>% ungroup() %>%
      group_by(fiscalia, fecha_inicio) %>%
      summarise(fiscales=n(),
                Total=sum(Total, na.rm = T)
      ) %>%
      mutate(media=Total/fiscales)
    
    # prom_territoriales_sd <- iniciadas %>%
    #   mutate(fecha_inicio=dmy(fecha_de_inicio)) %>%
    #   filter(fecha_inicio>=fecha_comienzo,
    #          fecha_inicio<=fecha_fin
    #   ) %>%
    #   filter(!id_ap %in% data$id_ap ) %>%
    #   mutate(detenido=case_when(
    #     id_ap %in% flagrancias$id_ap ~ "Con detenido", T ~ "Sin detenido"
    #   )) %>%
    #   filter(detenido=="Sin detenido") %>%
    #   group_by(fiscalia, fiscal) %>%
    #   summarise(Total=n()/periodo)
    
    prom_territoriales_sd <- data_1_sd_mp %>% ungroup() %>%
      # group_by(fiscalia) %>%
      summarise(Total=mean(Total))
    
    prom_territoriales_cd <- data_1_cd_mp %>% ungroup() %>%
      # group_by(fiscalia) %>%
      summarise(Total=mean(Total))
    
    orden_data_1_sd <- data_1_sd %>%
      group_by(fiscalia) %>% summarise(media=sum(Total)) %>%
      arrange(-media) %>% pull(fiscalia)
    orden_data_1_cd <- data_1_cd %>%
      group_by(fiscalia) %>% summarise(media=sum(Total)) %>%
      arrange(-media) %>% pull(fiscalia)
    
    # prom_fisc <- bind_rows(data_1_sd, data_1_cd) %>% ungroup() %>%
    #   summarise(media=round(mean(Total), 2)) %>% pull(media)
    
    data_agrupada <- bind_rows(data_1_sd %>%
                                 mutate(detenido="Sin detenido")
                               , data_1_cd%>%
                                 mutate(detenido="Con detenido")) %>%
      # rename(Total=media) %>%
      group_by(fiscalia, detenido) %>%
      summarise(Total=mean(media))
    
    # total_mp <- bind_rows(data_1_sd %>%
    #                         mutate(detenido="Sin detenido")
    #                       , data_1_cd%>%
    #                         mutate(detenido="Con detenido")) %>%
    #   drop_na(fiscal) %>%
    #
    #   # rename(Total=media) %>%
    #   group_by(fiscalia, detenido, fiscal) %>%
    #   summarise(Total=n()) %>% ungroup() %>%
    #   mutate(id_fiscal=paste0(fiscal, "-", detenido)) %>%
    #   filter(!duplicated(id_fiscal)) %>%
    #   group_by(fiscalia, detenido) %>%
    #   summarise(agentes=sum(Total))
    
    
    gr_sd <- data_1_sd_mp %>% 
      # data_agrupada %>%
      # filter(detenido=="Sin detenido") %>%
      mutate(fiscalia=factor(fiscalia, 
                             levels=orden_data_1_sd)) %>% 
      # left_join(total_mp %>%
      #             filter(detenido=="Sin detenido")
      #           , by="fiscalia") %>%
      # mutate(label=paste0(fiscalia, "\n", agentes)) %>%
      ggplot(aes(fiscalia, Total)) +
      # geom_col(fill=rgb(46,92,129, maxColorValue = 255), alpha=.9) +
      geom_col(
        data=data_1_sd_mp %>%
          group_by(fiscalia) %>% 
          summarise(Total=mean(Total)) %>% 
          mutate(fiscalia=factor(fiscalia, 
                                 levels=orden_data_1_sd)),
        aes(x=fiscalia, y=Total), 
        fill=rgb(46,92,129, maxColorValue = 255), alpha=.9
      ) +
      geom_jitter(
        data=data_1_sd_mp %>%
          mutate(fiscalia=factor(fiscalia, 
                                 levels=orden_data_1_sd)),
        aes(x=fiscalia, y=Total), size=2.5,
        color=rgb(248,175,103, maxColorValue = 255), fill=rgb(248,175,103, maxColorValue = 255), width = .1,
        shape=18
      ) +
      
      geom_hline(
        aes(yintercept=prom_territoriales_sd$Total),
        linetype="dashed", size=1, color="#B09D83") +
      theme_light() +
      geom_text(data=data_1_sd_mp %>%
                  group_by(fiscalia) %>% 
                  summarise(Total=mean(Total)) %>% 
                  mutate(fiscalia=factor(fiscalia, 
                                         levels=orden_data_1_sd)),
                aes(label=round(Total, 2)), fontface="bold",
                position = position_stack(vjust = 1.1), size=6
      ) +
      scale_y_continuous(breaks = seq(0,14, 2), labels=seq(0,14, 2)) +
      tema_fgj +
      geom_label(x=length(unique(data_1_sd$fiscalia)), size=6.5,
                 y=prom_territoriales_sd$Total*1.1, label=round(prom_territoriales_sd$Total,2),
                 fill="#B09D83", color="ghostwhite"
      ) +
      scale_fill_manual(values = colores[7:10]) +
      
      labs(x="", y="", title = "Sin detenido") +
      tema_ppp +
      theme(legend.position = "none", 
            axis.text.x = element_text(size=12)) 
    
    # gr_cd <- data_1_cd_mp %>% 
    #   # data_agrupada %>%
    #   # filter(detenido=="Sin detenido") %>%
    #   mutate(fiscalia=factor(fiscalia, 
    #                          levels=orden_data_1_cd)) %>% 
    #   # left_join(total_mp %>%
    #   #             filter(detenido=="Sin detenido")
    #   #           , by="fiscalia") %>%
    #   # mutate(label=paste0(fiscalia, "\n", agentes)) %>%
    #   ggplot(aes(fiscalia, Total)) +
    #   # geom_col(fill=rgb(46,92,129, maxColorValue = 255), alpha=.9) +
    #   geom_col(
    #     data=data_1_cd_mp %>%
    #       group_by(fiscalia) %>% 
    #       summarise(Total=mean(Total)) %>% 
    #       mutate(fiscalia=factor(fiscalia, 
    #                              levels=orden_data_1_cd)),
    #     aes(x=fiscalia, y=Total), 
    #     fill=rgb(46,92,129, maxColorValue = 255), alpha=.9
    #   ) +
    #   geom_jitter(
    #     data=data_1_cd_mp %>%
    #       mutate(fiscalia=factor(fiscalia, 
    #                              levels=orden_data_1_cd)),
    #     aes(x=fiscalia, y=Total), size=2.5,
    #     color=rgb(248,175,103, maxColorValue = 255), fill=rgb(248,175,103, maxColorValue = 255), width = .1,
    #     shape=18
    #   ) +
    #   
    #   geom_hline(
    #     aes(yintercept=prom_territoriales_cd$Total),
    #     linetype="dashed", size=1, color="#B09D83") +
    #   theme_light() +
    #   geom_text(data=data_1_cd_mp %>%
    #               group_by(fiscalia) %>% 
    #               summarise(Total=mean(Total)) %>% 
    #               mutate(fiscalia=factor(fiscalia, 
    #                                      levels=orden_data_1_cd)),
    #             aes(label=round(Total, 2)), fontface="bold",
    #             position = position_stack(vjust = 1.1), size=6
    #   ) +
    #   scale_y_continuous(breaks = seq(0,14, 2), labels=seq(0,14, 2)) +
    #   tema_fgj +
    #   geom_label(x=length(unique(data_1_sd$fiscalia)), size=6.5,
    #              y=prom_territoriales_cd$Total*1.1, label=round(prom_territoriales_cd$Total,2),
    #              fill="#B09D83", color="ghostwhite"
    #   ) +
    #   scale_fill_manual(values = colores[7:10]) +
    #   theme(legend.position = "none") +
    #   labs(x="", y="", title = "Con detenido") +
    #   tema_ppp
    
    gr_1 <- gr_sd
  }
  else if (tipo=="fiscalia_con") {
    
    periodo <-  as.integer(as_date(fecha_fin)-as_date(fecha_comienzo))+1
    
    data_1_sd_mp <- data %>%
      filter(quincena=="Actual",
             detenido=="Sin detenido") %>%
      group_by(fiscalia=siglas_buena, #detenido,
               fecha_inicio,
               fiscal
      ) %>%
      summarise(Total=n()) %>% 
      group_by(fiscalia, fiscal) %>% 
      summarise(Total=mean(Total))
    
    data_1_sd <- data %>%
      filter(quincena=="Actual",
             detenido=="Sin detenido") %>%
      group_by(fiscalia=siglas_buena, #detenido,
               fecha_inicio,
               fiscal
      ) %>%
      summarise(Total=n()) %>% ungroup() %>%
      group_by(fiscalia, fecha_inicio) %>%
      summarise(fiscales=n(),
                Total=sum(Total, na.rm = T)
      ) %>%
      mutate(media=Total/fiscales)
    
    data_1_cd_mp <- data %>%
      filter(quincena=="Actual",
             detenido=="Con detenido") %>%
      group_by(fiscalia=siglas_buena, #detenido,
               fecha_inicio,
               fiscal
      ) %>%
      summarise(Total=n()) %>% 
      group_by(fiscalia, fiscal) %>% 
      summarise(Total=mean(Total))
    
    
    
    data_1_cd <- data %>%
      filter(quincena=="Actual",
             detenido=="Con detenido") %>%
      group_by(fiscalia=siglas_buena, #detenido,
               fecha_inicio,
               fiscal
      ) %>%
      summarise(Total=n()) %>% ungroup() %>%
      group_by(fiscalia, fecha_inicio) %>%
      summarise(fiscales=n(),
                Total=sum(Total, na.rm = T)
      ) %>%
      mutate(media=Total/fiscales)
    
    # prom_territoriales_sd <- iniciadas %>%
    #   mutate(fecha_inicio=dmy(fecha_de_inicio)) %>%
    #   filter(fecha_inicio>=fecha_comienzo,
    #          fecha_inicio<=fecha_fin
    #   ) %>%
    #   filter(!id_ap %in% data$id_ap ) %>%
    #   mutate(detenido=case_when(
    #     id_ap %in% flagrancias$id_ap ~ "Con detenido", T ~ "Sin detenido"
    #   )) %>%
    #   filter(detenido=="Sin detenido") %>%
    #   group_by(fiscalia, fiscal) %>%
    #   summarise(Total=n()/periodo)
    
    prom_territoriales_sd <- data_1_sd_mp %>% ungroup() %>%
      # group_by(fiscalia) %>%
      summarise(Total=mean(Total))
    
    prom_territoriales_cd <- data_1_cd_mp %>% ungroup() %>%
      # group_by(fiscalia) %>%
      summarise(Total=mean(Total))
    
    orden_data_1_sd <- data_1_sd %>%
      group_by(fiscalia) %>% summarise(media=sum(Total)) %>%
      arrange(-media) %>% pull(fiscalia)
    orden_data_1_cd <- data_1_cd %>%
      group_by(fiscalia) %>% summarise(media=sum(Total)) %>%
      arrange(-media) %>% pull(fiscalia)
    
    # prom_fisc <- bind_rows(data_1_sd, data_1_cd) %>% ungroup() %>%
    #   summarise(media=round(mean(Total), 2)) %>% pull(media)
    
    data_agrupada <- bind_rows(data_1_sd %>%
                                 mutate(detenido="Sin detenido")
                               , data_1_cd%>%
                                 mutate(detenido="Con detenido")) %>%
      # rename(Total=media) %>%
      group_by(fiscalia, detenido) %>%
      summarise(Total=mean(media))
    
    # total_mp <- bind_rows(data_1_sd %>%
    #                         mutate(detenido="Sin detenido")
    #                       , data_1_cd%>%
    #                         mutate(detenido="Con detenido")) %>%
    #   drop_na(fiscal) %>%
    #
    #   # rename(Total=media) %>%
    #   group_by(fiscalia, detenido, fiscal) %>%
    #   summarise(Total=n()) %>% ungroup() %>%
    #   mutate(id_fiscal=paste0(fiscal, "-", detenido)) %>%
    #   filter(!duplicated(id_fiscal)) %>%
    #   group_by(fiscalia, detenido) %>%
    #   summarise(agentes=sum(Total))
    
    
    # gr_sd <- data_1_sd_mp %>% 
    #   # data_agrupada %>%
    #   # filter(detenido=="Sin detenido") %>%
    #   mutate(fiscalia=factor(fiscalia, 
    #                          levels=orden_data_1_sd)) %>% 
    #   # left_join(total_mp %>%
    #   #             filter(detenido=="Sin detenido")
    #   #           , by="fiscalia") %>%
    #   # mutate(label=paste0(fiscalia, "\n", agentes)) %>%
    #   ggplot(aes(fiscalia, Total)) +
    #   # geom_col(fill=rgb(46,92,129, maxColorValue = 255), alpha=.9) +
    #   geom_col(
    #     data=data_1_sd_mp %>%
    #       group_by(fiscalia) %>% 
    #       summarise(Total=mean(Total)) %>% 
    #       mutate(fiscalia=factor(fiscalia, 
    #                              levels=orden_data_1_sd)),
    #     aes(x=fiscalia, y=Total), 
    #     fill=rgb(46,92,129, maxColorValue = 255), alpha=.9
    #   ) +
    #   geom_jitter(
    #     data=data_1_sd_mp %>%
    #       mutate(fiscalia=factor(fiscalia, 
    #                              levels=orden_data_1_sd)),
    #     aes(x=fiscalia, y=Total), size=2.5,
    #     color=rgb(248,175,103, maxColorValue = 255), fill=rgb(248,175,103, maxColorValue = 255), width = .1,
    #     shape=18
    #   ) +
    #   
    #   geom_hline(
    #     aes(yintercept=prom_territoriales_sd$Total),
    #     linetype="dashed", size=1, color="#B09D83") +
    #   theme_light() +
    #   geom_text(data=data_1_sd_mp %>%
    #               group_by(fiscalia) %>% 
    #               summarise(Total=mean(Total)) %>% 
    #               mutate(fiscalia=factor(fiscalia, 
    #                                      levels=orden_data_1_sd)),
    #             aes(label=round(Total, 2)), fontface="bold",
    #             position = position_stack(vjust = 1.1), size=6
    #   ) +
    #   scale_y_continuous(breaks = seq(0,14, 2), labels=seq(0,14, 2)) +
    #   tema_fgj +
    #   geom_label(x=length(unique(data_1_sd$fiscalia)), size=6.5,
    #              y=prom_territoriales_sd$Total*1.1, label=round(prom_territoriales_sd$Total,2),
    #              fill="#B09D83", color="ghostwhite"
    #   ) +
    #   scale_fill_manual(values = colores[7:10]) +
    #   theme(legend.position = "none") +
    #   labs(x="", y="", title = "Sin detenido") +
    #   tema_ppp
    
    gr_cd <- data_1_cd_mp %>% 
      # data_agrupada %>%
      # filter(detenido=="Sin detenido") %>%
      mutate(fiscalia=factor(fiscalia, 
                             levels=orden_data_1_cd)) %>% 
      # left_join(total_mp %>%
      #             filter(detenido=="Sin detenido")
      #           , by="fiscalia") %>%
      # mutate(label=paste0(fiscalia, "\n", agentes)) %>%
      ggplot(aes(fiscalia, Total)) +
      # geom_col(fill=rgb(46,92,129, maxColorValue = 255), alpha=.9) +
      geom_col(
        data=data_1_cd_mp %>%
          group_by(fiscalia) %>% 
          summarise(Total=mean(Total)) %>% 
          mutate(fiscalia=factor(fiscalia, 
                                 levels=orden_data_1_cd)),
        aes(x=fiscalia, y=Total), 
        fill=rgb(46,92,129, maxColorValue = 255), alpha=.9
      ) +
      geom_jitter(
        # data=data_1_cd_mp %>%
        #   mutate(fiscalia=factor(fiscalia, 
        #                          levels=orden_data_1_cd)),
        aes(x=fiscalia, y=Total), size=2.5,
        color=rgb(248,175,103, maxColorValue = 255), fill=rgb(248,175,103, maxColorValue = 255), width = .1,
        shape=18
      ) +
      
      geom_hline(
        aes(yintercept=prom_territoriales_cd$Total),
        linetype="dashed", size=1, color="#B09D83") +
      theme_light() +
      geom_text(data=data_1_cd_mp %>%
                  group_by(fiscalia) %>% 
                  summarise(Total=mean(Total)) %>% 
                  mutate(fiscalia=factor(fiscalia, 
                                         levels=orden_data_1_cd)),
                aes(label=comma(Total, .1)), fontface="bold",
                position = position_stack(vjust = 1.1), size=6
      ) +
      # scale_y_continuous(breaks = seq(0,14, 2), labels=seq(0,14, 2)) +
      tema_fgj +
      geom_label(x=length(unique(data_1_sd$fiscalia)), size=6.5,
                 y=prom_territoriales_cd$Total*1.1, label=round(prom_territoriales_cd$Total,2),
                 fill="#B09D83", color="ghostwhite"
      ) +
      scale_fill_manual(values = colores[7:10]) +
      # theme(legend.position = "none") +
      labs(x="", y="", title = "Con detenido") +
      tema_ppp +
      theme(legend.position = "none", 
            axis.text.x = element_text(size=12)) 
    
    gr_1 <- gr_cd
  }
  else if (tipo=="agencia"){
    periodo <-  as.integer(as_date(fecha_fin)-as_date(fecha_comienzo))+1
    
    
    data_1_cd_mp <- data %>%
      filter(quincena=="Actual",
             detenido=="Con detenido") %>%
      group_by(ct_inicio_ap, #detenido,
               fecha_inicio,
               fiscal
      ) %>%
      summarise(Total=n()) %>% 
      group_by(ct_inicio_ap, fiscal) %>% 
      summarise(Total=mean(Total))
    
    
    
    data_1_cd <- data %>%
      filter(quincena=="Actual",
             detenido=="Con detenido") %>%
      group_by(ct_inicio_ap, #detenido,
               fecha_inicio,
               fiscal
      ) %>%
      summarise(Total=n()) %>% ungroup() %>%
      group_by(ct_inicio_ap, fecha_inicio) %>%
      summarise(fiscales=n(),
                Total=sum(Total, na.rm = T)
      ) %>%
      mutate(media=Total/fiscales)
    
    # prom_territoriales_sd <- iniciadas %>%
    #   mutate(fecha_inicio=dmy(fecha_de_inicio)) %>%
    #   filter(fecha_inicio>=fecha_comienzo,
    #          fecha_inicio<=fecha_fin
    #   ) %>%
    #   filter(!id_ap %in% data$id_ap ) %>%
    #   mutate(detenido=case_when(
    #     id_ap %in% flagrancias$id_ap ~ "Con detenido", T ~ "Sin detenido"
    #   )) %>%
    #   filter(detenido=="Sin detenido") %>%
    #   group_by(fiscalia, fiscal) %>%
    #   summarise(Total=n()/periodo)
    
    # prom_territoriales_sd <- data_1_sd_mp %>% ungroup() %>%
    #   # group_by(fiscalia) %>%
    #   summarise(Total=mean(Total))
    
    prom_territoriales_cd <- data_1_cd_mp %>% ungroup() %>%
      filter(!ct_inicio_ap %in% c("IZP-9", "IZP-4", "COY-3", "IZP-8")) %>% 
      # group_by(fiscalia) %>%
      summarise(Total=mean(Total))
    
    # orden_data_1_sd <- data_1_sd %>%
    #   group_by(ct_inicio_ap) %>% summarise(media=sum(Total)) %>%
    #   arrange(-media) %>% pull(ct_inicio_ap)
    orden_data_1_cd <- data_1_cd %>%
      # filter(!ct_inicio_ap %in% c("IZP-9", "IZP-4", "COY-3", "IZP-8")) %>% 
      group_by(ct_inicio_ap) %>% summarise(media=sum(Total)) %>%
      arrange(-media) %>% pull(ct_inicio_ap)
    
    # prom_fisc <- bind_rows(data_1_sd, data_1_cd) %>% ungroup() %>%
    #   summarise(media=round(mean(Total), 2)) %>% pull(media)
    
    # data_agrupada <- bind_rows(data_1_sd %>%
    #                              mutate(detenido="Sin detenido")
    #                            , data_1_cd%>%
    #                              mutate(detenido="Con detenido")) %>%
    #   # rename(Total=media) %>%
    #   group_by(fiscalia, detenido) %>%
    #   summarise(Total=mean(media))
    
    
    # gr_sd <- data_agrupada %>%
    #   filter(detenido=="Sin detenido") %>%
    #   mutate(ct_inicio_ap=factor(ct_inicio_ap, 
    #                              levels=orden_data_1_sd
    #                              )) %>% 
    #   # ggplot(aes(reorder(ct_inicio_ap, -Total), Total)) +
    #   ggplot(aes(ct_inicio_ap, Total)) +
    #   geom_col(fill=rgb(46,92,129, maxColorValue = 255), alpha=.9) +
    #   geom_hline(
    #     aes(yintercept=prom_territoriales_sd$Total),
    #     linetype="dashed", size=1, color="#B09D83") +
    #   theme_light() +
    #   geom_text(aes(label=round(Total, 2)), fontface="bold",
    #             position = position_stack(vjust = 1.1), size=6
    #   ) +
    #   tema_fgj +
    #   geom_label(x=length(unique(data_agrupada$fiscalia)), size=6.5,
    #              y=prom_territoriales_sd$Total*1.1, label=round(prom_territoriales_sd$Total,2),
    #              fill="#B09D83", color="ghostwhite"
    #   ) +
    #   scale_fill_manual(values = colores[7:10]) +
    #   theme(legend.position = "none") +
    #   labs(x="", y="", title = "Sin detenido") +
    #   tema_ppp
    
    
    gr_cd <- data_1_cd_mp %>% 
      # data_agrupada %>%
      # filter(detenido=="Sin detenido") %>%
      mutate(ct_inicio_ap=factor(ct_inicio_ap, 
                                 levels=orden_data_1_cd)) %>% 
      # left_join(total_mp %>%
      #             filter(detenido=="Sin detenido")
      #           , by="fiscalia") %>%
      # mutate(label=paste0(fiscalia, "\n", agentes)) %>%
      ggplot(aes(ct_inicio_ap, Total)) +
      # geom_col(fill=rgb(46,92,129, maxColorValue = 255), alpha=.9) +
      geom_col(
        data=data_1_cd_mp %>%
          group_by(ct_inicio_ap) %>% 
          summarise(Total=mean(Total)) %>% 
          mutate(ct_inicio_ap=factor(ct_inicio_ap, 
                                     levels=orden_data_1_cd)),
        aes(x=ct_inicio_ap, y=Total), 
        fill=rgb(46,92,129, maxColorValue = 255), alpha=.9
      ) +
      geom_jitter(
        data=data_1_cd_mp %>%
          mutate(ct_inicio_ap=factor(ct_inicio_ap, 
                                     levels=orden_data_1_cd)),
        aes(x=ct_inicio_ap, y=Total), size=2.5,
        color=rgb(248,175,103, maxColorValue = 255), fill=rgb(248,175,103, maxColorValue = 255), width = .1,
        shape=18
      ) +
      
      geom_hline(
        aes(yintercept=prom_territoriales_cd$Total),
        linetype="dashed", size=1, color="#B09D83") +
      theme_light() +
      geom_text(data=data_1_cd_mp %>%
                  group_by(ct_inicio_ap) %>% 
                  summarise(Total=mean(Total)) %>% 
                  mutate(ct_inicio_ap=factor(ct_inicio_ap, 
                                             levels=orden_data_1_cd)),
                aes(label=round(Total, 2)), fontface="bold",
                position = position_stack(vjust = 1.1), size=6
      ) +
      scale_y_continuous(breaks = seq(0,14, 2), labels=seq(0,14, 2)) +
      tema_fgj +
      geom_label(x=length(unique(data_1_cd$ct_inicio_ap)), size=6.5,
                 y=prom_territoriales_cd$Total*1.1, label=round(prom_territoriales_cd$Total,2),
                 fill="#B09D83", color="ghostwhite"
      ) +
      scale_fill_manual(values = colores[7:10]) +
      
      labs(x="", y="", title = "Con detenido") +
      tema_ppp +
      theme(legend.position = "none", 
            axis.text.x = element_text(angle = 90)
      ) 
    
    # gr_1 <- cowplot::plot_grid(gr_sd, gr_cd, nrow = 2)
    gr_1 <- gr_cd
  }
  
  
  
  return(gr_1)
  
  
  
}

gr_fisc_esp_sd <- grafica_cargas_trabajo2(
  datos = juntas %>% 
    filter(area!="Coordinación General de Investigación Territorial"), 
  #fiscalia_analisis = "GAM", 
  fecha_comienzo = "2024-10-01", 
  fecha_fin = "2025-01-15", 
  tipo = "fiscalia_sin"
)

ggsave(plot = gr_fisc_esp_sd, 
       "gr_fisc_esp_sd.svg", width = 14, height = 7
       )



gr_fisc_esp_cd <- grafica_cargas_trabajo2(
  datos = juntas %>% 
    filter(area!="Coordinación General de Investigación Territorial"), 
  #fiscalia_analisis = "GAM", 
  fecha_comienzo = "2024-10-01", 
  fecha_fin = "2025-01-15", 
  tipo = "fiscalia_con"
)


ggsave(plot = gr_fisc_esp_cd, 
       "gr_fisc_esp_cd.svg", width = 14, height = 7
)


#base para entrega a Marey

fecha_comienzo <- "2024-10-01"
fecha_fin <- "2025-01-15"

data_1_sd_esp <-  juntas %>% 
  filter(area!="Coordinación General de Investigación Territorial") %>%
  mutate(fecha_inicio=dmy(fecha_de_inicio)) %>% 
  mutate(detenido=case_when(
    id_ap %in% flagrancias$id_ap~ "Con detenido", T ~ "Sin detenido"
  )) %>% 
  filter(fecha_inicio>=fecha_comienzo, 
         fecha_inicio<=fecha_fin
  ) %>% 
  filter(#quincena=="Actual",
    detenido=="Sin detenido") %>%
  group_by(fiscalia=siglas_buena, #detenido,
           fecha_inicio,
           fiscal
  ) %>%
  summarise(Total=n()) %>% ungroup() %>%
  # mutate(fiscales=1) %>% 
  group_by(fiscalia, fecha_inicio) %>%
  summarise(fiscales=n(), 
            Total=sum(Total)
  ) %>% group_by(fiscalia) %>% 
  summarise(
    fiscales_promedio=mean(fiscales),
    Total=sum(Total, na.rm = T)
  ) %>% left_join(
    juntas %>% 
      filter(area!="Coordinación General de Investigación Territorial") %>%
      mutate(fecha_inicio=dmy(fecha_de_inicio)) %>% 
      mutate(detenido=case_when(
        id_ap %in% flagrancias$id_ap~ "Con detenido", T ~ "Sin detenido"
      )) %>% 
      filter(fecha_inicio>=fecha_comienzo, 
             fecha_inicio<=fecha_fin
      ) %>% 
      filter(#quincena=="Actual",
        detenido=="Sin detenido") %>%
      group_by(fiscalia=siglas_buena, #detenido,
               #fecha_inicio,
               fiscal
      ) %>%
      summarise(Total=n()) %>% 
      ungroup() %>%
      group_by(fiscalia) %>%
      summarise(fiscales=n()
      ), by="fiscalia"
  )

data_1_cd_esp <-   juntas %>% 
  filter(area!="Coordinación General de Investigación Territorial") %>%
  mutate(fecha_inicio=dmy(fecha_de_inicio)) %>% 
  mutate(detenido=case_when(
    id_ap %in% flagrancias$id_ap~ "Con detenido", T ~ "Sin detenido"
  )) %>% 
  filter(fecha_inicio>=fecha_comienzo, 
         fecha_inicio<=fecha_fin
  ) %>% 
  filter(#quincena=="Actual",
    detenido=="Con detenido") %>%
  group_by(fiscalia=siglas_buena, #detenido,
           fecha_inicio,
           fiscal
  ) %>%
  summarise(Total=n()) %>% ungroup() %>%
  # mutate(fiscales=1) %>% 
  group_by(fiscalia, fecha_inicio) %>%
  summarise(fiscales=n(), 
            Total=sum(Total)
  ) %>% group_by(fiscalia) %>% 
  summarise(
    fiscales_promedio=mean(fiscales),
    Total=sum(Total, na.rm = T)
  ) %>% left_join(
    juntas %>% 
      filter(area!="Coordinación General de Investigación Territorial") %>%
      mutate(fecha_inicio=dmy(fecha_de_inicio)) %>% 
      mutate(detenido=case_when(
        id_ap %in% flagrancias$id_ap~ "Con detenido", T ~ "Sin detenido"
      )) %>% 
      filter(fecha_inicio>=fecha_comienzo, 
             fecha_inicio<=fecha_fin
      ) %>% 
      filter(#quincena=="Actual",
        detenido=="Con detenido") %>%
      group_by(fiscalia=siglas_buena, #detenido,
               #fecha_inicio,
               fiscal
      ) %>%
      summarise(Total=n()) %>% 
      ungroup() %>%
      group_by(fiscalia) %>%
      summarise(fiscales=n()
      ), by="fiscalia"
  )


bind_rows(data_1_sd_esp %>%mutate(detenido="Sin detenido"), data_1_cd_esp %>%
            mutate(detenido="Con detenido")) %>% 
  write.csv("base_fiscalias_mps_totales.csv", row.names = F)



#####gráfica de vinculaiciones y detenciones no legales####
fisc_terr <- flagrancias %>% 
  filter(subprocuraduria=="COORDINACIÓN GENERAL DE INVESTIGACIÓN TERRITORIAL") %>% 
  filter(siglas_fiscalia_inicio!="FIDAMPU") %>% pull(siglas_fiscalia_inicio) %>% 
  unique()
data_9 <- flagrancias %>%
  filter(
    fecha_inicio>="2019-01-01",
    fecha_inicio<=fecha_fin,
    
    #siglas_fiscalia_inicio!="FIDAMPU",
    #!(delito %in% cat_masc$modalidad_delito)

  ) %>%
  filter(!siglas_fiscalia_inicio %in% fisc_terr) %>% 
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
         # fiscalia=substr(fiscalia, 3,nchar(fiscalia))
  ) %>% left_join(
        cat_fiscalias %>%
          select(siglas_fiscalia, siglas_buena, area, fiscalia),
        by=c("fiscalia"="siglas_fiscalia")
      ) %>% 
  filter(fiscalia!="FIEAE")
# ordenes <- incidencia$ordenes
# ordenes_area <- ordenes %>% 
#   left_join(
#     cat_fiscalias %>% 
#       select(siglas_fiscalia, siglas_buena, area, fiscalia) %>% 
#       filter(!duplicated(fiscalia)), 
#     by=c("adscripcion_homologada"="fiscalia")
#   ) %>% 
#   mutate(area=case_when(
#     adscripcion_homologada=="Dirección General de Atención a Víctimas del Delito" ~ "Coordinación General de Investigación de Delitos de Género y Atención a Víctimas", 
#     grepl("Judici", adscripcion_homologada) ~ "Subprocuraduria de Procesos", 
#     # grepl("STCMZV") ~ "Coordinación General de Investigación Territorial",
#     grepl("Desap", adscripcion_homologada) ~ "Coordinación General de Delitos de Alto Impacto",
#     grepl("Electo", adscripcion_homologada) ~ "Oficina de la Fiscalía General de Justicia de la Ciudad de México",
#     T ~ area
#   ), 
#   siglas_buena=case_when(
#     adscripcion_homologada=="Dirección General de Atención a Víctimas del Delito" ~"DGAVDG", 
#     grepl("Judici", adscripcion_homologada) ~ "FPJC", 
#     # grepl("STCMZV") ~ "Coordinación General de Investigación Territorial",
#     grepl("Desap", adscripcion_homologada) ~ "FDMDFDB",
#     
#     grepl("Electo", adscripcion_homologada) ~ "FEPADE",
#     T ~ siglas_buena
#   )
#   )
# 
# tabla_ordenes <-  ordenes_area %>% 
#   filter(as_date(fecha_cumplida)>="2020-01-01") %>% 
#   filter(area!="Coordinación General de Investigación Territorial") %>% 
#   group_by(siglas_buena, 
#            fecha=floor_date(as_date(fecha_cumplida), "1 month")) %>% 
#   summarise(oa_cumplida=n())
# 
# 
# tabla_flagr <-  flagrancias %>% 
#   filter(fecha_inicio>="2020-01-01") %>% 
#   left_join(
#     cat_fiscalias %>% 
#       select(siglas_fiscalia, siglas_buena, area, fiscalia), 
#     by=c("siglas_fiscalia_inicio"="siglas_fiscalia")
#   ) %>% 
#   filter(area!="Coordinación General de Investigación Territorial") %>% 
#   group_by(siglas_buena, 
#            fecha=floor_date(as_date(fecha_inicio), "1 month")) %>% 
#   summarise(vinculacion_flagrancias=sum(vinculacion_a_proceso_por_persona))
# 
# tabla_vinc <- full_join(tabla_flagr, tabla_ordenes, 
#                         by=c("siglas_buena", "fecha")) %>% 
#   replace_na(list(oa_cumplida=0, vinculacion_flagrancias=0)) %>% ungroup() %>% 
#   complete(siglas_buena=unique(tabla_flagr$siglas_buena), 
#            fecha=seq.Date(as_date("2020-01-01"), as_date("2025-01-01"), 
#                           "1 month"), 
#            fill=list(oa_cumplida=0, vinculacion_flagrancias=0)
#   ) %>% 
#   filter(siglas_buena!="DGAVDG") %>% 
#   mutate(total_vinculados=oa_cumplida +vinculacion_flagrancias)
# 

gr_9 <- data_9 %>%
  # gather(tipo, Total, vinculacion_flagrancias:oa_cumplida) %>% 
  # mutate(tipo=case_when(
  #   tipo=="vinculacion_flagrancias" ~ "Flagrancias", 
  #   T ~ "Ordenes cumplidas"
  # )) %>% 
  # rename(fiscalia=siglas_buena) %>% 
  # mutate(fiscalia=factor(fiscalia, 
  #                        levels=c("CUH", "CUJ", "IZC", "MC", "IZP", "VC",
  #                                 "GAM", "BJ", "MH", "TLH", "AO", "AZ", "MA","TLP","COY","XOC"))) %>% 
  # filter(detenido=="Sin detenido", 
  #        fecha_inicio>="2024-01-02", 
  #        fecha_inicio<="2025-01-20") %>% 
  ggplot(aes(fecha_inicio, Total, color=tipo)) +
  geom_point(alpha=.2#, color=colores[2]
             ) +
  # geom_line(color="grey") +
  geom_smooth(se=F#, color=colores[2]
              ) + theme_light() +
  tema_fgj + tema_ppp +
  geom_vline(xintercept=as_date("2024-10-05"), 
             linetype="dashed", size=1, color="#800040"
  ) +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y") +
  facet_wrap(.~fiscalia) +
  scale_color_manual(values = colores) +
  labs(color="")



ggsave(plot = gr_9, 
       "gr_vinculaciones_nolegal.svg", width = 16, height = 7
)

data_9 %>% 
  write.csv("tabla_vinculaciones_nolegal.csv", row.names = F)


# tabla_vinc %>%
#   gather(tipo, Total, vinculacion_flagrancias:oa_cumplida) %>% 
#   mutate(tipo=case_when(
#     tipo=="vinculacion_flagrancias" ~ "Flagrancias", 
#     T ~ "Ordenes cumplidas"
#   )) %>% rename(fiscalia=siglas_buena) %>% 
#   write.csv("tabla_vinculaciones.csv", row.names = F)



#####Grafica de vinculaciones y sentencias ######
#le damos limpieza a la base de sentencias
sentencias_22 <- sentencias %>% 
  filter(as_date(fecha_de_fallo)>="2022-01-01")

sentencias_22 <- sentencias_22 %>% 
  mutate(fiscalia_remite=str_to_upper(fiscalia_que_remitio_expediente_para_acusacion_contiene_catalogo))

sentencias_22 <- sentencias_22 %>% 
  mutate(fiscalia_remite2=case_when(
    grepl("ALVA", fiscalia_remite) ~ "AO", 
    grepl("AZCA", fiscalia_remite) ~ "AZ",
    grepl("COY", fiscalia_remite) ~ "COY",
    grepl("CUH|TEMOC", fiscalia_remite) ~ "CUH",
    grepl("MH|MIGUEL", fiscalia_remite) ~ "MH",
    grepl("XOCH", fiscalia_remite) ~ "XO",
    grepl("IZC", fiscalia_remite) ~ "IZC",
    grepl("MAGDA", fiscalia_remite) ~ "MC",
    grepl("VC|VENUSTI", fiscalia_remite) ~ "VC",
    grepl("GAM|MADERO|GUSTAVO", fiscalia_remite) ~ "GAM",
    grepl("IZP|PALAPA|IZTA", fiscalia_remite) ~ "IZP",
    grepl("MILPA", fiscalia_remite) ~ "MA",
    grepl("ALPAN", fiscalia_remite) ~ "TLP",
    grepl("TLH|TLAH", fiscalia_remite) ~ "TLP",
    grepl("BENITO|BJ", fiscalia_remite) ~ "BJ",
    grepl("CUAJI", fiscalia_remite) ~ "CUJ",
    grepl("SEX", fiscalia_remite) ~ "FIDS",
    grepl("VEH|PORTE", fiscalia_remite) ~ "FIERVT",
    grepl("NARCO", fiscalia_remite) ~ "FIDN",
    grepl("TRATA", fiscalia_remite) ~ "FIDMTP",
    grepl("PROCESO|ACUSA|SUR", fiscalia_remite) ~ "FPJC",
    grepl("SECUES", fiscalia_remite) ~ "FIDDS",
    grepl("DESAP", fiscalia_remite) ~ "FDMDFDB",
    grepl("FEMIN", fiscalia_remite) ~ "FEIDF",
    grepl("HOM", fiscalia_remite) ~ "FIEDH",
    grepl("FINAN", fiscalia_remite) ~ "FIEDF",
    grepl("CENTRAL", fiscalia_remite) ~ "FIEC",
    grepl("PGR|REPUBLICA", fiscalia_remite) ~ "FGR",
    grepl("RELEVAN", fiscalia_remite) ~ "FIAR",
    grepl("V.CTIMAS", fiscalia_remite) ~ "DGAVG",
    grepl("URBAN", fiscalia_remite) ~ "FIDAMPU",
    grepl("ESPECIA", fiscalia_remite) ~ "FIEAE",
    grepl("NIÑ", fiscalia_remite) ~ "FIDCANN",
    grepl("FAMIL", fiscalia_remite) ~ "FIDVF",
    grepl("SERVIDO|CORRU", fiscalia_remite) ~ "FIDCSP",
    grepl("ADOLES", fiscalia_remite) ~ "FJPA",
    grepl("PRIORIT", fiscalia_remite) ~ "FIDAGAP",
    T ~ fiscalia_remite
    
  )) %>% left_join(
    cat_fiscalias %>% 
      select(siglas_fiscalia, siglas_buena, area) %>% 
      filter(!duplicated(siglas_buena)), 
    by=c("fiscalia_remite2"="siglas_buena")
  ) %>% drop_na(area) %>% 
  filter(area!="Coordinación General de Investigación Territorial")

tabla_sentencias <- sentencias_22 %>% 
  mutate(fallo_homologado=ifelse(fallo_homologado=="MIXTA", "CONDENATORIA", fallo_homologado)) %>% 
  group_by(siglas_buena=fiscalia_remite2, fecha=floor_date(as_date(fecha_fallo), "1 month"), 
           fallo_homologado
           ) %>% 
  summarise(Total=n()) %>% 
  spread(fallo_homologado, Total, fill=0)

ordenes <- ordenes %>% 
  left_join(
    cat_fiscalias %>% 
      select(siglas_fiscalia, siglas_buena, area, fiscalia), 
    by=c("adscripcion_homologada"="fiscalia")
  ) %>% 
  mutate(area=case_when(
    adscripcion_homologada=="Dirección General de Atención a Víctimas del Delito" ~ "Coordinación General de Investigación de Delitos de Género y Atención a Víctimas", 
    grepl("Judici", adscripcion_homologada) ~ "Subprocuraduria de Procesos", 
    # grepl("STCMZV") ~ "Coordinación General de Investigación Territorial",
    grepl("Desap", adscripcion_homologada) ~ "Coordinación General de Delitos de Alto Impacto",
    grepl("Electo", adscripcion_homologada) ~ "Oficina de la Fiscalía General de Justicia de la Ciudad de México",
    T ~ area
  ), 
  siglas_buena=case_when(
    adscripcion_homologada=="Dirección General de Atención a Víctimas del Delito" ~"DGAVDG", 
    grepl("Judici", adscripcion_homologada) ~ "FPJC", 
    # grepl("STCMZV") ~ "Coordinación General de Investigación Territorial",
    grepl("Desap", adscripcion_homologada) ~ "FDMDFDB",
    
    grepl("Electo", adscripcion_homologada) ~ "FEPADE",
    T ~ siglas_buena
  )
  )
  filter(is.na(area)) 
  
tabla_ordenes <-  ordenes %>% 
  filter(as_date(fecha_cumplida)>="2020-01-01") %>% 
  filter(area!="Coordinación General de Investigación Territorial") %>% 
  group_by(siglas_buena, 
           fecha=floor_date(as_date(fecha_cumplida), "1 month")) %>% 
  summarise(oa_cumplida=n())


tabla_flagr <-  flagrancias %>% 
  
  filter(fecha_inicio>="2020-01-01") %>% 
  left_join(
    cat_fiscalias %>% 
      select(siglas_fiscalia, siglas_buena, area, fiscalia), 
    by=c("siglas_fiscalia_inicio"="siglas_fiscalia")
  ) %>% 
  filter(area!="Coordinación General de Investigación Territorial") %>% 
  group_by(siglas_buena, 
           fecha=floor_date(as_date(fecha_inicio), "1 month")) %>% 
  summarise(vinculacion_flagrancias=sum(vinculacion_a_proceso_por_persona))

tabla_vinc <- full_join(tabla_flagr, tabla_ordenes, 
                        by=c("siglas_buena", "fecha")) %>% 
  replace_na(list(oa_cumplida=0, vinculacion_flagrancias=0)) %>% ungroup() %>% 
  complete(siglas_buena=unique(tabla_flagr$siglas_buena), 
           fecha=seq.Date(as_date("2020-01-01"), as_date("2025-01-01"), 
                          "1 month"), 
           fill=list(oa_cumplida=0, vinculacion_flagrancias=0)
           ) %>% 
  mutate(vinculados=vinculacion_flagrancias+ oa_cumplida)

data_vinc_sen <- tabla_vinc %>% 
  left_join(tabla_sentencias) %>% 
  replace_na(list(Total=0)) %>% 
  replace_na(list(ABSOLUTORIA=0, CONDENATORIA=0)) %>% 
  select(-c(vinculacion_flagrancias, oa_cumplida)) %>% 
  gather(tipo, Total, vinculados:CONDENATORIA) %>% 
  mutate(tipo=str_to_sentence(tipo))

sentencias <- sentencias %>% 
  mutate(tipo_delito=case_when(
    delito_hom_uet=="FEMINICIDIO" ~ "Feminicidio", 
    delitos_alto_impacto=="HOMICIDIO DOLOSO" ~ "Homicidio doloso",
    grepl("NARCO", delito_hom_uet) ~ "Narcomenudeo", 
    delito_hom_uet=="VIOLACIÓN" ~ "Violación", 
    delito_hom_uet=="ABUSO SEXUAL" ~ "Abuso sexual", 
    delito_hom_uet=="CONTRA LA INTIMIDAD SEXUAL" ~ "Contra la intimidad", 
    delito_hom_uet=="ACOSO SEXUAL"~"Acoso sexual",
    (delitos_alto_impacto=="ROBO DE VEHÍCULO CON Y SIN VIOLENCIA" |
      grepl("PARTE|ENCUBR", delito_hom_uet)) ~ "Robo de vehículo"
  ))

gr_sen_vinc <- sentencias %>% drop_na(tipo_delito) %>% 
  group_by(tipo_delito, fecha=floor_date(fecha_fallo, "1 month"), 
           fallo_homologado) %>% 
  summarise(Total=n()) %>% 
  spread(fallo_homologado, Total, fill=0) %>% 
  mutate(porcentaje=(MIXTA+CONDENATORIA)/(MIXTA+CONDENATORIA+ABSOLUTORIA)) %>% 
  # mutate(fiscalia=factor(fiscalia, 
  #                        levels=c("CUH", "CUJ", "IZC", "MC", "IZP", "VC",
  #                                 "GAM", "BJ", "MH", "TLH", "AO", "AZ", "MA","TLP","COY","XOC"))) %>% 
  # filter(detenido=="Sin detenido", 
  #        fecha_inicio>="2024-01-02", 
  #        fecha_inicio<="2025-01-20") %>% 
  ggplot(aes(fecha, porcentaje)) +
  geom_point(alpha=.35, color=colores[8]) +
  # geom_line(color="grey") +
  geom_smooth(se=F, color=colores[8]) + theme_light() +
  tema_fgj + tema_ppp +
  geom_vline(xintercept=as_date("2024-10-05"), 
             linetype="dashed", size=1, color="#800040"
  ) +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y") +
  facet_wrap(.~tipo_delito) +
  scale_color_manual(values = c(colores[8], colores[7], colores[9])) +
  labs(color="", y="Porcentaje de condenatoria", x="Fecha de fallo")



ggsave(plot = gr_sen_vinc, 
       "gr_sentencias.svg", width = 16, height = 7
)

sentencias %>% drop_na(tipo_delito) %>% 
  group_by(tipo_delito, fecha=floor_date(fecha_fallo, "1 month"), 
           fallo_homologado) %>% 
  summarise(Total=n()) %>% 
  spread(fallo_homologado, Total, fill=0) %>% 
  mutate(porcentaje=(MIXTA+CONDENATORIA)/(MIXTA+CONDENATORIA+ABSOLUTORIA)) %>% 
  write.csv("tabla_sentencias_porcentaje.csv", row.names = F)

data_vinc_sen %>% 
  write.csv("tabla_vinculaciones_sentencias.csv", row.names = F)


#####ordenes libradas #####

ord_lib <- ord_lib %>% 
  mutate(fecha_lib=ymd(paste0(ano_lib, "-", mes_lib, "-01")), 
         fecha_uet=ymd(paste0(ano_reporte_a_uet, "-", mes_reporte_a_ued, "-01"))
         ) %>% 
  # select(fecha_lib, fecha_uet) %>% 
  mutate(diff=ifelse(fecha_lib==fecha_uet, 1, 0))

ord_lib %>% 
  mutate(rango_diff=case_when(
    fecha_uet-fecha_lib<=32 ~ "Menor a 1", 
    fecha_uet-fecha_lib>31 &
      fecha_uet-fecha_lib<=60~ "1 a 3 meses",
    fecha_uet-fecha_lib>60 &
      fecha_uet-fecha_lib<=90~ "3 a 6 meses",
    T ~ "Más de 6 meses"
  )) %>% 
  tabyl(rango_diff)

#obtenemos la tabla de ordenes
tabla_ordenes <-  ordenes_area %>%
  filter(as_date(fecha_cumplida)>="2020-01-01") %>%
  filter(area!="Coordinación General de Investigación Territorial") %>%
  group_by(siglas_buena,
           fecha=floor_date(as_date(fecha_cumplida), "1 month")) %>%
  summarise(oa_cumplida=n()) %>% 
  filter(siglas_buena!="DGAVDG")

gr_ordenes <- tabla_ordenes %>% ungroup() %>% 
  complete(fecha=seq.Date(as_date("2020-01-01"), 
                          as_date("2025-01-01"), 
                          "1 month"
                          ), 
           siglas_buena=c(.$siglas_buena), 
           fill=list(oa_cumplida=0)
           ) %>% 
  ggplot(aes(fecha, oa_cumplida)) +
  geom_point(alpha=.35, color=colores[8]) +
  # geom_line(color="grey") +
  geom_smooth(se=F, color=colores[8]) + theme_light() +
  tema_fgj + tema_ppp +
  geom_vline(xintercept=as_date("2024-10-05"), 
             linetype="dashed", size=1, color="#800040"
  ) +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_labels = "%b\n%Y") +
  facet_wrap(.~siglas_buena, scales = "free_y") +
  scale_color_manual(values = c(colores[8], colores[7], colores[9])) +
  labs(color="", y="Ordenes cumplidas", x="Fecha de fallo")



ggsave(plot = gr_ordenes, 
       "gr_ordenes.svg", width = 16, height = 7
)


tabla_ordenes %>% ungroup() %>% 
  complete(fecha=seq.Date(as_date("2020-01-01"), 
                          as_date("2025-01-01"), 
                          "1 month"
  ), 
  siglas_buena=c(.$siglas_buena), 
  fill=list(oa_cumplida=0)
  ) %>% 
  write.csv("tabla_ordenes.csv", row.names = F)

ordenes_area <- ordenes_area %>% 
  mutate(tipo_delito=case_when(
    delito_hom_uet=="FEMINICIDIO" ~ "Feminicidio", 
    delito_hom_uet=="HOMICIDIO DOLOSO" ~ "Homicidio doloso",
    grepl("NARCO", delito_hom_uet) ~ "Narcomenudeo", 
    delito_hom_uet=="VIOLACIÓN" ~ "Violación", 
    delito_hom_uet=="ABUSO SEXUAL" ~ "Abuso sexual", 
    delito_hom_uet=="CONTRA LA INTIMIDAD SEXUAL" ~ "Contra la intimidad", 
    delito_hom_uet=="ACOSO SEXUAL"~"Acoso sexual",
    (delito_alto_impacto=="ROBO DE VEHÍCULO CON Y SIN VIOLENCIA" |
       grepl("PARTE|ENCUBR", delito_hom_uet)) ~ "Robo de vehículo"
  ))

#ordenes por delito seleccionado
tabla_ordenes_delito <-  ordenes_area %>%
  filter(as_date(fecha_cumplida)>="2020-01-01", 
         as_date(fecha_cumplida)<="2025-01-31"
         ) %>%
  drop_na(tipo_delito) %>% 
  group_by(tipo_delito,
           fecha=floor_date(as_date(fecha_cumplida), "1 month")) %>%
  summarise(oa_cumplida=n()) 

gr_ordenes_delito <- tabla_ordenes_delito %>% ungroup() %>% 
  complete(fecha=seq.Date(as_date("2020-01-01"), 
                          as_date("2025-01-01"), 
                          "1 month"
  ), 
  tipo_delito=c(.$tipo_delito), 
  fill=list(oa_cumplida=0)
  ) %>% 
  ggplot(aes(fecha, oa_cumplida)) +
  geom_point(alpha=.35, color=colores[8]) +
  # geom_line(color="grey") +
  geom_smooth(se=F, color=colores[8]) + theme_light() +
  tema_fgj + tema_ppp +
  geom_vline(xintercept=as_date("2024-10-05"), 
             linetype="dashed", size=1, color="#800040"
  ) +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_labels = "%b\n%Y") +
  facet_wrap(.~tipo_delito, scales = "free_y") +
  scale_color_manual(values = c(colores[8], colores[7], colores[9])) +
  labs(color="", y="Ordenes cumplidas", x="Fecha de cumplimiento")


ggsave(plot = gr_ordenes_delito, 
       "gr_ordenes_delito.svg", width = 16, height = 7
)


tabla_ordenes_delito %>% ungroup() %>% 
  complete(fecha=seq.Date(as_date("2020-01-01"), 
                          as_date("2025-01-01"), 
                          "1 month"
  ), 
  tipo_delito=c(.$tipo_delito), 
  fill=list(oa_cumplida=0)
  ) %>% 
  write.csv("tabla_ordenes_delito.csv", row.names = F)

#de las ordenes cumplidas, vamos a meter la adcrispción homologada a 
#las ordenes libradas
ord_lib_area <- ord_lib %>% 
  mutate(rango_diff=case_when(
    fecha_uet-fecha_lib<=32 ~ "Menor a 1", 
    fecha_uet-fecha_lib>31 &
      fecha_uet-fecha_lib<=60~ "1 a 3 meses",
    fecha_uet-fecha_lib>60 &
      fecha_uet-fecha_lib<=90~ "3 a 6 meses",
    T ~ "Más de 6 meses"
  )) %>% 
  mutate(id_ap=as.integer(id_ap_uet)) %>% 
  left_join(ordenes_area %>% 
              mutate(id_ap=as.integer(id_carpeta_uet)) %>% 
              drop_na(id_ap) %>% 
              filter(!duplicated(id_ap)) %>% 
              select(id_ap, siglas_buena, area), 
            by="id_ap") 

tabla_ord_librada <- ord_lib_area %>% drop_na(area) %>% 
  filter(area!="Coordinación General de Investigación Territorial") %>% 
  group_by(siglas_buena, ano_reporte_a_uet, 
           rango_diff) %>% 
  summarise(Total=n()) %>% 
  mutate(rango_diff=factor(rango_diff, 
                           levels=rev(c("Menor a 1", "1 a 3 meses", 
                                    "3 a 6 meses", "Más de 6 meses")), 
                           labels=rev(c("Menor a 1 mes", "1 a 3 meses",
                                    "3 a 6 meses", "Más de 6 meses"))
                           )
         ) %>% 
  filter(siglas_buena!="DGAVDG") %>% 
  group_by(siglas_buena, ano_reporte_a_uet ) %>% 
  mutate(porcentaje=Total/sum(Total))

tabla_ord_librada %>% 
  mutate(porcentaje=percent(porcentaje)) %>% 
  write.csv("tabla_ordenes_libradas_rango_diferencia.csv", row.names = F)


ord_lib_area %>% 
  write.csv("ordenes_libradas_area_bruto.csv", row.names = F)

gr_ordenes_libradas_abs <- tabla_ord_librada %>% 
  ggplot(aes(ano_reporte_a_uet, Total, fill=rango_diff)) +
  geom_col() +
  geom_text(aes(label=Total), 
            position = position_stack(vjust = .8)) + theme_minimal() +
  tema_fgj + tema_ppp +
  facet_wrap(.~siglas_buena, scales = "free_y") +
  theme(legend) +
  scale_fill_manual(values = colores, 
                    guide = guide_legend(reverse = TRUE)) +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(angle=90) ) +
  labs(x="Año de reporte a UET", y="Total de ordenes libradas", 
       fill="")

ggsave(plot = gr_ordenes_libradas_abs, 
       "gr_ordenes_libradas_absolutas.svg", width = 16, height = 7
)

gr_ordenes_libradas_por <- tabla_ord_librada %>% 
  ggplot(aes(ano_reporte_a_uet, porcentaje, fill=rango_diff)) +
  geom_col() +
  geom_text(aes(label=percent(porcentaje, 1)), 
            position = position_stack(vjust = .8)) + theme_minimal() +
  tema_fgj + tema_ppp +
  facet_wrap(.~siglas_buena, scales = "free_y") +
  theme(legend) +
  scale_fill_manual(values = colores, 
                    guide = guide_legend(reverse = TRUE)
                    ) +
  scale_y_continuous(labels = percent) + 
  theme(axis.text.x = element_text(angle=90) ) +
  labs(x="Año de reporte a UET", y="Porcentaje de ordenes libradas", 
       fill="")

ggsave(plot = gr_ordenes_libradas_por, 
       "gr_ordenes_libradas_porcentaje.svg", width = 16, height = 7
)
#####tabla de radicaciones por coordinación y fiscalía ####

data_area_siglas <- data_area %>% 
  left_join(cat_fiscalias %>% 
              select(fiscalia, siglas_buena), 
            by=c("fiscalia_des"="fiscalia")
            ) %>% 
  mutate(siglas_buena=case_when(
    grepl("ALVA", fiscalia_des) ~ "AO", 
    grepl("AZCA", fiscalia_des) ~ "AZ",
    grepl("COY", fiscalia_des) ~ "COY",
    grepl("CUH|T.MOC", fiscalia_des) ~ "CUH",
    grepl("MH|MIGUEL", fiscalia_des) ~ "MH",
    grepl("XOCH", fiscalia_des) ~ "XO",
    grepl("IZC", fiscalia_des) ~ "IZC",
    grepl("MAGDA", fiscalia_des) ~ "MC",
    grepl("VC|VENUSTI", fiscalia_des) ~ "VC",
    grepl("GAM|MADERO|GUSTAVO", fiscalia_des) ~ "GAM",
    grepl("IZP|PALAPA|IZTA", fiscalia_des) ~ "IZP",
    grepl("MILPA", fiscalia_des) ~ "MA",
    grepl("ALPAN", fiscalia_des) ~ "TLP",
    grepl("TLH|TL.H", fiscalia_des) ~ "TLH",
    grepl("BENITO|BJ", fiscalia_des) ~ "BJ",
    
    grepl("CUAJI", fiscalia_des) ~ "CUJ",
    grepl("SEX", fiscalia_des) ~ "FIDS",
    grepl("VEH|PORTE", fiscalia_des) ~ "FIERVT",
    grepl("NARCO", fiscalia_des) ~ "FIDN",
    grepl("TRATA", fiscalia_des) ~ "FIDMTP",
    grepl("PROCESO|ACUSA|SUR|PENAL", fiscalia_des) ~ "FPJC",
    grepl("SECUES", fiscalia_des) ~ "FIDDS",
    grepl("DESAP", fiscalia_des) ~ "FDMDFDB",
    grepl("FEMIN", fiscalia_des) ~ "FEIDF",
    grepl("HOM", fiscalia_des) ~ "FIEDH",
    grepl("FINAN", fiscalia_des) ~ "FIEDF",
    grepl("CENTRAL", fiscalia_des) ~ "FIEC",
    grepl("PGR|REPUBLICA", fiscalia_des) ~ "FGR",
    grepl("RELEVAN", fiscalia_des) ~ "FIAR",
    grepl("V.CTIMAS", fiscalia_des) ~ "DGAVG",
    grepl("URBAN", fiscalia_des) ~ "FIDAMPU",
    grepl("ESPECIA", fiscalia_des) ~ "FIEAE",
    grepl("NIÑ", fiscalia_des) ~ "FIDCANN",
    grepl("FAMIL", fiscalia_des) ~ "FIDVF",
    grepl("SERVIDO|CORRU", fiscalia_des) ~ "FIDCSP",
    grepl("ADOLES", fiscalia_des) ~ "FJPA",
    grepl("PRIORIT", fiscalia_des) ~ "FIDAGAP",
    grepl("DIGITAL|INTER", fiscalia_des) ~ "ADD", 
    grepl("AUXILI|REVISI", fiscalia_des) ~ "CAMPAP", 
    
    T ~ siglas_buena
    
  ))


#exportamos la tabla para Fer
data_area_siglas %>% 
  filter(!duplicated(id_ap)) %>% 
  select(-c(cambio_radicacion)) %>% 
  mutate(fecha=case_when(
    is.na(fecharecep)~ fecha_inicio,
    T ~ as_date(fecharecep)),
    año_radicada=year(fecha)) %>% 
  rename(area_destino=area, 
         siglas_fiscalia_destino=siglas_buena
         ) %>% 
  write.csv("data_radicaciones_fer.csv", row.names = F)


data_area_siglas %>% 
  filter(!duplicated(id_ap)) %>%
  filter(area=="Coordinación General de Investigación Territorial", 
         fecha_inicio<="2025-01-31"
  ) %>% 
  group_by(fecha_inicio=floor_date(fecha_inicio, "1 month"), 
           tipo_delito
  ) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date("2020-01-01"), 
                                 as_date("2025-01-01"), "1 month"
  ), 
  tipo_delito=c(.$tipo_delito), 
  fill=list(Total=0)
  ) %>% 
  write.csv("data_radicaciones_territoriales_delito.csv", row.names = F)

#####rezago de fiscalía ######
#calculo de rezago 
data_estatus_milei %>% 
  filter(territoriales==0) %>% 
  filter(fecha_inicio<="2024-12-31") %>% 
  left_join(cat_fiscalias %>% 
              select(siglas_fiscalia, 
                     area, siglas_buena
              ), 
            by=c("fiscalia_ini"="siglas_fiscalia")
  ) %>% 
  mutate(siglas_buena=case_when(
    fiscalia_ini=="FDMDFDBP" ~ "FDMDFDB", 
    T ~ siglas_buena
  )) %>% 
  filter(area!="Coordinación General de Investigación Territorial") %>% 
  group_by(siglas_buena) %>% 
  summarise(Total=n()) %>% 
  write.csv("rezago_especialidades.csv", row.names = F)

# juntas %>% write.csv("iniciadas_coordinacion.csv", row.names = F)
# incidencia <- read_rds("H:/Mi unidad/bases_uet/incidencia_230225.rds")


campap_total <- readRDS("H:/Mi unidad/bases/campap_total_240225.rds")

base_coord <- base %>% 
  left_join(cat_fiscalias %>% 
              select(siglas_fiscalia, siglas_buena, area), 
            by=c("fiscalia_ini"="siglas_fiscalia")
  ) %>% 
  mutate(area=case_when(
    is.na(area) & 
      fiscalia_ini=="DGAVD" ~ "Coordinación General de Investigación de Delitos de Género y Atención a Víctimas", 
    is.na(area) & 
      fiscalia_ini=="FDMDFDBP" ~ "Coordinación General de Delitos de Alto Impacto", 
    T ~ area
  ))


flagrancias <- incidencia$flagrancias
ordenes <- incidencia$ordenes
sentencias <- incidencia$sentencias
ord_lib <- readxl::read_excel("I:/Mi unidad/0. Bases/2018-2025_03_06 ORDENES APREHENSION.xlsx", 
                              sheet = "LIBRADAS") %>% clean_names()

campap <- readRDS("H:/Mi unidad/bases/campap_240225.rds")

campap <- campap %>% 
  left_join(juntas %>% 
              select("id_ap"=id_ci, area, fecha_inicio), by="id_ap"
              )

base_coord <- base_coord %>% 
  left_join(campap_total %>% 
              select(-ctrluinv)
              )

base_coord %>% 
  # drop_na(fecdictamen) %>% 
  filter(estatus=="Aprobada     ") %>% 
  filter(fecha_inicio>="2024-01-01",
         fecha_inicio<="2025-02-15",
    yday(fecha_inicio)<=yday("2025-02-15")) %>% 
  drop_na(propuesta) %>% 
  mutate(año=year(fecha_inicio)) %>% 
  group_by(año, area ) %>% 
  summarise(Total=n()) %>% 
  # spread(año, Total, fill=0)
  pivot_wider(names_from = c("año"), 
              values_from = "Total", 
              values_fill = 0) %>% 
  View()

base_coord %>% 
  drop_na(fecdictamen) %>% 
  filter(estatus=="Pendiente    ") %>% 
  filter(fecdictamen>="2024-01-01",
         fecdictamen<="2025-02-15",
         yday(fecdictamen)<=yday("2025-02-15")) %>% 
  drop_na(propuesta) %>% 
  mutate(año=year(fecdictamen)) %>% 
  group_by(año,area ) %>% 
  summarise(Total=n()) %>% 
  # spread(año, Total, fill=0)
  pivot_wider(names_from = c("año", ), 
              values_from = "Total", 
              values_fill = 0) %>% 
  View()


#ordenes
ordenes <- ordenes %>% 
  left_join(cat_fiscalias %>% 
              filter(!duplicated(fiscalia)) %>% 
              select(fiscalia, siglas_buena, area), 
            by=c("adscripcion_homologada"="fiscalia")
              )


ordenes <- ordenes %>% 
  mutate(area=case_when(
    is.na(area) & 
      adscripcion_homologada=="Dirección General de Atención a Víctimas del Delito" ~ "Coordinación General de Investigación de Delitos de Género y Atención a Víctimas", 
    is.na(area) & 
      adscripcion_homologada=="Dirección General de Supervisión y Seguimiento de Ordenamientos Judiciales" ~ "Subprocuraduria de Procesos",
    is.na(area) & 
      adscripcion_homologada=="Fiscalia de Investigacion Especializada de la STCMZV" ~ "Coordinación General de Investigación Territorial", 
    is.na(area) & 
      adscripcion_homologada=="Fiscalía Especializada para la Atención de Delitos Electorales" ~ "Oficina de la Fiscalía General de Justicia de la Ciudad de México", 
    is.na(area) & 
      adscripcion_homologada=="Fiscalía de Investigación y Persecución de los Delitos en Materia de Desaparición Forzada de Personas y la Desaparición Cometida por Particulares y Búsqueda de Personas Desaparecidas" ~ "Coordinación General de Delitos de Alto Impacto", 
    is.na(area) &
      adscripcion_homologada=="Fiscalía de Mandamientos Judiciales" ~ "Subprocuraduria de Procesos", 
    T ~ area
  ))

ordenes %>% 
  filter(fecha_cumplida>="2024-01-01") %>% 
  filter(yday(as_date(fecha_cumplida))<=yday(as_date("2025-02-15"))) %>% 
  group_by(año=year(as_date(fecha_cumplida)), 
           area
           ) %>% 
  summarise(Total=n()) %>% drop_na(area) %>% 
  pivot_wider(names_from = "año", 
              values_from = "Total", 
              values_fill = 0)




##### Eficiencia ministierial ####
base <- incidencia$base
ordenes <- ordenes %>% 
  mutate(tipo_delito=case_when(
    grepl("FEM", delito_hom_uet) & !grepl("TENTA", delito_hom_uet) ~ "Feminicidio", 
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


ini <- readRDS("H:/Mi unidad/bases/iniciadas_240225.rds")

ini <- ini %>% 
  left_join(juntas %>% 
              select("id_ap"=id_ci, area, fiscalia, siglas_fiscalia), 
            by="id_ap"
              )

efi_ministerial <- function(
    datos=data, 
    fecha_inicio_global=fecha_inicio_global,
    fecha_lim=fecha_lim, 
    delito_elegido=delito 
    #tipo=c("Vinculaciones", "Sentencias")
){
  
  carpetas <- datos %>% 
    filter(yday(fecha_inicio)<=yday(fecha_lim)) %>% 
    group_by(area, año=year(fecha_inicio)) %>% 
    summarise(Carpetas=n()) 
  
  data_id <- datos%>% drop_na(id_ap) %>% pull(id_ap)
  
  
  vinculaciones_flagr <- flagrancias %>% 
    filter(id_ap %in% data_id) %>% 
    filter(yday(fecha_inicio)<=yday(fecha_lim)) %>% 
    left_join(juntas %>% 
                select("id_ap"=id_ci, area, fiscalia, siglas_fiscalia), 
              by="id_ap"
    ) %>% 
    group_by(area, año=year(fecha_inicio)) %>% 
    summarise(Vinculados_flagr=sum(vinculacion_a_proceso_por_persona, na.rm = T))
  
  
  vinculaciones_ord <- ordenes %>% 
    filter(as_date(fecha_cumplida)>=fecha_inicio_global) %>% 
    filter(yday(fecha_cumplida)<=yday(fecha_lim)) %>% 
    
    #filter(cat_mp2 %in% delito_elegido) %>% 
    filter(tipo_delito %in% delito_elegido) %>% 
    group_by(area, año=year(as_date(fecha_cumplida))) %>% 
    summarise(Vinculados_ord=n()) %>% 
    drop_na(area)
  
  total_vinc <- full_join(vinculaciones_flagr, vinculaciones_ord) %>% 
    replace_na(list(Vinculados_ord=0, Vinculados_flagr=0))
  
  # total_sen <- sentencias %>% 
  #   filter(tipo_delito %in% delito_elegido, 
  #          fallo_homologado=="CONDENATORIA"
  #          ) %>% 
  #   group_by(fecha_inicio=floor_date(fecha_de_fallo, "1 year")) %>% 
  #   summarise(Sentencias=n())
  
  
  efi <- carpetas %>% 
    full_join(total_vinc, by=c("area", "año")) %>% 
    mutate(total_vinculados=Vinculados_flagr+Vinculados_ord) %>% 
    gather(tipo, Total, Carpetas:total_vinculados) %>% 
    # mutate(fecha_inicio=as_date(fecha_inicio)) %>% 
    # complete(fecha_inicio=seq.Date(as_date(fecha_inicio_global), 
    #                                as_date(fecha_lim), "1 year"), 
    #          tipo=c("Carpetas", "Vinculaciones"),
    #          fill=list(Total=0) 
    # ) %>% mutate(año=year(fecha_inicio)) %>% select(-fecha_inicio) %>% 
    # spread(año, Total, fill=0)
    pivot_wider(names_from = c("area", "año"), 
                values_from = "Total", 
                values_fill = 0)
    # ggplot(aes(fecha_inicio, Total, color=tipo)) +
    # geom_point(alpha=.7) +
    # geom_smooth(se=F, ) + theme_light() + tema_fgj +
    # geom_label_repel(aes(label=comma(Total)), 
    #                  size=5, show.legend = F, direction = "y"
    # )+
    # # geom_smooth(se=F, color=colores[2], method = "lm") +
    # scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
    # labs(x="Fecha de inicio", y="Total", 
    #      title = paste0("Total de vinculaciones y carpetas por ", knitr::combine_words(delito_elegido, and = " y ")), 
    #      subtitle = paste0("Desde ",  format(as_date(fecha_inicio_global), "%B de %Y"), " a ",
    #                        format(as_date(fecha_lim), "%B de %Y"))) +
    # theme(axis.text.x = element_text(angle = 90)) +
    # tema_fgj + scale_y_continuous(labels = comma) +
    # theme(legend.position = "bottom") +
    # scale_color_manual(values = colores[7:9]) +
    # tema_ppp
  
 
  return(efi)
  
  
}


efi_hom <- efi_ministerial(datos=ini %>% 
                  filter(delito=="HOMICIDIO DOLOSO", 
                         grepl("HOM", modalidad_delito), 
                         year(fecha_inicio)>=2024
                         ), 
                  fecha_inicio_global="2024-01-01", 
                  fecha_lim="2025-02-23", 
                  delito_elegido = "Homicidio doloso"
                  )


efi_fem <- efi_ministerial(datos=ini %>% 
                             filter(delito=="HOMICIDIO DOLOSO", 
                                    grepl("FEM", modalidad_delito), 
                                    year(fecha_inicio)>=2024
                             ), 
                           fecha_inicio_global="2024-01-01", 
                           fecha_lim="2025-02-23", 
                           delito_elegido = "Feminicidio"
)


efi_fam <- efi_ministerial(datos=ini %>% 
                             filter(modalidad_delito=="VIOLENCIA FAMILIAR", 
                                    year(fecha_inicio)>=2024
                             ), 
                           fecha_inicio_global="2024-01-01", 
                           fecha_lim="2025-02-23", 
                           delito_elegido = "Violencia familiar"
)

efi_robo_veh <- efi_ministerial(datos=ini %>% 
                             filter(delito=="ROBO DE VEHÍCULO CON Y SIN VIOLENCIA", 
                                    year(fecha_inicio)>=2024
                             ), 
                           fecha_inicio_global="2024-01-01", 
                           fecha_lim="2025-02-23", 
                           delito_elegido = c("Robo de vehículo sin violencia", 
                                              "Robo de vehículo con violencia", 
                                              "Robo de vehículo"
                                              )
)


efi_robo_narco <- efi_ministerial(datos=ini %>% 
                                  filter(grepl("NARCO", modalidad_delito)
                                  ), 
                                fecha_inicio_global="2024-01-01", 
                                fecha_lim="2025-02-23", 
                                delito_elegido = c("Narcomenudeo simple", 
                                                   "Narcomenudeo con fines de venta"
                                )
)

efi_total <- efi_ministerial(datos=ini %>% 
                             filter(year(fecha_inicio)>=2024
                             ) %>% 
                               filter(fecha_inicio<="2025-02-15")
                               , 
                           fecha_inicio_global="2024-01-01", 
                           fecha_lim="2025-02-15", 
                           delito_elegido = unique(ordenes$tipo_delito)
)



#exportamos todo a un excel

library(openxlsx)
wb <- createWorkbook()

vector <- c("efi_hom", "efi_fem", "efi_fam", "efi_robo_veh", "efi_robo_narco", "efi_total")

for (i in 1:6) {
  addWorksheet(wb, vector[i])
  writeData(wb, vector[i], get(vector[i]))
}


saveWorkbook(wb, "eficiencia_ministerial_esp_250225.xlsx", overwrite = T)
