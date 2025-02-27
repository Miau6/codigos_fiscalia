####Script de evaluación de agencias####
setwd("~/R/fiscalia/Evaluaciones_agencias")
library(ggrepel)
# iniciadas_uet <- read_rds()
iniciadas <- readxl::read_excel("Iniciadas por Territoriales.xlsx") %>% clean_names()

personal <- readxl::read_excel("IniciadasporTerritoriales.xlsx") %>% clean_names() %>% 
  select(ctrluinv, fiscal)


iniciadas <- iniciadas %>% mutate(id_ap=as.integer(id_ap)) %>% 
  left_join(personal, "ctrluinv") #%>% 
  # left_join(incidencia$base %>% 
  #           select(id_ap, delito, modalidad_delito), 
  #           by="id_ap"
  #           )

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

#metemos nuevos datos de jaime
iniciadas <- read_excel("H:/Mi unidad/peticiones_jaime/Iniciadas por MP - Extraida el 24022025/iniciadas por  MP - listado 24022025.xlsx") %>% 
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
         )

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


agencias_fiscalias <- read_excel("fiscalias_agencias.xlsx")

iniciadas %>% 
  tabyl(fiscalia) %>% 
  left_join(agencias_fiscalias)

#filtramos por fiscalía territorial
grafica_cargas_trabajo <- function(
  datos=iniciadas, 
  fiscalia_analisis=siglas_fiscalia_analisis,
  fecha_comienzo=inicio_quincena, 
  fecha_fin=final_quincena, 
  tipo = c("total", "promedio", "mp", "agencia")
){
  data <- datos %>% 
    # filter(grepl(fiscalia_analisis, ct_inicio_ap)) %>% 
    mutate(fecha_inicio=as_date(fecha_de_inicio)) %>% 
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
  else if (tipo=="mp") {
    
    periodo <-  as.integer(as_date(fecha_fin)-as_date(fecha_comienzo))+1
    
    data_1_sd <- data %>% 
      filter(quincena=="Actual", 
             detenido=="Sin detenido") %>% 
      group_by(fiscalia, #detenido, 
               # fecha_inicio, 
               fiscal
      ) %>% 
      summarise(Total=n()/periodo) 
    
    
    
    data_1_cd <- data %>% 
      filter(quincena=="Actual", 
             detenido=="Con detenido")%>% 
      group_by(fiscalia, #detenido, 
               # fecha_inicio, 
               fiscal, grupo
      ) %>% 
      summarise(Total=n()/periodo) 

    
    prom_territoriales_sd <- data_1_sd %>% ungroup() %>% 
      # group_by(fiscalia) %>% 
      summarise(Total=mean(Total))
    
    # prom_territoriales_cd <- iniciadas %>% 
    #   mutate(fecha_inicio=dmy(fecha_de_inicio)) %>% 
    #   filter(fecha_inicio>=fecha_comienzo, 
    #          fecha_inicio<=fecha_fin
    #   ) %>% 
    #   filter(!id_ap %in% data$id_ap ) %>% 
    #   mutate(detenido=case_when(
    #     id_ap %in% flagrancias$id_ap ~ "Con detenido", T ~ "Sin detenido" 
    #   )) %>% 
    #   filter(detenido=="Con detenido") %>% 
    #   group_by(fecha_inicio, ct_inicio_ap, fiscal) %>% 
    #   summarise(Total=n()) %>% ungroup() %>%  
    #   complete(
    #     fecha_inicio=seq.Date(as_date(fecha_comienzo),
    #                           as_date(fecha_fin),
    #                           "1 day"
    #     ),
    #     ct_inicio_ap=unique(.$ct_inicio_ap), 
    #     fill=list(Total=0)
    #     
    #   )  %>% 
    #   summarise(media=round(mean(Total, na.rm = T), 2)) %>% pull(media)
    
    prom_territoriales_cd <- data_1_cd %>% ungroup() %>% 
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
      group_by(fiscalia, detenido, grupo) %>% 
      summarise(Total=mean(Total))
    
    total_mp <- bind_rows(data_1_sd %>% 
                            mutate(detenido="Sin detenido")
                          , data_1_cd%>% 
                            mutate(detenido="Con detenido")) %>% 
      drop_na(fiscal) %>% 
      
      # rename(Total=media) %>%
      group_by(fiscalia, detenido, fiscal) %>% 
      summarise(Total=n()) %>% ungroup() %>% 
      mutate(id_fiscal=paste0(fiscal, "-", detenido)) %>% 
      filter(!duplicated(id_fiscal)) %>% 
      group_by(fiscalia, detenido) %>% 
      summarise(agentes=sum(Total))
      
    
    gr_sd <- data_agrupada %>% 
      filter(detenido=="Sin detenido") %>% 
      mutate(fiscalia=factor(fiscalia, 
                             levels=orden_data_1_sd)) %>% 
      # left_join(total_mp %>% 
      #             filter(detenido=="Sin detenido")
      #             , by="fiscalia") %>% 
      # mutate(label=paste0(fiscalia, "\n", agentes)) %>%
      # ggplot(aes(reorder(label, -Total), Total)) +
      ggplot(aes(fiscalia, Total)) +
      geom_col(fill=rgb(46,92,129, maxColorValue = 255), alpha=.9) +
      geom_jitter(data=data_1_sd %>% 
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
      geom_text(aes(label=round(Total, 2)), fontface="bold",
                position = position_stack(vjust = 1.1), size=6
      ) +
      tema_fgj +
      geom_label(x=length(unique(data_1_sd$fiscalia)), size=6.5, 
                 y=prom_territoriales_sd$Total*1.1, label=round(prom_territoriales_sd$Total,2), 
                 fill="#B09D83", color="ghostwhite"
      ) +
      scale_fill_manual(values = colores[7:10]) +
      theme(legend.position = "none") +
      labs(x="", y="", title = "Sin detenido") +
      tema_ppp
    
    gr_cd <- data_agrupada %>% 
      filter(detenido=="Con detenido") %>% 
      mutate(fiscalia=factor(fiscalia, 
                             levels=orden_data_1_cd)) %>% 
      # left_join(total_mp %>% 
      #             filter(detenido=="Con detenido"), by="fiscalia") %>% 
      # mutate(label=paste0(fiscalia, "\n", agentes)) %>%
    ggplot(aes(fiscalia, Total)) +
      geom_col(fill=rgb(46,92,129, maxColorValue = 255), alpha=.9) +
      geom_jitter(data=data_1_cd %>% 
                    mutate(fiscalia=factor(fiscalia, 
                                           levels=orden_data_1_cd)), 
                  aes(x=fiscalia, y=Total), size=2, 
                  color=rgb(248,175,103, maxColorValue = 255), fill=rgb(248,175,103, maxColorValue = 255),
                  width = .1, 
                  shape=18
      ) +
      geom_hline( 
        aes(yintercept=prom_territoriales_cd$Total),
        linetype="dashed", size=1, color="#B09D83") +
      theme_light() +
      geom_text(aes(label=round(Total, 2)), fontface="bold",
                position = position_stack(vjust = 1.1), size=6
      ) +
      tema_fgj +
      geom_label(x=length(unique(data_1_cd$fiscalia)), size=6.5, 
                 y=prom_territoriales_cd$Total*1.1, label=round(prom_territoriales_cd$Total,2), 
                 fill="#B09D83", color="ghostwhite"
      ) +
      scale_fill_manual(values = colores[7:10]) +
      
      tema_ppp +
      labs(x="Agencia de inicio", y="Promedio diario de carpetasn por MP", 
           # legend.position="none",
           title = "Con detenido") +
      theme(legend.position = "none") 
    
    gr_1 <- cowplot::plot_grid(gr_sd, gr_cd, nrow = 2)
  } 
  else if (tipo=="agencia"){
    periodo <-  as.integer(as_date(fecha_fin)-as_date(fecha_comienzo))+1
    
    data_1_sd <- data %>% 
      filter(quincena=="Actual", 
             detenido=="Sin detenido") %>% 
      group_by(ct_inicio_ap, #detenido, 
               # fecha_inicio, 
               fiscal, grupo
      ) %>% 
      summarise(Total=n()/periodo) #%>% drop_na(fiscal)

    data_1_cd <- data %>% 
      filter(quincena=="Actual", 
             detenido=="Con detenido")%>% 
      group_by(ct_inicio_ap, #detenido, 
               # fecha_inicio, 
               fiscal, grupo
      ) %>% 
      summarise(Total=n()/periodo) %>% drop_na(fiscal) 

    
    prom_territoriales_sd <- data_1_sd %>% ungroup() %>% 
      # group_by(fiscalia) %>% 
      summarise(Total=mean(Total))
    
  
    prom_territoriales_cd <- data_1_cd %>% ungroup() %>% 
      filter(!ct_inicio_ap %in% c("IZP-4", "IZP-8", "IZP-9", 
                                  "COY-3")) %>% 
      # group_by(fiscalia) %>% 
      summarise(Total=mean(Total))
    
    orden_data_1_sd <- data_1_sd %>%
      group_by(ct_inicio_ap) %>% summarise(media=sum(Total)) %>% 
      arrange(-media) %>% pull(ct_inicio_ap)
    orden_data_1_cd <- data_1_cd %>% 
      group_by(ct_inicio_ap) %>% summarise(media=sum(Total)) %>% 
      arrange(-media) %>% pull(ct_inicio_ap)
    
    # prom_fisc <- bind_rows(data_1_sd, data_1_cd) %>% ungroup() %>% 
    #   summarise(media=round(mean(Total), 2)) %>% pull(media)
    
    data_agrupada <- bind_rows(data_1_sd %>% 
                                 mutate(detenido="Sin detenido")
                               , data_1_cd%>% 
                                 mutate(detenido="Con detenido")) %>% 
      # rename(Total=media) %>%
      group_by(ct_inicio_ap, detenido, grupo) %>% 
      summarise(Total=mean(Total)) %>% 
      mutate(fiscalia=gsub("-|[0-9]", "", ct_inicio_ap))
    
    total_mp <- bind_rows(data_1_sd %>% 
                            mutate(detenido="Sin detenido")
                          , data_1_cd%>% 
                            mutate(detenido="Con detenido")) %>% 
      drop_na(fiscal) %>% 
      
      # rename(Total=media) %>%
      group_by(ct_inicio_ap, detenido, fiscal) %>% 
      summarise(Total=n()) %>% ungroup() %>% 
      mutate(id_fiscal=paste0(fiscal, "-", detenido)) %>% 
      filter(!duplicated(id_fiscal)) %>% 
      group_by(ct_inicio_ap, detenido) %>% 
      summarise(agentes=sum(Total))
    
    gr_sd <- data_1_sd %>% 
      # filter(detenido=="Sin detenido") %>% 
      mutate(fiscalia=gsub("-|[0-9]", "", ct_inicio_ap)) %>% 
      # left_join(total_mp %>% 
      #             filter(detenido=="Sin detenido")
      #           , by="ct_inicio_ap") %>% 
      # mutate(label=paste0(ct_inicio_ap, "\n", agentes)) %>%
      mutate(ct_inicio_ap=factor(ct_inicio_ap, 
                             levels=orden_data_1_sd)) %>% 
    ggplot(aes(ct_inicio_ap, Total)) +
      geom_col(data=. %>% 
                 group_by(fiscalia, ct_inicio_ap) %>% 
                 summarise(Total=mean(Total)) %>% 
                 mutate(ct_inicio_ap=factor(ct_inicio_ap, 
                                            levels=orden_data_1_sd)),
               fill=rgb(46,92,129, maxColorValue = 255), alpha=.9) +
      geom_jitter(
                  aes(x=ct_inicio_ap, y=Total), size=2.5,
                  color=rgb(248,175,103, maxColorValue = 255), fill=rgb(248,175,103, maxColorValue = 255),
                  width = .1,
                  shape=18
      ) +
      geom_hline( 
        aes(yintercept=prom_territoriales_sd$Total),
        linetype="dashed", size=1, color="#B09D83") +
      theme_light() +
      geom_text(data=. %>% 
                  group_by(fiscalia, ct_inicio_ap) %>% 
                  summarise(Total=mean(Total)) %>% 
                  mutate(ct_inicio_ap=factor(ct_inicio_ap, 
                                             levels=orden_data_1_sd)),
                aes(label=round(Total, 2)), 
                position = position_stack(vjust = 1.1), size=4.5
      ) +
      tema_fgj +
      geom_label(x=length(unique(data_1_sd$ct_inicio_ap)), size=4.5, 
                 y=prom_territoriales_sd$Total*1.1, label=round(prom_territoriales_sd$Total,2), 
                 fill="#B09D83", color="ghostwhite"
      ) +
      scale_fill_manual(values = colores[7:10]) + tema_ppp +
      theme(legend.position = "none", 
            axis.text.x = element_text(size = 9, angle=90)) +
      labs(x="", y="", title = "Sin detenido") 
      
    
    gr_cd <- data_1_cd %>% 
      mutate(fiscalia=gsub("-|[0-9]", "", ct_inicio_ap)) %>% 
      # left_join(total_mp %>% 
      #             filter(detenido=="Con detenido")
      #           , by="ct_inicio_ap") %>% 
      # mutate(label=paste0(ct_inicio_ap, "\n", agentes)) %>%
      mutate(ct_inicio_ap=factor(ct_inicio_ap, 
                                 levels=orden_data_1_cd)) %>% 
      ggplot(aes(ct_inicio_ap, Total)) +
      geom_col(data=. %>% 
                 group_by(fiscalia, ct_inicio_ap) %>% 
                 summarise(Total=mean(Total)) %>% 
                 mutate(ct_inicio_ap=factor(ct_inicio_ap, 
                                            levels=orden_data_1_cd)),
               fill=rgb(46,92,129, maxColorValue = 255), alpha=.9) +
      geom_jitter(
        aes(x=ct_inicio_ap, y=Total), size=2.5,
        color=rgb(248,175,103, maxColorValue = 255), fill=rgb(248,175,103, maxColorValue = 255),
        width = .1,
        shape=18
      ) +
      geom_hline( 
        aes(yintercept=prom_territoriales_cd$Total),
        linetype="dashed", size=1, color="#B09D83") +
      theme_light() +
      geom_text(data=. %>% 
                  group_by(fiscalia, ct_inicio_ap) %>% 
                  summarise(Total=mean(Total)) %>% 
                  mutate(ct_inicio_ap=factor(ct_inicio_ap, 
                                             levels=orden_data_1_cd)),
                aes(label=round(Total, 2)), 
                position = position_stack(vjust = 1.1), size=4.5
      ) +
      tema_fgj +
      geom_label(x=length(unique(data_1_cd$ct_inicio_ap)), size=4.5, 
                 y=prom_territoriales_cd$Total*1.1, label=round(prom_territoriales_cd$Total,2), 
                 fill="#B09D83", color="ghostwhite"
      ) +
      scale_fill_manual(values = colores[7:10]) + tema_ppp +
      theme(legend.position = "none", 
            axis.text.x = element_text(size = 9, angle=90)) +
      labs(x="", y="", title = "Con detenido") 
    
    gr_1 <- cowplot::plot_grid(gr_sd, gr_cd, nrow = 2)
  }
  
  
  
  return(gr_1)
  
  
  
}



grafica_cargas_trabajo2 <- function(
    datos=iniciadas, 
    fiscalia_analisis=siglas_fiscalia_analisis,
    fecha_comienzo=inicio_quincena, 
    fecha_fin=final_quincena, 
    tipo = c("total", "promedio", "fiscalia_sin", "fiscalia_con", "agencia")
){
  data <- datos %>% 
    # filter(grepl(fiscalia_analisis, ct_inicio_ap)) %>% 
    mutate(fecha_inicio=as_date(fecha_de_inicio)) %>% 
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
      group_by(fiscalia, #detenido,
               fecha_inicio,
               fiscal
      ) %>%
      summarise(Total=n()) %>% 
      group_by(fiscalia, fiscal) %>% 
      summarise(Total=mean(Total))
    
    data_1_sd <- data %>%
      filter(quincena=="Actual",
             detenido=="Sin detenido") %>%
      group_by(fiscalia, #detenido,
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
      group_by(fiscalia, #detenido,
               fecha_inicio,
               fiscal
      ) %>%
      summarise(Total=n()) %>% 
      group_by(fiscalia, fiscal) %>% 
      summarise(Total=mean(Total))



    data_1_cd <- data %>%
      filter(quincena=="Actual",
             detenido=="Con detenido") %>%
      group_by(fiscalia, #detenido,
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
      theme(legend.position = "none") +
      labs(x="", y="", title = "Sin detenido") +
      tema_ppp

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
      group_by(fiscalia, #detenido,
               fecha_inicio,
               fiscal
      ) %>%
      summarise(Total=n()) %>% 
      group_by(fiscalia, fiscal) %>% 
      summarise(Total=mean(Total))
    
    data_1_sd <- data %>%
      filter(quincena=="Actual",
             detenido=="Sin detenido") %>%
      group_by(fiscalia, #detenido,
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
      group_by(fiscalia, #detenido,
               fecha_inicio,
               fiscal
      ) %>%
      summarise(Total=n()) %>% 
      group_by(fiscalia, fiscal) %>% 
      summarise(Total=mean(Total))
    
    
    
    data_1_cd <- data %>%
      filter(quincena=="Actual",
             detenido=="Con detenido") %>%
      group_by(fiscalia, #detenido,
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
        data=data_1_cd_mp %>%
          mutate(fiscalia=factor(fiscalia, 
                                 levels=orden_data_1_cd)),
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
                aes(label=round(Total, 2)), fontface="bold",
                position = position_stack(vjust = 1.1), size=6
      ) +
      scale_y_continuous(breaks = seq(0,14, 2), labels=seq(0,14, 2)) +
      tema_fgj +
      geom_label(x=length(unique(data_1_sd$fiscalia)), size=6.5,
                 y=prom_territoriales_cd$Total*1.1, label=round(prom_territoriales_cd$Total,2),
                 fill="#B09D83", color="ghostwhite"
      ) +
      scale_fill_manual(values = colores[7:10]) +
      theme(legend.position = "none") +
      labs(x="", y="", title = "Con detenido") +
      tema_ppp
    
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

# gr_gam <- grafica_cargas_trabajo(datos = iniciadas, 
#                                  fiscalia_analisis = "GAM", 
#                                  fecha_comienzo = "2024-10-01", 
#                                  fecha_fin = "2025-01-31", 
#                                  tipo = "mp"
#                                  )

fecha_comienzo<-as_date("2024-10-01")
fecha_fin<-as_date("2025-01-31")
#bases para la gráfica de cargas de trabajo2 (verisón nueva)
data <- juntas %>% 
  filter(grepl(fiscalias_territoriales, fiscalia),
         !grepl("CJM|URI|AOP", fiscalia), 
         fiscalia!="STCMH") %>% 
  mutate(fecha_inicio=as_date(fecha_de_inicio)) %>% 
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

#####

#agrupada por fiscalía versión nueva
data_1_sd <- data %>%
  filter(quincena=="Actual",
         detenido=="Sin detenido") %>%
  group_by(fiscalia, #detenido,
           fecha_inicio,
           fiscal
  ) %>%
  summarise(Total=n()) %>% ungroup() %>%
  group_by(fiscalia, fecha_inicio) %>%
  summarise(fiscales=n(),
            Total=sum(Total, na.rm = T)
  ) %>%
  mutate(media=Total/fiscales)

data_1_cd <- data %>%
  filter(quincena=="Actual",
         detenido=="Con detenido") %>%
  group_by(fiscalia, #detenido,
           fecha_inicio,
           fiscal
  ) %>%
  summarise(Total=n()) %>% ungroup() %>%
  group_by(fiscalia, fecha_inicio) %>%
  summarise(fiscales=n(),
            Total=sum(Total, na.rm = T)
  ) %>%
  mutate(media=Total/fiscales)


bind_rows(data_1_sd %>%mutate(detenido="Sin detenido"), data_1_cd%>%
            mutate(detenido="Con detenido"))  %>% 
  write.csv("H:/Mi unidad/Evaluacion_agencias/bases/data_graf_1_250225.csv",
            row.names = F)

#####
#agrupada por fiscalía versión vieja
#tablas para cargas de trabajo versión vieja
periodo <-  as.integer(as_date(fecha_fin)-as_date(fecha_comienzo))+1
data_1_sd <- data %>% 
  filter(quincena=="Actual", 
         detenido=="Sin detenido") %>% 
  group_by(fiscalia, #detenido, 
           # fecha_inicio, 
           fiscal, grupo
  ) %>% 
  summarise(Total=n()/periodo) 

data_1_cd <- data %>% 
  filter(quincena=="Actual", 
         detenido=="Con detenido")%>% 
  group_by(fiscalia, #detenido, 
           # fecha_inicio, 
           fiscal, grupo
  ) %>% 
  summarise(Total=n()/periodo) 

bind_rows(data_1_sd %>% 
            mutate(detenido="Sin detenido")
          , data_1_cd%>% 
            mutate(detenido="Con detenido")) %>% 
  write.csv("H:/Mi unidad/Evaluacion_agencias/bases/data_graf_1_250225_version_vieja.csv",
            row.names = F)

#####base para Marey#####
data_1_sd <- data %>%
  filter(quincena=="Actual",
         detenido=="Sin detenido") %>%
  group_by(ct_inicio_ap, #detenido,
           #fecha_inicio,
           fiscal
  ) %>%
  summarise(Total=n()) %>% ungroup() %>%
  group_by(ct_inicio_ap) %>%
  summarise(fiscales=n(),
            Total=sum(Total, na.rm = T)
  ) %>%
  mutate(media=Total/fiscales)

data_1_cd <- data %>%
  filter(quincena=="Actual",
         detenido=="Con detenido") %>%
  group_by(ct_inicio_ap, #detenido,
           #fecha_inicio,
           fiscal
  ) %>%
  summarise(Total=n()) %>% ungroup() %>%
  group_by(ct_inicio_ap) %>%
  summarise(fiscales=n(),
            Total=sum(Total, na.rm = T)
  ) %>%
  mutate(media=Total/fiscales)


bind_rows(data_1_sd %>%mutate(detenido="Sin detenido"), data_1_cd%>%
            mutate(detenido="Con detenido")) %>% 
  write.csv("base_agencias_mps_totales_fer.csv", row.names = F)

gr_fisc_sd <- grafica_cargas_trabajo(datos = juntas %>% 
                                 filter(grepl(fiscalias_territoriales, fiscalia),
                                        !grepl("CJM|URI|AOP", fiscalia), 
                                        fiscalia!="STCMH"), 
                                 #fiscalia_analisis = "GAM", 
                               fecha_comienzo = "2024-10-01", 
                               fecha_fin = "2025-01-31", 
                                 tipo = "mp"
)

ggsave(plot = gr_1, 
       "gr_1_250225_fiscalia.svg", width = 14, height = 6.5
)

#####
gr_fisc_sd <- grafica_cargas_trabajo2(datos = juntas %>% 
                                 filter(grepl(fiscalias_territoriales, fiscalia),
                                        !grepl("CJM|URI|AOP", fiscalia), 
                                        fiscalia!="STCMH"), 
                               #fiscalia_analisis = "GAM", 
                               fecha_comienzo = "2024-10-01", 
                               fecha_fin = "2025-01-15", 
                               tipo = "fiscalia_sin"
)

ggsave(plot = gr_fisc_sd, 
       "gr_1_260225_fiscalia_nueva_sd.svg", width = 14, height = 6.5
       )

gr_fisc_cd <- grafica_cargas_trabajo2(datos = juntas %>% 
                                        filter(grepl(fiscalias_territoriales, fiscalia),
                                               !grepl("CJM|URI|AOP", fiscalia), 
                                               fiscalia!="STCMH"), 
                                      #fiscalia_analisis = "GAM", 
                                      fecha_comienzo = "2024-10-01", 
                                      fecha_fin = "2025-01-15", 
                                      tipo = "fiscalia_con"
)

ggsave(plot = gr_fisc_cd, 
       "gr_1_260225_fiscalia_nueva_cd.svg", width = 14, height = 6.5
)



gr_1_nueva_agencia <- grafica_cargas_trabajo2(datos = juntas %>% 
                                        filter(grepl(fiscalias_territoriales, fiscalia),
                                               !grepl("CJM|URI|AOP", fiscalia), 
                                               fiscalia!="STCMH"), 
                                      #fiscalia_analisis = "GAM", 
                                      fecha_comienzo = "2024-10-01", 
                                      fecha_fin = "2025-01-15", 
                                      tipo = "agencia"
)

ggsave(plot = gr_1_nueva_agencia, 
       "gr_1_260225_agencia_cd_nueva.svg", width = 14, height = 6.5
)


total_iniciadas_quincena_agencia <-  juntas %>% 
  filter(grepl(fiscalias_territoriales, fiscalia),
         !grepl("CJM|URI|AOP", fiscalia), 
         fiscalia!="STCMH") %>% 
  mutate(fecha_inicio=as_date(fecha_de_inicio)) %>% 
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
  ) %>% 
  group_by(ct_inicio_ap, detenido) %>% 
  summarise(Total=n())

gr_agencia_vieja <- grafica_cargas_trabajo(datos = juntas %>% 
                                       filter(grepl(fiscalias_territoriales, fiscalia),
                                              !grepl("CJM|URI|AOP", fiscalia), 
                                              fiscalia!="STCMH"), 
                               #fiscalia_analisis = "GAM", 
                               fecha_comienzo = "2024-10-01", 
                               fecha_fin = "2025-01-31", 
                               tipo = "agencia"
)

ggsave(plot = gr_agencia_vieja, 
       "gr_1_agencia_250225_vieja.svg", width = 14, height = 6.5
)


gr_agencia_nueva <- grafica_cargas_trabajo2(datos = juntas %>% 
                                             filter(grepl(fiscalias_territoriales, fiscalia),
                                                    !grepl("CJM|URI|AOP", fiscalia), 
                                                    fiscalia!="STCMH"), 
                                           #fiscalia_analisis = "GAM", 
                                           fecha_comienzo = "2024-10-01", 
                                           fecha_fin = "2025-01-31", 
                                           tipo = "agencia"
)

ggsave(plot = gr_agencia_nueva, 
       "gr_1_agencia_250225_nueva.svg", width = 14, height = 6.5
)


ggsave(plot = gr_sd, 
       "gr_1_agencia_170225_sd.svg", width = 16, height = 7.5
)


ggsave(plot = gr_cd, 
       "gr_1_agencia_170225_cd.svg", width = 16, height = 7.5
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



library(wesanderson)

gr_libertad <- function(
    # fiscalia_analisis=siglas_fiscalia_analisis,
    fecha_comienzo=inicio_quincena, 
    fecha_fin=final_quincena
) {
  flagr <- flagrancias %>% 
    filter(#grepl(fiscalia_analisis, agencia_ini), 
           fecha_inicio>=fecha_comienzo, 
           fecha_inicio<=fecha_fin
    ) %>% 
    filter(subprocuraduria=="COORDINACIÓN GENERAL DE INVESTIGACIÓN TERRITORIAL", 
           siglas_fiscalia_inicio!="FIDAMPU"
           ) #%>% 
    # filter(res_puestas=="1. Libertad MP")
  
  # flagr_terri <-  flagrancias %>%
  #   filter( fecha_inicio>=fecha_comienzo, 
  #           fecha_inicio<=fecha_fin) %>% 
  #   filter(res_puestas=="1. Libertad MP", 
  #          !libertad_homologada %in% c("No se formuló imputación", "Acuerdo reparatorio", 
  #                                      "Pendientes por identificar")
  #          ) %>% 
  #   filter(!id_ap %in% flagr) %>% 
  #   mutate(res_puestas=case_when(
  #     libertad_homologada %in% c("Artículo 140 CNPP", "Personas mayores de 65 años")~ "Artículo 140", 
  #     libertad_homologada %in% c("Falta de querella",
  #     "Perdón de la víctima u ofendido") ~"Perdón y falta de querella", 
  #     libertad_homologada %in% "Criterio de oportunidad" ~"Criterio de oportunidad",
  #     T ~ "Artículo 149"
  #   )) %>% 
  #   # mutate(res_puestas=substr(res_puestas, 4, nchar(res_puestas))) %>% 
  #   group_by(res_puestas) %>% 
  #   summarise(Total=n()) %>% 
  #   mutate(Porcentaje=Total/sum(Total)) %>% ungroup() %>% 
  #   mutate(res_puestas=factor(res_puestas, levels = c("Artículo 140", "Perdón y falta de querella", 
  #                                                     "Criterio de oportunidad", "Artículo 149"
  #   ))) %>% 
  #   mutate(agencia_ini="Territoriales")
  
  nombres_fisc <- flagrancias %>% 
    filter(subprocuraduria=="COORDINACIÓN GENERAL DE INVESTIGACIÓN TERRITORIAL", 
           siglas_fiscalia_inicio!="FIDAMPU"
    ) %>% 
    filter(res_puestas=="1. Libertad MP") %>% 
    mutate(fiscalia=gsub("FI", "", siglas_fiscalia_inicio)) %>% 
    pull(fiscalia) %>% unique()
  
  libertades_importantes <- setdiff(unique(flagrancias$libertad_homologada), c("No se formuló imputación", "Acuerdo reparatorio", 
                                                                               "Pendientes por identificar", "Criterio de oportunidad", "0"))
  # no delito, no flagrancia, ilegal detencion, no vinculacion y vinculacion proceso
  
  data_der <- flagr %>% 
    mutate(criterio=case_when( 
           libertad_homologada %in% libertades_importantes ~ 1, 
           T ~ 0
           
    )
    ) %>% 
    mutate(
      legal=ifelse(judicializacion==1 &  legalidad_de_la_detencion==1, 1, 0),
      no_legal=ifelse(judicializacion==1 & legalidad_de_la_detencion==0, 1, 0), 
      vinculacion=case_when(
        legal==1 &
          vinculacion_a_proceso_por_persona==1 ~ 1,
        T ~0),
      no_vinculacion=case_when(
        legal==1 &
          vinculacion_a_proceso_por_persona==0 ~ 1,
        T ~0), 
      # prision_preventiva_justificada=sum(prision_preventiva_justificada, na.rm = T),
      # prision_preventiva_oficiosa=sum(prision_preventiva_oficiosa, na.rm = T),
      prision=case_when(
        prision_preventiva_justificada>0 ~ 1, 
        prision_preventiva_oficiosa>0 ~ 1, T ~0
      )
      ) %>% 
    mutate(
      res_puestas=case_when(
      # criterio==1 & libertad_homologada %in% c("Artículo 140 CNPP", "Personas mayores de 65 años")~ "Artículo 140", 
        # criterio==1 & libertad_homologada %in% c("Falta de querella", "Perdón de la víctima u ofendido",
        #                                          "Hechos no flagrantes", "No se cumplen los supuestos de la flagrancia",
        #                                          "Hechos no son constitutivos de delito", "No hay responsabilidad penal del imputado") ~"Libertad en MP",
        libertad==1 ~"Libertad en MP",
        # criterio==1 & libertad_homologada %in% c("Falta de querella") ~"Falta de querella", 
      # criterio==1 & libertad_homologada %in% "Perdón de la víctima u ofendido" ~ "Perdón",
      # criterio==1 & libertad_homologada %in% c("Hechos no flagrantes", "No se cumplen los supuestos de la flagrancia") ~ "No flagrancias", 
      # criterio==1 & libertad_homologada %in% c("Hechos no son constitutivos de delito", "No hay responsabilidad penal del imputado") ~ "No delito",
      no_legal==1 ~ "Ilegal detención",
      prision==1 ~ "Prisión preventiva",
      vinculacion==1 ~ "Vinculación", 
      no_vinculacion==1 ~ "No vinculación",
      # incompetencia==1 ~ "Incompetencia",
      # libertad_homologada %in% "Criterio de oportunidad" ~"Criterio de oportunidad",
      T ~ "Otros"
    )) %>% filter(res_puestas!="Otros") %>% 
    mutate(siglas_fiscalia_inicio=gsub("FI", "", siglas_fiscalia_inicio)) %>% 
    # mutate(res_puestas=substr(res_puestas, 4, nchar(res_puestas))) %>% 
    group_by(fiscalia=siglas_fiscalia_inicio, res_puestas, 
             criterio) %>% 
    summarise(Total=n(), .groups = "drop") %>% group_by(fiscalia) %>% 
    mutate(Porcentaje=Total/sum(Total)) %>% ungroup() %>% 
    # filter(criterio==1) %>% 
    filter(res_puestas!="Otros") %>% 
    complete(fiscalia=nombres_fisc, 
             res_puestas=unique(.$res_puestas), 
             fill = list(Total=0, Porcentaje=0, 
                         criterio=1)
    ) %>% 
    mutate(res_puestas=factor(res_puestas, levels = c("Libertad en MP","Ilegal detención", 
                                                      "No vinculación", "Vinculación", 
                                                      "Prisión preventiva"
    ))) 
    # bind_rows(flagr_terri) %>% 
    
  #orden a partir de 140 y sucesivo
  # orden_fisc <- data_der %>%
  #   select(-Total) %>%
  #   # filter(res_puestas=="Artículo 140") %>%
  #   spread(res_puestas, Porcentaje, fill=0) %>%
  #   # complete(fiscalia=nombres_fisc,
  #   #          fill = list(Total=0, Porcentaje=0)
  #   # ) %>%
  #   arrange(desc(`Artículo 149`), desc(`Artículo 140`), desc(Perdón), desc(`Falta de querella`)
  #           ) %>%
  #   pull(fiscalia) %>%
  #   unique()
  
  orden_fisc <- flagr %>% 
    mutate(siglas_fiscalia_inicio=gsub("FI", "", siglas_fiscalia_inicio)) %>% 
    group_by(siglas_fiscalia_inicio) %>% 
    summarise(Total=n()) %>% arrange(-Total) %>% 
    pull(siglas_fiscalia_inicio)

  # alcaldias_faltantes <- setdiff(nombres_fisc, orden_fisc)
  # 
  # alcaldias_completas <- c(orden_fisc, alcaldias_faltantes)
    
  library(forcats)
  gr_det <- data_der %>%
    mutate(fiscalia=factor(fiscalia, levels=rev(orden_fisc))) %>%
    ggplot(aes(fiscalia, Porcentaje, fill=fct_rev(res_puestas), 
               # group=fct_rev(res_puestas)
               )) +
    geom_col() + theme_light() + tema_fgj + tema_ppp +
    coord_flip() +
    geom_text_repel(data=. %>%
                filter(Total>0),
                aes(label=paste0(percent(Porcentaje, .1), " (", comma(Total), ")")),
              # color="black",
              color="white",
              size=6, fontface="bold", max.time = 1.5,
              position = position_stack(vjust = 0.7), direction = "x"
    ) +
    scale_y_continuous(labels = percent) +
    labs(x="Fiscalía de inicio", y="Total de personas",
         fill=""
    ) +
    theme(legend.position = "bottom")+
    scale_fill_manual(values = rev(c( "#929292", "#caeefb", "#a6caec", "#d9d9d9", 
                                               "#000000", 
                                               "#a9d18e", colores[7]  )), 
                      guide = guide_legend(reverse = TRUE)
                      )

  return(gr_det)
}

gr_libertades <- gr_libertad(#fiscalia_analisis = "GAM", 
  fecha_comienzo = "2025-01-16", 
  fecha_fin = "2025-02-15"
)

ggsave(plot = gr_libertades, 
       "gr_puestas_dispocion_mes_260225.svg", width = 16, height = 6
)


gr_libertades_hist <- gr_libertad(#fiscalia_analisis = "GAM", 
  fecha_comienzo = "2024-01-01", 
  fecha_fin = "2025-01-15"
)

ggsave(plot = gr_libertades_hist, 
       "gr_puestas_dispocion_hist_260225_blanco.svg", width = 16, height = 6
)


#####función para lbiertades por 140
gr_libertad_140 <- function(
    # fiscalia_analisis=siglas_fiscalia_analisis,
  fecha_comienzo=inicio_quincena, 
  fecha_fin=final_quincena
) {
  flagr <- flagrancias %>% 
    filter(#grepl(fiscalia_analisis, agencia_ini), 
      fecha_inicio>=fecha_comienzo, 
      fecha_inicio<=fecha_fin
    ) %>% 
    filter(subprocuraduria=="COORDINACIÓN GENERAL DE INVESTIGACIÓN TERRITORIAL", 
           siglas_fiscalia_inicio!="FIDAMPU"
    ) #%>% 

  
  nombres_fisc <- flagrancias %>% 
    filter(subprocuraduria=="COORDINACIÓN GENERAL DE INVESTIGACIÓN TERRITORIAL", 
           siglas_fiscalia_inicio!="FIDAMPU"
    ) %>% 
    filter(res_puestas=="1. Libertad MP") %>% 
    mutate(fiscalia=gsub("FI", "", siglas_fiscalia_inicio)) %>% 
    pull(fiscalia) %>% unique()
  
  libertades_importantes <- setdiff(unique(flagrancias$libertad_homologada), c("No se formuló imputación", "Acuerdo reparatorio", 
                                                                               "Pendientes por identificar", "Criterio de oportunidad", "0"))
  # no delito, no flagrancia, ilegal detencion, no vinculacion y vinculacion proceso
  
  data_der <- flagr %>% 
    filter(incompetencia!=1) %>% 
    filter(!grepl("revis", str_to_lower(observaciones_dgpec_2))) %>% 
    mutate(tipo=case_when(
      libertad_homologada %in% c("Artículo 140 CNPP", "Personas mayores de 65 años") ~"Artículo 140 CNPP", 
      T ~ "Otros"
    )) %>% 
    mutate(siglas_fiscalia_inicio=gsub("FI", "", siglas_fiscalia_inicio)) %>% 
    # mutate(res_puestas=substr(res_puestas, 4, nchar(res_puestas))) %>% 
    group_by(fiscalia=siglas_fiscalia_inicio, tipo) %>% 
    summarise(Total=n(), .groups = "drop") %>% group_by(fiscalia) %>% 
    mutate(Porcentaje=Total/sum(Total)) %>% 
    filter(tipo!="Otros") %>% ungroup() %>% 
    complete(fiscalia=nombres_fisc, 
             fill=list(Total=0, Porcentaje=0)
             )
  
  orden_fisc <- flagr %>% 
    mutate(siglas_fiscalia_inicio=gsub("FI", "", siglas_fiscalia_inicio)) %>% 
    group_by(fiscalia=siglas_fiscalia_inicio) %>% 
    summarise(Total=n()) %>% ungroup() %>% 
    complete(fiscalia=nombres_fisc, 
             fill=list(Total=0, Porcentaje=0)
    ) %>% arrange(-Total) %>% 
    pull(fiscalia)
  
  # alcaldias_faltantes <- setdiff(nombres_fisc, orden_fisc)
  # 
  # alcaldias_completas <- c(orden_fisc, alcaldias_faltantes)
  
  library(forcats)
  gr_det <- data_der %>%
    mutate(fiscalia=factor(fiscalia, levels=rev(orden_fisc))) %>%
    ggplot(aes(fiscalia, Porcentaje, #fill=fct_rev(res_puestas), 
               # group=fct_rev(res_puestas)
    )) +
    geom_col(fill=colores[1], alpha=.8) + theme_light() + tema_fgj + tema_ppp +
    coord_flip() +
    geom_text_repel(data=. %>%
                      filter(Total>0),
                    aes(label=paste0(percent(Porcentaje, .1), " (", comma(Total), ")")),
                    color="black",
                    # color="white",
                    size=6, fontface="bold", max.time = 1.5,
                    position = position_stack(vjust = 0.7), direction = "x"
    ) +
    scale_y_continuous(labels = percent) +
    labs(x="Fiscalía de inicio", y="Total de personas",
         fill=""
    ) +
    theme(legend.position = "bottom")
  
  return(gr_det)
}


gr_libertades_140 <- gr_libertad_140(#fiscalia_analisis = "GAM", 
  fecha_comienzo = "2025-01-16", 
  fecha_fin = "2025-02-15"
)

ggsave(plot = gr_libertades_140, 
       "gr_libertad_mes_260225_140.svg", width = 16, height = 6
)


gr_libertades_hist_140 <- gr_libertad_140(#fiscalia_analisis = "GAM", 
  fecha_comienzo = "2024-01-01", 
  fecha_fin = "2025-01-15"
)

ggsave(plot = gr_libertades_hist_140, 
       "gr_libertad_quincena_hist_260225_140.svg", width = 16, height = 6
)


#sacmos tabla para complementar gráfica
flagr <- flagrancias %>% 
  filter(#grepl(fiscalia_analisis, agencia_ini), 
    fecha_inicio>="2024-01-01", 
    fecha_inicio<="2025-02-15"
  ) %>% 
  filter(subprocuraduria=="COORDINACIÓN GENERAL DE INVESTIGACIÓN TERRITORIAL", 
         siglas_fiscalia_inicio!="FIDAMPU"
  )

flagr %>% 
  group_by(fecha_inicio, 
           fiscalia=siglas_fiscalia_inicio
           ) %>% 
  summarise(Puestas=n(), 
            libertades=sum(libertad, na.rm = T)
            ) %>% 
  write.csv("H:/Mi unidad/Evaluacion_agencias/bases/data_puestas_libertades_250225.csv",
            row.names = F)


gr_libertad_demas <- function(
    # fiscalia_analisis=siglas_fiscalia_analisis,
  fecha_comienzo=inicio_quincena, 
  fecha_fin=final_quincena
) {
  
  flagr <- flagrancias %>% 
    filter(#grepl(fiscalia_analisis, agencia_ini), 
      fecha_inicio>=fecha_comienzo, 
      fecha_inicio<=fecha_fin
    ) %>% 
    filter(subprocuraduria=="COORDINACIÓN GENERAL DE INVESTIGACIÓN TERRITORIAL", 
           siglas_fiscalia_inicio!="FIDAMPU"
    ) #%>% 
  # filter(res_puestas=="1. Libertad MP")
  
  
  nombres_fisc <- flagrancias %>% 
    filter(subprocuraduria=="COORDINACIÓN GENERAL DE INVESTIGACIÓN TERRITORIAL", 
           siglas_fiscalia_inicio!="FIDAMPU"
    ) %>% 
    filter(res_puestas=="1. Libertad MP") %>% 
    mutate(fiscalia=gsub("FI", "", siglas_fiscalia_inicio)) %>% 
    pull(fiscalia) %>% unique()
  
  libertades_importantes <- setdiff(unique(flagrancias$libertad_homologada), c("No se formuló imputación", "Acuerdo reparatorio", 
                                                                               "Artículo 140 CNPP", "Personas mayores de 65 años",
                                                                               "Pendientes por identificar", "Criterio de oportunidad", "0"))
  
  data_der <- flagr %>% 
    mutate(criterio=case_when( 
      libertad_homologada %in% libertades_importantes ~ 1, 
      T ~ 0
      
    )
    ) %>% 
    # filter(!id_ap %in% flagr) %>% 
    mutate(res_puestas=case_when(
      # criterio==1 & libertad_homologada %in% c("Artículo 140 CNPP", "Personas mayores de 65 años")~ "Artículo 140", 
      criterio==1 & libertad_homologada %in% c("Falta de querella") ~"Falta de querella", 
      criterio==1 & libertad_homologada %in% "Perdón de la víctima u ofendido" ~ "Perdón",
      criterio==1 & libertad_homologada %in% c("Hechos no flagrantes", "No se cumplen los supuestos de la flagrancia") ~ "No flagrancias", 
      criterio==1 & libertad_homologada %in% c("Hechos no son constitutivos de delito", "No hay responsabilidad penal del imputado") ~ "No delito",
      # libertad_homologada %in% "Criterio de oportunidad" ~"Criterio de oportunidad",
      T ~ "Otros"
    )) %>% 
    mutate(siglas_fiscalia_inicio=gsub("FI", "", siglas_fiscalia_inicio)) %>% 
    # mutate(res_puestas=substr(res_puestas, 4, nchar(res_puestas))) %>% 
    group_by(fiscalia=siglas_fiscalia_inicio, res_puestas, 
             criterio) %>% 
    summarise(Total=n(), .groups = "drop") %>% group_by(fiscalia) %>% 
    mutate(Porcentaje=Total/sum(Total)) %>% ungroup() %>% 
    filter(criterio==1) %>% 
    complete(fiscalia=nombres_fisc, 
             res_puestas=unique(.$res_puestas), 
             fill = list(Total=0, Porcentaje=0, 
                         criterio=1)
    ) %>% 
    mutate(res_puestas=factor(res_puestas, levels = c("No delito", "No flagrancias", 
                                                      "Perdón", "Falta de querella"
    ))) 
  
  orden <- flagr %>% 
    group_by(fiscalia=siglas_fiscalia_inicio) %>% 
    summarise(Total=n()) %>% ungroup() %>% 
    complete(fiscalia=c(.$fiscalia)) %>% 
    arrange(-Total) %>% 
    mutate(fiscalia=gsub("FI", "", fiscalia)) %>% 
    pull(fiscalia)
  
  library(forcats)
  gr_det <- data_der %>%
    mutate(fiscalia=factor(fiscalia, levels=rev(orden))) %>%
    ggplot(aes(fiscalia, Porcentaje, fill=fct_rev(res_puestas), 
               # group=fct_rev(res_puestas)
    )) +
    geom_col() + theme_light() + tema_fgj + tema_ppp +
    coord_flip() +
    geom_text_repel(data=. %>%
                      filter(Total>0),
                    aes(label=paste0(percent(Porcentaje, .1), " (", comma(Total), ")")),
                    color="black", size=6, fontface="bold", max.time = 1.5,
                    position = position_stack(vjust = 0.7), direction = "x"
    ) +
    scale_y_continuous(labels = percent) +
    labs(x="Fiscalía de inicio", y="Total de personas",
         fill=""
    ) +
    theme(legend.position = "bottom")+
    scale_fill_manual(values = rev(c( "#5D818B", "#59595C", "#A7A8AC", "#FDE4CB")), 
                      guide = guide_legend(reverse = TRUE)
    )
  
  return(gr_det)
}

gr_libertades_demas <- gr_libertad_demas(#fiscalia_analisis = "GAM", 
  fecha_comienzo = "2025-01-16", 
  fecha_fin = "2025-02-15"
)

ggsave(plot = gr_libertades_demas, 
       "gr_libertad_mes_260225_otras_libertades.svg", width = 16, height = 6
)


gr_libertades_hist_demas <- gr_libertad_demas(#fiscalia_analisis = "GAM", 
  fecha_comienzo = "2024-01-01", 
  fecha_fin = "2025-01-15"
)

ggsave(plot = gr_libertades_hist_demas, 
       "gr_libertad_quincena_hist_260225_otras_libertades.svg", width = 16, height = 6
)

gr_judicializacion <- function(
    # fiscalia_analisis=siglas_fiscalia_analisis,
  fecha_comienzo=inicio_quincena, 
  fecha_fin=final_quincena
) {
  
  data_9 <- flagrancias %>% 
    filter(
      fecha_inicio>=fecha_comienzo, 
      fecha_inicio<=fecha_fin, 
      subprocuraduria=="COORDINACIÓN GENERAL DE INVESTIGACIÓN TERRITORIAL",
      siglas_fiscalia_inicio!="FIDAMPU",
      #!(delito %in% cat_masc$modalidad_delito)
      
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
    group_by(#fecha_inicio=floor_date(fecha_inicio, "1 month"),
             fiscalia=siglas_fiscalia_inicio) %>% 
    summarise(
      puestas=n(),
      legal=sum(legal, na.rm = T), 
      "no legal"=sum(no_legal, na.rm = T), 
      
      vinculacion=sum(vinculacion, na.rm = T),
      "no vinculacion"=sum(no_vinculacion, na.rm = T)
    ) %>% 
    mutate_at(vars(contains("legal"), contains("vinculacion")), 
              list(absolutos = ~ . / 1 )) %>%
    # select(fiscalia, puestas, legal_absolutos:`no vinculacion_absolutos`) %>% 
    mutate_at(vars(contains("legal"), contains("vinculacion")), 
              list(porcentaje = ~ . / puestas )) %>% 
    select(fiscalia, legal_absolutos:`no vinculacion_absolutos_porcentaje`) %>% 
    # rename_with(~ str_replace(., ".*_porcentaje", "porcentaje"), contains("absolutos_porcentaje")) %>% 
    select(-contains("absolutos_porcentaje")) %>% 
    pivot_longer(
      cols = -fiscalia,  # Excluir la columna "puestas"
      names_to = c("tipo", "medida"),  # Separar en "tipo" y "medida"
      names_sep = "_",  # Separar los nombres por el guion bajo
      values_to = "Total"  # Nombre de la columna de valores
    ) %>% 
    mutate(fiscalia=gsub("FI", "", fiscalia)) %>% 
    filter(tipo!="legal")
  
 
  
  
  orden_fisc <- data_9 %>%
    filter(medida=="absolutos") %>%
    group_by(fiscalia) %>%
    summarise(Total=sum(Total)) %>%
    arrange(-Total) %>% pull(fiscalia)
  
  # alcaldias_faltantes <- setdiff(nombres_fisc, orden_fisc)
  # 
  # alcaldias_completas <- c(orden_fisc, alcaldias_faltantes)
  
  # orden_fisc <- flagrancias %>% 
  #   filter(
  #     fecha_inicio>=fecha_comienzo, 
  #     fecha_inicio<=fecha_fin, 
  #     subprocuraduria=="COORDINACIÓN GENERAL DE INVESTIGACIÓN TERRITORIAL",
  #     siglas_fiscalia_inicio!="FIDAMPU",
  #     #!(delito %in% cat_masc$modalidad_delito)
  #     
  #   ) %>% 
  #   mutate(fiscalia=gsub("FI", "", siglas_fiscalia_inicio)) %>% 
  #   pull(fiscalia) %>% unique()
  
  # orden_fisc <- flagrancias %>% 
  #   filter(
  #     fecha_inicio>=fecha_comienzo, 
  #     fecha_inicio<=fecha_fin, 
  #     subprocuraduria=="COORDINACIÓN GENERAL DE INVESTIGACIÓN TERRITORIAL",
  #     siglas_fiscalia_inicio!="FIDAMPU",
  #     #!(delito %in% cat_masc$modalidad_delito)
  #     
  #   ) %>% 
  #   mutate(fiscalia=gsub("FI", "", siglas_fiscalia_inicio)) %>% 
  #   pull(fiscalia) %>% unique() %>% sort()
    
  orden_fisc <- data_9 %>% 
    filter(tipo=="no legal", 
           medida=="porcentaje"
           ) %>% arrange(-Total) %>% pull(fiscalia)
  
  gr_det <- data_9 %>%
    spread(medida, Total, fill=0) %>% 
    mutate(fiscalia=factor(fiscalia, levels=rev(orden_fisc)), 
           tipo=factor(tipo, levels=c("no legal", "no vinculacion", "vinculacion"))
           ) %>%
    ggplot(aes(fiscalia, porcentaje, fill=fct_rev(tipo), 
               # group=fct_rev(res_puestas)
    )) +
    geom_col() + theme_light() + tema_fgj + tema_ppp +
    coord_flip() +
    geom_text_repel(data=. %>%
                      filter(absolutos>0),
                    aes(label=paste0(percent(porcentaje, .1), " (", comma(absolutos), ")")),
                    color="black", size=6, fontface="bold", max.time = 1.5,
                    position = position_stack(vjust = 0.7), direction = "x"
    ) +
    scale_y_continuous(labels = percent) +
    labs(x="Fiscalía de inicio", y="Total de personas",
         fill=""
    ) +
    theme(legend.position = "bottom")+
    scale_fill_manual(values = rev(c( "#2E5C81", "#59595C", "#A7A8AC")), 
                      guide = guide_legend(reverse = TRUE)
    )
  
  return(gr_det)
}



gr_judi_quincena <- gr_judicializacion(#fiscalia_analisis = "GAM", 
  fecha_comienzo = "2025-01-15", 
  fecha_fin = "2025-02-15"
)

ggsave(plot = gr_judi_quincena, 
       "gr_judi_mes_250225.svg", width = 16, height = 7
)


gr_judi_hist <- gr_judicializacion(#fiscalia_analisis = "GAM", 
  fecha_comienzo = "2024-01-01", 
  fecha_fin = "2025-02-15"
)

ggsave(plot = gr_judi_hist, 
       "gr_judi_hist_250225.svg", width = 16, height = 7
)

#bases para entregar
flagr %>% 
  group_by(fecha_inicio, 
           fiscalia=siglas_fiscalia_inicio
  ) %>% 
  summarise(Puestas=n(), 
            judicializacion=sum(judicializacion, na.rm = T)
  ) %>% 
  write.csv("H:/Mi unidad/Evaluacion_agencias/bases/data_puestas_judicializacion_250225.csv",
            row.names = F)


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
  
  
heatmap_agrup <- function(
    base = base_use, 
    fiscalia_analisis=siglas_fiscalia,
    fecha_comienzo = fecha_comienzo, 
    nombre = str_to_lower(nombre_plots),
    fecha_lim = fecha_lim
) {
  
  horarios <- c("De 00:00 a 03:59",
                "De 04:00 a 07:59",
                "De 08:00 a 11:59",
                "De 12:00 a 15:59",
                "De 16:00 a 19:59",
                "De 20:00 a 23:59")
  
  dias <- c("domingo", "lunes", "martes", "miércoles", "jueves", "viernes", "sábado")
  
  data_del <- 
    base %>% 
    filter(grepl(fiscalia_analisis, ct_inicio_ap)) %>% 
    mutate(fecha_inicio=dmy(fecha_de_inicio)) %>% 
    rename(fecha_hechos=fecha_inicio, 
           hora_de_los_hechos=hora_de_inicio
           ) %>% 
    drop_na(fecha_hechos) %>% 
    filter(fecha_hechos >= as_date(fecha_comienzo)) %>% 
    mutate(
      dia_sem = wday(fecha_hechos, label = T, abbr = F),
      # hora_de_los_hechos = as.numeric(hora_de_los_hechos),
      # hora = gsub(":\\d{2}", "", hora_de_los_hechos),
       hora = as.numeric(gsub(":\\d{2}", "", hora_de_los_hechos)),
      # hora=hora_de_los_hechos,
      grupo_hora = case_when(
        hora >= 0 & hora < 4 ~ horarios[1],
        hora >= 4 & hora < 8 ~ horarios[2],
        hora >= 8 & hora < 12 ~ horarios[3],
        hora >= 12 & hora < 16 ~ horarios[4],
        hora >= 16 & hora < 20 ~ horarios[5],
        hora >= 20 ~ horarios[6]
      )) %>% 
    group_by(dia_sem, grupo_hora) %>% 
    summarise(tot = n(),
              .groups = "drop") %>% 
    complete(grupo_hora = horarios, 
             dia_sem = dias, 
             fill = list(tot = 0)) %>% 
    pivot_wider(names_from = dia_sem, values_from = tot) %>% 
    adorn_totals() %>% 
    adorn_totals(where = "col") %>% 
    pivot_longer(cols = -grupo_hora, names_to = "dia_sem", values_to = "tot") %>% 
    mutate(orden = case_when(
      dia_sem == "lunes" ~ 1,
      dia_sem == "martes" ~ 2,
      dia_sem == "miércoles" ~ 3,
      dia_sem == "jueves" ~ 4,
      dia_sem == "viernes" ~ 5,
      dia_sem == "sábado" ~ 6,
      dia_sem == "domingo" ~ 7,
      dia_sem == "Total" ~ 8),
      tot_2 = ifelse(grupo_hora == "Total" | dia_sem == "Total", NA, tot),
      grupo_hora = factor(grupo_hora, levels = c(horarios, "Total"))
      # hora = ifelse(hora == "Total", 24, as.numeric(hora)),
      # hora_show = ifelse(hora < 10,
      #                    paste0("0", hora, ":00"),
      #                    paste0(hora, ":00")),
      # hora_show = ifelse(hora == 24, "Total", hora_show),
    ) %>% 
    arrange(grupo_hora)
  
  total <- 
    data_del %>%
    filter(grupo_hora == "Total" & dia_sem == "Total") %>% 
    pull(tot)
  
  porcentajes <- 
    data_del %>%
    filter(is.na(tot_2)) %>% 
    filter(!(grupo_hora == "Total" & dia_sem == "Total")) %>% 
    mutate(prc = tot/total)
  
  
  data_del <- 
    data_del %>% 
    left_join(porcentajes) %>% 
    mutate(texto = ifelse(!is.na(prc), paste0(comma(tot, accuracy = 1),
                                              "\n(", percent(prc, accuracy = .01), ")"), tot)) %>% 
    drop_na(grupo_hora)
  
  ggplot(data_del, 
         aes(x = reorder(dia_sem, orden), 
             y = rev(grupo_hora), 
             fill = tot_2)) +
    geom_tile(alpha = .5) +
    geom_vline(xintercept = 7.5) +
    geom_hline(yintercept = 1.5) +
    geom_text(aes(label = texto, color = tot_2), fontface = "bold") +
    labs(title = str_wrap(paste0("Distribución de horarios de hechos - ", nombre), 60),
         subtitle = paste0("Carpetas iniciadas del ", format(as_date(fecha_comienzo), "%d de %B de %Y"), " al ",
                           format(as_date(fecha_lim), "%d de %B de %Y")),
         x = "Día de la semana de hechos", 
         y = "Hora de hechos") +
    scale_fill_gradient(
      low = "#00b140",
      # mid = "#fdc10e",
      high = "#ff4f4f",
      # midpoint = mean(tot_2, na.rm = T),
      na.value = "white",
    ) +
    scale_y_discrete(labels = rev(c(horarios, "Total"))) +
    scale_color_gradient(
      low = "#00b140",
      # mid = "#fdc10e",
      high = "#ff4f4f",
      # midpoint = mean(tot_2, na.rm = T),
      na.value = "gray30",
    ) +
    theme_void() +
    theme(
      legend.position = "none",
      axis.title.x = element_text(),
      axis.title.y = element_text(angle = 90),
      axis.text = element_text(),
      plot.title = element_text(face = "bold")
    )
}  
  
heatmap_agrup(
    base = iniciadas, 
    fiscalia_analisis="GAM",
    fecha_comienzo = "2025-01-15", 
    nombre = str_to_lower("carpetas iniciadas"),
    fecha_lim = "2025-01-31"
)


##### datos de incompetencia internas ####
cat_esp  <- read_excel("Catalogo_Especializadas.xlsx") %>% 
  filter(Especializada==1)


data_co <- flagrancias %>% 
  filter(libertad_homologada=="Criterio de oportunidad", 
         fecha_inicio>="2020-01-01", 
         fecha_inicio<="2025-02-15", 
         subprocuraduria=="COORDINACIÓN GENERAL DE INVESTIGACIÓN TERRITORIAL") %>% 
  pull(id_ap)

fiscalias_territoriales <- paste(sep = "|",  "AZ", "BJ", "IZP", "IZC", "MH", "MIL",
                                 "CUJ", "COY", "CUH", "GAM", "MC", "TLH", "TLP", "VC", "XO", "AO")



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

id_libertad <- flagrancias %>% 
  filter(libertad==1) %>% pull(id_ap)


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


data_estatus <- data_estatus %>% 
  mutate(delito_especializada=case_when(
    modalidad_delito %in% cat_esp$modalidad_delito ~ 1, 
    T ~ 0
  ))

data_estatus_libertad <- data_estatus %>% 
  mutate(libertad=ifelse(id_ap %in% id_libertad, 1, 0)) %>% 
  left_join(flagrancias %>% filter(libertad==1) %>% 
              mutate(libertad_homologada=factor(libertad_homologada, 
                                                c(sort(unique(flagrancias$libertad_homologada))[7], 
                                                  sort(unique(flagrancias$libertad_homologada))[8], 
                                                  sort(unique(flagrancias$libertad_homologada))[2:6],
                                                  sort(unique(flagrancias$libertad_homologada))[9:13], 
                                                  sort(unique(flagrancias$libertad_homologada))[1])
              )) %>% 
              arrange(libertad_homologada) %>% 
              filter(!duplicated(id_ap)) %>% select(id_ap, libertad_homologada)
  ) %>% 
  filter(estatus=="Rezago")

data_estatus_libertad %>% 
  write.csv("C:/Users/mauri/OneDrive/Documentos/R/fiscalia/Evaluaciones_agencias/data_estatus_milei.csv", 
            row.names = F
            )


#####radicaciones####
base_radicaciones <- read.csv("H:/Mi unidad/bases/ini_radicaciones.csv", fileEncoding = "UTF-8"
                              )

####datos para emiliano 

data <- base %>% 
  filter(year(fecha_inicio)>=2024) %>% 
  left_join(iniciadas %>% 
              select(id_ap, fiscalia, grupo, "id_mp"=fiscalia), by="id_ap"
              ) 

#metemos campap

campap <- read_rds("campap.rds")

data <- data %>% 
  mutate(campap=ifelse(id_ap %in% campap$id_ap, 1, 0)) %>% 
  left_join(campap %>% 
              select(-ctrluinv)
              )

library(openxlsx)

wb <- createWorkbook()

flagr <- flagrancias %>% 
  filter(fecha_inicio>="2024-01-01")

#hoja 1
addWorksheet(wb, "incidencia")
writeData(wb, "incidencia", data)
#hoja 2
addWorksheet(wb, "flagrancias")
writeData(wb, "flagrancias", flagr)
#

saveWorkbook(wb, "datos_evaluacion_agencias.xlsx", overwrite = T)

#grafica 3



