####Script de evaluación de agencias####
setwd("~/R/fiscalia/Evaluaciones_agencias")
iniciadas_uet <- read_rds()
iniciadas <- readxl::read_excel("Iniciadas por Territoriales.xlsx") %>% clean_names()

iniciadas <- iniciadas %>% 
  mutate(ct_inicio_ap=str_remove(ct_inicio_ap, "UAT-"))

#filtramos por fiscalía territorial
data <- iniciadas %>% 
  filter(grepl("IZP", ct_inicio_ap)) %>% 
  mutate(fecha_inicio=dmy(fecha_de_inicio)) %>% 
  filter(fecha_inicio>="2024-12-16", 
         fecha_inicio<="2025-01-15"
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
  complete(fecha_inicio=seq.Date(as_date("2025-01-01"),
                                 as_date("2025-01-15"), 
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
  complete(fecha_inicio=seq.Date(as_date("2025-01-01"),
                                 as_date("2025-01-15"), 
                                 "1 day"
  ), 
  #detenido=c("Con detenido", "Sin detenido"), 
  ct_inicio_ap=unique(.$ct_inicio_ap), 
  fill=list(Total=0)
  
  )  %>% 
  group_by(ct_inicio_ap) %>% 
  summarise(media=round(mean(Total), 2)) %>% 
  mutate(detenido="Con detenido", 
         # ct_inicio_ap=paste0(ct_inicio_ap, "\nCD")
         )

prom_territoriales <- iniciadas %>% 
  mutate(fecha_inicio=dmy(fecha_de_inicio)) %>% 
  filter(fecha_inicio>="2025-01-01", 
         fecha_inicio<="2025-01-15"
  ) %>% 
  filter(!id_ap %in% data$id_ap ) %>% 
  group_by(ct_inicio_ap) %>% 
  summarise(Total=n()) %>% ungroup() %>%  
  complete(
  #   fecha_inicio=seq.Date(as_date("2025-01-01"),
  #                                as_date("2025-01-15"), 
  #                                "1 day"
  # ), 
  #detenido=c("Con detenido", "Sin detenido"), 
  ct_inicio_ap=unique(.$ct_inicio_ap), 
  fill=list(Total=0)
  
  )  %>% 
  # group_by(ct_inicio_ap) %>% 
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
  geom_vline(aes(xintercept=10.5), 
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
  
  
  
  

