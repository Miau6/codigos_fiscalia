setwd("~/R/fiscalia/analisis_fiscalia/XO")
base %>% 
  filter(grepl("CUH", agencia_ini), 
         year(fecha_inicio)>2022) %>% drop_na(id_ap) %>% 
  mutate(detenido=case_when(
    id_ap %in% flagrancias$id_ap ~ "Con detenido", 
    T ~ "Sin detenido"
  )) %>% 
  group_by(año=year(fecha_inicio), agencia_ini, detenido) %>% 
  summarise(Total=n()) %>% spread(año, Total, fill = 0)


#carpetas remitidas

remi_siap <- readxl::read_excel("7- SIAP - Iniciadas en Cuauhtémoc y primera radicacion efectiva fuera de CUAUHTEMOC.xlsx" )
remi_siap$auxfiscal <- as.character(remi_siap$auxfiscal)
remi_fsiap <- readxl::read_excel("8- FSIAP - Iniciadas en Cuauhtémoc y primera radicacion efectiva fuera de CUAUHTEMOC.xlsx")

get_ci_rara <- function(base) {
  base <- 
    base %>% 
    mutate(
      ci = gsub("CI-|CI-E-", "", ci_formato_siap_fsiap),
      fiscalia_ini = str_split(ci, "/", simplify = T)[,1],
      agencia_ini = str_split(ci, "/", simplify = T)[,2],
      ultima =str_split(ci, "/", simplify = T)[,6],
      rara=ifelse(grepl("R|D", ultima), 1, 0)
    )
  
  return(base)
}

total_remi <- bind_rows(remi_siap, remi_fsiap)
total_remi <- total_remi %>% 
  rename(ci_formato_siap_fsiap=carpetainv) %>% 
  get_ci_rara()

total_remi <- total_remi %>% 
  filter(rara==0)

#modificamos la fecha de remision
total_remi <- total_remi %>%
  mutate(fecharemision=as_date(fecharemision))

#### ultima radicación 

radi_siap <- readxl::read_excel("1- SIAP - Ultima remisión efectiva en Cuauhtémoc.xlsx"   )
radi_siap$auxfiscal <- as.character(radi_siap$auxfiscal)
radi_fsiap <- readxl::read_excel("2- FSIAP - Ultima remisión efectiva en Cuauhtémoc.xlsx"  )
radi_fsiap <- radi_fsiap %>% 
  rename(IdCI=idCI)


total_radi <- bind_rows(radi_siap, radi_fsiap) %>% 
  rename(ci_formato_siap_fsiap=carpetainv)

get_fiscalia <- function(base) {
  base <- 
    base %>% 
    mutate(
      ci = gsub("CI-|CI-E-", "", ci_formato_siap_fsiap),
      fiscalia_ini = str_split(ci, "/", simplify = T)[,1],
      agencia_ini = str_split(ci, "/", simplify = T)[,2]
    )
  
  return(base)
}

total_radi <- total_radi %>% get_fiscalia()



total_radi <- total_radi %>% get_ci_rara()
total_radi <- total_radi %>% filter(rara==0)

#función para sacar bases

#####

bases_fiscalias_estatus <- function(
  agencia_principal=siglas_agencia,
  fiscalia_principal=siglas_fiscalia
){
  #obtenemos los ids de las remisiones de las agencia que nos interesa 
  #debe de hacer un base que se llame total_remi y se le debió de dar tratamiento previo
  id_remi <- total_remi %>% 
    filter(agencia_ini==agencia_principal) %>% 
    filter(!duplicated(idCI)) %>% drop_na(idCI) %>% 
    filter(idCI %in% base$id_ap) %>% 
    pull(idCI)
  #debe de hacer un base que se llame total_radi y se le debió de dar tratamiento previo
  #sacamos los ids de las radicaciones
  id_rad <- total_radi %>% filter(agenciadestino==agencia_principal) %>% 
    filter(!duplicated(IdCI)) %>% drop_na(IdCI) %>% 
    filter(IdCI %in% base$id_ap) %>% 
    filter(agencia_ini!=agencia_principal) %>% pull(IdCI)
  
  iniciadas <- base %>% 
    filter(agencia_ini==agencia_principal) 
  #actual agencia, será la tabla que tiene las que iniciaron y no s ehan movido y las que actualemtne están en la agencia
  actual_agencia <- base %>% 
    filter(id_ap %in% c(id_rad, iniciadas$id_ap)) %>% 
    filter(!id_ap %in% c(id_remi)) %>% 
    mutate(tipo=case_when(
      agencia_ini==agencia_principal ~ "Iniciadas", 
      T ~ "Radicadas"
    )) %>% 
    left_join(total_radi %>% 
                mutate(IdCI=as.integer(IdCI), 
                       fecharemision=as_date(fecharemision)) %>% 
                filter(!duplicated(IdCI), 
                       !is.na(IdCI)) %>% 
                select("id_ap"=IdCI, fecharemision)
              
    ) %>% 
    mutate(fecharemision=case_when(is.na(fecharemision)~ fecha_inicio,
                                   T ~fecharemision))
  #la data_remitida son las que iniciaron pero ya se mandaron a otra agencia
  data_remitida_agencia <- base %>% 
    # filter(id_ap %in% c(id_rad, iniciadas$id_ap)) %>% 
    filter(id_ap %in% c(id_remi))
  
  #vamos a meter la fiscalia y agencia de destino
  #así queda después de pegarle info de a dónde fue remitida y su fecha
  data_remitida_agencia_principal <- data_remitida_agencia %>% 
    left_join(total_remi %>% mutate(idCI=as.integer(idCI)) %>% 
                filter(!duplicated(idCI), 
                       !is.na(idCI)) %>% 
                select("id_ap"=idCI, fiscaliadestino, agenciadestino, 
                       fecharemision)) 
  
  #### vamos a volver a hacer el proceso pero para toda la fiscalía territorial quitando la agencia principal
  
  id_remi <- total_remi %>% 
    filter(agencia_ini!=agencia_principal) %>% 
    filter(!duplicated(idCI)) %>% drop_na(idCI) %>% 
    filter(idCI %in% base$id_ap) %>% 
    pull(idCI)
  
  # fiscalia_principal <- "IZP"
  
  id_rad <- total_radi %>% filter(agenciadestino!=agencia_principal) %>% 
    filter(!duplicated(IdCI)) %>% drop_na(IdCI) %>% 
    filter(IdCI %in% base$id_ap) %>% 
    filter(!grepl(fiscalia_principal, agencia_ini)) %>% pull(IdCI)
  
  
  iniciadas <- base %>% 
    filter(grepl(fiscalia_principal, agencia_ini)) %>% 
    filter(agencia_ini!=agencia_principal)
  
  actual_fiscalia <- base %>% 
    filter(id_ap %in% c(id_rad, iniciadas$id_ap)) %>% 
    filter(!id_ap %in% c(id_remi)) %>% 
    mutate(tipo=case_when(
      # agencia_ini=="IZP-8" ~ "Iniciadas", 
      grepl(fiscalia_principal, agencia_ini) ~ "Iniciadas",
      T ~ "Radicadas"
    ))  %>% 
    left_join(total_radi %>% 
                mutate(IdCI=as.integer(IdCI), 
                       fecharemision=as_date(fecharemision)) %>% 
                filter(!duplicated(IdCI), 
                       !is.na(IdCI)) %>% 
                select("id_ap"=IdCI, fecharemision)
              
    ) %>% 
    mutate(fecharemision=case_when(is.na(fecharemision)~ fecha_inicio,
                                   T ~fecharemision))
  
  data_remitida <- base %>% 
    # filter(id_ap %in% c(id_rad, iniciadas$id_ap)) %>% 
    filter(id_ap %in% c(id_remi))
  
  #vamos a meter la fiscalia y agencia de destino
  data_remitida_fisc_actual <- data_remitida %>% 
    left_join(total_remi %>% mutate(idCI=as.integer(idCI)) %>% 
                filter(!duplicated(idCI), 
                       !is.na(idCI)) %>% 
                select("id_ap"=idCI, fiscaliadestino, agenciadestino,
                       fecharemision)) 
  
  #tabla de iniciadas en la agencia principal
  iniciadas <- base %>% 
    filter(agencia_ini==agencia_principal)
  
  #por últimso guardamos en una lista las bases creadas
  
  todas_bases <- list(actual_agencia=actual_agencia, 
                      data_remitida_agencia_principal=data_remitida_agencia_principal,
                      actual_fiscalia=actual_fiscalia,
                      data_remitida_fisc_actual=data_remitida_fisc_actual, 
                      iniciadas=iniciadas
                      )
  
  
  
  return(todas_bases)
  
}

#####
sectores <- sf::st_read("../ssc_sectores.shp")

sector <- sf::st_make_valid(sectores)

# todas_bases <- bases_fiscalias_estatus(agencia_principal = "CUH-1", 
#                                        fiscalia_principal = "CUH")
library(sf)
iniciadas <- base %>% 
  drop_na(coord_x) %>% 
  sf::st_as_sf(coords = c("coord_x", "coord_y"),
           crs=4326) %>% 
  sf::st_join(sector %>% 
            select(delegacion, nombre_sec, ct, geometry)) %>% 
  sf::st_drop_geometry()


#pruebas para las bases de flujo
todas_bases$actual_agencia %>% 
  tabyl(agencia_ini, tipo) #Las iniciadas solo deben ser de la agencia principal
#y las radicadas de la agencia principal deben ser 0
todas_bases$actual_fiscalia %>% 
  tabyl(agencia_ini, tipo) #Las iniciadas solo deben ser de la fiscalía principal
#y las radicadas de la fiscalía principal deben ser 0 y no deben de incluir la agencia principal
todas_bases$data_remitida_agencia_principal %>% 
  tabyl(agencia_ini) #todos deben ser de la agencia principal
todas_bases$data_remitida_fisc_actual %>% 
  tabyl(agencia_ini) #todos deben ser de la fiscalia principal

vinculados_a_proceso <- flagrancias %>% 
  filter(vinculacion_a_proceso_por_persona>0)


#####
#lista para PDI
lista <- base %>% 
  filter(ct_hechos=="IZP-6", 
         delito %in% c("HOMICIDIO DOLOSO", 
                       "LESIONES DOLOSAS POR DISPARO DE ARMA DE FUEGO", 
                       "VIOLACIÓN",
                       "ROBO DE VEHÍCULO CON Y SIN VIOLENCIA"
                       ), 
         fecha_inicio>="2019-01-01") %>% 
  select(id_ap, ci_formato_siap_fsiap, 
         fecha_inicio, delito
         ) %>% drop_na(id_ap) %>% 
  mutate(detenido=ifelse(id_ap %in% flagrancias$id_ap, 1, NA), 
         Vinculado_proceso=case_when(
           id_ap %in% vinculados_a_proceso$id_ap ~ 1,
           T ~NA), 
         Vinculado_proceso=ifelse(is.na(Vinculado_proceso) & detenido==1, 0, Vinculado_proceso))


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


mii <- sort(unique(flagrancias$res_puestas), decreasing = T)

puestas_disp_fisc <- flagrancias %>% 
  filter(agencia_ini==agencia_principal, 
         year(fecha_inicio) %in% c(2023, 2024)) %>% #drop_na(fiscalia) %>%
  group_by(res_puestas) %>%
  summarise(Total=n(), .groups = "drop") %>% #group_by(fiscalia)  %>%
  mutate(porcentaje=round(Total/sum(Total),2)) %>%
  complete(res_puestas=c("1. Libertad MP", "2. Ilegal detención PJ",
                         "3. Mecanismo alternativo",
                         "4. No vinculación",
                         "5. Vinculación simple", "6. Suspen. Condicional", "7. Prisión preventiva",
                         "8. Incompetencia", "9. Pendiente de estatus"),
           fill= list(Total=0, porcentaje=0)) %>%
  drop_na(res_puestas) %>% 
  mutate(res_puestas=factor(res_puestas,
                            levels = mii)) %>%
  drop_na(res_puestas)

#fiscalías territoriales
gr_puestas_fisc1 <- puestas_disp_fisc %>%
  # filter(grepl("Obregón|Cuauhtémoc|Iztapalapa|Milpa|Xochimilco|Azcapotzalco|Cuajimalpa|
  #              Benito|Coyoacán|Morelos|Cuauhtémoc|Madero|Iztacalco|Iztapalapa|
  #              Contreras|Hidalgo|Tláhuac|Tlalpan|Carranza|Benito|Contreras", fiscalia)) %>%
  arrange(desc(res_puestas)) %>%
  mutate(total_label=cumsum(Total)) %>%
  ggplot(aes(x="", y=Total, fill=res_puestas)) +
  geom_bar(stat = "identity") +
  # facet_wrap(.~fiscalia ) +
  #scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("gray65","#fe7f2d", "#fcca46",
                                       "#a1c181", "#619b8a", "#606c38",
                                       "#B28E50",
                                       "#e94f51", "#577590")) +
                                         theme_light() +
  labs(x="", fill="", y="") +
  theme(strip.text.x = element_text(colour="black"),
        legend.text = element_text(size = 16),
        axis.text = element_text(size=14, color="black")) +
  geom_label_repel(data=. %>% filter(Total>0),
                   aes(label=Total, y=total_label), max.time = 8, show.legend = F)+
  theme(strip.text = element_text(color="black", face = "bold", size = 16))


#####

#cálculos para presentación
cocca <- base %>% 
  filter(ct_hechos=="AO-1", 
         grepl("AO", fiscalia_ini))
flagrancias %>% 
  filter(id_ap %in% cocca$id_ap, 
         year(fecha_inicio) %in% c(2023, 2024)) %>% 
  group_by(year(fecha_inicio)) %>% 
  summarise(Total=n())




#promedio diario
# iniciadas <- base %>% 
#   filter(year(fecha_inicio) %in% c(2023,2024), 
#          ct_hechos=="AO-1", 
#          grepl("AO", fiscalia_ini), 
#          !id_ap %in% flagrancias$id_ap) 
#analisis por sectores

sector_principal <- c("TEPEPAN")
agencia_principal <- c("XO-2")
fiscalia_principal <- "XO"

#filtrar por sector
iniciadas <- iniciadas %>% get_fiscalia()

todas_bases$iniciadas <- iniciadas %>% 
  # get_fiscalia() %>% %
  filter(nombre_sec %in% sector_principal, 
    # grepl("IZC", ct),
         grepl(fiscalia_principal, agencia_ini)) %>% 
  filter(competencia== "FUERO COMUN")

#solo porque el sector comprende dos ct
# todas_bases$iniciadas <-  base %>% 
#     filter(ct_hechos==agencia_principal, 
#            competencia=="FUERO COMUN"
#            ) %>%  get_fiscalia() %>% 
#   filter(year(fecha_inicio)<2025, 
#          grepl(fiscalia_principal, agencia_ini)
#          )

todas_bases$iniciadas %>% st_drop_geometry() %>% 
  filter(#grepl("MH", agencia_ini), 
         year(fecha_inicio)>2022,
         !grepl("CJM", agencia_ini)
         ) %>% drop_na(id_ap) %>% 
  mutate(detenido=case_when(
    id_ap %in% flagrancias$id_ap ~ "Con detenido", 
    T ~ "Sin detenido"
  )) %>% 
  group_by(año=year(fecha_inicio), agencia_ini, detenido) %>% 
  summarise(Total=n()) %>% spread(año, Total, fill = 0) %>% 
  ungroup() %>% #group_by()
  mutate(porcentaje_2024=percent(`2024`/sum(`2024`), 1))


#comparación promedio diario
todas_bases$iniciadas %>% 
  filter(year(fecha_inicio)>2022) %>% 
  group_by(fecha_inicio) %>% 
  summarise(Total=n()) %>% 
  complete(fecha_inicio=seq.Date(as_date("2023-01-01"), 
                                 as_date("2024-12-31"), by="1 day"), 
           fill = list(Total=0)) %>% 
  group_by(año=year(fecha_inicio)) %>% 
  summarise(media=round(mean(Total), 1)) %>% 
  spread(año, media, fill=0) %>% 
  mutate(variacion=percent(`2024`/`2023`-1, .01))
  

inic_24 <- todas_bases$iniciada %>% st_drop_geometry() %>% 
  filter(year(fecha_inicio) %in% c(2023, 2024), 
         # grepl("IZC", ct), 
         # grepl("IZC", agencia_ini),
         #!(id_ap %in% todas_bases$iniciadas$id_ap)
  ) %>% 
  group_by(fecha_inicio) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date("2023-01-01"), 
                                 as_date("2024-12-31"), by="1 day"), 
           fill = list(Total=0)) %>% 
  group_by(año=year(fecha_inicio)) %>% 
  summarise(media=round(mean(Total), 1)) %>% spread(año, media, fill=0) %>% 
  pull(`2024`)


# todos los sectores de la fiscalía
tot_sector <- sectores %>% 
  filter(grepl(fiscalia_principal, ct)) %>%
  pull(nombre_sec) %>% length()-1
#comparación con la fiscalía sin contar agencia principal
fiscalias_territoriales <- paste(sep = "|",  "AZ", "BJ", "IZP", "IZC", "MH", "MIL",
                                 "CUJ", "COY", "CUH", "GAM", "MC", "TLH", "TLP", "VC", "XO", "AO")
# iniciadas <- iniciadas %>% get_fiscalia()
total_agencia <- iniciadas %>% 
  filter(grepl(fiscalias_territoriales, agencia_ini),
         !grepl("CJM|URI|UAT|ADD|AOP", agencia_ini),
         ) %>% pull(agencia_ini) %>% unique() %>% sort() %>% 
  length() -2

iniciadas %>% st_drop_geometry() %>% 
  # get_fiscalia() %>% 
  filter(competencia== "FUERO COMUN") %>% 
  filter(year(fecha_inicio) %in% c(2023, 2024), 
          #grepl(fiscalia_principal, ct), 
          grepl(fiscalias_territoriales, agencia_ini),
         !grepl("CJM|AOP", agencia_ini),
         !(id_ap %in% todas_bases$iniciadas$id_ap)
         ) %>% 
  group_by(fecha_inicio) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date("2023-01-01"), 
                                 as_date("2024-12-31"), by="1 day"), 
           fill = list(Total=0)) %>% 
  group_by(año=year(fecha_inicio)) %>% 
  summarise(media=round(mean(Total)/total_agencia, 1)) %>% spread(año, media, fill=0) %>% 
    mutate(variacion=percent(inic_24/`2024`-1, .1))



total_carp <- todas_bases$iniciadas %>% 
  filter(#tipo_impacto=="ALTO IMPACTO", 
         year(fecha_inicio)==2024) %>% nrow()
# alto impacto
todas_bases$iniciadas %>% st_drop_geometry() %>% 
  filter(tipo_impacto=="ALTO IMPACTO", 
         year(fecha_inicio) %in% c(2024, 2023)) %>% 
  group_by(año=year(fecha_inicio), delito) %>% 
  summarise(Total=n()) %>% arrange(-Total) %>% 
  spread(año, Total, fill=0) %>% arrange(-`2024`) %>% 
  mutate(variacion=percent(`2024`/`2023`-1, .01))

# bajo impacto
todas_bases$iniciadas %>% st_drop_geometry() %>% 
  filter(tipo_impacto=="BAJO IMPACTO", 
         year(fecha_inicio) %in% c(2024, 2023)) %>% 
  group_by(año=year(fecha_inicio), modalidad_delito) %>% 
  summarise(Total=n()) %>% top_n(5) %>% 
  arrange(-Total) %>% arrange(-Total) %>% 
  spread(año, Total, fill=0) %>% arrange(-`2024`) %>% 
  mutate(variacion=percent(`2024`/`2023`-1, .01))


#metemos la base de flagrancias
flagrancias <- flagrancias %>% 
  rename(ci_formato_siap_fsiap=ap_ci) %>% get_fiscalia()

#metemos nueva clasificación de  res_puestas
flagrancias <- flagrancias %>% 
  mutate(res_puestas=case_when(
    motivo_de_libertad=="Artículo 140 CNPP (Libertad durante la investigación)" ~ "1. Libertad por artículo 140",
    libertad==1 & observaciones_dgpec=="Ok" ~ "2. Otras libertades en MP",
    observaciones_dgpec_2=="No califica de legal la detención (libertad)" & observaciones_dgpec=="Ok" ~ "3. Ilegal detención PJ",
    mecanismo_alternativo==1 ~ "4. Mecanismo alternativo",
    incompetencia==1 & observaciones_dgpec=="Ok" ~
      "9. Incompetencia",
    vinculacion_a_proceso_por_persona==1 & observaciones_dgpec=="Ok" & (prision_preventiva_justificada>0 | prision_preventiva_oficiosa>0) ~ "8. Prisión preventiva",
    vinculacion_a_proceso_por_persona==1 & suspension_condicional==1 & observaciones_dgpec=="Ok" ~ "7. Suspen. Condicional",
    vinculacion_a_proceso_por_persona==1 & observaciones_dgpec=="Ok" ~ "6. Vinculación simple",
    observaciones_dgpec=="Ok" &
      (observaciones_dgpec_2 %in% c("No vincula a proceso",
                                    "Perdón de la víctima",
                                    "Juez determina libertad",
                                    "Acuerdo Reparatorio")) ~
      "4. No vinculación",
    observaciones_dgpec!="Ok" ~ "10. Pendiente de estatus"
    
  ))
#datos de agencia
flagr_agencia <- flagrancias %>% 
  filter(#agencia_ini==agencia_principal, 
    id_ap %in% todas_bases$iniciadas$id_ap,
         year(fecha_inicio) %in% c(2023, 2024)) %>% 
  group_by(año=year(fecha_inicio)) %>% 
  summarise(Puestas=n(), 
            judicializacion=sum(judicializacion, na.rm = T)
  ) %>% 
  gather(Indicador, Total, Puestas:judicializacion) %>% 
  spread(año, Total, fill=0)

library(forcats)
gr_flagr_lib <- flagrancias %>% 
  filter(#agencia_ini==agencia_principal, 
    id_ap %in% todas_bases$iniciadas$id_ap,
         year(fecha_inicio) %in% c(2023 , 2024)) %>% 
  mutate(tipo_libertad=case_when(
    res_puestas %in% c(#"1. Libertad por artículo 140",
                       "2. Otras libertades en MP", "3. Ilegal detención PJ") ~ 1,
    T ~ 0
  )) %>% 
  group_by(año=year(fecha_inicio), tipo_libertad, res_puestas) %>% 
  summarise(Total=n(), 
            # judicializacion=sum(judicializacion, na.rm = T)
  ) %>% group_by(año) %>% 
   mutate(porcentaje=Total/sum(Total), 
  #        color_texto=case_when(
  #          res_puestas %in% c("1. Libertad por artículo 140") ~ 0,
  #          T ~ 1
           #)
  ) %>% 
  filter(tipo_libertad==1) %>% 
  ggplot(aes(factor(reorder(año, -año)),
             # factor(año),
             porcentaje, fill=fct_rev(res_puestas), 
             fill=res_puestas,
             # group=res_puestas
             )) +
  geom_col(#aes()
    ) + theme_minimal() +
  tema_fgj + coord_flip() + 
  theme(legend.position = "bottom")+
  geom_text(aes(label=paste0(percent(porcentaje, 1), 
                              " (",comma(Total, 1), 
                              ")"), 
                #color=factor(color_texto),
                ), size=5.5,
           color="ghostwhite",
             position = position_stack(vjust = .5)
            ) +
  labs(x="", y="", fill="")+
  scale_y_continuous(labels=percent) +
  # scale_y_continuous(limits = c(0,.3), 
  #                    # labels = percent()
  # ) +
  scale_fill_manual(#values = rev(c("#fcca46", "#164a80", "grey45")), 
    values = rev(c("#164a80", "grey45")), 
    labels=rev(c("Libertades en MP\npor falta de elementos", 
             "Ilegal detención\nen PJ"
             )),
     guide = guide_legend(reverse = TRUE)
                    ) #+
  # scale_color_manual(values = c("ghostwhite", "black")) 
ggsave(plot = gr_flagr_lib, 
       "gr_flagr_lib.svg", width = 6, height = 3)


flagrancias %>% 
  filter(#agencia_ini==agencia_principal, 
    id_ap %in% todas_bases$iniciadas$id_ap,
    year(fecha_inicio) %in% c(2023, 2024)) %>% 
  group_by(año=year(fecha_inicio), res_puestas) %>% 
  summarise(Total=n(), 
            # judicializacion=sum(judicializacion, na.rm = T)
  ) %>% spread(año, Total, fill=0)


#promedio de diario de flagrancias
flagrancias %>% 
  filter(grepl(fiscalia_principal, ct_inicio)) %>% 
  mutate(comparacion=case_when(
    id_ap %in% todas_bases$iniciadas$id_ap ~ agencia_principal, 
    T ~ fiscalia_principal
  )) %>% filter(year(fecha_inicio)==2024) %>% 
  group_by(comparacion, fecha_inicio) %>% 
  summarise(Total=n()) %>% ungroup() %>% 
  complete(fecha_inicio=seq.Date(as_date("2024-01-01"), 
                                 as_date("2024-12-31"), by="1 day"), 
           comparacion=c(fiscalia_principal, agencia_principal), 
           fill = list(Total=0)) %>%
  group_by(comparacion) %>% 
  summarise(media=mean(Total))
  


#sacamos las radicaciones
#por agencia
total_radi %>% 
  filter(grepl("CUH-8|CUH-2", agenciadestino), 
         #grepl(agencia_principal, agenciadestino)
         ) %>% 
  nrow()

#usuarios
total_radi %>% 
  filter(#grepl("MH", agenciadestino), 
    grepl(agencia_principal, agenciadestino)) %>% 
  pull(usuariorad) %>% 
  unique() %>% 
  length()

personal %>% 
  filter(grepl("IZP-9|IZP-5", agencia)) %>% 
  summarise(Total=sum(personal_ministerial))


#fiscalía
total_radi %>% 
  filter(grepl(fiscalia_principal, agenciadestino), 
     #!grepl(agencia_principal, agenciadestino)
     !grepl("CUH-8|CUH-2", agenciadestino)
     
    ) %>% #tabyl(agenciadestino)
  nrow()

#usuarios
total_radi %>% 
  filter(grepl(fiscalia_principal, agenciadestino), 
    !grepl(agencia_principal, agenciadestino)) %>% 
  pull(usuariorad) %>% 
  unique() %>% 
  length() 


personal %>% 
  filter(
    grepl(fiscalia_principal, agencia),
    !grepl("CUH-8|CUH-2", agencia)
    ) %>% 
  summarise(Total=sum(personal_ministerial))
