setwd("C:/Users/mauri/OneDrive/Documentos/R/fiscalia/plan_politica_criminal")

poblacion <- read_excel("C:/Users/mauri/OneDrive/Documentos/R/fiscalia/bases/poblacion_total_tasa.xlsx")
poblacion <- poblacion %>% 
  rename(alcaldia=Alcaldia, pob_total=`Población total`)


shape_alcaldias <- st_read("C:/Users/mauri/OneDrive/Documentos/R/fiscalia/bases/poligonos_alcaldias_cdmx.shp")
shape_alcaldias <- shape_alcaldias %>% 
  rename(alcaldia=NOMGEO)
  
plot_alcaldias <- function(
    base, 
    tipo = c("incidencia", "victimas"),
    nombre = nombre_plots, 
    fecha_comienzo = fecha_comienzo,
    fecha_lim = fecha_lim,
    poblacion = poblacion,
    shape_alcaldias = shape_alcaldias
) {
  
  tabla_alc <-
    base %>% 
    filter(fecha_inicio >= as_date(fecha_comienzo)) %>% 
    group_by(alcaldia = str_to_title(delegacion_hechos)) %>% 
    summarise("CI totales" = n(),
              .groups = "drop") %>% 
    mutate(alcaldia = case_when(
      alcaldia == "Alvaro Obregon" ~ "Álvaro Obregón",
      alcaldia == "Benito Juarez" ~ "Benito Juárez",
      alcaldia == "Coyoacan" ~ "Coyoacán",
      alcaldia == "Cuajimalpa De Morelos" ~ "Cuajimalpa de Morelos",
      alcaldia == "Cuauhtemoc" ~ "Cuauhtémoc",
      alcaldia == "Gustavo A Madero" ~ "Gustavo A. Madero",
      alcaldia == "Tlahuac" ~ "Tláhuac",
      T ~ alcaldia
    )) %>%
    right_join(poblacion %>% 
                 select(alcaldia, pob_total)) %>% 
    mutate("CI por 100K habitantes" = (`CI totales`/pob_total) * 100000)
  
  
  
  
  shape_plot <- 
    tabla_alc %>% 
    left_join(shape_alcaldias %>% 
                mutate(
                  alcaldia = str_to_title(alcaldia),
                  alcaldia = case_when(
                    alcaldia == "Alvaro Obregon" ~ "Álvaro Obregón",
                    alcaldia == "Benito Juarez" ~ "Benito Juárez",
                    alcaldia == "Coyoacan" ~ "Coyoacán",
                    alcaldia == "Cuajimalpa De Morelos" ~ "Cuajimalpa de Morelos",
                    alcaldia == "Cuauhtemoc" ~ "Cuauhtémoc",
                    alcaldia == "Gustavo A Madero" ~ "Gustavo A. Madero",
                    alcaldia == "Tlahuac" ~ "Tláhuac",
                    T ~ alcaldia
                  )
                ) %>% 
                select(alcaldia, geometry)
    )  
  
  
  total <- 
    ggplot() +
    geom_sf(data = st_as_sf(shape_alcaldias), aes(geometry = geometry), fill = "white") +
    geom_sf(data = st_as_sf(shape_plot), 
            aes(geometry = geometry, fill = `CI totales`), 
            color = "white", size = .3, alpha = .7) +
    scale_fill_gradientn(colors = c("#00b140", "#f9a819", "#ff4f4f")) +
    labs(
      title = paste0("Concentración de CI\n", str_wrap(nombre, 30)),
      subtitle = paste0("Del ", format(as_date(fecha_comienzo), "%d de %B de %Y"), " al ",
                        format(as_date(fecha_lim), "%d de %B de %Y"))
    ) +
    theme_void() +
    #theme(legend.position = "bottom") +
    theme(
      #legend.position = "bottom",
      legend.key.size = unit(5, "mm"),
      plot.title = element_text(size = 12, face = "bold"),
      plot.subtitle = element_text(size = 10)
    )
  
  por_100k <- 
    ggplot() +
    geom_sf(data = st_as_sf(shape_alcaldias), aes(geometry = geometry), fill = "white") +
    geom_sf(data = st_as_sf(shape_plot), 
            aes(geometry = geometry, fill = `CI por 100K habitantes`), 
            color = "white", size = .3, alpha = .7) +
    scale_fill_gradientn(colors = c("#00b140", "#f9a819", "#ff4f4f")) +
    labs(
      title = paste0("Concentración por\n cien mil habitantes"),
      fill = ""
    ) +
    theme_void() +
    theme(
      #legend.position = "bottom",
      legend.key.size = unit(5, "mm"),
      plot.title = element_text(size = 12, face = "bold")
    )
  
  mapas <- plot_grid(total, NULL, por_100k,
                     rel_widths = c(1, .05, 1), ncol = 3)
  
  tabla_alc <- 
    tabla_alc %>% 
    mutate(alcaldia=case_when(
      grepl("agdalena", alcaldia) ~ "Magdalena Contreras", 
      T ~ alcaldia
    )) %>% arrange(-`CI por 100K habitantes`) %>% 
    rename("Alcaldía" = alcaldia) %>% 
    select(-pob_total) %>% 
    adorn_totals(na.rm = T) %>% 
    replace_na(list(`Alcaldía` = "No identificado")) %>%
    mutate(`CI totales` = ifelse(is.na(`CI totales`), "-", 
                                 comma(`CI totales`, accuracy = 1)),
           `CI por 100K habitantes` = ifelse(is.na(`CI por 100K habitantes`), "-",
                                             comma(`CI por 100K habitantes`, accuracy = .01))
    )
  
  tt_alc <- ttheme_minimal(
    base_size = 10,
    padding = unit(c(4,4), "mm"),
    core = list(bg_params = list(col = NA,
                                 fill=c(rep(c(NA),
                                            length.out = 16), "#bde0fe"))),
    colhead=list(fg_params = list(col = "white", fontface = "bold"),
                 bg_params = list(fill=c("#233d4d"))))
  
  tabla_final <- 
    tableGrob(tabla_alc, rows = NULL, theme = tt_alc) %>% 
    gtable_add_grob(grobs = replicate(ncol(.) - 1,
                                      segmentsGrob(x1 = unit(0, "npc"), 
                                                   gp = gpar(lty = 2)),
                                      simplify = FALSE),
                    t = 2, b = nrow(.), l = seq_len(ncol(.) - 1) + 1) 
  
  tabla_final <- plot_grid(tabla_final)
  
  final <- plot_grid(tabla_final, NULL,  mapas, ncol = 3, rel_widths = c(1, 0.05, 1))
  return(final)
}

#homicidio doloso
mapa_hom <- plot_alcaldias(base = data %>% 
                 filter(year(fecha_inicio)==2024, 
                        delito=="HOMICIDIO DOLOSO", 
                        grepl("HOM", modalidad_delito)
                        ), 
                tipo="incidencia", 
               nombre = "Homicidio doloso", 
               fecha_comienzo = "2024-01-01", 
               fecha_lim = "2024-12-31", 
               poblacion = poblacion, 
               shape_alcaldias = shape_alcaldias
    
)

ggsave(plot = mapa_hom, 
       "mapa_hom.png", width = 12, height = 7
       )

#Extorsión
mapa_ext <- plot_alcaldias(base = data %>% 
                             filter(year(fecha_inicio)==2024, 
                                    modalidad_delito=="EXTORSION"
                             ), 
                           tipo="incidencia", 
                           nombre = "Extorsión", 
                           fecha_comienzo = "2024-01-01", 
                           fecha_lim = "2024-12-31", 
                           poblacion = poblacion, 
                           shape_alcaldias = shape_alcaldias
                           
)

ggsave(plot = mapa_ext, 
       "mapa_ext.png", width = 12, height = 7
)


#Desaparición forzada
mapa_desap <- plot_alcaldias(base = data %>% 
                             filter(year(fecha_inicio)==2024, 
                                    modalidad_delito=="DESAPARICION FORZADA DE PERSONAS"
                             ), 
                           tipo="incidencia", 
                           nombre = "Desaparición forzada", 
                           fecha_comienzo = "2024-01-01", 
                           fecha_lim = "2024-12-31", 
                           poblacion = poblacion, 
                           shape_alcaldias = shape_alcaldias
                           
)

ggsave(plot = mapa_desap, 
       "mapa_desap.png", width = 12, height = 7
)

#Robo a casa habitación
mapa_casa <- plot_alcaldias(base = data %>% 
                               filter(delito=="ROBO A CASA HABITACIÓN CON VIOLENCIA"
                               ), 
                             tipo="incidencia", 
                             nombre = "Robo a casa habitación con violencia", 
                             fecha_comienzo = "2024-01-01", 
                             fecha_lim = "2024-12-31", 
                             poblacion = poblacion, 
                             shape_alcaldias = shape_alcaldias
                             
)

ggsave(plot = mapa_casa, 
       "mapa_casa.png", width = 12, height = 7
)


#robo a vehículo

mapa_coche <- plot_alcaldias(base = data %>% 
                              filter(delito=="ROBO DE VEHÍCULO CON Y SIN VIOLENCIA"
                              ), 
                            tipo="incidencia", 
                            nombre = "Robo de vehículo", 
                            fecha_comienzo = "2024-01-01", 
                            fecha_lim = "2024-12-31", 
                            poblacion = poblacion, 
                            shape_alcaldias = shape_alcaldias
                            
)

ggsave(plot = mapa_coche, 
       "mapa_coche.png", width = 12, height = 7
)


#robo de autopartes

mapa_auto_partes <- plot_alcaldias(base = data %>% 
                               filter(modalidad_delito=="ROBO DE ACCESORIOS DE AUTO"
                               ), 
                             tipo="incidencia", 
                             nombre = "Robo de autopartes", 
                             fecha_comienzo = "2024-01-01", 
                             fecha_lim = "2024-12-31", 
                             poblacion = poblacion, 
                             shape_alcaldias = shape_alcaldias
                             
)

ggsave(plot = mapa_auto_partes, 
       "mapa_auto_partes.png", width = 12, height = 7
)

####Genero
#Feminicidio


mapa_femi <- plot_alcaldias(base = data %>% 
                                     filter(delito=="HOMICIDIO DOLOSO", 
                                            grepl("FEM", modalidad_delito)
                                     ), 
                                   tipo="incidencia", 
                                   nombre = "Feminicidio", 
                                   fecha_comienzo = "2024-01-01", 
                                   fecha_lim = "2024-12-31", 
                                   poblacion = poblacion, 
                                   shape_alcaldias = shape_alcaldias
                                   
)

ggsave(plot = mapa_femi, 
       "mapa_femi.png", width = 12, height = 7
)


#Violación
mapa_viol <- plot_alcaldias(base = data %>% 
                              filter(delito=="VIOLACIÓN"
                              ), 
                            tipo="incidencia", 
                            nombre = "Violación", 
                            fecha_comienzo = "2024-01-01", 
                            fecha_lim = "2024-12-31", 
                            poblacion = poblacion, 
                            shape_alcaldias = shape_alcaldias
                            
)

ggsave(plot = mapa_viol, 
       "mapa_viol.png", width = 12, height = 7
)

##violencia familiar
mapa_fam <- plot_alcaldias(base = data %>% 
                              filter(modalidad_delito=="VIOLENCIA FAMILIAR"
                              ), 
                            tipo="incidencia", 
                            nombre = "Violencia familiar", 
                            fecha_comienzo = "2024-01-01", 
                            fecha_lim = "2024-12-31", 
                            poblacion = poblacion, 
                            shape_alcaldias = shape_alcaldias
                            
)

ggsave(plot = mapa_fam, 
       "mapa_fam.png", width = 12, height = 7
)



####corrupción
mapa_cohecho <- plot_alcaldias(base = data %>% 
                             filter(modalidad_delito=="COHECHO"
                             ), 
                           tipo="incidencia", 
                           nombre = "Cohecho", 
                           fecha_comienzo = "2024-01-01", 
                           fecha_lim = "2024-12-31", 
                           poblacion = poblacion, 
                           shape_alcaldias = shape_alcaldias
                           
)

ggsave(plot = mapa_cohecho, 
       "mapa_cohecho.png", width = 12, height = 7
)
