data <- readxl::read_excel("C:/Users/mauri/Downloads/nombres_despojo_arely2.xlsx")
data <- data %>% 
  filter(categoria_resumen %in% c("Imputado", "Defensor")) %>% 
  mutate(
    categoria_resumen=case_when(
      desccalidad=="DEFENSOR PUBLICO" ~ "Defensor público", 
      categoria_resumen=="Defensor" ~ "Defensor particular", 
      T ~ categoria_resumen
    ), nombre_completo=paste0(nombre_homologado, paterno_homologado,
                                materno_homologado, 
                                "-", categoria_resumen), 
         nombre_completo=gsub(" ", "", nombre_completo)
         ) %>% 
  filter(!grepl("OCUPANTE|POSEED", nombre_completo)) %>% 
  filter(delito=="1 ROJAS DESPOJO", 
         categoria_resumen!="Defensor público") %>% 
  group_by(nombre_completo) %>% 
  mutate(total=n()) %>% ungroup()
data2 <- data %>%
  select(ctrluinv, "persona1"=nombre_completo) %>% 
  inner_join(data %>% select(ctrluinv, "persona2"=nombre_completo), 
             by="ctrluinv") %>% 
  filter(persona1!=persona2) %>% 
  # mutate(tipo="pp") %>% 
  filter(!grepl("NANA|DESCONO|NN|OCUPANTE|POSEED", persona1)) %>% 
  filter(!grepl("NANA|DESCONO|NN|OCUPANTE|POSEED", persona2)) %>% 
  mutate(id=paste0(ctrluinv, pmax(persona1, persona2), pmin(persona1, persona2))) %>% 
  filter(!duplicated(id)) #%>% 
  # mutate(persona1=paste0(persona1, "-", tipo),
  #        persona2=paste0(persona2, "-", tipo))

data_total <- data2
library(igraph)
punibles <- punibles %>% 
  mutate(nombre_completo=paste0(apellidoPaterno, apellidoMaterno, nombre), 
         nombre_completo=gsub(" ", "", nombre_completo))

no_punibles <- no_punibles %>% 
  mutate(nombre_completo=paste0(apellidoPaterno, apellidoMaterno, nombre), 
         nombre_completo=gsub(" ", "", nombre_completo))
Encoding(punibles$nombre_completo) <- "UTF-8"
Encoding(no_punibles$nombre_completo) <- "UTF-8"

punibles$nombre_completo <- gsub('\\-|"', "", punibles$nombre_completo)
no_punibles$nombre_completo <- gsub('\\-|"', "", no_punibles$nombre_completo)


data1 <- tibble(id_ap=id_data) %>% 
  inner_join(no_punibles %>% select("id_ap"=idAveriguacionPrevia, "persona1"=nombre_completo)) %>% 
  inner_join(punibles %>% select("id_ap"=idAveriguacionPrevia, "persona2"=nombre_completo)) %>% 
  mutate(tipo1="nn", 
         tipo2="pp")%>% 
  filter(!grepl("NANA|DESCONO|NN", persona1)) %>% 
  filter(!grepl("NANA|DESCONO|NN", persona2)) %>% 
  mutate(id=paste0(id_ap, pmax(persona1, persona2), pmin(persona1, persona2))) %>% 
  filter(!duplicated(id)) %>% 
  mutate(persona1=paste0(persona1, "-", tipo1),
         persona2=paste0(persona2, "-", tipo2))


data2 <- punibles %>% select("id_ap"=idAveriguacionPrevia, "persona1"=nombre_completo) %>% 
  inner_join(punibles %>% select("id_ap"=idAveriguacionPrevia, "persona2"=nombre_completo), 
            by="id_ap") %>% 
  filter(persona1!=persona2) %>% 
  mutate(tipo="pp") %>% 
  filter(!grepl("NANA|DESCONO|NN", persona1)) %>% 
  filter(!grepl("NANA|DESCONO|NN", persona2)) %>% 
  mutate(id=paste0(id_ap, pmax(persona1, persona2), pmin(persona1, persona2))) %>% 
  filter(!duplicated(id)) %>% 
  mutate(persona1=paste0(persona1, "-", tipo),
         persona2=paste0(persona2, "-", tipo))


data3 <- no_punibles %>% select("id_ap"=idAveriguacionPrevia, "persona1"=nombre_completo) %>% 
  inner_join(no_punibles %>% select("id_ap"=idAveriguacionPrevia, "persona2"=nombre_completo), 
            by="id_ap") %>% 
  filter(persona1!=persona2)  %>% 
  mutate(tipo="nn") %>% 
  filter(!grepl("NANA|DESCONO|NN", persona1)) %>% 
  filter(!grepl("NANA|DESCONO|NN", persona2)) %>% 
  mutate(id=paste0(id_ap, pmax(persona1, persona2), pmin(persona1, persona2))) %>% 
  filter(!duplicated(id)) %>% 
  mutate(persona1=paste0(persona1, "-", tipo),
         persona2=paste0(persona2, "-", tipo))


data_total <- bind_rows(data1, data2) %>% 
  bind_rows(data3) 
  


#hacemos la matrix
friends_mat <- as.matrix(data_total %>% select(persona1, persona2))


g <- graph.edgelist(friends_mat, directed = FALSE)

####probar gráfica con nombres relevantes
friends_mat <- as.matrix(data_total %>% 
                           filter(persona1 %in% nombres_relacionados | 
                                    persona2 %in% nombres_relacionados) %>% 
                           # mutate(id_ap=as.character(id_ap)) %>% 
                           # separate(persona1, c("nombre1", "tipo1")) %>% 
                           # separate(persona2, c("nombre2", "tipo2")) %>% 
                           select(persona1, persona2))


g <- graph.edgelist(friends_mat, directed = F)


######

tabla1 <- tibble(nombre=names(degree(g, mode=c("all"))),
                conexiones=degree(g, mode=c("all")))

plot(g,
     # vertex.label = NA,
     # edge.color = 'black',
     vertex.size = 1.5,
     edge.arrow.size = 0.05,
     layout = layout_as_tree(g))

# 
# nombres <- tabla %>% 
#   filter(conexiones>=2) %>% pull(nombre)
# 
# 
# friends_mat <- as.matrix(data2 %>%
#                            filter(persona1 %in% nombres | 
#                                     persona2 %in% nombres) %>% 
#                            select(persona1, persona2) )
# 
# 
# g <- graph.edgelist(friends_mat, directed = FALSE)
# 
# plot(g,
#      vertex.label = NA,
#      # edge.color = 'black',
#      vertex.size = 1.5,
#      edge.arrow.size = 0.05,
#      layout = layout_nicely(g))
#                 




####otro estilo

tabla2 <- tibble(nombre=names(betweenness(g, directed = TRUE, normalized = T)), 
                conexiones=betweenness(g, directed = TRUE, normalized = T))

tabla <- tabla1 %>% 
  left_join(tabla2, by="nombre") %>% 
  mutate(tot_conexiones=conexiones.x*conexiones.y)
# nombres <- tabla %>% 
#   filter(conexiones>0) %>% pull(nombre)

nombres <- tabla %>% 
  arrange(desc(tot_conexiones)) %>%
  mutate(name=nombre) %>% 
  separate(name, c("name", "tipo"), "-") %>% 
  filter(tot_conexiones>0, 
         tipo!="Otrosnopunibles") %>% 
  # head(20) %>% 
  pull(nombre) %>% unique()


friends_mat <- as.matrix(data_total %>%
                           filter(persona1 %in% nombres |
                                    persona2 %in% nombres) %>%
                           mutate(persona1=paste0(persona1, "-", tipo),
                                  persona2=paste0(persona2, "-", tipo)) %>%
                           select(persona1, persona2) )


g <- graph.edgelist(friends_mat, directed = FALSE)

nombres <- data %>% 
  arrange(desc(total)) %>% pull(nombre_completo) %>% 
  unique()
# plot(g,
#      vertex.label.cex = .3,
#      # edge.color = 'black',
#      vertex.size = 1.5,
#      edge.arrow.size = 0.05,
#      layout = layout_nicely(g))

###con ggplot
# library(ggnetwork)
# gn <- ggnetwork(g)
# 
# gn <- gn %>% 
#   separate(name, c("nombre", "tipo"), sep="-") %>% 
#   mutate(tipo=case_when(
#     tipo=="nn"  ~ "No punible",
#     tipo=="pp" ~ "Punible", 
#     tipo=="np" ~ "No punible-Punible"
#   ))
# 
# graph_plot <- ggplot(gn  %>% filter(!duplicated(nombre)), aes(x = x, y = y, xend = xend, yend = yend)) +
#   geom_edges(linewidth=.1) +
#   geom_nodes(aes(color=tipo), size=1) +
#   scale_color_manual(values = colores[9:7]) +
#   theme_blank()+ 
#   geom_text_repel(data=. %>% filter(!duplicated(nombre)),
#                     
#                     aes(label=str_to_title(nombre)), hjust=.5, vjust=-1, 
#             size=.8)
# ggsave(plot = graph_plot, 
#        "x:/graficas/redes_total.png", width = 30, height = 30, dpi=600)

####con tipo
data_importate <- data_total %>% 
  filter(persona1 %in% nombres[10] | 
           persona2 %in% nombres[10])

nombres_importantes <- data_total %>% 
  # filter(persona1 %in% nombres | 
  #          persona2 %in% nombres) %>% 
  select(id_ap, persona1, persona2) %>% 
  separate(persona1, c("nombre1", "tipo1"), sep = "-") %>% 
  separate(persona2, c("nombre2", "tipo2"), sep = "-") %>% 
  mutate(id1=paste0(nombre1, id_ap), 
         id2=paste0(nombre2, id_ap)) %>% 
  filter(!duplicated(id1)) %>% 
  filter(!duplicated(id2)) %>% 
  pivot_longer(cols = c("id1", "id2"),
               names_to = "Variable",
               values_to = "ids") %>%
  pivot_longer(cols = starts_with("tipo"),
               names_to = "Variable2",
               values_to = "tipo") %>% 
  mutate(quitar=case_when(
    Variable=="id1" & Variable2=="tipo1" ~ 0, 
    Variable=="id2" & Variable2=="tipo2" ~ 0, 
    T~1
  )) %>% filter(quitar==0) %>% 
  filter(!duplicated(ids))

library(network)
red <- network(data_total %>% 
                 filter(persona1 %in% nombres[1:10] | 
                          persona2 %in% nombres[1:10]) %>% 
                 # mutate(id_ap=as.character(id_ap)) %>% 
                 # separate(persona1, c("nombre1", "tipo1")) %>% 
                 # separate(persona2, c("nombre2", "tipo2")) %>% 
                 select(persona1, persona2), multiple = T, loops = T, directed = T)

nombres_importantes <- unique(gn$vertex.names)

library(ggnetwork)
gn <- ggnetwork(red)

gn_tipo <- gn %>% 
  # filter(vertex.names %in% nombres) %>% 
  separate(vertex.names, c("nombre", "tipo"), sep="-") %>%
  # mutate(id=paste0(vertex.names, id_ap))
  mutate(tipo=case_when(
    tipo=="Imputado"  ~ "Imputado",
    tipo=="Otrosnopunibles" ~ "Otros no punibles",
    tipo=="Denuncianteovíctima" ~ "Denunciante o víctima"
  ))

nombres_solo <- tibble(nombres=nombres) %>% 
  separate(nombres, c("nombre", "tipo")) %>% 
  pull(nombre)

gn_tipo <- gn %>% 
  filter(vertex.names %in% nombres_solo) %>% 
  left_join(nombres_importantes %>% 
              select(ids, tipo), by=c("id"="ids")) 

library(ggrepel)
graph_plot <- ggplot(gn_tipo,
                     aes(x = x, y = y, xend = xend, yend = yend, 
                         text=nombre)) +
  geom_edges(#arrow = arrow(length = unit(0.3, "lines")), 
             linewidth=.2, alpha=.1, curvature = .1) +
  geom_nodes(aes(color=tipo), size=1, alpha=.5, 
             position = "identity") +
  theme_blank()+ 
  geom_nodetext_repel(aes(label=str_to_title(nombre),
                      color=tipo), hjust=.5, vjust=-1,
                  size=2, arrow = NULL, segment.color=NA, show.legend = F) +
  scale_color_manual(values = colores[7:9]) 

gr <- ggplotly(graph_plot)

htmlwidgets::saveWidget(gr, "total_despojo_gr.html")

ggsave(plot = graph_plot, 
       "redes_total_3.png", width = 12, height = 12, dpi=300)

ggsave(plot = graph_plot, 
       "redes_total.pdf", width = 16, height = 16, dpi=1400)

ggsave(plot = graph_plot, 
       "redes_total.svg", width = 16, height = 16, dpi=1400)


####hacer redes a partir de una persona
nombres_relacionados <- gn %>% 
  pull(vertex.names) %>% 
  unique()


red2 <- network(data_total %>% 
                 filter(persona1 %in% nombres_relacionados | 
                          persona2 %in% nombres_relacionados) %>% 
                 # mutate(id_ap=as.character(id_ap)) %>% 
                 # separate(persona1, c("nombre1", "tipo1")) %>% 
                 # separate(persona2, c("nombre2", "tipo2")) %>% 
                 select(persona1, persona2), multiple = T, loops = T)


# tabla <- tibble(nombre=names(betweenness(g, directed = TRUE, normalized = T)), 
#                 conexiones=betweenness(g, directed = TRUE, normalized = T))
gn2 <- ggnetwork(red2)

involucrados <- datos %>% 
  # mutate(name=nombre_completo) %>% 
  select(nombre, 
         cluster)
# gn2 <- gn2 %>% 
#   left_join(involucrados, 
#             by=c("vertex.names"="nombre_completo")) 

gn_tipo2 <- gn2 %>% 
  left_join(data %>%
              filter(!duplicated(nombre_completo)) %>%
              select(nombre_completo, total),
            by=c("vertex.names"="nombre_completo")) %>%
  mutate(id=paste(sep="-", x, y, xend, yend)) %>% 
  filter(!duplicated(id)) %>%
  # filter(vertex.names %in% nombres) %>% 
  separate(vertex.names, c("nombre", "tipo"), sep="-") %>%
    left_join(involucrados)
  
  # mutate(id=paste0(vertex.names, id_ap))
  # mutate(tipo=case_when(
  #   tipo=="nn"  ~ "No punible",
  #   tipo=="pp" ~ "Punible",
  #   tipo=="np" ~ "No punible-Punible"
  # ))


graph_plot2 <- ggplot(gn_tipo2,
                     aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(arrow = arrow(length = unit(0.3, "lines")),
             linewidth=.8, alpha=.3) +
  geom_nodes(shape="circle", aes(size=total, color=tipo),alpha=.8#, 
             # color="gold"
               ) +
  theme_blank() +
  # geom_nodetext_repel(data=. %>% filter(!duplicated(nombre)),
  #                 aes(label=str_to_title(nombre),
  #                     color=tipo), hjust=.5, vjust=-1, #color=colores[7],
  #                 size=3.5, arrow = NULL, segment.color=NA, show.legend = F) +
  scale_color_manual(values = colores[7:9]) 

ggsave(plot = graph_plot2, 
       "redes_total_are2.png", width = 16, height = 16, dpi=300)
#interactivos
plotly::ggplotly(graph_plot2)

#sacamos los nombres de tercer orden
nombres_relacionados2 <- gn2 %>% 
  pull(vertex.names) %>% unique()

# Crear el archivo HTML con el gráfico interactivo
htmlwidgets::saveWidget(network, "graph.html", selfcontained = TRUE)


despojo <- despojo %>% 
  left_join(ctrl_despojo)


datos <- data %>% 
  filter(nombre_completo %in% nombres_relacionados2) %>%
  left_join(despojo %>% 
              select(SAP, id_ap, resumen_ap, delegacion_hechos, 
                     colonia_hechos,
                     calle_1_hechos, calle_2_hechos,
                     coord_x, coord_y, 
                     fecha_inicio, tipo_final, subtipo_final),
            by=c("ctrluinv"="SAP"))

nombres_rel <- nombres_rel_2 <- list()
for (i in 1:10) {
  red <- network(data_total %>% 
                   filter(persona1 %in% nombres[i] | 
                            persona2 %in% nombres[i]) %>% 
                   # mutate(id_ap=as.character(id_ap)) %>% 
                   # separate(persona1, c("nombre1", "tipo1")) %>% 
                   # separate(persona2, c("nombre2", "tipo2")) %>% 
                   select(persona1, persona2), multiple = T, loops = T, directed = T)

  gn <- ggnetwork(red)
  
  
  ####hacer redes a partir de una persona
  nombres_rel[[i]] <- gn %>% 
    pull(vertex.names) %>% 
    unique()
  
  red_2 <- network(data_total %>% 
                   filter(persona1 %in% nombres_rel[[i]] | 
                            persona2 %in% nombres_rel[[i]]) %>% 
                   # mutate(id_ap=as.character(id_ap)) %>% 
                   # separate(persona1, c("nombre1", "tipo1")) %>% 
                   # separate(persona2, c("nombre2", "tipo2")) %>% 
                   select(persona1, persona2), multiple = T, loops = T, directed = T)
  
  gn_2 <- ggnetwork(red_2)
  
  nombres_rel_2[[i]] <- gn_2 %>% 
    pull(vertex.names) %>% 
    unique()
  
}

datos <- datos %>% 
  mutate(cluster=case_when(
    nombre_completo %in% nombres_rel_2[[1]] ~ 1, 
    nombre_completo %in% nombres_rel_2[[2]] ~ 2,
    nombre_completo %in% nombres_rel_2[[3]] ~ 3,
    nombre_completo %in% nombres_rel_2[[4]] ~ 4, 
    nombre_completo %in% nombres_rel_2[[5]] ~ 5, 
    nombre_completo %in% nombres_rel_2[[6]] ~ 6, 
    nombre_completo %in% nombres_rel_2[[7]] ~ 7, 
    nombre_completo %in% nombres_rel_2[[8]] ~ 8, 
    nombre_completo %in% nombres_rel_2[[9]] ~ 9, 
    nombre_completo %in% nombres_rel_2[[10]] ~ 10
    
  ))




flagr <- flagrancias %>% 
  filter(vinculacion_a_proceso_por_persona>0) %>% 
  group_by(id_ap) %>% 
  summarise(vinc_flagr=n())

ord <- ordenes %>% 
  group_by("id_ap"=as.integer(id_carpeta_uet)) %>% 
  summarise(ordenes=n()) %>% 
  drop_na(id_ap)

datos <- datos %>% left_join(flagr) %>% 
  left_join(ord) %>% 
  replace_na(list(vinc_flagr=0, 
                  ordenes=0))

datos <- datos %>% 
  left_join(orden %>% 
              select(-Total), by="cluster") %>% 
  select(-cluster) %>% 
  rename(cluster=nuevo_cluster) %>% 
  separate(nombre_completo, c("nombre", "tipo"))

write.csv(datos, "despojo_cluster.csv", row.names = F)
datos_imputado <- datos %>% 
  filter(tipo=="Imputado")
write.csv(datos, "despojo_cluster_imputado.csv", row.names = F)


gr <- list()

datos <- datos %>% 
  mutate(cluster=as.integer(cluster), 
         nombre_completo=paste0(nombre,"-", tipo))

for (i in 1:10) {
  
  red2 <- network(data_total %>% 
                    filter(persona1 %in% datos$nombre_completo[datos$cluster==i] | 
                             persona2 %in% datos$nombre_completo[datos$cluster==i]) %>% 
                    select(persona1, persona2), multiple = T, loops = T)

  gn2 <- ggnetwork(red2)
  
  gr[[i]] <- gn2 %>% 
    # filter(vertex.names %in% datos$nombre_completo[datos$cluster==i]) %>% 
    left_join(data %>% 
                filter(!duplicated(nombre_completo)) %>% 
                select(nombre_completo, total), 
              by=c("vertex.names"="nombre_completo")) %>% 
    mutate(id=paste(sep="-", x, y, xend, yend)) %>% 
    filter(!duplicated(id)) %>%
    separate(vertex.names, c("nombre", "tipo"), sep="-") %>%
    ggplot(
      aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_edges(#arrow = arrow(length = unit(0.3, "lines")),
      linewidth=.8, alpha=.3) +
    geom_nodes(shape="circle", aes(size=total, 
                                   color=tipo),alpha=.8) +
    theme_blank() +
    scale_size_continuous(range = c(3, 7)) +
    geom_nodetext_repel(data=. %>% filter(!duplicated(nombre)),
                        aes(label=str_to_title(nombre),
                            color=tipo), hjust=.5, vjust=-1, #color=colores[7],
                        size=2.5, arrow = NULL, segment.color=NA, show.legend = F) +
  scale_color_manual(values = colores_tipo)
  
  ggsave(plot = gr[[i]], 
         paste0("redes_ciclo_involucrados", i, ".png"),
         width = 12, height = 9, dpi=300)
}



####mapa de redes de despojo
data_des2 <- datos2 %>% 
  filter(!duplicated(id_ap))
data_join_imputados_2 <- data_des2 %>% 
  mutate(cluster=3) %>% 
  filter(id_ap %in% ids_imputados) %>% 
  full_join(data_des2 %>% 
              mutate(cluster=3) %>% 
              filter(id_ap %in% ids_imputados) %>% 
              rename(xend=coord_x, yend=coord_y) %>% 
              select(cluster, "id_cap"=id_ap, xend, yend)) %>%
  filter(id_ap!=id_cap)

data_des <- datos %>% 
  filter(!duplicated(id_ap))

data_join_imputados <- data_des %>% 
  filter(id_ap %in% ids_imputados) %>% 
  full_join(data_des %>% 
              filter(id_ap %in% ids_imputados) %>% 
              rename(xend=coord_x, yend=coord_y) %>% 
              select(cluster, "id_cap"=id_ap, xend, yend)) %>%
  filter(id_ap!=id_cap) 
library(ggmap)
data_join_imputados <- data_join_imputados %>% 
  filter(cluster %in% c(3,8,10,9))


map <- get_map(c(lon=-99.15860089666813, lat=19.35813277206228), 
                     zoom = 11, maptype = "roadmap", 
               size = c(640, 640), color = "bw",
               source = "google", language = "es-ES")

#sur
map <- get_map(c(lon=-99.17819317164302, lat=19.326291656619684), 
               zoom = 12, maptype = "roadmap", 
               size = c(640, 640), color = "bw",
               source = "google", language = "es-ES")

# map <- get_map()
ids_imputados <- datos %>% 
  filter(tipo=="Imputado") %>% pull(id_ap)

data_join_imputados <- data_join_imputados %>% 
  mutate(cluster=as.integer(cluster))

cluster_imputados <- c("3"=colores[3], "10"=colores[2], 
                       "9"=colores[4], "8"=colores[7])
mapa <- ggmap::ggmap(map) +
  geom_point(data=data_join_imputados %>% 
               filter(!duplicated(id_ap)), 
             aes(coord_x, coord_y,
                 color=factor(cluster)), 
             size=5.5, alpha=.9) +
  geom_segment(data=data_join_imputados %>% 
                 filter(!duplicated(id_ap)), 
               aes(coord_x, coord_y, xend=xend, yend=yend, 
                   color=factor(cluster)), 
               show.legend = F, 
               linewidth=1, linetype="dashed") +
  theme_void() +
  scale_color_manual(values = cluster_imputados, 
                     labels=c(1,3,2,4)) +
  theme(legend.position = "bottom") +
  labs(color="clúster")

ggsave(plot = mapa, 
       "mapa_redes_imputados_300523.png", width = 10, height = 10, dpi = 300)  

#ciclos de mapas por cluster
colores_cluster <- c(colores_cluster[1:8], "9"="#E41A1C", "10"="#377EB8")
mapas <- list()
for (i in 1:10) {
  
  base <- data_join_imputados %>% 
    filter(cluster==i)
  coordenada_x <- mean(base$coord_x, na.rm = T)
  coordenada_y <- mean(base$coord_y, na.rm = T)
  
  maps <- get_map(c(lon=coordenada_x, lat=coordenada_y), 
                 zoom = 16, maptype = "roadmap", 
                 size = c(640, 640), color = "bw",
                 source = "google", language = "es-ES")
  
  mapas[[i]] <- ggmap::ggmap(maps) +
    geom_point(data=base %>% 
                 filter(!duplicated(id_ap)), 
               aes(coord_x, coord_y,
                   color=factor(cluster)), 
               size=5.5, alpha=.9) +
    geom_segment(data=base %>% 
                   filter(!duplicated(id_ap)), 
                 aes(coord_x, coord_y, xend=xend, yend=yend, 
                     color=factor(cluster)), 
                 show.legend = F, 
                 linewidth=1, linetype="dashed") +
    theme_void() +
    scale_color_manual(values = colores_cluster) +
    theme(legend.position = "bottom") +
    labs(color="clúster")
  
  ggsave(plot = mapas[[i]] ,
         paste0("mapa_redes_", i, ".png"),
         width = 10, height = 10, dpi = 300)
  
  
}




#imputados
for (i in c(8,9,3,10)) {
  
  base <- data_join_imputados %>% 
    filter(cluster==i)
  coordenada_x <- mean(base$coord_x, na.rm = T)
  coordenada_y <- mean(base$coord_y, na.rm = T)
  
  maps <- get_map(c(lon=coordenada_x, lat=coordenada_y), 
                  zoom = 13, maptype = "roadmap", 
                  size = c(640, 640), color = "bw",
                  source = "google", language = "es-ES")
  
  mapas[[i]] <- ggmap::ggmap(maps) +
    geom_point(data=base %>% 
                 filter(!duplicated(id_ap)), 
               aes(coord_x, coord_y,
                   color=factor(cluster)), 
               size=5.5, alpha=.9) +
    geom_segment(data=base %>% 
                   filter(!duplicated(id_ap)), 
                 aes(coord_x, coord_y, xend=xend, yend=yend, 
                     color=factor(cluster)), 
                 show.legend = F, 
                 linewidth=1, linetype="dashed") +
    theme_void() +
    scale_color_manual(values = cluster_imputados) +
    theme(legend.position = "bottom") +
    labs(color="clúster")
  
  ggsave(plot = mapas[[i]] ,
         paste0("mapa_redes_imputados_2_", i, ".png"),
         width = 10, height = 10, dpi = 300)
  
  
}
