###########################################
## Script con funciones comunes
## Aliza Brzezinski, miau, fabi y jesus sin grafica
###########################################

pacman::p_load(tidyverse, lubridate, scales, janitor, 
               cowplot, gridExtra, flextable, officer, grid, 
               forecast)

# fecha_inicio_global <- "2018-01-01"
fecha_inicio_global <- "2022-01-01"

## Vectores de colores ---------------------------------------------------------
colores <- c("#233d4d", "#fe7f2d", "#fcca46", "#a1c181", "#619b8a",
             "#606c38", "#e94f51", "#577590", "gray65", "#E41A1C",
             "#377EB8", "#4DAF4A", "#984EA3","#20a29A", "#A65628", "#FBBB27")

col_aos <- c("2018" = "#00b140",
             "2019" = "#164a80",
             "2020" = "#ff4f4f",
             "2021" = "#f9a819",
             "2022" = "#4EA5D9", 
             "2023" = "#7fc19d", 
             "2024" = "#C1446b"
             )


col_aos <- c("2022"="#CD661D",
             "2023"="#164a80", 
             "2024"="#9F2241", 
             "2025"="#BC955C"
)

col_aos2 <- c("2022"="#CD661D",
             "2023"="#164a80", 
             "2024"="#9F2241", 
             "2025"="#BC955C"
)

col_aos2 <- c("2018" = "#00b140",
             "2019" = "#164a80",
             "2020" = "#ff4f4f",
             "2021" = "#f9a819",
             "2022" = "#4EA5D9", 
             "2023" = "#7fc19d",
             "2024" = "#C1446b", 
             "2017" = "#233d4d",
             "2016" = "#fe7f2d",
             "2015" = "#fcca46",
             "2014" = "#a1c181",
             "2013" = "#606c38",
             "2012" = "#619b8a",
             "2011" = "#984EA3")

colores_fgj <- c("#003763", "#bb0b24", "#20a29A", "#651080", "#faca38", "#a9007b")

#tema para gráficas
tema_fgj <- theme(text = element_text(size = 16), 
                  strip.text = element_text(face = "bold", color="black"))

## Tema para tablas ------------------------------------------------------------
tema <- ttheme_minimal(
  base_size = 10,
  padding = unit(c(4,4), "mm"),
  colhead = list(fg_params = list(col = "white", fontface = "bold"),
                 bg_params = list(fill=c("#003763"))))

tema2 <- ttheme_minimal(
  base_size = 14,
  padding = unit(c(4,4), "mm"),
  colhead = list(fg_params = list(col = "white", fontface = "bold"),
                 bg_params = list(fill=c("#003763"))))



tema_flextable <- function(x, ...) {
  x <- colformat_double(x, big.mark = "'", decimal.mark = ",", digits = 1)
  x <- set_table_properties(x, layout = "fixed")
  x <- border_remove(x)
  std_border <- fp_border(width = .5, color = "gray")
  x <- border_outer(x, part="all", border = std_border )
  x <- border_inner_h(x, border = std_border, part="all")
  x <- border_inner_v(x, border = std_border, part="all")
  # Agregar color azul cielo a la última fila
  x <- bg(x, part = "header", bg = "#003763")
  x <- fontsize(x, size = 10, part = "all")
  x <- bold(x, j = 1)
  x <- bold(x, part = "header")
  x <- color(x, part = "header", color = "white")
  x <- align(x, align = "center", part = "all")
  x <- bg(x, bg = "#bde0fe", part = "body", i = dim(x$body$dataset)[1])
  x <- bold(x, i = dim(x$body$dataset)[1])
  x <- set_formatter(x, ~ format(., big.mark = ","))
  #x <- width(x, width = 1) # Ancho de las columnas en pulgadas 
  #x <- height(x, height = 5) # Alto de las filas en pulgadas
  x<- autofit(x)
}



library(flextable)

library(grid)
library(officer)
std_border = fp_border(color="gray", width = 1)

## Información de las alcaldías ------------------------------------------------
info_alcs <- 
  tibble(
    delegacion_hechos = c(
      "ALVARO OBREGON", "AZCAPOTZALCO", "BENITO JUAREZ", "COYOACAN",
      "CUAJIMALPA DE MORELOS", "CUAUHTEMOC", "GUSTAVO A MADERO", "IZTACALCO",
      "IZTAPALAPA", "LA MAGDALENA CONTRERAS", "MIGUEL HIDALGO", "MILPA ALTA",
      "TLAHUAC", "TLALPAN", "VENUSTIANO CARRANZA", "XOCHIMILCO"
    ),
    clave = c(
      "AO", "AZ", "BJ", "COY", "CJ", "CUH", "GAM", "IZC",
      "IZP", "MC", "MH", "MIL", "TLH", "TLP", "VC", "XO"
    ),
    alcaldia = c(
      "Álvaro Obregón", "Azcapotzalco",  "Benito Juárez", "Coyoacán",
      "Cuajimalpa de Morelos", "Cuauhtémoc", "Gustavo A. Madero", "Iztacalco",
      "Iztapalapa", "La Magdalena Contreras",  "Miguel Hidalgo", "Milpa Alta",
      "Tláhuac", "Tlalpan",  "Venustiano Carranza",  "Xochimilco"
    )
  )

## Armado de bases -------------------------------------------------------------
## Conjunta las bases de incidencia de los diferentes años y filtra unicamente
## las carpetas de fuero comun.
## Se puede armar la base tanto de víctimas como de inicidencia y elegir si 
## tomar la información del mes en curso o solo las carpetas cerradas

arma_base <- function(
  tipo = c("incidencia", "victimas"), 
  wd = "../../", 
  #wd = "Y:/1 BASES UET/Carpetas iniciadas",
  momento = c("abierto", "cerrado")
) {
  path_incidencia <- paste0(wd#, "1 BASES UET/Carpetas iniciadas"
                            )
  if(tipo == "incidencia") {
    b18 <-
      read_csv(paste0(path_incidencia, "/2018_INICIADAS.csv"), 
               locale = readr::locale(encoding = "latin1")) %>%
      clean_names()
    b19 <-
      read_csv(paste0(path_incidencia, "/2019_INICIADAS.csv"), 
               locale = readr::locale(encoding = "latin1")) %>%
      clean_names()
    b20 <-
      read_csv(paste0(path_incidencia, "/2020_INICIADAS.csv"), 
               locale = readr::locale(encoding = "latin1")) %>%
      clean_names()
    
    b21 <-
      read_csv(paste0(path_incidencia, "/2021_INICIADAS.csv"),
               locale = readr::locale(encoding = "latin1")) %>%
      clean_names()

    if(momento == "abierto") {

      nuevo <- list.files(path = path_incidencia,
                          pattern = "2022.csv", full.names = T)

      nuevo <- nuevo[which(!grepl("\\$", nuevo))]


      b22 <-
        read_csv(nuevo,
                 locale = readr::locale(encoding = "latin1")) %>%
        clean_names()
      # b22 <-
      #   read_csv(nuevo,
      #            locale = readr::locale(encoding = "latin1")) %>%
      #   clean_names()

    } else {
      
      nuevo <- list.files(path = path_incidencia,
                          pattern = "2022.csv", full.names = T)
      
      nuevo <- nuevo[which(!grepl("\\$", nuevo))]
      date_filter <- floor_date(today(), "month")-1
      
      b22 <-
        read_csv(nuevo,
                 locale = readr::locale(encoding = "latin1")) %>%
        clean_names() %>% 
        filter(month(dmy(fecha_de_inicio))<=month(date_filter))
      # b22 <-
      #   read_csv(paste0(path_incidencia, "/2022_INICIADAS.csv"),
      #            locale = readr::locale(encoding = "latin1")) %>%
      #   clean_names()
      # b22 <-
      #   read_csv(paste0(path_incidencia, "/2022_INICIADAS.csv"),
      #            locale = readr::locale(encoding = "latin1")) %>%
      #   clean_names()
    }
    
    
    base <-
      bind_rows(b18, b19, b20, b21, b22
                ) %>%
      # bind_rows(b18, b19, b20, b21, b22) %>%
      filter(grepl("FUERO CO", competencia)) %>%
      mutate(fecha_inicio = dmy(fecha_de_inicio),
             fecha_hechos = dmy(fecha_de_los_hechos))
    
  } else {
    path_incidencia <- paste0(path_incidencia, "/victimas")
    b18 <- 
      read_csv(paste0(path_incidencia, "/VICTIMAS_2018.csv"), 
               locale = readr::locale(encoding = "latin1")) %>%
      clean_names()
    b19 <- 
      read_csv(paste0(path_incidencia, "/VICTIMAS_2019.csv"), 
               locale = readr::locale(encoding = "latin1")) %>%
      clean_names()
    b20 <- 
      read_csv(paste0(path_incidencia, "/VICTIMAS_2020.csv"), 
               locale = readr::locale(encoding = "latin1")) %>%
      clean_names()
    b21 <- 
      read_csv(paste0(path_incidencia, "/VICTIMAS_2021.csv"), 
      # read_csv(paste0(path_incidencia, "/VICTIMAS_2021 INICIADAS.csv"), 
               locale = readr::locale(encoding = "latin1")) %>%
      clean_names() 
    
    if(momento == "abierto") {
     # nuevo <- list.files(path = path_incidencia, pattern = " 2022 INICIADAS.csv", full.names = T)
      nuevo <- list.files(path = path_incidencia,
                          pattern = " 2022.csv", full.names = T)[1]
      if(!is_empty(nuevo)) {
        
        b22 <-
          read_csv(nuevo, 
                   locale = readr::locale(encoding = "latin1")) %>%
          clean_names() 
        
        base <- 
          bind_rows(b18, b19, b20, b21, b22) %>% 
          mutate(fecha_inicio = dmy(fecha_de_inicio))
        
      } else {

        base <- 
          bind_rows(b18, b19, b20, b21) %>% 
          mutate(fecha_inicio = dmy(fecha_de_inicio))
      }
      
    } else {
      
      nuevo <- list.files(path = path_incidencia,
                          pattern = "2022.csv", full.names = T)[1]
      
      nuevo <- nuevo[which(!grepl("\\$", nuevo))]
      date_filter <- floor_date(today(), "month")-1
      
      b22 <-
        read_csv(nuevo,
                 locale = readr::locale(encoding = "latin1")) %>%
        clean_names() %>% 
        filter(month(dmy(fecha_de_inicio))<=month(date_filter))
      base <- 
        bind_rows(b18, b19, b20, b21, b22) %>% 
        mutate(fecha_inicio = dmy(fecha_de_inicio))
    }
    
    
  }
  
  return(base)
  
}

arma_base2 <- function(
  tipo = c("incidencia", "victimas"), 
  wd = "../../", 
  #wd = "Y:/1 BASES UET/Carpetas iniciadas",
  momento = c("abierto", "cerrado"), 
  fuero_comun=c("fuero comun", "iniciadas")
) {
  library(parallel)
  # numCores <- detectCores()
  # cl <- makeCluster(detectCores())
  path_incidencia <- wd
  

  # path_incidencia_2022 <- paste0(wd, "/2022_INICIADAS.csv"
  # )
  
  
  if(tipo == "incidencia") {
    
    
    #este vector va desde 2018 hasta el año anterior al actual. 
     años <- seq(2018, year(Sys.Date())-1)
    
    paths <- list()
    #generamos el ciclo para jalar todas las bases de datos desde 2018 al año anterior al actual
    for(i in 1:length(años)){
      paths[[i]] <- paste0(wd, "/", años[i], "_INICIADAS.csv")
      
    }
    
    
    # path_incidencia_2018 <- paste0(wd, "/2018_INICIADAS.csv"
    # )
    # path_incidencia_2019 <- paste0(wd, "/2019_INICIADAS.csv"
    # )
    # path_incidencia_2020 <- paste0(wd, "/2020_INICIADAS.csv"
    # )
    # path_incidencia_2021 <- paste0(wd, "/2021_INICIADAS.csv"
    # )
    
    #jalamos la base del año actual
    nuevo <- list.files(path = path_incidencia,
                        pattern = paste0(year(Sys.Date()),".csv"), full.names = T)[1]
    
    nuevo <- nuevo[which(!grepl("\\$", nuevo))]
    
    
    if (is.na(nuevo)) {
      paths <- c(paths)
    } else{
      
      paths <- c(paths, nuevo)
    }
    # paths <- c(path_incidencia_2018, path_incidencia_2019, path_incidencia_2020, 
    #            path_incidencia_2021, nuevo
    #            )
    
    #juntamos todas las bases de datos que vamos unir
    


    # 
    # All <- lapply(paths, function(i){
    #   # paths <- paste(".\\",i,sep="")
    #   read_csv(i, locale = readr::locale(encoding = "latin1")) %>% 
    #     clean_names()
    # })
    
    #generamos la incorporación de bases de datos por programación en parelelo para eficientar los procesos
    cl <- makeCluster(detectCores())
    clusterEvalQ(cl, {
      library(dplyr)
      library(janitor)
    })
    
    
    
    a <- parLapply(cl, paths, function(path) {
      tryCatch({
        read.csv(path, encoding = "latin1") %>%
          janitor::clean_names() %>%
          mutate(id_ap=as.integer(id_ap), 
                            coord_x=as.numeric(coord_x),
                            coord_y=as.numeric(coord_y))
      }, error = function(e) {
        warning(paste("Error al leer el archivo:", path, "\n", e))
        return(NULL)
      })
    })
    stopCluster(cl)
    
    
    
    
    
    
    #a <- parLapply(cl,paths,function(...)try(read.csv(...)) %>%
     #                clean_names() %>% 
                     
  #  stopCluster(cl)
    
    if(fuero_comun=="fuero comun"){
      a <- bind_rows(a) %>% #clean_names() %>% 
        filter(grepl("FUERO CO", competencia)) %>%
        mutate(fecha_inicio = dmy(fecha_de_inicio), 
               fecha_hechos = dmy(fecha_de_los_hechos)) %>% as_tibble()
      
    } else {
      a <- bind_rows(a) %>% #clean_names() %>% 
        #filter(grepl("FUERO CO", competencia)) %>%
        mutate(fecha_inicio = dmy(fecha_de_inicio), 
               fecha_hechos = dmy(fecha_de_los_hechos)) %>% as_tibble()
      
    }

    
    if(momento == "abierto") {
      
      base <- a
      
    } else {
      
      date_filter <- floor_date(today(), "month")-1
      
      base <- a %>% 
        filter(fecha_inicio<=date_filter)
      # b22 <-
      #   read_csv(paste0(path_incidencia, "/2022_INICIADAS.csv"),
      #            locale = readr::locale(encoding = "latin1")) %>%
      #   clean_names()
      # b22 <-
      #   read_csv(paste0(path_incidencia, "/2022_INICIADAS.csv"),
      #            locale = readr::locale(encoding = "latin1")) %>%
      #   clean_names()
    }
  } else {
    
    #este vector va desde 2018 hasta el año anterior al actual. 
    años <- seq(2018, year(Sys.Date())-1)
    
    #modificaomos el path
    path_victimas <- paste0(path_incidencia, "/victimas")
    
    paths_vic <- list()
    #generamos el ciclo para jalar todas las bases de datos desde 2018 al añor anterior al actual
    for(i in 1:length(años)){
      paths_vic[[i]] <- paste0(path_victimas, "/VICTIMAS_", años[i], ".csv")
      
    }
    
    
    # 
    # path_victimas_2018 <- paste0(path_victimas, "/VICTIMAS_2018.csv"
    # )
    # path_victimas_2019 <- paste0(path_victimas, "/VICTIMAS_2019.csv"
    # )
    # path_victimas_2020 <- paste0(path_victimas, "/VICTIMAS_2020.csv"
    # )
    # path_victimas_2021 <- paste0(path_victimas, "/VICTIMAS_2021.csv"
    # )
    
    nuevo <- list.files(path = path_victimas,
                        pattern = paste0(year(Sys.Date()), ".csv"), full.names = T)[1]
    
    nuevo <- nuevo[which(!grepl("\\$", nuevo))]
    
    if (is.na(nuevo)) {
      paths <- c(paths_vic)
    } else{
      
      paths <- c(paths_vic, nuevo)
    }
    
   
    
    gato <- function(x){
      read.csv()
    }
    
    numCores <- detectCores()
    cl <- makeCluster(detectCores())
    clusterEvalQ(cl, {
      library(dplyr)
      library(janitor)
    })
   
    a <- parLapply(cl, paths, function(path) {
      tryCatch({
        read.csv(path, encoding = "latin1") %>%
          janitor::clean_names() %>%
          mutate(id_ap = as.integer(id_ap))
      }, error = function(e) {
        warning(paste("Error al leer el archivo:", path, "\n", e))
        return(NULL)
      })
    })
    stopCluster(cl)
    
    
     #a <- parLapply(cl,paths,function(...)try(read.csv(...)) %>%
      #               clean_names() %>% 
     #                mutate(id_ap=as.integer(id_ap)))
    #stopCluster(cl)
    
    if(fuero_comun=="fuero comun"){
      a <- bind_rows(a) %>% #clean_names() %>% 
        filter(grepl("FUERO CO", competencia)| is.na(competencia)) %>%
        mutate(fecha_inicio = dmy(fecha_de_inicio)) %>% as_tibble()
      
    } else {
      
      a <- bind_rows(a) %>% #clean_names() %>% 
        #filter(grepl("FUERO CO", competencia)) %>%
        mutate(fecha_inicio = dmy(fecha_de_inicio)) %>% as_tibble()
    }

    
    
    if(momento == "abierto") {
      base <- a
        
      } else {
        
        date_filter <- floor_date(today(), "month")-1
        
        base <- a %>% 
          filter(fecha_inicio<=date_filter)
      }
      
    #} else {
      
    #   nuevo <- list.files(path = path_incidencia,
    #                       pattern = "2022.csv", full.names = T)[1]
    #   
    #   nuevo <- nuevo[which(!grepl("\\$", nuevo))]
    #   date_filter <- floor_date(today(), "month")-1
    #   
    #   b22 <-
    #     read_csv(nuevo,
    #              locale = readr::locale(encoding = "latin1")) %>%
    #     clean_names() %>% 
    #     filter(month(dmy(fecha_de_inicio))<=month(date_filter))
    #   base <- 
    #     bind_rows(b18, b19, b20, b21, b22) %>% 
    #     mutate(fecha_inicio = dmy(fecha_de_inicio))
    # }
    
    
  }
  
  #creamos variable tipo_impacto_adip
  if(fuero_comun=="fuero comun") {
    base<-base %>% 
      mutate(tipo_impacto_adip=case_when(
        delito=="SECUESTRO" ~ "BAJO IMPACTO",
        delito=="VIOLACIÓN" ~ "BAJO IMPACTO",
        grepl("METROB", modalidad_delito) ~ "ALTO IMPACTO",
        grepl("ROBO A CASA HAB", modalidad_delito) ~ "ALTO IMPACTO",
        modalidad_delito=="ROBO A PASAJERO / CONDUCTOR DE VEHICULO CON VIOLENCIA" ~ "ALTO IMPACTO",
        tipo_impacto=="ALTO IMPACTO" ~ "ALTO IMPACTO",
        tipo_impacto=="BAJO IMPACTO" ~ "BAJO IMPACTO",
        T ~ "INDETERMINADO"
      ))
  } else {
    base<-base %>% 
      mutate(tipo_impacto_adip=case_when(
        delito=="SECUESTRO" ~ "BAJO IMPACTO",
        delito=="VIOLACIÓN" ~ "BAJO IMPACTO",
        grepl("METROB", modalidad_delito) ~ "ALTO IMPACTO",
        grepl("ROBO A CASA HAB", modalidad_delito) ~ "ALTO IMPACTO",
        modalidad_delito=="ROBO A PASAJERO / CONDUCTOR DE VEHICULO CON VIOLENCIA" ~ "ALTO IMPACTO",
        tipo_impacto=="ALTO IMPACTO" ~ "ALTO IMPACTO",
        tipo_impacto=="BAJO IMPACTO" ~ "BAJO IMPACTO",
        tipo_impacto=="INDETERMINADO" ~ "INDETERMINADO",
        T ~ "---"
      ))
  }
  
  return(base)
  
}

## Esta función es similar a la anterior, pero funciona solo para incidencia y
## toma todas las carpetas iniciadas, sin filtrar el fuero

arma_base_iniciadas <- function(
  wd = "../../", 
  momento = c("abierto", "cerrado")
) {
  path_incidencia <- paste0(wd, "1 BASES UET/Carpetas iniciadas")
 
    b18 <-
      read_csv(paste0(path_incidencia, "/2018_INICIADAS.csv"), 
               locale = readr::locale(encoding = "latin1")) %>%
      clean_names()
    b19 <-
      read_csv(paste0(path_incidencia, "/2019_INICIADAS.csv"), 
               locale = readr::locale(encoding = "latin1")) %>%
      clean_names()
    b20 <-
      read_csv(paste0(path_incidencia, "/2020_INICIADAS.csv"), 
               locale = readr::locale(encoding = "latin1")) %>%
      clean_names()
    
    if(momento == "abierto") {
      
      nuevo <- list.files(path = path_incidencia, 
                          pattern = "2021.csv", full.names = T)
      
      nuevo <- nuevo[which(!grepl("\\$", nuevo))]
      
      
      b21 <-
        read_csv(nuevo, 
                 locale = readr::locale(encoding = "latin1")) %>%
        clean_names() #%>%
      # rename(
      #   "coord_x" = lat,
      #   "coord_y" = long
      # )
      
    } else {
      b21 <-
        read_csv(paste0(path_incidencia, "/2021_INICIADAS.csv"), 
                 locale = readr::locale(encoding = "latin1")) %>%
        clean_names()
    }
    
    
    base <-
      bind_rows(b18, b19, b20, b21) %>%
      mutate(fecha_inicio = dmy(fecha_de_inicio),
             fecha_hechos = dmy(fecha_de_los_hechos))
    
 
  
  return(base)
  
}

## Extraer base de género (delitos puras víctimas mujeres) ---------------------
arma_base_genero <- function(
  wd = "../../", 
  tipo = c("incidencia", "victimas")
) {
  
  base_genero <- list.files(paste0(wd, "1 BASES UET/Violencia de género/"),
                            pattern = "VIOLENCIA_DE_GENERO", full.names = TRUE)
  
  if(tipo == "incidencia") {
    base <- 
      readxl::read_xlsx(base_genero, sheet = "INCIDENCIA") %>% 
      clean_names() %>% 
      mutate(
        fecha_inicio = as_date(fecha_de_inicio),
        fecha_hechos = as_date(fecha_de_los_hechos),
        hora_de_los_hechos = str_sub(hora_de_los_hechos, 12, 19)
      ) %>% 
      rename(id_ap = id)
  } else {
    base <-     
      readxl::read_xlsx(base_genero, sheet = "VICTIMAS") %>% 
      clean_names()
  }
  
  return(base)  
  
}







## Filtrado de bases -----------------------------------------------------------
## Esta función filtra las bases de incidencia y víctimas por delito y/o alcaldia;
## esta construida de forma que filtra la base de incidencia y, en caso de que 
## tambien tenga que arrojar la de victimas, solo filtra con base en el id de la
## carpeta. Al final arroja una lista. 
## Para reportes "rapidos" es mas util filtrar al momento

filtra_bases <- function(
  base = base,  #base de incidencia
  base_victimas = base_victimas, # base de victimas
  ambas = TRUE, #si solo se va a usar incidencia, cambiar a FALSE
  fecha_comienzo = fecha_inicio_global,
  fecha_lim = fecha_lim,
  delitos_sel = delitos_sel,
  alto_impacto = alto_impacto,
  alcaldia_sel = alcaldia_sel
) {
  
  base <- 
    base %>% 
    filter(fecha_inicio >= fecha_comienzo & fecha_inicio <= fecha_lim)
  
  if (alcaldia_sel != "todas") {
    base <- 
      base %>% 
      filter(delegacion_hechos == str_to_upper(alcaldia_sel))
  }
  
  if(paste(delitos_sel, collapse = ", ") == "TOTAL DE DELITOS") {
    base_use <- 
      base 
    
  } else if (paste(delitos_sel, collapse = ", ") ==  "TOTAL DE ALTO IMPACTO") {
    base_use <- 
      base %>% 
      filter(tipo_impacto == "ALTO IMPACTO")
    
  } else if (paste(delitos_sel, collapse = ", ") == "TOTAL DE BAJO IMPACTO") {
    base_use <- 
      base %>% 
      filter(tipo_impacto == "BAJO IMPACTO")
    
  } else if(paste(delitos_sel, collapse = ", ") %in% c("ROBO DE VEHÍCULO CON VIOLENCIA", "ROBO DE VEHÍCULO SIN VIOLENCIA")){
    base_use <- 
      base %>% 
      filter(delito == "ROBO DE VEHÍCULO CON Y SIN VIOLENCIA") %>% 
      mutate(delito_final = ifelse(grepl("CON VIOLENCIA|C/V", modalidad_delito), 
                                   "ROBO DE VEHÍCULO CON VIOLENCIA", 
                                   "ROBO DE VEHÍCULO SIN VIOLENCIA")) %>% 
      filter(delito_final %in% delitos_sel) #Se le cambió %in% por ==
    
  } else if (alto_impacto == "alto") {
    base_use <- 
      base %>% 
      filter(delito %in% delitos_sel)
    
  } else {
    base_use <- 
      base %>% 
      filter(modalidad_delito %in% delitos_sel)
    
  }
  if (ambas == TRUE) {
    base_vic <- 
      base_victimas %>% 
      filter(id_ap %in% base_use$id_ap)
    
    out <- list(base_use, base_vic)
  } else {
    out <- list(base_use)
  }
  return(out)
}


filtra_base_genero <- function(
  base = base, 
  delitos_sel = delito_sel, 
  alcaldia_sel= alcaldia_sel, 
  fecha_comienzo = fecha_inicio_global, 
  fecha_lim = fecha_lim) {
  
  if(alcaldia_sel != "todas") {
    base <- 
      base %>% 
      filter(delegacion_hechos == alcaldia_sel)
  }
  
  base <- 
    base %>%
    filter(delito_homologado %in% delitos_sel & fecha_inicio >= fecha_comienzo & 
             fecha_inicio <= fecha_lim)
  
  return(base)
}

## Generar variable de fiscalía de inicio --------------------------------------
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


## Gráfica de comparación por años (semanas) -----------------------------------
plot_barras <- function(
  base, 
  tipo = c("incidencia", "victimas", "pads"), 
  nombre = str_to_lower(nombre_plots),
  fecha_comienzo = fecha_inicio_global,
  fecha_lim = fecha_lim,
  tema_tablas = tema
) {
  
  if (tipo == "pads") {
    serie <- 
      base %>% 
      mutate(existe = 1) %>% 
      complete(fecha_inicio = seq.Date(from = as_date("2019-01-01"),
                                       to = as_date(fecha_lim),
                                       by = "day"),
               fill = list(existe = 0)) %>% 
      group_by(
        # semana = isoweek(fecha_inicio),
        semana = floor_date(fecha_inicio, "week", week_start = 6)
        # ao = year(fecha_inicio)
      ) %>% 
      summarise(
        tot_semana = sum(existe),
        fr = min(fecha_inicio),
        to = max(fecha_inicio),
        .groups = "drop"
      ) %>% 
      mutate(
        ao = year(semana),
        keep = ifelse(semana == fr, 1, 0)
      ) %>% 
      filter(keep == 1) %>% 
      group_split(ao) %>% 
      map(~rowid_to_column(.x, "semana_use")) %>% 
      bind_rows()
  } else {
    
  serie <- 
    base %>% 
    mutate(existe = 1) %>% 
    complete(fecha_inicio = seq.Date(from = as_date(fecha_comienzo),
                                     to = as_date(fecha_lim),
                                     by = "day"),
             fill = list(existe = 0)) %>% 
    group_by(
      # semana = isoweek(fecha_inicio),
      semana = floor_date(fecha_inicio, "week", week_start = 6)
      # ao = year(fecha_inicio)
    ) %>% 
    summarise(
      tot_semana = sum(existe),
      fr = min(fecha_inicio),
      to = max(fecha_inicio),
      .groups = "drop"
    ) %>% 
    mutate(
      ao = year(semana),
      keep = ifelse(semana == fr, 1, 0)
    ) %>% 
    filter(keep == 1) %>% 
    group_split(ao) %>% 
    map(~rowid_to_column(.x, "semana_use")) %>% 
    bind_rows()
  }
  
  
  dat_21 <- 
    serie %>% 
    filter(ao == max(ao))
  
  
  dat_ant <- 
    serie %>% 
    filter(ao != max(ao))
  
  sem <- as.numeric(max(dat_21$semana_use))
  
  # Plot
  serie_semanas <- 
    ggplot() +
    geom_bar(data = dat_21,
             aes(x = semana_use, y = tot_semana, color = factor(ao), fill = factor(ao)),
             stat = "identity", position = "dodge", alpha = .6, width = .6, show.legend = F) +
    geom_point(data = dat_ant %>% 
                 filter(semana_use != sem), 
               aes(x = semana_use, y = tot_semana, color = factor(ao)),
               alpha = .3, show.legend = F) +
    geom_point(data = dat_ant %>% 
                 filter(semana_use == sem), 
               aes(x = semana_use, y = tot_semana, color = factor(ao)),
               alpha = .9, show.legend = F, size = 2) +
    geom_smooth(data = dat_ant, 
                aes(x = semana_use, y = tot_semana, color = factor(ao)),
                se = F, span = .65) +
    labs(
      title = str_wrap(paste0("Incidencia delictiva - ", nombre),65),
      subtitle = paste0("Corte al ", format(as_date(fecha_lim), "%A %d de %B, %Y")),
      x = "Semana",
      y = "Carpetas iniciadas por semana",
      caption = "Semanas de sábado a viernes",
      color = "Año de inicio de las carpetas",
      fill = "Año de inicio de las carpetas"
    ) +
    scale_y_continuous(label = comma_format(accuracy = 1)) +
    scale_color_manual(values = col_aos, aesthetics = c("color", "fill")) +
    theme_light() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold")
    ) 
  
  if (tipo == "victimas") {
    serie_semanas <- 
      serie_semanas +
      labs(
        title = str_wrap(paste0("Incidencia delictiva (víctimas) - ", nombre),65)
        )
  }
  
  if (tipo == "pads") {
    serie_semanas <- 
      serie_semanas +
      labs(
        title = str_wrap(paste0("Puestas a disposición (flagrancia) - ", nombre),65),
           y = "Personas puestas a disposición"
          )
  }
  

  
  
  if(tipo == "pads") {
    # Tabla 1
    
    tabla_totales <- 
      base %>% 
      group_by(
        fecha_inicio
      ) %>% 
      summarise(
        tot_dia = n(),
        .groups = "drop"
      ) %>% 
      complete(fecha_inicio = seq.Date(from = as_date("2019-01-01"),
                                       to = as_date(fecha_lim),
                                       by = "day"),
               fill = list(tot_dia = 0)) %>% 
      group_by(ao = year(fecha_inicio)) %>% 
      summarise(Total = comma(sum(tot_dia), accuracy =  1),
                "Prom. diario" = comma(round(mean(tot_dia), 2), accuracy = .01)) %>% 
      pivot_longer(cols = -ao, names_to = " ") %>% 
      pivot_wider(names_from = ao, values_from = value)
    
    tb <- 
      tableGrob(tabla_totales, rows = NULL, theme = tema_tablas) %>% 
      gtable_add_grob(grobs = replicate(ncol(.) - 1,
                                        segmentsGrob(x1 = unit(0, "npc"), 
                                                     gp = gpar(lty = 2)),
                                        simplify = FALSE),
                      t = 2, b = nrow(.), l = seq_len(ncol(.) - 1) + 1) %>% 
      gtable_add_grob(grobs = segmentsGrob( # line across the bottom
        x0 = unit(0,"npc"),
        y0 = unit(0,"npc"),
        x1 = unit(1,"npc"),
        y1 = unit(0,"npc"),
        gp = gpar(lty = 2.0)),
        t = 2, b = 2, l = 1, r = ncol(.))
    
  tabla_comp <-
    tibble(
      Iniciadas = dat_21$tot_semana[dat_21$semana_use == sem],
      "Semana anterior" = dat_21$tot_semana[dat_21$semana_use == (sem - 1)],
      "Promedio semanal\nde la gestión" = round(mean(serie$tot_semana), 2),
      "Misma semana\n (Año anterior)" = serie$tot_semana[serie$semana_use == sem & serie$ao == max(serie$ao)-1]
    ) %>%
    pivot_longer(-Iniciadas, names_to = "tiempo") %>%
    mutate(
      diferencia = Iniciadas - value,
      color = ifelse(diferencia < 0, "#f94144", "#90be6d"),
      cambio = (Iniciadas/value) - 1,
      value = comma(value, accuracy = .1),
      texto = paste0("Diferencia: ", comma(diferencia, accuracy = .1),
                     "\n(", percent(cambio, accuracy = .1), ")")
    ) %>%
    select(-c(Iniciadas, diferencia, cambio)) %>%
    pivot_longer(-c(tiempo, color), values_to = "valor") %>%
    mutate(
      name = factor(name, levels = c("value", "texto")),
      color = ifelse(name == "value", "black", color)
    )
  } else {
    
    # Tabla 1
    
    tabla_totales <- 
      base %>% 
      group_by(
        fecha_inicio
      ) %>% 
      summarise(
        tot_dia = n(),
        .groups = "drop"
      ) %>% 
      complete(fecha_inicio = seq.Date(from = as_date(fecha_comienzo),
                                       to = as_date(fecha_lim),
                                       by = "day"),
               fill = list(tot_dia = 0)) %>% 
      group_by(ao = year(fecha_inicio)) %>% 
      summarise(Total = comma(sum(tot_dia), accuracy =  1),
                "Prom. diario" = comma(round(mean(tot_dia), 2), accuracy = .01)) %>% 
      pivot_longer(cols = -ao, names_to = " ") %>% 
      pivot_wider(names_from = ao, values_from = value)
    
    tb <- 
      tableGrob(tabla_totales, rows = NULL, theme = tema_tablas) %>% 
      gtable_add_grob(grobs = replicate(ncol(.) - 1,
                                        segmentsGrob(x1 = unit(0, "npc"), 
                                                     gp = gpar(lty = 2)),
                                        simplify = FALSE),
                      t = 2, b = nrow(.), l = seq_len(ncol(.) - 1) + 1) %>% 
      gtable_add_grob(grobs = segmentsGrob( # line across the bottom
        x0 = unit(0,"npc"),
        y0 = unit(0,"npc"),
        x1 = unit(1,"npc"),
        y1 = unit(0,"npc"),
        gp = gpar(lty = 2.0)),
        t = 2, b = 2, l = 1, r = ncol(.))
    
    
    
    tabla_comp <-
      tibble(
        Iniciadas = dat_21$tot_semana[dat_21$semana_use == sem],
        "Semana anterior" = dat_21$tot_semana[dat_21$semana_use == (sem - 1)],
        "Promedio semanal\nde la gestión" = round(mean(serie$tot_semana), 2),
        "Misma semana\n (Año anterior)" = serie$tot_semana[serie$semana_use == sem & serie$ao == max(serie$ao)-1]
      ) %>%
      pivot_longer(-Iniciadas, names_to = "tiempo") %>%
      mutate(
        diferencia = Iniciadas - value,
        color = ifelse(diferencia > 0, "#f94144", "#90be6d"),
        cambio = (Iniciadas/value) - 1,
        value = comma(value, accuracy = .1),
        texto = paste0("Diferencia: ", comma(diferencia, accuracy = .1),
                       "\n(", percent(cambio, accuracy = .1), ")")
      ) %>%
      select(-c(Iniciadas, diferencia, cambio)) %>%
      pivot_longer(-c(tiempo, color), values_to = "valor") %>%
      mutate(
        name = factor(name, levels = c("value", "texto")),
        color = ifelse(name == "value", "black", color)
      )
  }
  
  
  comparacion <-
    ggplot(tabla_comp,
           aes(x = name, y = tiempo)) +
    geom_tile( fill = NA) +
    geom_text(aes(label = valor, color = color), size = 3.2, fontface = "bold") +
    labs(
      y = paste0(comma(dat_21$tot_semana[dat_21$semana_use == sem]), "\n\nCI iniciadas")
    ) +
    theme_void() +
    theme_minimal() +
    geom_vline(xintercept = c(.5, 1.5), alpha = .5, color = "gray75") +
    geom_hline(yintercept = c(1.5, 2.5), alpha = .5, color = "gray75") +
    scale_fill_identity(aesthetics = c("fill", "color")) +
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.text.y = element_text(hjust = 1, size = 9),
      axis.title.y = element_text(size = 14, face = "bold", color = col_aos[4],
                                  vjust = .5, angle = 0),
      axis.line = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank()
    )
  
  
  
  t_fin <- plot_grid(comparacion, tb, 
                     nrow = 1, rel_widths = c(.4, .3))
  final <- plot_grid(serie_semanas, t_fin, 
                     nrow = 2, rel_heights = c(2, 0.5))
  
  return(final)
}

## Gráfica de comparación por años por alcaldía (semanas) ----------------------
plot_barras_alc <- function(
  base,
  tipo = c("incidencia", "victimas"),
  nombre = str_to_lower(nombre_plots),
  fecha_comienzo = fecha_inicio_global,
  fecha_lim = fecha_lim,
  info_alcs = info_alcs
) {
  
  
  crea_serie <- function(base) {
    b <- 
      base %>%
      mutate(existe = 1) %>%
      complete(fecha_inicio = seq.Date(from = as_date(fecha_comienzo),
                                       to = as_date(fecha_lim),
                                       by = "day"),
               fill = list(existe = 0)) %>%
      group_by(
        delegacion_hechos,
        # semana = isoweek(fecha_inicio),
        semana = floor_date(fecha_inicio, "week", week_start = 6)
        # ao = year(fecha_inicio)
      ) %>%
      summarise(
        tot_semana = sum(existe),
        fr = min(fecha_inicio),
        to = max(fecha_inicio),
        .groups = "drop"
      ) %>%
      mutate(
        ao = year(semana),
        keep = ifelse(semana == fr, 1, 0)
      ) %>%
      filter(keep == 1) %>%
      group_split(ao) %>%
      map(~arrange(.x, semana)) %>% 
      map(~rowid_to_column(.x, "semana_use")) %>%
      bind_rows()
    
    alc <- unique(b$delegacion_hechos)
    alc <- alc[which(!is.na(alc))]
    
    
    b %>% 
      mutate(delegacion_hechos = alc)
  }
  
  
  serie <- 
    base %>% 
    drop_na(delegacion_hechos) %>% 
    group_split(delegacion_hechos) %>% 
    map_df(.,~crea_serie(.)) %>% 
    left_join(info_alcs)
  
  dat_21 <-
    serie %>%
    filter(ao == max(ao))
  
  
  dat_ant <-
    serie %>%
    filter(ao != max(ao))
  
  sem <- as.numeric(max(dat_21$semana_use))
  
  # col_aos <- c("2018" = "#00b140",
  #              "2019" = "#164a80",
  #              "2020" = "#ff4f4f",
  #              "2021" = "#f9a819",
  #              "2022" = "#4EA5D9")
  
  # Plot
  serie_semanas <-
    ggplot() +
    facet_wrap(~alcaldia, scales = "free_y") + 
    geom_bar(data = dat_21,
             aes(x = semana_use, y = tot_semana, color = factor(ao), fill = factor(ao)),
             stat = "identity", position = "dodge", alpha = .6, width = .6, show.legend = F) +
    geom_point(data = dat_ant %>%
                 filter(semana_use != sem),
               aes(x = semana_use, y = tot_semana, color = factor(ao)),
               alpha = .3, show.legend = F) +
    geom_point(data = dat_ant %>%
                 filter(semana_use == sem),
               aes(x = semana_use, y = tot_semana, color = factor(ao)),
               alpha = .9, show.legend = F, size = 2) +
    geom_smooth(data = dat_ant,
                aes(x = semana_use, y = tot_semana, color = factor(ao)),
                se = F, span = .65) +
    labs(
      title = str_wrap(paste0("Incidencia delictiva - ", nombre),65),
      subtitle = paste0("Corte al ", format(as_date(fecha_lim), "%A %d de %B, %Y")),
      x = "Semana",
      y = "Carpetas iniciadas por semana",
      caption = "Semanas de sábado a viernes",
      color = "Año de inicio de las carpetas",
      fill = "Año de inicio de las carpetas"
    ) +
    scale_y_continuous(label = comma_format(accuracy = 1)) +
    scale_color_manual(values = col_aos, aesthetics = c("color", "fill")) +
    theme_light() +
    theme(
      legend.position = "bottom",
      strip.background = element_rect(fill = "gray80"),
      strip.text = element_text(color = "black"),
      plot.title = element_text(face = "bold")
    )
  
  if (tipo == "victimas") {
    serie_semanas <-
      serie_semanas +
      labs(title = str_wrap(paste0("Incidencia delictiva (víctimas) - ", nombre),65))
  }
  
  return(serie_semanas)
}


## Gráfica de comparación por años (meses) -------------------------------------
plot_barras_mensual <- function(
  base, 
  tipo = c("incidencia", "victimas"), 
  nombre = str_to_lower(nombre_plots),
  fecha_comienzo = fecha_inicio_global,
  fecha_lim = fecha_lim,
  tema_tablas = tema
) {
  
  serie <- 
    base %>% 
    group_by(
      ao = year(fecha_inicio),
      mes = month(fecha_inicio)
    ) %>% 
    summarise(
      tot_mes = n(),
      fecha = as_date(paste(min(ao), min(mes), "01", sep = "-")),
      .groups = "drop"
    ) %>% 
    complete(fecha = seq.Date(from = as_date(fecha_comienzo),
                              to = as_date(fecha_lim),
                              by = "month"),
             fill = list(tot_mes = 0)) %>% 
    mutate(
      ao = year(fecha),
      mes = month(fecha),
      mes_use = month(fecha, abbr = F, label = T),
      mes_use = factor(mes_use, levels = c("enero", "febrero", "marzo",
                                           "abril", "mayo", "junio",
                                           "julio", "agosto", "septiembre",
                                           "octubre", "noviembre", "diciembre"))
    )
  
  
  dat_21 <- 
    serie %>% 
    filter(ao == max(ao))
  
  
  dat_ant <- 
    serie %>% 
    filter(ao != max(ao))
  
  # Plot
  serie_mes <- 
    ggplot() +
    geom_bar(data = dat_21,
             aes(x = mes, y = tot_mes, color = factor(ao), fill = factor(ao)),
             # aes(x = reorder(mes_use, mes), y = tot_mes, color = factor(ao), fill = factor(ao)),
             stat = "identity", position = "dodge", alpha = .6, width = .6, show.legend = F) +
    geom_point(data = dat_ant %>% 
                 filter(month(fecha) != month(fecha_lim)), 
               aes(x = mes, y = tot_mes, color = factor(ao)),
               # aes(x = reorder(mes_use, mes), y = tot_mes, color = factor(ao)),
               alpha = .3, show.legend = F) +
    geom_point(data = dat_ant %>% 
                 filter(month(fecha) == month(fecha_lim)), 
               aes(x = mes, y = tot_mes, color = factor(ao)),
               alpha = .9, show.legend = F, size = 2) +
    geom_smooth(data = dat_ant, 
                aes(x = mes, y = tot_mes, color = factor(ao)),
                se = F, span = .65) +
    labs(
      title = str_wrap(paste0("Incidencia delictiva - ", nombre),65),
      subtitle = paste0("Corte al ", format(as_date(fecha_lim), "%d de %B, %Y")),
      x = "Mes",
      y = "Carpetas iniciadas por mes",
      color = "Año de inicio de las carpetas",
      fill = "Año de inicio de las carpetas"
    ) +
    scale_x_continuous(breaks = 1:12, labels = c("enero", "febrero", "marzo",
                                                 "abril", "mayo", "junio",
                                                 "julio", "agosto", "septiembre",
                                                 "octubre", "noviembre", "diciembre")) +
    scale_y_continuous(label = comma_format(accuracy = 1)) +
    scale_color_manual(values = col_aos, aesthetics = c("color", "fill")) +
    theme_light() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold")
    ) 
  
  if (tipo == "victimas") {
    serie_mes <- 
      serie_mes +
      labs(title = str_wrap(paste0("Incidencia delictiva (víctimas) - ", nombre),65))
  }
  
  # Tabla 1
  tabla_totales <- 
    base %>% 
    group_by(
      fecha_inicio
    ) %>% 
    summarise(
      tot_dia = n(),
      .groups = "drop"
    ) %>% 
    complete(fecha_inicio = seq.Date(from = as_date(fecha_comienzo),
                                     to = as_date(fecha_lim),
                                     by = "day"),
             fill = list(tot_dia = 0)) %>% 
    group_by(ao = year(fecha_inicio)) %>% 
    summarise(Total = comma(sum(tot_dia), accuracy =  1),
              "Prom. diario" = comma(round(mean(tot_dia), 2), accuracy = .01)) %>% 
    pivot_longer(cols = -ao, names_to = " ") %>% 
    pivot_wider(names_from = ao, values_from = value)
  
  tb <- 
    tableGrob(tabla_totales, rows = NULL, theme = tema_tablas) %>% 
    gtable_add_grob(grobs = replicate(ncol(.) - 1,
                                      segmentsGrob(x1 = unit(0, "npc"), 
                                                   gp = gpar(lty = 2)),
                                      simplify = FALSE),
                    t = 2, b = nrow(.), l = seq_len(ncol(.) - 1) + 1) %>% 
    gtable_add_grob(grobs = segmentsGrob( # line across the bottom
      x0 = unit(0,"npc"),
      y0 = unit(0,"npc"),
      x1 = unit(1,"npc"),
      y1 = unit(0,"npc"),
      gp = gpar(lty = 2.0)),
      t = 2, b = 2, l = 1, r = 5)
  
  # Tabla 2 
  tabla_comp <-
    tibble(
      Iniciadas = dat_21$tot_mes[dat_21$mes == month(fecha_lim)],
      "Mes anterior" = dat_21$tot_mes[dat_21$mes == month(rollback(as_date(fecha_lim)))],
      "Promedio mensual" = round(mean(serie$tot_mes), 2),
      # "Promedio mensual\n(2018-2021)" = round(mean(serie$tot_mes), 2),
      "Mismo mes 2019" = serie$tot_mes[serie$mes == month(fecha_lim) & serie$ao == 2019]
    ) %>%
    pivot_longer(-Iniciadas, names_to = "tiempo") %>%
    mutate(
      diferencia = Iniciadas - value,
      color = ifelse(diferencia > 0, "#f94144", "#90be6d"),
      cambio = (Iniciadas/value) - 1,
      value = comma(value, accuracy = .1),
      texto = paste0("Diferencia: ", comma(diferencia, accuracy = .1),
                     "\n(", percent(cambio, accuracy = .1), ")")
    ) %>%
    select(-c(Iniciadas, diferencia, cambio)) %>%
    pivot_longer(-c(tiempo, color), values_to = "valor") %>%
    mutate(
      name = factor(name, levels = c("value", "texto")),
      color = ifelse(name == "value", "black", color)
    )
  
  
  comparacion <-
    ggplot(tabla_comp,
           aes(x = name, y = tiempo)) +
    geom_tile( fill = NA) +
    geom_text(aes(label = valor, color = color), size = 2.8, fontface = "bold") +
    labs(
      y = paste0(comma(dat_21$tot_mes[dat_21$mes == month(fecha_lim)]), "\n\nCI iniciadas")
    ) +
    theme_void() +
    theme_minimal() +
    geom_vline(xintercept = c(.5, 1.5), alpha = .5, color = "gray75") +
    geom_hline(yintercept = c(1.5, 2.5), alpha = .5, color = "gray75") +
    scale_fill_identity(aesthetics = c("fill", "color")) +
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.text.y = element_text(hjust = 1, size = 9),
      axis.title.y = element_text(size = 14, face = "bold", color = col_aos[4],
                                  vjust = .5, angle = 0),
      axis.line = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank()
    )
  
  
  t_fin <- plot_grid(comparacion, tb, 
                     nrow = 1, rel_widths = c(.4, .3))
  final <- plot_grid(serie_mes, t_fin, 
                     nrow = 2, rel_heights = c(2, 0.5))
  
  return(final)
}

## Gráfica de tendencias anuales -----------------------------------------------

plot_tendencia <- function(
  base, 
  tipo = c("incidencia", "victimas"), 
  tabla = c("anual", "mensual"), 
  nombre = str_to_lower(nombre_plots),
  fecha_comienzo = fecha_inicio_global,
  fecha_lim = fecha_lim,
  tema_tablas = tema2
) {
  
  serie <- 
    base %>% 
    group_by(
      ao = year(fecha_inicio),
      mes = month(fecha_inicio)
    ) %>% 
    summarise(
      total_mes = n(),
      fecha = as_date(paste(min(ao), min(mes), "01", sep = "-")),
      .groups = "drop"
    ) %>% 
    complete(fecha = seq.Date(
      as_date(fecha_comienzo), 
      as_date(fecha_lim), 
      by = "month"),
      fill = list(total_mes = 0)) %>% 
    mutate(dias = days_in_month(fecha),
           ao = year(fecha),
           mes = month(fecha))
  
  
  
  if(day(fecha_lim) < serie$dias[serie$fecha == max(serie$fecha)]) {
    serie_lm <- 
      serie %>% 
      filter(fecha < max(fecha))
    comp <- FALSE
  } else {
    serie_lm <- serie
    comp <- TRUE
  }
  
  
  serie <- serie %>% filter(year(fecha) == 2024)
  
  tendencias <- 
    ggplot(serie, aes(x = fecha, y = total_mes)) +
    geom_vline(data = serie %>% 
                 select(fecha) %>% 
                 distinct() %>% 
                 filter(month(fecha) == 1),
               aes(xintercept = fecha),
               linetype = "dotdash", color = "gray50") +
    geom_line(aes(group = 1), color = colores[9], size = 1.01, alpha = .7) +
    geom_point(color = colores[9], size = 2, alpha = .7) +
    geom_smooth(data = serie_lm,
                aes(x = fecha, y = total_mes, 
                    color = factor(ao), group = ao), 
                method = "lm", se = F, linetype = "dashed") +
    geom_text(data = serie %>% filter(total_mes != 0 & year(fecha) == 2024), aes(label = total_mes), vjust = -0.5, size = 3, fontface = "bold", color = "black")+
    labs(
      title = str_wrap(paste0("Incidencia delictiva (tendencias anuales) - ", nombre), 65),
      subtitle = paste0("Corte al ", format(as_date(fecha_lim), "%d de %B, %Y")),
      x = "Mes",
      y = "Carpetas iniciadas por mes",
      color = "Año de inicio de las carpetas"
    ) +
    scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y",
                 date_minor_breaks = "1 month", limits = c(floor_date(min(serie$fecha), "year")-31, NA)) +
    scale_y_continuous(label = comma_format(accuracy = 1)) +
    scale_color_manual(values = col_aos, aesthetics = c("color", "fill")) +
    theme_light() +
    guides(colour = guide_legend(override.aes = list(size = 2))) +
    theme(
      legend.position = "bottom",
      legend.key = element_rect(fill = NA),
      plot.title = element_text(face = "bold")
    )
  
  if(comp == FALSE) {
    tendencias <- 
      tendencias +
      labs(caption = "La tendencia no considera el mes en curso")
  }  
  
  if (tipo == "victimas") {
    tendencias <- 
      tendencias +
      labs(title = paste0("Víctimas (tendencias anuales) - ", delitos_sel))
  }
  
  
  
  
  
  if (tabla == "anual") {
    
    tabla_totales <- 
      base %>% 
      group_by(
        fecha_inicio
      ) %>% 
      summarise(
        tot_dia = n(),
        .groups = "drop"
      ) %>% 
      complete(fecha_inicio = seq.Date(from = as_date(fecha_comienzo),
                                       to = as_date(fecha_lim),
                                       by = "day"),
               fill = list(tot_dia = 0)) %>% 
      group_by(ao = year(fecha_inicio)) %>% 
      summarise(Total = comma(sum(tot_dia), accuracy =  1),
                "Prom.\ndiario" = comma(round(mean(tot_dia), 2), accuracy = .01)) %>% 
      rename("Año" = ao)
    
    
    tb <- 
      tableGrob(tabla_totales, rows = NULL, theme = tema_tablas) %>% 
      gtable_add_grob(grobs = replicate(ncol(.) - 1,
                                        segmentsGrob(x1 = unit(0, "npc"), 
                                                     gp = gpar(lty = 2)),
                                        simplify = FALSE),
                      t = 2, b = nrow(.), l = seq_len(ncol(.) - 1) + 1)
    
    
    
    
    final <- plot_grid(tendencias, NULL, tb, 
                       ncol = 3, rel_widths = c(1.7, .05, .5))
  } else {
    tt_year <- ttheme_minimal(
      base_size = 12,
      padding = unit(c(4,4), "mm"),
      core = list(bg_params = list(col = NA,
                                   fill=c(rep(c(NA),
                                              length.out = 12), "#bde0fe"))),
      colhead=list(fg_params = list(col = "white", fontface = "bold"),
                   bg_params = list(fill=c("#003763"))))
    
    
    
    # tab <- 
    #   base %>% 
    #   count(
    #     ao = year(fecha_inicio),
    #     mes = month(fecha_inicio, abbr = F, label = T)
    #   ) %>% 
    #   # mutate(n = comma(n, accuracy = 1)) %>% 
    #   pivot_wider(names_from = ao, values_from = n) %>% 
    #   adorn_totals() %>% 
    #   mutate(
    #     across(-mes, ~comma(.x, accuracy = 1)),
    #     across(-mes, ~replace_na(.x, "-")),
    #     mes = factor(mes, levels = c("enero", "febrero", "marzo",
    #                                  "abril", "mayo", "junio",
    #                                  "julio", "agosto", "septiembre",
    #                                  "octubre", "noviembre", "diciembre",
    #                                  "Total"))
    #   ) %>% 
    #   arrange(mes) %>% 
    #   rename("Mes/Año" = mes) 
    
    tab <- base %>% 
      count(
        ao = year(fecha_inicio),
        mes = month(fecha_inicio, abbr = F, label = T)
      ) %>% 
      complete(ao=seq(year(min(base$fecha_inicio)), year(max(base$fecha_inicio)), by=1), 
               mes=c("enero", "febrero", "marzo",
                     "abril", "mayo", "junio",
                     "julio", "agosto", "septiembre",
                     "octubre", "noviembre", "diciembre"), fill=list(n=NA)) %>% pivot_wider(names_from = ao, values_from = n) %>% 
      adorn_totals() %>% 
      mutate(
        across(-mes, ~comma(.x, accuracy = 1)),
        across(-mes, ~replace_na(.x, "-")),
        mes = factor(mes, levels = c("enero", "febrero", "marzo",
                                     "abril", "mayo", "junio",
                                     "julio", "agosto", "septiembre",
                                     "octubre", "noviembre", "diciembre",
                                     "Total"))
      ) %>% 
      arrange(mes) %>% 
      rename("Mes/Año" = mes) 
    
    tabla_final <- 
      tableGrob(tab, rows = NULL, theme = tt_year) %>% 
      gtable_add_grob(grobs = replicate(ncol(.) - 1,
                                        segmentsGrob(x1 = unit(0, "npc"), 
                                                     gp = gpar(lty = 2)),
                                        simplify = FALSE),
                      t = 2, b = nrow(.), l = seq_len(ncol(.) - 1) + 1)  
    
    # incidencia <- plot_grid(tabla_final, labels = "Incidencia")
    
    final <- plot_grid(tendencias, NULL, tabla_final, 
                       ncol = 3, rel_widths = c(.6, .05, .35))
    
  }
  
  return(final)
  
  # return(tendencias)
} 



## Gráfica de promedio diario  jesus-----------------------------------------------

plot_promedio <- function(
    base,
    tipo = c("incidencia", "victimas"), 
    tabla = c("anual", "mensual"), 
    nombre = str_to_lower(nombre_plots),
    fecha_comienzo = fecha_inicio_global,
    fecha_lim = fecha_lim,
    tema_tablas = tema2
) {
  
  serie <- 
    base %>% 
    group_by(
      ao = year(fecha_inicio),
      mes = month(fecha_inicio)
    ) %>% 
    summarise(
      total_mes = n(),
      fecha = as_date(paste(min(ao), min(mes), "01", sep = "-")),
      .groups = "drop"
    ) %>% 
    complete(fecha = seq.Date(
      as_date(fecha_comienzo), 
      as_date(fecha_lim), 
      by = "month"),
      fill = list(total_mes = 0)) %>% 
    mutate(
      dias_totales = days_in_month(fecha),
      ao = year(fecha),
      mes = month(fecha),
      dias = if_else(ao == year(Sys.Date()) & mes == month(Sys.Date()), day(Sys.Date()), dias_totales),
      mes_actual = if_else(month(fecha) == month(Sys.Date()), TRUE, FALSE)
    )
  
  
  serie_anual <- serie %>%
    group_by(ao) %>%
    summarise(
      total_anual = sum(total_mes),  # Suma de todos los meses de cada año
      dias_anual = sum(dias),  # Total de días de todos los meses del año
      .groups = "drop"
    ) %>% 
    mutate(
      fecha_inicio = as.Date(paste(ao, "01", "01", sep = "-")),  # Crear fecha para cada año
      fecha_fin = as.Date(paste(ao,"12","31", sep = "-")),
      promedio_anual = total_anual / dias_anual,
      tasa = (total_anual*100000)/9209944,
      redondeo = round(promedio_anual)
      #redondeo = round(promedio_anual)+2
    )
  
  
  
  
  if(day(fecha_lim) < serie$dias[serie$fecha == max(serie$fecha)]) {
    serie_lm <- 
      serie %>% 
      filter(fecha < max(fecha))
    comp <- FALSE
  } else {
    serie_lm <- serie
    comp <- TRUE
  }
  
  fecha_inicio_sombreado <- as.Date("2024-10-05") 
  fecha_fin_sombreado <- as.Date(fecha_lim)
  
  tendencias <- 
    ggplot(serie, aes(x=fecha, y =total_mes/dias))+
    geom_vline(data = serie %>%           ##Crea linea vertical en el grafico donde empieza el año
                 select(fecha) %>% 
                 distinct() %>% 
                 filter(month(fecha)==1),
               aes(xintercept = fecha),
               linetype = "dotdash", color = "gray50")+
    geom_rect(aes(xmin = fecha_inicio_sombreado, xmax = fecha_fin_sombreado, ymin = -Inf, ymax = Inf), fill = "#ebf3fa", alpha = 0.2) + ##sombrear periodo de la administración actual
    geom_line(aes(group = 1), color = colores[9], size = 1.01, alpha = .7) + 
    geom_point(color = colores[9], size = 2, alpha = .7) +
    geom_segment(
      data = serie_anual,
      aes(x = fecha_inicio, xend = fecha_fin, y = promedio_anual, yend = promedio_anual),
      color = "#003763",
      linewidth = .8,
      linetype = "dotdash"
    )+
    geom_smooth(method = "lm", se = FALSE, color = "#DE434A") + ##crea la linea de tendencia
    
    ## Añade etiquetas al gráfico del mes actual
    geom_label(data = serie %>% filter(mes_actual),  
               aes(label = round(total_mes / dias, 2)), 
               vjust = -0.5, color = "black", fill = "white", size = 2, alpha = .7)+
    
    
    ### Etiquetas año
    geom_text(
      data = serie_anual,
      aes(x = fecha_inicio, y = (min(promedio_anual, na.rm = TRUE)-min(promedio_anual*.5, na.rm = TRUE)), label = sprintf("%d", ao)), # Ubicar el texto arriba del gráfico
      color = "#004C8D", 
      hjust = -1.3,  # Centrado
      vjust = 0 # Ubicar el texto arriba del punto
    )+
    
    ### Etiquetas de Tasa por 100000 habitantes
    geom_text(
      data = serie_anual,
      aes(x = fecha_inicio, y = 0, label = sprintf("Tasa por cada 100,000\n      habitantes: %.2f", tasa)),  # Mostrar la tasa de población
      color = "#000000",
      hjust = -0.1,
      vjust = 0,
      size = 2.5
    )+
    
    
    ### eTIQUETAS DE PROMEDIO DIARIO
    geom_text(
      data = serie_anual,
      aes(x = fecha_inicio, y = max(promedio_anual*1.4, na.rm = TRUE), label = sprintf("Promedio diario\n  (%d): %.2f", ao, promedio_anual)), # Ubicar el texto arriba del gráfico
      color = "#000000", 
      hjust = -0.25,  # Centrado
      vjust = 0.2,
      size = 3# Ubicar el texto arriba del punto
    )+
    
    
    labs(
      title = str_wrap(paste0("Incidencia delictiva (promedio diario) - ", nombre), 65),
      subtitle = paste0("Corte al ", format(as_date(fecha_lim), "%d de %B, %Y")),
      x = "Mes",
      y = "Promedio Diario",
      color = "año de inicio de las carpetas"
    ) +
    scale_x_date(
      date_breaks = "2 months",
      date_labels = "%b",
      date_minor_breaks = "1 month",
      limits = c(floor_date(min(serie$fecha), "year") - 31, NA)
      
    ) +
    scale_y_continuous(label = comma_format(accuracy = 1),
                        limits = c(0, max(serie_anual$promedio_anual*1.4), na.rm = TRUE))+
    scale_color_manual(values = col_aos, aesthetics = c("color", "fill")) +
    theme_light() +
    guides(colour = guide_legend(override.aes = list(size = 2))) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 0.5),  # Texto horizontal con centrado
      legend.position = "bottom",
      legend.key = element_rect(fill = NA),
      plot.title = element_text(face = "bold")
    )
  
  if(comp == FALSE) {
    tendencias <- 
      tendencias +
      labs(caption = "El promedio no considera el mes en curso")
  }  
  
  if (tipo == "victimas") {
    tendencias <- 
      tendencias +
      labs(title = paste0("víctimas (tendencias anuales) - ", delitos_sel))
  }
  
  
  library(gridExtra) 
  library(gtable)
  
  
  if (tabla == "anual") {
    
    tabla_totales <- 
      base %>% 
      group_by(
        fecha_inicio
      ) %>% 
      summarise(
        tot_dia = n(),
        .groups = "drop"
      ) %>% 
      complete(fecha_inicio = seq.Date(from = as_date(fecha_comienzo),
                                       to = as_date(fecha_lim),
                                       by = "day"),
               fill = list(tot_dia = 0)) %>% 
      group_by(ao = year(fecha_inicio)) %>% 
      summarise(Total = comma(sum(tot_dia), accuracy =  1),
                "Prom.\ndiario" = comma(round(mean(tot_dia), 2), accuracy = .01)) %>% 
      rename("año" = ao)
    
    
    tb <- 
      tableGrob(tabla_totales, rows = NULL, theme = tema_tablas) %>% 
      gtable_add_grob(grobs = replicate(ncol(.) - 1,
                                        segmentsGrob(x1 = unit(0, "npc"), 
                                                     gp = gpar(lty = 2)),
                                        simplify = FALSE),
                      t = 2, b = nrow(.), l = seq_len(ncol(.) - 1) + 1)
    
    
    
    
    final <- plot_grid(tendencias, NULL, tb, 
                       ncol = 3, rel_widths = c(1.7, .05, .5))
  } else {
    tt_year <- ttheme_minimal(
      base_size = 12,
      padding = unit(c(4,4), "mm"),
      core = list(bg_params = list(col = NA,
                                   fill=c(rep(c(NA),
                                              length.out = 12), "#bde0fe"))),
      colhead=list(fg_params = list(col = "#FFFFFF", fontface = "bold"),
                   bg_params = list(fill=c("#003763"))))
    
    
    
    # tab <- 
    #   base %>% 
    #   count(
    #     ao = year(fecha_inicio),
    #     mes = month(fecha_inicio, abbr = F, label = T)
    #   ) %>% 
    #   # mutate(n = comma(n, accuracy = 1)) %>% 
    #   pivot_wider(names_from = ao, values_from = n) %>% 
    #   adorn_totals() %>% 
    #   mutate(
    #     across(-mes, ~comma(.x, accuracy = 1)),
    #     across(-mes, ~replace_na(.x, "-")),
    #     mes = factor(mes, levels = c("enero", "febrero", "marzo",
    #                                  "abril", "mayo", "junio",
    #                                  "julio", "agosto", "septiembre",
    #                                  "octubre", "noviembre", "diciembre",
    #                                  "Total"))
    #   ) %>% 
    #   arrange(mes) %>% 
    #   rename("Mes/año" = mes) 
    
    tab <- base %>% 
      count(
        ao = year(fecha_inicio),
        mes = month(fecha_inicio, abbr = F, label = T)
      ) %>% 
      complete(ao=seq(year(min(base$fecha_inicio)), year(max(base$fecha_inicio)), by=1), 
               mes=c("enero", "febrero", "marzo",
                     "abril", "mayo", "junio",
                     "julio", "agosto", "septiembre",
                     "octubre", "noviembre", "diciembre"), fill=list(n=NA)) %>% pivot_wider(names_from = ao, values_from = n) %>% 
      adorn_totals() %>% 
      mutate(
        across(-mes, ~comma(.x, accuracy = 1)),
        across(-mes, ~replace_na(.x, "-")),
        mes = factor(mes, levels = c("enero", "febrero", "marzo",
                                     "abril", "mayo", "junio",
                                     "julio", "agosto", "septiembre",
                                     "octubre", "noviembre", "diciembre",
                                     "Total"))
      ) %>% 
      arrange(mes) %>% 
      rename("Mes/año" = mes) 
    
    tabla_final <- 
      tableGrob(tab, rows = NULL, theme = tt_year) %>% 
      gtable_add_grob(grobs = replicate(ncol(.) - 1,
                                        segmentsGrob(x1 = unit(0, "npc"), 
                                                     gp = gpar(lty = 2)),
                                        simplify = FALSE),
                      t = 2, b = nrow(.), l = seq_len(ncol(.) - 1) + 1)  
    
    # incidencia <- plot_grid(tabla_final, labels = "Incidencia")
    
    final <- plot_grid(tendencias, NULL, tabla_final, 
                       ncol = 3, rel_widths = c(.6, .03, .35))
    
  }
  
  return(final)
  
  # return(tendencias)
} 


## Gráfica de tendencias - alcaldias--------------------------------------------

plot_tendencia_alc <- function(
  base, 
  tipo = c("incidencia", "victimas"), 
  nombre = str_to_lower(nombre_plots),
  fecha_comienzo = fecha_inicio_global,
  fecha_lim = fecha_lim,
  info_alcs = info_alcs, 
  tabla = c(0,1)
) {
  
  
  serie <- 
    base %>% 
    group_by(
      ao = year(fecha_inicio),
      mes = month(fecha_inicio),
      delegacion_hechos
    ) %>% 
    summarise(
      total_mes = n(),
      fecha = as_date(paste(min(ao), min(mes), "01", sep = "-")),
      .groups = "drop"
    ) %>% 
    complete(fecha = seq.Date(
      as_date(fecha_comienzo), 
      as_date(fecha_lim), 
      by = "month"),
      delegacion_hechos,
      fill = list(total_mes = 0)) %>% 
    mutate(dias = days_in_month(fecha),
           ao = year(fecha),
           mes = month(fecha)) %>% 
    left_join(info_alcs) %>% drop_na(alcaldia) #se le agregro el drop na para quitar el Na del facets
  
  ###hacemos la base de la tabla de crecimiento
  # tabla <- base %>% mutate(Total=1) %>% 
  #   complete(fecha_inicio=seq.Date(as_date(fecha_comienzo), as_date(fecha_lim), 
  #                                  by="1 day"),
  #            delegacion_hechos=unique(info_alcs$delegacion_hechos),
  #            fill=list(Total=0)) %>% 
  #   group_by(fecha_inicio, delegacion_hechos) %>% summarise(Total=sum(Total), .groups = "drop") %>%
  #   group_by(año=year(fecha_inicio), delegacion_hechos) %>% summarise(media=round(mean(Total), 2)) %>% 
  #   filter(año %in% c(year(min(base$fecha_inicio)),year(max(base$fecha_inicio)))) %>% spread(año, media) %>% 
  #   mutate(cambio=(`2022`/`2019`-1)*100, 
  #          delegacion_hechos=str_to_title(delegacion_hechos)) %>% arrange(desc(cambio)) %>% 
  #   mutate("Cambio porcentual"=paste0(round(cambio,2), "%")) %>% 
  #   select(-cambio) %>%
  #   rename("Alcaldía"=delegacion_hechos, "Promedio diario 2019"=`2019`, 
  #          "Promedio diario 2022"=`2022`)
  # 
  # tb_raster <- tabla %>% 
  #   flextable() %>% border(border =  std_border) %>% 
  #   bg(i= 1, bg="#003763", part = "header") %>%
  #   color(i= 1, color="ghostwhite", part = "header") %>% 
  #   bold(i= 1, bold=T, part = "header") %>% 
  #   align(align = "center") %>% 
  #   bold(j=1, bold = T) %>% color(i=1:6, j=4, color = "#CE3C3F") %>% 
  #   color(i=10:16, j=4, color = "#06945f")
  
  # gr_tabla_incremento <- ggplot() + 
  #   theme_void() + 
  #   annotation_custom(rasterGrob(tb_raster), 
  #                     xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
  
  if(day(fecha_lim) < unique(serie$dias[serie$fecha == max(serie$fecha)])) {
    serie_lm <- 
      serie %>% 
      filter(fecha < max(fecha))
    comp <- FALSE
  } else {
    serie_lm <- serie
    comp <- TRUE
  }
  
  tendencias <- 
    ggplot(serie, aes(x = fecha, y = total_mes)) +
    facet_wrap(~alcaldia, scales = "free_y") +
    geom_vline(data = serie %>% 
                 select(fecha) %>% 
                 distinct() %>% 
                 filter(month(fecha) == 1),
               aes(xintercept = fecha),
               linetype = "dotdash", color = "gray50") +
    geom_point(color = colores[9], size = 1.7, alpha = .7) +
    geom_smooth(data = serie_lm, 
                aes(x = fecha, y = total_mes),
                se = F, color = colores[4]) +
    labs(
      title = str_wrap(paste0("Incidencia delictiva (tendencias anuales) - ", nombre), 65),
      subtitle = paste0("Corte al ", format(as_date(fecha_lim), "%d de %B, %Y")),
      x = "Mes",
      y = "Carpetas iniciadas por mes",
      color = "Año de inicio de las carpetas"
    ) +
    scale_x_date(
      # date_breaks = "3 months", 
      date_labels = "%b\n%Y",
      # date_minor_breaks = "1 month", 
      # breaks = NULL,
      # limits = c(as_date("2017-12-01"), NA)) +
      limits = c(min(serie$fecha)-30, NA)) +
    scale_y_continuous(label = comma_format(accuracy = 1)) +
    theme_light() +
    guides(colour = guide_legend(override.aes = list(size = 2))) +
    theme(
      plot.title = element_text(face = "bold"),
      strip.background = element_rect(fill = "gray80"),
      strip.text = element_text(color = "black"),
      axis.text.x = element_text()
    )
  
  if(comp == FALSE) {
    tendencias <- 
      tendencias +
      labs(caption = "La tendencia no considera el mes en curso")
  }  
  
  if (tipo == "victimas") {
    tendencias <- 
      tendencias +
      labs(title = paste0("Víctimas (tendencias anuales) - ", delitos_sel))
  }
  
  if (tabla==1){
  std_border = fp_border(color="gray", width = 1)
  
  tabla_crecimiento <- base %>% mutate(Total=1) %>% 
    complete(fecha_inicio=seq.Date(min(base$fecha_inicio), max(base$fecha_inicio), 
                                   by="1 day"),
              delegacion_hechos=unique(info_alcs$delegacion_hechos),
             fill=list(Total=0)) %>% 
    group_by(fecha_inicio, delegacion_hechos) %>% summarise(Total=sum(Total), .groups = "drop") %>%
    group_by(año=year(fecha_inicio), delegacion_hechos) %>% summarise(media=round(mean(Total), 2)) %>% 
    filter(año %in% c(year(min(base$fecha_inicio)),year(max(base$fecha_inicio)))) %>% 
    spread(año, media) %>% drop_na(delegacion_hechos) %>% 
    left_join(info_alcs) %>% select(-clave) %>% 
    `colnames<-`(c("delegacion_hechos", "año_pasado", "año_actual", "Alcaldía")) %>% 
    mutate(cambio=(año_actual/año_pasado-1)*100, 
           delegacion_hechos=str_to_title(delegacion_hechos)) %>% 
    mutate(cambio=case_when(
      is.nan(cambio) ~ 0, is.infinite(cambio) ~ 100, T ~ cambio
    )) %>% drop_na(delegacion_hechos) %>% 
    arrange(desc(cambio))
  
  tabla_raster <- tabla_crecimiento %>% select(-delegacion_hechos) %>% relocate(Alcaldía) %>% 
    `colnames<-`(c("Alcaldía", paste0("Promedio\ndiario ", year(min(base$fecha_inicio))), 
                   paste0("Promedio\ndiario ", year(max(base$fecha_inicio))), 
                   "Cambio porcentual")) %>% 
    mutate("Cambio porcentual"=paste0(round(`Cambio porcentual`,2), "%")) %>% 
    drop_na(Alcaldía) %>% 
    flextable() %>% border(border =  std_border) %>% 
    bg(i= 1, bg="#003763", part = "header") %>%
    color(i= 1, color="ghostwhite", part = "header") %>% 
    bold(i= 1, bold=T, part = "header") %>% 
    align(align = "center", part = "all") %>% 
    width(j=1, width=.5) %>% 
    bold(j=1, bold = T) %>% 
    #autofit() %>%
    as_raster()
  
  serie_lm <- serie_lm %>% 
    mutate(delegacion_hechos=str_to_title(delegacion_hechos)) %>% 
    left_join(tabla_crecimiento) %>% 
    mutate(crecimiento=case_when(
      cambio>0 ~"A la alza", cambio<0 ~ "A la baja", cambio==0 ~ "Sin cambio"
    ))
  
  tendencias <- serie %>% 
    ggplot(aes(x = fecha, y = total_mes)) +
    facet_wrap(~alcaldia, scales = "free_y") +
    geom_vline(data = serie %>% 
                 select(fecha) %>% 
                 distinct() %>% 
                 filter(month(fecha) == 1),
               aes(xintercept = fecha),
               linetype = "dotdash", color = "gray50") +
    geom_point(color = colores[9], size = 1.7, alpha = .7) +
    geom_smooth(data = serie_lm, 
                aes(x = fecha, y = total_mes, color=crecimiento),
                se = F) +
    labs(
      title = str_wrap(paste0("Incidencia delictiva (tendencias anuales) - ", nombre), 65),
      subtitle = paste0("Corte al ", format(as_date(fecha_lim), "%d de %B, %Y")),
      x = "Mes",
      y = "Carpetas iniciadas por mes",
      color = "Año de inicio de las carpetas"
    ) +
    scale_color_manual(values = c("A la alza"="#ff4f4f", "A la baja"="#00b140",
                                  "Sin cambio"="gray")) +
    scale_x_date(
      # date_breaks = "3 months", 
      date_labels = "%b\n%Y",
      # date_minor_breaks = "1 month", 
      # breaks = NULL,
      # limits = c(as_date("2017-12-01"), NA)) +
      limits = c(min(serie$fecha)-30, NA)) +
    scale_y_continuous(label = comma_format(accuracy = 1)) +
    theme_light() +
    guides(colour = guide_legend(override.aes = list(size = 2))) +
    theme(
      plot.title = element_text(face = "bold"),
      strip.background = element_rect(fill = "gray80"),
      strip.text = element_text(color = "black"),
      axis.text.x = element_text(), 
      legend.position = "none"
    )
  
  gtabla <- ggplot() + 
    theme_void() + 
    annotation_custom(rasterGrob(tabla_raster)#, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf
                      )
  
  
  final <- plot_grid(tendencias, gtabla, nrow = 1, 
                     rel_widths = c(10, 4))
  } else {
    final <- tendencias
  }
  
  return(final)
}

## Incidencia diaria -----------------------------------------------------------
plot_2021 <- function(
  base, 
  tipo = c("incidencia", "victimas"),
  nombre = nombre_plots, 
  fecha_comienzo = fecha_comienzo,
  fecha_lim = fecha_lim,
  tema_tablas = tema
) {
  serie_2021 <- 
    base %>% 
    filter(fecha_inicio >= as_date(fecha_comienzo)) %>% 
    count(fecha_inicio, name = "tot_dia") %>% 
    complete(fecha_inicio = seq.Date(from = as_date(fecha_comienzo),
                                     to = as_date(fecha_lim),
                                     by = "day"),
             fill = list(tot_dia = 0)) 
  

  tabla <- 
    serie_2021 %>% 
    # filter(year(fecha_inicio) == 2021) %>% 
    group_by(
      ao = year(fecha_inicio),
      mes = month(fecha_inicio, label = T, abbr = F)
    ) %>% 
    summarise(
      Total = comma(sum(tot_dia), accuracy =  1),
      prom = round(mean(tot_dia), 2)
    ) %>% 
    ungroup() %>% 
    mutate(fecha = as_date(paste(ao, mes, "01", sep = "-"))) %>% 
    arrange(fecha) %>% 
    mutate(
      mes = format(fecha, "%b %Y"),
      "Cambio\nporcentual" = percent((prom/lag(prom)) - 1),
      `Cambio\nporcentual` = ifelse(`Cambio\nporcentual` == Inf, "100%", `Cambio\nporcentual`),
      prom = comma(prom, accuracy = .01)
    ) %>% 
    select(-c(ao, fecha)) %>% 
    replace_na(list("Cambio\nporcentual" = "-")) %>% 
    rename("Prom.\ndiario" = prom,
           "Mes" = mes)
  
  plot_diaria <- 
    ggplot(serie_2021, aes(x = fecha_inicio, y = tot_dia)) +
    geom_vline(data = serie_2021 %>% 
                 filter(day(fecha_inicio) == 1),
               aes(xintercept = fecha_inicio), linetype = "dashed",
               color = "gray65") +
    geom_point(alpha = .3, col = colores[5]) +
    tidyquant::geom_ma(n = 7, color = colores[5], size = 1.01,
                       linetype = "solid") +
    geom_smooth(color = colores[5], size = 1.1, se = F) +
    labs(
      title = str_wrap(paste0("Evolución diaria - ", nombre),65),
      subtitle = paste0("Del ", format(as_date(fecha_comienzo), "%d de %B de %Y"), " al ",
                        format(as_date(fecha_lim), "%d de %B de %Y")),
      x = "Fecha de inicio",
      y = "Carpetas iniciadas",
      caption = "Con promedio móvil de 7 días"
    ) +
    theme_light() +
    theme(
      plot.title = element_text(face = "bold")
    )
  
  if(year(fecha_comienzo) != 2021) {
    plot_diaria <- 
      plot_diaria +
      geom_vline(data = serie_2021 %>% 
                   filter(day(fecha_inicio) == 1 & month(fecha_inicio) == 1),
                 aes(xintercept = fecha_inicio), linetype = "dashed",
                 color = "gray50", size = 1.002) +
      scale_x_date(date_breaks = "3 months", date_minor_breaks = "1 month",
                   date_labels = "%B\n%Y")
  }
  
  tb <- plot_grid(
    tableGrob(tabla, rows = NULL, theme = tema_tablas) %>% 
      gtable_add_grob(grobs = replicate(ncol(.) - 1,
                                        segmentsGrob(x1 = unit(0, "npc"), 
                                                     gp = gpar(lty = 2)),
                                        simplify = FALSE),
                      t = 2, b = nrow(.), l = seq_len(ncol(.) - 1) + 1) %>% 
      gtable_add_grob(grobs = replicate(nrow(.) - 1,
                                        segmentsGrob(y1 = unit(0, "npc"), 
                                                     gp = gpar(lty = 2)),
                                        simplify = FALSE),
                      t = 2, l = 1, b = seq_len(nrow(.) - 1) , r = 4) 
  )
  
  
  final <- plot_grid(plot_diaria, NULL, tb, 
                     ncol = 3, rel_widths = c(1.7, .05, .5))
  
  return(final)
  
} 

## Gráfica de carpetas con víctimas mujeres ------------------------------------

carp_mujeres <- function(
  base = base_use, 
  base_victimas = base_vic, 
  nombre = str_to_lower(nombre_plots),
  fecha_comienzo = fecha_inicio_global,
  fecha_lim = fecha_lim,
  fecha_vic = fecha_vic
) {
  
  
  if(fecha_lim > fecha_vic) {
    
    base <- 
      base %>% 
      filter(fecha_inicio <= fecha_vic)
  }
  
  victimas_mujeres <- 
    base_victimas %>% 
    filter(sexo == "Femenino") %>% 
    pull(id_ap)
  
  base_mujeres <- 
    base %>% 
    filter(id_ap %in% victimas_mujeres) %>% 
    group_by(
      ao = year(fecha_inicio),
      mes = month(fecha_inicio)
    ) %>% 
    summarise(
      total_mes_mujeres = n(),
      fecha = as_date(paste(min(ao), min(mes), "01", sep = "-")),
      dias = days_in_month(fecha),
      .groups = "drop"
    ) %>% 
    complete(fecha = seq.Date(as_date(fecha_comienzo),
                              as_date(fecha_vic),
                              by = "month"),
             fill = list(total_mes_mujeres = 0)) %>% 
    select(fecha, total_mes_mujeres)
  
  serie <- 
    base %>% 
    group_by(
      ao = year(fecha_inicio),
      mes = month(fecha_inicio)
    ) %>% 
    summarise(
      total_mes = n(),
      fecha = as_date(paste(min(ao), min(mes), "01", sep = "-")),
      dias = days_in_month(fecha),
      .groups = "drop"
    ) %>% 
    complete(fecha = seq.Date(as_date(fecha_comienzo),
                              as_date(fecha_vic),
                              by = "month"),
             fill = list(total_mes = 0)) %>% 
    
    left_join(base_mujeres) %>% 
    pivot_longer(cols = c(total_mes, total_mes_mujeres))
  
  
  
  
  ci_mujeres <- 
    ggplot(serie, aes(x = fecha, y = value, color = name)) +
    geom_vline(data = serie %>% 
                 filter(month(fecha) == 1 & day(fecha) == 1),
               aes(xintercept = fecha),
               linetype = "dotdash", color = "gray50") +
    geom_line(size = 1.01) +
    geom_point(size = 2) +
    labs(
      title = str_wrap(paste0("Incidencia delictiva (CI totales vs CI con víctimas mujeres) - ", nombre),65),
      subtitle = paste0("Corte al ", format(as_date(fecha_vic), "%d de %B, %Y")),
      x = "Mes",
      y = "Carpetas iniciadas por mes",
      color = "Año de inicio de las carpetas"
    ) +
    scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y",
                 date_minor_breaks = "1 month", limits = c(min(serie$fecha)-30, NA)) +
    scale_y_continuous(label = comma_format(accuracy = 1)) +
    scale_color_manual(values = c("gray80", "#7b2cbf"),
                       labels = c("Incidencia total", "Carpetas con víctimas mujeres"),
                       aesthetics = c("color", "fill")) +
    theme_light() +
    theme(
      legend.position = "bottom",
      legend.key = element_rect(fill = NA),
      plot.title = element_text(face = "bold")
    )
  
  tabla_totales <- 
    base %>% 
    filter(id_ap %in% victimas_mujeres) %>% 
    count(
      fecha_inicio
    ) %>% 
    
    complete(fecha_inicio = seq.Date(as_date(fecha_comienzo),
                                     as_date(fecha_vic),
                                     by = "day"),
             fill = list(n = 0)) %>%  
    group_by(ao = year(fecha_inicio)) %>% 
    summarise(Total = comma(sum(n), accuracy =  1),
              "Prom.\ndiario" = comma(round(mean(n), 2), accuracy = .01)) %>% 
    rename("Año" = ao)
  
  tema <- ttheme_minimal(
    base_size = 10,
    padding = unit(c(4,4), "mm"),
    colhead = list(fg_params = list(col = "white", fontface = "bold"),
                   bg_params = list(fill=c("#7b2cbf"))))
  
  tb <- 
    tableGrob(tabla_totales, rows = NULL, theme = tema) %>% 
    gtable_add_grob(grobs = replicate(ncol(.) - 1,
                                      segmentsGrob(x1 = unit(0, "npc"), 
                                                   gp = gpar(lty = 2)),
                                      simplify = FALSE),
                    t = 2, b = nrow(.), l = seq_len(ncol(.) - 1) + 1)# %>% 
  
  
  final <- plot_grid(ci_mujeres, NULL, tb, 
                     ncol = 3, rel_widths = c(1.7, .05, .5))
  
  
  return(final)
  
}



## Heatmap todas horas ---------------------------------------------------------
heatmap <- function(base) {
  data_del <- 
    base %>% 
    #filter(fecha_inicio >= "2021-01-01") %>% 
    mutate(dia_sem = wday(fecha_hechos, label = T, abbr = F),
           hora = as.numeric(gsub(":\\d{2}", "", hora_de_los_hechos))) %>% 
    group_by(dia_sem, hora) %>% 
    summarise(tot = n(),
              .groups = "drop") %>% 
    complete(hora, dia_sem, fill = list(tot = 0)) %>% 
    pivot_wider(names_from = dia_sem, values_from = tot) %>% 
    adorn_totals() %>% 
    adorn_totals(where = "col") %>% 
    pivot_longer(cols = -hora, names_to = "dia_sem", values_to = "tot") %>% 
    mutate(orden = case_when(
      dia_sem == "lunes" ~ 1,
      dia_sem == "martes" ~ 2,
      dia_sem == "miércoles" ~ 3,
      dia_sem == "jueves" ~ 4,
      dia_sem == "viernes" ~ 5,
      dia_sem == "sábado" ~ 6,
      dia_sem == "domingo" ~ 7,
      dia_sem == "Total" ~ 8),
      hora = ifelse(hora == "Total", 24, as.numeric(hora)),
      hora_show = ifelse(hora < 10,
                         paste0("0", hora, ":00"),
                         paste0(hora, ":00")),
      hora_show = ifelse(hora == 24, "Total", hora_show),
      tot_2 = ifelse(hora_show == "Total" | dia_sem == "Total", NA, tot)
    ) 
  
  
  ggplot(data_del, 
         aes(x = reorder(dia_sem, orden), 
             y = reorder(hora_show, rev(hora)), 
             fill = tot_2)) +
    geom_tile(alpha = .5) +
    geom_vline(xintercept = 7.5) +
    geom_hline(yintercept = 1.5) +
    geom_text(aes(label = comma(tot, accuracy = 1), color = tot_2), fontface = "bold") +
    labs(title = paste0("Distribución de horarios de hechos"),
         subtitle = paste0("Carpetas iniciadas del ", 
                           format(as_date(fecha_inicio_global), "%d de %B de %Y"), 
                           " al ",
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

## Heatmap agrupado ------------------------------------------------------------
heatmap_agrup <- function(
  base = base_use, 
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
    base %>% drop_na(fecha_hechos) %>% 
    filter(fecha_hechos >= as_date(fecha_comienzo)) %>% 
    mutate(
      dia_sem = wday(fecha_hechos, label = T, abbr = F),
      # hora_de_los_hechos = as.numeric(hora_de_los_hechos),
      # hora = gsub(":\\d{2}", "", hora_de_los_hechos),
      hora = as.numeric(gsub(":\\d{2}", "", hora_de_los_hechos)),
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

## Tiempos entre hechos y denuncia ---------------------------------------------
plot_tiempos <- function(
  base, 
  nombre = str_to_lower(nombre_plots),
  fecha_comienzo = fecha_comienzo,
  fecha_lim = fecha_lim
) {
  
  tiempos <- 
    base %>% 
    filter(fecha_inicio >= as_date(fecha_comienzo)) %>% 
    mutate(
      diferencia = as.numeric(fecha_inicio - fecha_hechos),
      grupo = case_when(
        diferencia <= 0 ~ "Mismo\ndía",
        diferencia >= 1 & diferencia < 8 ~ "Misma\nsemana",
        diferencia >= 8 & diferencia < 31 ~ "Mismo mes",
        diferencia >= 31 & diferencia < 181 ~ "Primeros seis meses",
        #diferencia >= 8 & diferencia < 181 ~ "Primeros\nseis meses",
        diferencia >= 181 & diferencia <= 366 ~ "Mismo\naño",
        TRUE ~ "Tarda más de\nun año"
      )
    ) %>% 
    group_by(grupo,
             ao = year(fecha_inicio)) %>% 
    summarise(
      tot_g = n(),
      .groups = "drop"
    ) %>% mutate(grupo=str_wrap(grupo, 8)) %>% 
    group_by(ao) %>% 
    mutate(prc = tot_g/sum(tot_g)) %>% 
    ungroup() %>% 
    mutate(grupo = factor(grupo, levels=c("Mismo\ndía",
                                        "Misma\nsemana",
                                   "Mismo\nmes",
                                   "Primeros\nseis\nmeses",
                                   "Mismo\naño",
                                   "Tarda\nmás de\nun año")))
  
  
  ggplot(tiempos, aes(x = grupo, y = prc, 
                      color = grupo, fill = grupo)) +
    facet_wrap(~ao) +
    geom_bar(stat = "identity", position = "dodge", alpha = .7) +
    geom_label(aes(label = percent(prc, accuracy = .1)),
               fill = "white", vjust = "inward") +
    labs(title = str_wrap(paste0("Tiempo transcurrido entre los hechos y el inicio de la carpeta - ", nombre), 65),
         subtitle = paste0("Del ", format(as_date(fecha_comienzo), "%d de %B de %Y"), " al ",
                           format(as_date(fecha_lim), "%d de %B de %Y")),
         x = "Tiempo transcurrido",
         y = "Porcentaje de carpetas iniciadas",
         caption = "Las categorías son excluyentes") +
    # scale_y_continuous(label = comma, limits = c(0, 6500)) +
    scale_y_continuous(label = percent_format(accuracy = 1)) +
    scale_color_manual(values = colores, aesthetics = c("color", "fill")) +
    theme_light() +
    theme(
      legend.position = "none",
      text=element_text(size=15),
      strip.background = element_rect(fill = "gray80"),
      strip.text = element_text(color = "black", face = "bold"),
      plot.title = element_text(face = "bold")
    )
}

## Tiempos entre hechos y denuncia - tendencia ---------------------------------
plot_tiempos_serie <- function(
  base, 
  tipo = c("incidencia", "victimas"),
  nombre = str_to_lower(nombre_plots),
  fecha_comienzo = fecha_inicio_global,
  fecha_lim = fecha_lim
) {
  
  serie <- 
    base %>% 
    # filter(fecha_inicio >= as_date(fecha_comienzo)) %>% 
    mutate(
      diferencia = as.numeric(fecha_inicio - fecha_hechos),
      grupo = case_when(
        diferencia <= 0 ~ "Mismo\ndía",
        # diferencia >= 1 & 
        diferencia < 8 ~ "Misma semana",
        diferencia >= 8 & diferencia < 31 ~ "Mismo mes",
        diferencia >= 31 & diferencia < 181 ~ "Primeros seis meses",
        # diferencia >= 8 & diferencia < 181 ~ "Primeros\nseis meses",
        diferencia >= 31 & diferencia <= 366 ~ "Mismo año",
        TRUE ~ "Tarda más de un año"
      )
    ) %>% 
    group_by(grupo,
             ao = year(fecha_inicio),
             mes = month(fecha_inicio)) %>% 
    summarise(
      tot_g = n(),
      fecha = as_date(paste(min(ao), min(mes), "01", sep = "-")),
      .groups = "drop"
    ) %>% 
    complete(fecha = seq.Date(as_date(fecha_comienzo),
                              as_date(fecha_lim),
                              by = "month"),
             grupo, fill = list(tot_g = 0)) %>% 
    mutate(dias = days_in_month(fecha),
           grupo = factor(grupo, 
                          levels = c("Mismo\ndía",
                            "Misma semana",
                            "Mismo mes",
                            "Primeros seis meses",
                            "Mismo año",
                            "Tarda más de un año")))
  
  
  if(day(fecha_lim) < unique(serie$dias[serie$fecha == max(serie$fecha)])) {
    serie_lm <- 
      serie %>% 
      filter(fecha < max(fecha))
    comp <- FALSE
  } else {
    serie_lm <- serie
    comp <- TRUE
  }
  
  tendencias <- 
    ggplot(serie, aes(x = fecha, y = tot_g)) +
    facet_wrap(~grupo, scales = "free_y", nrow = 2) + 
    geom_vline(data = serie %>% 
                 select(fecha) %>% 
                 distinct() %>% 
                 filter(month(fecha) == 1),
               aes(xintercept = fecha),
               linetype = "dotdash", color = "gray50") +
    # geom_point(aes(group = 1), color = colores[9], size = 1.01, alpha = .7) +
    geom_point(color = colores[2], size = 2, alpha = .7) +
    geom_smooth(data = serie_lm,
                aes(x = fecha, y = tot_g), 
                color = colores[2], se = F,
                span = .9) +
    labs(
      title = str_wrap(paste0("Tendencias del tiempo transcurrido entre hechos y denuncia - ", nombre), 65),
      subtitle = paste0("Corte al ", format(as_date(fecha_lim), "%d de %B, %Y")),
      x = "Mes",
      y = "Carpetas iniciadas por mes"
    ) +
    scale_x_date(date_breaks = "5 months", date_labels = "%b\n%Y",
                 date_minor_breaks = "5 months",
                 limits = c(as_date(as_date(fecha_comienzo) - 31), NA)) +
    scale_y_continuous(label = comma_format(accuracy = 1)) +
    scale_color_manual(values = col_aos, aesthetics = c("color", "fill")) +
    theme_light() +
    guides(colour = guide_legend(override.aes = list(size = 2))) +
    theme(
      legend.position = "bottom",
      legend.key = element_rect(fill = NA),
      strip.background = element_rect(fill = "gray80"),
      strip.text = element_text(color = "black", face = "bold"),
      plot.title = element_text(face = "bold")
    )
  
  if(comp == FALSE) {
    tendencias <- 
      tendencias +
      labs(caption = "La tendencia no considera el mes en curso")
  }  
  
  if (tipo == "victimas") {
    tendencias <- 
      tendencias +
      labs(title = paste0("Víctimas (tendencias anuales) - ", delitos_sel))
  }
  
  
  return(tendencias)
}

## Mapas alcaldias -------------------------------------------------------------

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

## Mapas ct -------------------------------------------------------------
plot_ct_alc <- function(
  base, 
  tipo = c("incidencia", "victimas"),
  nombre = nombre_plots, 
  fecha_comienzo = fecha_comienzo,
  fecha_lim = fecha_lim,
  alcaldia_sel = alcaldia_sel,
  shape_alcaldias = shape_alcaldias,
  shape_ct = shape_ct,
  clave_ct = clave_ct
) {
  
  tabla_ct <-
    base %>% 
    filter(fecha_inicio >= as_date(fecha_comienzo)) %>% 
    mutate(ct = ifelse(grepl("-0", ct_hechos) | is.na(ct_hechos), paste0(clave_ct, "-0"), ct_hechos)) %>% 
    group_by(agencia = ct) %>% 
    summarise("CI totales" = n(),
              .groups = "drop") 
  
  
  
  shape_plot <- 
    shape_ct %>% 
    mutate(
      alcaldia = case_when(
        delegacion == "CUAJIMALPA" ~ "CUAJIMALPA DE MORELOS",
        delegacion == "GUSTAVO A. MADERO" ~ "GUSTAVO A MADERO",
        delegacion == "MAGDALENA CONTRERAS" ~ "LA MAGDALENA CONTRERAS",
        TRUE ~ delegacion
      )
    ) %>% 
    filter(alcaldia == alcaldia_sel) %>% 
    select(agencia, geometry) %>% 
    left_join(tabla_ct)
  
  
  mapa <- 
    ggplot() +
    geom_sf(data = st_as_sf(shape_alcaldias %>% 
                              filter(alcaldia == alcaldia_sel)), 
            aes(geometry = geometry), fill = "white") +
    geom_sf(data = st_as_sf(shape_plot), 
            aes(geometry = geometry, fill = `CI totales`), 
            color = "white", size = .3, alpha = .7) +
    scale_fill_gradientn(colors = c("#00b140", "#f9a819", "#ff4f4f")) +
    labs(
      title = paste0("Concentración de CI\n", nombre),
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
  
  
  
  tabla_ct <- 
    tabla_ct %>% 
    rename("CT" = agencia) %>%
    mutate(
      Representa = `CI totales`/sum(`CI totales`),
      ord = extract_numeric(CT) * -1
    ) %>% 
    arrange(ord) %>% 
    adorn_totals(na.rm = T) %>% 
    select(-ord) %>%     
    mutate(
      `CI totales` = ifelse(is.na(`CI totales`), "-", 
                            comma(`CI totales`, accuracy = 1)
      ),
      Representa = percent(Representa)
    )
  
  
  
  tt_alc <- ttheme_minimal(
    base_size = 10,
    padding = unit(c(4,4), "mm"),
    core = list(bg_params = list(col = NA,
                                 fill=c(rep(c(NA),
                                            length.out = 16), "#bde0fe"))),
    colhead = list(fg_params = list(col = "white", fontface = "bold"),
                   bg_params = list(fill=c("#233d4d"))))
  
  tabla_final <- 
    tableGrob(tabla_ct, rows = NULL, theme = tt_alc) %>% 
    gtable_add_grob(grobs = replicate(ncol(.) - 1,
                                      segmentsGrob(x1 = unit(0, "npc"), 
                                                   gp = gpar(lty = 2)),
                                      simplify = FALSE),
                    t = 2, b = nrow(.), l = seq_len(ncol(.) - 1) + 1) 
  
  tabla_final <- plot_grid(tabla_final)
  
  final <- plot_grid(tabla_final, NULL,  mapa, ncol = 3, rel_widths = c(1, 0.05, 1))
  return(final)
}

plot_ct_cdmx <- function(
  base, 
  tipo = c("incidencia", "victimas"),
  nombre = nombre_plots, 
  fecha_comienzo = fecha_comienzo,
  fecha_lim = fecha_lim,
  shape_alcaldias = shape_alcaldias,
  shape_ct = shape_ct,
  tema = tema
) {
  
  tabla_ct <-
    base %>% 
    filter(fecha_inicio >= as_date(fecha_comienzo)) %>% 
    # mutate(ct = ifelse(grepl("-0", ct_hechos) | is.na(ct_hechos), paste0(clave_ct, "-0"), ct_hechos)) %>% 
    group_by(agencia = ct_hechos) %>% 
    summarise("CI totales" = n(),
              .groups = "drop") %>% drop_na(agencia) %>% 
    filter(!grepl("-0", agencia))
  
  tabla <- 
    tabla_ct %>% 
    arrange(desc(`CI totales`)) %>% 
    head(15) %>% 
    mutate(`CI totales` = comma(`CI totales`, accuracy = 1)) %>% 
    rename("CT hechos" = agencia)
  
  shape_plot <- 
    shape_ct %>% 
    mutate(
      alcaldia = case_when(
        delegacion == "CUAJIMALPA" ~ "CUAJIMALPA DE MORELOS",
        delegacion == "GUSTAVO A. MADERO" ~ "GUSTAVO A MADERO",
        delegacion == "MAGDALENA CONTRERAS" ~ "LA MAGDALENA CONTRERAS",
        TRUE ~ delegacion
      )
    ) %>% 
    select(agencia, geometry) %>% 
    left_join(tabla_ct)
  
  
  mapa <- 
    ggplot() +
    geom_sf(data = st_as_sf(shape_plot), 
            aes(geometry = geometry, fill = `CI totales`), 
            color = "white", size = .3, alpha = .7) +
    geom_sf(data = st_as_sf(shape_alcaldias), 
            aes(geometry = geometry), fill = NA) +
    scale_fill_gradientn(colors = c("#00b140", "#f9a819", "#ff4f4f")) +
    labs(
      title = paste0("Concentración de CI\n", nombre),
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
  
  
  
  tabla_final <- 
    tableGrob(tabla, rows = NULL, theme = tema) %>% 
    gtable_add_grob(grobs = replicate(ncol(.) - 1,
                                      segmentsGrob(x1 = unit(0, "npc"), 
                                                   gp = gpar(lty = 2)),
                                      simplify = FALSE),
                    t = 2, b = nrow(.), l = seq_len(ncol(.) - 1) + 1) 
  
  
  tabla_final <- plot_grid(tabla_final, labels = paste("Top", nrow(tabla), "coordinaciones"))
  
  final <- plot_grid(mapa, NULL,  tabla_final, 
                     ncol = 3, rel_widths = c(1, 0.05, 1))
  return(final)
  
  
}

# plot_ct_cambio <- function(
#   base, 
#   tipo = c("incidencia"),
#   nombre = nombre_plots, 
#   fecha_comienzo = fecha_comienzo,
#   fecha_lim = fecha_lim,
#   tema = tema
# )
# 

## Tabla año/mes ---------------------------------------------------------------

tabla_mensual <- function(base, base_vic, ambas) {
  tt_year <- ttheme_minimal(
    base_size = 10,
    padding = unit(c(4,4), "mm"),
    core = list(bg_params = list(col = NA,
                                 fill=c(rep(c(NA),
                                            length.out = 12), "#bde0fe"))),
    colhead=list(fg_params = list(col = "white", fontface = "bold"),
                 bg_params = list(fill=c("#003763"))))
  
  
  
  tab <- 
    base %>% 
    count(
      ao = year(fecha_inicio),
      mes = month(fecha_inicio, abbr = F, label = T)
    ) %>% 
    # mutate(n = comma(n, accuracy = 1)) %>% 
    pivot_wider(names_from = ao, values_from = n) %>% 
    adorn_totals() %>% 
    mutate(
      across(-mes, ~comma(.x, accuracy = 1)),
      across(-mes, ~replace_na(.x, "-")),
      mes = factor(mes, levels = c("enero", "febrero", "marzo",
                                   "abril", "mayo", "junio",
                                   "julio", "agosto", "septiembre",
                                   "octubre", "noviembre", "diciembre",
                                   "Total"))
    ) %>% 
    arrange(mes) %>% 
    rename("Mes/Año" = mes) 
  
  
  
  tabla_final <- 
    tableGrob(tab, rows = NULL, theme = tt_year) %>% 
    gtable_add_grob(grobs = replicate(ncol(.) - 1,
                                      segmentsGrob(x1 = unit(0, "npc"), 
                                                   gp = gpar(lty = 2)),
                                      simplify = FALSE),
                    t = 2, b = nrow(.), l = seq_len(ncol(.) - 1) + 1)  
  
  incidencia <- plot_grid(tabla_final, labels = "Incidencia")
  
  if(ambas == TRUE) {
    tab_vic <- 
      base_victimas %>% 
      count(
        ao = year(fecha_inicio),
        mes = month(fecha_inicio, abbr = F, label = T)
      ) %>% 
      # mutate(n = comma(n, accuracy = 1)) %>% 
      pivot_wider(names_from = ao, values_from = n) %>% 
      adorn_totals() %>% 
      mutate(
        across(-mes, ~comma(.x, accuracy = 1)),
        across(-mes, ~replace_na(.x, "-")),
        mes = factor(mes, levels = c("enero", "febrero", "marzo",
                                     "abril", "mayo", "junio",
                                     "julio", "agosto", "septiembre",
                                     "octubre", "noviembre", "diciembre",
                                     "Total"))
      ) %>% 
      arrange(mes) %>% 
      rename("Mes/Año" = mes) 
    
    
    
    tabla_final_vic <- 
      tableGrob(tab_vic, rows = NULL, theme = tt_year) %>% 
      gtable_add_grob(grobs = replicate(ncol(.) - 1,
                                        segmentsGrob(x1 = unit(0, "npc"), 
                                                     gp = gpar(lty = 2)),
                                        simplify = FALSE),
                      t = 2, b = nrow(.), l = seq_len(ncol(.) - 1) + 1) 
    
    victimas <- plot_grid(tabla_final_vic, labels = "Víctimas")
    final <- plot_grid(incidencia, victimas)
  } else {
    final <- incidencia
  }
  
  return(final)
  
}


## Mensual barras con tabla inc ----------------------------------
barras_fecha <- function(base, tipo, nombre, fecha_comienzo,
                         fecha_lim, tema, base_vic, ambas) {
  p <- plot_barras_mensual(base, tipo, nombre, fecha_comienzo, fecha_lim, tema)
  t <- tabla_mensual(base, base_vic, ambas)
  plot_grid(p, NULL, t, 
            ncol = 3, rel_widths = c(1.6, .05, .8)
  )
}

###Función para armar la base y realizar gráfica anual


# plot_anual <- function(
#   base, 
#   tipo = c("incidencia"),
#   nombre = nombre_plots, 
#   fecha_comienzo = fecha_comienzo,
#   fecha_lim = fecha_lim#,
#   # tema_tablas = tema
# ) {
#   tema <- ttheme_minimal(
#     base_size = 12,
#     padding = unit(c(4,4), "mm"),
#     colhead = list(fg_params = list(col = "white", fontface = "bold"),
#                    bg_params = list(fill=c("#003763"))))
#   
#   serie_2021 <- 
#     base %>% 
#     filter(fecha_inicio >= as_date(fecha_comienzo)) %>% 
#     group_by(ao=year(fecha_inicio)) %>% summarise(total=n())
#   
#   
#   tabla <-
#     serie_2021 %>% 
#     rename(Año=ao, Total=total) %>% 
#     mutate("Cambio"=percent(Total/lag(Total)-1, accuracy = .01),
#            "Cambio\nporcentual"=ifelse(Año %in% c(serie_2021 %>% slice(1) %>% pull(ao),
#                                                   serie_2021 %>% slice(nrow(.)) %>% pull(ao)),
#                                        "-", Cambio),
#       Total=comma(Total, accuracy = 1)) %>% select(-Cambio)
#   
#   plot_diaria <- 
#     ggplot(serie_2021, aes(x = ao, y = total, fill=as.factor(ao))) +
#     geom_bar(stat = "identity") +
#     scale_fill_manual(values = col_aos) +
#     geom_label(aes(label=comma(total, accuracy = 1)), 
#                fill="ghostwhite") +
#     labs(
#       title = str_wrap(paste0("Incidencia delictiva por año - ", nombre),65),
#       subtitle = paste0("Del ", format(as_date(fecha_comienzo), "%d de %B de %Y"), " al ",
#                         format(as_date(fecha_lim), "%d de %B de %Y")),
#       x = "Año de inicio",
#       y = "Carpetas iniciadas"
#     ) +
#     theme_light() +
#     theme(
#       plot.title = element_text(face = "bold"), 
#       legend.position = "none"
#     )
#   # 
#   # if(year(fecha_comienzo) != 2021) {
#   #         plot_diaria <- 
#   #                 plot_diaria +
#   #                 geom_vline(data = serie_2021 %>% 
#   #                                    filter(day(fecha_inicio) == 1 & month(fecha_inicio) == 1),
#   #                            aes(xintercept = fecha_inicio), linetype = "dashed",
#   #                            color = "gray50", size = 1.002) +
#   #                 scale_x_date(date_breaks = "3 months", date_minor_breaks = "1 month",
#   #                              date_labels = "%B\n%Y")
#   # }
#   # 
#   tb <- plot_grid(
#     tableGrob(tabla, rows = NULL, theme = tema) %>% 
#       gtable_add_grob(grobs = replicate(ncol(.) - 1,
#                                         segmentsGrob(x1 = unit(0, "npc"), 
#                                                      gp = gpar(lty = 2)),
#                                         simplify = FALSE),
#                       t = 2, b = nrow(.), l = seq_len(ncol(.) - 1) + 1) %>% 
#       gtable_add_grob(grobs = replicate(nrow(.) - 1,
#                                         segmentsGrob(y1 = unit(0, "npc"),
#                                                      gp = gpar(lty = 2)),
#                                         simplify = FALSE),
#                       t = 2, l = 1, b = seq_len(nrow(.) -1) , r = 2)
#   )
#   
#   total <- serie_2021 %>% summarise(Total=sum(total)) %>% 
#     ggplot() +
#     annotate(geom = "text", x=0, y=0, 
#              label=paste0(comma(sum(serie_2021$total), accuracy = 1),"\n",
#                           "Carpetas iniciadas"), 
#              size=6.5, colour=colores[1]) +
#     theme_void() + theme(text = element_text(face = "bold"))
#   
#   tabla_final <- plot_grid(total,tb, nrow = 2, 
#                            rel_heights = c(5, 6))
#   final <- plot_grid(plot_diaria, NULL, tabla_final, 
#                      ncol = 3, rel_widths = c(1.7, .05, .5))
#   
#   return(final)
#   
# }

plot_anual <- function(
  base = base,
  tipo = c("incidencia"),
  nombre = nombre_plots, 
  fecha_comienzo = fecha_comienzo,
  fecha_lim = fecha_lim,
  comparacion_mensual= c(1,0)
  # tema_tablas = tema
) {
  tema <- ttheme_minimal(
    base_size = 15,
    padding = unit(c(3,3), "mm"),
    colhead = list(fg_params = list(col = "white", fontface = "bold"),
                   bg_params = list(fill=c("#003763"))))
  
  serie_2021 <- 
    base %>% 
    filter(fecha_inicio >= as.Date(fecha_comienzo)) %>% 
    group_by(ao=year(fecha_inicio)) %>% summarise(total=n())
  
  if(comparacion_mensual==1){
    serie_2021 <- 
      base %>%
      filter(fecha_inicio >= as.Date(fecha_comienzo)) %>% 
      group_by(ao=year(fecha_inicio)) %>% summarise(total=n())
    
    mes_serie <- base %>% 
      filter(fecha_inicio >= as.Date(fecha_comienzo), 
             yday(fecha_inicio)<=yday(fecha_lim)) %>% 
      group_by(ao=year(fecha_inicio)) %>% summarise(total_m=n())
    serie_2021 <- left_join(serie_2021, mes_serie) 
    
    tabla <-
      serie_2021 %>% 
      set_names(c("Año", "Total", paste0("Ene-", month(max(base$fecha_inicio), label = T)))) %>% 
      mutate("Cambio"=percent(Total/lag(Total)-1, accuracy = .01),
             "Cambio\nporcentual"=ifelse(Año %in% c(serie_2021 %>% slice(1) %>% pull(ao),
                                                    serie_2021 %>% slice(nrow(.)) %>% pull(ao)),
                                         "-", Cambio),
             Total=comma(Total, accuracy = 1)) %>% select(-Cambio) %>% 
      relocate(`Cambio\nporcentual`, .after = "Total")
    tabla[is.na(tabla)] <- 0
    
    plot_diaria <- 
      ggplot(serie_2021, aes(x = ao, y = total, fill=as.factor(ao))) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = col_aos2) +
      # geom_label_repel(aes(label=comma(total, accuracy = 1), y=total*1.15),
      #            fill="ghostwhite", size=6.3, direction = "y", max.time = 4, force_pull = 1.5) +
      geom_label(aes(label=comma(total, accuracy = 1), y=total*1.1),
                       fill="ghostwhite", size=6.3) +
      labs(
        title = str_wrap(paste0("Incidencia delictiva por año - ", nombre),65),
        subtitle = paste0("Del ", format(as.Date(fecha_comienzo), "%d de %B de %Y"), " al ",
                          format(as.Date(fecha_lim), "%d de %B de %Y")),
        x = "Año de inicio",
        y = "Carpetas iniciadas",
        caption = paste0("Las líneas punteadas indican la incidencia hasta el ",
                         format(as.Date(fecha_lim), "%d de %B"), " de cada año", 
                         "\n Fuente:Unidad de Estadística y Transparencia")
      ) +
      theme_light() +
      theme(
        plot.title = element_text(face = "bold"), 
        legend.position = "none"
      ) +
      geom_segment(data=serie_2021 %>% filter(ao<max(ao)), 
                   aes(x = ao-.45, y = total_m, xend = ao+.458, yend = total_m), 
                   color="black", linetype="longdash",
                   size=2) +
      geom_label_repel(data=serie_2021 %>% filter(ao<max(ao)),
                 aes(label=comma(total_m, accuracy = 1), y=total_m*.70), 
                 fill="ghostwhite", size=6.3, direction = "y", max.time = 8, 
                 max.overlaps = 2, max.iter = 30000)+
      scale_x_continuous(breaks = unique(serie_2021$ao))
  } else {
    tabla <-
      serie_2021 %>% 
      rename(Año=ao, Total=total) %>% 
      mutate("Cambio"=percent(Total/lag(Total)-1, accuracy = .01),
             "Cambio\nporcentual"=ifelse(Año %in% c(serie_2021 %>% slice(1) %>% pull(ao),
                                                    serie_2021 %>% slice(nrow(.)) %>% pull(ao)),
                                         "-", Cambio),
             Total=comma(Total, accuracy = 1)) %>% select(-`Cambio\nporcentual`) %>% 
      replace_na(list(Cambio="-"))
    
    plot_diaria <- 
      ggplot(serie_2021, aes(x = ao, y = total, fill=as.factor(ao))) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = col_aos2) +
      geom_label(aes(label=comma(total, accuracy = 1)), 
                 fill="ghostwhite", size=6.3) +
      labs(
        title = str_wrap(paste0("Incidencia delictiva por año - ", nombre),65),
        subtitle = paste0("Del ", format(as.Date(fecha_comienzo), "%d de %B de %Y"), " al ",
                          format(as.Date(fecha_lim), "%d de %B de %Y")),
        x = "Año de inicio",
        y = "Carpetas iniciadas"
      ) +
      theme_light() +
      theme(
        text = element_text(size = 13),
        plot.title = element_text(face = "bold"), 
        legend.position = "none"
      )+
      scale_x_continuous(breaks = unique(serie_2021$ao))
    # 
    # if(year(fecha_comienzo) != 2021) {
    #         plot_diaria <- 
    #                 plot_diaria +
    #                 geom_vline(data = serie_2021 %>% 
    #                                    filter(day(fecha_inicio) == 1 & month(fecha_inicio) == 1),
    #                            aes(xintercept = fecha_inicio), linetype = "dashed",
    #                            color = "gray50", size = 1.002) +
    #                 scale_x_date(date_breaks = "3 months", date_minor_breaks = "1 month",
    #                              date_labels = "%B\n%Y")
    # }
    # 
    
  }
  tb <- plot_grid(
    tableGrob(tabla, rows = NULL, theme = tema) %>% 
      gtable_add_grob(grobs = replicate(ncol(.) - 1,
                                        segmentsGrob(x1 = unit(0, "npc"), 
                                                     gp = gpar(lty = 2)),
                                        simplify = FALSE),
                      t = 2, b = nrow(.), l = seq_len(ncol(.) - 1) + 1) %>% 
      gtable_add_grob(grobs = replicate(nrow(.) - 1,
                                        segmentsGrob(y1 = unit(0, "npc"),
                                                     gp = gpar(lty = 2)),
                                        simplify = FALSE),
                      t = 2, l = 1, b = seq_len(nrow(.) -1) , r = 2)
  )
  
  total <- serie_2021 %>% summarise(Total=sum(total)) %>% 
    ggplot() +
    annotate(geom = "text", x=0, y=0, 
             label=paste0(comma(sum(serie_2021$total), accuracy = 1),"\n",
                          "Carpetas\niniciadas"), 
             size=10, colour="ghostwhite") +
    theme_void() + theme(text = element_text(face = "bold"), 
                         plot.background = element_rect(fill = "#003763"))
  
  tabla_final <- plot_grid(NULL, total,tb, nrow = 3, 
                           rel_heights = c(1, 5, 6))
  final <- plot_grid(plot_diaria, NULL, tabla_final, 
                     ncol = 3, rel_widths = c(1.5, .05, .5))
  
  return(final)
  
}

#modificación al grágico de barras semanal
plot_barras2 <- function(
  base, 
  tipo = c("incidencia", "victimas", "pads"), 
  nombre = str_to_lower(nombre_plots),
  fecha_comienzo = fecha_inicio_global,
  fecha_lim = fecha_lim,
  tema_tablas = tema2, 
  semana=c(1,0)
) {
  
  if (tipo == "pads") {
    serie <- 
      base %>% 
      mutate(existe = 1) %>% 
      complete(fecha_inicio = seq.Date(from = as_date("2019-01-01"),
                                       to = as_date(fecha_lim),
                                       by = "day"),
               fill = list(existe = 0)) %>% 
      group_by(
        # semana = isoweek(fecha_inicio),
        semana = floor_date(fecha_inicio, "week", week_start = 6)
        # ao = year(fecha_inicio)
      ) %>% 
      summarise(
        tot_semana = sum(existe),
        fr = min(fecha_inicio),
        to = max(fecha_inicio),
        .groups = "drop"
      ) %>% 
      mutate(
        ao = year(semana),
        keep = ifelse(semana == fr, 1, 0)
      ) %>% 
      filter(keep == 1) %>% 
      group_split(ao) %>% 
      map(~rowid_to_column(.x, "semana_use")) %>% 
      bind_rows()
  } else {
    
    serie <- 
      base %>% 
      mutate(existe = 1) %>% 
      complete(fecha_inicio = seq.Date(from = as_date(fecha_comienzo),
                                       to = as_date(fecha_lim),
                                       by = "day"),
               fill = list(existe = 0)) %>% 
      group_by(
        # semana = isoweek(fecha_inicio),
        semana = floor_date(fecha_inicio, "week", week_start = 6)
        # ao = year(fecha_inicio)
      ) %>% 
      summarise(
        tot_semana = sum(existe),
        fr = min(fecha_inicio),
        to = max(fecha_inicio),
        .groups = "drop"
      ) %>% 
      mutate(
        ao = year(semana),
        keep = ifelse(semana == fr, 1, 0)
      ) %>% 
      filter(keep == 1) %>% 
      group_split(ao) %>% 
      map(~rowid_to_column(.x, "semana_use")) %>% 
      bind_rows()
  }
  
  
  dat_21 <- 
    serie %>% 
    filter(ao == max(ao))
  
  
  dat_ant <- 
    serie %>% 
    filter(ao != max(ao))
  
  sem <- as.numeric(max(dat_21$semana_use))
  
  # Plot
  serie_semanas <- 
    ggplot() +
    geom_bar(data = dat_21,
             aes(x = semana_use, y = tot_semana, color = factor(ao), fill = factor(ao)),
             stat = "identity", position = "dodge", alpha = .6, width = .6, show.legend = F) +
    geom_point(data = dat_ant %>% 
                 filter(semana_use != sem), 
               aes(x = semana_use, y = tot_semana, color = factor(ao)),
               alpha = .3, show.legend = F) +
    geom_point(data = dat_ant %>% 
                 filter(semana_use == sem), 
               aes(x = semana_use, y = tot_semana, color = factor(ao)),
               alpha = .9, show.legend = F, size = 2) +
    geom_smooth(data = dat_ant, 
                aes(x = semana_use, y = tot_semana, color = factor(ao)),
                se = F, span = .65) +
    labs(
      title = str_wrap(paste0("Incidencia delictiva - ", nombre),65),
      subtitle = paste0("Corte al ", format(as_date(fecha_lim), "%A %d de %B, %Y")),
      x = "Semana",
      y = "Carpetas iniciadas por semana",
      caption = "Semanas de sábado a viernes",
      color = "Año de inicio de las carpetas",
      fill = "Año de inicio de las carpetas"
    ) +
    scale_y_continuous(label = comma_format(accuracy = 1)) +
    scale_color_manual(values = col_aos2, aesthetics = c("color", "fill")) +
    theme_light() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold")
    ) 
  
  if (tipo == "victimas") {
    serie_semanas <- 
      serie_semanas +
      labs(
        title = str_wrap(paste0("Incidencia delictiva (víctimas) - ", nombre),65)
      )
  }
  
  if (tipo == "pads") {
    serie_semanas <- 
      serie_semanas +
      labs(
        title = str_wrap(paste0("Puestas a disposición (flagrancia) - ", nombre),65),
        y = "Personas puestas a disposición"
      )
  }
  
  
  
  
  if(tipo == "pads") {
    # Tabla 1
    
    # tabla_totales <- 
    #         base %>% 
    #         group_by(
    #                 fecha_inicio
    #         ) %>% 
    #         summarise(
    #                 tot_dia = n(),
    #                 .groups = "drop"
    #         ) %>% 
    #         complete(fecha_inicio = seq.Date(from = as_date("2019-01-01"),
    #                                          to = as_date(fecha_lim),
    #                                          by = "day"),
    #                  fill = list(tot_dia = 0)) %>% 
    #         group_by(ao = year(fecha_inicio)) %>% 
    #         summarise(Total = comma(sum(tot_dia), accuracy =  1),
    #                   "Prom. diario" = comma(round(mean(tot_dia), 2), accuracy = .01)) %>% 
    #         pivot_longer(cols = -ao, names_to = " ") %>% 
    #         pivot_wider(names_from = ao, values_from = value)
    
    tabla_totales <- serie %>% group_by(ao) %>% summarise(Promedio=comma(mean(tot_semana), accuracy=.01)) %>%
      spread(ao, Promedio) %>% bind_cols(data.frame(Año=c("Promedio\n semanal"))) %>%
      select(Año, `2018`:`2022`)
    
    tb <- 
      tableGrob(tabla_totales, rows = NULL, theme = tema2) %>% 
      gtable_add_grob(grobs = replicate(ncol(.) - 1,
                                        segmentsGrob(x1 = unit(0, "npc"), 
                                                     gp = gpar(lty = 2)),
                                        simplify = FALSE),
                      t = 2, b = nrow(.), l = seq_len(ncol(.) - 1) + 1) %>% 
      gtable_add_grob(grobs = segmentsGrob( # line across the bottom
        x0 = unit(0,"npc"),
        y0 = unit(0,"npc"),
        x1 = unit(1,"npc"),
        y1 = unit(0,"npc"),
        gp = gpar(lty = 2.0)),
        t = 2, b = 2, l = 1, r = ncol(.))
    
    tabla_comp <-
      tibble(
        Iniciadas = dat_21$tot_semana[dat_21$semana_use == sem],
        "Semana anterior" = dat_21$tot_semana[dat_21$semana_use == (sem - 1)],
        "Promedio semanal\nde la gestión" = round(mean(serie$tot_semana), 2),
        "Misma semana\n (Año anterior)" = serie$tot_semana[serie$semana_use == sem & serie$ao == max(serie$ao)-1]
      ) %>%
      pivot_longer(-Iniciadas, names_to = "tiempo") %>%
      mutate(
        diferencia = Iniciadas - value,
        color = ifelse(diferencia < 0, "#f94144", "#90be6d"),
        cambio = (Iniciadas/value) - 1,
        value = comma(value, accuracy = .1),
        texto = paste0("Diferencia: ", comma(diferencia, accuracy = .1),
                       "\n(", percent(cambio, accuracy = .1), ")")
      ) %>%
      select(-c(Iniciadas, diferencia, cambio)) %>%
      pivot_longer(-c(tiempo, color), values_to = "valor") %>%
      mutate(
        name = factor(name, levels = c("value", "texto")),
        color = ifelse(name == "value", "black", color)
      )
  } else {
    
    # Tabla 1
    
    # tabla_totales <- 
    #         base %>% 
    #         group_by(
    #                 fecha_inicio
    #         ) %>% 
    #         summarise(
    #                 tot_dia = n(),
    #                 .groups = "drop"
    #         ) %>% 
    #         complete(fecha_inicio = seq.Date(from = as_date(fecha_comienzo),
    #                                          to = as_date(fecha_lim),
    #                                          by = "day"),
    #                  fill = list(tot_dia = 0)) %>% 
    #         group_by(ao = year(fecha_inicio)) %>% 
    #         summarise(Total = comma(sum(tot_dia), accuracy =  1),
    #                   "Prom. diario" = comma(round(mean(tot_dia), 2), accuracy = .01)) %>% 
    #         pivot_longer(cols = -ao, names_to = " ") %>% 
    #         pivot_wider(names_from = ao, values_from = value)
    
    tabla_totales <- serie %>% group_by(ao) %>% summarise(Promedio=mean(tot_semana)) %>%
      mutate(Cambio=percent(Promedio/lag(Promedio)-1, accuracy = .01), 
             Promedio=comma(Promedio, accuracy = .01)) %>%
      gather(Indice, Total, Promedio, Cambio) %>% 
      spread(ao, Total, fill = "-") %>% arrange(desc(Indice)) %>% 
      rename(Año=Indice) %>% 
      mutate(Año=ifelse(Año=="Promedio", "Promedio\nsemanal", Año))
      # bind_cols(data.frame(Año=c("Promedio\n semanal"))) %>%
      # select(Año, starts_with("20"))
    
    tb <- 
      tableGrob(tabla_totales, rows = NULL, theme = tema2) %>% 
      gtable_add_grob(grobs = replicate(ncol(.) - 1,
                                        segmentsGrob(x1 = unit(0, "npc"), 
                                                     gp = gpar(lty = 2)),
                                        simplify = FALSE),
                      t = 2, b = nrow(.), l = seq_len(ncol(.) - 1) + 1) %>% 
      gtable_add_grob(grobs = segmentsGrob( # line across the bottom
        x0 = unit(0,"npc"),
        y0 = unit(0,"npc"),
        x1 = unit(1,"npc"),
        y1 = unit(0,"npc"),
        gp = gpar(lty = 2.0)),
        t = 2, b = 2, l = 1, r = ncol(.))
    
    
    
    tabla_comp <-
      tibble(
        Iniciadas = dat_21$tot_semana[dat_21$semana_use == sem],
        "Semana anterior" = dat_21$tot_semana[dat_21$semana_use == (sem - 1)],
        "Promedio semanal\nde la gestión" = round(mean(serie$tot_semana), 2),
        "Misma semana\n (Año anterior)" = serie$tot_semana[serie$semana_use == sem & serie$ao == max(serie$ao)-1]
      ) %>%
      pivot_longer(-Iniciadas, names_to = "tiempo") %>%
      mutate(
        diferencia = Iniciadas - value,
        color = ifelse(diferencia > 0, "#f94144", "#90be6d"),
        cambio = (Iniciadas/value) - 1,
        value = comma(value, accuracy = .1),
        texto = paste0("Diferencia: ", comma(diferencia, accuracy = .1),
                       "\n(", percent(cambio, accuracy = .1), ")")
      ) %>%
      select(-c(Iniciadas, diferencia, cambio)) %>%
      pivot_longer(-c(tiempo, color), values_to = "valor") %>%
      mutate(
        name = factor(name, levels = c("value", "texto")),
        color = ifelse(name == "value", "black", color)
      )
  }
  
  
  comparacion <-
    ggplot(tabla_comp,
           aes(x = name, y = tiempo)) +
    geom_tile( fill = NA) +
    geom_text(aes(label = valor, color = color), size = 3.2, fontface = "bold") +
    labs(
      x = paste0(comma(dat_21$tot_semana[dat_21$semana_use == sem]), "\n\nCI iniciadas")
    ) +
    theme_void() +
    theme_minimal() +
    geom_vline(xintercept = c(.5, 1.5), alpha = .5, color = "gray75") +
    geom_hline(yintercept = c(1.5, 2.5), alpha = .5, color = "gray75") +
    scale_fill_identity(aesthetics = c("fill", "color")) +
    scale_x_discrete(position = "top")  +
    theme(
      axis.text.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_text(hjust = 1, size = 9),
      axis.title.x = element_text(size = 14, face = "bold", color = col_aos2[4],
                                  vjust = .5, angle = 0),
      axis.line = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank()
    )
  
  if(semana == 1) {
  
  t_fin <- plot_grid(NULL, comparacion, tb, NULL,
                     nrow = 4, rel_heights = c(.4, 1, .3, .4))
  } else{
    t_fin <- plot_grid(tb)
  }
  final <- plot_grid(serie_semanas, t_fin, 
                     nrow = 1, rel_widths = c(3, 2.2))
  
  return(final)
}

#modificación de la evoluación diaria
plot_2021_b <- function(
  base, 
  tipo = c("incidencia", "victimas"),
  nombre = nombre_plots, 
  fecha_comienzo = fecha_comienzo,
  fecha_lim = fecha_lim,
  tema_tablas = tema
) {
if (fecha_inicio_global<"2021-01-01")  {
  serie_2021 <- 
    base %>% 
    filter(fecha_inicio>=fecha_comienzo,
      fecha_inicio >= as_date(fecha_lim)-760) %>% 
    count(fecha_inicio, name = "tot_dia") %>% 
    complete(fecha_inicio = seq.Date(from = as_date(fecha_lim)-760,
                                     to = as_date(fecha_lim),
                                     by = "day"),
             fill = list(tot_dia = 0)) 
} else {
  serie_2021 <- 
    base %>% 
    filter(fecha_inicio>=fecha_comienzo,
           fecha_inicio >= as_date(fecha_lim)-760) %>% 
    count(fecha_inicio, name = "tot_dia") %>% 
    complete(fecha_inicio = seq.Date(from = as_date(fecha_comienzo),
                                     to = as_date(fecha_lim),
                                     by = "day"),
             fill = list(tot_dia = 0))   
  }    
  
  tabla <- 
    serie_2021 %>% 
    # filter(year(fecha_inicio) == 2021) %>% 
    group_by(
      ao = year(fecha_inicio),
      mes = month(fecha_inicio, label = T, abbr = F)
    ) %>% 
    summarise(
      Total = comma(sum(tot_dia), accuracy =  1),
      prom = round(mean(tot_dia), 2)
    ) %>% 
    ungroup() %>% tail(24) %>% 
    mutate(fecha = as_date(paste(ao, mes, "01", sep = "-"))) %>% 
    arrange(fecha) %>% 
    mutate(
      mes = format(fecha, "%b %Y"),
      "Cambio\nporcentual" = percent((prom/lag(prom)) - 1),
      `Cambio\nporcentual` = ifelse(`Cambio\nporcentual` == Inf, "100%", `Cambio\nporcentual`),
      prom = comma(prom, accuracy = .01)
    ) %>% 
    select(-c(ao, fecha, Total)) %>% 
    replace_na(list("Cambio\nporcentual" = "-")) %>% 
    rename("Prom.\ndiario" = prom,
           "Mes" = mes)
  
  plot_diaria <- 
    ggplot(serie_2021, aes(x = fecha_inicio, y = tot_dia)) +
    geom_vline(data = serie_2021 %>% 
                 filter(day(fecha_inicio) == 1),
               aes(xintercept = fecha_inicio), linetype = "dashed",
               color = "gray65") +
    geom_point(alpha = .3, col = colores[5]) +
    tidyquant::geom_ma(n = 7, color = colores[5], size = 1.01,
                       linetype = "solid") +
    geom_smooth(color = colores[5], size = 1.1, se = F) +
    labs(
      title = str_wrap(paste0("Evolución diaria - ", nombre),65),
      subtitle = paste0("Del ", format(as_date(fecha_comienzo), "%d de %B de %Y"), " al ",
                        format(as_date(fecha_lim), "%d de %B de %Y")),
      x = "Fecha de inicio",
      y = "Carpetas iniciadas",
      caption = "Con promedio móvil de 7 días"
    ) +
    theme_light() +
    theme(
      plot.title = element_text(face = "bold")
    )
  
  if(year(fecha_comienzo) != 2021) {
    plot_diaria <- 
      plot_diaria +
      geom_vline(data = serie_2021 %>% 
                   filter(day(fecha_inicio) == 1 & month(fecha_inicio) == 1),
                 aes(xintercept = fecha_inicio), linetype = "dashed",
                 color = "gray50", size = 1.002) +
      scale_x_date(date_breaks = "3 months", date_minor_breaks = "1 month",
                   date_labels = "%B\n%Y")
  }
  
  tb <- plot_grid(
    tableGrob(tabla, rows = NULL, theme = tema_tablas) %>% 
      gtable_add_grob(grobs = replicate(ncol(.) - 1,
                                        segmentsGrob(x1 = unit(0, "npc"), 
                                                     gp = gpar(lty = 2)),
                                        simplify = FALSE),
                      t = 2, b = nrow(.), l = seq_len(ncol(.) - 1) + 1) #%>% 
      # gtable_add_grob(grobs = replicate(nrow(.) - 1,
      #                                   segmentsGrob(y1 = unit(0, "npc"), 
      #                                                gp = gpar(lty = 2)),
      #                                   simplify = FALSE),
      #                 t = 2, l = 1, b = seq_len(nrow(.) - 1) , r = 3) 
  )
  
  
  final <- plot_grid(plot_diaria, NULL, tb, 
                     ncol = 3, rel_widths = c(1.7, .05, .5))
  
  return(final)
  
}


cuadro_comparativo_trend <-  function(
  
  base, 
  fecha_comienzo = fecha_comienzo, 
  fecha_lim = fecha_lim, 
  periodo = c("semana", "mes")
) {
   if (periodo=="mes"){
     #versión de mes
     data <- base %>% filter(fecha_inicio>= as_date(fecha_comienzo)) %>%
       group_by(mes=as_date(paste0(year(fecha_inicio), "-",
                                   month(fecha_inicio), "-01")),
                delito_sesnsp) %>%
       summarise(Total=n(), .groups = "drop") %>%
       filter(mes<=floor_date(as_date(fecha_lim), "month")-1) %>%
       complete(mes=seq.Date(min(mes), max(mes), by="month"),
                delito_sesnsp,
                fill=list(Total=0)) %>%
       group_by(delito_sesnsp) %>%
       mutate(trend_l=(Total[mes==max(mes)]-Total[1])/n(),
              trend_m=(Total[mes==max(mes)]-Total[n()-5])/6,
              trend_c=Total-lag(Total)) %>%
       mutate(espacio=NA,
              cambio_l=abs(trend_m)/abs(trend_l)-1,
              cambio_m=abs(trend_c)/abs(trend_m)-1) %>%
       filter(mes==max(mes))
     
     data$cambio_l[is.nan(data$cambio_l)] <- 0
     data$cambio_l[is.infinite(data$cambio_l)] <- 0
     data$cambio_m[is.nan(data$cambio_m)] <- 0
     data$cambio_m[is.infinite(data$cambio_m)] <- 0
     
     comparacion_1 <- data %>% slice_tail(n=1) %>%
       pivot_longer(c(trend_l, trend_m, trend_c, espacio, cambio_l, cambio_m)) %>%
       mutate(label=case_when(
         name=="trend_l" &  value<0 ~ paste0("Negativa", " ", round(value, 2)),
         name=="trend_l" &  value>0 ~ paste0("Positiva", " ", round(value, 2)),
         name=="trend_l" &  value==0 ~ paste0("Sin pendiente", " ", round(value, 2)),
         name=="trend_m" &  value<0 ~ paste0("Negativa", " ", round(value, 2)) ,
         name=="trend_m" &  value>0 ~ paste0("Positiva", " ", round(value, 2)) ,
         name=="trend_m" &  value==0 ~ paste0("Sin pendiente", " ", round(value, 2)),
         name=="trend_c" &  value<0 ~ paste0("Negativa", " ", round(value, 2)),
         name=="trend_c" &  value>0 ~ paste0("Positiva", " ", round(value, 2)),
         name=="trend_c" &  value==0 ~ paste0("Sin pendiente", " ", round(value, 2)),
         name=="trend_c" &  value<0 ~ paste0("Negativa", " ", round(value, 2)),
         name=="trend_c" &  value>0 ~ paste0("Positiva", " ", round(value, 2)),
         name=="trend_c" &  value==0 ~ paste0("Sin pendiente", " ", round(value, 2)),
         name=="cambio_l" ~ paste0(round(value, 2)),
         name=="cambio_m" ~ paste0(round(value, 2)),
         
         TRUE ~ " "
       ),
       valor=case_when(
         name=="trend_l" & value>0 ~5, name=="trend_l" & value<=0 ~3,
         name=="trend_m" & value>0 ~5, name=="trend_m" & value<=0 ~3,
         name=="trend_c" & value>0 ~5, name=="trend_c" & value<=0 ~3,
         name=="cambio_l" & value==0 ~3, name=="cambio_l" & value>0 & value<=5~4,
         name=="cambio_l" & value>5~5, name=="cambio_l" & value<0 & value>= -.20~2,
         name=="cambio_l" & value< -.20~1,
         name=="cambio_m" & value==1 ~3, name=="cambio_m" & value>1 & value<=5~4,
         name=="cambio_m" & value>5~5, name=="cambio_m" & value<1 & value>=.20~2,
         name=="cambio_m" & value<.20~1, TRUE~ 3
       ),
       name=factor(name, levels = c("trend_l", "trend_m", "trend_c", "espacio",
                                    "cambio_l", "cambio_m"),
                   labels = c("Tendencia\n (largo plazo)",
                              "Tendencia\n (mediano plazo)",
                              "Tendencia\n (corto plazo)",
                              " ",
                              "Veces más grande a\n mediano plazo",
                              "Crecimiento a\n corto plazo")),
       delito=str_to_sentence(delito_sesnsp)) %>%
       ggplot(aes(x=name, y=delito, fill=valor)) +
       geom_tile() +
       geom_text(aes(label=label), size=2.5) +
       scale_fill_gradient2(
         low = "#00b140",
         mid = "white",
         high = "#ff4f4f",
         midpoint = 3,
         na.value = "white",
       ) + theme_minimal() +
       labs(title = "Cuadro comparativo de tendencias y cambios porcentuales (por mes)",
            subtitle = paste0("Desde ",  format(as_date(fecha_comienzo), "%B de %Y"), " a ",
                              format(max(data$mes), "%B de %Y")),
            x="", y="",
            caption = paste0("La tendencia de largo plazo va desde ", 
                             format(as_date(fecha_comienzo), "%B de %Y"), " al mes actual", "\n",
                             "La tendencia de corto plazo se utilizan los últimos 6 meses", "\n",
                             "El cambio porcentual es respecto al mes anterior")) +
       theme(legend.position = "none")
     

     
   }  
  else if(periodo=="semana"){
     ###Versión de semana
     
     data <- base %>%
       filter(fecha_inicio>= as_date(fecha_comienzo),
              fecha_inicio<=floor_date(as_date(fecha_lim), "week", week_start = 6)-1) %>%
       mutate(existe = 1) %>%
       complete(fecha_inicio = seq.Date(from = min(fecha_inicio),
                                        to = max(fecha_inicio),
                                        by = "day"),
                delito_sesnsp,
                fill = list(existe = 0)) %>%
       group_by(
         # semana = isoweek(fecha_inicio),
         semana = floor_date(fecha_inicio, "week", week_start = 6),
         delito_sesnsp
         # ao = year(fecha_inicio)
       ) %>%
       summarise(Total=sum(existe)) %>%
       filter(semana<=floor_date(max(base$fecha_inicio), "week", week_start = 6)) %>%
       # complete(mes=seq.Date(min(mes), max(mes), by="month"),
       #          delito_sesnsp,
       #          fill=list(Total=0)) %>%
       group_by(delito_sesnsp) %>%
       mutate(trend_l=(Total[semana==max(semana)]-Total[1])/n(),
              trend_m=(Total[semana==max(semana)]-Total[n()-26])/26,
              trend_c=Total-lag(Total)) %>%
       mutate(espacio=NA,
              cambio_l=abs(trend_m)/abs(trend_l)-1,
              cambio_m=abs(trend_c)/abs(trend_m)-1) %>%
       filter(semana==max(semana))
     
     data$cambio_l[is.nan(data$cambio_l)] <- 0
     data$cambio_l[is.infinite(data$cambio_l)] <- 0
     data$cambio_m[is.nan(data$cambio_m)] <- 0
     data$cambio_m[is.infinite(data$cambio_m)] <- 0
     
     comparacion_1 <- data %>% slice_tail(n=1) %>%
       pivot_longer(c(trend_l, trend_m, trend_c, espacio, cambio_l, cambio_m)) %>%
       mutate(label=case_when(
         name=="trend_l" &  value<0 ~ paste0("Negativa", " ", round(value, 2)),
         name=="trend_l" &  value>0 ~ paste0("Positiva", " ", round(value, 2)),
         name=="trend_l" &  value==0 ~ paste0("Sin pendiente", " ", round(value, 2)),
         name=="trend_m" &  value<0 ~ paste0("Negativa", " ", round(value, 2)) ,
         name=="trend_m" &  value>0 ~ paste0("Positiva", " ", round(value, 2)) ,
         name=="trend_m" &  value==0 ~ paste0("Sin pendiente", " ", round(value, 2)),
         name=="trend_c" &  value<0 ~ paste0("Negativa", " ", round(value, 2)),
         name=="trend_c" &  value>0 ~ paste0("Positiva", " ", round(value, 2)),
         name=="trend_c" &  value==0 ~ paste0("Sin pendiente", " ", round(value, 2)),
         name=="trend_c" &  value<0 ~ paste0("Negativa", " ", round(value, 2)),
         name=="trend_c" &  value>0 ~ paste0("Positiva", " ", round(value, 2)),
         name=="trend_c" &  value==0 ~ paste0("Sin pendiente", " ", round(value, 2)),
         name=="cambio_l" ~ paste0(round(value, 2), "%"),
         name=="cambio_m" ~ paste0(round(value, 2), "%"),
         
         TRUE ~ " "
       ),
       valor=case_when(
         name=="trend_l" & value>0 ~5, name=="trend_l" & value<=0 ~3,
         name=="trend_m" & value>0 ~5, name=="trend_m" & value<=0 ~3,
         name=="trend_c" & value>0 ~5, name=="trend_c" & value<=0 ~3,
         name=="cambio_l" & value==0 ~3, name=="cambio_l" & value>0 & value<=5~4,
         name=="cambio_l" & value>5~5, name=="cambio_l" & value<0 & value>= -.20~2,
         name=="cambio_l" & value< -.20~1,
         name=="cambio_m" & value==0 ~3, name=="cambio_m" & value>0 & value<=5~4,
         name=="cambio_m" & value>5~5, name=="cambio_m" & value<0 & value>=.20~2,
         name=="cambio_m" & value<.20~1, TRUE~ 3
       ),
       name=factor(name, levels = c("trend_l", "trend_m", "trend_c", "espacio",
                                    "cambio_l", "cambio_m"),
                   labels = c("Tendencia\n (largo plazo)",
                              "Tendencia\n (mediano plazo)",
                              "Tendencia\n (corto plazo)",
                              " ",
                              "Crecimiento % de mediano\n respecto largo plazo",
                              "Crecimiento % de corto\n respecto mediano plazo")),
       delito=str_to_sentence(delito_sesnsp)) %>%
       ggplot(aes(x=name, y=delito, fill=valor)) +
       geom_tile(colour=alpha("gray", alpha = .45), lwd =.05) +
       geom_text(aes(label=label)) +
       scale_fill_gradient2(
         low = "#00b140",
         mid = "white",
         high = "#ff4f4f",
         midpoint = 3,
         na.value = "white",
       ) + theme_minimal() +
       labs(title = "Cuadro comparativo de tendencias y cambios porcentuales de las tendencias (por semana)",
            subtitle = paste0("Desde ", format(as_date(fecha_comienzo), "%d de %B de %Y"), " a ",
                              format(max(data$semana)+7, "%d de %B de %Y")),
            x="", y="",
            caption = paste0("La tendencia de largo plazo va desde ",  
                             format(as_date(fecha_comienzo),  "%B de %Y"),
                             " a la última semana", "\n",
                             "La tendencia de corto plazo se utilizan los últimos 6 meses", "\n")) +
       theme(legend.position = "none")
  }
  else if(periodo=="anual"){
    data <- base %>% filter(fecha_inicio>= as_date(fecha_comienzo)) %>%
      group_by(mes=as_date(paste0(year(fecha_inicio), "-",
                                  month(fecha_inicio), "-01")),
               delito_sesnsp) %>%
      summarise(Total=n(), .groups = "drop") %>%
      filter(month()) %>%
      complete(mes=seq.Date(min(mes), max(mes), by="month"),
               delito_sesnsp,
               fill=list(Total=0)) %>%
      group_by(delito_sesnsp) %>%
      mutate(trend_l=(Total[mes==max(mes)]-Total[1])/n(),
             trend_m=(Total[mes==max(mes)]-Total[n()-5])/6,
             trend_c=Total-lag(Total)) %>%
      mutate(espacio=NA,
             cambio_l=abs(trend_m)/abs(trend_l)-1,
             cambio_m=abs(trend_c)/abs(trend_m)-1) %>%
      filter(mes==max(mes))
    
    data$cambio_l[is.nan(data$cambio_l)] <- 0
    data$cambio_l[is.infinite(data$cambio_l)] <- 0
    data$cambio_m[is.nan(data$cambio_m)] <- 0
    data$cambio_m[is.infinite(data$cambio_m)] <- 0
    
    comparacion_1 <- data %>% slice_tail(n=1) %>%
      pivot_longer(c(trend_l, trend_m, trend_c, espacio, cambio_l, cambio_m)) %>%
      mutate(label=case_when(
        name=="trend_l" &  value<0 ~ paste0("Negativa", " ", round(value, 2)),
        name=="trend_l" &  value>0 ~ paste0("Positiva", " ", round(value, 2)),
        name=="trend_l" &  value==0 ~ paste0("Sin pendiente", " ", round(value, 2)),
        name=="trend_m" &  value<0 ~ paste0("Negativa", " ", round(value, 2)) ,
        name=="trend_m" &  value>0 ~ paste0("Positiva", " ", round(value, 2)) ,
        name=="trend_m" &  value==0 ~ paste0("Sin pendiente", " ", round(value, 2)),
        name=="trend_c" &  value<0 ~ paste0("Negativa", " ", round(value, 2)),
        name=="trend_c" &  value>0 ~ paste0("Positiva", " ", round(value, 2)),
        name=="trend_c" &  value==0 ~ paste0("Sin pendiente", " ", round(value, 2)),
        name=="trend_c" &  value<0 ~ paste0("Negativa", " ", round(value, 2)),
        name=="trend_c" &  value>0 ~ paste0("Positiva", " ", round(value, 2)),
        name=="trend_c" &  value==0 ~ paste0("Sin pendiente", " ", round(value, 2)),
        name=="cambio_l" ~ paste0(round(value, 2)),
        name=="cambio_m" ~ paste0(round(value, 2)),
        
        TRUE ~ " "
      ),
      valor=case_when(
        name=="trend_l" & value>0 ~5, name=="trend_l" & value<=0 ~3,
        name=="trend_m" & value>0 ~5, name=="trend_m" & value<=0 ~3,
        name=="trend_c" & value>0 ~5, name=="trend_c" & value<=0 ~3,
        name=="cambio_l" & value==0 ~3, name=="cambio_l" & value>0 & value<=5~4,
        name=="cambio_l" & value>5~5, name=="cambio_l" & value<0 & value>= -.20~2,
        name=="cambio_l" & value< -.20~1,
        name=="cambio_m" & value==1 ~3, name=="cambio_m" & value>1 & value<=5~4,
        name=="cambio_m" & value>5~5, name=="cambio_m" & value<1 & value>=.20~2,
        name=="cambio_m" & value<.20~1, TRUE~ 3
      ),
      name=factor(name, levels = c("trend_l", "trend_m", "trend_c", "espacio",
                                   "cambio_l", "cambio_m"),
                  labels = c("Tendencia\n (largo plazo)",
                             "Tendencia\n (mediano plazo)",
                             "Tendencia\n (corto plazo)",
                             " ",
                             "Veces más grande a\n mediano plazo",
                             "Crecimiento a\n corto plazo")),
      delito=str_to_sentence(delito_sesnsp)) %>%
      ggplot(aes(x=name, y=delito, fill=valor)) +
      geom_tile() +
      geom_text(aes(label=label)) +
      scale_fill_gradient2(
        low = "#00b140",
        mid = "white",
        high = "#ff4f4f",
        midpoint = 3,
        na.value = "white",
      ) + theme_minimal() +
      labs(title = "Cuadro comparativo de tendencias y cambios porcentuales (por mes)",
           subtitle = paste0("Desde enero 2019 a ",
                             format(max(data$mes), "%B de %Y")),
           x="", y="",
           caption = paste0("La tendencia de largo plazo va desde enero 2019 al mes actual", "\n",
                            "La tendencia de corto plazo se utilizan los últimos 6 meses", "\n",
                            "El cambio porcentual es respecto al mes anterior")) +
      theme(legend.position = "none")
  }
  
  
  return(comparacion_1)
}



comparacion_grupo_etarios <- function(
  
  base, 
  fecha_comienzo=fecha_comienzo, 
  fecha_lim=fecha_lim, 
  nombre=nombre
) {
  library(ggrepel)
  base %>% 
    group_by(tipo_espacio, relacion, grupo_etario) %>% 
    summarise(Total=n(), .groups = "drop") %>% group_by(tipo_espacio, relacion) %>% 
    arrange(desc(grupo_etario)) %>% 
    mutate(position=cumsum(Total)*1.1, 
           porcentaje=Total/sum(Total)*100, 
           porcentaje_label=cumsum(porcentaje), 
           colorcito=ifelse(grupo_etario %in% "Adolescentes", "ghostwhite", "darkblue")) %>%  
    ggplot(aes(x=relacion, y=porcentaje, group=grupo_etario, fill=grupo_etario)) +
    geom_bar(stat = "identity") +
    geom_label(aes(y=ifelse(grupo_etario %in% "Adolescentes", porcentaje_label+1,
                            porcentaje_label*.95), label=paste0(comma(porcentaje, accuracy = 1), "%"), 
                   colour= colorcito
                   )
               ) +
    scale_fill_manual(values = colores) +
    scale_color_manual(values = c("black", "white")) +
    facet_wrap(.~tipo_espacio, scales = "free") +
    theme_light() +
    labs(title = str_wrap(paste0("Incidencia delictiva por año - ", nombre),65),
         subtitle = paste0("Del ", format(as_date(fecha_comienzo), "%d de %B de %Y"), " al ",
                           format(as_date(fecha_lim), "%d de %B de %Y")),
      x="Relación", y="Total", fill="Grupo etario") +
    theme(legend.position = "bottom", 
          axis.text.x = element_text(size=7),
  plot.title = element_text(face = "bold")
  ) + guides(colour="none")
  

}

####Para realizar gráfica perfil de víctima

graph_perfil_vic <- function(
  base_victimas=base, 
  nombre_plots=nombre, 
  fecha_inicio=fecha_comienzo, 
  fecha_lim = fecha_lim
) {
  library(ggrepel)
  
  
  graph_perfil_vic <- base_victimas %>% #filter(grepl(delito_sel, modalidad_delito)) %>%
    filter(fecha_inicio>=fecha_inicio) %>% 
    drop_na(edad) %>% filter(sexo %in% c("Femenino", "Masculino")) %>% 
    mutate(gr_edad=case_when(
      #edad<12 ~ "0 a 11",
      edad<18~ "0 a 17", edad>=18 & edad<30~ "18 a 29",
      edad>=30 & edad<45~ "30 a 44", edad>=45 & edad<60~ "45 a 59",
      edad>=60 ~ "+60"),
      gr_edad=factor(gr_edad,
                     levels = c("0 a 17", "18 a 29", "30 a 44",
                                "45 a 59", "+60"))) %>%
    group_by(year(fecha_inicio), gr_edad, sexo ) %>%
    summarise(Total=n(), .groups = "drop") %>%
    drop_na(sexo) %>% group_by(`year(fecha_inicio)`, gr_edad) %>%
    mutate(porcentaje=Total/sum(Total)*100) %>% arrange(desc(sexo)) %>%
    mutate(Total_label=ifelse(sexo=="Masculino", cumsum(Total)*.25,
                              cumsum(Total)*1.15),
           porcentaje_label=ifelse(sexo=="Masculino", cumsum(porcentaje)*.25,
                                   cumsum(porcentaje)*.95),
           colorcito=ifelse(sexo=="Femenino", "yep", "no")) %>%
    ggplot(aes(x=gr_edad, y=Total, fill=sexo)) +
    facet_wrap(.~`year(fecha_inicio)`, scales = "free") +
    geom_bar(stat = "identity") +
    geom_label_repel(#data=. %>% filter(porcentaje>1),
      aes(y=ifelse(sexo=="Masculino", Total_label*.15, Total_label*1.1), 
          label=paste0(comma(Total, accuracy = 1)),
          colour=colorcito), max.time = 8, direction = "y",
      size=5, force_pull = 3, show.legend = FALSE
      #fill="white"
    ) +
    scale_fill_manual(values = colores) +
    scale_colour_manual(values=c("black", "ghostwhite")) +
    theme_light()+
    labs(x="Grupos de edad", y="Total", fill="Sexo",
         subtitle = paste0("Corte al ", format(as_date(fecha_lim), "%d de %B de %Y")),
         title =paste0("Total de víctimas por - ", str_to_lower(nombre_plots))) +
    theme(plot.title = element_text(face = "bold"),
          legend.position = "bottom",
          strip.text.x = element_text(size = 12, face = "bold", colour = "black"),
          axis.title.x = element_text(face = "bold"),
          axis.title.y = element_text(face = "bold")) +
    guides(colour="none") 
  
  return(graph_perfil_vic)
}


####Para realizar tablas de las ct con mayor y menor incremento del promedio diario anual
tabla_ct_cambios <- function(
  data=base, 
  fecha_inicio=fecha_comienzo, 
  fecha_lim=fecha_lim
) {
  std_border = fp_border(color="gray", width = 1)
  
  tabla_crecimiento <- base %>% mutate(Total=1) %>% 
    complete(fecha_inicio=seq.Date(min(base$fecha_inicio), max(base$fecha_inicio), 
                                   by="1 day"),
             ct_inicio_ap=unique(base$ct_inicio_ap),
             fill=list(Total=0)) %>% 
    group_by(fecha_inicio, ct_inicio_ap) %>% summarise(Total=sum(Total), .groups = "drop") %>%
    group_by(año=year(fecha_inicio), ct_inicio_ap) %>% summarise(media=round(mean(Total), 2)) %>% 
    filter(año %in% c(year(min(base$fecha_inicio)),year(max(base$fecha_inicio)))) %>% 
    spread(año, media) %>% 
    `colnames<-`(c("delegacion_hechos", "año_pasado", "año_actual", "Alcaldía")) %>% 
    mutate(cambio=(año_actual/año_pasado-1)*100, 
           cambio=ifelse(is.infinite(cambio), 0, cambio)) %>% arrange(desc(cambio))
  
  top_mas <-  tabla_crecimiento %>% head(10) %>% 
    `colnames<-`(c("Coordinación territorial", paste0("Promedio\ndiario ", year(min(base$fecha_inicio))), 
                   paste0("Promedio\ndiario ", year(max(base$fecha_inicio))), 
                   "Cambio porcentual")) %>% 
    mutate("Cambio porcentual"=paste0(comma(`Cambio porcentual`,2), "%")) %>% 
    flextable() %>% border(border =  std_border) %>% 
    bg(i= 1, bg="#003763", part = "header") %>%
    color(i= 1, color="ghostwhite", part = "header") %>% 
    bold(i= 1, bold=T, part = "header") %>% 
    align(align = "center", part = "all") %>% 
    width(j=1, width=.5) %>% 
    bold(j=1, bold = T) %>% 
    #autofit() %>%
    as_raster()
  
  
  tabla_mayor <- ggplot() + 
    theme_void() + 
    annotation_custom(rasterGrob(top_mas)#, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf
    )
  
  top_menos <-  tabla_crecimiento %>% filter(!is.nan(cambio)) %>% 
    tail(10) %>% 
    `colnames<-`(c("Coordinación territorial", paste0("Promedio\ndiario ", year(min(base$fecha_inicio))), 
                   paste0("Promedio\ndiario ", year(max(base$fecha_inicio))), 
                   "Cambio porcentual")) %>% 
    mutate("Cambio porcentual"=paste0(comma(`Cambio porcentual`,2), "%")) %>% 
    flextable() %>% border(border =  std_border) %>% 
    bg(i= 1, bg="#003763", part = "header") %>%
    color(i= 1, color="ghostwhite", part = "header") %>% 
    bold(i= 1, bold=T, part = "header") %>% 
    align(align = "center", part = "all") %>% 
    width(j=1, width=.5) %>% 
    bold(j=1, bold = T) %>% 
    #autofit() %>%
    as_raster()
  
  
  tabla_menor <- ggplot() + 
    theme_void() + 
    annotation_custom(rasterGrob(top_menos)#, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf
    )
  
  final <- plot_grid(tabla_mayor, NULL ,tabla_menor, nrow = 1, 
                     rel_widths = c(10, 2, 10))
  
  return(graph_perfil_vic)
}



# graph_grupo_social <- base_victimas %>% filter(grepl(delito_sel, modalidad_delito)) %>% 
#   group_by(grupo_social) %>% summarise(Total=n()) %>% 
#   drop_na(grupo_social) %>% arrange(desc(Total)) %>% 
#   filter(!grupo_social %in% c("NO APLICA", "NO SE ESPECIFICA")) %>% 
#   ggplot(aes(x=reorder(grupo_social, -Total), y=Total)) +
#   geom_bar(stat = "identity", fill=colores_fgj[6]) +
#   theme(plot.title = element_text(face = "bold"), 
#         legend.position = "none",
#         axis.title.x = element_text(face = "bold"), 
#         axis.title.y = element_text(face = "bold")) +
#   geom_label(aes(label=comma(Total, accuracy = 1))) +
#   labs(x="Grupos social", y="Víctimas", fill="Sexo", 
#        subtitle = paste0("Corte al ", format(max(base_victimas$fecha_inicio), "%d de %B de %Y")), 
#        title =paste0("Total de víctimas por grupo social - ", str_to_lower(delito))) +
#   theme_light() +
#   coord_flip()

# gr_trend <-  base %>% filter(year(fecha_inicio)>2020,
#                 tipo_impacto=="ALTO IMPACTO") %>%
#   group_by(#mes=as_date(paste0(year(fecha_inicio), "-",
#             #                  month(fecha_inicio), "-01")),
#     fecha_inicio,
#            delito_sesnsp) %>%
#   summarise(Total=n(), .groups = "drop") %>%
#   #filter(mes<=floor_date(max(base$fecha_inicio), "month")-1) %>%
#   complete(#mes=seq.Date(min(mes), max(mes), by="month"),
#     fecha_inicio=seq.Date(as_date("2021-01-01"), max(base$fecha_inicio), "day"),
#     delito_sesnsp,
#            fill=list(Total=0)) %>%
#   mutate(delito_sesnsp=str_to_sentence(delito_sesnsp)) %>% 
#   #filter(delito_sesnsp %in% c("FEMINICIDIO")) %>%
#   ggplot(aes(fecha_inicio, Total)) + geom_point(alpha=.1) +
#   facet_wrap(.~delito_sesnsp, scales = "free") +
#   geom_smooth(colour=colores[1], fill=colores[1]) +
#   geom_smooth(data=. %>% filter(year(fecha_inicio)==2022), colour=colores[2], fill=colores[2]) +
#   theme_cowplot()

library(forecast)

descomponer_serie <- function(
  delito_secretariado=del, 
  fecha_comienzo=fecha_comienzo,
  fecha_lim=fecha_lim
){
  

  
data <- base %>% filter(year(fecha_inicio)>2020, delito_homologado %in% delito_secretariado,
                      tipo_impacto=="ALTO IMPACTO") %>%
  group_by(#mes=as_date(paste0(year(fecha_inicio), "-",
    #                  month(fecha_inicio), "-01")),
    fecha_inicio,
    delito_homologado) %>%
  summarise(Total=n(), .groups = "drop") %>%
  #filter(mes<=floor_date(max(base$fecha_inicio), "month")-1) %>%
  complete(#mes=seq.Date(min(mes), max(mes), by="month"),
    fecha_inicio=seq.Date(as_date(fecha_comienzo), as_date(fecha_lim), "day"),
    delito_homologado,
    fill=list(Total=0)) %>% select(fecha_inicio, Total)




prueba <- ts(data$Total, frequency = 7#, start = c(year(as_date(fecha_comienzo)), 1)
             )

#prueba <- ts(data$Total,  frequency = 7)

gr <- stl(prueba,"per") %>% autoplot() + labs(title = str_to_sentence(delito_secretariado), 
                                              x="semana") + 
  theme_light() 

return(gr)
 
}

library(strucchange)
serie_cambio_str <- function(
  delito_secretariado=del, 
  fecha_comienzo=fecha_comienzo,
  fecha_lim=fecha_lim
){
  
  
  
  data <- base %>% filter(year(fecha_inicio)>2020, delito_homologado %in% delito_secretariado,
                          tipo_impacto=="ALTO IMPACTO") %>%
    group_by(#mes=as_date(paste0(year(fecha_inicio), "-",
      #                  month(fecha_inicio), "-01")),
      fecha_inicio,
      delito_homologado) %>%
    summarise(Total=n(), .groups = "drop") %>%
    #filter(mes<=floor_date(max(base$fecha_inicio), "month")-1) %>%
    complete(#mes=seq.Date(min(mes), max(mes), by="month"),
      fecha_inicio=seq.Date(as_date(fecha_comienzo), as_date(fecha_lim), "day"),
      delito_homologado,
      fill=list(Total=0)) %>% select(fecha_inicio, Total)
  
  
  prueba <- ts(data$Total,  frequency = 7)
  
  cambio <- breakpoints(prueba ~1)
  

  

  
  return(cambio)
  
  
}


serie_tiempo <- function(
  delito_secretariado=del, 
  fecha_comienzo=fecha_comienzo,
  fecha_lim=fecha_lim
){
  
  
  
  data <- base %>% filter(year(fecha_inicio)>2020, delito_homologado %in% delito_secretariado,
                          tipo_impacto=="ALTO IMPACTO") %>%
    group_by(#mes=as_date(paste0(year(fecha_inicio), "-",
      #                  month(fecha_inicio), "-01")),
      fecha_inicio,
      delito_homologado) %>%
    summarise(Total=n(), .groups = "drop") %>%
    #filter(mes<=floor_date(max(base$fecha_inicio), "month")-1) %>%
    complete(#mes=seq.Date(min(mes), max(mes), by="month"),
      fecha_inicio=seq.Date(as_date(fecha_comienzo), as_date(fecha_lim), "day"),
      delito_homologado,
      fill=list(Total=0)) %>% select(fecha_inicio, Total)
  
  
  prueba <- ts(data$Total,  frequency = 7)
  
  
  
  
  
  
  return(prueba)
  
  
}


gr_heat_agencia <- function(
  data=base, 
  delito=nom_delito, 
  fecha_lim=fecha_lim
){
  data_agencia <- data %>%
    get_fiscalia() %>% 
    rename(fiscalia=fiscalia_ini) %>% 
    mutate(nom_fis=case_when(
      grepl("AMBIENTALES|FEDAPUR|Protección Urbana",fiscalia) ~ "FIDAMPU", grepl("REVISI|Ministerio",fiscalia) ~ "CAMPAP", 
      grepl("FIZP|FIIZP|Iztapalapa",fiscalia) ~ "FIT Iztapalapa", grepl("FTL|Tlalpan|FITLP",fiscalia) ~ "FIT Tlalpan",
      grepl("FIBJ|FBJ|Benito",fiscalia) ~ "FIT Benito Juárez", grepl("FAO|FIAO|Álvaro",fiscalia) ~ "FIT Álvaro Obregón",
      grepl("FAAE|Atención Prioritaria", fiscalia) ~ "Grupos de Atención Prioritaria", 
      grepl("FCI|Central", fiscalia) ~ "Investigación estrégica Central", grepl("FCH|FICUH|Cuauhtémoc", fiscalia) ~ "FIT Cuauhtémoc",
      grepl("FJF|FIDVF|Familiar", fiscalia) ~ "Violencia familiar", grepl("FCY|FICOY|Coyoacán",fiscalia) ~ "FIT Coyoacán",
      grepl("FAS|Fuerza Anti Secuestro", fiscalia) ~ "Fuerza Anti Secuestro", 
      grepl("FIMH|Miguel Hidalgo",fiscalia) ~ "FIT Miguel Hidalgo", grepl("FITLH|FTH|Tláhuac",fiscalia) ~ "FIT Tláhuac",
      grepl("FIGAM|FGAM|Madero",fiscalia) ~ "FIT Gustavo A. Madero", grepl("FXH|FIXOC|Xochimilco",fiscalia) ~ "FIT Xochimilco",
      grepl("FMC|FIMC|Magdalena",fiscalia) ~ "FIT Magdalena Contreras", grepl("FIIZC|FIZC|Iztacalco",fiscalia) ~ "FIT Iztacalco",
      grepl("FDF|Financieros",fiscalia) ~ "DELITOS FINANCIEROS", grepl("FAZ|FIAZ|Azcapotzalco",fiscalia) ~ "FIT Azcapotzalco",
      grepl("FCJ|FICJ|Cuajimalpa",fiscalia) ~ "FIT Cuajimalpa", grepl("FSP|Servidores|FIDCSP",fiscalia) ~ "Servidores Públicos", 
      grepl("FPC|Civiles", fiscalia) ~ "Procesos En Juzgados Civiles", grepl("FNNA|Niñas, Niños", fiscalia) ~ "Niñas, Niños y Adolescentes", 
      grepl("FVC|FIVC|Venustiano",fiscalia) ~ "FIT Venustiano Carranza", grepl("FIEAE|FAE|Asuntos Especiales", fiscalia) ~ "Asuntos especiales",
      grepl("FIDAGAP|Grupos de Atención Prioritaria", fiscalia) ~ "Grupos de Atención Prioritaria",
      grepl("FMP|FIMP|Milpa",fiscalia) ~ "FIT Milpa Alta", 
      grepl("FIERVT|Transporte", fiscalia) ~ "Robo de vehículo", grepl("ADD|URI|Denuncia Digital", fiscalia) ~ "Denuncia digital",
      TRUE ~ str_to_title(fiscalia)
    )) %>% filter(grepl("FIT", nom_fis)) %>% 
    mutate(num_agencia=case_when(
      grepl("1", agencia_ini) ~ 1, grepl("2", agencia_ini) ~ 2, grepl("3", agencia_ini) ~ 3,
      grepl("4", agencia_ini) ~ 4, grepl("5", agencia_ini) ~ 5, grepl("6", agencia_ini) ~ 6,
      grepl("7", agencia_ini) ~ 7, grepl("8", agencia_ini) ~ 8, grepl("9", agencia_ini) ~ 9,
      grepl("10", agencia_ini) ~ 10
    )) %>% 
    group_by(nom_fis, num_agencia) %>% summarise(Total=n()) %>% drop_na(Total) %>% 
    complete(num_agencia=seq(1:9), fill=list(Total=NA))
  
  gr_agencia <- data_agencia %>% 
    ggplot(aes(y=nom_fis, x=num_agencia, fill=Total)) + 
    geom_tile(color="black") +
    geom_text(aes(label=comma(Total, accuracy = 1))) +
    scale_y_discrete(limits=rev) +
    scale_x_continuous(breaks = seq(1,9, 1)) +
    theme_minimal() + 
    scale_fill_gradient2(
      low = "#FEFFC3",
      mid = "#E5A507",
      high = "#FD2B2B",
      midpoint = median(data_agencia$Total, na.rm = T),
      na.value = "gray", labels=comma
      # trans = "log1p"
    ) + 
    labs(x="Número de agencia", y="Fiscalía territorial", 
         title = paste0("Agencias de inicio - ", delito),
         subtitle = paste0("Corte al ", format(as_date(fecha_lim), "%d de %B de %Y"))) +
    theme(axis.text.x = element_text(size = 12), 
          legend.text = element_text(size = 12))
  

  
  gr_top_agencia <- data %>%
    get_fiscalia() %>% 
    mutate(fiscalia=fiscalia_ini) %>% 
    mutate(nom_fis=case_when(
      grepl("AMBIENTALES|FEDAPUR|Protección Urbana",fiscalia) ~ "FIDAMPU", grepl("REVISI|Ministerio",fiscalia) ~ "CAMPAP", 
      grepl("FIZP|FIIZP|Iztapalapa",fiscalia) ~ "FIT Iztapalapa", grepl("FTL|Tlalpan|FITLP",fiscalia) ~ "FIT Tlalpan",
      grepl("FIBJ|FBJ|Benito",fiscalia) ~ "FIT Benito Juárez", grepl("FAO|FIAO|Álvaro",fiscalia) ~ "FIT Álvaro Obregón",
      grepl("FAAE|Atención Prioritaria", fiscalia) ~ "Grupos de Atención Prioritaria", 
      grepl("FCI|Central", fiscalia) ~ "Investigación estrégica Central", grepl("FCH|FICUH|Cuauhtémoc", fiscalia) ~ "FIT Cuauhtémoc",
      grepl("FJF|FIDVF|Familiar", fiscalia) ~ "Violencia familiar", grepl("FCY|FICOY|Coyoacán",fiscalia) ~ "FIT Coyoacán",
      grepl("FAS|Fuerza Anti Secuestro", fiscalia) ~ "Fuerza Anti Secuestro", 
      grepl("FIMH|Miguel Hidalgo",fiscalia) ~ "FIT Miguel Hidalgo", grepl("FITLH|FTH|Tláhuac",fiscalia) ~ "FIT Tláhuac",
      grepl("FIGAM|FGAM|Madero",fiscalia) ~ "FIT Gustavo A. Madero", grepl("FXH|FIXOC|Xochimilco",fiscalia) ~ "FIT Xochimilco",
      grepl("FMC|FIMC|Magdalena",fiscalia) ~ "FIT Magdalena Contreras", grepl("FIIZC|FIZC|Iztacalco",fiscalia) ~ "FIT Iztacalco",
      grepl("FDF|Financieros",fiscalia) ~ "DELITOS FINANCIEROS", grepl("FAZ|FIAZ|Azcapotzalco",fiscalia) ~ "FIT Azcapotzalco",
      grepl("FCJ|FICJ|Cuajimalpa",fiscalia) ~ "FIT Cuajimalpa", grepl("FSP|Servidores|FIDCSP",fiscalia) ~ "Servidores Públicos", 
      grepl("FPC|Civiles", fiscalia) ~ "Procesos En Juzgados Civiles", grepl("FNNA|Niñas, Niños", fiscalia) ~ "Niñas, Niños y Adolescentes", 
      grepl("FVC|FIVC|Venustiano",fiscalia) ~ "FIT Venustiano Carranza", grepl("FIEAE|FAE|Asuntos Especiales", fiscalia) ~ "Asuntos especiales",
      grepl("FIDAGAP|Grupos de Atención Prioritaria", fiscalia) ~ "Grupos de Atención Prioritaria",
      grepl("FMP|FIMP|Milpa",fiscalia) ~ "FIT Milpa Alta", 
      grepl("FIERVT|Transporte", fiscalia) ~ "Robo de vehículo", grepl("ADD|URI|Denuncia Digital", fiscalia) ~ "Denuncia digital",
      TRUE ~ str_to_title(fiscalia)
    )) %>% filter(grepl("FIT", nom_fis)) %>% 
    group_by(agencia_ini) %>% summarise(Total=n()) %>% arrange(desc(Total)) %>% head(10) %>% 
    mutate(Total=comma(Total, accuracy = 1)) %>% rename(Agencia=agencia_ini) %>% 
    flextable() %>% border(border =  std_border) %>% 
    bg(i= 1, bg="#003763", part = "header") %>%
    color(i= 1, color="ghostwhite", part = "header") %>% 
    bold(i= 1, bold=T, part = "header") %>% 
    align(align = "center", part = "all") %>% 
    width(j=1, width=.5) %>% 
    bold(j=1, bold = T) %>% 
    #autofit() %>%
    as_raster()
  
  gtabla <- ggplot() + 
    theme_void() + 
    annotation_custom(rasterGrob(gr_top_agencia)#, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf
    )
  
  
  final <- plot_grid(gtabla, NULL, gr_agencia, ncol = 3, nrow = 1,
                     rel_widths = c(2, .5, 12))
  
  return(final)
}



library(treemapify)
library(grid)
library(officer)

gr_sub_tree <- function(
    base=base  
){
  std_border = fp_border(color="gray", width = 1)
  
  tr_cosas <- data %>% group_by(subtipo) %>% summarise(Total=n()) %>% 
    #ungroup() %>% group_by(cuenta) %>% 
    mutate(porcentaje=percent(Total/sum(Total), , accuracy=.1),
           etiqueta=paste0(subtipo, "\n", comma(Total, accuracy = 1), " (", porcentaje, ")")#, 
           # subtipo=factor(subtipo, 
           #                levels = ord_subtipo)
    ) %>% 
    ggplot(aes(area=Total, fill=subtipo)) + #facet_wrap(cuenta~., scales="free") +
    geom_treemap(alpha=.80) +
    geom_treemap_text(aes(label=etiqueta), color="ghostwhite", 
                      place = "center", grow = F) +
    scale_fill_manual(values = colores) + theme_light() +
    theme(legend.position = "bottom") +
    theme(strip.text = element_text(color="black", face = "bold", size = 16)) +
    labs(fill="")
  
  tabla_cosas <- base %>% group_by(subtipo) %>% summarise(Total=n()) %>% 
    arrange(desc(Total)) %>%
    #spread(cuenta, Total, fill=0) %>% 
    flextable()  %>% border(border =  std_border) %>% 
    bg(i= 1, bg="#003763", part = "header") %>%
    color(i= 1, color="ghostwhite", part = "header") %>% 
    bold(i= 1, bold=T, part = "header") %>% 
    align(align = "center", part = "all") %>% 
    width(j=1, width=.5) %>% 
    bold(j=1, bold = T) %>% 
    #autofit() %>%
    as_raster()
  
  gtabla <- ggplot() + 
    theme_void() + 
    annotation_custom(rasterGrob(tabla_cosas)#, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf
    )
  
  final <- plot_grid(tr_cosas, gtabla, nrow = 1, 
                     rel_widths = c(10, 3))
  return(final)
}

gr_sub_ts <- function(
    base=base, 
    fecha_comienzo=fecha_inicio_global, 
    fecha_lim=fecha_lim, 
    nombre=nombre_del
){
  
  ts_cosas <- base %>% # filter(cuenta=="Se dió cuenta\nen el momento") %>% 
    mutate(Total=1) %>% 
    complete(fecha_inicio=seq.Date(as_date(fecha_comienzo), as_date(fecha_lim),by="1 day"),
             subtipo=unique(data$subtipo), 
             #cuenta=unique(data$cuenta),
             fill=list(Total=0)) %>% 
    group_by(f=ymd(paste0(year(fecha_inicio), "-", month(fecha_inicio), "-01")),
             subtipo) %>% 
    summarise(Total=sum(Total)) %>% 
    #filter(!subtipo %in% c("Animales", "Joyería", "Ropa", "Pertencias")) %>% 
    ggplot(aes(x=f, y=Total)) + facet_wrap(.~subtipo, scales = "free") +
    geom_point(alpha=.5, colour=colores[1]) + 
    geom_smooth(se=F, color=colores[2]) +
    labs(x="Fecha", y="Carpetas de investigación", 
         title=  paste0("Carpetas de investigación por subtipo - ", nombre), 
         subtitle = paste0("Corte al ", format.Date(fecha_lim, "%d de %B de %Y"))#, 
         #caption = "Se eliminaron las categorias de: Animales, joyería, ropa y pertenencias\n porque tenían muy pocos casos para mostrar una tendencia"
    ) +
    theme_light() +
    theme(strip.text = element_text(color="black", face="bold"))
}

filtrar_ordenes <- function(
    tipo_delito=delitos, 
    modalidad = c("delito", "modalidad", "genero")
){
  if(modalidad=="delito"){
    ordenes_fil <- ordenes %>% filter(delito_hom_uet %in% tipo_delito)
  } else if(modalidad=="modalidad") {
    ordenes_fil <- ordenes %>% filter(modalidad_delito_estadistico_uet %in% tipo_delito)
  } else if(modalidad=="genero"){
    ordenes_fil <- ordenes %>% filter(delitos_violencia_genero %in% tipo_delito)
  }
  
  if(modalidad=="genero"){
    sentencias_fil <- sentencias %>% filter(delito_base_vg %in% tipo_delito)
  } else {
    sentencias_fil <- sentencias %>% filter(delito_hom_uet %in% tipo_delito)
  }
  
  triki <- list(ordenes=ordenes_fil, sentencias=sentencias_fil)
  
  
  return(triki)
}


resultado_flagrancias <- function(base=data, 
                                  traza=c(0,1), 
                                  delito_ord=b, 
                                  fecha_lim =fecha){
  
  fecha_lim <- as_date(fecha_lim)
  # dia <- weekdays(fecha_lim)
  # empieza <- case_when(
  #   dia=="lunes" ~ 1, dia=="martes" ~ 2, dia=="miércoles" ~ 3, dia=="jueves" ~ 4,
  #   dia=="viernes" ~ 5, dia=="sábado" ~ 6, dia=="domingo" ~ 7
  # )
  empieza <- wday(fecha_lim, week_start = 1)
  flagr <- flagrancias %>% filter(id_ap %in% base$id_ap) %>% 
    mutate(fecha_inicio=as_date(fecha_de_inicio),
           res_puestas=case_when(
             #motivo_de_libertad=="Artículo 140 CNPP (Libertad durante la investigación)" ~ "LIBERTAD MP",
             libertad==1 ~ "1. Libertad MP",
             observaciones_dgpec_2=="No califica de legal la detención (libertad)" ~ "2. Ilegal detención PJ",
             incompetencia==1 ~ "7. Incompetencia",
             prision_preventiva_justificada>0 | prision_preventiva_oficiosa>0 ~ "6. Prisión preventiva",
             suspension_condicional==1 ~ "5. Suspen. Condicional",
             vinculacion_a_proceso_por_persona==1 ~ "4. Vinculación simple", 
             vinculacion_a_proceso_por_persona==0 ~ "3. No vinculación",
             TRUE ~ "8. Pendiente de estatus"
             
           ), 
           fecha_sem=floor_date(fecha_inicio, "week", week_start = empieza)
    )
  
  base_hoy <- base %>% mutate(detenido=ifelse(id_ap%in% flagr$id_ap, "Con detenido", "Sin detenido")) %>% 
    filter(fecha_inicio<=fecha_lim, fecha_inicio>=fecha_lim-7)
  
  if(traza==0){
    ord <- ordenes %>% filter(delito_hom_uet %in% delito_ord) %>% 
      mutate(fecha_cumplida=as_date(fecha_cumplida), 
             fecha_sem=floor_date(fecha_cumplida, "week", week_start = empieza))
  } else{
    ord <- ordenes %>% mutate(id_ap=as.integer(id_carpeta_uet)) %>% 
      filter(id_ap %in% base_hoy$id_ap) %>% 
      mutate(fecha_cumplida=as_date(fecha_cumplida), 
             fecha_sem=floor_date(fecha_cumplida, "week", week_start = empieza))
  }
  
  
  
  base_past <- base %>% mutate(detenido=ifelse(id_ap%in% flagr$id_ap, "Con detenido", "Sin detenido")) %>% 
    filter(year(fecha_inicio)==year(Sys.Date())-1)
  
  flagr_hoy <- flagr %>% 
    filter(fecha_inicio<=fecha_lim, fecha_inicio>=fecha_lim-7)
  
  flagr_past <- flagr %>% 
    filter(year(fecha_inicio)==year(Sys.Date())-1)
  
  ord_hoy <- ord %>% filter(fecha_cumplida<=fecha_lim, fecha_cumplida>=fecha_lim-7)
  
  ord_past <- ord %>% filter(year(fecha_cumplida)==year(Sys.Date())-1)
  
  prom_detenido <- base_past %>% group_by(detenido, fecha_sem=floor_date(fecha_inicio, "week", week_start = empieza)) %>% 
    filter(fecha_sem>=floor_date(Sys.Date()-365, "year")) %>% 
    summarise(Total=n()) %>% group_by(fecha_sem) %>% 
    mutate(porcentaje=Total/sum(Total)) %>% group_by(detenido) %>%
    summarise(Promedio=mean(porcentaje, na.rm=T)) %>% filter(detenido=="Con detenido")
  
  if (base_hoy %>% filter(detenido=="Con detenido") %>% 
      nrow()>0) {
    gr_det <- base_hoy %>% 
      group_by(detenido) %>% summarise(Total=n()) %>% 
      mutate(porcentaje=Total/sum(Total)) %>% #arrange(desc(detenido)) %>% 
      # mutate(total_label=cumsum(porcentaje), 
      #        total_label=ifelse(detenido=="Con detenido", total_label*.5, total_label*.5)) %>% 
      ggplot(aes(x="", y=porcentaje, fill=detenido, group=detenido)) + 
      geom_col(position = position_stack(reverse = TRUE)) +
      # geom_label(aes(label=paste(percent(porcentaje), "\n", "(",
      #                            comma(Total,1), ")"), y=total_label), show.legend = F, 
      #            size=5) +
      geom_label(aes(label=paste(percent(porcentaje), "\n", "(",
                                 comma(Total,1), ")"), group=detenido, fill=detenido), show.legend = F, 
                 size=5, position = position_stack(reverse = T, vjust = .5)) +
      scale_y_continuous(labels = percent) + theme_void() +
      theme(legend.position = "bottom", 
            axis.title.y = element_text(color="black", face = "bold"), 
            plot.margin = unit(c(.5, 0, 0, 0), "cm")
      ) +
      scale_fill_manual(values = c(colores[5], "gray")) +
      geom_segment(data=prom_detenido, aes(y=Promedio, yend=Promedio, 
                                           colour="Promedio de año anterior"), 
                   size=2, linetype="dashed",  x=.55, xend=1.45) + 
      scale_color_manual(values = colores[3], labels=paste0("Promedio ", year(Sys.Date())-1)) +
      geom_label(data=prom_detenido, aes(y=Promedio+.02, label=percent(Promedio)),x=1.3, 
                 fill=colores[3], size=5) + 
      scale_color_manual(values = colores[3], labels=paste0("Promedio ", year(Sys.Date())-1)) +
      labs(fill="", color="", x=str_wrap("Carpetas con detenido", width = 15)) + 
      coord_flip()
  } else {
    
    gr_det <- base_hoy %>% 
      ggplot() +
      annotate(geom = "text", x=0, y=0, 
               label=paste0("No hay carpetas con detenido por este delito"), 
               size=10, colour="black") +
      theme_void() + theme(text = element_text(face = "bold"))
    
    
  }
  
  prom_judi <- flagr_past %>% 
    group_by(fecha_sem) %>% summarise(libertad=sum(libertad, na.rm = T), 
                                      judicializacion=sum(judicializacion, na.rm = T)) %>% 
    mutate(Total=libertad+judicializacion,
           porcentaje=judicializacion/Total) %>% ungroup() %>% 
    summarise(Promedio=mean(porcentaje, na.rm=T))
  
  if (sum(flagr_hoy$vinculacion_a_proceso_por_persona, na.rm = T)>0) {
    
    gr_judi <- flagr_hoy %>% summarise(libertad=sum(libertad, na.rm = T), 
                                       judicializacion=sum(judicializacion, na.rm = T)) %>% 
      gather(detenido, Total, libertad:judicializacion) %>% arrange(detenido) %>% 
      mutate(porcentaje=Total/sum(Total), 
             # total_label=cumsum(porcentaje), 
             # total_label=ifelse(detenido=="libertad", total_label*.9, total_label*.9), 
             detenido=ifelse(detenido=="libertad", "Libertad en MP", "Judicialización")) %>% 
      ggplot(aes(x="", y=porcentaje)) + 
      geom_col(position = position_stack(reverse = TRUE), 
               aes(fill=detenido, group=detenido)) +
      geom_segment(data=prom_judi, aes(y=Promedio, yend=Promedio,
                                       colour="Promedio de año anterior"),
                   size=2, linetype="dashed",  x=.55, xend=1.45) +
      geom_label(data=. %>% filter(porcentaje>0),
                 aes(label=paste(percent(porcentaje), "\n", "(",
                                 comma(Total,1), ")"), group=detenido, fill=detenido), show.legend = F, 
                 size=5, position = position_stack(reverse = T, vjust = .5)) +
      scale_y_continuous(labels = percent) + theme_void() +
      theme(legend.position = "bottom",
            axis.title.y = element_text(color="black", face = "bold"), 
            plot.margin = unit(c(.5, 0, 0, 0), "cm")
      ) +
      scale_fill_manual(values = c(colores[5], "gray")) +
      geom_label(data=prom_judi, aes(y=Promedio+.02, label=percent(Promedio)),x=1.3, 
                 fill=colores[3], size=5) + 
      scale_color_manual(values = colores[3], labels=paste0("Promedio ", year(Sys.Date())-1)) +
      labs(fill="", color="", x=str_wrap("% de judicializadas", width = 15)) + 
      coord_flip()
    
  } else {
    
    gr_judi <- base_hoy %>% 
      ggplot() +
      annotate(geom = "text", x=0, y=0, 
               label=paste0("No hay personas judicializadas por este delito"), 
               size=10, colour="black") +
      theme_void() + theme(text = element_text(face = "bold"))
  }
  
  prom_vinc <- flagr_past %>% 
    filter(judicializacion==1, 
           res_puestas %in% c("4. Vinculación simple","6. Prisión preventiva",  
                              "2. Ilegal detención PJ", "3. No vinculación")) %>%
    mutate(detenido=case_when(
      res_puestas %in% c("4. Vinculación simple","6. Prisión preventiva") ~ "Vinculación a proceso", 
      res_puestas %in% "2. Ilegal detención PJ" ~ "Ilegal detención", 
      T ~ "No vinculación"
    )) %>% group_by(detenido, fecha_sem) %>% 
    summarise(Total=n()) %>% ungroup() %>%  group_by(fecha_sem) %>% 
    mutate(porcentaje=Total/sum(Total)) %>% ungroup() %>% 
    filter(detenido=="Vinculación a proceso") %>% 
    summarise(Promedio=mean(porcentaje, na.rm=T))
  
  
  if(sum(flagr_hoy$vinculacion_a_proceso_por_persona, na.rm = T)>0) {
    gr_vinc <- flagr_hoy %>% 
      filter(judicializacion==1, 
             res_puestas %in% c("4. Vinculación simple","6. Prisión preventiva",  
                                "2. Ilegal detención PJ", "3. No vinculación")) %>%
      mutate(detenido=case_when(
        res_puestas %in% c("4. Vinculación simple","6. Prisión preventiva") ~ "Vinculación a proceso", 
        res_puestas %in% "2. Ilegal detención PJ" ~ "Ilegal detención", 
        T ~ "No vinculación"
      ), 
      detenido=factor(detenido, levels = c("Vinculación a proceso",
                                           "No vinculación", "Ilegal detención")
      )) %>% group_by(detenido) %>% 
      summarise(Total=n()) %>% 
      mutate(porcentaje=Total/sum(Total), 
             total_label=cumsum(porcentaje)*.9) %>% 
      ggplot(aes(x="", y=porcentaje)) + 
      geom_col(position = position_stack(reverse = TRUE), 
               aes(fill=detenido, group=detenido)) +
      geom_segment(data=prom_vinc, aes(y=Promedio, yend=Promedio,
                                       colour="Promedio de año anterior"),
                   size=2, linetype="dashed",  x=.55, xend=1.45) +
      # geom_label(data=. %>% filter(porcentaje>0),
      #            aes(label=paste(percent(porcentaje), "\n", "(",
      #                            comma(Total,1), ")"), y=total_label, fill=detenido), show.legend = F, 
      #            size=5) +
      geom_label(data=. %>% filter(porcentaje>0),
                 aes(label=paste(percent(porcentaje), "\n", "(",
                                 comma(Total,1), ")"), group=detenido, fill=detenido), show.legend = F,
                 size=5, position = position_stack(vjust = .5, reverse = T)) +
      scale_y_continuous(labels = percent) + theme_void() +
      theme(legend.position = "bottom",
            axis.title.y = element_text(color="black", face = "bold"), 
            plot.margin = unit(c(.5, 0, 0, 0), "cm")
      ) +
      scale_fill_manual(values = c(colores[5], "gray", colores[7])) +
      geom_label(data=prom_vinc, aes(y=Promedio+.02, label=percent(Promedio)),x=1.3, 
                 fill=colores[3], size=5) + 
      scale_color_manual(values = colores[3], labels=paste0("Promedio ", year(Sys.Date())-1)) +
      labs(fill="", color="", x=str_wrap("% de vinculación", width = 15)) + 
      coord_flip()
    
  } else{
    
    gr_vinc <- base_hoy %>% 
      ggplot() +
      annotate(geom = "text", x=0, y=0, 
               label=paste0("No hay personas vinculadas por flagrancias en este delito"), 
               size=10, colour="black") +
      theme_void() + theme(text = element_text(face = "bold"))
  }
  
  prom_pris <- flagr_past %>% 
    filter(vinculacion_a_proceso_por_persona>0) %>%
    group_by(fecha_sem) %>% 
    summarise(prision_preventiva=sum(prision_preventiva_oficiosa, na.rm = T) +
                sum(prision_preventiva_justificada, na.rm = T), 
              #prision_preventiva_justificada=sum(prision_preventiva_justificada, na.rm = T), 
              otras_medidas=sum(otras_medidas_cautelares, na.rm = T) +
                sum(sin_medida_cautelar, na.rm = T)) %>% 
    gather(detenido, Total, prision_preventiva:otras_medidas) %>%  
    group_by(fecha_sem) %>% 
    mutate(porcentaje=Total/sum(Total)) %>% ungroup() %>% 
    filter(detenido=="prision_preventiva") %>% 
    summarise(Promedio=mean(porcentaje, na.rm=T))
  
  if (flagr_hoy %>% 
      filter(prision_preventiva_oficiosa>0 | 
             prision_preventiva_justificada>0) %>% 
      nrow()>0) {
    
    pal_pri <- c("Prisión preventiva"=colores[5], 
                 "Otras medidas"= "gray")
    gr_pris <- flagr_hoy %>% 
      filter(vinculacion_a_proceso_por_persona>0) %>%
      summarise(prision_preventiva=sum(prision_preventiva_oficiosa, na.rm = T) +
                  sum(prision_preventiva_justificada, na.rm = T), 
                #prision_preventiva_justificada=sum(prision_preventiva_justificada, na.rm = T), 
                otras_medidas=sum(otras_medidas_cautelares, na.rm = T) + sum(sin_medida_cautelar, na.rm = T)) %>% 
      gather(detenido, Total, prision_preventiva:otras_medidas) %>%  
      mutate(detenido=case_when(
        detenido=="prision_preventiva" ~ "Prisión preventiva", 
        T ~ "Otras medidas"
      )) %>% #arrange(desc(detenido)) %>% 
      mutate(porcentaje=Total/sum(Total), 
             #total_label=cumsum(porcentaje)-.035#, 
             # detenido=case_when(
             #   grepl("oficiosa", detenido) ~ "Oficiosa", T~ "Justificada"
             # )
      ) %>% 
      ggplot(aes(x="", y=porcentaje)) + 
      geom_col(position = position_stack(reverse = TRUE), 
               aes(fill=detenido, group=detenido)) +
      geom_segment(data=prom_pris, aes(y=Promedio, yend=Promedio,
                                       colour="Promedio de año anterior"),
                   size=2, linetype="dashed",  x=.55, xend=1.45) +
      # geom_label(data=. %>% filter(porcentaje>0),
      #            aes(label=paste(percent(porcentaje), "\n", "(",
      #                            comma(Total,1), ")"), y=total_label, fill=detenido), show.legend = F, 
      #            size=5) +
      geom_label(data=. %>% filter(porcentaje>0),
                 aes(label=paste(percent(porcentaje), "\n", "(",
                                 comma(Total,1), ")"), group=detenido, fill=detenido), show.legend = F,
                 size=5, position = position_stack(vjust = .5, reverse = T)) +
      scale_y_continuous(labels = percent) + theme_void() +
      theme(legend.position = "bottom",
            axis.title.y = element_text(color="black", face = "bold"), 
            plot.margin = unit(c(.5, 0, 0, 0), "cm")
            
      )+
      scale_fill_manual(values = pal_pri) +
      geom_label(data=prom_pris, aes(y=Promedio*.98, label=percent(Promedio)),x=1.3, 
                 fill=colores[3], size=5) + 
      scale_color_manual(values = colores[3], labels=paste0("Promedio ", year(Sys.Date())-1)) +
      labs(fill="", color="", x=str_wrap("% prisión preventiva", width = 15)) + 
      coord_flip()
  } else{
    gr_pris <- base_hoy %>% 
      ggplot() +
      annotate(geom = "text", x=0, y=0, 
               label=paste0("No hay personas en prisión preventiva en este delito"), 
               size=10, colour="black") +
      theme_void() + theme(text = element_text(face = "bold"))
    
  }
  
  
  
  gr_flagr <- plot_grid(gr_det, gr_judi, gr_vinc, gr_pris, nrow = 4)
  
  gr_flagr <- plot_grid(ggdraw() +
                          draw_label("Flagrancias", fontface = "bold", size = 30), gr_flagr, 
                        ncol = 1, rel_heights = c(.1, .9))
  
  gr_cant_ord <- ord_hoy %>% 
    ggplot() +
    annotate(geom = "text", x=0, y=0, 
             label=paste0(nrow(ord_hoy), "\n",
                          "Ordenes\ncumplidas"), 
             size=8, colour=colores[1]) +
    theme_void() + theme(text = element_text(face = "bold"))
  
  col_aos <- c("Años anteriores"="grey", 
               "2019"="#164a80", "2020"="#ff4f4f", "2021"="#f9a819", 
               "2022"="#4EA5D9", "2023"="#7fc19d")
  
  if(nrow(ord_hoy)>0){
    gr_ord_hoy <- ord_hoy %>% drop_na(ano_inicio_ci) %>% 
      mutate(ano_inicio_ci=case_when(
        ano_inicio_ci<2019 ~ "Años\nanteriores", 
        T ~ as.character(ano_inicio_ci)
      ), 
      ano_inicio_ci=factor(ano_inicio_ci, 
                           levels = c("Años\nanteriores", "2019", "2020", "2021", 
                                      "2022", "2023"))) %>% 
      group_by(ano_inicio_ci) %>% summarise(Total=n()) %>% 
      mutate(porcentaje=Total/sum(Total)) %>%
      ggplot(aes(x="", y=porcentaje, group=ano_inicio_ci, fill=ano_inicio_ci)) +
      theme_minimal_hgrid() +
      geom_col(position = position_stack(reverse = TRUE)) + scale_y_continuous(labels = percent) +
      # geom_label_repel(aes(label=paste(percent(porcentaje, 1), "\n", 
      #                            " (", comma(Total, 1), ")"), 
      #                group=ano_inicio_ci, fill=ano_inicio_ci), 
      #            size=5, position = position_stack(vjust = .5, T), show.legend = F, direction = "y") +
      geom_label(aes(label=paste(percent(porcentaje, 1), #"\n", 
                                 " (", comma(Total, 1), ")"), 
                     group=ano_inicio_ci, fill=ano_inicio_ci), 
                 size=5, position = position_stack(vjust = .5, T), show.legend = F) +
      scale_fill_manual(values = c("Años\nanteriores"="gray", col_aos)) +
      labs(x="", y="", fill="Año de inicio") +
      theme(legend.position = "bottom", 
            legend.key.size = unit(10, "points"))
    
    # ord_hoy <- ord_hoy %>% 
    #   mutate(tiempos=case_when(
    #     tiempo_respuesta_2=="A. Mismo día" ~ "Mismo día", 
    #     tiempo_respuesta_2=="B. 2 a 7 días" ~ "1 semana",
    #     tiempo_respuesta_2=="C. 8 a 31 días" ~ "1 mes",
    #     tiempo_respuesta_2=="D. 32 a 365 días" ~ "1 año",
    #     T ~ "Más de un año"
    #   ), tiempos=factor(tiempos, 
    #                     levels = c("Mismo día", "1 semana", "1 mes", "1 año", 
    #                                "Más de un año")))
    # 
    # ord_past_t <- ord_past %>% 
    #   mutate(tiempos=case_when(
    #     tiempo_respuesta_2=="A. Mismo día" ~ "Mismo día", 
    #     tiempo_respuesta_2=="B. 2 a 7 días" ~ "1 semana",
    #     tiempo_respuesta_2=="C. 8 a 31 días" ~ "1 mes",
    #     tiempo_respuesta_2=="D. 32 a 365 días" ~ "1 año",
    #     T ~ "Más de un año"
    #   ), tiempos=factor(tiempos, 
    #                     levels = c("Mismo día", "1 semana", "1 mes", "1 año", 
    #                                "Más de un año"))) %>% 
    #   group_by(tiempos) %>% summarise(Total=n()) %>% 
    #   mutate(por_ant=Total/sum(Total)) %>% select(-Total)
    
    # gr_ord_tiempo <- ord_hoy %>% 
    #   group_by(tiempos) %>% summarise(Total=n()) %>% left_join(ord_past_t) %>% 
    #   
    #   mutate(porcentaje=Total/sum(Total))  %>% 
    #   ggplot(aes(x=0, porcentaje, fill=tiempos, group=tiempos)) +
    #   geom_col(alpha=.8, position = position_stack(reverse = TRUE)) +
    #   scale_fill_manual(values = c(colores[12], colores[8], colores[9], 
    #                                colores[7], colores[11])) +
    #   geom_label(aes(label=paste0(percent(porcentaje, accuracy = 1), " (", Total, ")"), 
    #                  group=tiempos, fill=tiempos), 
    #              size=5, show.legend = F, 
    #              position = position_stack(vjust = .5, T)) + theme_minimal_hgrid() +
    #   labs(x="", y="", fill="", color="") + scale_y_continuous(labels = percent) +
    #   geom_text(aes(x=.25, y=porcentaje, group=tiempos, color="Promedio año anterior",
    #                 label=percent(por_ant, accuracy = 1)), size=6,
    #             position = position_stack(vjust = .5, T)) +
    #   scale_color_manual(values = colores[3], labels="Promedio\naño anterior") +
    #   theme(axis.text.x = element_blank(), 
    #         legend.position = "bottom", legend.key.size = unit(10, "points")) +
    #   guides(color = guide_legend(nrow = 2), 
    #          fill = guide_legend(nrow = 2))
    
    # gr_prom_anio <- ord_past %>% summarise(Total=round(n()/52), .groups = "drop") %>% 
    #   ggplot() +
    #   # geom_text( x=.2, y=.5, 
    #   #          aes(label=Total), 
    #   #          size=10, colour=colores[1]) +
    #   geom_text( x=.4, y=.5, 
    #              aes(label=paste0(Total, "   Promedio ", year(floor_date(fecha_lim, "year")-1))), 
    #              size=7, colour="black") +
    #   theme_void() + theme(text = element_text(face = "bold"))
    # 
    # gr_prom_mes <- ord %>% 
    #   filter(year(fecha_cumplida)==year(floor_date(fecha_lim, "month")-1),
    #          month(fecha_cumplida)==month(floor_date(fecha_lim, "month")-1)) %>% 
    #   summarise(Total=round(n()/4), .groups = "drop") %>% 
    #   ggplot() +
    #   # geom_text( x=.2, y=.5, 
    #   #            aes(label=Total), 
    #   #            size=10, colour=colores[1]) +
    #   geom_text( x=.45, y=.5, 
    #              aes(label=paste0(Total, "   Promedio mes\nanterior")), 
    #              size=7, colour="black") +
    #   theme_void() + theme(text = element_text(face = "bold"))
    
    # gr_pro <- plot_grid(gr_prom_anio, gr_prom_mes, ncol = 1)
    # 
    # 
    # gr_ord_tiempos_final <- plot_grid(gr_pro, gr_ord_tiempo, ncol = 1, 
    #                                   rel_heights = c(2.2, 10))
    
    # gr_ord_hoy <- plot_grid(gr_ord_hoy, gr_ord_tiempos_final, ncol = 2, 
    #                         rel_widths = c(5.5, 5))
    
    
  } else{
    gr_ord_hoy <- ord_hoy %>% ggplot() + theme_void()
  }
  
  
  # gr_ord_hoy <- plot_grid(gr_cant_ord, gr_ord_hoy, nrow = 2,
  #                         rel_heights = c(2.2, 10))
  #creamos la segunda gráfica de ordenes cuánto tiempo tardo en cumplirse
  
  # gr_ord_hoy <- plot_grid(gr_ord_hoy, gr_ord_tiempos_final, ncol = 2, 
  #                         rel_widths = c(5,5.5))
  
  gr_ord_hoy <- plot_grid(gr_cant_ord, gr_ord_hoy, nrow = 2,
                          rel_heights = c(2.2, 10))
  
  gr_ord_hoy <- plot_grid(ggdraw() +
                            draw_label("Investigación sin detenido",
                                       fontface = "bold", size = 30), gr_ord_hoy, 
                          ncol = 1, rel_heights = c(.1, .9))
  
  
  # ord_mean_year <- ord_past %>% 
  #   group_by(fecha_sem) %>% summarise(Total=n(), .groups = "drop") %>% 
  #   summarise(mean=round(mean(Total, na.rm=T), 2)) %>% pull(mean)
  # 
  # ord_mean_month <- ord %>% 
  #   filter(floor_date(fecha_cumplida, "month")==floor_date(floor_date(fecha_lim, "month")-1, "month")) %>% 
  #   group_by(fecha_sem) %>% summarise(Total=n(), .groups = "drop") %>% 
  #   summarise(mean=round(mean(Total, na.rm=T), 2)) %>% pull(mean)
  # 
  # gr_mean <- ggplot() +
  #   annotate(geom = "text", x=0, y=0, 
  #            label=paste0(ord_mean_year, "  ",
  #                         " Promedio ", year(max(ord_past$fecha_cumplida)), "\n", "\n",
  #                         ord_mean_month, "   "," Promedio mes\n anterior"
  #                         ), 
  #            size=10, colour=colores[1]) +
  #   theme_void() + theme(text = element_text(face = "bold"))
  
  gr_restul <- plot_grid(gr_flagr, gr_ord_hoy, nrow = 1, rel_widths = c(10, 7))
  
  final_plot <- plot_grid(ggdraw() +
                            draw_label(paste0("Resultados ministeriales semana del ", 
                                              format(fecha_lim-7, "%d de %B de %Y"), " al ", 
                                              format(fecha_lim, "%d de %B de %Y")), 
                                       size = 30, fontface = "bold"), 
                          gr_restul, rel_heights = c(0.1, 0.9), ncol = 1)
  return(final_plot)
  # return(prom_pris)
  
  
  #función para comparar las tendencias de las alcaldías eligiendo el año de comparación
  plot_tendencia_alc2 <- function(
    base, 
    tipo = c("incidencia", "victimas"), 
    nombre = str_to_lower(nombre_plots),
    fecha_comienzo = fecha_inicio_global,
    fecha_lim = fecha_lim,
    info_alcs = info_alcs, 
    tabla = c(0,1), 
    num=ano_comparacion
  ) {
    
    
    serie <- 
      base %>% 
      group_by(
        ao = year(fecha_inicio),
        mes = month(fecha_inicio),
        delegacion_hechos
      ) %>% 
      summarise(
        total_mes = n(),
        fecha = as_date(paste(min(ao), min(mes), "01", sep = "-")),
        .groups = "drop"
      ) %>% 
      complete(fecha = seq.Date(
        as_date(fecha_comienzo), 
        as_date(fecha_lim), 
        by = "month"),
        delegacion_hechos,
        fill = list(total_mes = 0)) %>% 
      mutate(dias = days_in_month(fecha),
             ao = year(fecha),
             mes = month(fecha)) %>% 
      left_join(info_alcs) %>% drop_na(alcaldia) #se le agregro el drop na para quitar el Na del facets
    
    ###hacemos la base de la tabla de crecimiento
    # tabla <- base %>% mutate(Total=1) %>% 
    #   complete(fecha_inicio=seq.Date(as_date(fecha_comienzo), as_date(fecha_lim), 
    #                                  by="1 day"),
    #            delegacion_hechos=unique(info_alcs$delegacion_hechos),
    #            fill=list(Total=0)) %>% 
    #   group_by(fecha_inicio, delegacion_hechos) %>% summarise(Total=sum(Total), .groups = "drop") %>%
    #   group_by(año=year(fecha_inicio), delegacion_hechos) %>% summarise(media=round(mean(Total), 2)) %>% 
    #   filter(año %in% c(year(min(base$fecha_inicio)),year(max(base$fecha_inicio)))) %>% spread(año, media) %>% 
    #   mutate(cambio=(`2022`/`2019`-1)*100, 
    #          delegacion_hechos=str_to_title(delegacion_hechos)) %>% arrange(desc(cambio)) %>% 
    #   mutate("Cambio porcentual"=paste0(round(cambio,2), "%")) %>% 
    #   select(-cambio) %>%
    #   rename("Alcaldía"=delegacion_hechos, "Promedio diario 2019"=`2019`, 
    #          "Promedio diario 2022"=`2022`)
    # 
    # tb_raster <- tabla %>% 
    #   flextable() %>% border(border =  std_border) %>% 
    #   bg(i= 1, bg="#003763", part = "header") %>%
    #   color(i= 1, color="ghostwhite", part = "header") %>% 
    #   bold(i= 1, bold=T, part = "header") %>% 
    #   align(align = "center") %>% 
    #   bold(j=1, bold = T) %>% color(i=1:6, j=4, color = "#CE3C3F") %>% 
    #   color(i=10:16, j=4, color = "#06945f")
    
    # gr_tabla_incremento <- ggplot() + 
    #   theme_void() + 
    #   annotation_custom(rasterGrob(tb_raster), 
    #                     xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
    
    if(day(fecha_lim) < unique(serie$dias[serie$fecha == max(serie$fecha)])) {
      serie_lm <- 
        serie %>% 
        filter(fecha < max(fecha))
      comp <- FALSE
    } else {
      serie_lm <- serie
      comp <- TRUE
    }
    
    tendencias <- 
      ggplot(serie, aes(x = fecha, y = total_mes)) +
      facet_wrap(~alcaldia, scales = "free_y") +
      geom_vline(data = serie %>% 
                   select(fecha) %>% 
                   distinct() %>% 
                   filter(month(fecha) == 1),
                 aes(xintercept = fecha),
                 linetype = "dotdash", color = "gray50") +
      geom_point(color = colores[9], size = 1.7, alpha = .7) +
      geom_smooth(data = serie_lm, 
                  aes(x = fecha, y = total_mes),
                  se = F, color = colores[4]) +
      labs(
        title = str_wrap(paste0("Incidencia delictiva (tendencias anuales) - ", nombre), 65),
        subtitle = paste0("Corte al ", format(as_date(fecha_lim), "%d de %B, %Y")),
        x = "Mes",
        y = "Carpetas iniciadas por mes",
        color = "Año de inicio de las carpetas"
      ) +
      scale_x_date(
        # date_breaks = "3 months", 
        date_labels = "%b\n%Y",
        # date_minor_breaks = "1 month", 
        # breaks = NULL,
        # limits = c(as_date("2017-12-01"), NA)) +
        limits = c(min(serie$fecha)-30, NA)) +
      scale_y_continuous(label = comma_format(accuracy = 1)) +
      theme_light() +
      guides(colour = guide_legend(override.aes = list(size = 2))) +
      theme(
        plot.title = element_text(face = "bold"),
        strip.background = element_rect(fill = "gray80"),
        strip.text = element_text(color = "black"),
        axis.text.x = element_text()
      )
    
    if(comp == FALSE) {
      tendencias <- 
        tendencias +
        labs(caption = "La tendencia no considera el mes en curso")
    }  
    
    if (tipo == "victimas") {
      tendencias <- 
        tendencias +
        labs(title = paste0("Víctimas (tendencias anuales) - ", delitos_sel))
    }
    
    if (tabla==1){
      std_border = fp_border(color="gray", width = 1)
      
      tabla_crecimiento <- base %>% mutate(Total=1) %>% 
        complete(fecha_inicio=seq.Date(min(base$fecha_inicio), max(base$fecha_inicio), 
                                       by="1 day"),
                 delegacion_hechos=unique(info_alcs$delegacion_hechos),
                 fill=list(Total=0)) %>% 
        group_by(fecha_inicio, delegacion_hechos) %>% summarise(Total=sum(Total), .groups = "drop") %>%
        group_by(año=year(fecha_inicio), delegacion_hechos) %>% summarise(media=round(mean(Total), 2)) %>% 
        # filter(año %in% c(year(min(base$fecha_inicio)),year(max(base$fecha_inicio)))) %>% 
        filter(año %in% c(num, year(max(base$fecha_inicio)))) %>% 
        spread(año, media) %>% drop_na(delegacion_hechos) %>% 
        left_join(info_alcs) %>% select(-clave) %>% 
        `colnames<-`(c("delegacion_hechos", "año_pasado", "año_actual", "Alcaldía")) %>% 
        mutate(cambio=(año_actual/año_pasado-1)*100, 
               delegacion_hechos=str_to_title(delegacion_hechos)) %>% 
        mutate(cambio=case_when(
          is.nan(cambio) ~ 0, is.infinite(cambio) ~ 100, T ~ cambio
        )) %>% drop_na(delegacion_hechos) %>% 
        arrange(desc(cambio))
      
      tabla_raster <- tabla_crecimiento %>% select(-delegacion_hechos) %>% relocate(Alcaldía) %>% 
        # `colnames<-`(c("Alcaldía", paste0("Promedio\ndiario ", year(min(base$fecha_inicio))), 
        #                paste0("Promedio\ndiario ", year(max(base$fecha_inicio))), 
        #                "Cambio porcentual")) %>% 
        `colnames<-`(c("Alcaldía", paste0("Promedio\ndiario ", num), 
                       paste0("Promedio\ndiario ", year(max(base$fecha_inicio))), 
                       "Cambio porcentual")) %>% 
        mutate("Cambio porcentual"=paste0(round(`Cambio porcentual`,2), "%")) %>% 
        drop_na(Alcaldía) %>% 
        flextable() %>% border(border =  std_border) %>% 
        bg(i= 1, bg="#003763", part = "header") %>%
        color(i= 1, color="ghostwhite", part = "header") %>% 
        bold(i= 1, bold=T, part = "header") %>% 
        align(align = "center", part = "all") %>% 
        width(j=1, width=.5) %>% 
        bold(j=1, bold = T) %>% 
        #autofit() %>%
        as_raster()
      
      serie_lm <- serie_lm %>% 
        mutate(delegacion_hechos=str_to_title(delegacion_hechos)) %>% 
        left_join(tabla_crecimiento) %>% 
        mutate(crecimiento=case_when(
          cambio>0 ~"A la alza", cambio<0 ~ "A la baja", cambio==0 ~ "Sin cambio"
        ))
      
      tendencias <- serie %>% 
        ggplot(aes(x = fecha, y = total_mes)) +
        facet_wrap(~alcaldia, scales = "free_y") +
        geom_vline(data = serie %>% 
                     select(fecha) %>% 
                     distinct() %>% 
                     filter(month(fecha) == 1),
                   aes(xintercept = fecha),
                   linetype = "dotdash", color = "gray50") +
        geom_point(color = colores[9], size = 1.7, alpha = .7) +
        geom_smooth(data = serie_lm, 
                    aes(x = fecha, y = total_mes, color=crecimiento),
                    se = F) +
        labs(
          title = str_wrap(paste0("Incidencia delictiva (tendencias anuales) - ", nombre), 65),
          subtitle = paste0("Corte al ", format(as_date(fecha_lim), "%d de %B, %Y")),
          x = "Mes",
          y = "Carpetas iniciadas por mes",
          color = "Año de inicio de las carpetas"
        ) +
        scale_color_manual(values = c("A la alza"="#ff4f4f", "A la baja"="#00b140",
                                      "Sin cambio"="gray")) +
        scale_x_date(
          # date_breaks = "3 months", 
          date_labels = "%b\n%Y",
          # date_minor_breaks = "1 month", 
          # breaks = NULL,
          # limits = c(as_date("2017-12-01"), NA)) +
          limits = c(min(serie$fecha)-30, NA)) +
        scale_y_continuous(label = comma_format(accuracy = 1)) +
        theme_light() +
        guides(colour = guide_legend(override.aes = list(size = 2))) +
        theme(
          plot.title = element_text(face = "bold"),
          strip.background = element_rect(fill = "gray80"),
          strip.text = element_text(color = "black"),
          axis.text.x = element_text(), 
          legend.position = "none"
        )
      
      gtabla <- ggplot() + 
        theme_void() + 
        annotation_custom(rasterGrob(tabla_raster)#, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf
        )
      
      
      final <- plot_grid(tendencias, gtabla, nrow = 1, 
                         rel_widths = c(10, 4))
    } else {
      final <- tendencias
    }
    
    return(final)
  }
  
  
}

plot_esta <- function(
    base=data, 
    fecha_ini=fecha_inicio_global, 
    fecha_lim=fecha_lim,
    nombre=delito
){
  if(days_in_month(fecha_lim)==mday(fecha_lim)){
    fecha_lim <- fecha_lim
  } else {
    fecha_lim <- floor_date(floor_date(as_date(fecha_lim), "month")-1, 
                            "month")
  }

if(fecha_ini<=fecha_lim-730){
  fecha_ini
} else {fecha_ini=fecha_lim-730}
  
  data_ts <- base %>% 
    filter(fecha_inicio<=ceiling_date(fecha_lim, "month")-1) %>% 
    group_by(fecha=floor_date(fecha_inicio, "month")) %>% summarise(Total=n()) %>% 
    ungroup() %>% 
    complete(fecha=seq.Date(as_date(fecha_ini),
                            as.Date(fecha_lim), 
                            "1 month"), 
             fill=list(Total=0))
  ts_total <- ts(data_ts$Total, frequency = 12, start = c(year(fecha_ini), 1))
  
  
  df_stl <- cbind(stl(ts_total, "per")$time.series[, "trend"],
                  stl(ts_total, "per")$time.series[, "seasonal"],
                  stl(ts_total, "per")$time.series[, "remainder"])
  
  colnames(df_stl) <- c("Tendencia", "Estacionalidad", "Residuo")
  
  df_stl <- df_stl %>% as_tibble() %>% 
    mutate(fecha=seq.Date(as_date(fecha_ini),fecha_lim, 
                          "1 month"))
  
  barra <- tibble(
    tipo=c("Total", "Tendencia", "Estacionalidad",
           "Residuo"),
    fecha=ceiling_date(
      ceiling_date(fecha_lim, "month"), "month"),
    valor_min=min(df_stl$Estacionalidad, na.rm=T),
    valor_max=max(df_stl$Estacionalidad, na.rm=T),
    diff_valor=valor_max-valor_min,
    mediana=c(median(data_ts$Total), median(df_stl$Tendencia),
              median(df_stl$Estacionalidad),
              median(df_stl$Residuo)),
    valor_minimo=mediana+valor_min,valor_maximo=mediana+valor_max,
    
  ) %>% 
    select(-c(valor_min, valor_max, diff_valor, mediana)) %>% 
    # gather(tipo_valor, valor, valor_minimo, valor_maximo) %>%
    mutate(tipo=factor(tipo, 
                       levels=c("Total", "Tendencia", "Estacionalidad", 
                                "Residuo"))) 
  gr_ts <- df_stl %>% 
    left_join(data_ts %>% mutate(fecha=floor_date(fecha, "month")), #%>%
              #select(fecha, "Media"=media), 
              by="fecha") %>%
    gather(tipo, Total, Total, Tendencia, Estacionalidad, Residuo) %>% 
    mutate(tipo=factor(tipo, 
                       levels=c("Total", "Tendencia", "Estacionalidad", 
                                "Residuo"))) %>% 
    ggplot(aes(fecha, Total, color=tipo)) +
    facet_grid(tipo~., scales = "free") + 
    geom_point(size=1.15, alpha=.5) +
    geom_line(linewidth=.9) + 
    geom_segment(data=barra,
                 aes(x=fecha, xend=fecha, y=valor_minimo,
                     yend=valor_maximo), linewidth=5,
                 alpha=.5, color="black") +
    theme_light() +
    geom_vline(data=. %>% group_by(ao=floor_date(fecha, "year")) %>% summarise(Total=n()), 
               aes(xintercept=ao), linewidth=1.35, color="black", alpha=.7, 
               linetype="longdash") +
    # geom_text(data=. %>% group_by(ao=floor_date(fecha, "year"), 
    #                               tipo) %>% summarise(Total=min(Total)), 
    #            aes(x=ao, y=min(Total), label=year(ao)),
    #           color="black", fontface="bold") +
    geom_hline(data=. %>% filter(tipo %in% c("Estacionalidad")), 
               aes(yintercept = 0), linewidth=1, linetype="solid", 
               alpha=.85, color=colores[2]) +
    geom_hline(data=. %>% filter(tipo %in% c("Residuo")), 
               aes(yintercept = 0), linewidth=1, linetype="solid", 
               alpha=.5) +
    theme(strip.placement = "outside",
          panel.spacing.x = unit(0.5, "cm"), 
          legend.position = "none", 
          strip.text = element_text(size=14, color="black", 
                                    face = "bold"), 
          text=element_text(size=15),
          plot.title = element_text(face = "bold")) + 
    scale_y_continuous(labels = comma) +
    labs(title = paste0("Serie de tiempo - ", nombre), 
         subtitle = paste("Desde", format(as_date(fecha_ini), "%d de %B de %Y"), 
                          "hasta", format(as_date(fecha_lim), "%d de %B de %Y")),
         x="Fecha de inicio", y="Carpetas de investigación") + 
    scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y") +
    scale_color_manual(values = c(colores[1:2], colores[8], "gray"))
  
  
  return(gr_ts)
}


get_radicacion <- function(datos=data){
  
  id_data <- datos %>% drop_na(id_ap) %>% 
    pull(id_ap)
  
  require(pacman)
  p_load(RJDBC, tidyverse, lubridate, janitor, flextable, webshot, yaml)
  
  auth <- read_yaml("Y:/3 CODIGOS R (Version 2)/tablero_petif/config.yml")
  library(dbplyr)
  library(pool)
  library(RMySQL)
  
  Pool <- dbPool(
    MySQL(),
    host = auth$server,
    dbname = "nominal",
    user = auth$UID,
    #password = auth$PWD,
    password = "78b2/Z4oQIvu*D4C3*E46A76#..,",
    encoding = auth$encoding
  )
  
  ###obtenemos el ctrl de SIED
  
  datos_ctrl <- dbGetQuery(Pool, 
                           paste0("select SAP,  idAveriguacionPrevia id_ap
                      from averiguacionPrevia
                      where idAveriguacionPrevia in (", paste(id_data, collapse=","),
                                  ")")) %>% tibble() 
  poolClose(Pool)
  
  #conectamos con SIAP Y FSIAP
  
  drv <- JDBC("com.informix.jdbc.IfxDriver", "Y:/3 CODIGOS R/keys/ifxjdbc.jar")
  
  url_inf = paste0("jdbc:informix-sqli", "://", "172.22.2.80", ":","1550", "/", "uinv", ":informixserver=", "sjp_central")
  url_inf2 = paste0("jdbc:informix-sqli", "://", "172.22.109.253", ":","1590", "/", "uinv", ":informixserver=", "fgj_central")
  
  
  
  jdbcConnection <- dbConnect(drv, url_inf, 
                              "uet", "23#Ag*rG&JE2024V2#..,") #"H1rw70*G&v@J"
  jdbcConnection2 <- dbConnect(drv, url_inf2, 
                               "uet", "23#Ag*rG&JE2024V2#..,")
  #hacemos variable para que con ciclo podamos obtener todas las querys
  data_ctrl <- tibble(datos_ctrl) %>% 
    mutate(row=row_number(), 
           ciclo=ifelse(row %% 500 == 0, 
                        (row %/% 500) + 1, 
                        (row %/% 500) + 1))
  
  radicacion_siap <- vector_ctrl <- list()
  
  for (i in 1:length(unique(data_ctrl$ciclo))) {
    id_ctrl <- data_ctrl %>% filter(ciclo %in% i)
    vector_ctrl[[i]] <- paste("'", paste(id_ctrl$SAP, collapse = "', '"), "'", sep = "")
    radicacion_siap[[i]] <- dbGetQuery(jdbcConnection, 
                                       paste0("select a.ctrluinv, concat(f.cenu_paterno,concat(' ',concat(f.cenu_materno,concat(' ',f.cenu_nombre)))) usuario_rad,
                              c.descfiscalia fiscalia_des, d.desc_agencia agencia_des,e.desc_unidad unidad_des, a.fecharecep
                              from cenremision a 
                              inner join 
                              (select ctrluinv, max(idremision) idremision from cenremision where status=1 and fecharecep is not null group by ctrluinv) b
                              on a.ctrluinv=b.ctrluinv and a.idremision=b.idremision
                              left join catalogos:catpgjdf c on a.cvefiscaliades=c.cvefiscalia
                              left join catalogos:catagencias d on a.cvefiscaliades=d.cvefiscalia and a.cveagenciades=d.cveagencia
                              left join catalogos:catunidades e on a.cvefiscaliades=e.cvefiscalia and a.cveagenciades=e.cveagencia and a.cveunidaddes=e.cveunidad
                              left join s36ur1d4d:cen_users f on a.usuariorad=f.id_user
                                     where a.ctrluinv in (", vector_ctrl[[i]],")"))
  }
  
  radi_siap <- bind_rows(radicacion_siap)
  ###para fsiap
  radicacion_fsiap <- vector_ctrl <- list()
  
  for (i in 1:length(unique(data_ctrl$ciclo))) {
    id_ctrl <- data_ctrl %>% filter(ciclo %in% i)
    vector_ctrl[[i]] <- paste("'", paste(id_ctrl$SAP, collapse = "', '"), "'", sep = "")
    radicacion_fsiap[[i]] <- dbGetQuery(jdbcConnection2, 
                                        paste0("select a.ctrluinv, concat(f.cenu_paterno,concat(' ',concat(f.cenu_materno,concat(' ',f.cenu_nombre)))) usuario_rad,
                              c.descfiscalia fiscalia_des, d.desc_agencia agencia_des,e.desc_unidad unidad_des, a.fecharecep
                              from cenremision a 
                              inner join 
                              (select ctrluinv, max(idremision) idremision from cenremision where status=1 and fecharecep is not null group by ctrluinv) b
                              on a.ctrluinv=b.ctrluinv and a.idremision=b.idremision
                              left join catalogos:catpgjdf c on a.cvefiscaliades=c.cvefiscalia
                              left join catalogos:catagencias d on a.cvefiscaliades=d.cvefiscalia and a.cveagenciades=d.cveagencia
                              left join catalogos:catunidades e on a.cvefiscaliades=e.cvefiscalia and a.cveagenciades=e.cveagencia and a.cveunidaddes=e.cveunidad
                              left join s36ur1d4d:cen_users f on a.usuariorad=f.id_user
                                     where a.ctrluinv in (", vector_ctrl[[i]],")"))
  }
  
  radi_fsiap <- bind_rows(radicacion_fsiap)
  
  #determinanos cuáles tuvieron cambio de radicación y cuáles no
  radi_total <- bind_rows(radi_siap, radi_fsiap) %>% 
    right_join(datos_ctrl, 
               by=c("ctrluinv"="SAP")) %>% 
    mutate(cambio_radicacion=ifelse(is.na(usuario_rad), 
                                    "sin cambio", "con cambio"
    )) %>% tibble()
  
  #obtenemos las que no han cambiado
  ctrl_sin_rad <- radi_total %>% 
    filter(is.na(usuario_rad)) %>% pull(ctrluinv)
  
  #creamos ciclo para las que no han cambiado de fiscalia
  ctrl_sin_rad <- tibble(ctrl_sin_rad) %>% 
    mutate(row=row_number(), 
           ciclo=ifelse(row %% 500 == 0, 
                        (row %/% 500) + 1, 
                        (row %/% 500) + 1))
  #siap
  vector_ctrl_sinrad <- radicacion_fsiap_sinrad <- list()
  
  for (i in 1:length(unique(ctrl_sin_rad$ciclo))) {
    id_ctrl <- ctrl_sin_rad %>% filter(ciclo %in% i)
    vector_ctrl_sinrad[[i]] <- paste("'", paste(id_ctrl$ctrl_sin_rad, collapse = "', '"), "'", sep = "")
    radicacion_fsiap_sinrad[[i]] <- dbGetQuery(jdbcConnection, 
                                               paste0("select
           a.ctrluinv, b.descfiscalia, c.desc_agencia,d.desc_unidad, concat(e.cenu_paterno,concat(' ',concat(e.cenu_materno,concat(' ',e.cenu_nombre)))) fiscal_inicia
           from cencontrol a
           left join catalogos:catpgjdf b on a.cvefiscalia=b.cvefiscalia
           left join catalogos:catagencias c on a.cvefiscalia=c.cvefiscalia and a.cveagencia=c.cveagencia
           left join catalogos:catunidades d on a.cvefiscalia=d.cvefiscalia and a.cveagencia=d.cveagencia and a.cveunidad=d.cveunidad
           left join s36ur1d4d:cen_users e on a.fiscal=e.id_user
           where a.ctrluinv in (", vector_ctrl_sinrad[[i]],")"))
  }
  
  ci_inicio_siap <- bind_rows(radicacion_fsiap_sinrad)
  
  #fsiap
  vector_ctrl_sinrad2 <- radicacion_fsiap_sinrad2 <- list()
  for (i in 1:length(unique(ctrl_sin_rad$ciclo))) {
    id_ctrl <- ctrl_sin_rad %>% filter(ciclo %in% i)
    vector_ctrl_sinrad2[[i]] <- paste("'", paste(id_ctrl$ctrl_sin_rad, collapse = "', '"), "'", sep = "")
    radicacion_fsiap_sinrad2[[i]] <- dbGetQuery(jdbcConnection2, 
                                                paste0("select 
           a.ctrluinv, b.descfiscalia, c.desc_agencia,d.desc_unidad, concat(e.cenu_paterno,concat(' ',concat(e.cenu_materno,concat(' ',e.cenu_nombre)))) fiscal_inicia
           from cencontrol a 
           left join catalogos:catpgjdf b on a.cvefiscalia=b.cvefiscalia
           left join catalogos:catagencias c on a.cvefiscalia=c.cvefiscalia and a.cveagencia=c.cveagencia
           left join catalogos:catunidades d on a.cvefiscalia=d.cvefiscalia and a.cveagencia=d.cveagencia and a.cveunidad=d.cveunidad
           left join s36ur1d4d:cen_users e on a.fiscal=e.id_user
           where a.ctrluinv in (", vector_ctrl_sinrad2[[i]],")"))
  }
  
  ci_inicio_fsiap <- bind_rows(radicacion_fsiap_sinrad2)
  
  
  ci_inicio_total <- bind_rows(ci_inicio_siap, ci_inicio_fsiap) %>% 
    rename(usuario_rad=fiscal_inicia, 
           fiscalia_des=descfiscalia, agencia_des=desc_agencia, 
           unidad_des=desc_unidad)
  
  
  #juntamos los ids con las que cambiaron y no cambiaron
  radicaciones <- radi_total %>% 
    filter(!ctrluinv %in% ctrl_sin_rad$ctrl_sin_rad) %>% 
    bind_rows(ci_inicio_total %>% left_join(radi_total %>% 
                                              select(ctrluinv, id_ap), 
                                            by="ctrluinv")) %>% 
    mutate(cambio_radicacion=ifelse(is.na(cambio_radicacion), "sin cambio", cambio_radicacion))
  
  dbDisconnect(jdbcConnection)
  dbDisconnect(jdbcConnection2)
  return(radicaciones)
}

get_campap <- function(datos=data){
  
  id_data <- datos %>% drop_na(id_ap) %>% 
    pull(id_ap)
  
  require(pacman)
  p_load(RJDBC, tidyverse, lubridate, janitor, flextable, webshot, yaml)
  
  auth <- read_yaml("Y:/3 CODIGOS R (Version 2)/tablero_petif/config.yml")
  library(dbplyr)
  library(pool)
  library(RMySQL)
  
  Pool <- dbPool(
    MySQL(),
    host = auth$server,
    dbname = "nominal",
    user = auth$UID,
    #password = auth$PWD,
    password = "78b2/Z4oQIvu*D4C3*E46A76#..,",
    encoding = auth$encoding
  )
  
  ###obtenemos el ctrl de SIED
  
  datos_ctrl <- dbGetQuery(Pool, 
                           paste0("select SAP,  idAveriguacionPrevia id_ap
                      from averiguacionPrevia
                      where idAveriguacionPrevia in (", paste(id_data, collapse=","),
                                  ")")) %>% tibble() 
  poolClose(Pool)
  
  #conectamos con SIAP Y FSIAP
  
  drv <- JDBC("com.informix.jdbc.IfxDriver", "Y:/3 CODIGOS R/keys/ifxjdbc.jar")
  
  url_inf = paste0("jdbc:informix-sqli", "://", "172.22.2.80", ":","1550", "/", "uinv", ":informixserver=", "sjp_central")
  url_inf2 = paste0("jdbc:informix-sqli", "://", "172.22.109.253", ":","1590", "/", "uinv", ":informixserver=", "fgj_central")
  
  
  
  jdbcConnection <- dbConnect(drv, url_inf, 
                              "uet", "23#Ag*rG&JE2024V2#..,")
  jdbcConnection2 <- dbConnect(drv, url_inf2, 
                               "uet", "23#Ag*rG&JE2024V2#..,")
  #hacemos variable para que con ciclo podamos obtener todas las querys
  data_ctrl <- tibble(datos_ctrl) %>% 
    mutate(row=row_number(), 
           ciclo=ifelse(row %% 500 == 0, 
                        (row %/% 500) + 1, 
                        (row %/% 500) + 1))
  campap_siap <- campap_fsiap <- vector_ctrl_sinrad2 <- list()
  for (i in 1:length(unique(data_ctrl$ciclo))) {
    id_ctrl <- data_ctrl %>% filter(ciclo %in% i)
    vector_ctrl_sinrad2[[i]] <- paste("'", paste(id_ctrl$SAP, collapse = "', '"), "'", sep = "")
    campap_siap[[i]] <- dbGetQuery(jdbcConnection, 
                                   paste0("select 
                                              ctrluinv, case when propuesta='R' then 'Reserva o Archivo Temporal'
                                              when propuesta='N' then 'No Ejercicio de la Acción Penal' end propuesta, 
                                              case determinacion when 1 then 'Aprobada' when 2 then 'Objetada' 
                                              when 3 then 'Incompetencia' else 'Pendiente' end estatus, fecdictamen
                                              from invecampap 
                                              where status=1
           and ctrluinv in (", vector_ctrl_sinrad2[[i]],")"
                                   )
    )
    
    campap_fsiap[[i]] <- dbGetQuery(jdbcConnection2, 
                                    paste0("select 
                                              ctrluinv, case when propuesta='R' then 'Reserva o Archivo Temporal'
                                              when propuesta='N' then 'No Ejercicio de la Acción Penal' end propuesta, 
                                              case determinacion when 1 then 'Aprobada' when 2 then 'Objetada' 
                                              when 3 then 'Incompetencia' else 'Pendiente' end estatus, fecdictamen
                                              from invecampap 
                                              where status=1
           and ctrluinv in (", vector_ctrl_sinrad2[[i]],")"
                                    )
    )
  }
  
  tot_campap <- bind_rows(campap_siap) %>% bind_rows(campap_fsiap)
  dbDisconnect(jdbcConnection)
  dbDisconnect(jdbcConnection2)
  
  
  tot_campap <- tot_campap %>% 
    left_join(data_ctrl %>% select(SAP, id_ap), 
              by=c("ctrluinv"="SAP"))
  return(tot_campap)
}


get_punibles <- function(datos=data){
  id_data <- datos %>% drop_na(id_ap) %>% 
    pull(id_ap)
  
  library(DBI)
  library(yaml)
  library(pool)
  library(RMySQL)
  auth <- read_yaml("Y:/3 CODIGOS R (Version 2)/tablero_petif/config.yml")
  
  Pool <- dbPool(
    MySQL(),
    host = auth$server,
    dbname = "nominal",
    user = auth$UID,
    #password = auth$PWD,
    password = "78b2/Z4oQIvu*D4C3*E46A76#..,"
    #encoding = auth$encoding
  )
  
  punibles <- dbGetQuery(
    Pool, 
    paste0("select a.idAveriguacionPrevia, a.idPersona, b.apellidoPaterno, b.apellidoMaterno, b.nombre, b.razonSocial,
  c.descripcion categoria, b.sexo, b.edad
  from dgpec.personaAveriguacionPrevia a
  left join dgpec.persona b
  on a.idPersona=b.idPersona
  left join nominal.categoriaPersona c
  on a.idCategoriaPersona=c.idCategoriaPersona
         where a.idAveriguacionPrevia in (", paste(id_data, collapse=","), ")"))
  
  poolClose(Pool)
  
  return(punibles)
  
}

get_no_punibles <- function(datos=data){
  id_data <- datos %>% drop_na(id_ap) %>% 
    pull(id_ap)
  
  library(DBI)
  library(yaml)
  library(pool)
  library(RMySQL)
  auth <- read_yaml("Y:/3 CODIGOS R (Version 2)/tablero_petif/config.yml")
  
  Pool <- dbPool(
    MySQL(),
    host = auth$server,
    dbname = "nominal",
    user = auth$UID,
    #password = auth$PWD,
    password = "78b2/Z4oQIvu*D4C3*E46A76#..,"#,
    #encoding = auth$encoding
  )
  
  no_punibles <- dbGetQuery(
    Pool, 
    paste0("select a.idAveriguacionPrevia, a.idPersona, b.apellidoPaterno, b.apellidoMaterno, b.nombre, b.razonSocial,
  c.descripcion categoria, b.edad, b.sexo
  from dgpec.personaNoPunibleAveriguacionPrevia a
  left join dgpec.persona b
  on a.idPersona=b.idPersona
  left join nominal.categoriaPersona c
  on a.idCategoriaPersona=c.idCategoriaPersona
  where a.idAveriguacionPrevia in (", paste(id_data, collapse=","), ")"))
  
  poolClose(Pool)
  
  return(no_punibles)
  
}

get_ministerial <- function(datos=data){
  id_data <- datos %>% drop_na(id_ap) %>% 
    pull(id_ap)
  
  library(DBI)
  library(yaml)
  library(pool)
  library(RMySQL)
  auth <- read_yaml("Y:/3 CODIGOS R (Version 2)/tablero_petif/config.yml")
  
  Pool <- dbPool(
    MySQL(),
    host = auth$server,
    dbname = "nominal",
    user = auth$UID,
    #password = auth$PWD,
    password = "78b2/Z4oQIvu*D4C3*E46A76#..,"#,
    #encoding = auth$encoding
  )
  
  ministerial <- dbGetQuery(
    Pool, 
    paste0("select a.idAveriguacionPrevia, c.descripcion delitoSIAP, d.descripcion modalidadSIAP
  from dgpec.averiguacionPrevia a 
  left join nominal.delitoInicialAveriguacionPrevia b
  on a.idAveriguacionPrevia=b.idAveriguacionPrevia
  left join nominal.delitoSAP c
  on b.idDelitoSAP=c.idDelitoSAP
  left join nominal.modalidadDelitoSAP d
  on b.idModalidadDelitoSAP=d.idModalidadDelitoSAP
         where a.idAveriguacionPrevia in (", paste(id_data, collapse=","), ")"))
  
  poolClose(Pool)
  
  Encoding(ministerial$delitoSIAP) <- "UTF-8"
  Encoding(ministerial$modalidadSIAP) <- "UTF-8"
  
  return(ministerial)
  
}

get_entrevistas <- function(datos=data){
  id_data <- datos %>% drop_na(id_ap) %>% 
    pull(id_ap)
  
  library(DBI)
  library(yaml)
  library(pool)
  library(RMySQL)
  auth <- read_yaml("Y:/3 CODIGOS R (Version 2)/tablero_petif/config.yml")
  
  Pool <- dbPool(
    MySQL(),
    host = auth$server,
    dbname = "nominal",
    user = auth$UID,
    #password = auth$PWD,
    password = "78b2/Z4oQIvu*D4C3*E46A76#..,"#,
    #encoding = auth$encoding
  )
  
  entrevistas <- dbGetQuery(
    Pool, 
    paste0(paste0("select a.idAveriguacionPrevia, idDeclaracionAveriguacionPrevia, descripcion idFormato, 
nominal.getNumAveriguacionPrevia(a.idAveriguacionPrevia) carpeta, declaracion 
from nominal.averiguacionPrevia a 
inner join nominal.declaracionAveriguacionPrevia b
on a.idAveriguacionPrevia=b.idAveriguacionPrevia
left join nominal.formato c
on b.idFormato=c.idFormato
         where a.idAveriguacionPrevia in (", paste(id_data, collapse=","), ")")))
  
  poolClose(Pool)
  
  Encoding(entrevistas$declaracion) <- "UTF-8"
  
  return(entrevistas)
  
}

get_delito_hom <- function(datos=data){
  data <- datos %>% 
    mutate(tipo_delito=case_when(
      delito=="ROBO DE VEHÍCULO CON Y SIN VIOLENCIA" &  
        grepl("CON VIOLENCIA|C/V", modalidad_delito) ~ "Robo de vehículo con violencia", 
      delito=="ROBO DE VEHÍCULO CON Y SIN VIOLENCIA" &  
        grepl("SIN VIOLENCIA|S/V", modalidad_delito) ~ "Robo de vehículo sin violencia",
      grepl("METROB", modalidad_delito) ~ "Robo a pasajero de metrobús", #No es alto impacto, pero lo agregamos
      grepl("TRANSE", delito) ~ "Robo a transeúnte en vía pública", 
      grepl("DISPARO", delito) ~ "Lesiones dolosas por disparo de arma de fuego",
      grepl("CUENTA", delito) ~ "Robo a cuentahabiente",
      grepl("MICRO", delito) ~ "Robo a pasajero en microbús",
      grepl("METRO", delito) ~ "Robo a pasajero en metro", 
      delito=="HOMICIDIO DOLOSO" & grepl("FEMINI", modalidad_delito) ~ "Feminicidio",
      delito=="HOMICIDIO DOLOSO" ~ str_to_sentence(delito), 
      delito=="ROBO A NEGOCIO CON VIOLENCIA" ~ str_to_sentence(delito), 
      delito=="ROBO A PASAJERO A BORDO DE TAXI CON VIOLENCIA" ~ str_to_sentence(delito),
      delito=="ROBO A REPARTIDOR CON Y SIN VIOLENCIA" ~ str_to_sentence(delito),
      delito=="ROBO A CASA HABITACIÓN CON VIOLENCIA" ~ str_to_sentence(delito),
      delito=="ROBO A TRANSPORTISTA CON Y SIN VIOLENCIA" ~ str_to_sentence(delito),
      delito=="SECUESTRO" ~ str_to_sentence(delito),
      delito=="VIOLACIÓN" ~ str_to_sentence(delito),
      modalidad_delito=="AMENAZAS" ~ str_to_sentence(modalidad_delito),
      modalidad_delito=="FRAUDE" ~ str_to_sentence(modalidad_delito),
      grepl("ROBO DE OBJE", modalidad_delito) ~ "Robo de objetos",
      modalidad_delito=="DESPOJO" ~ str_to_sentence(modalidad_delito),
      modalidad_delito=="EXTORSION" ~ "Extorsión", 
      modalidad_delito=="COBRANZA ILEGITIMA" ~ str_to_sentence(modalidad_delito), 
      modalidad_delito %in% c("ROBO S/V DENTRO DE NEGOCIOS, AUTOSERVICIOS, CONVENIENCIA",
                              "ROBO A NEGOCIO SIN VIOLENCIA", 
                              "ROBO A NEGOCIO Y VEHICULO SIN VIOLENCIA") ~ "Robo a negocio sin violencia",
      grepl("FARDERO", modalidad_delito) ~ "Robo a negocio por farderos",
      grepl("ROBO A CASA HAB", modalidad_delito) ~ "Robo a casa habitación sin violencia",
      grepl("SUSTRAC", modalidad_delito) ~ "Sustracción de menores",
      modalidad_delito=="DELITOS AMBIENTALES" ~ str_to_sentence(modalidad_delito), 
      modalidad_delito=="MALTRATO ANIMAL" ~ str_to_sentence(modalidad_delito), 
      grepl("ALLANAMIENTO", modalidad_delito) ~ "Allanamiento de morada",
      grepl("PORNO", modalidad_delito) ~ "Pornografía",
      modalidad_delito=="COHECHO" ~ str_to_sentence(modalidad_delito), 
      modalidad_delito=="VIOLENCIA FAMILIAR" ~ str_to_sentence(modalidad_delito), 
      modalidad_delito=="TRATA DE PERSONAS" ~ str_to_sentence(modalidad_delito), 
      modalidad_delito=="CONTRA LA INTIMIDAD SEXUAL" ~ str_to_sentence(modalidad_delito),
      modalidad_delito=="TALA" ~ str_to_sentence(modalidad_delito),
      modalidad_delito=="SECUESTRO EXPRESS (PARA COMETER ROBO O EXTORSIÓN)" ~ "Secuestro express",
      modalidad_delito=="NARCOMENUDEO POSESION SIMPLE" ~ "Narcomenudeo simple",
      modalidad_delito=="NARCOMENUDEO POSESIÓN CON FINES DE VENTA, COMERCIO Y SUMINISTRO" ~ "Narcomenudeo venta",
      modalidad_delito=="TENTATIVA DE HOMICIDIO" ~ str_to_sentence(modalidad_delito),
      modalidad_delito=="ABUSO SEXUAL" ~ str_to_sentence(modalidad_delito),
      grepl("ACOSO SEXUAL", modalidad_delito) ~ "Acoso sexual",
      T ~ str_to_sentence(modalidad_delito)
    )) 
  
  return(data)
  
}

get_ci_rara <- function(base) {
  base <- 
    base %>% 
    mutate(
      ci = gsub("CI-|CI-E-", "", carpetainv),
      fiscalia_ini = str_split(ci, "/", simplify = T)[,1],
      agencia_ini = str_split(ci, "/", simplify = T)[,2],
      ultima =str_split(ci, "/", simplify = T)[,6],
      rara=ifelse(grepl("R|D", ultima), 1, 0)
    )
  
  return(base)
}
