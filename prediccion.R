###Crear modelo de predicción de delitos en alto impacto}
library(tidytext)
library(tm)
library(stringr)
library(tidyverse)


carpetas <- read_rds("C:/Users/mauri/OneDrive/Documentos/R/fiscalia/prediccion_delitos/alto_impacto.rds")

delito <- carpetas %>% 
  filter(delito == "HOMICIDIO DOLOSO")

Encoding(carpetas$delito) <- "latin1"
Encoding(carpetas$resumen_ap) <- "latin1"

carpetas <- carpetas %>% 
  mutate(resumenOK = stripWhitespace(removeNumbers(removePunctuation(tolower(resumen_ap)))))

carpetas <- carpetas %>% 
  mutate(resumenOK = stripWhitespace(removeWords(resumenOK, words = stopwords("spanish"))))

tmp <- list()
datos <- list()

for(i in 5:1) {
  tmp <- carpetas %>% select(delito, resumenOK) %>%  
    unnest_tokens(palabra, resumenOK, token = "ngrams", n = i)  %>%
    group_by(delito, palabra) %>% 
    summarise(total = n(), nivel = i)
  
  tmp <- tmp  %>% 
    mutate(porcentaje = total/max(total)) %>% 
    filter(porcentaje >= 0.05)
  
  datos[[i]] <- tmp
}


palabras_delito <- bind_rows(datos)
# palabras_delito = do.call(rbind, datos)
rm(datos, tmp)

palabras_delito <- palabras_delito %>% 
  ungroup() %>% 
  group_by(palabra) %>% 
  mutate(repetida=n(), 
         por_norm=porcentaje*nivel/repetida)

texto = "SE RECIBE UN FORMATO UNICO DE NOTIFICACION DE CASO MEDICO LEGAL DEL HOSPITAL PSIQUIATRICO INFANTIL DR. JUAN N. NAVARRO SUSCRITO Y FIRMADO POR EL MEDICO DR. EMMANUEL I. SARMIENTO HERNANDEZ EN DONDE SER REFIERE UNA POSIBLE AGRESION SEXUAL EN AGRAVIO DE LA MENOR DE IDENTIDAD RESERVADA CON INICIALES U.N.P.A. DE 13 AÑOS DE EDAD QUIEN SE ENCUENTRA EN HOSPITALIZACION."
texto = stripWhitespace(removeNumbers(removePunctuation(tolower(texto))))
texto = stripWhitespace(removeWords(texto, words = stopwords("spanish")))
# 
palabras_delito %>% 
  mutate(encontrado = str_detect(texto,palabra)) %>% 
  filter(encontrado == T) %>% 
  group_by(delito) %>% 
  summarise(valor = sum(por_norm)) %>% 
  arrange(desc(valor))


predecir <- function(texto=a){
  # texto = stripWhitespace(removeNumbers(removePunctuation(tolower(texto))))
  # texto = stripWhitespace(removeWords(texto, words = stopwords("spanish")))
  
  p <- palabras_delito %>% 
    filter(str_detect(texto,palabra)) %>% 
    #filter(encontrado == T) %>% 
    group_by(delito) %>% 
    summarise(valor = sum(porc_fabi)) %>% 
    arrange(desc(valor)) #%>% slice(1) %>% pull(delito)
  
  return(p)
}

predicciones <- list()
for (i in 1:nrow(carpetas)) {
  predicciones[[i]] <- predecir(carpetas$resumen_ap[i])
}



for (i in 1:5) {
  delito_predecir <- predecir(carpetas$resumen_ap[i])
}
total <- delito_predecir <- NULL
i <- 1
system.time( while (i<100) {
  delito_predecir <- predecir(carpetas$resumen_ap[i])
  total <- c(total, delito_predecir)
  i = i+1
})

class(predicciones[[1]])

i <- 1
while (i < 6) {
  print(i)
  i = i+1
}

carpetas_2022 <- carpetas %>% filter(fecha_inicio>="2022-01-01") %>% 
  mutate(delito_pre=predecir(palabras_delito =palabras_delito, texto = resumen_ap))



#ejemplo para hacer while con programación en paralelo
library(foreach)
library(parallel)
#setup parallel backend to use many processors
cores=detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
doParallel::registerDoParallel(cl)

finalMatrix <- foreach(i=1:150000, .combine=cbind) %dopar% {
  tempMatrix = mean(rnorm(100, mean = i, sd=1)) #calling a function
  #do other things if you want
  
  tempMatrix #Equivalent to finalMatrix = cbind(finalMatrix, tempMatrix)
}
#stop cluster
stopCluster(cl)

#nuestro cagadero
total <- delito_predecir <- NULL
i <- 1

prueba <- foreach(i=1:5) %dopar% {
  delito_predecir <- predecir(carpetas$resumen_ap[i])
  
  total <- c(total, delito_predecir)

}

system.time( while (i<100) {
  delito_predecir <- predecir(carpetas$resumen_ap[i])
  total <- c(total, delito_predecir)
  i = i+1
})



###doparallel
x <- iris[which(iris[,5] != "setosa"), c(1,5)]
trials <- 10000
ptime <- system.time({
   r <- foreach(icount(trials), .combine=cbind) %dopar% {
     ind <- sample(100, 100, replace=TRUE)
     result1 <- glm(x[ind,2]~x[ind,1], family=binomial(logit))
     coefficients(result1)
     }
   })[3]
ptime


total <- delito_predecir <- NULL
ptime <- system.time({
  r <- foreach(1:5, .combine=cbind) %dopar% {

    delito_predecir <- predecir(carpetas$resumen_ap[i])
    total <- c(total, delito_predecir)
  }
})[3]
ptime


carpetas_2022 <- carpetas %>% slice(1:20) %>% 
  mutate(delito_estimado=predecir(resumen_ap))


carpetas_2022 <- carpetas %>% filter(fecha_inicio>="2022-01-01")



base <- delito_predecir <- list()

for (i in 1:nrow(carpetas_2022)) {
  delito_predecir[[i]] <- predecir(carpetas_2022$resumen_ap[i])
  #total <- c(total, delito_predecir)
  
  base[[i]] <- tibble(id_ap=carpetas_2022$id_ap[i], 
                      delito_estimado=delito_predecir[[i]])
  
}


bases <- bind_rows(base)

palabras_delito <- read.csv("C:/Users/mauri/OneDrive/Documentos/R/fiscalia/prediccion_delitos/palabras_delito.csv")
Encoding(palabras_delito$delito) <- "latin1"
Encoding(bases$delito_estimado) <- "latin1"

palabras_delito <- palabras_delito %>% 
  mutate(porc_fabi=log(por_norm*total))
carpetas_estimados <- carpetas_2022 %>% 
  left_join(bases, by="id_ap") %>% drop_na(delito_estimado) %>% 
  mutate(atinado=ifelse(delito==delito_estimado, "Correcto", "Falso"))


resultados <- carpetas_estimados %>% 
  group_by(delito, atinado) %>% 
  summarise(Total=n()) %>% group_by(delito) %>% 
  mutate(porcentaje=Total/sum(Total))

base_fabi <- delito_predecir <- list()

carpetas_fabi <- carpetas_2022 %>% 
  filter(delito %in% c("ROBO A TRANSEUNTE EN VÍA PÚBLICA CON Y SIN VIOLENCIA", 
                       "ROBO A REPARTIDOR CON Y SIN VIOLENCIA"))

for (i in 1:nrow(carpetas_2022)) {
  delito_predecir[[i]] <- predecir(carpetas_2022$resumenOK[i])
  #total <- c(total, delito_predecir)
  
  base_fabi[[i]] <- tibble(id_ap=carpetas_2022$id_ap[i], 
                      delito_estimado=delito_predecir[[i]])
  
}


bases <- bind_rows(base_fabi)

# palabras_delito <- read.csv("C:/Users/mauri/OneDrive/Documentos/R/fiscalia/prediccion_delitos/palabras_delito.csv")
# Encoding(palabras_delito$delito) <- "latin1"
# Encoding(bases$delito_estimado) <- "latin1"


#moficar la formula del valorabra) &
           # grepl("bancaria", palabra))

palabras_delito2 <- palabras_delito %>% 
  mutate(policia=ifelse(grepl("elementos |policia", palabra), 1, 0), 
         bancaria=ifelse(grepl("bancari", palabra), 1, 0), 
         quitar=ifelse(policia==1 & bancaria==0, 1, 0))
palabras_delito <- palabras_delito2 %>% 
  filter(quitar==0)

palabras_delito <- palabras_delito %>% 
  mutate(porc_fabi=porcentaje*log(total)*nivel/repetida)


carpetas_estimados <- carpetas_2022 %>% 
  left_join(bases, by="id_ap") %>% drop_na(delito_estimado) %>% 
  mutate(atinado=ifelse(delito==delito_estimado, "Correcto", "Falso"))


resultados2 <- carpetas_estimados %>% 
  group_by(delito, atinado) %>% 
  summarise(Total=n()) %>% group_by(delito) %>% 
  mutate(porcentaje=Total/sum(Total))

###modificación de la función para que podamos predecir en programación paralela
predecir2 <- function(i){
  # texto = stripWhitespace(removeNumbers(removePunctuation(tolower(texto))))
  # texto = stripWhitespace(removeWords(texto, words = stopwords("spanish")))
  a <- carpetas$id_ap[i]
  b <- palabras_delito %>% 
    filter(str_detect(resumenes[i],palabra)) %>% 
    #filter(encontrado == T) %>% 
    group_by(delito) %>% 
    summarise(valor = sum(porc_fabi)) %>% 
    arrange(desc(valor)) %>% slice(1) %>% pull(delito)
  p <- tibble(id_ap=a, delito_estimado=b)
  
  return(p)
}

####ejemplo de hacer foreach 

library(doParallel)  
no_cores <- detectCores() - 1  
cl <- makeCluster(no_cores)  
registerDoParallel(cl)  
result <- foreach(i=10:10000) %dopar% getPrimeNumbers(i)


####sin programación paralela
delito_predecir <- c()
system.time(
for (i in 1:5000) {
  delito_predecir[[i]] <- predecir2(i)

  
}
)


####nuestro ciclo

library(doParallel)  
no_cores <- detectCores() - 1  
cl <- makeCluster(no_cores) 
clusterEvalQ(cl, {
  library(tidyverse)
  library(stringr)
})

registerDoParallel(cl)  

system.time(prueba_delito_est <- foreach(i=1:5000) %dopar% predecir2(i))

# system.time(prueba_delito_est_lap <- parLapply(cl, 10:5000,predecir2))

#para predecir 50 delitos, se tardó 7.11 segundos
#para predecir 5000 delitos, se tardó 29 segundos

# system.time(prueba <- foreach(i=10:10000) %dopar% getPrimeNumbers(i))
stopImplicitCluster()

bases <- bind_rows(prueba_delito_est)

remove(prueba_delito_est)


prueba_delito_est2 <- c()
system.time(
for (i in 1:5000) {
  prueba_delito_est2 <- predecir2(i)
}
)

####en 5000 se tardó 168.48