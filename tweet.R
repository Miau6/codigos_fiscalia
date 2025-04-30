#####tuiter
library(twitteR)
library(rtweet)
# 
# Sys.setenv(BEARER_TOKEN = "AAAAAAAAAAAAAAAAAAAAANcvgQEAAAAA%2BoyhaqtrhyT9zlXnX8r0Uur%2Bbdw%3DtN9kc0lQoiGQqILC0YTai6RegAO7PUprVDbEOqLNyg2cnwAfGW")


consumerKey <- "0uqOqGJMu2PO1nKVzNPDRoisQ"
consumerSecret = "GWwoQGj2VOdmqqdNO8fvp3RWsMYtL0TzGOey6f6EkdaecBxYNy"
accessToken = "1051665522700546048-UkoSK6Oixkkks44wGXVez8GVMrSUa6"
accessSecret = "vMVBg3IeAzLhjJM69GtnIAPMa1f3voKjm349TTwBfrskE"
appname <- "Lilini"

# setup_twitter_oauth(consumer_key = consumerKey, consumer_secret = consumerSecret,
#                     access_token = accessToken, access_secret = accessSecret)
# 
# ####con twitter
# tuits <- searchTwitter("from:@Miau Falcón", n=500, since = "2022-01-01",
#                        # resultType="popular"
# )
# 
# data <- tbl_df(map_df(tuits, as.data.frame))
# 



twiter_token <- create_token(
  app=appname, 
  consumer_key = consumerKey, 
  consumer_secret = consumerSecret, 
  access_token = accessToken, 
  access_secret = accessSecret
)

jess <- search_users("Diego Cocca")

tuits <- search_tweets(q="desabasto+medicinas", 
                       n=5000, include_rts = F)

tuits2 <- search_tweets(q="cerodesabasto", 
                       n=5000, include_rts = F)

library(tidytext)



###nube
##Nube
palabras_chat <- tuits %>%
  bind_rows(tuits2) %>% 
  #filter(created_at>="2022-08-26") %>% 
  filter(!duplicated(id_str)) %>% 
  select(id_str, text, created_at)%>%  
  unnest_tokens(word, token="sentences", text) 

# Stop Words
stop <- read.csv('C:/Users/luis_orduna/Documents/wordcloud/StopWords.csv')

my_stop_words <- tibble( 
  word = c("te", "cuál", "cuáles", "dónde", "denunciante", "refiere", 
           "querellante", "realizo", "denuncia", "o", "n", "d", "c", "recibe", "escrito", 
           "delito", "despojo", "da", "asi", "quien", "era", "os", "s", "dijo", "dije",
           "des", "mí", "à", "te", "tú", "tus", "tu", "00", "as", "ah", "ba", "or", "esto", 
           "iba", "muy", "mismo", "ten", "m", "quer", "vdc", "hab", "ba", 
           "multimedia", "omitido",
           "estás", "o", "no", "ni", "ese",
           "jeje", "jiji", "jajaja", "jajajaja", "jaja", 
           "qué", "sí", "no", "de",  "t.co", "y", "a", 
           "los", "que", "de", "t.co", "la", "en",
           "https", "hay", "han", "así", "qué", "vs",
           "montadeudas", "gil", "alcala", "alcalá", "julio", 
           "gonzález", "rt", "gil30alcala", "alcalá", "pumasmx",
           "tigres", "romeothecaster", "telcel", "cocaco", "caicedo", "gt", 
           "nico", "jose","dinenno", "pachuca", "soumayaamar", "rafadato2",
           "cocacolamx", "gignac", "vix", "q", "juandinenno", "danialvesd2",
           "soydepumas", "ugm8dmgfry", "cocca", "coca", "diego", 
           "diegococca1", "atlasfc", "atlas", "está", "gobernador",
           "está", "van", "alfaro", "enrique", "enriquealfaror",
           "yosoypedrero", "jgnaredo", "alvaro_delgado",
           as.character(seq(0, 200, by=1))))



stop <- stop %>% bind_rows(my_stop_words) 

# stop$word <- iconv(stop$word,from="LATIN1",to="ASCII//TRANSLIT")

palabras_agrup <- palabras_chat %>% filter(!word %in%stop$word) %>% 
  group_by(word) %>% summarise(Total=n()) %>% arrange(desc(Total)) %>% slice(1:100) %>%
  mutate(angle = 90 * sample(c(0, 1), n(), replace = TRUE, prob = c(60, 40)))
#left_join(lexico, by=c("word"="Palabra"))

library(ggwordcloud)

#con logo de pumas
grafica <- ggplot(palabras_agrup, aes(label=word, size=Total, #angle=angle,
                                      color=factor(sample.int(2, nrow(palabras_agrup), replace = TRUE)))) +
  scale_size_area(max_size = 30) +
  # geom_text_wordcloud(rm_outside = TRUE) + 
  geom_text_wordcloud_area(
    mask = png::readPNG("C:/Users/luis_orduna/Pictures/pumas_bn.png",
    ),
    rm_outside = TRUE) +
  theme_minimal() +
  scale_color_manual(values = c("#112147", "#c9ab5a"))


#con algunas forma
grafica <- ggplot(palabras_agrup, aes(label=word, size=Total, #angle=angle,
                                      color=factor(sample.int(2, nrow(palabras_agrup), replace = TRUE)))) +
  scale_size_area(max_size = 10) +
  # geom_text_wordcloud(rm_outside = TRUE) + 
  geom_text_wordcloud_area(
    rm_outside = T) +
  theme_bw() +
  scale_color_manual(values = c("#9843ff", "#fb0a3f")) #+
  # labs(caption = "Elaborado por miau", 
  #      title = "Tuits en que mencionaron a Enrique Álfaro", 
  #      subtitle = "26 de agosto de 2022")

ggsave(plot = grafica,dpi = 400, width = 9, height = 9,
       "C:/Users/luis_orduna/Downloads/wordcloud_nosotrxs2.png")  

sentimental <- read.csv("C:/Users/luis_orduna/Downloads/Telegram Desktop/lexico_afinn.en.es.csv")

library(scales)
gr_sent <- palabras_chat %>% filter(!word %in%stop$word) %>% 
  group_by(word) %>% summarise(Total=n()) %>% arrange(desc(Total)) %>% slice(1:500) %>% 
  left_join(sentimental, by=c("word"="Palabra")) %>% drop_na() %>% 
  mutate(efecto=Puntuacion*Total, 
         signo=ifelse(efecto>0, "positivo", "negativo"), 
         efecto=abs(efecto)) %>% group_by(signo) %>% 
  summarise(Total=sum(Total)) %>% 
  mutate(porcentaje=Total/sum(Total)) %>% 
  ggplot(aes(x=signo, y=porcentaje, fill=signo)) + 
  geom_bar(stat = "identity") +
  labs(x="sentimientos"#, title = "Mensajes positivos o negativos - Enrique Álfaro"
       ) +
  theme_bw() +
  scale_y_continuous(labels = percent) +
  theme(legend.position = "none") +
  geom_label(aes(label=percent(porcentaje, accuracy = .01)), 
             fill="ghostwhite")
ggsave(plot = gr_sent, #width = 6, height = 6, 
       "C:/Users/luis_orduna/Downloads/sentimiento_alfaro.png")  

gr_sent_ts <- palabras_chat %>% filter(!word %in%stop$word) %>% 
  group_by(word, created_at) %>% summarise(Total=n()) %>% arrange(desc(Total)) %>% #slice(1:500) %>% 
  left_join(sentimental, by=c("word"="Palabra")) %>% drop_na() %>% 
  mutate(efecto=Puntuacion*Total, 
         signo=ifelse(efecto>0, "positivo", "negativo"), 
         efecto=abs(efecto), 
         tiempo=ymd_hms(created_at), 
         hora=hour(tiempo), 
         minuto=minute(tiempo), 
         tiempo_t=hm(paste0(hora, ":",minuto))
         ) %>% group_by(signo,  tiempo_t=as.numeric(tiempo_t)) %>% 
  filter(hora>9) %>% 
  summarise(Total=sum(Total), .groups = "drop") %>% 

  #mutate(porcentaje=Total/sum(Total)) %>% 
  ggplot(aes(x=tiempo_t, y=Total, colour=signo)) + 
  facet_wrap(.~signo, scales = "free_x", ncol = 1)+
  geom_line() + geom_point(alpha=.3) +
  labs(x="Hora", title = "Mensajes positivos o negativos (serie) - Enrique Álfaro") +
  theme_bw() +
  scale_x_time() +
  theme(legend.position = "none") 
ggsave(plot = gr_sent, #width = 6, height = 6, 
       "C:/Users/luis_orduna/Downloads/sentimiento_alfaro.png")  


source("Y:/3 CODIGOS R (Version 2)/reportes/source/scr_1.R", encoding = "UTF-8")


victimas <- arma_base2("victimas", "Y:/1 BASES UET/Carpetas iniciadas/", "abierto")

base <- readxl::read_excel("C:/Users/luis_orduna/Downloads/base765_v2.xlsx", 
                           sheet="BASE")


data <- base %>% 
  left_join(carpetas %>% select(id_ap,ci_formato_siap_fsiap),
            by=c("Carpeta de Investigación"="ci_formato_siap_fsiap")) %>% 
  left_join(carpetas %>% select(id_ap,ap),
            by=c("Carpeta de Investigación"="ap"))


data_tuits <- data_tuits %>% 
  mutate(tipo=case_when(grepl("iphone", source) ~ "iphone", 
                        grepl("android", source) ~ "android", 
                        grepl("Web App", source) ~ "web app", 
                        T ~ "escritorio"
         ))



gr_sent_ts <- palabras_chat %>% filter(!word %in%stop$word) %>% 
  group_by(word, created_at) %>% summarise(Total=n()) %>% arrange(desc(Total)) %>% #slice(1:500) %>% 
  left_join(sentimental, by=c("word"="Palabra")) %>% drop_na() %>% 
  mutate(efecto=Puntuacion*Total, 
         signo=ifelse(efecto>0, "positivo", "negativo"), 
         efecto=abs(efecto), 
         fecha=as_date(created_at)
  ) %>% group_by(signo,  fecha) %>% 
  #filter(hora>9) %>% 
  summarise(Total=sum(Total), .groups = "drop") %>% 
  
  #mutate(porcentaje=Total/sum(Total)) %>% 
  ggplot(aes(x=fecha, y=Total, colour=signo)) + 
  facet_wrap(.~signo, scales = "free_x", ncol = 1)+
  geom_line() + geom_point(alpha=.3) +
  #labs(x="Hora", title = "Mensajes positivos o negativos (serie) - Enrique Álfaro") +
  theme_bw() +
  #scale_x_time() +
  theme(legend.position = "none") 
ggsave(plot = gr_sent, #width = 6, height = 6, 
       "C:/Users/luis_orduna/Downloads/sentimiento_alfaro.png")  
