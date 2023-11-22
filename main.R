#Pregunta 3
install.packages("lubridate")
install.packages("tidyverse")
install.packages("stringr")
install.packages("dplyr")
install.packages("vetiver")
install.packages("lubridate")
#install.packages("factoextra")

if (!requireNamespace("writexl", quietly = TRUE)) {
  install.packages("writexl")
}


library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

#Leer el archivo epa http.cvs
library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(writexl)
library(ggplot2)

#Librerias kmeans
library(mltools)
library(data.table)




####################### PREGUNTA 1 INICIO #######################

epa_http <- read_table("epa-http.csv", col_names = FALSE)

#cambiar nombre de los columnas
colnames(epa_http)[1]<-"origen"
colnames(epa_http)[2]<-"dataTimeStamp"
colnames(epa_http)[3]<-"metodoHttp"
colnames(epa_http)[4]<-"uri"
colnames(epa_http)[5]<-"protocolo"
colnames(epa_http)[6]<-"respuestaHttp"
colnames(epa_http)[7]<-"bytes"
#Normalizar información de columnas:
epa_http$metodoHttp <- gsub('^"|"$', '', epa_http$metodoHttp)
epa_http$protocolo <- gsub('^"|"$', '', epa_http$protocolo)
epa_http$dataTimeStamp <- as.POSIXct(epa_http$dataTimeStamp,format="[%d:%H:%M:%S]",tz = "utc")  
epa_http$bytes <-  as.numeric(epa_http$bytes)
epa_http$bytes <-  epa_http$bytes <- ifelse(is.na(epa_http$bytes), 0, epa_http$bytes)
epa_http$origen <- str_trim(epa_http$origen)
View(epa_http)

# Validar si la columna es de tipo Date
if (all(is.Date(epa_http$dataTimeStamp))) {
  print("La columna dataTimeStamp es de tipo Date.")
} else {
  print("La columna dataTimeStamp no es de tipo Date.")
  print(class(epa_http$dataTimeStamp))
}

####################### PREGUNTA 1 FIN #######################

####################### PREGUNTA 2 INICIO #######################

## NORMALIZANDO la informaciòn de HREF, convirtiendo una URL Relativa a absoluta (tipo2)
epa_http <- epa_http %>%
  mutate(
    exitoso = case_when(
      stringr::str_detect(respuestaHttp, "^[123]") ~ TRUE,
      TRUE ~ FALSE
    )
  )

p1_df <- epa_http %>% group_by(origen,exitoso) %>% summarise(peticiones  = n()) 
View(p1_df)
####################### PREGUNTA 2 FIN #######################

####################### PREGUNTA 3 INICIO #######################

#Filtrar y agregar a una columna nueva si es archivo de imagen

epa_http <- epa_http %>%
  mutate(
    is_image = case_when(
      stringr::str_detect(uri, "(?i)\\.(jpg|jpeg|png|xbm|svg|gif|tiff|psd|bmp|eps|ps)$") ~ TRUE,
      TRUE ~ FALSE
    )
  )
p2_df <- epa_http %>% group_by(metodoHttp,is_image) %>% summarise(peticiones  = n()) 

write_xlsx(epa_http, "epa_http_data.xlsx")


####################### PREGUNTA 3 FIN #######################

####################### PREGUNTA 4 INICIO #######################



####################### PREGUNTA 4 FIN #######################


####################### PREGUNTA CLUSTERING INICIO 6 #######################

epa_http$exitoso <-factor(epa_http$exitoso)
epa_http$is_image <-factor(epa_http$is_image)
epa_http$metodoHttp<- factor(epa_http$metodoHttp)
epa_http$protocolo<- factor(epa_http$protocolo)

epa_http$num_letras <- nchar(epa_http$uri)
epa_http$num_letras <- NULL


epa_http_one_hot <- one_hot(epa_http, sparsifyNAs = TRUE)
epa_http_one_hot <- one_hot(as.data.table(epa_http), sparsifyNAs = TRUE)


columnas_a_mantener <- c("metodoHttp", "protocolo", "respuestaHttp","bytes","exitoso","is_image")
epa_http_new <- epa_http[, columnas_a_mantener, drop = FALSE]

epa_http_one_hot <- one_hot(as.data.table(epa_http_new), sparsifyNAs = TRUE)

names(epa_http_one_hot)

set.seed(1234)
kmeans <- kmeans(epa_http_one_hot, 2, iter.max = 10000, nstart = 1)

fviz_cluster(kmeans, data = epa_http_one_hot,geom = c("point","text"),
             ellipse = TRUE,
             ellipse.type = "convex",
             ellipse.level = 0.95,
             ellipse.alpha = 0.2)

