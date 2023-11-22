#Pregunta 3
install.packages("lubridate")
install.packages("tidyverse")
install.packages("stringr")
install.packages("dplyr")
install.packages("vetiver")
install.packages("lubridate")
install.packages("factoextra")
install.packages("mltools")

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
p2_df


#write_xlsx(epa_http, "epa_http_data.xlsx")


####################### PREGUNTA 3 FIN #######################

####################### PREGUNTA 4 INICIO #######################
####################### PREGUNTA 4 INICIO #######################

### Gráfico que muestra la cantidad de HTTPStatus diferenciando si son imagenes o no 

ggplot(data = epa_http, aes(x = is_image, fill = factor(respuestaHttp))) +
  geom_bar(color = "black", position = "stack", alpha = 0.7) +
  scale_fill_manual(values = c("green", "blue", "purple","red","pink", "orange", "black", "purple")) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, position = position_stack(vjust = 0.5)) +
  labs(title = "HTTP Status Code (Request con Imagenes)", x = "Es Imagen (True / False)", y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotar etiquetas en el eje x
  facet_wrap(~respuestaHttp, scales = "free_y")  # 


ggplot(epa_http, aes(x = factor(respuestaHttp))) +
  geom_bar(stat = "count", fill = "blue", color = "green") +
  pie("y") +  # Convertir el gráfico en un gráfico de torta
  labs(title = "Distribución de responseCode", x = "Response Code") +
  theme_minimal()

## Porcentajes de distribución

pie_chart <- PieChart(
  respuestaHttp,
  hole = 0.8,
  values = "%",
  data = epa_http,
  fill = c("green", "blue", "purple", "red", "pink", "orange", "black"),  # Corregir aquí
  main = "Porcentajes de Status Code",
  percent_label_color = "black", 
  show_legend = FALSE  
)


### Gráfico que muestra la cantidad de HTTPStatus diferenciando si son exitosos o no.

ggplot(data = epa_http, aes(x = exitoso, fill = factor(metodoHttp))) +
  geom_bar(color = "black", position = "stack", alpha = 0.7) +
  scale_fill_manual(values = c("green", "blue", "purple")) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, position = position_stack(vjust = 0.5)) +
  labs(title = "Métodos HTTP", x = "Es Exitoso (True / False)", y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotar etiquetas en el eje x
  facet_wrap(~metodoHttp, scales = "free_y")  #



# Crear una nueva columna para el día
epa_http <- epa_http %>%
  mutate(dia_hora = format(dataTimeStamp, "%Y-%m-%d %H:00"))

# Agrupar por la nueva columna
epa_agrupado <- epa_http %>%
  group_by(dia_hora,respuestaHttp) %>%
  summarise(
    conteo_exitoso_ = sum(exitoso == TRUE),
    conteo_noexitoso = sum(exitoso == FALSE)
  )





####################### PREGUNTA CLUSTERING INICIO 6 #######################

epa_http$exitoso <-factor(epa_http$exitoso)
epa_http$is_image <-factor(epa_http$is_image)
epa_http$metodoHttp<- factor(epa_http$metodoHttp)
epa_http$protocolo<- factor(epa_http$protocolo)
epa_http$respuestaHttp<- factor(epa_http$respuestaHttp)

epa_http$num_letras <- nchar(epa_http$uri)
epa_http$num_letras <- NULL


#epa_http_one_hot <- one_hot(epa_http, sparsifyNAs = TRUE)
epa_http_one_hot <- one_hot(as.data.table(epa_http), sparsifyNAs = TRUE)


columnas_a_mantener <- c("metodoHttp", "protocolo", "respuestaHttp","bytes","exitoso","is_image")
epa_http_new <- epa_http[, columnas_a_mantener, drop = FALSE]

epa_http_one_hot <- one_hot(as.data.table(epa_http_new), sparsifyNAs = TRUE)

names(epa_http_one_hot)

set.seed(1234)
kmeans1 <- kmeans(epa_http_one_hot, 3, iter.max = 100, nstart = 1)
kmeans2 <- kmeans(epa_http_one_hot, 7, iter.max = 100, nstart = 1)
#printing
kmeans1
kmeans2

epa_http_kmeans <- epa_http
epa_http_kmeans$kmean <- kmeans1$cluster

epa_http_kmeans
##Pregunta 7
fviz_cluster(kmeans1, data = epa_http_one_hot,ggtheme = theme_minimal(),show.clust.cent = TRUE,
             main = "Pregunta KMeans",
             ellipse = TRUE,
             axes = c(1, 2),
             ellipse.type = "convex",
             ellipse.level = 0.95,
             ellipse.alpha = 0.2)

fviz_cluster(kmeans2, data = epa_http_one_hot,ggtheme = theme_minimal(),show.clust.cent = TRUE,
             main = "Pregunta KMeans",
             ellipse = TRUE,
             #axes = c(6, 10),
             axes = c(1, 2),
             ellipse.type = "convex",
             ellipse.level = 0.95,
             ellipse.alpha = 0.2)


