#1. Carga de paquetes y WD ----
rm(list = ls())

library(DataEditR)
library(tidyverse) #es un universo de paquetes que integra ggplot, stringR, dplyr, tidyR(LONG-WIDE)
library(readxl) #paquete para leer datos de excel
library(sf)
library(magrittr)
library(mapview)
library(printr)
library(shiny)
library(lubridate) #sirve para manipular y darle un data.type a las fechas.
library(ggrepel) #mejorar el etiquetado de los datos en un gr?fico
#install.packages("printr")
#install.packages("googledrive")
setwd("C:/Users/56989/Desktop/Github/GranitosdeMostaza.github.io")
getwd() #espacio de trabajo actualizado
list.files()
#https://docs.google.com/spreadsheets/d/e/2PACX-1vQawLKRruyLNHg1LSMjfxt0siwuj9ng_wxQlDM-a3aWUD1Z4LiVIP9E8lfrvAywQQUaluxVGFMlZhx5/pub?gid=1371277324&single=true&output=csv
myurl <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQawLKRruyLNHg1LSMjfxt0siwuj9ng_wxQlDM-a3aWUD1Z4LiVIP9E8lfrvAywQQUaluxVGFMlZhx5/pub?gid=1371277324&single=true&output=csv"
wti <- read_csv(url(myurl))
head(wti) #primeras 6 filas
tail(wti) #ultimas 6 filas

data_1 <- wti

#2. Lectura e importacion de datos ----

#sheets <- readxl::excel_sheets("CATASTRO INTERACTIVO.xlsx")
#data_1 <- readxl::read_excel("CATASTRO INTERACTIVO.xlsx", sheets[1])
#data_2 <- readxl::read_excel("CATASTRO INTERACTIVO.xlsx", sheets[2])
#data_3 <- readxl::read_excel("CATASTRO INTERACTIVO.xlsx", sheets[3])

#3. Chequeo de datos ----
summary(data_1)
glimpse(data_1)

#4. coercion de coordenadas LONGITUD y LATITUD y transformacion a sf

# las coordenadas geograficas vienen con errores de formato
# para arreglarlas, extraje los primeros 3 valores,
# y lo pegue (o concatene) con los valores de la 4ta posici?n al ultimo, con una coma al medio
# esto para colocar el punto decimal donde corresponde.

#las coordenadas buenas estan en las columnas lon_x y lat_y.
data_1 %<>%
  mutate(lon_x = as.double(paste0(str_sub(LONGITUD, start = 1, end = 3), ".",
                                  str_sub(LONGITUD, start = 4))),
         lat_y = as.double(paste0(str_sub(LATITUD, start = 1, end = 3), ".",
                                  str_sub(LATITUD, start = 4))),
         X_UTM = as.double(paste0(str_sub(UTM_X, start = 1, end = 6), ".",
                                  str_sub(UTM_X, start = 7))),
         Y_UTM = as.double(paste0(str_sub(UTM_Y, start = 1, end = 7), ".",
                                  str_sub(UTM_Y, start = 8))))
data_1 <- data_1 %>% 
  rename(ID_FAMILIA = ID,
         AYUDA_RECIBIDA = AYUDA_ENTREGADA,
         AYUDA_RECIBIDA2 = AYUDA_ENTREGADA2,
         AYUDA_RECIBIDA3 = AYUDA_ENTREGADA3,
         AYUDA_RECIBIDA4 = AYUDA_ENTREGADA4,
         AYUDA_RECIBIDA5 = AYUDA_ENTREGADA5)
colnames(data_1)
glimpse(data_1)
str(data_1)
View(data_1)
class(data_1$INTEGRANTES_GRUPO_FAMILIAR)
data_1 <- data_1 %>% mutate(INTEGRANTES_GRUPO_FAMILIAR = as.integer(INTEGRANTES_GRUPO_FAMILIAR))
class(data_1$INTEGRANTES_GRUPO_FAMILIAR)
is.integer(data_1$INTEGRANTES_GRUPO_FAMILIAR)

#4.1. Editar con el paquete DataEditR

granitos_logo <- "Capturas/granitos.jpg"

choferes <- data_edit(data_1,
          col_bind = "CHOFER_DISTRIBUIDOR5",
          col_options = list(CHOFER_DISTRIBUIDOR5 = c("Bryan",
                                                      "Camila",
                                                      "Fidel",
                                                      "Samy",
                                                      "Shirley",
                                                      "Padre",
                                                      "Andrea")),
          logo = granitos_logo,
          #logo_size = 100,
          title = "Granitos de Mostaza",
          save_as = "data_edit.csv",
          write_fun = "write.table",
          write_args = list(sep = ",", row.names = FALSE))
#Exportar base de datos data_1
write.csv(data_1, file = "FAMILIAS_agosto.csv", row.names = F)
##3.1. Google Maps----
familias_apoyadas <- data_1 %>% filter(!is.na(CHOFER_DISTRIBUIDOR4)) %>% 
  select(DESCRIPCION, ID_FAMILIA, COMUNA, INTEGRANTES_GRUPO_FAMILIAR, NACIONALIDAD, AYUDA_RECIBIDA, FECHA_AYUDA,
         AYUDA_RECIBIDA2, FECHA_AYUDA2, AYUDA_RECIBIDA3, FECHA_AYUDA3, AYUDA_RECIBIDA4, FECHA_AYUDA4,
         AYUDA_RECIBIDA5, FECHA_AYUDA5, lon_x, lat_y) %>%
  view()
count(familias_apoyadas) #116

#Exportar base de datos data_1
write.csv(familias_apoyadas, file = "Gmaps/FAMILIAS_Septiembre_GMAPS.csv", row.names = F)
#3.2. Informacion para choferes
familias <- data_1 %>% filter(!is.na(CHOFER_DISTRIBUIDOR4)) %>% 
  select(ID_FAMILIA, NOMBRE, DIRECCION, COMUNA, TELEFONO, INTEGRANTES_GRUPO_FAMILIAR, NACIONALIDAD, 
         OBSERVACIONES, CHOFER_DISTRIBUIDOR3, CHOFER_DISTRIBUIDOR4, AYUDA_RECIBIDA4,
         PERSONA_QUE_AGREGO, lon_x, lat_y) %>%
  view()
glimpse(familias)
count(familias) #122
#Exportar base de datos data_1
write.csv(familias, file = "FAMILIAS_APOYADAS_1_agosto.csv", row.names = F)
#BRYAN
bryan4 <- familias %>% filter(CHOFER_DISTRIBUIDOR4 == "Bryan") %>% view()
write.csv(bryan4, file = "BRYAN4.csv", row.names = F)
#CAMILA
camila4 <- familias %>% filter(CHOFER_DISTRIBUIDOR4 == "Camila") %>% view()
write.csv(camila4, file = "CAMILA4.csv", row.names = F)
#FIDEL
fidel4 <- familias %>% filter(CHOFER_DISTRIBUIDOR4 == "Fidel") %>% view()
write.csv(fidel4, file = "FIDEL4.csv", row.names = F)
#MIGUEL
miguel4 <- familias %>% filter(CHOFER_DISTRIBUIDOR4 == "Miguel") %>% view()
write.csv(miguel4, file = "MIGUEL4.csv", row.names = F)
#SAMY
samy4 <- familias %>% filter(CHOFER_DISTRIBUIDOR4 == "Samy") %>% view()
write.csv(samy4, file = "SAMY4.csv", row.names = F)
#SHIRLEY
shirley4 <- familias %>% filter(CHOFER_DISTRIBUIDOR4 == "Shirley") %>% view()
write.csv(shirley4, file = "SHIRLEY4.csv", row.names = F)
#PADRE
padre4 <- familias %>% filter(CHOFER_DISTRIBUIDOR4 == "Padre") %>% view()
write.csv(padre4, file = "PADRE4.csv", row.names = F)
#ANDREA
andrea4 <- familias %>% filter(CHOFER_DISTRIBUIDOR4 == "Andrea") %>% view()
write.csv(andrea4, file = "ANDREA4.csv", row.names = F)
#Exportar base de datos data_1
write.csv(familias, file = "FAMILIAS_APOYADAS_agosto.csv", row.names = F)

colnames(familias_apoyadas)
sum(is.na(familias_apoyadas$INTEGRANTES_GRUPO_FAMILIAR)) #0 familias que no se conoce el n integrantes

#4. Estadisticas#####----
#Promedio de integrantes por familia (3,969), Min: 1 Max:9
select_not_NA <- data_1 %>% 
  filter(!is.na(INTEGRANTES_GRUPO_FAMILIAR))
summary(select_not_NA$INTEGRANTES_GRUPO_FAMILIAR)
#Familias ayudadas por comuna
datos_por_comuna <- data_1 %>% 
  count(COMUNA) %>% 
  view()
NUM_COMUNAS <- unique(data_1$COMUNA) %>% length %>% print()
#Familias por tipo de Ayuda
familias_segun_ayuda <- data_1 %>% filter(!is.na(AYUDA_RECIBIDA4)) %>% 
  count(AYUDA_RECIBIDA4) %>% 
  view()
#Familias por chofer
unique(familias$CHOFER_DISTRIBUIDOR4)
choferes_descartados <- c("Cesar","Kathya","SAN ANDRES")
chofer_familias <- familias %>% filter(!(CHOFER_DISTRIBUIDOR4 %in% choferes_descartados)) %>% 
  count(CHOFER_DISTRIBUIDOR4) %>% rename(n_familias = n) %>% 
  view()
#Familias Bryan
bryan <- familias_apoyadas %>% 
  filter(CHOFER_DISTRIBUIDOR3 == "Bryan") %>% select(-c(9:13), -16, -17) %>%  
  view()
write.csv(bryan, file = "Bryan.csv", row.names = F)
#Familias Camila
Camila <- familias_apoyadas %>% 
  filter(CHOFER_DISTRIBUIDOR3 == "Camila") %>% select(-c(9:13), -16, -17) %>%  
  view()
write.csv(Camila, file = "Camila.csv", row.names = F)
#Familias Fidel
Fidel <- familias_apoyadas %>% 
  filter(CHOFER_DISTRIBUIDOR3 == "Fidel") %>% select(-c(9:13), -16, -17) %>%  
  view()
write.csv(Fidel, file = "Fidel.csv", row.names = F)
#Familias Miguel
Miguel <- familias_apoyadas %>% 
  filter(CHOFER_DISTRIBUIDOR3 == "Miguel") %>% select(-c(9:13), -16, -17) %>%  
  view()
write.csv(Miguel, file = "Miguel.csv", row.names = F)
#Familias Samy
Samy <- familias_apoyadas %>% 
  filter(CHOFER_DISTRIBUIDOR3 == "Samy") %>% select(-c(9:13), -16, -17) %>%  
  view()
write.csv(Samy, file = "Samy.csv", row.names = F)
#Familias Shirley
Shirley <- familias_apoyadas %>% 
  filter(CHOFER_DISTRIBUIDOR3 == "Shirley") %>% select(-c(9:13), -16, -17) %>%  
  view()
write.csv(Shirley, file = "Shirley.csv", row.names = F)
#st_drop_geometry() para sacar los datos geometricos de una clase sf
#Familias ayudadas por COMUNA y NACIONALIDAD
datos_nacionalidad <- data_1 %>% 
  count(COMUNA, NACIONALIDAD) %>% 
  view()

#ESTADISTICAS DE LAS PERSONAS BENEFICIADAS EN LO PRADO SEGUN NACIONALIDAD
familias_apoyadas_LP2 <- familias_apoyadas %>%
  group_by(COMUNA = "Lo Prado", NACIONALIDAD) %>% 
  summarise(Personas_beneficiadas = sum(INTEGRANTES_GRUPO_FAMILIAR, na.rm= T)) %>%
  #summarise(n_casos = n()) %>% 
  ungroup() %>% 
  view()
#FAMILIAS APOYADAS POR COMUNA Y FECHA
familias_apoyadas <- familias_apoyadas %>% mutate(date = dmy(DATE))
names(familias_apoyadas)
class(familias_apoyadas$date)
datos_nacionalidad <- familias_apoyadas %>% 
  count(COMUNA, date) %>% arrange(desc(date)) %>% 
  view()
#plot de entrega de ayuda a familias
(c <- familias_apoyadas %>% 
    group_by(date) %>% 
    summarise(n = n()) %>%  
    ggplot(aes(date, n)) +
    geom_line(colour ="#00CD66") +
    geom_point(colour = "#008B45") +
    xlab(NULL) +
    ylab("Número de familias \n") +
    labs(title = "Total",
         subtitle = paste0("Fecha actualización: ", Sys.Date()-1)) +
    scale_y_continuous(breaks = seq(0, 50, 10))+
    theme_minimal() +
    theme(legend.title = element_blank()))

#Familias ayudadas por fecha
str(familias_apoyadas)
familias <- familias_apoyadas %>%
  mutate(date = dmy(DATE))
class(familias_apoyadas$date)

(a <- familias %>% 
    group_by(date) %>% 
    summarise(n = n()) %>% 
    mutate(nfam = cumsum(n)) %>% 
    ggplot(aes(date, nfam)) +
    geom_line(colour ="#00CD66") +
    geom_point(colour = "#008B45") +
    xlab(NULL) +
    ylab("Número de familias \n") +
    labs(title = "Cantidad de familias ayudadas",
         subtitle = paste0("Fecha actualización: ", Sys.Date()-1)) +
    #scale_x_date(date_breaks = "3 days") +
    scale_y_continuous(breaks = seq(0, 200, 20))+
    theme_minimal() +
    theme(legend.title = element_blank()))

ggsave(paste("familias_ayudadas_21_junio",".jpeg", sep = ""),
       plot = a, width = 9.69, height = 6.55, units = c("in"), dpi = 500)

library(plotly)
pp1 <- ggplotly(a)

htmlwidgets::saveWidget(as_widget(pp1),"mi_grafico.html")

b <- familias %>% group_by(date) %>% summarise(n =n()) %>% mutate(nfam = cumsum(n)) %>% view()

#SACAR ESTADISTICAS DE LAS FAMILIAS AYUDADAS POR COMUNA
familias_apoyadas_comuna <- familias_apoyadas %>%
  group_by(COMUNA) %>% 
  #group_by(COMUNA, NACIONALIDAD, AYUDA_RECIBIDA, INTEGRANTES_GRUPO_FAMILIAR) %>% 
  #summarise(familias_ayudadas = sum(COMUNA), na.rm=T) %>%
  summarise(familias_ayudadas = n()) %>% 
  ungroup() %>% 
  view()

#funci?n para sacar sumario en word
LP2 <- summary(familias_apoyadas_LP2)
capture.output(LP2, file = "Sumario_LP2.doc")

#5. Transformación a shp----
#Aqui transformo el objeto data_1 (data.frame) a un ojeto sf, que se utiliza para guardar informacion geoespacial
# es como un shape pero en R
data_1_sf <- st_as_sf(data_1,
                      coords = c("lon_x", "lat_y"),
                      crs = 4326)

# Con esto exportas el objeto sf a un shapefile externo
st_write(obj = data_1_sf, dsn = "data_1_sf.shp", driver = "ESRI Shapefile", delete_dsn = T)

st_write(obj = data_1_sf,
         dsn = "data_1_sf.kml",
         driver = "KML",
         delete_layer = T)

#5. Visualizacion
ggplot() +
  geom_sf(data = data_1_sf) 

mapview(data_1_sf)

ggplot(data_1_sf) +
  geom_sf(aes(fill = AYUDA_SOLICITADA3), colour = "white", alpha = 0.75)+
  #scale_fill_viridis_d(option = "inferno") + #para escalas discretas
  theme_minimal()

#colour. Modifica el color de borde de las entidades espaciales.
#fill. Agrega color de relleno a las entidades espaciales (punto línea o polígono). 
#Puede usarse dentro de aes() para vincular los valores de una variable a la paleta de colores.
#variables discretas o categóricas: scale_fill_viridis_d(),
#variables continuas: scale_fill_viridis_c(),
##Crear intervalos para una variable continua: scale_fill_viridis_b().
