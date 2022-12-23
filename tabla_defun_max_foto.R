fs::dir_create("pruebas") #- primero creamos la carpeta "pruebas"
my_url <- "https://ine.es/jaxi/files/tpx/es/csv_bd/49870.csv?nocab=1"
curl::curl_download(my_url, "./pruebas/causadefun.csv")
causadefun <- rio::import("./pruebas/causadefun.csv")

library(tidyverse)
library(gt)
names(causadefun)
a<- causadefun %>% filter(Edad=="Total", Sexo=="Total")
a$'Covid-19'[2] <- "covid"
a$'Covid-19'[6] <- "covid"
a$'Covid-19'[10] <- "covid"
a$'Covid-19'[14] <- "covid"
a$'Covid-19'[18] <- "covid"
a$'Covid-19'[22] <- "covid"
a$'Covid-19'[26] <- "covid"
a$'Covid-19'[30] <- "covid"
a$'Covid-19'[34] <- "covid"
a$'Covid-19'[38] <- "covid"
a$'Covid-19'[42] <- "covid"
a$'Covid-19'[46] <- "covid"
a$'Covid-19'[50] <- "covid"
a$'Covid-19'[54] <- "covid"
a$'Covid-19'[58] <- "covid"
a$'Covid-19'[62] <- "covid"
a$'Covid-19'[66] <- "covid"
a$'Covid-19'[70] <- "covid"
a$'Covid-19'[74] <- "covid"
a$'Covid-19'[78] <- "covid"
colnames(a)[3] <- "Covid"
a_b <-a %>% filter(Covid=="covid") %>% slice(c(2:20)) 
datos_ordenados <- arrange(a, desc(Total))


colnames(a_b)[2] <- "Comunidades autónomas"
names(a_b)
tabla_def <- a_b %>% select('Comunidades autónomas', Total)

tabla_max <- tabla_def %>% slice(13) 
tabla_max$'Comunidades autónomas'[1] <- "Comunidad de Madrid"

Bandera <- "https://todosobremadrid.com/wp-content/uploads/2016/06/01-Bandera-CAM.jpg"

dftop1 <- tabla_max %>% add_column(Bandera)

Tabla_Pmascal <- dftop1 %>% gt()

Tabla_Pmascal <- Tabla_Pmascal %>%
  tab_header(title = md("**Comunidad con más defunciones (Covid)**"))

Tabla_Pmascal <- Tabla_Pmascal %>%
  tab_options(heading.background.color = "#df1c44") %>% tab_options(heading.title.font.size = 15,  column_labels.font.weight =  "bold")


Tabla_Pmascal <- Tabla_Pmascal  %>%
  gt::text_transform(locations = cells_body(columns = c(Bandera)), fn = function(x) {gt::web_image(x, height = 50)}) %>%  cols_align(
    align = "center")

Tabla_Pmascal



