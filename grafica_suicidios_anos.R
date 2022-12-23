library(tidyverse)

#descargamos la tabla del INE de los suicidos por años
fs::dir_create("pruebas") #- primero creamos la carpeta "pruebas"
df_suicidios2 <- rio::import("./pruebas/datos_suicidios_2.csv")

#ahora vamos a arreglar los datos
names(df_suicidios2)
df_ss2 <- janitor::clean_names(df_suicidios2) 
names(df_ss2)
df_ss3 <- df_ss2 %>% select(-c("periodo")) %>% slice(c(21:42))

#lo vemos
library(ggplot2)
library(ggthemes)


ggplot(df_ss3, aes(x = ano , y = total)) +  geom_point(color="dark blue") + geom_line(color="orange") + labs(title="Suicidios por años",subtitle = "(Datos tomados en miles)" ,caption="Fuente: Epdata", x="Años", y="Total") +  theme_economist()


