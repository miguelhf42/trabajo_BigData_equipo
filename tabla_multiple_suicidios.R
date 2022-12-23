library(tidyverse)

#descargamos la tabla del INE de los suicidos 
fs::dir_create("pruebas") #- primero creamos la carpeta "pruebas"
my_url <- "https://ine.es/jaxi/files/tpx/es/csv_bd/49956.csv?nocab=1"
curl::curl_download(my_url, "./pruebas/suicidios.csv")
df_suicidios <- rio::import("./pruebas/suicidios.csv")

#ahora vamos a arreglar los datos
names(df_suicidios)
df_ss <- janitor::clean_names(df_suicidios) 
names(df_ss)
df_ss <- df_ss %>% dplyr::rename(numero_total_suicidios_por_comunidad = total)

# seleccionamos filters
suicidios_mod <- df_ss %>% filter(edad =="Todas las edades") %>% select(-nacional) %>% arrange(desc(numero_total_suicidios_por_comunidad)) 

suicidios_mod_1 <- suicidios_mod %>% slice(-c(45:46, 57,59:61))



# hacemos las gráficas
graficas_comunidades <- ggplot(suicidios_mod_1, aes(x =numero_total_suicidios_por_comunidad , y= comunidades_y_ciudades_autonomas)) +  geom_bar(stat = "identity" , fill = "blue") +  facet_wrap(vars(sexo)) +  theme_light() 

graficas_comunidades


# resaltamos la Comunidad Valenciana
comunidad_valenciana <- suicidios_mod_1 %>% filter(comunidades_y_ciudades_autonomas %in% c("Comunitat Valenciana"))


graficas_comunidad_valenciana <- graficas_comunidades + geom_bar(data = comunidad_valenciana, aes(x = numero_total_suicidios_por_comunidad, y = comunidades_y_ciudades_autonomas), stat = "identity", fill = "orange") + labs(title = "Suicidios en la pandemia", caption = "Fuente: INE",x="Número de suicidios", y="Comunidades autónomas")

graficas_comunidad_valenciana


