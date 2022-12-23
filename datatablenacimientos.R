library(tidyverse)
my_url <- "https://www.ine.es/jaxiT3/files/t/es/csv_bd/46678.csv?nocab=1"
curl::curl_download(my_url, "./pruebas/nacimientos.csv")
nacimientos <- rio::import("./pruebas/nacimientos.csv")
nacimientos <- janitor::clean_names(nacimientos)
names(nacimientos)
zz <- pjpv.curso.R.2022::pjp_estadisticos_basicos(nacimientos)
zz <- pjpv.curso.R.2022::pjp_valores_unicos(nacimientos)

#- 
nacimientos <- nacimientos %>% 
  mutate(total = parse_number(total, locale = locale(decimal_mark = "," , grouping_mark = ".")), .after = total) %>% 
  mutate(total = as.numeric(total))


#- 
nacimientos <- nacimientos %>% rename(CCAA = comunidades_y_ciudades_autonomas)
nacimientos <- nacimientos %>% mutate(CCAA = ifelse(CCAA == "", "00 Nacional", CCAA)) %>% 
  select(-total_nacional)

#- 
nacimientos <- nacimientos %>% separate(CCAA, sep = " ", into = c("ine_ccaa", "ine.ccaa.n"), extra = "merge")

nacimientos <- nacimientos %>% 
  mutate(fecha = lubridate::ym(periodo), .after = periodo) %>% 
  mutate(year = lubridate::year(fecha), .after = fecha) %>% 
  mutate(mes = lubridate::month(fecha), .after = year) %>%
  select(-periodo)

zz <- pjpv.curso.R.2022::pjp_valores_unicos(nacimientos)


nacimientos <- nacimientos %>% mutate(tipo_de_dato = case_when(
  tipo_de_dato == "Dato base" ~ "numero",
  tipo_de_dato == "Acumulado en lo que va de año" ~ "numero_acu",
  tipo_de_dato == "Variación anual del acumulado en lo que va de año" ~ "var_anu_acu",
  TRUE ~ "var_acu_respecto_19"))

df <- nacimientos


df_wide <- df %>% 
  pivot_wider(names_from = tipo_de_dato, values_from = total) 

#- ver si esta ok
zz <- df_wide %>% filter(ine_ccaa == "00") %>% 
  filter(edad_de_la_madre == "Todas las edades") %>% 
  arrange(year, mes)

names(df_wide)
df_new <- df_wide %>% select(ine_ccaa, ine.ccaa.n, edad_de_la_madre, fecha, numero) %>% filter(edad_de_la_madre== "Todas las edades") %>% slice(-c(1:81))




library(ggplot2)
library(ggthemes)
library(gganimate)

df_new <- df_new %>% dplyr::rename("Comunidades"="ine.ccaa.n")

grafico <- ggplot()
df_new <- df_new %>% slice(-c(1))
ggplot(df_new) +  geom_col(aes(numero,Comunidades,fill=fecha))+ theme_minimal() + labs(title = "Nacimientos durante la Covid-19 según CCAA",
                                                                          subtitle = "Datos del INE, periodo 2019-2022 - Fecha: {frame_time}",
                                                                          x= "Número nacimientos total",
                                                                          y= "CCAA") +transition_time(fecha)


