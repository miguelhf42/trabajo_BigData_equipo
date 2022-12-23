library(tidyverse)
library(tmap)
fs::dir_create("pruebas") #- primero creamos la carpeta "pruebas"
my_url <- "https://ine.es/jaxi/files/tpx/es/csv_bd/49870.csv?nocab=1"
curl::curl_download(my_url, "./pruebas/causadefun.csv")
causadefun <- rio::import("./pruebas/causadefun.csv")


causadefun <- read_delim("pruebas/causadefun.csv", 
                         delim = "\t", escape_double = FALSE, 
                         col_types = cols(Total = col_number()),  
                         locale = locale(date_names = "es", decimal_mark = ",", grouping_mark = "."), 
                         trim_ws = TRUE)

names(df)
df <- janitor::clean_names(causadefun)
df <- df %>% rename(ine_ccaa.n = comunidades_y_ciudades_autonomas)
zz <- pjpv.curso.R.2022::pjp_valores_unicos(df)

#- poner codigo de CCAA
#- h de ver q se llaman igual
zz <- df %>% distinct(ine_ccaa.n) %>% filter(!is.na(ine_ccaa.n)) 
codigos <- pjpv.curso.R.2022::ine_pob_prov_1996_2021 %>% distinct(ine_ccaa, ine_ccaa.n)
zz <- full_join(codigos, zz)
#- ok tienen los mismo nombres, asi q fusiono
df <- full_join(df, codigos)


df <- df %>% 
  mutate(ine_ccaa.n = ifelse(is.na(ine_ccaa.n), "Total", ine_ccaa.n)) %>% 
  mutate(ine_ccaa = ifelse(is.na(ine_ccaa), "00", ine_ccaa))  %>% 
  select(-nacional) %>% 
  relocate(ine_ccaa, .after = ine_ccaa.n)

zz <- pjpv.curso.R.2022::pjp_valores_unicos(df)

df <- df %>% mutate(covid_19 = case_when(
  covid_19 == "Total" ~ "total",
  covid_19 == "Covid-19 Virus identificado" ~ "covid_identif",
  covid_19 == "Covid-19 Virus no identificado (sospechoso)" ~ "covid_no_identif",
  covid_19 == "Otras causas" ~ "otras_causas"))

janitor::tabyl(df, covid_19)

df_wide <- df %>% pivot_wider(names_from = covid_19, values_from = total) 

df_new <- df_wide %>% filter(edad == "Total", sexo == "Total") 


#- cargo geometrías de provincias
df_geo_prov <- pjpv.curso.R.2022::LAU2_prov_2020_canarias


#- podemos ver q la última columna de df_geo_prov tiene las "geometrías"
names(df_geo_prov)
head(df_geo_prov)

#- me quedo con las vv. q me interesan
df_geo_prov <- df_geo_prov %>% select(ine_prov, ine_prov.n, ine_ccaa, ine_ccaa.n)
names(df_geo_prov)

#- podemos "agregar" geometrías
df_geo_ccaa <- df_geo_prov %>% 
  group_by(ine_ccaa, ine_ccaa.n) %>% summarize() %>% ungroup()
names(df_geo_ccaa)

# df_geo_esp <- df_geo_ccaa %>% group_by(1) %>% summarise()
# plot(df_geo_esp, max.plot = 1)


#- junto geometría (df_geo_ccaa) con datos INE (df_ccaa_2021)
#- las geometrías a la izquierda

#mapa escalas

df_ok <- left_join(df_geo_ccaa, df_new, by = c("ine_ccaa" = "ine_ccaa"))
names(df_ok)

p <- ggplot() +
  geom_sf(data = df_ok, 
          aes(geometry = geometry, fill = total), 
          color = "white", size = 0.09) 

p + scale_fill_distiller(palette = 2)
p + pjpv.curso.R.2022::theme_pjp_maps()
p + scale_fill_viridis_c(option = "plasma")


p + pjpv.curso.R.2022::theme_pjp_maps() +
  labs(title = "Escala defunciones según la CCAA", 
       
       caption = "Datos provenientes del INE")
p



