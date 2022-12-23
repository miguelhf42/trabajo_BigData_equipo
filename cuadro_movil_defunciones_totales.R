library(tidyverse)
library(sf)
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

df_new <- df_wide %>% filter(edad == "Total", sexo == "Total") %>% select(-c(covid_no_identif, otras_causas, edad, sexo, ine_ccaa))








DT::datatable(df_new)
DT::datatable(df_new, filter = 'top', 
              options = list(pageLength = 7, autoWidth = TRUE ))
df_new %>%
  tibble::as_tibble() %>%
  DT::datatable(filter = 'top', options = list(pageLength = 7, autoWidth = TRUE))

