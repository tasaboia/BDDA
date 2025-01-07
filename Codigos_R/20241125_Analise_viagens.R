
# Removing objects ####
rm(list = ls())

# Packages ####
require(tidyverse)
require(janitor)

# Setting Location ####
setwd('C:/Users/dsgan/OneDrive/Documentos/arquivos_monitriip')

# Ler em RData para atualizar somente, nao precisar ficar conectando toda a hora ####
dados = read_rds('20241125_dadosviagens.rds')

# Data Cleaning ####
dados = dados %>%
  janitor::clean_names()

# Data Wrangling ####
dados = dados %>%
  dplyr::mutate(
    data_inicio_viagem_cod = lubridate::dmy_hms(data_inicio_viagem),
    data_fim_viagem_cod = lubridate::dmy_hms(data_fim_viagem),
    latitude_cod = round(as.numeric(latitude),2), 
    longitude_cod = round(as.numeric(longitude),2)
    ) %>%
  dplyr::mutate(latitude_cod = ifelse(latitude_cod > -0.001 & latitude_cod < 0.001, NA,latitude_cod),
                longitude_cod = ifelse(longitude_cod > -0.001 & longitude_cod < 0.001, NA,longitude_cod)) %>%
  dplyr::mutate(latitude_cod = ifelse(latitude_cod < -90 | latitude_cod > 90, NA,latitude_cod),
                longitude_cod = ifelse(longitude_cod < -180 | longitude_cod > 180, NA,longitude_cod)) %>%
  
  dplyr::mutate(latitude_cod = ifelse(is.na(longitude_cod), NA,latitude_cod),
                longitude_cod = ifelse(is.na(latitude_cod), NA,longitude_cod)) %>%
  dplyr::mutate(codigo_origem_destino = paste0( latitude_cod, ';', longitude_cod )) %>%
  dplyr::mutate(tempo_deslocamento = as.numeric(difftime(data_fim_viagem_cod, data_inicio_viagem_cod, units = "hours"))) %>%
  dplyr::mutate(
    flag_erro_geo = ifelse(is.na(latitude_cod) | latitude_cod == 0 | is.na(longitude_cod) | longitude_cod == 0,1,0),
    flag_erro_velocidade = ifelse(tempo_deslocamento <=0 | is.na(tempo_deslocamento),1,0) ) %>%
  dplyr::mutate(tempo_deslocamento = ifelse(tempo_deslocamento <= 0, NA, tempo_deslocamento)) %>%
  dplyr::mutate(ano = year(data_inicio_viagem_cod),
                mes = month(data_inicio_viagem_cod))
  

# Estatisticas da Duracao  ####
# Apesar da disponibilidade, temos muitos dados de latitude e longitude ausentes.
# Qualidade dos dados 
table(dados$flag_erro_geo, useNA = 'always')
table(dados$flag_erro_velocidade, useNA = 'always')

getwd()




