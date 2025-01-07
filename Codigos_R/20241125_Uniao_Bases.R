# Removing Garbage ####
rm(list=ls())

# Packages  ####
library(dplyr)
library(readr)

# Defina o diretório onde estão os arquivos baixados
input_folder = "C:/Users/dsgan/OneDrive/Área de Trabalho/Deskc/Mestrado/BDDA/Trabalho/arquivos_monitriip"

# Liste todos os arquivos CSV na pasta
csv_files = list.files(input_folder, pattern = "\\.csv$", full.names = TRUE)


# Função para ler o arquivo e garantir que colunas problemáticas sejam numéricas
read_and_convert = function(file_path) {
  data <- read_csv2(file_path, show_col_types = FALSE)
  
  # Converte todas as colunas com nome 'latitude' e 'longitude' para tipo character
  data <- data %>%
    mutate(across(contains("latitude"), as.character)) %>%
    mutate(across(contains("pdop"), as.character)) %>%
    mutate(across(contains("longitude"), as.character))      # Força longitude a ser character
  
  
  
  return(data)
}

# Ler e combinar os arquivos CSV2, com conversão das colunas para character
dados = csv_files %>%
  lapply(read_and_convert) %>%  # Aplica a função de leitura e conversão
  bind_rows() 


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



# Estatisticas por caminho
# Calcular as estatísticas descritivas para a latitude
estatisticas_viagens = dados %>%
  dplyr::group_by(latitude_cod, longitude_cod) %>%
  dplyr::summarise(
    quantidade_viagens = n(),                          
    minimo = min(tempo_deslocamento, na.rm = TRUE),      
    maximo = max(tempo_deslocamento, na.rm = TRUE),     
    media = mean(tempo_deslocamento, na.rm = TRUE),    
    desvio = sd(tempo_deslocamento, na.rm = TRUE),      
    mediana = median(tempo_deslocamento, na.rm = TRUE),
    p25 = quantile(tempo_deslocamento,.25, na.rm = TRUE),
    p75 = quantile(tempo_deslocamento,.25, na.rm = TRUE),
    iqr = IQR(tempo_deslocamento, na.rm = TRUE)         
  ) %>%
  dplyr::mutate(categoria_viagens = cut(quantidade_viagens, 
                                        breaks = seq(0, max(quantidade_viagens, na.rm = TRUE) + 500, by = 500),
                                        include.lowest = TRUE,
                                        labels = paste0(seq(0, max(quantidade_viagens, na.rm = TRUE), by = 500), 
                                                        "-", 
                                                        seq(500, max(quantidade_viagens, na.rm = TRUE) + 500, by = 500)))) %>%
  dplyr::mutate(valor_outlier_sup = p25 - 1.5 *  iqr,
                valor_outlier_inf = p75 + 1.5 *  iqr)


# Base de dados para investigacao - Missings ####
df_missings = dados %>%
  dplyr::filter(is.na(latitude_cod) | is.na(longitude_cod) | is.na(tempo_deslocamento))

# Base de dados para investigacao -  ####
ponto_corte = 5/60
df_investigar = dados %>%
  dplyr::filter(is.na(tempo_deslocamento) | tempo_deslocamento < ponto_corte)

# Estatisticas dos veiculos ####
df_veiculos = dados %>%
  dplyr::group_by(placa) %>%
  dplyr::summarise(
    quantidade_viagens = n(),  
    total = sum(tempo_deslocamento, na.rm = TRUE),
    minimo = min(tempo_deslocamento, na.rm = TRUE),      
    maximo = max(tempo_deslocamento, na.rm = TRUE),     
    media = mean(tempo_deslocamento, na.rm = TRUE),    
    desvio = sd(tempo_deslocamento, na.rm = TRUE),      
    mediana = median(tempo_deslocamento, na.rm = TRUE),
    p25 = quantile(tempo_deslocamento,.25, na.rm = TRUE),
    p75 = quantile(tempo_deslocamento,.25, na.rm = TRUE),
    iqr = IQR(tempo_deslocamento, na.rm = TRUE)         
  ) %>%
  dplyr::mutate(valor_outlier_sup = p25 - 1.5 *  iqr,
                valor_outlier_inf = p75 + 1.5 *  iqr) %>%
  arrange(desc(quantidade_viagens))

# Criar CSVS com os arquivos processados ####
write.csv(dados, 'DF_VIAGENS.csv')
write.csv(df_veiculos, 'DF_RESUMO_VEICULOS.csv')
write.csv(df_missings, 'DF_MISSINGS_VIAGENS.csv')
write.csv(df_investigar, 'DF_INVESTIGAR_VIAGENS.csv')
write.csv(estatisticas_viagens, 'DF_RESUMO_CAMINHOS.csv')






