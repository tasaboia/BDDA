# Removing Garbage ####
rm(list=ls())

# Packages  ####
library(rvest)
library(httr)

# Setting Location
setwd('C:/Users/dsgan/OneDrive/Área de Trabalho/Deskc/Mestrado/BDDA/Trabalho')
# URL da página com os arquivos
page_url = 'https://dados.antt.gov.br/dataset/monitriip-servico-fretado-viagens'
# Ler o conteúdo da página
webpage = read_html(page_url)

# Extrair todos os links
links = html_nodes(webpage, "a") %>% html_attr("href")

# Filtrar links que terminam em .csv, .xlsx ou .zip
file_links = links[grepl("https://dados.antt.gov.br/dataset/", links)]

# Pasta para salvar os arquivos
output_folder = "arquivos_monitriip"
dir.create(output_folder, showWarnings = FALSE)

# Fazer o download dos arquivos
for (file_url in file_links) {
  # Obter o nome do arquivo
  file_name <- basename(file_url)
  
  # Caminho completo para salvar o arquivo
  file_path <- file.path(output_folder, file_name)
  
  # Baixar o arquivo
  message("Baixando: ", file_url)
  response <- GET(file_url, write_disk(file_path, overwrite = TRUE))
  
  if (response$status_code == 200) {
    message("Salvo em: ", file_path)
  } else {
    message("Falha ao baixar: ", file_url)
  }
}



