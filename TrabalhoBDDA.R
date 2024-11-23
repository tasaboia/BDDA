library(dplyr)


### IMPORTAR DATASETS ###
# Substituam 'caminho/para/vosso_arquivo.csv' pelo caminho completo do vosso arquivo
caminho_arquivo <- "/Users/afonsoventura/Desktop/TrabalhoBBDA/veiculos_habilitados_09_2021.csv"
caminho_arquivo2 <- "/Users/afonsoventura/Desktop/TrabalhoBBDA/viagem_fretado_09_2021.csv"

### IMPORTAR O ARQUIVO CSV ###
dados_veiculos_habilitados <- read.csv(caminho_arquivo, header = TRUE, sep = ";")
dados_viagem_fretados <- read.csv(caminho_arquivo2, header = TRUE, sep = ";")

# Visualizar as primeiras linhas do dataset
head(dados_veiculos_habilitados)
head(dados_viagem_fretados)


### LIMPAR DADOS ###
# Remover o hífen da variável 'placa'
dados_veiculos_habilitados$placa <- gsub("-", "", dados_veiculos_habilitados$placa)

# Remover duplicados no dataset de veiculos habilitados com base na variável 'placa'
dados_veiculos_habilitados <- dados_veiculos_habilitados %>%
  distinct(placa, .keep_all = TRUE)


### JUNTAR DATASETS ###
# Unir os datasets pela variável 'placa'
dados_unidos <- dados_viagem_fretados %>%
  left_join(dados_veiculos_habilitados, by = "placa")

# Salvar o resultado num novo arquivo CSV
write.csv(dados_unidos, "dados_unidos.csv", row.names = FALSE)

# Exibir as primeiras linhas do dataset unido
head(dados_unidos)


### LIMPAR DADOS ###
# Inspecionar os valores únicos nas variáveis
unique(dados_unidos$in_transbordo)
unique(dados_unidos$servico_regular)
unique(dados_unidos$servico_fretado)
unique(dados_unidos$servico_semiurbano)

# Substituir "N<c3>O" por "NAO" nas variáveis in_transbordo, servico_regular, servico_fretado, servico_semiurbano
dados_unidos$in_transbordo <- gsub("N\xc3O", "Nao", dados_unidos$in_transbordo)
dados_unidos$servico_regular <- gsub("N\xe3o", "Nao", dados_unidos$servico_regular)
dados_unidos$servico_fretado <- gsub("N\xe3o", "Nao", dados_unidos$servico_fretado)
dados_unidos$servico_semiurbano <- gsub("N\xe3o", "Nao", dados_unidos$servico_semiurbano)

# Verificar colunas
table(dados_unidos$in_transbordo, dados_unidos$servico_regular, dados_unidos$servico_fretado, dados_unidos$servico_semiurbano)

# Inspecionar os valores únicos na variável
unique(dados_unidos$razao_social)

# Arranjar os nomes das empresas (razao_social)
dados_unidos$razao_social <- gsub("ITAPOR\xc3 TRANSPORTES COLETIVOS LTDA EPP", "ITAPORA TRANSPORTES COLETIVOS LTDA EPP", dados_unidos$razao_social)
dados_unidos$razao_social <- gsub("VIA\xc7\xc3O SANTA CLARA TRANSPORTE E TURISMO LTDA - ME", "VIACAO SANTA CLARA TRANSPORTE E TURISMO LTDA - ME", dados_unidos$razao_social)
dados_unidos$razao_social <- gsub("EXPRESSO SAT\xc9LITE NORTE LTDA", "EXPRESSO SATELITE NORTE LTDA", dados_unidos$razao_social)
dados_unidos$razao_social <- gsub("VIA\xc7\xc3O PERNAMBUCANA TRANSPORTE E TURISMO LTDA", "VIACAO PERNAMBUCANA TRANSPORTE E TURISMO LTDA", dados_unidos$razao_social)
dados_unidos$razao_social <- gsub("FAVI AUTO VIA\xc7\xc3O LTDA", "FAVI AUTO VIACAO LTDA", dados_unidos$razao_social)
dados_unidos$razao_social <- gsub("AG\xcaNCIA DE TURISMO MONTE ALEGRE LTDA", "AGENCIA DE TURISMO MONTE ALEGRE LTDA", dados_unidos$razao_social)
dados_unidos$razao_social <- gsub("VIA\xc7\xc3O PRETTI LTDA", "VIACAO PRETTI LTDA", dados_unidos$razao_social)
dados_unidos$razao_social <- gsub("TRANSPORTADORA TUR\xcdSTICA NATAL LTDA", "TRANSPORTADORA TURISTICA NATAL LTDA", dados_unidos$razao_social)
dados_unidos$razao_social <- gsub("VIA\xc7\xc3O GARCIA LTDA", "VIACAO GARCIA LTDA", dados_unidos$razao_social)
dados_unidos$razao_social <- gsub("VIA\xc7\xc3O UNI\xc3O SANTA CRUZ LTDA", "VIACAO UNIAO SANTA CRUZ LTDA", dados_unidos$razao_social)
dados_unidos$razao_social <- gsub("VIA\xc7\xc3O OLIVEIRA TORRES LTDA.", "VIACAO OLIVEIRA TORRES LTDA.", dados_unidos$razao_social)
dados_unidos$razao_social <- gsub("VIA\xc7\xc3O PIRACICABANA LTDA", "VIACAO PIRACICABANA LTDA", dados_unidos$razao_social)
dados_unidos$razao_social <- gsub("TRANSGUERRA TRANSPORTADORA TUR\xcdSTICA LTDA", "TRANSGUERRA TRANSPORTADORA TURISTICA LTDA", dados_unidos$razao_social)
dados_unidos$razao_social <- gsub("EMPRESA S\xc3O CRISTOV\xc3O LTDA", "EMPRESA SAO CRISTOVAO LTDA", dados_unidos$razao_social)
dados_unidos$razao_social <- gsub("EMPRESA UNI\xc3O DE TRANSPORTE LTDA", "EMPRESA UNIAO DE TRANSPORTE LTDA", dados_unidos$razao_social)
dados_unidos$razao_social <- gsub("AUTO VIA\xc7\xc3O CAMBUI LTDA.", "AUTO VIACAO CAMBUI LTDA.", dados_unidos$razao_social)
dados_unidos$razao_social <- gsub("VIA\xc7\xc3O CANARINHO LTDA", "VIACAO CANARINHO LTDA", dados_unidos$razao_social)
dados_unidos$razao_social <- gsub("ROTA TRANSPORTES RODOVI\xc1RIOS LTDA.", "ROTA TRANSPORTES RODOVIARIOS LTDA.", dados_unidos$razao_social)
dados_unidos$razao_social <- gsub("VIA\xc7\xc3O SANTOS LTDA", "VIACAO SANTOS LTDA", dados_unidos$razao_social)
dados_unidos$razao_social <- gsub("GIAN CARLO TUR EXCURS\xd5ES E TURISMO LTDA", "GIAN CARLO TUR EXCURSOES E TURISMO LTDA", dados_unidos$razao_social)
dados_unidos$razao_social <- gsub("TRANSPORTES BAR\xc3O LTDA - ME", "TRANSPORTES BARAO LTDA - ME", dados_unidos$razao_social)
dados_unidos$razao_social <- gsub("VIA\xc7\xc3O JEQUI\xc9 CIDADE SOL LTDA.", "VIACAO JEQUIE CIDADE SOL LTDA.", dados_unidos$razao_social)
dados_unidos$razao_social <- gsub("EXTREMOESTE AG\xcaNCIA DE VIAGENS E TURISMO LTDA", "EXTREMOESTE AGENCIA DE VIAGENS E TURISMO LTDA", dados_unidos$razao_social)
dados_unidos$razao_social <- gsub("H L DOS SANTOS TRANSPORTES E LOCA\xc7OES-ME", "H L DOS SANTOS TRANSPORTES E LOCACOES-ME", dados_unidos$razao_social)
dados_unidos$razao_social <- gsub("EXPRESSO S\xc3O MARCOS LTDA", "EXPRESSO SAO MARCOS LTDA", dados_unidos$razao_social)
dados_unidos$razao_social <- gsub("GALV\xc3O & NEVES  LOCADORA LTDA - ME", "GALVAO & NEVES  LOCADORA LTDA - ME", dados_unidos$razao_social)

# Converter variáveis para o tipo Date ou POSIXt
dados_unidos$data_inicio_viagem <- as.POSIXct(dados_unidos$data_inicio_viagem, format = "%d-%m-%Y %H:%M:%S")
dados_unidos$data_fim_viagem <- as.POSIXct(dados_unidos$data_fim_viagem, format = "%d-%m-%Y %H:%M:%S")

dados_unidos$data_validade_csv <- as.Date(dados_unidos$data_validade_csv)
dados_unidos$validade_seguro <- as.Date(dados_unidos$validade_seguro)

# Salvar o resultado num novo arquivo CSV
write.csv(dados_unidos, "dados_unidos_final.csv", row.names = FALSE)

