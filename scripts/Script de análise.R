install.packages("dplyr")
install.packages("tidyr")
install.packages("gapminder")
install.packages("readxl")
install.packages("writexl")
install.packages("ggplot2")

library(dplyr)
library(tidyr)
library(gapminder)
library(readxl)
library(writexl)
library(ggplot2)

# 1. Ler o arquivo (33 milhões de linhas, CUIDADO!)
depressao <- read.csv("../data/raw/DB-Anvisa2-002.csv")

# 2. Extrair os dados que precisamos para um novo dataframe
# Neste caso foram todos as linhas da substancia CLORIDRATO DE NORTRIPTILINA de 2019 a 2021
cloridrato <- depressao %>% filter(ANO_VENDA > 2018, PRINCIPIO_ATIVO == "CLORIDRATO DE NORTRIPTILINA")

# 3. Salvar o dataframe filtrado para que não haja a necessidade de carregar sempre ambos os codigos anteriores
write.csv(cloridrato, file = "cloridrato.csv", row.names = FALSE)

# dropar o dataframe depressao
rm(depressao)

### CARREGAR O DATAFRAME BRUTO ###
cloridrato <- read.csv("cloridrato.csv")

# 4. Verificar quantos valores NA existem no DF para definir se serão tratados ou se a coluna/linhas serão descartadas
colSums(is.na(cloridrato))

# Lembrando que o DF tem 280.256 linhas
# SEXO, IDADE e UNIDADE_IDADE = 217.936 é NA
# As demais colunas não tem valores NA

# 5. Dropar as colunas que contém dados desnecessários para análise:
# Neste caso, as colunas: QTD_ATIVO_POR_UNID_FARMACOTEC, CONSELHO_PRESCRITOR, UF_CONSELHO_PRESCRITOR, CID10, SEXO, IDADE e UNIDADE_IDADE 
cloridrato <- cloridrato[, -c(7, 11, 12, 14, 15, 16, 17)]

# 6. Dropar todos as linhas cujo:
# UNIDADE_MEDIDA_PRINCIPIO_ATIVO == "UNIDADE"
# TIPO_UNIDADE_FARMACOTECNICA == "MILILITRO" && "GRAMA"
cloridrato <- subset(cloridrato, UNIDADE_MEDIDA_PRINCIPIO_ATIVO != "UNIDADE")
cloridrato <- subset(cloridrato, TIPO_UNIDADE_FARMACOTECNICA == "CÁPSULA")

# 7. Salvar o dataframe novamente para que não seja necessário dropar as colunas novamente, mas manter o anterior também como base bruta e este como base tratada
write.csv(cloridrato, file = "cloridrato_tratado.csv", row.names = FALSE)

# 8. Alterando "," para "." para que não haja perca de valores ao trocar o tipo de dados para numérico
cloridrato$QTD_UNIDADE_FARMACOTECNICA <- gsub(",", ".", cloridrato$QTD_UNIDADE_FARMACOTECNICA)

### CARREGAR O DATAFRAME FILTRADO ###
cloridrato <- read.csv("cloridrato_tratado.csv")

# 9. Mudando dados da coluna de caracteres para númericos
cloridrato$QTD_UNIDADE_FARMACOTECNICA <- as.numeric(cloridrato$QTD_UNIDADE_FARMACOTECNICA)

### CARREGAR O DATAFRAME DE POPULACAO ###
populacao <- read_excel("../data/processed/populacao_municipios.xlsx")

# 10. Padronizar os nomes dos munícipios em maíusculas assim como no dataset principal
populacao <- mutate(populacao, NOME_DO_MUNICIPIO = toupper(NOME_DO_MUNICIPIO))

# 11. Salvar o dataset de população com os dados padronizados
write_xlsx(populacao, "../data/processed/populacao_municipios1.xlsx")

### CARREGAR O DATAFRAME DE POPULACAO PÓS PADRONIZAÇÃO DOS DADOS ###
populacao <- read_excel("../data/processed/populacao_municipios1.xlsx")

# 11. Agrupar os dados de venda por estado e por ano
vendas_2019_por_estado <- cloridrato %>% filter(ANO_VENDA == 2019) %>% group_by(UF_VENDA)%>% summarize(sum(QTD_UNIDADE_FARMACOTECNICA))
populacao_estados_2019 <- populacao %>% filter(ANO == 2019) %>% group_by(UF) %>% summarize(sum(POPULACAO_ESTIMADA))

vendas_2020_por_estado <- cloridrato %>% filter(ANO_VENDA == 2020) %>% group_by(UF_VENDA)%>% summarize(sum(QTD_UNIDADE_FARMACOTECNICA))
populacao_estados_2020 <- populacao %>% filter(ANO == 2020) %>% group_by(UF) %>% summarize(sum(POPULACAO_ESTIMADA))

vendas_2021_por_estado <- cloridrato %>% filter(ANO_VENDA == 2021) %>% group_by(UF_VENDA)%>% summarize(sum(QTD_UNIDADE_FARMACOTECNICA))
populacao_estados_2021 <- populacao %>% filter(ANO == 2021) %>% group_by(UF) %>% summarize(sum(POPULACAO_ESTIMADA))

# 12. Padronizar os nomes das coluna e cruzar os dados por ano
vendas_2019_por_estado <- rename(vendas_2019_por_estado, UF = UF_VENDA)
vendas_2019_estado_populacao <- inner_join(populacao_estados_2019, vendas_2019_por_estado, by = "UF")
vendas_2019_estado_populacao <- rename(vendas_2019_estado_populacao, 'POPULACAO_ESTIMADA' = 'sum(POPULACAO_ESTIMADA)')
vendas_2019_estado_populacao <- rename(vendas_2019_estado_populacao, 'QTD_UNIDADE_FARMACOTEC_VENDIDA' = 'sum(QTD_UNIDADE_FARMACOTECNICA)')

vendas_2020_por_estado <- rename(vendas_2020_por_estado, UF = UF_VENDA)
vendas_2020_estado_populacao <- inner_join(populacao_estados_2020, vendas_2020_por_estado, by = "UF")
vendas_2020_estado_populacao <- rename(vendas_2020_estado_populacao, 'POPULACAO_ESTIMADA' = 'sum(POPULACAO_ESTIMADA)')
vendas_2020_estado_populacao <- rename(vendas_2020_estado_populacao, 'QTD_UNIDADE_FARMACOTEC_VENDIDA' = 'sum(QTD_UNIDADE_FARMACOTECNICA)')

vendas_2021_por_estado <- rename(vendas_2021_por_estado, UF = UF_VENDA)
vendas_2021_estado_populacao <- inner_join(populacao_estados_2021, vendas_2021_por_estado, by = "UF")
vendas_2021_estado_populacao <- rename(vendas_2021_estado_populacao, 'POPULACAO_ESTIMADA' = 'sum(POPULACAO_ESTIMADA)')
vendas_2021_estado_populacao <- rename(vendas_2021_estado_populacao, 'QTD_UNIDADE_FARMACOTEC_VENDIDA' = 'sum(QTD_UNIDADE_FARMACOTECNICA)')

# Calcular a quantidade de gramas de medicamento por pessoa em 2019
indice_de_depressao_2019 <- mutate(vendas_2019_estado_populacao, GRAMAS_POR_PESSOA = QTD_UNIDADE_FARMACOTEC_VENDIDA / POPULACAO_ESTIMADA)

grafico_2019 <- ggplot(data = indice_de_depressao_2019, aes(x = UF, y = GRAMAS_POR_PESSOA)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Estado", y = "gramas por pessoa", title = "Quantidade de gramas p/ pessoa em cada estado em 2019")
print(grafico_2019)

indice_de_depressao_2020 <- mutate(vendas_2020_estado_populacao, GRAMAS_POR_PESSOA = QTD_UNIDADE_FARMACOTEC_VENDIDA / POPULACAO_ESTIMADA)

grafico_2020 <- ggplot(data = indice_de_depressao_2020, aes(x = UF, y = GRAMAS_POR_PESSOA)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Estado", y = "gramas por pessoa", title = "Quantidade de gramas p/ pessoa em cada estado em 2020")
print(grafico_2020)


indice_de_depressao_2021 <- mutate(vendas_2021_estado_populacao, GRAMAS_POR_PESSOA = QTD_UNIDADE_FARMACOTEC_VENDIDA / POPULACAO_ESTIMADA)

grafico_2021 <- ggplot(data = indice_de_depressao_2021, aes(x = UF, y = GRAMAS_POR_PESSOA)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Estado", y = "gramas por pessoa", title = "Quantidade de gramas p/ pessoa em cada estado em 2021")
print(grafico_2021)
