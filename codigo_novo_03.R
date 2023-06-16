######################################################DADOS DE 2020
# 1) Carregar bibliotecas
#install.packages("tidyverse")
#install.packages("dplyr")
#insstall.package("rjson")
#install.packages("RJSONIO")
library(tidyverse)
library(magrittr)
library(dplyr)
library(readr)
library(rjson)
library(RJSONIO)
library(lubridate)





#dados de 2020
dados_gerais <- read_csv("data/dadosGerais2020.csv")
#dados de 2021
dados_gerais2021 <- read_csv("data/dadosGerais2021.csv")

#view(dados_gerais)
glimpse(dados_gerais2021)

#Todos os casos estão com status confirmado
dados_status <- table(dados_gerais$STATUS)
dados_status21 <- table(dados_gerais2021$STATUS)

#separar data da confirmação
#2020
dados_classificacao <- dados_gerais[,c("DATA_DA_NOTIFICACAO","DATA_DE_NASCIMENTO", "STATUS")]
#dados_classificacao$idade <- difftime(dados_classificacao$DATA_DA_NOTIFICACAO, dados_classificacao$DATA_DE_NASCIMENTO, units = "years")
dados_classificacao$IDADE <- as.numeric(interval(dados_classificacao$DATA_DE_NASCIMENTO, dados_classificacao$DATA_DA_NOTIFICACAO) / dyears(1))


#2021
dados_classificacao2021 <- dados_gerais2021[,c("DATA_DA_NOTIFICACAO","DATA_DE_NASCIMENTO", "STATUS")]
dados_classificacao2021$IDADE <- as.numeric(interval(dados_classificacao2021$DATA_DE_NASCIMENTO, dados_classificacao2021$DATA_DA_NOTIFICACAO) / dyears(1))


dados <- rbind(dados_classificacao, dados_classificacao2021)


dados$data_status <- paste(format(dados$DATA_DA_NOTIFICACAO, "%B"), format(dados$DATA_DA_NOTIFICACAO, "%Y"),",")

























#dados_idade <- read_csv("data/confirmadoFaixaEtaria.csv")
#view(dados_idade)
#glimpse(dados_idade)

#dados <- as.data.frame(t(dados_idade))

# Transformar o índice em uma nova coluna



#view(dados)


#dados <- table(dados_raca_cor$RACA_COR)
#dados <- as.data.frame(dados)



#names(dados) <- c("Faixa Etária", "Número de casos confirmados")
#nomes <- names(dados)








