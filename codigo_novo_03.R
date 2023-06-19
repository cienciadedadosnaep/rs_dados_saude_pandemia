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

#juntar dados de 2020 e 2021
dados <- rbind(dados_classificacao, dados_classificacao2021)

#separar mês e ano
dados$data_status <- paste(format(dados$DATA_DA_NOTIFICACAO, "%B"), format(dados$DATA_DA_NOTIFICACAO, "%Y"),",")
glimpse(dados)

#Fazer intervalos de faixa etária de 10 em 10 anos
intervalos_idade <- c(0, 9, 19, 29, 39, 49, 59, 69, 79, Inf)
dados$faixa_etaria <- cut(dados$IDADE, breaks = intervalos_idade, labels = FALSE,  right = FALSE)
glimpse(dados)

#mudar nomes de faixa etária
dados <- dados |>
  mutate(faixa_etaria = as.character(faixa_etaria))
dados <- dados |>
  mutate(faixa_etaria = recode(faixa_etaria, "1" = "0-9"))
dados <- dados |>
  mutate(faixa_etaria = recode(faixa_etaria, "2" = "10-19"))
dados <- dados |>
  mutate(faixa_etaria = recode(faixa_etaria, "3" = "20-29"))
dados <- dados |>
  mutate(faixa_etaria = recode(faixa_etaria, "4" = "30-39"))
dados <- dados |>
  mutate(faixa_etaria = recode(faixa_etaria, "5" = "40-49"))
dados <- dados |>
  mutate(faixa_etaria = recode(faixa_etaria, "6" = "50-59"))
dados <- dados |>
  mutate(faixa_etaria = recode(faixa_etaria, "7" = "60-69"))
dados <- dados |>
  mutate(faixa_etaria = recode(faixa_etaria, "8" = "70-79"))
dados <- dados |>
  mutate(faixa_etaria = recode(faixa_etaria, "9" = "80+"))


#frequencia de data e faixa etaria
dados2 <- table(dados$data_status, dados$faixa_etaria)
dados2 <- as.data.frame(dados2)

dados3 <- dados2 %>%
  pivot_wider(names_from = Var2, values_from = Freq)
glimpse(dados3)

meses <- seq(as.Date("2020-01-01"), as.Date("2021-12-01"), by = "month")
meses <- format(meses, format = "%B %Y")
meses <- paste(meses, ",", sep = " ")

dados4 <- dados3 %>% 
  mutate(Var1  = factor(Var1, levels= meses)) %>% 
  arrange(Var1)

names(dados4) <- c("Mês", "0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")
nomes <- names(dados4)
glimpse(dados4)

dados4 <- dados4 |>
  mutate(Mês = recode(Mês, "dezembro 2021 ," = "dezembro 2021"))
#view(dados3)
view(dados4)

