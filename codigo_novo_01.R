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
dados_classificacao <- dados_gerais[,c("DATA_DA_NOTIFICACAO", "STATUS")]
dados_classificacao$mes <- month(dados_classificacao$DATA_DA_NOTIFICACAO)
#2021
dados_classificacao2021 <- dados_gerais2021[,c("DATA_DA_NOTIFICACAO", "STATUS")]
dados_classificacao2021$mes <- month(dados_classificacao2021$DATA_DA_NOTIFICACAO)
#view(dados_classificacao)

#Quantidade por mês
#2020
dados <- table(dados_classificacao$mes)
dados <- as.data.frame(dados)
names(dados) <- c("Mês", "Número de casos confirmados")
nomes <- names(dados)
#2021
dados2 <- table(dados_classificacao2021$mes)
dados2 <- as.data.frame(dados2)
names(dados2) <- c("Mês", "Número de casos confirmados")
#nomes <- names(dados)


#colocar os nomes dos meses e anos
#2020
glimpse(dados)
dados <- dados |>
  mutate(Mês = as.character(Mês))
dados <- dados |>
  mutate(Mês = recode(Mês, "1" = "Janeiro/2020"))
dados <- dados |>
  mutate(Mês = recode(Mês, "2" = "Fevereiro/2020"))
dados <- dados |>
  mutate(Mês = recode(Mês, "3" = "Março/2020"))
dados <- dados |>
  mutate(Mês = recode(Mês, "4" = "Abril/2020"))
dados <- dados |>
  mutate(Mês = recode(Mês, "5" = "Maio/2020"))
dados <- dados |>
  mutate(Mês = recode(Mês, "6" = "Junho/2020"))
dados <- dados |>
  mutate(Mês = recode(Mês, "7" = "Julho/2020"))
dados <- dados |>
  mutate(Mês = recode(Mês, "8" = "Agosto/2020"))
dados <- dados |>
  mutate(Mês = recode(Mês, "9" = "Setembro/2020"))
dados <- dados |>
  mutate(Mês = recode(Mês, "10" = "Outubro/2020"))
dados <- dados |>
  mutate(Mês = recode(Mês, "11" = "Novembro/2020"))
dados <- dados |>
  mutate(Mês = recode(Mês, "12" = "Dezembro/2020"))
#2021
dados2 <- dados2 |>
  mutate(Mês = as.character(Mês))
dados2 <- dados2 |>
  mutate(Mês = recode(Mês, "1" = "Janeiro/2021"))
dados2 <- dados2 |>
  mutate(Mês = recode(Mês, "2" = "Fevereiro/2021"))
dados2 <- dados2 |>
  mutate(Mês = recode(Mês, "3" = "Março/2021"))
dados2 <- dados2 |>
  mutate(Mês = recode(Mês, "4" = "Abril/2021"))
dados2 <- dados2 |>
  mutate(Mês = recode(Mês, "5" = "Maio/2021"))
dados2 <- dados2 |>
  mutate(Mês = recode(Mês, "6" = "Junho/2021"))
dados2 <- dados2 |>
  mutate(Mês = recode(Mês, "7" = "Julho/2021"))
dados2 <- dados2 |>
  mutate(Mês = recode(Mês, "8" = "Agosto/2021"))
dados2 <- dados2 |>
  mutate(Mês = recode(Mês, "9" = "Setembro/2021"))
dados2 <- dados2 |>
  mutate(Mês = recode(Mês, "10" = "Outubro/2021"))
dados2 <- dados2 |>
  mutate(Mês = recode(Mês, "11" = "Novembro/2021"))
dados2 <- dados2 |>
  mutate(Mês = recode(Mês, "12" = "Dezembro/2021"))

#arredondar para visualização
dados %<>% mutate(`Número de casos confirmados` = round(`Número de casos confirmados`/1000,2))
dados2 %<>% mutate(`Número de casos confirmados` = round(`Número de casos confirmados`/1000,2))

#juntar dados de 2020 e 2021
dados <- rbind(dados, dados2)

#view(dados)


glimpse(dados)


T_ST_P_No_SAUDE <- read_csv("data/TEMA_SUBTEMA_P_No -  SAUDE.csv")


## Arquivo de saida 

SAIDA_POVOAMENTO <- T_ST_P_No_SAUDE %>% 
  select(TEMA,SUBTEMA,PERGUNTA,NOME_ARQUIVO_JS)
SAIDA_POVOAMENTO <- as.data.frame(SAIDA_POVOAMENTO)



# Cores secundarias paleta pantone -
corsec_recossa_azul <- c('#a094e1','#dc6f6c','#62acd1','#8bc6d2',
                         '#d62839','#20cfef','#fe4641','#175676')
# Cor 1 - Roxo; Cor 2, 5, 7 - Vermelho; Cor 3, 4, 6, 8 - Azul

simbolo_linhas <- c('emptyCircle','emptyTriangle','emptySquare',
                    'emptyDiamond','emptyRoundRect')



objeto_0 <- dados %>%
  #filter(classe %in% c(classes[1])) %>%
  select(Mês,`Número de casos confirmados`) %>% #filter(ano<2019) %>%
  #arrange(trimestre) %>%
  mutate(Mês= as.character(Mês)) %>% list()               

exportJson0 <- toJSON(objeto_0)


titulo<-T_ST_P_No_SAUDE$TITULO[5]
subtexto<-"Fonte: Saúde Salvador"
link <- T_ST_P_No_SAUDE$LINK[5]

data_axis <- paste('["',gsub(' ','","',
                             paste(paste(as.vector(objeto_0[[1]]$Mês)),
                                   collapse = ' ')),'"]',sep = '')


data_serie <- paste('[',gsub(' ',',',
                             paste(paste(as.vector(objeto_0[[1]]$`Número de casos confirmados`)),
                                   collapse = ' ')),']',sep = '')


texto<-paste('{"title":{"text":"',titulo,
             '","subtext":"',subtexto,
             '","sublink":"',link,'"},',
             '"tooltip":{"trigger":"item","responsive":true,"position":"top","formatter":"{b}: {c} mil"},',
             '"toolbox":{"left":"center","orient":"horizontal","itemSize":20,"top":20,"show":true,',
             '"feature":{"dataZoom":{"yAxisIndex":"none"},',
             '"dataView":{"readOnly":false},',
             '"restore":{},"saveAsImage":{}}},"legend":{"show":false,"top":"bottom"},"xAxis":{"type":"category",',
             '"data":',data_axis,'},',
             '"yAxis":{"type":"value","axisLabel":{"formatter":"{value} mil"}},',
             '"graphic":[{"type":"text","left":"center","top":"bottom","z":100,"style":{"fill":"gray","text":"Obs: Ponto é separador decimal", "font":"8px sans-srif","fontSize":12}}],',
             '"series":[{"name":"',nomes[2],'","data":',data_serie,',',
             '"type":"line","color":"',corsec_recossa_azul[1],'","showBackground":true,',
             '"backgroundStyle":{"color":"rgba(180, 180, 180, 0.2)"},"symbol":"',simbolo_linhas[1],
             '","symbolSize":10,"itemStyle":{"borderRadius":10,"borderColor":"',corsec_recossa_azul[1],'","borderWidth":2}}',
             ']}',sep='')

#SAIDA_POVOAMENTO$CODIGO[i] <- texto   
texto<-noquote(texto)


write(exportJson0,file = paste('data/',gsub('.csv','',T_ST_P_No_SAUDE$NOME_ARQUIVO_JS[5]),
                               '.json',sep =''))
write(texto,file = paste('data/',T_ST_P_No_SAUDE$NOME_ARQUIVO_JS[5],
                         sep =''))

#}

# Arquivo dedicado a rotina de atualizacao global. 

write_csv2(SAIDA_POVOAMENTO,file ='data/POVOAMENTO.csv',quote='all',escape='none')
#quote="needed")#,escape='none')


objeto_autm <- SAIDA_POVOAMENTO %>% list()
exportJson_aut <- toJSON(objeto_autm)

#write(exportJson_aut,file = paste('data/povoamento.json'))







