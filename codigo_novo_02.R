library(tidyverse)
library(magrittr)
library(dplyr)
library(readr)
library(rjson)
library(RJSONIO)
library(lubridate)
library(stringi)



vacinacao_geral <- read.csv("data/vacinacaoPorDia.csv")

#view(vacinacao_geral)
glimpse(vacinacao_geral)

#transformar coluna chr em date
vacinacao_geral$DATAS <- dmy(vacinacao_geral$DATAS)

#agrupar por mes e ano
dados <- vacinacao_geral %>%
  group_by(mes_ano = format(DATAS, "%Y-%m")) %>%
  summarize(Soma = sum(NUMEROS))

#view(dados)
glimpse(dados)

#escrever por extenso o mes e ano (as vígulas servem para separar um dado do outro posteriormente no data_axis)
dados$mes_ano <- as.Date(paste(dados$mes_ano, "01", sep = "-"))
dados$ano_mes_escrito <- paste(format(dados$mes_ano, "%B"), format(dados$mes_ano, "%Y"),",")

#arredondamento
dados %<>% mutate(Soma = round(Soma/1000,2))

#separar dados que irão para o gráfico
dados <- dados[,c("Soma", "ano_mes_escrito")]
names(dados) <- c("Número de vacinas aplicadas", "Data")
nomes <- names(dados)

#tirar vígula do último dado
dados |>
  mutate(Data= as.character(Data))
dados <- dados |>
  mutate(Data = recode(Data, "junho 2023 ," = "junho 2023"))


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
  select(Data,`Número de vacinas aplicadas`) %>% #filter(ano<2019) %>%
  #arrange(trimestre) %>%
  mutate(Data= as.character(Data)) %>% list()               

exportJson0 <- toJSON(objeto_0)


titulo<-T_ST_P_No_SAUDE$TITULO[6]
subtexto<-"Fonte: Vacinômetro Saúde Salvador"
link <- T_ST_P_No_SAUDE$LINK[6]

data_axis <- paste('["',gsub(',','","',
                             paste(paste(as.vector(objeto_0[[1]]$Data)),
                                   collapse = ' ')),'"]',sep = '')



data_serie <- paste('[',gsub(' ',',',
                             paste(paste(as.vector(objeto_0[[1]]$`Número de vacinas aplicadas`)),
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


write(exportJson0,file = paste('data/',gsub('.csv','',T_ST_P_No_SAUDE$NOME_ARQUIVO_JS[6]),
                               '.json',sep =''))
write(texto,file = paste('data/',T_ST_P_No_SAUDE$NOME_ARQUIVO_JS[6],
                         sep =''))

#}

# Arquivo dedicado a rotina de atualizacao global. 

write_csv2(SAIDA_POVOAMENTO,file ='data/POVOAMENTO.csv',quote='all',escape='none')
#quote="needed")#,escape='none')


objeto_autm <- SAIDA_POVOAMENTO %>% list()
exportJson_aut <- toJSON(objeto_autm)

#write(exportJson_aut,file = paste('data/povoamento.json'))







