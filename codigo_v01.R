######################################################
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
library(readr)
library(lubridate)




all_data_v01 <- read_csv("data/confirmadoObito.csv", 
                                      col_types = cols(DATA = col_date(format = "%d/%m/%Y")))


#Qual é o número de mortes por COVID de em Salvador (acumulativo)?
dados <- all_data_v01 %>% select(DATA, -CONFIRMADO, OBITO) %>%  arrange(OBITO)

#criar coluna com o último dia de cada mês
dados$last_day <- ceiling_date(ymd(dados$DATA), 'month') - days(1)
glimpse(dados)

#df com os valores apenas do último dia de cada mês
dados2 <- subset(dados, DATA == last_day)

#escrever por extenso o mes e ano (as vígulas servem para separar um dado do outro posteriormente no data_axis)
dados2$ano_mes_escrito <- paste(format(dados2$DATA, "%B"), format(dados2$DATA, "%Y"),",")

#arredondamento
dados2 %<>% mutate(OBITO = round(OBITO/1000,2))

#tirar vígula do último dado
dados2 |>
  mutate(ano_mes_escrito= as.character(ano_mes_escrito))
dados2 <- dados2 |>
  mutate(ano_mes_escrito = recode(ano_mes_escrito, "maio 2023 ," = "maio 2023"))
glimpse(dados2)

dados2 <- dados2[,c("OBITO", "ano_mes_escrito")]
names(dados2) <- c("Número de mortes", "Data")
nomes <- names(dados2)

#view(dados)
view(dados2)


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



objeto_0 <- dados2 %>%
  #filter(classe %in% c(classes[1])) %>%
  select(Data,`Número de mortes`) %>% #filter(ano<2019) %>%
  #arrange(trimestre) %>%
  mutate(Data= as.character(Data)) %>% list()               

exportJson0 <- toJSON(objeto_0)


titulo<-T_ST_P_No_SAUDE$TITULO[2]
subtexto<-"Fonte: Saúde Salvador"
link <- T_ST_P_No_SAUDE$LINK[2]

data_axis <- paste('["',gsub(',','","',
                             paste(paste(as.vector(objeto_0[[1]]$Data)),
                                   collapse = ' ')),'"]',sep = '')



data_serie <- paste('[',gsub(' ',',',
                             paste(paste(as.vector(objeto_0[[1]]$`Número de mortes`)),
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


write(exportJson0,file = paste('data/',gsub('.csv','',T_ST_P_No_SAUDE$NOME_ARQUIVO_JS[2]),
                               '.json',sep =''))
write(texto,file = paste('data/',T_ST_P_No_SAUDE$NOME_ARQUIVO_JS[2],
                         sep =''))

#}

# Arquivo dedicado a rotina de atualizacao global. 

write_csv2(SAIDA_POVOAMENTO,file ='data/POVOAMENTO.csv',quote='all',escape='none')
#quote="needed")#,escape='none')


objeto_autm <- SAIDA_POVOAMENTO %>% list()
exportJson_aut <- toJSON(objeto_autm)

#write(exportJson_aut,file = paste('data/povoamento.json'))





