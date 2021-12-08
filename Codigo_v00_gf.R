# Dados de consumo anual de água por classe em Salvador 
# Data criacao 01/9/2021
# Tabela de dados obtida do site 
# http://sim.sei.ba.gov.br/metaside/consulta/frame_metadados.wsp?tmp.tabela=t128

######################################################
# 1) Carregar bibliotecas

library(tidyverse)
library(magrittr)
#library(dplyr)
library(readr)
library(rjson)
library(RJSONIO)

library(readr)
dados_farmacia <- read_delim("data/dados_farmacia.csv", 
                                   delim = ";", escape_double = FALSE, trim_ws = TRUE)
names(dados_farmacia)


#dbHasCompleted(rs)
#dbClearResult(rs)

dados <- dados_farmacia %>% select(`Ano`,
                                         `FARMACIAS`) 

names(dados) = c("ano","farmacias")


dados %<>% gather(key = classe,
                  value = farmacias,-ano) 


# Temas Subtemas Perguntas

##  Perguntas e titulos 
T_ST_P_No_SAUDE <- read_csv("data/TEMA_SUBTEMA_P_No - SAUDE.csv")


## Arquivo de saida 

SAIDA_POVOAMENTO <- T_ST_P_No_SAUDE %>% 
  select(TEMA,SUBTEMA,PERGUNTA,NOME_ARQUIVO_JS)
SAIDA_POVOAMENTO <- as.data.frame(SAIDA_POVOAMENTO)

classes <- NULL
classes <- levels(as.factor(dados$classe))

# Cores secundarias paleta pantone -
corsec_recossa_vermelho <- c('#fe4641')#c('#175676','#62acd1','#8bc6d2','#20cfef')

#for ( i in 1:length(classes)) {

objeto_0 <- dados %>%
  filter(classe %in% c(classes[1])) %>%
  select(ano,farmacias) %>% 
  arrange(ano) %>%
  mutate(ano = as.character(ano)) %>% list()               

exportJson0 <- toJSON(objeto_0)


titulo<-T_ST_P_No_SAUDE$TITULO[1]
subtexto<-"DATASUS"
link <- T_ST_P_No_SAUDE$LINK[1]

data_axis <- paste('[',gsub(' ',',',
                            paste(paste(as.vector(objeto_0[[1]]$ano)),
                                  collapse = ' ')),']',sep = '')

data_serie <- paste('[',gsub(' ',',',
                             paste(paste(as.vector(objeto_0[[1]]$farmacias)),
                                   collapse = ' ')),']',sep = '')

texto<-paste('{"title":{"text":"',titulo,
             '","subtext":"',subtexto,
             '","sublink":"',link,'"},',
             '"tooltip":{"trigger":"axis"},',
             '"toolbox":{"left":"center","orient":"horizontal","itemSize":20,"top":45,"show":true,',
             '"feature":{"dataZoom":{"yAxisIndex":"none"},',
             '"dataView":{"readOnly":false},"magicType":{"type":["line","bar"]},',
             '"restore":{},"saveAsImage":{}}},"xAxis":{"type":"category",',
             '"data":',data_axis,'},',
             '"yAxis":{"type":"value","axisLabel":{"formatter":"{value}"}},',
             '"series":[{"data":',data_serie,',',
             '"type":"bar","color":"',corsec_recossa_vermelho[1],'","showBackground":true,',
             '"backgroundStyle":{"color":"rgba(180, 180, 180, 0.2)"},',
             '"itemStyle":{"borderRadius":10,"borderColor":"',corsec_recossa_vermelho[1],'","borderWidth":2}}]}',sep='')

#  SAIDA_POVOAMENTO$CODIGO[i] <- texto   
texto<-noquote(texto)


write(exportJson0,file = paste('data/',gsub('.csv','',T_ST_P_No_SAUDE$NOME_ARQUIVO_JS[1]),
                               '.json',sep =''))
write(texto,file = paste('data/',T_ST_P_No_SAUDE$NOME_ARQUIVO_JS[1],
                         sep =''))

#}

# Arquivo dedicado a rotina de atualizacao global. 

write_csv2(SAIDA_POVOAMENTO,file ='data/POVOAMENTO.csv',quote='all',escape='none')
#quote="needed")#,escape='none')


objeto_autm <- SAIDA_POVOAMENTO %>% list()
exportJson_aut <- toJSON(objeto_autm)

#write(exportJson_aut,file = paste('data/povoamento.json'))

