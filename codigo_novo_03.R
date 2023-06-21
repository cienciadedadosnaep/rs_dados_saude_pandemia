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
intervalos_idade <- c(0, 14, 29, 44, 59, 74, Inf)
dados$faixa_etaria <- cut(dados$IDADE, breaks = intervalos_idade, labels = FALSE,  right = FALSE)
glimpse(dados)

#mudar nomes de faixa etária
dados <- dados |>
  mutate(faixa_etaria = as.character(faixa_etaria))
dados <- dados |>
  mutate(faixa_etaria = recode(faixa_etaria, "1" = "0-14"))
dados <- dados |>
  mutate(faixa_etaria = recode(faixa_etaria, "2" = "15-29"))
dados <- dados |>
  mutate(faixa_etaria = recode(faixa_etaria, "3" = "30-44"))
dados <- dados |>
  mutate(faixa_etaria = recode(faixa_etaria, "4" = "45-59"))
dados <- dados |>
  mutate(faixa_etaria = recode(faixa_etaria, "5" = "60-74"))
dados <- dados |>
  mutate(faixa_etaria = recode(faixa_etaria, "6" = "75+"))



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

names(dados4) <- c("Mês", "0-14", "15-29", "30-44", "45-59", "60-74", "75+")
nomes <- names(dados4)
glimpse(dados4)

dados4 <- dados4 |>
  mutate(Mês = recode(Mês, "dezembro 2021 ," = "dezembro 2021"))
#view(dados3)
#view(dados4)
glimpse(dados4)

dados4 %<>% mutate(`0-14`=round(`0-14`/1000,1))
dados4 %<>% mutate(`15-29`=round(`15-29`/1000,1))
dados4 %<>% mutate(`30-44`=round(`30-44`/1000,1))
dados4 %<>% mutate(`45-59`=round(`45-59`/1000,1))
dados4 %<>% mutate(`60-74`=round(`60-74`/1000,1))
dados4 %<>% mutate(`75+`=round(`75+`/1000,1))

## Arquivo de saida 

SAIDA_POVOAMENTO <- T_ST_P_No_SAUDE %>% 
  select(TEMA,SUBTEMA,PERGUNTA,NOME_ARQUIVO_JS)
SAIDA_POVOAMENTO <- as.data.frame(SAIDA_POVOAMENTO)

#classes <- NULL
#classes <- levels(as.factor(dados_ca$classe))
# 
# # Cores secundarias paleta pantone -
# corsec_recossa_azul <- c('#175676','#62acd1','#8bc6d2','#20cfef',
#                          '#d62839','#20cfef','#fe4641','#175676',
#                          '#175676','#62acd1','#8bc6d2','#20cfef')
# 
# #for ( i in 1:length(classes)) {
# dados_f <- NULL
# dados_f <- data_serie
#############################################################################################################

#classes <- NULL
#classes <- levels(as.factor(dados$classe))

# Cores secundarias paleta pantone -
corsec_recossa_azul <- c('#a094e1','#dc6f6c','#62acd1','#8bc6d2',
                         '#d62839','#20cfef','#fe4641','#175676')
# Cor 1 - Roxo; Cor 2, 5, 7 - Vermelho; Cor 3, 4, 6, 8 - Azul

simbolo_linhas <- c('emptyCircle','emptyTriangle','emptySquare',
                    'emptyDiamond','emptyRoundRect')


objeto_0 <- dados4 %>%
  #filter(classe %in% c(classes[1])) %>%
  select(Mês,`0-14`,`15-29`,`30-44`,`45-59`,`60-74`,`75+`) %>% #filter(ano<2019) %>%) %>% #filter(ano<2019) %>%
  #arrange(trimestre) %>%
  mutate(ano = as.character(Mês)) %>% list()               

exportJson0 <- toJSON(objeto_0)


titulo<-T_ST_P_No_SAUDE$TITULO[5]
subtexto<-"Fonte: Saúde Salvador"
link <- T_ST_P_No_SAUDE$LINK[5]




data_axis <- paste('["',gsub(',','","',
                             paste(paste(as.vector(objeto_0[[1]]$Mês)),
                                   collapse = ' ')),'"]',sep = '')


data_serie <- paste('[',gsub(' ',',',
                             paste(paste(as.vector(objeto_0[[1]]$`0-14`)),
                                   collapse = ' ')),']',sep = '')

data_serie2 <- paste('[',gsub(' ',',',
                              paste(paste(as.vector(objeto_0[[1]]$`15-29`)),
                                    collapse = ' ')),']',sep = '')
data_serie3 <- paste('[',gsub(' ',',',
                              paste(paste(as.vector(objeto_0[[1]]$`30-44`)),
                                    collapse = ' ')),']',sep = '')
data_serie4 <- paste('[',gsub(' ',',',
                              paste(paste(as.vector(objeto_0[[1]]$`45-59`)),
                                    collapse = ' ')),']',sep = '')

data_serie5 <- paste('[',gsub(' ',',',
                              paste(paste(as.vector(objeto_0[[1]]$`60-74`)),
                                    collapse = ' ')),']',sep = '')
data_serie6 <- paste('[',gsub(' ',',',
                              paste(paste(as.vector(objeto_0[[1]]$`75+`)),
                                    collapse = ' ')),']',sep = '')



texto <- paste('{"title":{"text":"',titulo,
               '","subtext":"',subtexto,
               '","sublink":"',link,'"},',
               '"tooltip":{"trigger":"item","responsive":"true","position":"top","formatter":"{c0} mil"},',
               '"toolbox":{"left":"center","orient":"horizontal","itemSize":20,"top":20,"show":true,',
               '"feature":{"dataZoom":{"yAxisIndex":"none"},',
               '"dataView":{"readOnly":false},',
               '"restore":{},"saveAsImage":{}}},"legend":{"show":true,"bottom":10},"grid":{"bottom":80},"xAxis":{"type":"category",',
               '"data":',data_axis,'},',
               '"yAxis":{"type":"value","axisLabel":{"formatter":"{value} mil"}},',
               '"graphic":[{"type":"text", "left":"center","top":"bottom","z":100, "style":{"fill":"gray","text":"Obs: Ponto é separador decimal", "font":"8px sans-srif","fontSize":12}}],',
               '"series":[{"name":"',nomes[2],'","data":',data_serie,',',
               '"type":"line","color":"',corsec_recossa_azul[6],'","showBackground":true,',
               '"backgroundStyle":{"color":"rgba(180, 180, 180, 0.2)"},"symbol":"',simbolo_linhas[1],
               '","symbolSize":10,"itemStyle":{"borderRadius":10,"borderColor":"',corsec_recossa_azul[6],'","borderWidth":2}},',
               '{"name":"',nomes[3],'","data":',data_serie2,',',
               '"type":"line","color":"',corsec_recossa_azul[7],'","showBackground":true,',
               '"backgroundStyle":{"color":"rgba(180, 180, 180, 0.2)"},"symbol":"',simbolo_linhas[2],
               '","symbolSize":10,"itemStyle":{"borderRadius":10,"borderColor":"',corsec_recossa_azul[7],'","borderWidth":2}},',
               '{"name":"',nomes[4],'","data":',data_serie3,',',
               '"type":"line","color":"',corsec_recossa_azul[8],'","showBackground":true,',
               '"backgroundStyle":{"color":"rgba(180, 180, 180, 0.2)"},"symbol":"',simbolo_linhas[3],
               '","symbolSize":10,"itemStyle":{"borderRadius":10,"borderColor":"',corsec_recossa_azul[8],'","borderWidth":2}},',
               '{"name":"',nomes[5],'","data":',data_serie4,',',
               '"type":"line","color":"',corsec_recossa_azul[1],'","showBackground":true,',
               '"backgroundStyle":{"color":"rgba(180, 180, 180, 0.2)"},"symbol":"',simbolo_linhas[4],
               '","symbolSize":10,"itemStyle":{"borderRadius":10,"borderColor":"',corsec_recossa_azul[1],'","borderWidth":2}},',
               '{"name":"',nomes[6],'","data":',data_serie5,',',
               '"type":"line","color":"',corsec_recossa_azul[3],'","showBackground":true,',
               '"backgroundStyle":{"color":"rgba(180, 180, 180, 0.2)"},"symbol":"',simbolo_linhas[5],
               '","symbolSize":10,"itemStyle":{"borderRadius":10,"borderColor":"',corsec_recossa_azul[3],'","borderWidth":2}},',
               '{"name":"',nomes[7],'","data":',data_serie6,',',
               '"type":"line","color":"',corsec_recossa_azul[5],'","showBackground":true,',
               '"backgroundStyle":{"color":"rgba(180, 180, 180, 0.2)"},"symbol":"',simbolo_linhas[4],
               '","symbolSize":10,"itemStyle":{"borderRadius":10,"borderColor":"',corsec_recossa_azul[5],'","borderWidth":2}}',
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



#view(dados4)
