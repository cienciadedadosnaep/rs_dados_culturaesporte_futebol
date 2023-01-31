
######################################################
# 1) Carregar bibliotecas

library(tidyverse)
library(magrittr)
#library(dplyr)
library(readr)
library(rjson)
library(RJSONIO)

# # Library para importar dados SQL
# library(DBI)
# library(RMySQL)
# library(pool)
# library(sqldf)
# library(RMariaDB)
# 
# # Carragamento de banco de dados
# 
# # Settings
# db_user <-'admin'
# db_password <-'password'
# db_name <-'cdnaep'
# #db_table <- 'your_data_table'
# db_host <-'127.0.0.1' # for local access
# db_port <-3306
# 
# # 3. Read data from db
# # drv=RMariaDB::MariaDB(),
# mydb <-  dbConnect(drv =RMariaDB::MariaDB(),user =db_user, 
#                    password = db_password ,
#                    dbname = 'cdnaep', host = db_host, port = db_port)
# 
# dbListTables(mydb)
# 
# s <- paste0("SELECT * from", " consumo_agua")
# rs<-NULL
# rs <- dbSendQuery(mydb, s)
# 
# dados<- NULL
# dados <-  dbFetch(rs, n = -1)
# dados
# #dbHasCompleted(rs)
# #dbClearResult(rs)

library(readr)
brasileirao_serie_a <- read_csv("data/brasileirao_serie_a.csv")
View(brasileirao_serie_a)

tab <- brasileirao_serie_a %>% select(ano_campeonato,estadio) %>% 
  filter(estadio == "Arena Fonte Nova")

tab2 <- brasileirao_serie_a %>% select(estadio,ano_campeonato,publico) %>%
  filter(estadio == "Arena Fonte Nova") %>% filter(ano_campeonato >= 2004)

tab3 <- tab2 %>% group_by(ano_campeonato) %>% select(publico) %>%
  summarise(sum(publico))
names(tab3) <- c("ano","Público")

###library(readxl)
###dados_hoteis_ssa2 <- read_excel("data/dados_hoteis_ssa.xlsx")
###View(dados_hoteis_ssa2)

###names(dados_hoteis_ssa2) <- c("ano","Diária média anual","Taxa de ocupação","RevPAR")

dados2 <- tab3 %>% select(ano,`Público`) %>% arrange(ano)

dados2 %<>% mutate(`Público`=`Público`/1000) #Reorganizando a escala dos dados

##  Perguntas e titulos 
T_ST_P_No_Culturaesporte <- read_csv("data/TEMA_SUBTEMA_P_No - CULTURAESPORTE.csv")
dados2 %<>% gather(key = classe,
                   value = publico,-ano) 
#dados %<>% select(-id)
# Temas Subtemas Perguntas



## Arquivo de saida 

SAIDA_POVOAMENTO <- T_ST_P_No_Culturaesporte %>% 
  select(TEMA,SUBTEMA,PERGUNTA,NOME_ARQUIVO_JS)
SAIDA_POVOAMENTO <- as.data.frame(SAIDA_POVOAMENTO)



# Cores secundarias paleta pantone -
corsec_recossa_azul <- c('#175676','#62acd1','#8bc6d2','#20cfef',
                         '#d62839','#20cfef','#fe4641','#175676',
                         '#175676','#62acd1','#8bc6d2','#20cfef')

#for ( i in 1:length(classes)) {

objeto_0 <- dados2 %>%
  filter(classe %in% c(classes[1])) %>%
  select(ano,publico) %>% #filter(ano<2019) %>%
  #arrange(trimestre) %>%
  mutate(ano = as.character(ano)) %>% list()               

exportJson0 <- toJSON(objeto_0)


titulo<-T_ST_P_No_Culturaesporte$TITULO[2]
subtexto<-"Fonte: Transfermarkt"
link <- T_ST_P_No_Culturaesporte$LINK[2]

data_axis <- paste('["',gsub(' ','","',
                             paste(paste(as.vector(objeto_0[[1]]$ano)),
                                   collapse = ' ')),'"]',sep = '')


data_serie <- paste('[',gsub(' ',',',
                             paste(paste(as.vector(objeto_0[[1]]$publico)),
                                   collapse = ' ')),']',sep = '')

texto<-paste('{"title":{"text":"',titulo,
             '","subtext":"',subtexto,
             '","sublink":"',link,'"},',
             '"tooltip":{"trigger":"axis"},',
             '"toolbox":{"left":"center","orient":"horizontal","itemSize":20,"top":45,"show":true,',
             '"feature":{"dataZoom":{"yAxisIndex":"none"},',
             '"dataView":{"readOnly":false},',
             '"restore":{},"saveAsImage":{}}},"xAxis":{"type":"category",',
             '"data":',data_axis,'},',
             '"yAxis":{"type":"value","axisLabel":{"formatter":"{value}mil"}},',
             '"series":[{"data":',data_serie,',',
             '"type":"bar","color":"',corsec_recossa_azul[5],'","showBackground":true,',
             '"backgroundStyle":{"color":"rgba(180, 180, 180, 0.2)"},',
             '"itemStyle":{"borderRadius":10,"borderColor":"',corsec_recossa_azul[5],'","borderWidth":2}}]}',sep='')

#SAIDA_POVOAMENTO$CODIGO[i] <- texto   
texto<-noquote(texto)


write.csv2(exportJson0,file = paste('data/',gsub('.csv','',T_ST_P_No_Culturaesporte$NOME_ARQUIVO_JS[2]),
                               '.json',sep =''),fileEncoding = 'latin1')
write.csv2(texto,file = paste('data/',T_ST_P_No_Culturaesporte$NOME_ARQUIVO_JS[2],
                         sep =''),fileEncoding = 'latin1')

#}

# Arquivo dedicado a rotina de atualizacao global. 

write_csv2(SAIDA_POVOAMENTO,file ='data/POVOAMENTO.csv',quote='all',escape='none')
#quote="needed")#,escape='none')


objeto_autm <- SAIDA_POVOAMENTO %>% list()
exportJson_aut <- toJSON(objeto_autm)

#write(exportJson_aut,file = paste('data/povoamento.json'))