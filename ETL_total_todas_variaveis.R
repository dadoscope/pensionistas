library(purrr)
library(dplyr)
library(stringr)
library(lubridate)
library(readxl)
library(tidyverse)

#Para entender o que foi feito para trazer para valores presentes os valores históricos inclusive do período pré plano real, procure pelo comentário indicado abaixo
#################Cálculos econômicos


#################Cálculos econômicos
#Dados do IPCA com a série temporal
IPCA_historico <- read_delim("bcdata.sgs.4449.csv", 
                              ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                          grouping_mark = "."), trim_ws = TRUE)

#################Cálculos econômicos
#Calcula o número índice para todos os meses da série temporal
num_index<-
map_dbl(1:341, function(a_i){
  if (a_i==1){
    100
  }else {
    100+ prod(1+IPCA_historico$valor[1:a_i]/100)
  }
  
})


#################Cálculos econômicos
#Acrescenta a coluna do número índice
IPCA_historico$num_indice <- num_index

IPCA_historico<-
IPCA_historico %>%
  mutate(data= dmy(data)) %>%
  select(data,num_indice)


num_index_mai_2020<-25995.97 

faz_download<- FALSE

#df_pensionista_1994_1995<-

for (a_ano in c (1996:2019) ){

#map_dfr(1994:1995,function(a_ano){
  
  
  if (faz_download){
    
    address<- paste0("http://repositorio.dados.gov.br/segrt/pensionistas/", a_ano, ".zip")
    print(address)
    
    download.file(address, destfile = paste0("data/", a_ano,".zip"),   mode="wb")
    
    
    
    unzip(paste0("data/", a_ano,".zip"), exdir = "data")
    
    
    file.remove(paste0("data/", a_ano,".zip"))
    
    
  }
  
  
  meses<- c("01", "02","03","04","05","06","07","08","09","10","11", "12")
  
  df_pensionistas<-
  map_dfr(meses, function(a_mes){
    
    print(a_mes)
    
    gz_gile<-paste0("data/", a_ano, "/PENSIONISTAS_", a_mes, a_ano, ".csv.gz")
    
    
    df_pensionista<- read_delim(gzfile(gz_gile), 
                                ";", escape_double = FALSE,  
                                trim_ws = TRUE, locale = locale(encoding = "LATIN1"))
    

    df_pensionista<- 
    df_pensionista %>%
      mutate(`DATA DE NASCIMENTO` = dmy(`DATA DE NASCIMENTO`)) %>%
      mutate(idade = as.period(interval(start =  `DATA DE NASCIMENTO`, end = dmy(paste0("01",a_mes, a_ano)) ))$year)%>%
      mutate(faixa_etaria = case_when(
        idade <9 ~"0 a 9",
        idade %in% 10:19 ~ "10-19",
        idade %in% 20:29 ~ "20-29",
        idade %in% 30:39 ~ "30-39",
        idade %in% 40:49 ~ "40-49",
        idade %in% 50:59 ~ "50-59",
        idade %in% 60:69 ~ "60-69",
        idade %in% 70:79 ~ "70-79",
        idade>80   ~ "80 ou  mais"
        
      ))
      
    
    
    
    df_pensionista<-
      df_pensionista %>%
      mutate(`RENDIMENTO BRUTO` = stringr::str_remove_all(`RENDIMENTO BRUTO`,"[.]"),
             `RENDIMENTO BRUTO` = as.numeric(stringr::str_remove_all(`RENDIMENTO BRUTO`,","))/100,
             `RENDIMENTO LIQUIDO` = stringr::str_remove_all(`RENDIMENTO LIQUIDO`,"[.]"),
             `RENDIMENTO LIQUIDO` = as.numeric(stringr::str_remove_all(`RENDIMENTO LIQUIDO`,","))/100)
    
    
    data_processamento<- paste0("01",a_mes,a_ano)
    
    print(data_processamento)
    
    #################Cálculos econômicos
    #Aqui tem o cálculo do rendimento bruto corrigido e também do rendimento líquido corrigido
    
    
    df_pensionista<-
    df_pensionista%>%
      mutate(data = dmy(data_processamento)) %>%
      inner_join(IPCA_historico) %>%
      mutate(tipo_prazo = ifelse(`DATA FIM DO BENEFICIO` == "00000000","Prazo Indefinido", "Prazo Definido"),
             rend_bruto_corrigido = ifelse(data < dmy("01071994"), (`RENDIMENTO BRUTO`/2750) * num_index_mai_2020/num_indice, `RENDIMENTO BRUTO` * num_index_mai_2020/num_indice),
             rend_liq_corrigido   = ifelse(data < dmy("01071994"), (`RENDIMENTO LIQUIDO`/2750) * num_index_mai_2020/num_indice, `RENDIMENTO LIQUIDO` * num_index_mai_2020/num_indice))
    
    
    
    
  })
  
  
  
  write.csv2(df_pensionistas,file=paste0("data/csv/",a_ano,".csv"))
  
  rm(list= "df_pensionistas")
  
}  
#})


#http://repositorio.dados.gov.br/segrt/pensionistas/PENSIONISTAS_012020.zip
#http://repositorio.dados.gov.br/segrt/pensionistas/PENSIONISTAS_012020.zip
#http://repositorio.dados.gov.br/segrt/pensionistas/012020.zip



meses<- c("01", "02","03","04","05")
a_ano<- 2020
faz_download<- FALSE


for (a_mes in meses){
#df_pensionista_2020<-map_dfr(meses, function(a_mes){
  
  if (faz_download){
    address<- paste0("http://repositorio.dados.gov.br/segrt/pensionistas/PENSIONISTAS_", a_mes, a_ano, ".zip")
    print(address)
    
    print(a_mes)
    dest<- paste0("data/",a_ano,"/", "PENSIONISTAS_", a_mes, a_ano,".zip")
    print(dest)
    
    download.file(address, destfile = dest,   mode="wb")
    
    
    
    unzip(dest, exdir = paste0("data/",a_ano))
    
    
    file.remove(dest)

  }
  
  if (a_mes %in% c("01","02","03")){
    df_file<- paste0("data/2020/arquivo-",a_ano,a_mes,".csv")
    
  } else{
    df_file<- paste0("data/2020/PENSIONISTAS_",a_mes,a_ano,".csv")
  }
    
  
  
  
  df_pensionista <- read_delim(df_file, 
                               ";", escape_double = FALSE, col_types = cols(`DATA FIM DO BENEFICIO` = col_character(), 
                                                                            `DATA INICIO DO BENEFICIO` = col_character()), 
                               locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "LATIN1"), 
                               trim_ws = TRUE)
  
  data_processamento<- paste0("01",a_mes,a_ano)
  
  df_pensionista<- 
    df_pensionista %>%
    mutate(`DATA DE NASCIMENTO` = dmy(`DATA DE NASCIMENTO`)) %>%
    mutate(idade = as.period(interval(start =  `DATA DE NASCIMENTO`, end = dmy(paste0("01",a_mes, a_ano)) ))$year)%>%
    mutate(faixa_etaria = case_when(
      idade <9 ~"0 a 9",
      idade %in% 10:19 ~ "10-19",
      idade %in% 20:29 ~ "20-29",
      idade %in% 30:39 ~ "30-39",
      idade %in% 40:49 ~ "40-49",
      idade %in% 50:59 ~ "50-59",
      idade %in% 60:69 ~ "60-69",
      idade %in% 70:79 ~ "70-79",
      idade>80   ~ "80 ou  mais"
      
    ))
  
  
  df_pensionista<-
  df_pensionista%>%
    mutate(data = dmy(data_processamento)) %>%
    inner_join(IPCA_historico) %>%
    mutate(`DATA INICIO DO BENEFICIO` = ifelse( str_length(`DATA INICIO DO BENEFICIO`)==7,str_c("0",`DATA INICIO DO BENEFICIO`),`DATA INICIO DO BENEFICIO`),
           `DATA FIM DO BENEFICIO` = ifelse( str_length(`DATA FIM DO BENEFICIO`)==7,str_c("0",`DATA FIM DO BENEFICIO`),`DATA FIM DO BENEFICIO`),
           tipo_prazo = ifelse(as.numeric(`DATA FIM DO BENEFICIO`) == 0,"Prazo Indefinido", "Prazo Definido"),
           rend_bruto_corrigido = ifelse(data < dmy("01071994"), (`RENDIMENTO BRUTO`/2750) * num_index_mai_2020/num_indice, `RENDIMENTO BRUTO` * num_index_mai_2020/num_indice),
           rend_liq_corrigido   = ifelse(data < dmy("01071994"), (`RENDIMENTO LIQUIDO`/2750) * num_index_mai_2020/num_indice, `RENDIMENTO LIQUIDO` * num_index_mai_2020/num_indice))
  
  write.csv2(df_pensionista,file=paste0("data/csv/",data_processamento,".csv"))
  
  rm(list= "df_pensionista")
  
}
#})


df_pensionista_total<-
  df_pensionista_1994_1995%>%
  bind_rows(df_pensionista_1996_2019,
            df_pensionista_2020)

df_pensionista_total<-
df_pensionista_total %>%
  ungroup() %>%
  mutate(min_data_inicio_beneficio = dmy(min_data_inicio_beneficio),
         `Data Processamento` = dmy(`Data Processamento`))




saveRDS(df_pensionista_1994_1995, file="df_pensionista_1994_1995_new.rds")
saveRDS(df_pensionista_1996_2019, file="df_pensionista_1996_2019_new.rds")
saveRDS(df_pensionista_2020, file="df_pensionista_2020_new.rds")
saveRDS(df_pensionista_total, file= "df_pensionista_total_new.rds")

write.csv2(df_pensionista_total,file="pensionistas_new.csv")
    
saveRDS(df_pensionista_1994_1995, file="df_pensionista_1994_1995.rds")
saveRDS(df_pensionista_1996_2019, file="df_pensionista_1996_2019.rds")
saveRDS(df_pensionista_2020, file="df_pensionista_2020.rds")

saveRDS(df_pensionista_total, file= "df_pensionista_total.rds")

write.csv2(df_pensionista_total,file="pensionistas.csv")

