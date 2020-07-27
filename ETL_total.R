library(purrr)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyverse)

casos_outlierns_um_mi <- read_excel("casos_outlierns_um_mi.xlsx")

casos_outlierns_um_mi <- casos_outlierns_um_mi[-c(40,41),]

ano_outlier<-unique(casos_outlierns_um_mi$Ano)

casos_outliers<-
  casos_outlierns_um_mi %>%
  mutate(data_processamento = paste(Ano,ifelse(as.numeric(Mes)<10,paste0("0",Mes),Mes),"01", sep="-"),
         `MATRICULA SERVIDOR INSTITUIDOR`= as.numeric(`Matricula Servidor Instituidor`),
         `NOME DO BENEFICIARIO` = `Nome Do Beneficiario`) %>%
  select(data_processamento,`MATRICULA SERVIDOR INSTITUIDOR`, `NOME DO BENEFICIARIO`)



anos<- ano_outlier

processa_outliers<- TRUE

faz_download<- FALSE

df_pensionista_outlier<-
map_dfr(anos,function(a_ano){
  
  
  if (faz_download){
    
    address<- paste0("http://repositorio.dados.gov.br/segrt/pensionistas/", a_ano, ".zip")
    print(address)
    
    download.file(address, destfile = paste0("data/", a_ano,".zip"),   mode="wb")
    
    
    
    unzip(paste0("data/", a_ano,".zip"), exdir = "data")
    
    
    file.remove(paste0("data/", a_ano,".zip"))
    
    
  }
  
  
  meses<- c("01", "02","03","04","05","06","07","08","09","10","11", "12")
  
  
  map_dfr(meses, function(a_mes){
    
    print(a_mes)
    
    gz_gile<-paste0("data/", a_ano, "/PENSIONISTAS_", a_mes, a_ano, ".csv.gz")
    
    
    df_pensionista<- read_delim(gzfile(gz_gile), 
                                ";", escape_double = FALSE,  
                                trim_ws = TRUE)
    

    #data_processamento<- paste0("01",a_mes,a_ano)
    data_processamento<- paste(a_ano,a_mes,"01", sep="-")
    
    df_pensionista$data_processamento<- data_processamento
    
    if (processa_outliers){
      df_pensionista<-
        df_pensionista %>%
        anti_join(casos_outliers)
    }
    

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
      
    
    print(gzfile)
    
    
    df_pensionista<-
      df_pensionista %>%
      mutate(`RENDIMENTO BRUTO` = stringr::str_remove_all(`RENDIMENTO BRUTO`,"[.]"),
             `RENDIMENTO BRUTO` = as.numeric(stringr::str_remove_all(`RENDIMENTO BRUTO`,","))/100,
             `RENDIMENTO LIQUIDO` = stringr::str_remove_all(`RENDIMENTO LIQUIDO`,"[.]"),
             `RENDIMENTO LIQUIDO` = as.numeric(stringr::str_remove_all(`RENDIMENTO LIQUIDO`,","))/100)
    
    
    
    data_processamento<- paste0("01",a_mes,a_ano)
    
    print(data_processamento)
    
    
    
    df_pensionista%>%
      mutate(`Data Processamento` = data_processamento,
             tipo_prazo = ifelse(`DATA FIM DO BENEFICIO` == "00000000","Prazo Indefinido", "Prazo Definido")) %>%
      group_by( `Data Processamento`,
                `TIPO DE BENEFICIARIO`,
                `TIPO PENSAO`,
                `NATUREZA PENSAO`,
                tipo_prazo,
                `PAGAMENTO SUSPENSO`,
                faixa_etaria
      ) %>%
      summarise(
        total_rendimento_bruto = sum(`RENDIMENTO BRUTO`),
        max_rendimento_bruto = max(`RENDIMENTO BRUTO`),
        total_rendimento_iquido = sum(`RENDIMENTO LIQUIDO`),
        max_rendimento_liquido = max(`RENDIMENTO LIQUIDO`),
        min_data_inicio_beneficio = min(`DATA INICIO DO BENEFICIO`),
        quantidade = n()
        
      )
    
    
  })
  
  
  
})


#http://repositorio.dados.gov.br/segrt/pensionistas/PENSIONISTAS_012020.zip
#http://repositorio.dados.gov.br/segrt/pensionistas/PENSIONISTAS_012020.zip
#http://repositorio.dados.gov.br/segrt/pensionistas/012020.zip



meses<- c("01", "02","03","04","05")
a_ano<- 2020
faz_download<- FALSE
df_pensionista_2020<-map_dfr(meses, function(a_mes){
  
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
                               locale = locale(decimal_mark = ",", grouping_mark = "."), 
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
  
  
  
  df_pensionista%>%
    mutate(`Data Processamento` = data_processamento,
           `DATA INICIO DO BENEFICIO` = ifelse( str_length(`DATA INICIO DO BENEFICIO`)==7,str_c("0",`DATA INICIO DO BENEFICIO`),`DATA INICIO DO BENEFICIO`),
           `DATA FIM DO BENEFICIO` = ifelse( str_length(`DATA FIM DO BENEFICIO`)==7,str_c("0",`DATA FIM DO BENEFICIO`),`DATA FIM DO BENEFICIO`),
           tipo_prazo = ifelse(as.numeric(`DATA FIM DO BENEFICIO`) == 0,"Prazo Indefinido", "Prazo Definido")) %>%
    group_by( `Data Processamento`,
              `TIPO DE BENEFICIARIO`,
              `TIPO PENSAO`,
              `NATUREZA PENSAO`,
              tipo_prazo,
              `PAGAMENTO SUSPENSO`,
              faixa_etaria
    ) %>%
    summarise(
      total_rendimento_bruto = sum(`RENDIMENTO BRUTO`),
      max_rendimento_bruto = max(`RENDIMENTO BRUTO`),
      total_rendimento_iquido = sum(`RENDIMENTO LIQUIDO`),
      max_rendimento_liquido = max(`RENDIMENTO LIQUIDO`),
      min_data_inicio_beneficio = min(`DATA INICIO DO BENEFICIO`),
      quantidade = n()
      
    )
  

})


df_pensionista_outlier_COPY<- df_pensionista_outlier

df_pensionista_outlier<-
  df_pensionista_outlier %>%
  ungroup() %>%
  mutate(min_data_inicio_beneficio = dmy(min_data_inicio_beneficio),
         `Data Processamento` = dmy(`Data Processamento`))

df_trabalho<-
df_trabalho %>%
  filter(!(lubridate::year(`Data Processamento`) %in% anos)) %>%
  bind_rows(df_pensionista_outlier)

saveRDS(df_trabalho, "df_trabalho.rds")

glimpse(df_pensionista_outlier)
  


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

