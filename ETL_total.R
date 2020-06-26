library(purrr)
library(dplyr)
library(stringr)
library(lubridate)




faz_download<- FALSE
df_pensionista_1994_2019<-
map_dfr(1994:2019,function(a_ano){
  
  
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
    
    as.period(interval(start =  dmy('20011936'), end = dmy(paste0("01",a_mes, a_ano)) ))$year
    as.period(interval(start =  dmy(`DATA DE NASCIMENTO`), end = dmy(paste0("01",a_mes, a_ano)) ))$year
    
    df_pensionista<- 
    df_pensionista %>%
      mutate(`DATA DE NASCIMENTO` = dmy(`DATA DE NASCIMENTO`)) %>%
      mutate(idade = as.period(interval(start =  `DATA DE NASCIMENTO`, end = dmy(paste0("01",a_mes, a_ano)) ))$year) 
      
    
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
                `PAGAMENTO SUSPENSO`
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

df_pensionista_2020<-map_dfr(meses, function(a_mes){
  address<- paste0("http://repositorio.dados.gov.br/segrt/pensionistas/PENSIONISTAS_", a_mes, a_ano, ".zip")
  print(address)
  
  print(a_mes)
  dest<- paste0("data/",a_ano,"/", "PENSIONISTAS_", a_mes, a_ano,".zip")
  print(dest)
  
  download.file(address, destfile = dest,   mode="wb")
  
  
  
  unzip(dest, exdir = paste0("data/",a_ano))
  
  
  file.remove(dest)
  
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
              `PAGAMENTO SUSPENSO`
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


df_pensionista_total<-
  df_pensionista_1994_1995%>%
  bind_rows(df_pensionista_1996_2019,
            df_pensionista_2020)

df_pensionista_total<-
df_pensionista_total %>%
  ungroup() %>%
  mutate(min_data_inicio_beneficio = dmy(min_data_inicio_beneficio),
         `Data Processamento` = dmy(`Data Processamento`))

    
saveRDS(df_pensionista_1994_1995, file="df_pensionista_1994_1995.rds")
saveRDS(df_pensionista_1996_2019, file="df_pensionista_1996_2019.rds")
saveRDS(df_pensionista_2020, file="df_pensionista_2020.rds")

saveRDS(df_pensionista_total, file= "df_pensionista_total.rds")

write.csv2(df_pensionista_total,file="pensionistas.csv")

