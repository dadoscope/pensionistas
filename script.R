require(tidyverse)
require(lubridate)
require(ggExtra)
library(ggplot2)
library(dplyr)
library(stringr)

# Load Data

setwd("/Users/isiscosta/RScript/milicos")
load("data/pensionistas.RData")



aux <- pensionistas %>% 
  filter(`DATA DE NASCIMENTO`!="00000000") %>%
  filter(stringr::str_starts(`TIPO DE BENEFICIARIO`,"FILHA"))%>%
  mutate(idade = as.numeric(difftime(dmy("31-12-2019"), ymd(`DATA DE NASCIMENTO`), unit="weeks"))/52.25) %>% 
  mutate(milico = str_detect(`NOME DO ORGAO`, "COMANDO")) %>%
  mutate(tempo_de_pensao = as.numeric(difftime(ymd("2019-12-31"),  ymd(`DATA INICIO DO BENEFICIO`)), unit="weeks")/52.25) %>% 
  mutate(orgao = if_else(milico, "Militar","Civil")) %>%
  mutate(prazo_definido = ifelse(is.na(`DATA FIM DO BENEFICIO`),"Prazo Indefinido", "Prazo Definido"))




t.test(aux$idade[aux$prazo_definido == "Prazo Definido"], aux$idade[aux$prazo_definido == "Prazo Indefinido"])

t.test(aux$tempo_de_pensao[aux$prazo_definido == "Prazo Definido" ], aux$tempo_de_pensao[aux$prazo_definido == "Prazo Indefinido"])

t.test(aux$`RENDIMENTO BRUTO`[aux$prazo_definido == "Prazo Definido" & aux$`DATA PROCESSAMENTO`=="Nov/2019"], aux$`RENDIMENTO BRUTO`[aux$prazo_definido == "Prazo Indefinido" & aux$`DATA PROCESSAMENTO`=="Nov/2019"])




aux %>%
  filter(`DATA PROCESSAMENTO`=="Nov/2019")%>%
  ggplot() +
  geom_bar(aes(x=prazo_definido))

aux %>% 
  filter(`DATA PROCESSAMENTO`=="Nov/2019")%>%
  ggplot() +
  geom_bar(aes(x=`TIPO DE BENEFICIARIO`))+
  facet_grid(prazo_definido~., scales="free_y", space = "free_y")+
  coord_flip()

aux %>% 
  filter(`DATA PROCESSAMENTO`=="Nov/2019")%>% 
  ggplot() +
  geom_bar(aes(x=`TIPO DE BENEFICIARIO`))+
  scale_y_log10() +
  facet_grid(prazo_definido~., scales="free_y", space = "free_y")+
  coord_flip()



aux %>% 
  ggplot() +
  geom_boxplot(aes(x=`TIPO DE BENEFICIARIO`, y= idade))+
  facet_grid(prazo_definido~., scales="free_y", space = "free_y")+
  coord_flip()


aux%>%
  ggplot()+
  geom_violin(aes(x=prazo_definido, y= idade))


aux%>%
  filter(`TIPO DE BENEFICIARIO` %in% c("FILHA", "FILHA MAIOR INVALIDA", "FILHA (MILITAR)", "FILHA MAIOR SOLTEIRA SEM CARGO PUBLICO PERMANENTE")) %>%
  ggplot()+
  geom_violin(aes(x=`TIPO DE BENEFICIARIO`, y= idade))

aux%>%
  ggplot()+
  geom_violin(aes(x=prazo_definido, y= tempo_de_pensao))

aux%>%
  filter(`TIPO DE BENEFICIARIO` %in% c("FILHA", "FILHA MAIOR INVALIDA", "FILHA (MILITAR)", "FILHA MAIOR SOLTEIRA SEM CARGO PUBLICO PERMANENTE")) %>%
  ggplot()+
  geom_violin(aes(x=`TIPO DE BENEFICIARIO`, y= tempo_de_pensao))



aux%>%
  filter(`DATA PROCESSAMENTO`=="Nov/2019")%>% 
  filter(`TIPO DE BENEFICIARIO` %in% c("FILHA", "FILHA MAIOR INVALIDA", "FILHA (MILITAR)", "FILHA MAIOR SOLTEIRA SEM CARGO PUBLICO PERMANENTE")) %>%
  ggplot()+
  geom_violin(aes(x=`TIPO DE BENEFICIARIO`, y= `RENDIMENTO BRUTO`))+
  scale_y_log10()



aux%>%
  filter(`TIPO DE BENEFICIARIO` %in% c("FILHA", "FILHA MAIOR INVALIDA", "FILHA (MILITAR)", "FILHA MAIOR SOLTEIRA SEM CARGO PUBLICO PERMANENTE"),
         `DATA PROCESSAMENTO`=="Nov/2019") %>%
  ggplot() +
  geom_bar() +
  aes(x= `TIPO PENSAO` , fill= `TIPO DE BENEFICIARIO`) +
  coord_flip()
