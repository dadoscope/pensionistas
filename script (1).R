require(tidyverse)
require(lubridate)
require(ggExtra)
library(ggplot2)
library(dplyr)
library(stringr)
library(viridis)

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
  theme_light()+
  labs(x="Tipo de beneficiário",
       y="Número de pensionistas")+
  scale_y_continuous(labels=function(y) format(y, big.mark = ".", scientific = FALSE))+
  coord_flip() 

aux %>% 
  filter(`DATA PROCESSAMENTO`=="Nov/2019")%>% 
  ggplot() +
  geom_bar(aes(x=`TIPO DE BENEFICIARIO`))+
  scale_y_log10(labels=function(y) format(y, big.mark = ".", scientific = FALSE)) +
  facet_grid(prazo_definido~., scales="free_y", space = "free_y")+
  theme_light()+
  labs(x="Tipo de beneficiário",
       y="Número de pensionistas")+
  coord_flip()



aux %>% 
  ggplot() +
  geom_boxplot(aes(x=`TIPO DE BENEFICIARIO`, y= idade))+
  facet_grid(prazo_definido~., scales="free_y", space = "free_y")+
  coord_flip()


aux%>%
  ggplot()+
  geom_violin(aes(x=prazo_definido, y= idade, fill=prazo_definido))+
  theme_light()+
  theme(
    legend.title = element_blank(),
    axis.text.x = element_blank()
  )+
  labs(x="",
       y="Idade de pensionistas")+
  viridis::scale_fill_viridis(discrete = TRUE)


aux%>%
  filter(`TIPO DE BENEFICIARIO` %in% c("FILHA", "FILHA MAIOR INVALIDA", "FILHA (MILITAR)", "FILHA MAIOR SOLTEIRA SEM CARGO PUBLICO PERMANENTE")) %>%
  ggplot()+
  geom_violin(aes(x=`TIPO DE BENEFICIARIO`, y= idade, fill= `TIPO DE BENEFICIARIO`))+
  theme_light()+
  theme(
    legend.title = element_blank(),
    axis.text.x = element_blank(),
    legend.position = "bottom"
  )+
  labs(x="",
       y="Idade de pensionistas")+
  viridis::scale_fill_viridis(discrete = TRUE)

aux%>%
  ggplot()+
  geom_violin(aes(x=prazo_definido, y= tempo_de_pensao, fill = prazo_definido))+
  theme_light()+
  theme(
    legend.title = element_blank(),
    axis.text.x = element_blank()
  )+
  labs(x="",
       y="Temp de pensão")+
  viridis::scale_fill_viridis(discrete = TRUE)


aux%>%
  filter(`TIPO DE BENEFICIARIO` %in% c("FILHA", "FILHA MAIOR INVALIDA", "FILHA (MILITAR)", "FILHA MAIOR SOLTEIRA SEM CARGO PUBLICO PERMANENTE")) %>%
  ggplot()+
  geom_violin(aes(x=`TIPO DE BENEFICIARIO`, y= tempo_de_pensao, fill= `TIPO DE BENEFICIARIO`))+
  theme_light()+
  theme(
    legend.title = element_blank(),
    axis.text.x = element_blank(),
    legend.position = "bottom"
  )+
  labs(x="",
       y="Tempo de pensão")+
  viridis::scale_fill_viridis(discrete = TRUE)


aux%>%
  filter(`DATA PROCESSAMENTO`=="Nov/2019")%>% 
  filter(`TIPO DE BENEFICIARIO` %in% c("FILHA", "FILHA MAIOR INVALIDA", "FILHA (MILITAR)", "FILHA MAIOR SOLTEIRA SEM CARGO PUBLICO PERMANENTE")) %>%
  ggplot()+
  geom_violin(aes(x=`TIPO DE BENEFICIARIO`, y= `RENDIMENTO BRUTO`, fill = `TIPO DE BENEFICIARIO`))+
  scale_y_log10(labels=function(y) format(y, big.mark = ".", scientific = FALSE))+ 
  theme_light()+
  theme(
    legend.title = element_blank(),
    axis.text.x = element_blank(),
    legend.position = "bottom"
  )+
  labs(x="",
       y="Rendimento bruto")+
  viridis::scale_fill_viridis(discrete = TRUE)




aux%>%
  filter(`DATA PROCESSAMENTO`=="Nov/2019")%>% 
  ggplot()+
  geom_violin(aes(x=prazo_definido, y= `RENDIMENTO BRUTO`, fill = prazo_definido))+
  scale_y_log10(labels=function(y) format(y, big.mark = ".", scientific = FALSE))+ 
  theme_light()+
  theme(
    legend.title = element_blank(),
    axis.text.x = element_blank()
  )+
  labs(x="",
       y="Rendimento bruto")+
  viridis::scale_fill_viridis(discrete = TRUE)



aux%>%
  filter(`TIPO DE BENEFICIARIO` %in% c("FILHA", "FILHA MAIOR INVALIDA", "FILHA (MILITAR)", "FILHA MAIOR SOLTEIRA SEM CARGO PUBLICO PERMANENTE"),
         `DATA PROCESSAMENTO`=="Nov/2019") %>%
  group_by( `TIPO DE BENEFICIARIO`, `TIPO PENSAO`) %>%
  summarise(
    quantidade = n()
  ) %>%
  ungroup() %>%
  arrange(quantidade)%>%
  mutate(`TIPO PENSAO` = reorder(`TIPO PENSAO`, quantidade)) %>%
  ggplot() +
  geom_col() +
  aes(x= `TIPO PENSAO` , y = quantidade, fill= `TIPO DE BENEFICIARIO`) +
  coord_flip() + 
  theme_light()+
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.text.align = 0
  )+
  labs(x="",
       y="Número de pensionstas")+
  scale_y_continuous(labels=function(y) format(y, big.mark = ".", scientific = FALSE))+
  viridis::scale_fill_viridis(discrete = TRUE)



aux%>%
  filter(`TIPO DE BENEFICIARIO` %in% c("FILHA", "FILHA MAIOR INVALIDA", "FILHA (MILITAR)", "FILHA MAIOR SOLTEIRA SEM CARGO PUBLICO PERMANENTE"),
         `DATA PROCESSAMENTO`=="Nov/2019") %>%
  ggplot() +
  geom_bar() +
  aes(x= `TIPO PENSAO` , fill= `TIPO DE BENEFICIARIO`) +
  coord_flip() + 
  theme_light()+
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.text.align = 0
  )+
  labs(x="",
       y="Número de pensionstas")+
  scale_y_continuous(labels=function(y) format(y, big.mark = ".", scientific = FALSE))+
  viridis::scale_fill_viridis(discrete = TRUE)

aux%>%
  group_by(prazo_definido) %>%
  summarise(
    total_rendimentos = sum(`RENDIMENTO BRUTO`)
  ) %>%
  ggplot() +
  geom_col(aes(x=prazo_definido, y = total_rendimentos, fill=prazo_definido))+
  theme_light()+
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    axis.text.y =  element_blank()
  )+
  labs(x="",
       y="Rendimento Bruto")+
  scale_y_continuous(labels=function(y) format(y, big.mark = ".", scientific = FALSE))+
  viridis::scale_fill_viridis(discrete = TRUE)+
  coord_flip() 


aux%>%
  filter(`TIPO DE BENEFICIARIO` %in% c("FILHA", "FILHA MAIOR INVALIDA", "FILHA (MILITAR)", "FILHA MAIOR SOLTEIRA SEM CARGO PUBLICO PERMANENTE")) %>%
  group_by(`TIPO DE BENEFICIARIO`) %>%
  summarise(
    total_rendimentos = sum(`RENDIMENTO BRUTO`)
  ) %>%
  ggplot() +
  geom_col(aes(x=`TIPO DE BENEFICIARIO`, y = total_rendimentos, fill=`TIPO DE BENEFICIARIO`))+
  theme_light()+
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    axis.text.y =  element_blank()
  )+
  labs(x="",
       y="Rendimento Bruto")+
  scale_y_continuous(labels=function(y) format(y, big.mark = ".", scientific = FALSE))+
  viridis::scale_fill_viridis(discrete = TRUE)+
  coord_flip() 
  
  
aux%>%
  filter(`TIPO DE BENEFICIARIO` == "FILHA (MILITAR)" )%>%
  group_by(`NOME DO ORGAO`) %>%
  summarise(
    n()
  )

