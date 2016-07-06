setwd("/Users/natalia/documents/manoel/previdencia/Despesa-com-benefícios_Clientela_Arquivos-Finais_2002_2015")

# url <- "http://dadosabertos.dataprev.gov.br/opendata/con02/formato=json"
# url1 <- "http://dadosabertos.dataprev.gov.br/opendata/con02/formato=csv"

# dados de gasto com a previdência em 2015
prev <- read.table("prev_2015.txt", header=T, dec=",", sep="\t", fileEncoding="UTF-16LE",
                   colClasses = c("factor", "numeric", "factor", "factor", "factor", "numeric"))

summary(prev)
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)

## gasto prev por regiao, rural ou urbana
prev %>%
  group_by(regiao) %>%
  summarise(valor = sum(valor)) %>%
  mutate( total = sum(valor),
          perc = round(valor / total, 2))

## gasto prev por regiao, tipo_específico
prev %>%
  group_by(regiao, tipo.específico) %>%
  summarise(total = sum(valor)) %>%
  spread( regiao, total)

## gasto rural
prev %>%
  filter(regiao == "rural") %>%
  group_by(tipo.geral, tipo.específico) %>%
  summarise(total = sum(valor)) 
  

library(devtools)
# install_github("wilkox/treemapify")
library(treemapify) 

prev1 <- prev %>%
  filter(regiao == "urbana")

# Generate coordinates for the rectangles

treemap_coords <- treemapify(prev1, area="valor", fill="tipo.específico", label="benefício", group="tipo.geral")


prev2 <- prev1 %>%
  select( valor = valor, beneficio = benefício, regiao) 

treemap_coords1 <- inner_join(treemap_coords,  prev2, by=c("label" = "beneficio")) %>%
  mutate(label = paste(label, "\n", round(valor/1000000000, 1)))
ggplotify(treemap_coords1) + labs(title="Despesa da Previdência Urbana por tipo em R$ bilhões")

## treemap_coords <- treemapify(prev1, area="valor", fill="tipo.específico", label="benefício", group="tipo.geral")


prev3 <- prev1 %>%
  mutate(grupo = ifelse(grepl("invalidez", benefício), "problema de saúde", 
                        ifelse(grepl("Auxílio", benefício), "problema de saúde", 
                               ifelse(grepl("Morte", benefício), "Pensões por Morte", 
                               "outro"))), grupo = as.factor(grupo)) %>%
  group_by(grupo) %>%
  summarise(valor = sum(valor)) %>%
  mutate( total = sum(valor),
          perc = round(valor / total, 2)) %>%
  mutate(saude = grupo == "outro") %>%
  group_by(saude) %>%
  mutate(subtotal = sum(valor),
         perc_subtotal = round(subtotal / total, 2))

treemap_coords1 <- treemapify(prev3, area="valor", fill="grupo", label="grupo")

ggplotify(treemap_coords1) + labs(title="Despesa da Previdência Urbana por tipo em R$ bilhões")

## bar plot
library(scales)
ggplot(prev3, aes(y=perc, x=grupo)) + geom_bar(stat="identity") + 
  coord_flip() + scale_y_continuous(labels = scales::percent)
