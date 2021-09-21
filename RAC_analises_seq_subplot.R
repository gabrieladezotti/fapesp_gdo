#FAPESP - Análises Estatísticas em RAC_change - sequencial
#Carregando os pacotes necessários:
library(tidyverse)
library(extrafont)

setwd("C:/R/fapesp_gdo")
RAC_subplots <- read.table("RAC_seq_subplots.txt", h = T,
                             stringsAsFactors = T, sep="\t")
str(RAC_subplots)
RAC_subplots$parcela <- as.factor(RAC_subplots$parcela)
RAC_subplots$subparcela <- as.factor(RAC_subplots$subparcela)
RAC_subplots$tempo <- as.factor(RAC_subplots$tempo)
RAC_subplots$tempo2<- as.factor(RAC_subplots$tempo2)
RAC_subplots <- RAC_subplots[,-8]
summary(RAC_subplots)


