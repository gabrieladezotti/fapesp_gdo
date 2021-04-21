#FAPESP-2021 - betatemporal
library(tidyverse)
library(vegan)
library(betapart)

setwd("C:/R/fapesp_gdo")
itirapina_plots <- read.table("14042021_plots_pa.txt", h = T,
                             stringsAsFactors = T, sep="\t")

##criando uma df para cada tratamento:
for (variable in unique(itirapina_plots$tratamento)) {
  assign(variable, itirapina_plots %>% filter (tratamento == variable),
         envir =  .GlobalEnv)
} #não entendi porque variavle entrou como tardia no environment

#teste para controle:
teste <- controle %>%
 dplyr::filter(data == "2014_05" | data == "2016_10")

betamulti.controle = double()
 for (i in seq_along(unique(teste$parcela))) 
   {betamulti.controle[[i]] = beta.multi(teste[ ,-c(1:5)], 
                                       index.family = "sorensen")
 }

  
##preciso: pegar a planilha e para cada parcela pegar todos
##os valores de pa e fazer e gerar uma nova planilha apenas com esses dados
