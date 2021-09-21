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
}

#teste para controle:

output <- vector("double", length(unique(controle$parcela)))
 for (variable in unique(controle$parcela)) {
  betapart::beta.multi(controle[ ,-c(1:5)], index.family = "sorensen")
 }

  
##preciso: pegar a planilha e para cada parcela pegar todos
##os valores de pa e fazer e gerar uma nova planilha apenas com esses dados
