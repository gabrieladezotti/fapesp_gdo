#betadisper
#FAPESP-2021 
library(tidyverse)
library(vegan)

setwd("C:/R/fapesp_gdo")
itirapina_plots <- read.table("14042021_plots_pa.txt", h = T,
                              stringsAsFactors = T, sep="\t")

dist <- vegdist(itirapina_plots[ ,-c(1:5)], method = "bray", binary = T)
betadisper_itirapina <- betadisper(dist, group = itirapina_plots$tratamento)
plot(betadisper(dist, group = itirapina_plots$tratamento))

anova(betadisper_itirapina)
TukeyHSD(betadisper_itirapina)
