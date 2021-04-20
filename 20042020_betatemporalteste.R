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

#Cálculo da diversidade beta inicial final:
#FE-6
betamulti.6 <- beta.multi(FE[c(1,3),
                             -c(1:4)], index.family="sorensen")
betamulti.6

#FE-12
betamulti.12 <- beta.multi(FE[c(2,4) ,
                              -c(1:4)], index.family="sorensen")
betamulti.12

#FE-13
betamulti.13 <- beta.multi(FE[c(5,7) ,
                              -c(1:4)], index.family="sorensen")
betamulti.13

#AF-4
betamulti.4 <- beta.multi(AF[c(1,3) ,
                             -c(1:4)], index.family="sorensen")
betamulti.4

#AF-11
betamulti.11 <- beta.multi(AF[c(4,6) ,
                              -c(1:4)], index.family="sorensen")
betamulti.11

#AF-16
betamulti.16 <- beta.multi(AF[c(7,9) ,
                              -c(1:4)], index.family="sorensen")
betamulti.16

#AF-20
betamulti.20 <- beta.multi(AF[c(10,12) ,
                              -c(1:4)], index.family="sorensen")
betamulti.20

#BF-2
betamulti.2 <- beta.multi(BF[c(1,3) ,
                             -c(1:4)], index.family="sorensen")
betamulti.2

#BF-10
betamulti.10 <- beta.multi(BF[c(4,6) ,
                              -c(1:4)], index.family="sorensen")
betamulti.10

#BF-17
betamulti.17 <- beta.multi(BF[c(7,9) ,
                              -c(1:4)], index.family="sorensen")
betamulti.17

#BF-23

betamulti.23 <- beta.multi(BF[c(10,12) ,
                              -c(1:4)], index.family="sorensen")
betamulti.23

######################
beta.multi.df <- rbind(unlist(betamulti.6),
                       unlist(betamulti.12),
                       unlist(betamulti.13),
                       unlist(betamulti.4),
                       unlist(betamulti.11),
                       unlist(betamulti.16),
                       unlist(betamulti.20),
                       unlist(betamulti.2),
                       unlist(betamulti.10),
                       unlist(betamulti.17),
                       unlist(betamulti.23))

parcela <- c("6", "12","13","4","11","16","20","2","10","17","23")
tratamento <- c("FE","FE","FE","AF","AF","AF","AF","BF","BF","BF","BF")

beta.multi <- data.frame(parcela, tratamento, beta.multi.df)
beta.multi$tratamento <- as.factor(beta.multi$tratamento)
beta.multi$parcela <- as.factor(beta.multi$parcela)

#############Gráfico: 

ggplot(beta.multi) +
  geom_boxplot(aes(tratamento, beta.SOR, fill = tratamento),  show.legend = FALSE) +
  scale_fill_manual(values = c("lightsalmon", "slategray2", "palegreen3")) +
  labs(x = "Tratamentos", y = "Diversidade Beta Total", 
       title = "") +
  theme_test()

###melhorando
pdf("betatemporal.pdf")

ggplot(beta.multi) +
  geom_boxplot(aes(tratamento, beta.SOR, fill = tratamento), 
               show.legend = FALSE,
               width = 0.2, alpha  = 0.7) +
  ylim(0.25, 0.50) +
  labs(x = "Tratamentos", y = "Diversidade Beta Total", 
       title = "") +
  scale_fill_manual(values = c("#FC4E07", "#E7B800", "#30A612")) +
  theme_test(base_size = 12)

dev.off()

betatemporal
##########
modelo.betatemporal <- lm(beta.SOR ~ tratamento, 
                          data = beta.multi)
summary(modelo.betatemporal)


aov(modelo.betatemporal)

TukeyHSD(aov(beta.SOR ~ tratamento, 
             data = beta.multi))

modelo.temporal.turn <- lm(beta.SIM ~ tratamento, 
                           data = beta.multi)
summary(modelo.temporal.turn)

modelo.temporal.nest <- lm(beta.SNE ~ tratamento, 
                           data = beta.multi)
summary(modelo.temporal.nest)


########################
tapply(beta.multi$beta.SOR, beta.multi$tratamento, sd)
tapply(beta.multi$beta.SOR, beta.multi$tratamento, quantile)
