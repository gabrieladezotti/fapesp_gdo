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

#CONTROLE
betamulti.8 <- beta.multi(controle[c(1,81),
                             -c(1:5)], index.family="sorensen")

betamulti.18 <- beta.multi(controle[c(2,82) ,
                              -c(1:5)], index.family="sorensen")

betamulti.28 <- beta.multi(controle[c(3,83) ,
                              -c(1:5)], index.family="sorensen")

betamulti.46 <- beta.multi(controle[c(4,84) ,
                             -c(1:5)], index.family="sorensen")

betamulti.12 <- beta.multi(controle[c(5,85) ,
                              -c(1:5)], index.family="sorensen")

betamulti.24 <- beta.multi(controle[c(6,86) ,
                              -c(1:5)], index.family="sorensen")

betamulti.26 <- beta.multi(controle[c(7,87) ,
                              -c(1:5)], index.family="sorensen")

betamulti.39 <- beta.multi(controle[c(8,88) ,
                             -c(1:5)], index.family="sorensen")

######################
beta.multi.df <- rbind(unlist(betamulti.8),
                       unlist(betamulti.18),
                       unlist(betamulti.28),
                       unlist(betamulti.46),
                       unlist(betamulti.12),
                       unlist(betamulti.24),
                       unlist(betamulti.26),
                       unlist(betamulti.39))

parcela <- c("8", "18", "28", "46", "12", "24","26","39")
tratamento <- c("FE","FE","FE","FE","FE","FE","FE","FE")
invasora <- c("MM", "MM", "MM", "MM", "UD", "UD", "UD", "UD")
beta.multi.controle <- data.frame(parcela, invasora, tratamento, beta.multi.df)

#PRECOCE
betamulti.3 <- beta.multi(precoce[c(1,41) ,
                              -c(1:5)], index.family="sorensen")

betamulti.17 <- beta.multi(precoce[c(2,42) ,
                              -c(1:5)], index.family="sorensen")

betamulti.22 <- beta.multi(precoce[c(3,43) ,
                                  -c(1:5)], index.family="sorensen")

betamulti.31 <- beta.multi(precoce[c(4,45) ,
                                   -c(1:5)], index.family="sorensen")

betamulti.14 <- beta.multi(precoce[c(5,46) ,
                                  -c(1:5)], index.family="sorensen")

betamulti.21 <- beta.multi(precoce[c(6,47) ,
                                   -c(1:5)], index.family="sorensen")

betamulti.23 <- beta.multi(precoce[c(7,48) ,
                                   -c(1:5)], index.family="sorensen")

betamulti.36 <- beta.multi(precoce[c(8,49) ,
                                   -c(1:5)], index.family="sorensen")
######################
beta.multi.df <- rbind(unlist(betamulti.3),
                       unlist(betamulti.17),
                       unlist(betamulti.22),
                       unlist(betamulti.31),
                       unlist(betamulti.14),
                       unlist(betamulti.21),
                       unlist(betamulti.23),
                       unlist(betamulti.36))

parcela <- c("3", "17","22","31","14","21","23","36")
tratamento <- c("EAR","EAR","EAR","EAR","EAR","EAR","EAR","EAR")

beta.multi.precoce <- data.frame(parcela, invasora, tratamento, beta.multi.df)

#TARDIA
betamulti.16 <- beta.multi(tardia[c(1,41) ,
                                  -c(1:5)], index.family="sorensen")

betamulti.29 <- beta.multi(tardia[c(2,42) ,
                                   -c(1:5)], index.family="sorensen")

betamulti.32 <- beta.multi(tardia[c(3,43) ,
                                   -c(1:5)], index.family="sorensen")

betamulti.42 <- beta.multi(tardia[c(4,44) ,
                                   -c(1:5)], index.family="sorensen")

betamulti.15 <- beta.multi(tardia[c(5,45) ,
                                   -c(1:5)], index.family="sorensen")

betamulti.20 <- beta.multi(tardia[c(6,46) ,
                                   -c(1:5)], index.family="sorensen")

betamulti.27 <- beta.multi(tardia[c(7,47) ,
                                   -c(1:5)], index.family="sorensen")

betamulti.38 <- beta.multi(tardia[c(8,48) ,
                                   -c(1:5)], index.family="sorensen")
######################
beta.multi.df <- rbind(unlist(betamulti.16),
                       unlist(betamulti.29),
                       unlist(betamulti.32),
                       unlist(betamulti.42),
                       unlist(betamulti.15),
                       unlist(betamulti.20),
                       unlist(betamulti.27),
                       unlist(betamulti.38))

parcela <- c("16","29","32","42","15","20","27","38")
tratamento <- c("LATE","LATE","LATE","LATE","LATE","LATE","LATE","LATE")

beta.multi.tardia <- data.frame(parcela, invasora, tratamento, beta.multi.df)

#MODAL
betamulti.9 <- beta.multi(modal[c(1,33) ,
                                  -c(1:5)], index.family="sorensen")

betamulti.30 <- beta.multi(modal[c(2,34) ,
                                  -c(1:5)], index.family="sorensen")

betamulti.40 <- beta.multi(modal[c(3,35) ,
                                  -c(1:5)], index.family="sorensen")

betamulti.43 <- beta.multi(modal[c(4,36) ,
                                  -c(1:5)], index.family="sorensen")

betamulti.25 <- beta.multi(modal[c(5,37) ,
                                  -c(1:5)], index.family="sorensen")

betamulti.33 <- beta.multi(modal[c(6,38) ,
                                  -c(1:5)], index.family="sorensen")

betamulti.37 <- beta.multi(modal[c(7,39) ,
                                  -c(1:5)], index.family="sorensen")

betamulti.41 <- beta.multi(modal[c(8,40) ,
                                  -c(1:5)], index.family="sorensen")
######################
beta.multi.df <- rbind(unlist(betamulti.9),
                       unlist(betamulti.30),
                       unlist(betamulti.40),
                       unlist(betamulti.43),
                       unlist(betamulti.25),
                       unlist(betamulti.33),
                       unlist(betamulti.37),
                       unlist(betamulti.41))

parcela <- c("9","30","40","43","25","33","37","41")
tratamento <- c("MOD","MOD","MOD","MOD","MOD","MOD","MOD","MOD")

beta.multi.modal <- data.frame(parcela, invasora, tratamento, beta.multi.df)

betamulti.parcelas <- rbind(beta.multi.controle, beta.multi.modal, 
      beta.multi.precoce, beta.multi.tardia)


####Salvando a planilha nova:
write.csv2(betamulti.parcelas, file = "21042021_betamulti_parcelas.csv", 
           sep = "\t", row.names = F)
