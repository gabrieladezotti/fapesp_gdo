##betaespaçotemporal - FAPESP
library(betapart)
library(tidyverse)

setwd("C:/R/fapesp_gdo")
itirapina_subplots <- read.table("14042021_subplots_pa.txt", h = T,
                                 stringsAsFactors = T, sep="\t")
#conferindo dados
str(itirapina_subplots)
itirapina_subplots$parcela <- as.factor(itirapina_subplots$parcela)
itirapina_subplots$subparcela <- as.factor(itirapina_subplots$subparcela)

itirapina_subplots <- itirapina_subplots[,c(-7,-20)]

for (variable in unique(itirapina_subplots$tratamento)) {
  assign(variable, itirapina_subplots %>% filter (tratamento == variable),
         envir =  .GlobalEnv)
}

#######
#CONTROLE

beta_sub_8_t0 <- beta.multi(controle[controle$tempo == 0 &
                                       controle$parcela == 8, -c(1:6)],
                            index.family="sorensen")

beta_sub_8_t29 <- beta.multi(controle[controle$tempo == 29 &
                                        controle$parcela == 8, -c(1:6)],
                             index.family="sorensen")

beta_sub_18_t0 <- beta.multi(controle[controle$tempo == 0 &
                                        controle$parcela == 18, -c(1:6)],
                             index.family="sorensen")

beta_sub_18_t29 <- beta.multi(controle[controle$tempo == 29 &
                                         controle$parcela == 18, -c(1:6)],
                              index.family="sorensen")

beta_sub_28_t0 <- beta.multi(controle[controle$tempo == 0 &
                                        controle$parcela == 28, -c(1:6)],
                             index.family="sorensen")

beta_sub_28_t29 <- beta.multi(controle[controle$tempo == 29 &
                                         controle$parcela == 28, -c(1:6)],
                              index.family="sorensen")

beta_sub_12_t0 <- beta.multi(controle[controle$tempo == 0 &
                                        controle$parcela == 12, -c(1:6)],
                             index.family="sorensen")

beta_sub_12_t29 <- beta.multi(controle[controle$tempo == 29 &
                                         controle$parcela == 12, -c(1:6)],
                              index.family="sorensen")

beta_sub_46_t0 <- beta.multi(controle[controle$tempo == 0 &
                                        controle$parcela == 46, -c(1:6)],
                             index.family="sorensen")

beta_sub_46_t29 <- beta.multi(controle[controle$tempo == 29 &
                                         controle$parcela == 46, -c(1:6)],
                              index.family="sorensen")

beta_sub_24_t0 <- beta.multi(controle[controle$tempo == 0 &
                                        controle$parcela == 24, -c(1:6)],
                             index.family="sorensen")

beta_sub_24_t29 <- beta.multi(controle[controle$tempo == 29 &
                                         controle$parcela == 24, -c(1:6)],
                              index.family="sorensen")

beta_sub_26_t0 <- beta.multi(controle[controle$tempo == 0 &
                                        controle$parcela == 26, -c(1:6)],
                             index.family="sorensen")

beta_sub_26_t29 <- beta.multi(controle[controle$tempo == 29 &
                                         controle$parcela == 26, -c(1:6)],
                              index.family="sorensen")

beta_sub_39_t0 <- beta.multi(controle[controle$tempo == 0 &
                                        controle$parcela == 39, -c(1:6)],
                             index.family="sorensen")

beta_sub_39_t29 <- beta.multi(controle[controle$tempo == 29 &
                                         controle$parcela == 39, -c(1:6)],
                              index.family="sorensen")

resultado.controle <- rbind(unlist(beta_sub_8_t0),
                            unlist(beta_sub_8_t29),
                            unlist(beta_sub_18_t0),
                            unlist(beta_sub_18_t29),
                            unlist(beta_sub_28_t0),
                            unlist(beta_sub_28_t29),
                            unlist(beta_sub_46_t0),
                            unlist(beta_sub_46_t29),
                            unlist(beta_sub_12_t0),
                            unlist(beta_sub_12_t29),
                            unlist(beta_sub_24_t0),
                            unlist(beta_sub_24_t29),
                            unlist(beta_sub_26_t0),
                            unlist(beta_sub_26_t29),
                            unlist(beta_sub_39_t0),
                            unlist(beta_sub_39_t29))


tratamento <- vector("list", length(0))
for (i in 1:16) {
  tratamento[[i]] <- print(paste("controle"))
}

tratamento <- unlist(tratamento)

invasora <- c("MM","MM","MM","MM","MM","MM","MM",
              "MM","UD","UD","UD","UD","UD","UD",
              "UD","UD")

tempo <- c("t0","tf","t0","tf","t0","tf","t0","tf",
           "t0","tf","t0","tf","t0","tf","t0","tf")

parcela <- c(8,8,18,18,28,28,46,46,12,12,
             24,24,26,26,39,39)

beta.controle <- data.frame(tratamento, invasora, tempo,
                            parcela, resultado.controle)
beta.controle$tratamento <- as.factor(beta.controle$tratamento)
beta.controle$invasora <- as.factor(beta.controle$invasora)
beta.controle$tempo <- as.factor(beta.controle$tempo)
beta.controle$parcela <- as.factor(beta.controle$parcela)
str(beta.controle)


write.csv2(beta.controle, file = "betasubplot_controle_seminvasora.csv", 
           sep = "\t", row.names = F)
