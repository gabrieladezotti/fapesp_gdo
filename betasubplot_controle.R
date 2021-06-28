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

for (variable in unique(itirapina_subplots$tratamento)) {
  assign(variable, itirapina_subplots %>% filter (tratamento == variable),
         envir =  .GlobalEnv)
}

#######
#CONTROLE

beta_sub_8_t0 <- beta.multi(controle[controle$tempo == 0 &
                            controle$parcela == 8, -c(1:6)],
                              index.family="sorensen")

beta_sub_8_t3 <- beta.multi(controle[controle$tempo == 3 &
                            controle$parcela == 8, -c(1:6)],
                            index.family="sorensen")

beta_sub_8_t9 <- beta.multi(controle[controle$tempo == 9 &
                            controle$parcela == 8, -c(1:6)],
                            index.family="sorensen")

beta_sub_8_t12 <- beta.multi(controle[controle$tempo == 12 &
                             controle$parcela == 8, -c(1:6)],
                            index.family="sorensen")

beta_sub_8_t17 <- beta.multi(controle[controle$tempo == 17 &
                            controle$parcela == 8, -c(1:6)],
                            index.family="sorensen")

beta_sub_8_t24 <- beta.multi(controle[controle$tempo == 24 &
                            controle$parcela == 8, -c(1:6)],
                            index.family="sorensen")

beta_sub_8_t29 <- beta.multi(controle[controle$tempo == 29 &
                             controle$parcela == 8, -c(1:6)],
                             index.family="sorensen")
#CONTROLE

beta_sub_18_t0 <- beta.multi(controle[controle$tempo == 0 &
                                       controle$parcela == 18, -c(1:6)],
                            index.family="sorensen")

beta_sub_18_t3 <- beta.multi(controle[controle$tempo == 3 &
                                       controle$parcela == 18, -c(1:6)],
                            index.family="sorensen")

beta_sub_18_t9 <- beta.multi(controle[controle$tempo == 9 &
                                       controle$parcela == 18, -c(1:6)],
                            index.family="sorensen")

beta_sub_18_t12 <- beta.multi(controle[controle$tempo == 12 &
                                        controle$parcela == 18, -c(1:6)],
                             index.family="sorensen")

beta_sub_18_t17 <- beta.multi(controle[controle$tempo == 17 &
                                        controle$parcela == 18, -c(1:6)],
                             index.family="sorensen")

beta_sub_18_t24 <- beta.multi(controle[controle$tempo == 24 &
                                        controle$parcela == 18, -c(1:6)],
                             index.family="sorensen")

beta_sub_18_t29 <- beta.multi(controle[controle$tempo == 29 &
                                        controle$parcela == 18, -c(1:6)],
                             index.family="sorensen")

#CONTROLE

beta_sub_28_t0 <- beta.multi(controle[controle$tempo == 0 &
                                        controle$parcela == 28, -c(1:6)],
                             index.family="sorensen")

beta_sub_28_t3 <- beta.multi(controle[controle$tempo == 3 &
                                        controle$parcela == 28, -c(1:6)],
                             index.family="sorensen")

beta_sub_28_t9 <- beta.multi(controle[controle$tempo == 9 &
                                        controle$parcela == 28, -c(1:6)],
                             index.family="sorensen")

beta_sub_28_t12 <- beta.multi(controle[controle$tempo == 12 &
                                         controle$parcela == 28, -c(1:6)],
                              index.family="sorensen")

beta_sub_28_t17 <- beta.multi(controle[controle$tempo == 17 &
                                         controle$parcela == 28, -c(1:6)],
                              index.family="sorensen")

beta_sub_28_t24 <- beta.multi(controle[controle$tempo == 24 &
                                         controle$parcela == 28, -c(1:6)],
                              index.family="sorensen")

beta_sub_28_t29 <- beta.multi(controle[controle$tempo == 29 &
                                         controle$parcela == 28, -c(1:6)],
                              index.family="sorensen")

#CONTROLE

beta_sub_12_t0 <- beta.multi(controle[controle$tempo == 0 &
                                        controle$parcela == 12, -c(1:6)],
                             index.family="sorensen")

beta_sub_12_t3 <- beta.multi(controle[controle$tempo == 3 &
                                        controle$parcela == 12, -c(1:6)],
                             index.family="sorensen")

beta_sub_12_t9 <- beta.multi(controle[controle$tempo == 9 &
                                        controle$parcela == 12, -c(1:6)],
                             index.family="sorensen")

beta_sub_12_t12 <- beta.multi(controle[controle$tempo == 12 &
                                         controle$parcela == 12, -c(1:6)],
                              index.family="sorensen")

beta_sub_12_t17 <- beta.multi(controle[controle$tempo == 17 &
                                         controle$parcela == 12, -c(1:6)],
                              index.family="sorensen")

beta_sub_12_t24 <- beta.multi(controle[controle$tempo == 24 &
                                         controle$parcela == 12, -c(1:6)],
                              index.family="sorensen")

beta_sub_12_t29 <- beta.multi(controle[controle$tempo == 29 &
                                         controle$parcela == 12, -c(1:6)],
                              index.family="sorensen")

#CONTROLE

beta_sub_46_t0 <- beta.multi(controle[controle$tempo == 0 &
                                        controle$parcela == 46, -c(1:6)],
                             index.family="sorensen")

beta_sub_46_t3 <- beta.multi(controle[controle$tempo == 3 &
                                        controle$parcela == 46, -c(1:6)],
                             index.family="sorensen")

beta_sub_46_t9 <- beta.multi(controle[controle$tempo == 9 &
                                        controle$parcela == 46, -c(1:6)],
                             index.family="sorensen")

beta_sub_46_t12 <- beta.multi(controle[controle$tempo == 12 &
                                         controle$parcela == 46, -c(1:6)],
                              index.family="sorensen")

beta_sub_46_t17 <- beta.multi(controle[controle$tempo == 17 &
                                         controle$parcela == 46, -c(1:6)],
                              index.family="sorensen")

beta_sub_46_t24 <- beta.multi(controle[controle$tempo == 24 &
                                         controle$parcela == 46, -c(1:6)],
                              index.family="sorensen")

beta_sub_46_t29 <- beta.multi(controle[controle$tempo == 29 &
                                         controle$parcela == 46, -c(1:6)],
                              index.family="sorensen")

#CONTROLE

beta_sub_24_t0 <- beta.multi(controle[controle$tempo == 0 &
                                        controle$parcela == 24, -c(1:6)],
                             index.family="sorensen")

beta_sub_24_t3 <- beta.multi(controle[controle$tempo == 3 &
                                        controle$parcela == 24, -c(1:6)],
                             index.family="sorensen")

beta_sub_24_t9 <- beta.multi(controle[controle$tempo == 9 &
                                        controle$parcela == 24, -c(1:6)],
                             index.family="sorensen")

beta_sub_24_t12 <- beta.multi(controle[controle$tempo == 12 &
                                         controle$parcela == 24, -c(1:6)],
                              index.family="sorensen")

beta_sub_24_t17 <- beta.multi(controle[controle$tempo == 17 &
                                         controle$parcela == 24, -c(1:6)],
                              index.family="sorensen")

beta_sub_24_t24 <- beta.multi(controle[controle$tempo == 24 &
                                         controle$parcela == 24, -c(1:6)],
                              index.family="sorensen")

beta_sub_24_t29 <- beta.multi(controle[controle$tempo == 29 &
                                         controle$parcela == 24, -c(1:6)],
                              index.family="sorensen")

#CONTROLE

beta_sub_26_t0 <- beta.multi(controle[controle$tempo == 0 &
                                        controle$parcela == 26, -c(1:6)],
                             index.family="sorensen")

beta_sub_26_t3 <- beta.multi(controle[controle$tempo == 3 &
                                        controle$parcela == 26, -c(1:6)],
                             index.family="sorensen")

beta_sub_26_t9 <- beta.multi(controle[controle$tempo == 9 &
                                        controle$parcela == 26, -c(1:6)],
                             index.family="sorensen")

beta_sub_26_t12 <- beta.multi(controle[controle$tempo == 12 &
                                         controle$parcela == 26, -c(1:6)],
                              index.family="sorensen")

beta_sub_26_t17 <- beta.multi(controle[controle$tempo == 17 &
                                         controle$parcela == 26, -c(1:6)],
                              index.family="sorensen")

beta_sub_26_t24 <- beta.multi(controle[controle$tempo == 24 &
                                         controle$parcela == 26, -c(1:6)],
                              index.family="sorensen")

beta_sub_26_t29 <- beta.multi(controle[controle$tempo == 29 &
                                         controle$parcela == 26, -c(1:6)],
                              index.family="sorensen")

#CONTROLE

beta_sub_39_t0 <- beta.multi(controle[controle$tempo == 0 &
                                        controle$parcela == 39, -c(1:6)],
                             index.family="sorensen")

beta_sub_39_t3 <- beta.multi(controle[controle$tempo == 3 &
                                        controle$parcela == 39, -c(1:6)],
                             index.family="sorensen")

beta_sub_39_t9 <- beta.multi(controle[controle$tempo == 9 &
                                        controle$parcela == 39, -c(1:6)],
                             index.family="sorensen")

beta_sub_39_t12 <- beta.multi(controle[controle$tempo == 12 &
                                         controle$parcela == 39, -c(1:6)],
                              index.family="sorensen")

beta_sub_39_t17 <- beta.multi(controle[controle$tempo == 17 &
                                         controle$parcela == 39, -c(1:6)],
                              index.family="sorensen")

beta_sub_39_t24 <- beta.multi(controle[controle$tempo == 24 &
                                         controle$parcela == 39, -c(1:6)],
                              index.family="sorensen")

beta_sub_39_t29 <- beta.multi(controle[controle$tempo == 29 &
                                         controle$parcela == 39, -c(1:6)],
                              index.family="sorensen")

resultado.controle <- rbind(unlist(beta_sub_8_t0),
                            unlist(beta_sub_8_t3),
                            unlist(beta_sub_8_t9),
                            unlist(beta_sub_8_t12),
                            unlist(beta_sub_8_t17),
                            unlist(beta_sub_8_t24),
                            unlist(beta_sub_8_t29),
                            unlist(beta_sub_18_t0),
                            unlist(beta_sub_18_t3),
                            unlist(beta_sub_18_t9),
                            unlist(beta_sub_18_t12),
                            unlist(beta_sub_18_t17),
                            unlist(beta_sub_18_t24),
                            unlist(beta_sub_18_t29),
                            unlist(beta_sub_28_t0),
                            unlist(beta_sub_28_t3),
                            unlist(beta_sub_28_t9),
                            unlist(beta_sub_28_t12),
                            unlist(beta_sub_28_t17),
                            unlist(beta_sub_28_t24),
                            unlist(beta_sub_28_t29),
                            unlist(beta_sub_46_t0),
                            unlist(beta_sub_46_t3),
                            unlist(beta_sub_46_t9),
                            unlist(beta_sub_46_t12),
                            unlist(beta_sub_46_t17),
                            unlist(beta_sub_46_t24),
                            unlist(beta_sub_46_t29),
                            unlist(beta_sub_12_t0),
                            unlist(beta_sub_12_t3),
                            unlist(beta_sub_12_t9),
                            unlist(beta_sub_12_t12),
                            unlist(beta_sub_12_t17),
                            unlist(beta_sub_12_t24),
                            unlist(beta_sub_12_t29),
                            unlist(beta_sub_24_t0),
                            unlist(beta_sub_24_t3),
                            unlist(beta_sub_24_t9),
                            unlist(beta_sub_24_t12),
                            unlist(beta_sub_24_t17),
                            unlist(beta_sub_24_t24),
                            unlist(beta_sub_24_t29),
                            unlist(beta_sub_26_t0),
                            unlist(beta_sub_26_t3),
                            unlist(beta_sub_26_t9),
                            unlist(beta_sub_26_t12),
                            unlist(beta_sub_26_t17),
                            unlist(beta_sub_26_t24),
                            unlist(beta_sub_26_t29),
                            unlist(beta_sub_39_t0),
                            unlist(beta_sub_39_t3),
                            unlist(beta_sub_39_t9),
                            unlist(beta_sub_39_t12),
                            unlist(beta_sub_39_t17),
                            unlist(beta_sub_39_t24),
                            unlist(beta_sub_39_t29))


tratamento <- vector("list", length(0))
for (i in 1:56) {
  tratamento[[i]] <- print(paste("controle"))
}

tratamento <- unlist(tratamento)

invasora <- c("MM","MM","MM","MM","MM","MM","MM",
              "MM","MM","MM","MM","MM","MM","MM",
              "MM","MM","MM","MM","MM","MM","MM",
              "MM","MM","MM","MM","MM","MM","MM",
              "UD","UD","UD","UD","UD","UD","UD",
              "UD","UD","UD","UD","UD","UD","UD",
              "UD","UD","UD","UD","UD","UD","UD",
              "UD","UD","UD","UD","UD","UD","UD")

tempo <- c(0,3,9,12,17,24,29,
           0,3,9,12,17,24,29,
           0,3,9,12,17,24,29,
           0,3,9,12,17,24,29,
           0,3,9,12,17,24,29,
           0,3,9,12,17,24,29,
           0,3,9,12,17,24,29,
           0,3,9,12,17,24,29)

parcela <- c(8,8,8,8,8,8,8,
             18,18,18,18,18,18,18,
             28,28,28,28,28,28,28,
             46,46,46,46,46,46,46,
             12,12,12,12,12,12,12,
             24,24,24,24,24,24,24,
             26,26,26,26,26,26,26,
             39,39,39,39,39,39,39)

beta.controle <- data.frame(tratamento, invasora, tempo,
                            parcela, resultado.controle)
beta.controle$tratamento <- as.factor(beta.controle$tratamento)
beta.controle$invasora <- as.factor(beta.controle$invasora)
beta.controle$tempo <- as.factor(beta.controle$tempo)
beta.controle$parcela <- as.factor(beta.controle$parcela)
str(beta.controle)


write.csv2(beta.controle, file = "22042021_betasubplot_controle.csv", 
           sep = "\t", row.names = F)
