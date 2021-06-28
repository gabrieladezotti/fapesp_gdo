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
#PRECOCE
beta_sub_3_t0 <- beta.multi(precoce[precoce$tempo == 0 &
                                      precoce$parcela == 3, -c(1:6)],
                            index.family="sorensen")

beta_sub_3_t24 <- beta.multi(precoce[precoce$tempo == 24 &
                                       precoce$parcela == 3, -c(1:6)],
                             index.family="sorensen")

#PRECOCE
beta_sub_17_t0 <- beta.multi(precoce[precoce$tempo == 0 &
                                       precoce$parcela == 17, -c(1:6)],
                             index.family="sorensen")

beta_sub_17_t24 <- beta.multi(precoce[precoce$tempo == 24 &
                                        precoce$parcela == 17, -c(1:6)],
                              index.family="sorensen")

#PRECOCE
beta_sub_22_t0 <- beta.multi(precoce[precoce$tempo == 0 &
                                       precoce$parcela == 22, -c(1:6)],
                             index.family="sorensen")

beta_sub_22_t24 <- beta.multi(precoce[precoce$tempo == 24 &
                                        precoce$parcela == 22, -c(1:6)],
                              index.family="sorensen")

#PRECOCE
beta_sub_31_t0 <- beta.multi(precoce[precoce$tempo == 0 &
                                       precoce$parcela == 31, -c(1:6)],
                             index.family="sorensen")

beta_sub_31_t24 <- beta.multi(precoce[precoce$tempo == 24 &
                                        precoce$parcela == 31, -c(1:6)],
                              index.family="sorensen")

#PRECOCE
beta_sub_14_t0 <- beta.multi(precoce[precoce$tempo == 0 &
                                       precoce$parcela == 14, -c(1:6)],
                             index.family="sorensen")

beta_sub_14_t24 <- beta.multi(precoce[precoce$tempo == 24 &
                                        precoce$parcela == 14, -c(1:6)],
                              index.family="sorensen")

#PRECOCE
beta_sub_21_t0 <- beta.multi(precoce[precoce$tempo == 0 &
                                       precoce$parcela == 21, -c(1:6)],
                             index.family="sorensen")

beta_sub_21_t24 <- beta.multi(precoce[precoce$tempo == 24 &
                                        precoce$parcela == 21, -c(1:6)],
                              index.family="sorensen")

#PRECOCE
beta_sub_23_t0 <- beta.multi(precoce[precoce$tempo == 0 &
                                       precoce$parcela == 23, -c(1:6)],
                             index.family="sorensen")

beta_sub_23_t24 <- beta.multi(precoce[precoce$tempo == 24 &
                                        precoce$parcela == 23, -c(1:6)],
                              index.family="sorensen")

#PRECOCE
beta_sub_36_t0 <- beta.multi(precoce[precoce$tempo == 0 &
                                       precoce$parcela == 36, -c(1:6)],
                             index.family="sorensen")

beta_sub_36_t24 <- beta.multi(precoce[precoce$tempo == 24 &
                                        precoce$parcela == 36, -c(1:6)],
                              index.family="sorensen")

resultado.precoce  <- rbind(unlist(beta_sub_3_t0),
                            unlist(beta_sub_3_t24),
                            unlist(beta_sub_17_t0),
                            unlist(beta_sub_17_t24),
                            unlist(beta_sub_22_t0),
                            unlist(beta_sub_31_t24),
                            unlist(beta_sub_31_t0),
                            unlist(beta_sub_31_t24),
                            unlist(beta_sub_14_t0),
                            unlist(beta_sub_14_t24),
                            unlist(beta_sub_21_t0),
                            unlist(beta_sub_21_t24),
                            unlist(beta_sub_23_t0),
                            unlist(beta_sub_23_t24),
                            unlist(beta_sub_36_t0),
                            unlist(beta_sub_36_t24))


tratamento <- vector("list", length(0))
for (i in 1:16) {
  tratamento[[i]] <- print(paste("precoce"))
}

tratamento <- unlist(tratamento)

invasora <- c("MM","MM",
              "MM","MM",
              "MM","MM",
              "MM","MM",
              "UD","UD",
              "UD","UD",
              "UD","UD",
              "UD","UD")

tempo <- c("inicial", "final",
           "inicial", "final",
           "inicial", "final",
           "inicial", "final",
           "inicial", "final",
           "inicial", "final",
           "inicial", "final",
           "inicial", "final")

parcela <- c(3,3,
             17,17,
             22,22,
             31,31,
             14,14,
             21,21,
             23,23,
             36,36)

beta.precoce <- data.frame(tratamento, invasora, tempo,
                           parcela, resultado.precoce)
beta.precoce$tratamento <- as.factor(beta.precoce$tratamento)
beta.precoce$invasora <- as.factor(beta.precoce$invasora)
beta.precoce$tempo <- as.factor(beta.precoce$tempo)
beta.precoce$parcela <- as.factor(beta.precoce$parcela)
str(beta.precoce)


write.csv2(beta.precoce, file = "betasubplot_precoce_seminvasora.csv", 
           sep = "\t", row.names = F)
