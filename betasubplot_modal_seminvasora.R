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
#MODAL
beta_sub_9_t0 <- beta.multi(modal[modal$tempo == 0 &
                                    modal$parcela == 9, -c(1:6)],
                            index.family="sorensen")

beta_sub_9_t23 <- beta.multi(modal[modal$tempo == 23 &
                                     modal$parcela == 9, -c(1:6)],
                             index.family="sorensen")

#MODAL
beta_sub_30_t0 <- beta.multi(modal[modal$tempo == 0 &
                                     modal$parcela == 30, -c(1:6)],
                             index.family="sorensen")

beta_sub_30_t23 <- beta.multi(modal[modal$tempo == 23 &
                                      modal$parcela == 30, -c(1:6)],
                              index.family="sorensen")

#MODAL
beta_sub_40_t0 <- beta.multi(modal[modal$tempo == 0 &
                                     modal$parcela == 40, -c(1:6)],
                             index.family="sorensen")

beta_sub_40_t23 <- beta.multi(modal[modal$tempo == 23 &
                                      modal$parcela == 40, -c(1:6)],
                              index.family="sorensen")

#MODAL
beta_sub_43_t0 <- beta.multi(modal[modal$tempo == 0 &
                                     modal$parcela == 43, -c(1:6)],
                             index.family="sorensen")

beta_sub_43_t23 <- beta.multi(modal[modal$tempo == 23 &
                                      modal$parcela == 43, -c(1:6)],
                              index.family="sorensen")

#MODAL
beta_sub_25_t0 <- beta.multi(modal[modal$tempo == 0 &
                                     modal$parcela == 25, -c(1:6)],
                             index.family="sorensen")

beta_sub_25_t23 <- beta.multi(modal[modal$tempo == 23 &
                                      modal$parcela == 25, -c(1:6)],
                              index.family="sorensen")

#MODAL
beta_sub_33_t0 <- beta.multi(modal[modal$tempo == 0 &
                                     modal$parcela == 33, -c(1:6)],
                             index.family="sorensen")

beta_sub_33_t23 <- beta.multi(modal[modal$tempo == 23 &
                                      modal$parcela == 33, -c(1:6)],
                              index.family="sorensen")

#MODAL
beta_sub_37_t0 <- beta.multi(modal[modal$tempo == 0 &
                                     modal$parcela == 37, -c(1:6)],
                             index.family="sorensen")

beta_sub_37_t23 <- beta.multi(modal[modal$tempo == 23 &
                                      modal$parcela == 37, -c(1:6)],
                              index.family="sorensen")

#MODAL
beta_sub_41_t0 <- beta.multi(modal[modal$tempo == 0 &
                                     modal$parcela == 41, -c(1:6)],
                             index.family="sorensen")

beta_sub_41_t23 <- beta.multi(modal[modal$tempo == 23 &
                                      modal$parcela == 41, -c(1:6)],
                              index.family="sorensen")

resultado.modal    <- rbind(unlist(beta_sub_9_t0),
                            unlist(beta_sub_9_t23),
                            unlist(beta_sub_30_t0),
                            unlist(beta_sub_30_t23),
                            unlist(beta_sub_40_t0),
                            unlist(beta_sub_40_t23),
                            unlist(beta_sub_43_t0),
                            unlist(beta_sub_43_t23),
                            unlist(beta_sub_25_t0),
                            unlist(beta_sub_25_t23),
                            unlist(beta_sub_33_t0),
                            unlist(beta_sub_33_t23),
                            unlist(beta_sub_37_t0),
                            unlist(beta_sub_37_t23),
                            unlist(beta_sub_41_t0),
                            unlist(beta_sub_41_t23))


tratamento <- vector("list", length(0))
for (i in 1:16) {
  tratamento[[i]] <- print(paste("modal"))
}

tratamento <- unlist(tratamento)

invasora <- c("MM","MM","MM",
              "MM","MM","MM",
              "MM","MM",
              "UD","UD","UD",
              "UD","UD","UD",
              "UD","UD")

tempo <- c("inicial", "final",
           "inicial", "final",
           "inicial", "final",
           "inicial", "final",
           "inicial", "final",
           "inicial", "final",
           "inicial", "final",
           "inicial", "final")

parcela <- c(9,9,
             30,30,
             40,40,
             43,43,
             25,25,
             33,33,
             37,37,
             41,41)

beta.modal <- data.frame(tratamento, invasora, tempo,
                         parcela, resultado.modal)
beta.modal$tratamento <- as.factor(beta.modal$tratamento)
beta.modal$invasora <- as.factor(beta.modal$invasora)
beta.modal$tempo <- as.factor(beta.modal$tempo)
beta.modal$parcela <- as.factor(beta.modal$parcela)
str(beta.modal)


write.csv2(beta.modal, file = "betasubplot_modal_seminvasora.csv", 
           sep = "\t", row.names = F)
