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
#TARDIA
beta_sub_16_t0 <- beta.multi(tardia[tardia$tempo == 0 &
                                      tardia$parcela == 16, -c(1:6)],
                             index.family="sorensen")

beta_sub_16_t25 <- beta.multi(tardia[tardia$tempo == 25 &
                                       tardia$parcela == 16, -c(1:6)],
                              index.family="sorensen")

#TARDIA
beta_sub_29_t0 <- beta.multi(tardia[tardia$tempo == 0 &
                                      tardia$parcela == 29, -c(1:6)],
                             index.family="sorensen")

beta_sub_29_t25 <- beta.multi(tardia[tardia$tempo == 25 &
                                       tardia$parcela == 29, -c(1:6)],
                              index.family="sorensen")

#TARDIA
beta_sub_32_t0 <- beta.multi(tardia[tardia$tempo == 0 &
                                      tardia$parcela == 32, -c(1:6)],
                             index.family="sorensen")

beta_sub_32_t25 <- beta.multi(tardia[tardia$tempo == 25 &
                                       tardia$parcela == 32, -c(1:6)],
                              index.family="sorensen")

#TARDIA
beta_sub_42_t0 <- beta.multi(tardia[tardia$tempo == 0 &
                                      tardia$parcela == 42, -c(1:6)],
                             index.family="sorensen")


beta_sub_42_t25 <- beta.multi(tardia[tardia$tempo == 25 &
                                       tardia$parcela == 42, -c(1:6)],
                              index.family="sorensen")

#TARDIA
beta_sub_15_t0 <- beta.multi(tardia[tardia$tempo == 0 &
                                      tardia$parcela == 15, -c(1:6)],
                             index.family="sorensen")

beta_sub_15_t25 <- beta.multi(tardia[tardia$tempo == 25 &
                                       tardia$parcela == 15, -c(1:6)],
                              index.family="sorensen")

#TARDIA
beta_sub_20_t0 <- beta.multi(tardia[tardia$tempo == 0 &
                                      tardia$parcela == 20, -c(1:6)],
                             index.family="sorensen")

beta_sub_20_t25 <- beta.multi(tardia[tardia$tempo == 25 &
                                       tardia$parcela == 20, -c(1:6)],
                              index.family="sorensen")

#TARDIA
beta_sub_27_t0 <- beta.multi(tardia[tardia$tempo == 0 &
                                      tardia$parcela == 27, -c(1:6)],
                             index.family="sorensen")

beta_sub_27_t25 <- beta.multi(tardia[tardia$tempo == 25 &
                                       tardia$parcela == 27, -c(1:6)],
                              index.family="sorensen")

#TARDIA
beta_sub_38_t0 <- beta.multi(tardia[tardia$tempo == 0 &
                                      tardia$parcela == 38, -c(1:6)],
                             index.family="sorensen")

beta_sub_38_t25 <- beta.multi(tardia[tardia$tempo == 25 &
                                       tardia$parcela == 38, -c(1:6)],
                              index.family="sorensen")


resultado.tardia <- rbind(unlist(beta_sub_16_t0),
                          unlist(beta_sub_16_t25),
                          unlist(beta_sub_29_t0),
                          unlist(beta_sub_29_t25),
                          unlist(beta_sub_32_t0),
                          unlist(beta_sub_32_t25),
                          unlist(beta_sub_42_t0),
                          unlist(beta_sub_42_t25),
                          unlist(beta_sub_15_t0),
                          unlist(beta_sub_15_t25),
                          unlist(beta_sub_20_t0),
                          unlist(beta_sub_20_t25),
                          unlist(beta_sub_27_t0),
                          unlist(beta_sub_27_t25),
                          unlist(beta_sub_38_t0),
                          unlist(beta_sub_38_t25))



tratamento <- vector("list", length(0))
for (i in 1:16) {
  tratamento[[i]] <- print(paste("tardia"))
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

parcela <- c(16,16,
             29,29,
             32,32,
             42,42,
             15,15,
             20,20,
             27,27,
             38,38)

beta.tardia <- data.frame(tratamento, invasora, tempo,
                          parcela, resultado.tardia)
beta.tardia$tratamento <- as.factor(beta.tardia$tratamento)
beta.tardia$invasora <- as.factor(beta.tardia$invasora)
beta.tardia$tempo <- as.factor(beta.tardia$tempo)
beta.tardia$parcela <- as.factor(beta.tardia$parcela)
str(beta.tardia)

##duas parcelas retornaram NaN, retirei;
beta.tardia <- beta.tardia[c(-1:-4),]
 
write.csv2(beta.tardia, file = "betasubplot_tardia_seminvasora.csv", 
           sep = "\t", row.names = F)
