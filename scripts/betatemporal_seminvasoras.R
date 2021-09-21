#FAPESP-2021 - ANÁLISES SEM INVASORAS
###Para essas análises vamos retirar as espécies invasoras
###como parte das comunidades, deixando-as apenas como variáveis explanatórias.
library(tidyverse)
library(betapart)
setwd("C:/R/fapesp_gdo")

itirapina_plots <- read.table("14042021_plots_pa.txt", h = T,
                              stringsAsFactors = T, sep="\t")
str(itirapina_plots)
itirapina_plots$parcela <- as.factor(itirapina_plots$parcela)
itirapina_plots$tempo <- as.factor(itirapina_plots$tempo)

itirapina_plots <- itirapina_plots %>%
  mutate(invasora = recode(invasora, "melinis minutiflora" = "MM"),
         invasora = recode(invasora, "urochloa decumbens" = "UD"))

for (variable in unique(itirapina_plots$invasora)) {
  assign(variable, itirapina_plots %>% filter (invasora == variable),
         envir =  .GlobalEnv)
}

MM <- MM[,c(-6,-19)]
UD <- UD[,c(-6,-19)]

##Diversidade Beta INICIAL-FINAL
#MM
betamulti.3 <- beta.multi(MM[c(5,93),
                             -c(1:5)], index.family="sorensen")

betamulti.8 <- beta.multi(MM[c(1, 106),
                             -c(1:5)], index.family="sorensen")

betamulti.9 <- beta.multi(MM[c(13, 102),
                             -c(1:5)], index.family="sorensen")

betamulti.16 <- beta.multi(MM[c(21, 110),
                              -c(1:5)], index.family="sorensen")

betamulti.17 <- beta.multi(MM[c(6, 94),
                              -c(1:5)], index.family="sorensen")

betamulti.18 <- beta.multi(MM[c(2, 107),
                              -c(1:5)], index.family="sorensen")

betamulti.22 <- beta.multi(MM[c(7, 95),
                              -c(1:5)], index.family="sorensen")

betamulti.28 <- beta.multi(MM[c(3, 108),
                              -c(1:5)], index.family="sorensen")

betamulti.29 <- beta.multi(MM[c(22, 111),
                              -c(1:5)], index.family="sorensen")

betamulti.30 <- beta.multi(MM[c(14, 103),
                              -c(1:5)], index.family="sorensen") ##ver o que deu errado

betamulti.31 <- beta.multi(MM[c(8, 97),
                              -c(1:5)], index.family="sorensen")

betamulti.32 <- beta.multi(MM[c(23, 112),
                              -c(1:5)], index.family="sorensen")

betamulti.40 <- beta.multi(MM[c(15, 104),
                              -c(1:5)], index.family="sorensen")

betamulti.42 <- beta.multi(MM[c(24, 113),
                              -c(1:5)], index.family="sorensen")

betamulti.43 <- beta.multi(MM[c(16, 105),
                              -c(1:5)], index.family="sorensen")

betamulti.46 <- beta.multi(MM[c(4, 109),
                              -c(1:5)], index.family="sorensen")

#UD
betamulti.12 <- beta.multi(MM[c(1,105),
                              -c(1:5)], index.family="sorensen")

betamulti.14 <- beta.multi(MM[c(5, 93),
                              -c(1:5)], index.family="sorensen")

betamulti.15 <- beta.multi(MM[c(21, 109),
                              -c(1:5)], index.family="sorensen")

betamulti.20 <- beta.multi(MM[c(22, 110),
                              -c(1:5)], index.family="sorensen")

betamulti.21 <- beta.multi(MM[c(6, 94),
                              -c(1:5)], index.family="sorensen")

betamulti.23 <- beta.multi(MM[c(7,95),
                              -c(1:5)], index.family="sorensen")

betamulti.24 <- beta.multi(MM[c(2, 106),
                              -c(1:5)], index.family="sorensen")

betamulti.25 <- beta.multi(MM[c(13, 101),
                              -c(1:5)], index.family="sorensen")

betamulti.26 <- beta.multi(MM[c(3, 107),
                              -c(1:5)], index.family="sorensen")

betamulti.27 <- beta.multi(MM[c(23, 111),
                              -c(1:5)], index.family="sorensen")

betamulti.33 <- beta.multi(MM[c(14, 102),
                              -c(1:5)], index.family="sorensen") ##conferir essa aqui

betamulti.36 <- beta.multi(MM[c(8, 96),
                              -c(1:5)], index.family="sorensen")

betamulti.37 <- beta.multi(MM[c(15, 103),
                              -c(1:5)], index.family="sorensen")

betamulti.38 <- beta.multi(MM[c(24, 112),
                              -c(1:5)], index.family="sorensen")

betamulti.39 <- beta.multi(MM[c(4, 108),
                              -c(1:5)], index.family="sorensen")

betamulti.41 <- beta.multi(MM[c(16, 104),
                              -c(1:5)], index.family="sorensen")

resultado.betatemporal   <- rbind(unlist(betamulti.12),
                                  unlist(betamulti.14),
                                  unlist(betamulti.15),
                                  unlist(betamulti.16),
                                  unlist(betamulti.17),
                                  unlist(betamulti.18),
                                  unlist(betamulti.20),
                                  unlist(betamulti.21),
                                  unlist(betamulti.22),
                                  unlist(betamulti.23),
                                  unlist(betamulti.24),
                                  unlist(betamulti.25),
                                  unlist(betamulti.26),
                                  unlist(betamulti.27),
                                  unlist(betamulti.28),
                                  unlist(betamulti.29),
                                  unlist(betamulti.3),
                                  unlist(betamulti.30),
                                  unlist(betamulti.31),
                                  unlist(betamulti.32),
                                  unlist(betamulti.33),
                                  unlist(betamulti.36),
                                  unlist(betamulti.37),
                                  unlist(betamulti.38),
                                  unlist(betamulti.39),
                                  unlist(betamulti.40),
                                  unlist(betamulti.41),
                                  unlist(betamulti.42),
                                  unlist(betamulti.43),
                                  unlist(betamulti.46),
                                  unlist(betamulti.8),
                                  unlist(betamulti.9))
####
tratamento <- c("FE", "EARLY", "LATE", "LATE", "EARLY", "FE", "LATE", "EARLY",
                "EARLY", "EARLY", "FE", "MODAL", "FE", "LATE", "FE", "LATE",
                "EARLY", "MODAL", "EARLY", "LATE", "MODAL", "EARLY", "MODAL",
                "LATE", "FE", "MODAL", "MODAL", "LATE", "MODAL", "FE", "FE", 
                "MODAL")

invasora <- c("UD","UD","UD","MM","MM","MM", "UD","UD","MM",
              "UD","UD","UD","UD","UD","MM","MM","MM", "MM",
              "MM", "MM","UD","UD","UD","UD","UD","MM","UD",
              "MM", "MM", "MM", "MM", "MM")

beta_temporal <- data.frame(tratamento, invasora, resultado.betatemporal)
beta_temporal$tratamento <- as.factor(beta_temporal$tratamento)
beta_temporal$invasora <- as.factor(beta_temporal$invasora)
str(beta_temporal)


write.csv2(beta.modal, file = "26042021_betasubplot_modal_IF.csv", 
           sep = "\t", row.names = F)
