# Projeto FAPESP - RAC_change
#carregando os pacotes que serão usados
library(tidyverse)
library(codyn)

setwd("C:/R/fapesp_gdo")
itirapina_base <- read.table("30032021.txt", h = T,
                             stringsAsFactors = T, sep="\t")
#conferência de dados
str(itirapina_base)
summary(itirapina_base)

itirapina_base$parcela <- as.factor(itirapina_base$parcela)
itirapina_base$subparcela <- as.factor(itirapina_base$subparcela)
itirapina_base$tempo <- as.factor(itirapina_base$tempo)

distinct(itirapina_base, tratamento)

#manipulação inicial de dados (retirada de espécies, mudança de grafia)
itirapina_mod <- itirapina_base %>%
  filter(especie != "bio morta" &
           especie != "bio morta mel" &
           especie != "solo nu" &
           especie != "bio morta uroch" &
           cobertura != 0) %>%
  mutate(tratamento = recode(tratamento, Precoce = "precoce")) %>%
  select(!grupo.funcional)

summary(itirapina_mod)

#para cada parcela - subsets:
df_3 <- subset(itirapina_mod, tempo == c(0, 9, 14, 21, 24) & parcela %in% 3)
df_17 <- subset(itirapina_mod, tempo == c(0, 9, 14, 21, 24) & parcela %in% 17)
df_22 <- subset(itirapina_mod, tempo == c(0, 9, 14, 21, 24) & parcela %in% 22)
df_31 <- subset(itirapina_mod, tempo == c(0, 9, 14, 21, 24) & parcela %in% 31)

df_14 <- subset(itirapina_mod, tempo == c(0, 9, 14, 21, 24) & parcela %in% 14)
df_21 <- subset(itirapina_mod, tempo == c(0, 9, 14, 21, 24) & parcela %in% 21)
df_23 <- subset(itirapina_mod, tempo == c(0, 9, 14, 21, 24) & parcela %in% 23)
df_36 <- subset(itirapina_mod, tempo == c(0, 9, 14, 21, 24) & parcela %in% 36)

df_9 <- subset(itirapina_mod, tempo == c(0, 6, 11, 18, 23) & parcela %in% 9)
df_30 <- subset(itirapina_mod, tempo == c(0, 6, 11, 18, 23) & parcela %in% 30)
df_40 <- subset(itirapina_mod, tempo == c(0, 6, 11, 18, 23) & parcela %in% 40)
df_43 <- subset(itirapina_mod, tempo == c(0, 6, 11, 18, 23) & parcela %in% 43)

df_25 <- subset(itirapina_mod, tempo == c(0, 6, 11, 18, 23) & parcela %in% 25)
df_33 <- subset(itirapina_mod, tempo == c(0, 6, 11, 18, 23) & parcela %in% 33)
df_37 <- subset(itirapina_mod, tempo == c(0, 6, 11, 18, 23) & parcela %in% 37)
df_41 <- subset(itirapina_mod, tempo == c(0, 6, 11, 18, 23) & parcela %in% 41)

df_16 <- subset(itirapina_mod, tempo == c(0, 5, 10, 17, 25) & parcela %in% 16)
df_29 <- subset(itirapina_mod, tempo == c(0, 5, 10, 17, 25) & parcela %in% 29)
df_32 <- subset(itirapina_mod, tempo == c(0, 5, 10, 17, 25) & parcela %in% 32)
df_42 <- subset(itirapina_mod, tempo == c(0, 5, 10, 17, 25) & parcela %in% 42)

df_15 <- subset(itirapina_mod, tempo == c(0, 5, 10, 17, 25) & parcela %in% 15)
df_20 <- subset(itirapina_mod, tempo == c(0, 5, 10, 17, 25) & parcela %in% 20)
df_27 <- subset(itirapina_mod, tempo == c(0, 5, 10, 17, 25) & parcela %in% 27)
df_38 <- subset(itirapina_mod, tempo == c(0, 5, 10, 17, 25) & parcela %in% 38)

df_8 <- subset(itirapina_mod, tempo == c(0, 9, 14, 21, 24) & parcela %in% 8)
df_18 <- subset(itirapina_mod, tempo == c(0, 9, 14, 21, 24) & parcela %in% 18)
df_28 <- subset(itirapina_mod, tempo == c(0, 9, 14, 21, 24) & parcela %in% 28)
df_46 <- subset(itirapina_mod, tempo == c(0,9, 14, 21, 24) & parcela %in% 46)

df_12 <- subset(itirapina_mod, tempo == c(0, 9, 14, 21, 24) & parcela %in% 12)
df_24 <- subset(itirapina_mod, tempo == c(0, 9, 14, 21, 24) & parcela %in% 24)
df_26 <- subset(itirapina_mod, tempo == c(0, 9, 14, 21, 24) & parcela %in% 26)
df_39 <- subset(itirapina_mod, tempo == c(0, 9, 14, 21, 24) & parcela %in% 39)

##rac_diff

rac_3 <- RAC_change(df_3, time.var = "tempo", species.var = "especie",
                    abundance.var = "cobertura", replicate.var = "subparcela")
rac_3$invasora <- c("MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM")
rac_3$parcela <- c(3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3)
rac_3$tratamento <- c("EAR","EAR","EAR","EAR","EAR","EAR","EAR","EAR","EAR","EAR","EAR","EAR","EAR","EAR","EAR")
rac_17 <- RAC_change(df_17, time.var = "tempo", species.var = "especie",
                    abundance.var = "cobertura", replicate.var = "subparcela")
rac_17$invasora <- c("MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM")
rac_17$parcela <- c(17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17)
rac_17$tratamento <- c("EAR","EAR","EAR","EAR","EAR","EAR", "EAR","EAR","EAR","EAR","EAR","EAR", "EAR","EAR","EAR","EAR","EAR","EAR", "EAR", "EAR")
rac_22 <- RAC_change(df_22, time.var = "tempo", species.var = "especie",
                    abundance.var = "cobertura", replicate.var = "subparcela")
rac_22$invasora <- c("MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM")
rac_22$parcela <- c(22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22)
rac_22$tratamento <- c("EAR","EAR","EAR","EAR","EAR","EAR","EAR","EAR", "EAR","EAR","EAR","EAR","EAR","EAR")
rac_31 <- RAC_change(df_31, time.var = "tempo", species.var = "especie",
                     abundance.var = "cobertura", replicate.var = "subparcela")
rac_31$invasora <- c("MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM")
rac_31$parcela <- c(31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31)
rac_31$tratamento <- c("EAR","EAR","EAR","EAR","EAR","EAR","EAR", "EAR","EAR","EAR","EAR","EAR","EAR","EAR", "EAR","EAR","EAR","EAR","EAR")

rac_14 <- RAC_change(df_14, time.var = "tempo", species.var = "especie",
                    abundance.var = "cobertura", replicate.var = "subparcela")
rac_14$invasora <- c("UD", "UD", "UD", "UD", "UD", "UD", "UD", "UD", "UD")
rac_14$parcela <- c(14, 14, 14, 14, 14, 14, 14, 14, 14)
rac_14$tratamento <- c("EAR","EAR","EAR","EAR","EAR","EAR","EAR", "EAR", "EAR")
rac_21 <- RAC_change(df_21, time.var = "tempo", species.var = "especie",
                     abundance.var = "cobertura", replicate.var = "subparcela")
rac_21$invasora <- c("UD", "UD", "UD", "UD", "UD", "UD", "UD", "UD", "UD", "UD")
rac_21$parcela <- c(21, 21, 21, 21, 21, 21, 21, 21, 21, 21)
rac_21$tratamento <- c("EAR","EAR","EAR","EAR","EAR","EAR", "EAR","EAR","EAR","EAR")
rac_23 <- RAC_change(df_23, time.var = "tempo", species.var = "especie",
                     abundance.var = "cobertura", replicate.var = "subparcela")
rac_23$invasora <- c("UD", "UD", "UD", "UD", "UD", "UD", "UD")
rac_23$parcela <- c(23, 23, 23, 23, 23, 23, 23)
rac_23$tratamento <- c("EAR","EAR","EAR","EAR","EAR", "EAR", "EAR")
rac_36 <- RAC_change(df_36, time.var = "tempo", species.var = "especie",
                     abundance.var = "cobertura", replicate.var = "subparcela")
rac_36$invasora <- c("UD", "UD", "UD", "UD", "UD", "UD", "UD", "UD", "UD", "UD", "UD", "UD", "UD", "UD", "UD", "UD")
rac_36$parcela <- c(36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36)
rac_36$tratamento <- c("EAR","EAR","EAR","EAR","EAR","EAR","EAR", "EAR", "EAR", "EAR","EAR","EAR","EAR","EAR", "EAR", "EAR")

rac_9 <- RAC_change(df_9, time.var = "tempo", species.var = "especie",
                     abundance.var = "cobertura", replicate.var = "subparcela")
rac_9$invasora <- c("MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM")
rac_9$parcela <- c(9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9)
rac_9$tratamento <- c("MOD","MOD","MOD","MOD","MOD","MOD","MOD","MOD","MOD", "MOD","MOD","MOD","MOD","MOD","MOD","MOD","MOD")
rac_30 <- RAC_change(df_30, time.var = "tempo", species.var = "especie",
                     abundance.var = "cobertura", replicate.var = "subparcela")
rac_30$invasora <- c("MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM")
rac_30$parcela <- c(30, 30, 30, 30, 30, 30, 30, 30, 30)
rac_30$tratamento <- c("MOD","MOD","MOD","MOD", "MOD","MOD","MOD","MOD", "MOD")
rac_40 <- RAC_change(df_40, time.var = "tempo", species.var = "especie",
                     abundance.var = "cobertura", replicate.var = "subparcela")
rac_40$invasora <- c("MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM")
rac_40$parcela <- c(40, 40, 40 ,40, 40, 40, 40 ,40, 40)
rac_40$tratamento <- c("MOD","MOD","MOD","MOD","MOD", "MOD","MOD","MOD","MOD")
rac_43 <- RAC_change(df_43, time.var = "tempo", species.var = "especie",
                     abundance.var = "cobertura", replicate.var = "subparcela")
rac_43$invasora <- c("MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM")
rac_43$parcela <- c(43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43)
rac_43$tratamento <- c("MOD","MOD","MOD","MOD","MOD","MOD","MOD", "MOD","MOD","MOD","MOD","MOD","MOD","MOD", "MOD", "MOD")

rac_25 <- RAC_change(df_25, time.var = "tempo", species.var = "especie",
                     abundance.var = "cobertura", replicate.var = "subparcela")
rac_25$invasora <- c("UD", "UD", "UD", "UD", "UD", "UD", "UD", "UD", "UD")
rac_25$parcela <- c(25, 25, 25, 25, 25, 25, 25, 25, 25)
rac_25$tratamento <- c("EAR","EAR","EAR","EAR","EAR","EAR", "EAR","EAR","EAR")
rac_33 <- RAC_change(df_33, time.var = "tempo", species.var = "especie",
                     abundance.var = "cobertura", replicate.var = "subparcela")
rac_33$invasora <- c("UD", "UD", "UD", "UD", "UD")
rac_33$parcela <- c(33, 33, 33, 33, 33)
rac_33$tratamento <- c("EAR","EAR", "EAR","EAR","EAR")
rac_37 <- RAC_change(df_37, time.var = "tempo", species.var = "especie",
                     abundance.var = "cobertura", replicate.var = "subparcela")
rac_37$invasora <- c("UD", "UD", "UD", "UD", "UD", "UD","UD", "UD", "UD", "UD", "UD", "UD")
rac_37$parcela <- c(37, 37, 37, 37, 37, 37,37, 37, 37, 37, 37, 37)
rac_37$tratamento <- c("EAR","EAR","EAR","EAR","EAR","EAR","EAR","EAR","EAR","EAR","EAR","EAR")
rac_41 <- RAC_change(df_41, time.var = "tempo", species.var = "especie",
                     abundance.var = "cobertura", replicate.var = "subparcela")
rac_41$invasora <- c("UD", "UD", "UD", "UD", "UD", "UD","UD", "UD", "UD", "UD", "UD", "UD", "UD", "UD", "UD", "UD","UD", "UD", "UD", "UD", "UD", "UD", "UD", "UD", "UD", "UD")
rac_41$parcela <- c(41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41)
rac_41$tratamento <- c("EAR","EAR","EAR","EAR","EAR","EAR","EAR","EAR","EAR","EAR", "EAR","EAR","EAR","EAR","EAR","EAR","EAR","EAR","EAR","EAR", "EAR","EAR","EAR","EAR","EAR","EAR")

rac_16 <- RAC_change(df_16, time.var = "tempo", species.var = "especie",
                     abundance.var = "cobertura", replicate.var = "subparcela")
rac_16$invasora <- c("MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM")
rac_16$parcela <- c(16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16)
rac_16$tratamento <- c("LATE","LATE","LATE","LATE","LATE","LATE","LATE","LATE","LATE","LATE","LATE","LATE","LATE")
rac_29 <- RAC_change(df_29, time.var = "tempo", species.var = "especie",
                     abundance.var = "cobertura", replicate.var = "subparcela")
rac_29$invasora <- c("MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM")
rac_29$parcela <- c(29, 29 , 29, 29, 29,29, 29 , 29, 29, 29,29)
rac_29$tratamento <- c("LATE","LATE","LATE","LATE","LATE","LATE","LATE","LATE","LATE","LATE", "LATE")
rac_32 <- RAC_change(df_32, time.var = "tempo", species.var = "especie",
                     abundance.var = "cobertura", replicate.var = "subparcela")
rac_32$invasora <- c("MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM","MM", "MM", "MM", "MM", "MM")
rac_32$parcela <- c(32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32)
rac_32$tratamento <- c("LATE","LATE","LATE","LATE","LATE","LATE","LATE", "LATE","LATE","LATE","LATE","LATE","LATE","LATE", "LATE","LATE","LATE","LATE","LATE")
rac_42 <- RAC_change(df_42, time.var = "tempo", species.var = "especie",
                     abundance.var = "cobertura", replicate.var = "subparcela")
rac_42$invasora <- c("MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM")
rac_42$parcela <- c(42, 42, 42, 42, 42, 42, 42,42, 42, 42, 42, 42, 42, 42, 42, 42)
rac_42$tratamento <- c("LATE","LATE","LATE","LATE","LATE","LATE","LATE","LATE","LATE","LATE","LATE","LATE","LATE","LATE","LATE", "LATE")

rac_15 <- RAC_change(df_15, time.var = "tempo", species.var = "especie",
                     abundance.var = "cobertura", replicate.var = "subparcela")
rac_15$invasora <- c("UD", "UD", "UD", "UD", "UD", "UD", "UD", "UD", "UD", "UD", "UD")
rac_15$parcela <- c(15, 15, 15, 15, 15, 15,15, 15, 15, 15, 15)
rac_15$tratamento <- c("LATE","LATE","LATE","LATE","LATE","LATE","LATE","LATE","LATE","LATE","LATE")
rac_20 <- RAC_change(df_20, time.var = "tempo", species.var = "especie",
                     abundance.var = "cobertura", replicate.var = "subparcela")
rac_20$invasora <- c("UD", "UD", "UD", "UD", "UD","UD", "UD", "UD")
rac_20$parcela <- c(20, 20, 20, 20, 20, 20, 20, 20)
rac_20$tratamento <- c("LATE","LATE","LATE","LATE","LATE","LATE","LATE","LATE")
rac_27 <- RAC_change(df_27, time.var = "tempo", species.var = "especie",
                     abundance.var = "cobertura", replicate.var = "subparcela")
rac_27$invasora <- c("UD", "UD", "UD", "UD", "UD","UD","UD", "UD", "UD","UD")
rac_27$parcela <- c(27, 27, 27, 27, 27, 27, 27, 27, 27, 27)
rac_27$tratamento <- c("LATE","LATE","LATE","LATE","LATE","LATE","LATE","LATE","LATE","LATE")
rac_38 <- RAC_change(df_38, time.var = "tempo", species.var = "especie",
                     abundance.var = "cobertura", replicate.var = "subparcela")
rac_38$invasora <- c("UD", "UD", "UD", "UD", "UD","UD","UD", "UD", "UD", "UD", "UD","UD", "UD", "UD")
rac_38$parcela <- c(38, 38, 38, 38, 38, 38,38, 38, 38, 38, 38, 38, 38, 38)
rac_38$tratamento <- c("LATE","LATE","LATE","LATE","LATE","LATE", "LATE","LATE","LATE","LATE","LATE","LATE", "LATE", "LATE")

rac_8 <- RAC_change(df_8, time.var = "tempo", species.var = "especie",
                     abundance.var = "cobertura", replicate.var = "subparcela")
rac_8$invasora <- c("MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM")
rac_8$parcela <- c(8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8)
rac_8$tratamento <- c("FE", "FE", "FE", "FE", "FE", "FE","FE", "FE", "FE", "FE", "FE", "FE", "FE")
rac_18 <- RAC_change(df_18, time.var = "tempo", species.var = "especie",
                     abundance.var = "cobertura", replicate.var = "subparcela")
rac_18$invasora <- c("MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM")
rac_18$parcela <- c(18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18)
rac_18$tratamento <- c("FE", "FE", "FE", "FE", "FE", "FE", "FE", "FE", "FE", "FE", "FE", "FE", "FE", "FE", "FE", "FE", "FE")
rac_28 <- RAC_change(df_28, time.var = "tempo", species.var = "especie",
                     abundance.var = "cobertura", replicate.var = "subparcela")
rac_28$invasora <- c("MM", "MM", "MM", "MM", "MM", "MM", "MM","MM", "MM", "MM", "MM", "MM", "MM", "MM", "MM")
rac_28$parcela <- c(28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28)
rac_28$tratamento <- c("FE", "FE", "FE", "FE", "FE", "FE", "FE","FE", "FE", "FE", "FE", "FE", "FE", "FE","FE")
rac_46 <- RAC_change(df_46, time.var = "tempo", species.var = "especie",
                     abundance.var = "cobertura", replicate.var = "subparcela")
rac_46$invasora <- c("MM", "MM", "MM", "MM", "MM","MM", "MM", "MM", "MM", "MM")
rac_46$parcela <- c(46, 46, 46, 46, 46, 46, 46, 46, 46, 46)
rac_46$tratamento <- c("FE", "FE", "FE", "FE", "FE","FE", "FE", "FE", "FE", "FE")

rac_12 <- RAC_change(df_12, time.var = "tempo", species.var = "especie",
                     abundance.var = "cobertura", replicate.var = "subparcela")
rac_12$invasora <- c("UD", "UD", "UD", "UD", "UD","UD", "UD", "UD", "UD","UD", "UD", "UD", "UD", "UD","UD", "UD", "UD", "UD")
rac_12$parcela <- c(12, 12, 12, 12, 12, 12, 12, 12, 12,12, 12, 12, 12, 12, 12, 12, 12, 12)
rac_12$tratamento <- c("FE", "FE", "FE", "FE", "FE", "FE", "FE", "FE", "FE","FE", "FE", "FE", "FE", "FE", "FE", "FE", "FE", "FE")
rac_24 <- RAC_change(df_24, time.var = "tempo", species.var = "especie",
                     abundance.var = "cobertura", replicate.var = "subparcela")
rac_24$invasora <- c("UD", "UD", "UD", "UD", "UD","UD", "UD", "UD", "UD","UD", "UD","UD", "UD", "UD", "UD")
rac_24$parcela <- c(24, 24, 24, 24, 24, 24, 24, 24, 24,24, 24, 24, 24, 24, 24)
rac_24$tratamento <- c("FE", "FE", "FE", "FE", "FE", "FE", "FE", "FE", "FE","FE", "FE", "FE", "FE", "FE", "FE")
rac_26 <- RAC_change(df_26, time.var = "tempo", species.var = "especie",
                     abundance.var = "cobertura", replicate.var = "subparcela")
rac_26$invasora <- c("UD", "UD", "UD", "UD", "UD","UD", "UD", "UD", "UD","UD", "UD", "UD", "UD", "UD")
rac_26$parcela <- c(26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26)
rac_26$tratamento <- c("FE", "FE", "FE", "FE", "FE", "FE", "FE", "FE", "FE","FE", "FE", "FE", "FE", "FE")
rac_39 <- RAC_change(df_39, time.var = "tempo", species.var = "especie",
                     abundance.var = "cobertura", replicate.var = "subparcela")
rac_39$invasora <- c("UD", "UD", "UD", "UD", "UD","UD", "UD", "UD", "UD", "UD", "UD", "UD","UD", "UD", "UD")
rac_39$parcela <- c(39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39)
rac_39$tratamento <- c("FE", "FE", "FE", "FE", "FE", "FE", "FE", "FE", "FE", "FE", "FE", "FE", "FE", "FE", "FE")

RAC_subplots <- rbind(rac_3, rac_17, rac_22, rac_31, rac_14, rac_21, rac_23,
                      rac_36, rac_9, rac_30, rac_40, rac_43, rac_25, rac_33,
                      rac_37, rac_41, rac_16, rac_29, rac_32, rac_42, rac_15,
                      rac_20, rac_27, rac_38, rac_8, rac_18, rac_28, rac_46,
                      rac_12, rac_24, rac_26, rac_39)

RAC_subplots <- RAC_subplots %>%
  mutate(tempo = recode(tempo, "0" = "1",
                        "9" = "2",
                        "14" = "3",
                        "21" = "4",
                        "24" = "5",
                        "5" = "2",
                        "10" = "3",
                        "17" = "4",
                        "25" = "5",
                        "6" = "2",
                        "11" = "3",
                        "18" = "4",
                        "23" = "5"))

RAC_subplots <- RAC_subplots %>%
  mutate(tempo2 = recode(tempo2, "0" = "1",
                        "9" = "2",
                        "14" = "3",
                        "21" = "4",
                        "24" = "5",
                        "5" = "2",
                        "10" = "3",
                        "17" = "4",
                        "25" = "5",
                        "6" = "2",
                        "11" = "3",
                        "18" = "4",
                        "23" = "5"))


#passagem de dados organizados para nova planilha
write.csv2(RAC_subplots, file = "RAC_seq_subplots.csv", row.names = T)
