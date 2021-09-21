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

#agrupando os valores referentes às subparcelas 
itirapina_plots_mean <- itirapina_mod %>%
  group_by(data, tratamento, tempo, invasora, especie, parcela) %>%
  summarise(across(where(is.numeric), mean))

itirapina_plots_qnt <- itirapina_mod %>%
  group_by(data, tratamento, tempo, invasora, especie, parcela) %>%
  summarise(across(where(is.numeric), median))

#subsets dos tratamentos
df_precoce <- subset(itirapina_plots_qnt, tempo == c(0, 24)
                     & tratamento %in% "precoce")

df_controle <- subset(itirapina_plots_qnt, tempo == c(0,29)
                      & tratamento %in% "controle")

df_tardia <- subset(itirapina_plots_qnt, tempo == c(0,25)
                    & tratamento %in% "tardia")

df_modal <- subset(itirapina_plots_qnt, tempo == c(0,23)
                   & tratamento %in% "modal")

#função aplicada pra cada tratamento
rac_controle <- RAC_change(df = df_controle,
                           time.var = "tempo",
                           species.var = "especie",
                           abundance.var = "cobertura",
                           replicate.var = "parcela")
rac_controle$invasora <- c("MM", "UD", "MM", "UD", "UD", "MM", "UD", "MM")
rac_controle$tratamento <- c("controle", "controle", "controle", "controle",
                             "controle", "controle", "controle", "controle")

rac_precoce <- RAC_change(df = df_precoce,
                          time.var = "tempo",
                          species.var = "especie",
                          abundance.var = "cobertura",
                          replicate.var = "parcela")
rac_precoce$invasora <- c("MM", "UD", "MM", "UD", "MM", "UD", "MM", "UD")
rac_precoce$tratamento <- c("precoce", "precoce", "precoce", "precoce",
                            "precoce", "precoce", "precoce", "precoce")

rac_modal <- RAC_change(df = df_modal,
                        time.var = "tempo",
                        species.var = "especie",
                        abundance.var = "cobertura",
                        replicate.var = "parcela")
rac_modal$invasora <- c("MM", "UD", "UD", "MM", "UD", "MM")
rac_modal$tratamento <- c("modal", "modal", "modal",
                          "modal", "modal", "modal")

rac_tardia <- RAC_change(df = df_tardia,
                         time.var = "tempo",
                         species.var = "especie",
                         abundance.var = "cobertura",
                         replicate.var = "parcela")
rac_tardia$invasora <- c("UD", "MM", "UD", "UD", "MM", "MM", "UD", "MM")
rac_tardia$tratamento <- c("tardia", "tardia", "tardia", "tardia",
                           "tardia", "tardia", "tardia", "tardia")

#organização da planilha final
df_rac <- rbind(rac_controle, rac_modal, rac_precoce, rac_tardia)
df_rac <- df_rac %>%
  mutate(tratamento = recode(tratamento,
                             "controle" = "FE",
                             "modal" = "MOD",
                             "tardia" = "LATE",
                             "precoce" = "EAR"))

#passagem de dados organizados para nova planilha
write.csv2(df_rac, file = "RAC_change_invasora.csv", row.names = T)
