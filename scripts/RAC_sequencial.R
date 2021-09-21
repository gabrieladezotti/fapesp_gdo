# Projeto FAPESP - RAC_change (sequencial)
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

#manipulação inicial de dados (retirada de espécies, mudança de grafia)
itirapina_mod <- itirapina_base %>%
  filter(especie != "bio morta" &
           especie != "bio morta mel" &
           especie != "solo nu" &
           especie != "bio morta uroch" &
           especie != "melinis minutiflora" &
           especie != "urochloa decumbens" &
           cobertura != 0) %>%
  mutate(tratamento = recode(tratamento, Precoce = "precoce")) %>%
  select(!grupo.funcional)

summary(itirapina_mod)
itirapina_mod$especie <- droplevels(itirapina_mod$especie)

#agrupando os valores referentes às subparcelas 
itirapina_plots_qnt <- itirapina_mod %>%
  group_by(data, tratamento, tempo, invasora, especie, parcela) %>%
  summarise(across(where(is.numeric), median))

#subsets dos tratamentos
df_precoce <- subset(itirapina_plots_qnt, tempo == c(0, 9, 14, 21, 24)
                     & tratamento %in% "precoce")

df_controle <- subset(itirapina_plots_qnt, tempo == c(0,9, 14, 21, 24)
                      & tratamento %in% "controle")

df_tardia <- subset(itirapina_plots_qnt, tempo == c(0, 5, 10, 17, 25)
                    & tratamento %in% "tardia")

df_modal <- subset(itirapina_plots_qnt, tempo == c(0, 6, 11, 18, 23)
                   & tratamento %in% "modal")

teste <- rbind(df_precoce, df_controle, df_tardia, df_modal)

teste <- teste %>%
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

MM <- subset(teste, invasora %in% "melinis minutiflora")
UD <- subset(teste, invasora %in% "urochloa decumbens")

#função aplicada pra cada tratamento
rac_mm <- RAC_difference(df = MM,
                              time.var = "tempo",
                              species.var = "especie",
                              abundance.var = "cobertura",
                              replicate.var = "parcela",
                              treatment.var = "tratamento",
                              pool = T)
invasora  <- vector("list", length(0))
for (i in 1:30) {
  invasora[[i]] <- print(paste("MM"))
}
invasora <- unlist(invasora)
rac_mm$invasora <- invasora

rac_ud <- RAC_difference(df = UD,
                         time.var = "tempo",
                         species.var = "especie",
                         abundance.var = "cobertura",
                         replicate.var = "parcela",
                         treatment.var = "tratamento",
                         pool = T)
invasora  <- vector("list", length(0))
for (i in 1:30) {
  invasora[[i]] <- print(paste("UD"))
}
invasora <- unlist(invasora)
rac_ud$invasora <- invasora

#organização da planilha final
df_rac <- rbind(rac_ud, rac_mm)

df_rac$trat <- paste(df_rac$tratamento, df_rac$tratamento2, sep = "-")

#passagem de dados organizados para nova planilha
write.csv2(df_rac, file = "RAC_change.csv", row.names = T)

###GRUPO FUNCIONAL
itirapina_mod_funcional <- itirapina_base %>%
  filter(especie != "bio morta" &
           especie != "bio morta mel" &
           especie != "solo nu" &
           especie != "bio morta uroch" &
           cobertura != 0) %>%
  mutate(tratamento = recode(tratamento, Precoce = "precoce"))

itirapina_mod_funcional <- itirapina_mod_funcional %>%
  select(!especie)

summary(itirapina_mod_funcional)
itirapina_mod_funcional$grupo.funcional <- droplevels(itirapina_mod_funcional$grupo.funcional)

#agrupando os valores referentes às subparcelas 
itirapina_funcional_qnt <- itirapina_mod_funcional %>%
  group_by(data, tratamento, tempo, invasora, grupo.funcional, parcela) %>%
  summarise(across(where(is.numeric), median))

#subsets dos tratamentos
df_precoce_fun <- subset(itirapina_funcional_qnt, tempo == c(0, 24)
                         & tratamento %in% "precoce")

df_controle_fun <- subset(itirapina_funcional_qnt, tempo == c(0,24)
                          & tratamento %in% "controle")

df_tardia_fun <- subset(itirapina_funcional_qnt, tempo == c(0,25)
                        & tratamento %in% "tardia")

df_modal_fun <- subset(itirapina_funcional_qnt, tempo == c(0,23)
                       & tratamento %in% "modal")

#função aplicada pra cada tratamento
rac_controle_fun <- RAC_change(df = df_controle_fun,
                               time.var = "tempo",
                               species.var = "grupo.funcional",
                               abundance.var = "cobertura",
                               replicate.var = "parcela")
rac_controle$invasora <- c("MM", "UD", "MM", "UD", "UD", "MM", "UD", "MM")
rac_controle$tratamento <- c("controle", "controle", "controle", "controle",
                             "controle", "controle", "controle", "controle")

rac_precoce_fun <- RAC_change(df = df_precoce_fun,
                              time.var = "tempo",
                              species.var = "grupo.funcional",
                              abundance.var = "cobertura",
                              replicate.var = "parcela")
rac_precoce$invasora <- c("MM", "UD", "MM", "UD", "MM", "UD", "MM", "UD")
rac_precoce$tratamento <- c("precoce", "precoce", "precoce", "precoce",
                            "precoce", "precoce", "precoce", "precoce")

rac_modal_fun <- RAC_change(df = df_modal_fun,
                            time.var = "tempo",
                            species.var = "grupo.funcional",
                            abundance.var = "cobertura",
                            replicate.var = "parcela")
rac_modal$invasora <- c("MM", "UD", "UD", "MM", "UD", "MM")
rac_modal$tratamento <- c("modal", "modal", "modal",
                          "modal", "modal", "modal")

rac_tardia_fun <- RAC_change(df = df_tardia_fun,
                         time.var = "tempo",
                         species.var = "grupo.funcional",
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
