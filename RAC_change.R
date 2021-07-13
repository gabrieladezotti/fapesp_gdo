##teste codyn
##testando rac_change para os dados da fapesp
library(tidyverse)
library(codyn)

setwd("C:/R/fapesp_gdo")
itirapina_base <- read.table("30032021.txt", h = T,
                             stringsAsFactors = T, sep="\t")
#conferindo dados
str(itirapina_base)
itirapina_base$parcela <- as.factor(itirapina_base$parcela)
itirapina_base$subparcela <- as.factor(itirapina_base$subparcela)

distinct(itirapina_base, tratamento)

#organizando os dados
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

itirapina_plots <- itirapina_mod %>%
  group_by(data, tratamento, tempo, invasora, especie, parcela) %>%
  summarise(across(where(is.numeric), sum))

df_precoce <- subset(itirapina_plots, tempo == c(0, 24) & 
            invasora %in% "melinis minutiflora" & tratamento %in% "precoce")

df_controle <- subset(itirapina_plots, tempo == c(0,29)
                      & tratamento %in% "controle")

df_tardia <- subset(itirapina_plots, tempo == c(0,25)
                      & tratamento %in% "tardia")

df_modal <- subset(itirapina_plots, tempo == c(0,23)
                      & tratamento %in% "modal")

rac_controle <- RAC_change(df = df_controle,
           time.var = "tempo",
           species.var = "especie",
           abundance.var = "cobertura",
           replicate.var = "parcela")

rac_precoce <- RAC_change(df = df_precoce,
                           time.var = "tempo",
                           species.var = "especie",
                           abundance.var = "cobertura",
                           replicate.var = "parcela")

rac_modal <- RAC_change(df = df_modal,
                           time.var = "tempo",
                           species.var = "especie",
                           abundance.var = "cobertura",
                           replicate.var = "parcela")

rac_tardia <- RAC_change(df = df_tardia,
                           time.var = "tempo",
                           species.var = "especie",
                           abundance.var = "cobertura",
                           replicate.var = "parcela")
