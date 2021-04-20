#FAPESP-2021 - base_data
#organização de dados
library(tidyverse)

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
  dplyr::filter(especie != "bio morta" &
                especie != "bio morta mel" &
                especie != "solo nu" &
                especie != "bio morta uroch") %>%
  dplyr::filter(cobertura != 0) %>%
  mutate(tratamento = recode(tratamento,
                             Precoce = "precoce")) %>%
  dplyr::select(!grupo.funcional)

##long -> wide
 itirapina_wide <- itirapina_mod %>%
   tidyr::pivot_wider(names_from = especie,
                      values_from = cobertura,
                      values_fill = 0,
                      values_fn = sum)

##dividindo em soma de plots e subplots separados
  ##tirar dúvida tadeu##
  itirapina_plots <- itirapina_wide %>%
    group_by(data, tratamento, tempo, invasora, parcela) %>%
    summarise(across(where(is.numeric), sum)) 
   
 itirapina_plots_pa <- vegan::decostand(itirapina_plots[ ,6:152], "pa")
 itirapina_plots_pa <- cbind(itirapina_plots[ ,1:5], itirapina_plots_pa)
  
##subplots em pa:
 itirapina_subplots_pa <- vegan::decostand(itirapina_wide[ ,7:153], "pa")
 itirapina_subplots_pa <- cbind(itirapina_wide[ ,1:6], itirapina_subplots_pa)
 

##salvando novos dados: 
 write.csv2(itirapina_subplots_pa, file = "14042021_subplots_pa.csv", 
             sep = "\t", row.names = F)
 
 write.csv2(itirapina_plots_pa, file = "14042021_plots_pa.csv", 
            sep = "\t", row.names = F)
 
##########
 
 
 
 tidyr::separate(data, c("ano", "mês"), sep = "_")