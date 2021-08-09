#FAPESP - Análises Estatísticas em RAC_change
#Carregando os pacotes necessários:
 library(tidyverse)

#Arquivo que será utilizado:
 itirapina_rac <- read.table("RAC_change.txt", h = T,
                             stringsAsFactors = T, sep="\t")
 str(itirapina_rac)
 itirapina_rac$parcela <- as.factor(itirapina_rac$parcela)
 summary(itirapina_rac)

#gráficos
 #richness
 itirapina_rac %>%
   mutate(tratamento = fct_relevel(tratamento,
                       "FE", "EAR", "MOD", "LATE")) %>%
   ggplot(aes(x = tratamento, y = richness_change, fill = invasora)) +
   geom_boxplot(width = 0.4, alpha = 0.6)
 
 #evenness
 itirapina_rac %>%
   mutate(tratamento = fct_relevel(tratamento,
                       "FE", "EAR", "MOD", "LATE")) %>%
   ggplot(aes(x = tratamento, y = evenness_change, fill = invasora)) +
   geom_boxplot(width = 0.4, alpha = 0.6)
 
 #rank_change
 itirapina_rac %>%
   mutate(tratamento = fct_relevel(tratamento,
                                   "FE", "EAR", "MOD", "LATE")) %>%
   ggplot(aes(x = tratamento, y = rank_change, fill = invasora)) +
   geom_boxplot(width = 0.4, outlier.shape = NA, alpha = 0.6)
 

 
#modelos:
 modelo.richness <- lm(richness_change ~ tratamento*invasora, 
                        data = itirapina_rac)
 summary(modelo.richness) 
 
 modelo.evenness <- lm(evenness_change ~ tratamento*invasora, 
                       data = itirapina_rac)
 summary(modelo.evenness)
 
 modelo.rank <- lm(rank_change ~ tratamento*invasora,
                   data = itirapina_rac)
 summary(modelo.rank) 

####com invasoras:
 itirapina_rac_inv <- read.table("RAC_change_invasora.txt", h = T,
                             stringsAsFactors = T, sep="\t")
 str(itirapina_rac_inv)
 itirapina_rac_inv$parcela <- as.factor(itirapina_rac_inv$parcela)
 summary(itirapina_rac_inv)

 modelo.richness.inv <- lm(richness_change ~ tratamento*invasora, 
                       data = itirapina_rac_inv)
 summary(modelo.richness.inv) 
 
 modelo.evenness.inv <- lm(evenness_change ~ tratamento*invasora, 
                       data = itirapina_rac_inv)
 summary(modelo.evenness.inv)
 
 modelo.rank.inv <- lm(rank_change ~ tratamento*invasora,
                   data = itirapina_rac_inv)
 summary(modelo.rank.inv) 
 