#FAPESP - Análises Estatísticas em RAC_change
#Carregando os pacotes necessários:
 library(tidyverse)

#Arquivo que será utilizado:
 itirapina_rac <- read.table("RAC_change.txt", h = T,
                             stringsAsFactors = T, sep="\t") #inicial-final
 str(itirapina_rac)
 itirapina_rac$parcela <- as.factor(itirapina_rac$parcela)
 summary(itirapina_rac)

#gráficos
 #richness
 itirapina_rac %>%
   mutate(tratamento = fct_relevel(tratamento,
                       "FE", "EAR", "MOD", "LATE")) %>%
   ggplot(aes(x = tratamento, y = richness_change, fill = invasora)) +
   geom_boxplot(width = 0.5, alpha = 0.6) +
   labs(y = "Riqueza\n", x = "\nTratamento") +
   scale_y_continuous(limits = c(0, 1)) +
   scale_x_discrete(labels = c("Exclusão", "Precoce", "Modal", "Tardia")) +
   scale_fill_manual(values = c("#34cf48", "#cf6d34"),
                     labels = c("M. minutiflora", "U. decumbens")) +
    scale_colour_manual(values = c("#34cf48", "#cf6d34"),
                        guide = F) +
    theme(legend.position="top", 
          text = element_text(family = "Calibri", size = 25),
          axis.text.x = element_text(size = 22),
          axis.text.y = element_text(size = 22),
          axis.title.y = element_text(size = 24),
          axis.title.x = element_text(size = 24),
          legend.text = element_text(size= 20))
 
 #evenness
 itirapina_rac %>%
   mutate(tratamento = fct_relevel(tratamento,
                       "FE", "EAR", "MOD", "LATE")) %>%
   ggplot(aes(x = tratamento, y = evenness_change, fill = invasora)) +
   geom_boxplot(width = 0.4, alpha = 0.6) +
   scale_y_continuous(limits = c(-0.6, 0.6))
 
 #rank_change
 itirapina_rac %>%
   mutate(tratamento = fct_relevel(tratamento,
                                   "FE", "EAR", "MOD", "LATE")) %>%
   ggplot(aes(x = tratamento, y = rank_change, fill = invasora)) +
   geom_boxplot(width = 0.4, outlier.shape = NA, alpha = 0.6) +
    labs(y = "Rank\n", x = "\nTratamento") +
    scale_y_continuous(limits = c(0, 1)) +
    scale_x_discrete(labels = c("Exclusão", "Precoce", "Modal", "Tardia")) +
    scale_fill_manual(values = c("#34cf48", "#cf6d34"),
                      labels = c("M. minutiflora", "U. decumbens")) +
    scale_colour_manual(values = c("#34cf48", "#cf6d34"),
                        guide = F)
 
 #gains
 itirapina_rac %>%
    mutate(tratamento = fct_relevel(tratamento,
                                    "FE", "EAR", "MOD", "LATE")) %>%
    ggplot(aes(x = tratamento, y = gains, fill = invasora)) +
    geom_boxplot(width = 0.4, outlier.shape = NA, alpha = 0.6) +
    labs(y = "Rank\n", x = "\nTratamento") +
    scale_y_continuous(limits = c(0, 1)) +
    scale_x_discrete(labels = c("Exclusão", "Precoce", "Modal", "Tardia")) +
    scale_fill_manual(values = c("#34cf48", "#cf6d34"),
                      labels = c("M. minutiflora", "U. decumbens")) +
    scale_colour_manual(values = c("#34cf48", "#cf6d34"),
                        guide = F)
 
 #losses
 itirapina_rac %>%
    mutate(tratamento = fct_relevel(tratamento,
                                    "FE", "EAR", "MOD", "LATE")) %>%
    ggplot(aes(x = tratamento, y = losses, fill = invasora)) +
    geom_boxplot(width = 0.4, outlier.shape = NA, alpha = 0.6) +
    labs(y = "Rank\n", x = "\nTratamento") +
    scale_y_continuous(limits = c(0, 1)) +
    scale_x_discrete(labels = c("Exclusão", "Precoce", "Modal", "Tardia")) +
    scale_fill_manual(values = c("#34cf48", "#cf6d34"),
                      labels = c("M. minutiflora", "U. decumbens")) +
    scale_colour_manual(values = c("#34cf48", "#cf6d34"),
                        guide = F)
 
 
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
 