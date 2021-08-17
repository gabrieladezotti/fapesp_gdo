##FAPESP - Análises Estatísticas em RAC_change (subplots)
#Carregando os pacotes necessários:
library(tidyverse)

#Arquivo que será utilizado:
itirapina_rac_subplots <- read.table("RAC_change_subplots.txt", h = T,
                          stringsAsFactors = T, sep="\t") #inicial-final
str(itirapina_rac_subplots)
itirapina_rac_subplots$parcela <- as.factor(itirapina_rac_subplots$parcela)
itirapina_rac_subplots$subparcela <- as.factor(itirapina_rac_subplots$subparcela)
summary(itirapina_rac_subplots)

##gráficos:
#richness
itirapina_rac_subplots %>%
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


#rank_change
itirapina_rac_subplots %>%
  mutate(tratamento = fct_relevel(tratamento,
                                  "FE", "EAR", "MOD", "LATE")) %>%
  ggplot(aes(x = tratamento, y = rank_change, fill = invasora)) +
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


#análises:
modelo_richsubplot <- lm(richness_change ~ tratamento*invasora, 
                         data = itirapina_rac_subplots)
summary(modelo_richsubplot)

tuckey_rich <- TukeyHSD(aov(lm(richness_change ~ tratamento*invasora, 
                               data = itirapina_rac_subplots)))
tuckey_rich

modelo_evensubplot <- lm(evenness_change ~ tratamento*invasora, 
                         data = itirapina_rac_subplots)
summary(modelo_evensubplot) #muito NA

modelo_ranksubplot <- lm(rank_change ~ tratamento*invasora,
                         data = itirapina_rac_subplots)
summary(modelo_ranksubplot)

tuckey_rank <- TukeyHSD(aov(lm(rank_change ~ tratamento*invasora, 
                               data = itirapina_rac_subplots)))
tuckey_rank
