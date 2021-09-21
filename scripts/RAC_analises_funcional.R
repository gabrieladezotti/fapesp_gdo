#FAPESP - Análises Estatísticas em RAC_change com grupos funcionais
#Carregando os pacotes necessários:
library(tidyverse)

#Arquivo que será utilizado:
itirapina_fun <- read.table("RAC_change_fun.txt", h = T,
                            stringsAsFactors = T, sep="\t") #inicial-final
str(itirapina_fun)
itirapina_fun$parcela <- as.factor(itirapina_fun$parcela)
summary(itirapina_fun)

#gráficos
#richness
itirapina_fun %>%
  mutate(tratamento = fct_relevel(tratamento,
                                  "FE", "EAR", "MOD", "LATE")) %>%
  ggplot(aes(x = tratamento, y = richness_change, fill = invasora)) +
  geom_boxplot(width = 0.5, alpha = 0.6) +
  labs(y = "Riqueza\n", x = "\nTratamento") +
  scale_y_continuous(limits = c(-1, 1)) +
  scale_x_discrete(labels = c("Exclusão", "Precoce", "Modal", "Tardia")) +
  scale_fill_manual(values = c("#34cf48", "#cf6d34"),
                    labels = c("M. minutiflora", "U. decumbens")) +
  scale_colour_manual(values = c("#34cf48", "#cf6d34"),
                      guide = F) +
  theme_niwot() +
  theme(legend.position="top", 
        text = element_text(family = "Calibri", size = 25),
        axis.text.x = element_text(size = 22),
        axis.text.y = element_text(size = 22),
        axis.title.y = element_text(size = 24),
        axis.title.x = element_text(size = 24),
        legend.text = element_text(size= 20))

#evenness
itirapina_fun %>%
  mutate(tratamento = fct_relevel(tratamento,
                                  "FE", "EAR", "MOD", "LATE")) %>%
  ggplot(aes(x = tratamento, y = evenness_change, fill = invasora)) +
  geom_boxplot(width = 0.5, alpha = 0.6) +
  labs(y = "Equibilidade\n", x = "\nTratamento") +
  scale_y_continuous(limits = c(-1, 1)) +
  scale_x_discrete(labels = c("Exclusão", "Precoce", "Modal", "Tardia")) +
  scale_fill_manual(values = c("#34cf48", "#cf6d34"),
                    labels = c("M. minutiflora", "U. decumbens")) +
  scale_colour_manual(values = c("#34cf48", "#cf6d34"),
                      guide = F) +
  theme_niwot() +
  theme(legend.position="top", 
        text = element_text(family = "Calibri", size = 25),
        axis.text.x = element_text(size = 22),
        axis.text.y = element_text(size = 22),
        axis.title.y = element_text(size = 24),
        axis.title.x = element_text(size = 24),
        legend.text = element_text(size= 20))

#rank_change
itirapina_fun %>%
  mutate(tratamento = fct_relevel(tratamento,
                                  "FE", "EAR", "MOD", "LATE")) %>%
  ggplot(aes(x = tratamento, y = rank_change, fill = invasora)) +
  geom_boxplot(width = 0.5, alpha = 0.6) +
  labs(y = "Rank\n", x = "\nTratamento") +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_discrete(labels = c("Exclusão", "Precoce", "Modal", "Tardia")) +
  scale_fill_manual(values = c("#34cf48", "#cf6d34"),
                    labels = c("M. minutiflora", "U. decumbens")) +
  scale_colour_manual(values = c("#34cf48", "#cf6d34"),
                      guide = F) +
  theme_niwot() +
  theme(legend.position="top", 
        text = element_text(family = "Calibri", size = 25),
        axis.text.x = element_text(size = 22),
        axis.text.y = element_text(size = 22),
        axis.title.y = element_text(size = 24),
        axis.title.x = element_text(size = 24),
        legend.text = element_text(size= 20))

#gains
itirapina_fun %>%
  mutate(tratamento = fct_relevel(tratamento,
                                  "FE", "EAR", "MOD", "LATE")) %>%
  ggplot(aes(x = tratamento, y = gains, fill = invasora)) +
  geom_boxplot(width = 0.4, outlier.shape = NA, alpha = 0.6) +
  labs(y = "Ganhos\n", x = "\nTratamento") +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_discrete(labels = c("Exclusão", "Precoce", "Modal", "Tardia")) +
  scale_fill_manual(values = c("#34cf48", "#cf6d34"),
                    labels = c("M. minutiflora", "U. decumbens")) +
  scale_colour_manual(values = c("#34cf48", "#cf6d34"),
                      guide = F) +
  theme_niwot() +
  theme(legend.position="top", 
        text = element_text(family = "Calibri", size = 25),
        axis.text.x = element_text(size = 22),
        axis.text.y = element_text(size = 22),
        axis.title.y = element_text(size = 24),
        axis.title.x = element_text(size = 24),
        legend.text = element_text(size= 20))

#losses
itirapina_fun %>%
  mutate(tratamento = fct_relevel(tratamento,
                                  "FE", "EAR", "MOD", "LATE")) %>%
  ggplot(aes(x = tratamento, y = losses, fill = invasora)) +
  geom_boxplot(width = 0.4, outlier.shape = NA, alpha = 0.6) +
  labs(y = "Perdas\n", x = "\nTratamento") +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_discrete(labels = c("Exclusão", "Precoce", "Modal", "Tardia")) +
  scale_fill_manual(values = c("#34cf48", "#cf6d34"),
                    labels = c("M. minutiflora", "U. decumbens")) +
  scale_colour_manual(values = c("#34cf48", "#cf6d34"),
                      guide = F) +
  theme_niwot() +
  theme(legend.position="top", 
        text = element_text(family = "Calibri", size = 25),
        axis.text.x = element_text(size = 22),
        axis.text.y = element_text(size = 22),
        axis.title.y = element_text(size = 24),
        axis.title.x = element_text(size = 24),
        legend.text = element_text(size= 20))

#modelos:
modelo.richness <- lm(richness_change ~ tratamento*invasora, 
                      data = itirapina_fun)
summary(modelo.richness) 

modelo.rank <- lm(rank_change ~ tratamento*invasora,
                  data = itirapina_fun)
summary(modelo.rank) 

modelo.gains <- lm(gains ~ tratamento*invasora,
                   data = itirapina_fun)
summary(modelo.gains)

modelo.losses <- lm(losses ~ tratamento*invasora,
                   data = itirapina_fun)
summary(modelo.losses)

####com invasoras:
itirapina_fun_invasora <- read.table("RAC_fun_invasora.txt", h = T,
                            stringsAsFactors = T, sep="\t") #inicial-final
str(itirapina_fun_invasora)
itirapina_fun_invasora$parcela <- as.factor(itirapina_fun_invasora$parcela)
summary(itirapina_fun_invasora)

modelo.richness.inv <- lm(richness_change ~ tratamento*invasora, 
                      data = itirapina_fun_invasora)
summary(modelo.richness.inv) 
anova(modelo.richness.inv)

modelo.evenness.inv <- lm(evenness_change ~ tratamento*invasora, 
                          data = itirapina_fun_invasora)
summary(modelo.evenness.inv) 

modelo.rank.inv <- lm(rank_change ~ tratamento*invasora, 
                          data = itirapina_fun_invasora)
summary(modelo.rank.inv) 

modelo.gains.inv <- lm(gains ~ tratamento*invasora, 
                      data = itirapina_fun_invasora)
summary(modelo.gains.inv) 

modelo.losses.inv <- lm(losses ~ tratamento*invasora, 
                      data = itirapina_fun_invasora)
summary(modelo.losses.inv) 