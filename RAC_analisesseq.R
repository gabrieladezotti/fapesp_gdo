#FAPESP - Análises RAC_difference
#Carregando os pacotes necessários:
library(tidyverse)
library(extrafont)
library(ggpubr)

#Arquivo que será utilizado:
itirapina_diff <- read.table("RAC_diff.txt", h = T,
                            stringsAsFactors = T, sep="\t") #seq
str(itirapina_diff)
itirapina_diff$tempo <- as.factor(itirapina_diff$tempo)
summary(itirapina_diff)

#gráficos:
#riqueza
p1 <- itirapina_diff %>%
 filter(invasora == "MM") %>%
 ggplot(aes(x = tempo, y = richness_diff,
            group = interaction(trat, invasora))) +
 geom_point(aes(x = tempo, y = richness_diff,
                colour = trat), size = 2.5) +
 geom_line(aes(colour = trat)) +
 labs(y = "Diferença de Riqueza\n", x = "", title = "M. minutiflora") +
 scale_y_continuous(limits = c(-0.5, 0.5)) +
 theme_niwot() +
 theme(text = element_text(family = "Calibri", size = 14))

p2 <- itirapina_diff %>%
  filter(invasora == "UD") %>%
  ggplot(aes(x = tempo, y = richness_diff,
             group = interaction(trat, invasora))) +
  geom_point(aes(x = tempo, y = richness_diff,
                 colour = trat), size = 2.5) +
  geom_line(aes(colour = trat)) +
  labs(y = "", x = "", title = "U. decumbens") +
  scale_y_continuous(limits = c(-0.5, 0.5)) +
  theme_niwot() +
  theme(text = element_text(family = "Calibri", size = 14))

teste <- p1 + theme(legend.text = element_text(size = 18))
ggarrange(teste, p2, ncol = 2,
          common.legend = T, legend = "bottom",
          font.label = list(size = 30, face = "bold"))

#evenness
p3 <- itirapina_diff %>%
  filter(invasora == "MM") %>%
  ggplot(aes(x = tempo, y = evenness_diff,
             group = interaction(trat, invasora))) +
  geom_point(aes(x = tempo, y = evenness_diff,
                 colour = trat), size = 2.5) +
  geom_line(aes(colour = trat)) +
  labs(y = "Diferença de Equibilidade\n", x = "", title = "M. minutiflora") +
  scale_y_continuous(limits = c(-0.75, 0.5)) +
  theme_niwot() +
  theme(text = element_text(family = "Calibri", size = 14))

p4 <- itirapina_diff %>%
  filter(invasora == "UD") %>%
  ggplot(aes(x = tempo, y = evenness_diff,
             group = interaction(trat, invasora))) +
  geom_point(aes(x = tempo, y = evenness_diff,
                 colour = trat), size = 2.5) +
  geom_line(aes(colour = trat)) +
  labs(y = "", x = "", title = "U. decumbens") +
  scale_y_continuous(limits = c(-0.75, 0.5)) +
  theme_niwot() +
  theme(text = element_text(family = "Calibri", size = 14))

teste1 <- p3 + theme(legend.text = element_text(size = 18))
ggarrange(teste1, p4, ncol = 2,
          common.legend = T, legend = "bottom",
          font.label = list(size = 30, face = "bold"))

#rank
p5 <- itirapina_diff %>%
  filter(invasora == "MM") %>%
  ggplot(aes(x = tempo, y = rank_diff,
             group = interaction(trat, invasora))) +
  geom_point(aes(x = tempo, y = rank_diff,
                 colour = trat), size = 2.5) +
  geom_line(aes(colour = trat)) +
  labs(y = "Diferença de Rank\n", x = "", title = "M. minutiflora") +
  scale_y_continuous(limits = c(0.1, 0.4)) +
  theme_niwot() +
  theme(text = element_text(family = "Calibri", size = 14))

p6 <- itirapina_diff %>%
  filter(invasora == "UD") %>%
  ggplot(aes(x = tempo, y = rank_diff,
             group = interaction(trat, invasora))) +
  geom_point(aes(x = tempo, y = rank_diff,
                 colour = trat), size = 2.5) +
  geom_line(aes(colour = trat)) +
  labs(y = "", x = "", title = "U. decumbens") +
  scale_y_continuous(limits = c(0.1, 0.4)) +
  theme_niwot() +
  theme(text = element_text(family = "Calibri", size = 14))

teste2 <- p5 + theme(legend.text = element_text(size = 18))
ggarrange(teste2, p6, ncol = 2,
          common.legend = T, legend = "bottom",
          font.label = list(size = 30, face = "bold"))

#species
p7 <- itirapina_diff %>%
  filter(invasora == "MM") %>%
  ggplot(aes(x = tempo, y = species_diff,
             group = interaction(trat, invasora))) +
  geom_point(aes(x = tempo, y = species_diff,
                 colour = trat), size = 2.5) +
  geom_line(aes(colour = trat)) +
  labs(y = "Diferença de Espécies\n", x = "", title = "M. minutiflora") +
  scale_y_continuous(limits = c(0, 1)) +
  theme_niwot() +
  theme(text = element_text(family = "Calibri", size = 14))

p8 <- itirapina_diff %>%
  filter(invasora == "UD") %>%
  ggplot(aes(x = tempo, y = species_diff,
             group = interaction(trat, invasora))) +
  geom_point(aes(x = tempo, y = species_diff,
                 colour = trat), size = 2.5) +
  geom_line(aes(colour = trat)) +
  labs(y = "", x = "", title = "U. decumbens") +
  scale_y_continuous(limits = c(0, 1)) +
  theme_niwot() +
  theme(text = element_text(family = "Calibri", size = 14))

teste3 <- p7 + theme(legend.text = element_text(size = 18))
ggarrange(teste3, p8, ncol = 2,
          common.legend = T, legend = "bottom",
          font.label = list(size = 30, face = "bold"))
#modelos:
modelo.richness <- lm(richness_diff ~ trat*invasora, 
                      data = itirapina_diff)
summary(modelo.richness)
anova(modelo.richness)

modelo.evenness <- lm(evenness_diff ~ trat*invasora, 
                      data = itirapina_diff)
summary(modelo.evenness)

modelo.rank <- lm(rank_diff ~ trat*invasora, 
                      data = itirapina_diff)
summary(modelo.rank)

modelo.species <- lm(species_diff ~ trat*invasora, 
                  data = itirapina_diff)
summary(modelo.species)
anova(modelo.richness)
