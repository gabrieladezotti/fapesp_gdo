#FAPESP-2021 - betatemporal
library(tidyverse)
library(gridExtra)
library(extrafont)
library(ggpubr)
library(cowplot)
library(patchwork)

setwd("C:/R/fapesp_gdo")
beta_controle <- read.table("22042021_subplot_controle_IF.txt", h = T,
                         stringsAsFactors = T, sep="\t")
beta_modal <- read.table("26042021_betasubplot_modal_IF.txt", h = T,
                         stringsAsFactors = T, sep="\t")
beta_precoce <- read.table("22042021_betasubplot_precoce_IF.txt", h = T,
                          stringsAsFactors = T, sep="\t")
beta_tardia <- read.table("26042021_betasubplot_tardia_IF.txt", h = T,
                          stringsAsFactors = T, sep="\t")

beta_espacotemp <- rbind(beta_controle, beta_modal, beta_precoce, beta_tardia)
str(beta_espacotemp)
beta_espacotemp$parcela <- as.factor(beta_espacotemp$parcela)
beta_modal$parcela <- as.factor(beta_modal$parcela)
beta_controle$parcela <- as.factor(beta_controle$parcela)
beta_precoce$parcela <- as.factor(beta_precoce$parcela)
beta_tardia$parcela <- as.factor(beta_tardia$parcela)

##CONTROLE
p1 <- beta_controle %>%
  mutate(tempo = fct_reorder(tempo, desc(tempo))) %>%
  ggplot(aes(x = tempo, y = beta.SOR)) +
  ylim(0,1) +
  geom_line(aes(group = parcela)) +
  geom_point(aes(colour = invasora), size = 3) +
  scale_x_discrete(labels = c("t0", "tf")) +
  scale_colour_manual(values = c("#34cf48", "#cf6d34"),
    labels = c("M. minutiflora", "U. brizantha")) +
  labs(x = "", y = "Total Beta-diversity\n", 
       colour = "", title = "Fire Exclusion") +
  theme_test() +
  theme(text = element_text(family = "Calibri", size = 25),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 24),
        axis.title.x = element_text(size = 24),
        legend.text = element_text(size= 24))

##PRECOCE
p2 <- beta_precoce %>%
  mutate(tempo = fct_reorder(tempo, desc(tempo))) %>%
  ggplot(aes(x = tempo, y = beta.SOR)) +
  ylim(0,1) +
  geom_line(aes(group = parcela)) +
  geom_point(aes(colour = invasora), size = 3) +
  scale_x_discrete(labels = c("t0", "tf")) +
  scale_colour_manual(values = c("#34cf48", "#cf6d34"),
                      labels = c("M. minutiflora", "U. brizantha")) +
  labs(x = "", y = "", 
       colour = "", title = "Early-Dry") +
  theme_test() +
  theme(text = element_text(family = "Calibri", size = 25),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 24),
        axis.title.x = element_text(size = 24),
        legend.text = element_text(size= 24))

##TARDIA
p4 <- beta_tardia %>%
      mutate(tempo = fct_reorder(tempo, desc(tempo))) %>%
      ggplot(aes(x = tempo, y = beta.SOR)) +
  ylim(0,1) +
      geom_line(aes(group = parcela)) +
      geom_point(aes(colour = invasora), size = 3) +
      scale_x_discrete(labels = c("t0", "tf")) +
  scale_colour_manual(values = c("#34cf48", "#cf6d34"),
                      labels = c("M. minutiflora", "U. brizantha")) +
      labs(x = "", y = "", 
           colour = "", title = "Late-Dry") +
      theme_test() +
  theme(text = element_text(family = "Calibri", size = 25),
                      axis.text.x = element_text(size = 20),
                      axis.text.y = element_text(size = 20),
                      axis.title.y = element_text(size = 24),
                      axis.title.x = element_text(size = 24),
                      legend.text = element_text(size= 24))

##MODAL
p3 <- beta_modal %>%
  mutate(tempo = fct_reorder(tempo, desc(tempo))) %>%
  ggplot(aes(x = tempo, y = beta.SOR)) +
  ylim(0,1) +
  geom_line(aes(group = parcela)) +
  geom_point(aes(colour = invasora), size = 3) +
  scale_x_discrete(labels = c("t0", "tf")) +
  scale_colour_manual(values = c("#34cf48", "#cf6d34"),
                      labels = c("M. minutiflora", "U. brizantha")) +
  labs(x = "", y = "Total Beta-diversity\n",
       colour = "", title = "Mid-Dry") +
  theme_test() +
  theme(text = element_text(family = "Calibri", size = 25),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 24),
        axis.title.x = element_text(size = 24),
        legend.text = element_text(size= 24))


##GR?FICO CONJUNTO
teste <- p1 +
          theme(legend.text = element_text(size = 26))
ggarrange(teste, p2, p3, p4, ncol = 2, nrow = 2, 
          common.legend = T, legend = "bottom",
          font.label = list(size = 30, face = "bold"))

grid <- plot_grid(
  p1 + theme(legend.position="none"),
  p2 + theme(legend.position="none"), 
  p3 + theme(legend.position="none"),
  p4 + theme(legend.position="none"),
  align = "hv",
  nrow = 2,
  ncol = 2
)

#teste:
lista <- list(p1, p2, p3, p4)

plot <- wrap_plots(lista, ncol = 2, nrow = 2, guides = "collect") & theme(legend.position = "bottom")
###########
#LM
modelo_betasubplot <- lm(beta.SOR ~ tratamento*invasora, 
                          data = beta_espacotemp)

summary(modelo_betasubplot)
anova(modelo_betasubplot)

tapply(beta_espacotemp$beta.SOR, list(beta_espacotemp$tratamento,
                                      beta_espacotemp$invasora), sd)
tapply(beta_espacotemp$beta.SOR, 
       list(beta_espacotemp$tratamento, beta_espacotemp$invasora), quantile)

#lme4 - linear mixed effects
modelo_betasubplot2 <- lmer(beta.SOR ~ tratamento + (tratamento | invasora),
                            data = beta_espacotemp)
summary(modelo_betasubplot2)