#betatemporal, without MM and UD as species
#FAPESP/2021
library(tidyverse)
library(extrafont)
library(ggpubr)
library(cowplot)

beta_temporal <- read.table("betatemporal_seminvasora.txt", h = T,
                              stringsAsFactors = T, sep="\t")
str(beta_temporal)

#####ggplot

beta_temporal %>%
mutate(tratamento = fct_relevel(tratamento,
       "FE", "EARLY", "MODAL", "LATE")) %>%
  ggplot(aes(x = tratamento, y = beta.SOR, fill = invasora)) +
  geom_point(aes(x = tratamento, y = beta.SOR, colour = invasora)) +
  geom_boxplot(width = 0.4, outlier.shape = NA, alpha = 0.6) +
  labs(y = "Diversidade Beta Total\n", x = "\nTratamento",
       title = "Sem Invasora") +
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
        legend.text = element_text(size= 22))


###statistical analyses
modelo.betatemporal_seminvasora <- lm(beta.SOR ~ tratamento*invasora, 
                          data = beta_temporal)

summary(modelo.betatemporal_seminvasora)
anova(modelo.betatemporal_seminvasora)
tuckey <- TukeyHSD(aov(lm(beta.SOR ~ tratamento*invasora, 
                          data = beta_temporal)))
tuckey

###bar-plot percentage nestedness and turnover in treatments

beta_mean <- data.table::melt(data.frame(
  beta_temporal %>%
    group_by(tratamento, invasora) %>%
    summarise_each(funs(mean))),
  id.vars = c("tratamento", "invasora", "beta.SOR")) %>%
  mutate(tratamento = fct_relevel(tratamento,
                                  "FE", "EARLY", "MODAL", "LATE")) %>%
  mutate(tratamento = fct_recode(tratamento,
                                 Exclusão = "FE", Precoce = "EARLY",
                                 Modal = "MODAL", Tardia = "LATE")) 

beta_mean %>%
  ggplot(aes(x = invasora, y = value/beta.SOR, fill = variable)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_grid(cols = vars(tratamento)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#87CEFA", "#FA8072"), 
                    labels = c("Turnover", "Aninhamento")) +
  labs(y = "", x = "", fill = "", title = "Sem Invasora") +
  theme_bw() +
  theme(legend.position="bottom", 
        text = element_text(family = "Calibri", size = 15),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        legend.text = element_text(size = 20),
        strip.text.x = element_text(size = 20))

#####mean and sd
tapply(beta_multi$beta.SOR, list(beta_multi$tratamento, beta_multi$invasora), sd)
tapply(beta_multi$beta.SOR, 
       list(beta_multi$tratamento, beta_multi$invasora), mean)
