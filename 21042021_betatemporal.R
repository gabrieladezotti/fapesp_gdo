#FAPESP-2021 - betatemporal
library(tidyverse)
library(gridExtra)
library(extrafont)
library(ggpubr)
library(lme4)
library(Hmisc)
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")

setwd("C:/R/fapesp_gdo")
beta_multi <- read.table("21042021_betamulti_parcelas.txt", h = T,
                              stringsAsFactors = T, sep="\t")

str(beta_multi)
beta_multi$parcela <- as.factor(beta_multi$parcela)

#############Gr?fico: 

##BETA-SOR
#s? boxplot
beta_multi  %>%
  mutate(tratamento = fct_relevel(tratamento,
                                  "FE", "EAR", "MOD", "LATE")) %>%
  ggplot(aes(x = tratamento, y = beta.SOR, fill = invasora)) +
  geom_point(aes(x = tratamento, y = beta.SOR, colour = invasora), size = 3) +
  geom_boxplot(width = 0.4, outlier.shape = NA, alpha = 0.8) +
  labs(y = "Total Beta-diversity\n", x = "\nTreatment") +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_discrete(labels = c("Fire Exclusion", "Early-Dry", "Mid-Dry", "Late-Dry")) +
  scale_fill_manual(values = c("#34cf48", "#cf6d34"),
                    labels = c("M. minutiflora", "U. brizantha")) +
  scale_colour_manual(values = c("#34cf48", "#cf6d34"),
                      guide = F) +
  theme_niwot() +
  theme(legend.position="top", 
        text = element_text(family = "Calibri", size = 25),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24),
        axis.title.y = element_text(size = 28),
        axis.title.x = element_text(size = 28),
        legend.text = element_text(size= 22))

###bar_plot
#mean
beta_mean <- data.table::melt(data.frame(
  beta_multi[,-1] %>%
    group_by(tratamento, invasora) %>%
    summarise_each(funs(mean))),
  id.vars = c("tratamento", "invasora", "beta.SOR"))

teste <- beta_mean %>%
  mutate(tratamento = fct_relevel(tratamento,
  "FE", "EAR", "MOD", "LATE")) %>%
  mutate(tratamento = fct_recode(tratamento,
  Exclusão = "FE", Precoce = "EAR",
  Modal = "MOD", Tardia = "LATE")) 

teste %>%
  ggplot(aes(x = invasora, y = value/beta.SOR, fill = variable)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_grid(cols = vars(tratamento)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#87CEFA", "#FA8072"), 
                    labels = c("Turnover", "Aninhamento")) +
  labs(y = "", x = "", fill = "") +
  theme_bw() +
  theme(legend.position="bottom", 
        text = element_text(family = "Calibri", size = 15),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        legend.text = element_text(size = 20),
        strip.text.x = element_text(size = 20))

##########
##BETA-TOTAL
modelo.betatemporal <- lm(beta.SOR ~ tratamento*invasora, 
                          data = beta_multi)

summary(modelo.betatemporal)
anova(modelo.betatemporal)

tuckey <- TukeyHSD(aov(lm(beta.SOR ~ tratamento*invasora, 
             data = beta_multi)))

tuckey
#MOD:MM-FE:MM 0.0212477
#MOD:UD-MOD:MM 0.0022200

write.csv2(tuckey$`tratamento:invasora`, file = "tuckeyteste.csv", sep = "\t",
           row.names = T)

modelo.temporal.turn <- lm(beta.SIM ~ tratamento, 
                           data = beta_multi)
summary(modelo.temporal.turn)

modelo.temporal.nest <- lm(beta.SNE ~ tratamento, 
                           data = beta_multi)
summary(modelo.temporal.nest)

########################
tapply(beta_multi$beta.SOR, list(beta_multi$tratamento, beta_multi$invasora), sd)
tapply(beta_multi$beta.SOR, 
       list(beta_multi$tratamento, beta_multi$invasora), mean)
