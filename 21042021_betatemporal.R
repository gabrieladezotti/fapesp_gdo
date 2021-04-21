#FAPESP-2021 - betatemporal
library(tidyverse)
library(wesanderson)

setwd("C:/R/fapesp_gdo")
beta_multi <- read.table("21042021_betamulti_parcelas.txt", h = T,
                              stringsAsFactors = T, sep="\t")

str(beta_multi)
beta_multi$parcela <- as.factor(beta_multi$parcela)

#############Gráfico: 

##BETA-SOR
ggplot(beta_multi) +
  geom_boxplot(aes(tratamento, beta.SOR, fill = tratamento),  
               show.legend = FALSE) +
  scale_fill_manual(values = wes_palette("Royal2", n = 4)) +
  labs(x = "Tratamentos", y = "Diversidade Beta Total", 
       title = "") +
  theme_test()

##BETA-NES
ggplot(beta_multi) +
  geom_boxplot(aes(tratamento, beta.SNE, fill = tratamento),  
               show.legend = FALSE) +
  scale_fill_manual(values = wes_palette("Royal2", n = 4)) +
  labs(x = "Tratamentos", y = "Diversidade Beta Total", 
       title = "") +
  theme_test()

##BETA-SIM
ggplot(beta_multi) +
  geom_boxplot(aes(tratamento, beta.SIM, fill = tratamento),  
               show.legend = FALSE) +
  scale_fill_manual(values = wes_palette("Royal2", n = 4)) +
  labs(x = "Tratamentos", y = "Diversidade Beta Total", 
       title = "") +
  theme_test()

###melhorando
pdf("betatemporal.pdf")

ggplot(beta.multi) +
  geom_boxplot(aes(tratamento, beta.SOR, fill = tratamento), 
               show.legend = FALSE,
               width = 0.2, alpha  = 0.7) +
  ylim(0.25, 0.50) +
  labs(x = "Tratamentos", y = "Diversidade Beta Total", 
       title = "") +
  scale_fill_manual(values = c("#FC4E07", "#E7B800", "#30A612")) +
  theme_test(base_size = 12)

dev.off()


##########
modelo.betatemporal <- lm(beta.SOR ~ tratamento*invasora, 
                          data = beta_multi)

summary(modelo.betatemporal)

modelo.betatemporal2 <- lm(beta.SOR ~ tratamento, 
                          data = beta_multi)


aov(modelo.betatemporal)

TukeyHSD(aov(beta.SOR ~ tratamento, 
             data = beta.multi))

modelo.temporal.turn <- lm(beta.SIM ~ tratamento, 
                           data = beta.multi)
summary(modelo.temporal.turn)

modelo.temporal.nest <- lm(beta.SNE ~ tratamento, 
                           data = beta.multi)
summary(modelo.temporal.nest)


########################
tapply(beta.multi$beta.SOR, beta.multi$tratamento, sd)
tapply(beta.multi$beta.SOR, beta.multi$tratamento, quantile)
