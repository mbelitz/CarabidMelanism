library(tidyverse)
library(DHARMa)
library(lme4)
library(lmerTest)
library(MuMIn)
library(sjPlot)

## read in model data frame
mdf <- read.csv("data/melanism_modelDF.csv") %>% 
  mutate(AnnualPrecipitation = scale(AnnualPrecipitation),
         MajorAxis_Dorsal = scale(MajorAxis_Dorsal),
         meanSoilMoisture = scale(meanSoilMoisture),
         beetleAbundance = scale(beetleAbundance),
         meanSoilTemp = scale(meanSoilTemp),
         mammalAbundance = scale(mammalAbundance),
         meanVPD = scale(meanVPD)) %>% 
  mutate(sppSite = paste(SpeciesName,IL_Site,sep = "_"))


## Immune model!!
dorsal_immuneModel <- lmer(formula = ave_melanism_dorsal ~ 
                             AnnualPrecipitation + 
                             meanSoilMoisture +
                             #mammalAbundance + 
                             #beetleAbundance + 
                             #meanVPD   + 
                             MajorAxis_Dorsal + # this is body size
                             #MajorAxis_Dorsal:meanVPD +
                             #MajorAxis_Dorsal:meanSoilMoisture +
                             (1|Tribe/Genus) + (1|IL_Site),
                           data = mdf, 
                           REML = F,
                           na.action = "na.fail") 

summary(dorsal_immuneModel)
sjPlot::tab_model(dorsal_immuneModel, 
          file = "modelResults/dorsal_immuneModel_satterthwaite.doc",
          p.val = "satterthwaite", show.df = TRUE)
r.squaredGLMM(dorsal_immuneModel)

dim <- plot_model(dorsal_immuneModel, type = "est", p.val = "satterthwaite")$data %>% 
  mutate(term = 
           case_when(term == "AnnualPrecipitation" ~"Annual precipitation",
                     term == "meanSoilMoisture" ~ "Soil moisture",
                     term == "MajorAxis_Dorsal" ~ "Body size"))

dorsalImmunePlot <- ggplot() +
  geom_point(dim, mapping = aes(y = term, x = estimate)) +
  geom_errorbar(dim, mapping = aes(y = term, xmin = conf.low, 
                                   xmax = conf.high)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(limits = c(-10,10)) +
  labs(x = "Fixed effect estimate", y = "") +
  theme_classic() +
  ggtitle("Dorsal lightness") +
  theme(plot.title = element_text(hjust = 0.5))

dorsalImmunePlot
#ggsave(filename = "figureOutputs/dorsalImmuneModelResults.png",
#       width = 5, height = 3
#)

## Immune model!!
ventral_immuneModel <- lmer(formula = ave_melanism_ventral ~ 
                              AnnualPrecipitation + 
                              meanSoilMoisture +
                              #mammalAbundance + 
                              #beetleAbundance + 
                              #meanVPD   + 
                              MajorAxis_Dorsal + # this is body size
                              #MajorAxis_Dorsal:meanVPD +
                              #MajorAxis_Dorsal:meanSoilMoisture +
                              (1|Tribe/Genus) + (1|IL_Site),
                            data = mdf, 
                            REML = F,
                            na.action = "na.fail") # problem with beetleAbundance...fix this

summary(ventral_immuneModel)

vim <- plot_model(ventral_immuneModel, type = "est", p.val = "satterthwaite")$data %>% 
  mutate(term = 
           case_when(term == "AnnualPrecipitation" ~"Annual precipitation",
                     term == "meanSoilMoisture" ~ "Soil moisture",
                     term == "MajorAxis_Dorsal" ~ "Body size"))

ventralImmunePlot <- ggplot() +
  geom_point(vim, mapping = aes(y = term, x = estimate)) +
  geom_errorbar(vim, mapping = aes(y = term, xmin = conf.low, 
                                   xmax = conf.high)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(limits = c(-12,12)) +
  labs(x = "Fixed effect estimate", y = "") +
  theme_classic() +
  ggtitle("Ventral lightness") +
  theme(plot.title = element_text(hjust = 0.5))

ventralImmunePlot
#ggsave(filename = "figureOutputs/ventralImmuneModel.png", width = 5, height = 3)

#sjPlot::tab_model(ventral_immuneModel, 
#                  file = "modelResults/ventral_immuneModel_satterthwaite.doc",
#                  p.val = "satterthwaite", show.df = TRUE)

r.squaredGLMM(ventral_immuneModel)

cp <- cowplot::plot_grid(dorsalImmunePlot, ventralImmunePlot,
                         labels = c("A", 
                                    "B"),
                         nrow = 2,
                         ncol = 1)
cp
#ggsave(filename = "figureOutputs/combinedImmuneModel.png", plot = cp,
#       width = 5, height = 6)
