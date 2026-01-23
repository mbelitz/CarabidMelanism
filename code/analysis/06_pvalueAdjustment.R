library(tidyverse)
library(DHARMa)
library(lme4)
library(lmerTest)
library(MuMIn)
library(sjPlot)
library(broom.mixed)

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

## desiccation hypothesis model
# Dorsal lightness
dorsal_desiccationModel <- lmer(formula = ave_melanism_dorsal ~ 
                                  AnnualPrecipitation + 
                                  meanSoilMoisture +
                                  #mammalAbundance + 
                                  #beetleAbundance + 
                                  meanVPD   + 
                                  MajorAxis_Dorsal + # this is body size
                                  MajorAxis_Dorsal:meanVPD +
                                  MajorAxis_Dorsal:meanSoilMoisture +
                                  (1|Tribe/Genus) + (1|IL_Site) ,
                                data = mdf, 
                                REML = F,
                                na.action = "na.fail") # problem with beetleAbundance...fix this

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

ventral_desiccationModel <- lmer(formula = ave_melanism_ventral ~ 
                                   AnnualPrecipitation + 
                                   meanSoilMoisture +
                                   #mammalAbundance + 
                                   #beetleAbundance + 
                                   meanVPD   + 
                                   MajorAxis_Dorsal + # this is body size
                                   MajorAxis_Dorsal:meanVPD +
                                   MajorAxis_Dorsal:meanSoilMoisture +
                                   (1|Tribe/Genus) + (1|IL_Site),
                                 data = mdf, 
                                 REML = F,
                                 na.action = "na.fail") # problem with beetleAbundance...fix this

summary(ventral_desiccationModel)

#### STRUCTURAL MODEL 
#### structural biotic interactions
dorsal_structuralModel <- lmer(formula = ave_melanism_dorsal ~ 
                                 #AnnualPrecipitation + 
                                 #meanSoilMoisture +
                                 mammalAbundance + 
                                 #meanVPD   + 
                                 MajorAxis_Dorsal + # this is body size
                                 MajorAxis_Dorsal:mammalAbundance +
                                 (1|Tribe/Genus) + (1|IL_Site),
                               data = mdf, 
                               REML = F,
                               na.action = "na.fail") # problem with beetleAbundance...fix this

summary(dorsal_structuralModel)

### adjust p-values post hoc across the five models 
mods <- list(
  dorsal_immune        = dorsal_immuneModel,
  ventral_immune       = ventral_immuneModel,
  dorsal_desiccation   = dorsal_desiccationModel,
  ventral_desiccation  = ventral_desiccationModel,
  dorsal_structural    = dorsal_structuralModel
)


pvals <- map_dfr(
  mods,
  ~ tidy(.x, effects = "fixed") %>%
    select(term, p.value),
  .id = "model"
)


pvals_adj <- pvals %>%
  group_by(term) %>%     
  mutate(p_adj = p.adjust(p.value, method = "holm")) %>%
  ungroup()
