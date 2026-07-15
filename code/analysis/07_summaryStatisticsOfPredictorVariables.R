library(tidyverse)

mdf <- read.csv("data/melanism_modelDF.csv") %>% 
 #mutate(AnnualPrecipitation = scale(AnnualPrecipitation),
 #       MajorAxis_Dorsal = scale(MajorAxis_Dorsal),
 #       meanSoilMoisture = scale(meanSoilMoisture),
 #       beetleAbundance = scale(beetleAbundance),
 #       meanSoilTemp = scale(meanSoilTemp),
 #       mammalAbundance = scale(mammalAbundance),
 #       meanVPD = scale(meanVPD)) %>% 
  mutate(sppSite = paste(SpeciesName,IL_Site,sep = "_"))

## summary table of number of individual beetle photos per species
speciesIndividuals <- mdf %>% 
  group_by(SpeciesName) %>% 
  summarise(numberOfIndividuals = n())

write.csv(x = speciesIndividuals, file = "table/individualsPerSpecies.csv")

##summary table of predictor variable range 
# Environmental predictors: one row per site (values are site-constant)
site_env <- mdf %>%
  distinct(IL_Site, .keep_all = TRUE) %>%
  select(IL_Site, AnnualPrecipitation, meanSoilMoisture, meanVPD, mammalAbundance) %>%
  pivot_longer(-IL_Site, names_to = "predictor", values_to = "value") %>%
  summarise(mean = mean(value), min = min(value), max = max(value),
            sd = sd(value), n = n(), .by = predictor)   # n should be 17

# Body size: specimen-level trait, summarise across individuals
body <- mdf %>%
  summarise(predictor = "MajorAxis_Dorsal",
            mean = mean(MajorAxis_Dorsal), min = min(MajorAxis_Dorsal),
            max  = max(MajorAxis_Dorsal), sd = sd(MajorAxis_Dorsal), n = n())

summary_table <- bind_rows(site_env, body)

write.csv(x = summary_table, file = "table/predictorVariablesSummary.csv", row.names = F)
