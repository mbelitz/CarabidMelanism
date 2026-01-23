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

summary_table <- mdf %>%
  select(
    AnnualPrecipitation,
    meanSoilMoisture,
    MajorAxis_Dorsal,
    meanVPD,
    mammalAbundance
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "predictor",
    values_to = "value"
  ) %>%
  summarise(
    mean  = mean(value, na.rm = TRUE),
    min   = min(value, na.rm = TRUE),
    max   = max(value, na.rm = TRUE),
    sd    = sd(value, na.rm = TRUE),
    .by = predictor
  )

summary_table

write.csv(x = summary_table, file = "table/predictorVariablesSummary.csv", row.names = F)
