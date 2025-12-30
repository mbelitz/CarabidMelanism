library(tidyverse)
load("data/neonBeetlePitfallData.rda")

data_beetle <- data_beetle%>% 
  mutate(site = siteID) %>% 
  mutate(year = year(observation_datetime))

## average count per trap per day
siteAbundance <- data_beetle %>% 
  group_by(site, taxon_name, year) %>% 
  summarise(meanRelAbundance = mean(value)) %>% 
  ungroup()

siteAbundance <- siteAbundance %>% 
  group_by(site, taxon_name) %>% 
  summarise(meanRelAbundance = mean(meanRelAbundance)) %>% 
  ungroup()

write.csv(x = siteAbundance, "data/relativeAbundanceBySite.csv", row.names = F)
