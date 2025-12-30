library(tidyverse)
library(sf)
library(rnaturalearth)

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

## states_sf
proj_proj <- "+proj=aea +lon_0=-97.03125 +lat_1=12.8085481 +lat_2=47.182968 +lat_0=29.9957581 +datum=WGS84 +units=m +no_defs"
states_sf <- ne_states(country = "United States of America", returnclass = "sf") %>% 
  filter(!postal %in% c("AK","HI","PR")) %>% 
  st_transform(proj_proj)

mdf_sf <- st_as_sf(mdf, coords = c("longitude", "latitude"),
                   crs = "WGS84") %>% 
  st_transform(crs = proj_proj)

## unique sites # how many individuals and species per site
sites <- mdf_sf %>% 
  group_by(IL_Site) %>% 
  summarise(Individuals = n(),
            Species = length(unique(SpeciesName)))

ggplot() +
  geom_sf(states_sf, mapping = aes(), fill = NA) +
  geom_sf(sites, mapping = aes(color = Species,
                               size = Individuals),
          alpha = 0.5,
          shape = 19) +
  scale_color_viridis_c(option = "inferno") + 
  theme_classic()

ggsave(filename = "figureOutputs/studyMap.png", 
       dpi = 450, width = 6, height = 4)
