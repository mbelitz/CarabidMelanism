# Example script of how to download and calculate daily mean temps at sites.
# not fully reproducible, but the concept is there to adapt for your own use

# must first download isric soils database data 

library(terra)
library(sf)

# soil data
soil1 <- rast("SoilDownload/wv0010_0-5cm_mean.tif")
soil2 <- rast("SoilDownload/wv0010_5-15cm_mean.tif")
soil3 <- rast("SoilDownload/wv0010_15-30cm_mean.tif")

soils_combined <- c(soil1,soil2,soil3)

## extract values per site
mel<-read.csv("data/melanism.csv", header=TRUE) # data from Katie

# mel, longitude, latitude
mel_ll <- select(mel, longitude, latitude) %>% 
  st_as_sf(coords = c("longitude", "latitude"),
           crs = "WGS84") %>% 
  vect()

e <- extract(soils_combined, mel_ll) 

soil_at_coords_df <- e %>% 
  rowwise %>% 
  mutate(meanSoilMoisture = mean(c(`wv0010_0-5cm_mean`,
                                 `wv0010_5-15cm_mean`)))

### soil moisture df
soilMoistureDF <- mel %>% 
  mutate(meanSoilMoisture = soil_at_coords_df$meanSoilMoisture) %>% 
  select(siteName, longitude, latitude, meanSoilMoisture)

write.csv(x = soilMoistureDF, 
          file = "covariatesOutputs/meanSoilMoisture.csv", 
          row.names = F)

