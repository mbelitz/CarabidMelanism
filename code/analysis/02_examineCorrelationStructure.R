library(tidyverse)
library(cowplot)
library(lme4)
library(lmerTest)
library(MuMIn)
library(sjPlot)

## read in melanism data
mel<-read.csv("data/melanism_modelDF.csv", header=TRUE) # data from Katie
mel <- filter(mel, Habit == "nocturnal")

## read in covariates
meanSoilMoisture <- read.csv("data/covariatesOutputs/meanSoilMoisture.csv") %>% 
  distinct(siteName, longitude, latitude, .keep_all = TRUE)
beetleAbundance <- read.csv("data/covariatesOutputs/beetlecounts.csv") %>% 
  select(-X) %>% 
  rename(beetleAbundance = abundance)
soilTempSummer <- read.csv("data/covariatesOutputs/monthlySoilMeansAll_AtNEONSites.csv") %>%
  filter(month %in% c(6,7,8)) %>% 
  group_by(site) %>% 
  summarise(meanSummerSoilTemp = mean(montlyMeanTemp)) %>% 
  ungroup()
soilTemp <- read.csv("data/covariatesOutputs/monthlySoilMeansAll_AtNEONSites.csv") %>%
  group_by(site) %>% 
  summarise(meanSoilTemp = mean(montlyMeanTemp)) %>% 
  ungroup()
soilTemp2 <- read.csv("data/covariatesOutputs/monthlySoilMeansAll_AtNEONSites.csv") %>%
  filter(year != 2025) %>% 
  group_by(site) %>% 
  summarise(meanSoilTemp = mean(montlyMeanTemp)) %>% 
  ungroup()
cor(soilTemp$meanSoilTemp, soilTempSummer$meanSummerSoilTemp)
smallMammal <- read.csv("data/covariatesOutputs/smallmammalcounts.csv") %>% 
  rename(mammalAbundance = abundance)
vpd <- read.csv("data/covariatesOutputs/NEON_climate - CHELSA_VPD_1981-2010_by_month.csv") %>%
  rename(vpd1 = 2, vpd2 = 3, vpd3 = 4,
         vpd4 = 5, vpd5 = 6, vpd6 = 7,
         vpd7 = 8, vpd8 = 9, vpd9 = 10,
         vpd10 = 11, vpd11 = 12, vpd12 = 13) %>% 
  rowwise %>% 
  mutate(meanVPD = mean(c(vpd1, vpd2, vpd3, vpd4, vpd5, vpd6,
                          vpd7, vpd8, vpd9, vpd10, vpd11, vpd12))) %>% 
  select(siteCode, meanVPD)
rsds <- read.csv("data/covariatesOutputs/rsds_chelsa.csv") %>% 
  rename(rsds = 1)

### combine with our mel data
mel <- mel %>% 
  select(ave_melanism_dorsal, ave_melanism_ventral,
         MAT,AnnualPrecipitation,MajorAxis_Dorsal,
         siteName, IL_Site, Tribe, Genus, SpeciesName)

mel2 <- left_join(mel, meanSoilMoisture)

## next join with beetle abundance
mel3 <- left_join(mel2, beetleAbundance, 
                  by = c("IL_Site" = "siteID"))

## join with mean soil temperature
mel4 <- left_join(mel3, soilTemp, 
                  by = c("IL_Site" = "site"))

## join with small mammals
mel5 <- left_join(mel4, smallMammal, 
                  by = c("IL_Site" = "siteID"))

mel5 <- select(mel5, -X)

# join with vpd
mel6 <- left_join(mel5, vpd, by = c("IL_Site" = "siteCode")) 

# join with rsds
mel6 <- left_join(mel6, rsds,by = c("IL_Site" = "siteCode")) 

## write model datafreame
#write.csv(x = mel6, file = "data/melanism_modelDF.csv", row.names = F)

### generate correlation matrix
m <- select(mel6, AnnualPrecipitation, MajorAxis_Dorsal,
            beetleAbundance, meanSoilTemp, mammalAbundance, meanSoilMoisture, 
            meanVPD, rsds, MAT) 
m <- m %>% 
  rename(BodySize = MajorAxis_Dorsal,
         #BeetleAbundance = beetleAbundance,
         SoilTemp = meanSoilTemp,
         MammalAbundance = mammalAbundance,
         SoilMoisture = meanSoilMoisture,
         VPD = meanVPD,
         #SolarRadiation = rsds,
         #AnnualTemp = MAT
         ) %>% 
  select(-rsds,-MAT,-beetleAbundance)

library(corrplot)
M = cor(m)
corrplot(M, method = 'number') # colorful number

# generate plot
png(filename = "figureOutputs/CorrelationMatrix4.png", 
    width=3600, height = 3600, res = 300)
  corrplot(M, 
           method = "shade", 
           type = "lower", 
           title = "", 
           mar = c(2,2,2,2), 
           number.cex = 1.3, 
           number.digits = 2,
           tl.cex = 1.2,
           tl.col = "black",
           addCoef.col = 'grey60');
dev.off()

## what's going on b/w meanVPD and meanSoilTemp??
library(ggrepel)
a <- ggplot(distinct(mel6, IL_Site, .keep_all = TRUE),
       mapping = aes(x = meanSoilTemp, y = meanVPD)) +
  geom_point() +
  geom_text_repel(aes(label = IL_Site)) +
  theme_classic()

## do this for mean temperature and meanSoilTemp
m2 <- select(mel6, AnnualPrecipitation, MajorAxis_Dorsal, MAT, IL_Site,
            meanRelAbundance, meanSoilTemp, abundance, meanSoilMoisture, meanVPD) %>% 
  rename(beetleAbundance = meanRelAbundance,
         mammalAbundance = abundance)

b <- ggplot(distinct(m2, IL_Site, .keep_all = TRUE),
       mapping = aes(x = meanSoilTemp, y = MAT)) +
  geom_point() +
  geom_text_repel(aes(label = IL_Site)) +
  theme_classic()

cp <- cowplot::plot_grid(a, b)

ggsave(filename = "figureOutputs/VPD_Correlation.png", width = 8, height = 4)
