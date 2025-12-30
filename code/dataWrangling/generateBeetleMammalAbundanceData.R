# Example script of how to download and calculate daily mean temps at sites.
# not fully reproducible, but the concept is there to adapt for your own use
# Read in NEON small mammal data
library(lubridate)
library(neonDivData)
library(tidyverse)
mammals <- neonDivData::data_small_mammal
mammals <- mammals %>% 
  mutate(year = isoyear(observation_datetime))

# Read in Elton Traits
traits <- read.table("C:/Users/sydne.record/Dropbox/NEON_RCN/MamFuncDat.txt", stringsAsFactors=FALSE, fill=TRUE, header=TRUE)
traits <- tibble(traits)
traits <- traits %>%
  mutate(taxon_name = str_c(Scientific, MSWFamilyLatin, sep=' '))

# Sites in Katie Marshall's data from 2016
sites <- c("WOOD", "DELA", "UKFS", "NIWO", "TREE", "UNDE", "SERC", "BLAN", "SCBI", "NOGP", "CPER", "OSBS", "ORNL", "KONZ", "OAES", "MOAB", "STEI")

# Pull mammals by 2016 sampling year and sites of interest
mammals_subset <- filter(mammals, year == "2016" & siteID %in% sites)

# Determine what unique small mammal species are in mammal_subset to match with Elton Traits forgaging characteristics
mammalsp <- unique(mammals_subset$taxon_name)

# Subset Elton Traits to those species of interest at NEON sites
trait_subset <- filter(traits, taxon_name %in% mammalsp)

# Determine which mammals are insectivores. Note Diet.Vend is Diet.Inv because row numbers were added in filtering.
insectivore_traits <- filter(trait_subset, Diet.Vend >0)

# Create list of species that are insectivores
insectivoresp <- insectivore_traits$taxon_name

# Create mammal counts by year and site
insectivore_counts <- filter(mammals_subset, taxon_name %in% insectivoresp)

insectivore_counts_siteyr <- insectivore_counts %>%
  group_by(siteID) %>%
  summarise(abundance=sum(value))

# Determine which species were not in Elton Traits
spxElton <- setdiff(mammalsp, trait_subset$taxon_name)

# What percentage of the NEON small mammal data counts were from species without
# foraging trait information?
spxElton_subset <- filter(mammals_subset, taxon_name %in% spxElton)
spxElton_counts <- spxElton_subset %>%
  summarise(abundance=sum(value))
total_counts <- mammals_subset %>%
  summarise(abundance=sum(value))
spxElton_counts/total_counts

# Pull beetle data for sites of interest for 2016
beetles <- neonDivData::data_beetle
beetles <- beetles %>% 
  mutate(year = isoyear(observation_datetime))
beetles_subset <- filter(beetles, year == "2016" & siteID %in% sites)

# Generate counts of beetle_subset of total beetles
beetle_counts_siteyr <- beetles_subset %>%
  group_by(siteID) %>%
  summarise(abundance=sum(value))

write.csv(beetle_counts_siteyr, "C:/Users/sydne.record/Dropbox/NEON_RCN/beetlecounts.csv")
write.csv(insectivore_counts_siteyr, "C:/Users/sydne.record/Dropbox/NEON_RCN/smallmammalcounts.csv")
