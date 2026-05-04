# =============================================================================
# HSV SENSITIVITY ANALYSIS
# Tests whether RGB-derived lightness is a reliable proxy for melanin by
# examining: (1) correlation between mean RGB and HSV Value (brightness),
# and (2) distribution of HSV Saturation to flag structurally-coloured specimens
# High saturation indicates structural/iridescent colour rather than pigment
# =============================================================================

library(tidyverse)

# --- RGB to HSV conversion ---
# Input: R, G, B on 0-255 scale
# Output: H (0-360 degrees), S (0-1), V (0-1)
rgb_to_hsv_components <- function(R, G, B) {
  r <- R / 255
  g <- G / 255
  b <- B / 255
  
  M     <- pmax(r, g, b)
  m     <- pmin(r, g, b)
  delta <- M - m
  
  # Value
  V <- M
  
  # Saturation
  S <- ifelse(M > 0, delta / M, 0)
  
  # Hue
  H <- case_when(
    delta == 0 ~ 0,
    M == r     ~ (60 * ((g - b) / delta)) %% 360,
    M == g     ~ 60 * ((b - r) / delta) + 120,
    M == b     ~ 60 * ((r - g) / delta) + 240,
    TRUE       ~ 0
  )
  
  tibble(H = H, S = S, V = V)
}

# --- Compute HSV for dorsal and ventral surfaces ---
melanism <- read.csv("data/melanism.csv")  
# (use your raw melanism df that has separate channel columns)

melanism_hsv <- melanism %>%
  mutate(
    # Dorsal HSV
    dorsal_hsv  = pmap(
      list(mean_Red_Dorsal, mean_Green_Dorsal, mean_Blue_Dorsal),
      ~ rgb_to_hsv_components(..1, ..2, ..3)
    ),
    HSV_H_dorsal = map_dbl(dorsal_hsv, "H"),
    HSV_S_dorsal = map_dbl(dorsal_hsv, "S"),
    HSV_V_dorsal = map_dbl(dorsal_hsv, "V") * 255,  # rescale to 0-255 for comparability
    
    # Ventral HSV
    ventral_hsv  = pmap(
      list(mean_Red_Ventral, mean_Green_Ventral, mean_Blue_Ventral),
      ~ rgb_to_hsv_components(..1, ..2, ..3)
    ),
    HSV_H_ventral = map_dbl(ventral_hsv, "H"),
    HSV_S_ventral = map_dbl(ventral_hsv, "S"),
    HSV_V_ventral = map_dbl(ventral_hsv, "V") * 255
  ) %>%
  select(-dorsal_hsv, -ventral_hsv)

melanism_hsv <- select(melanism_hsv, ave_melanism_dorsal, ave_melanism_ventral, 
                       siteName, IL_Site, Tribe, Genus, SpeciesName,
                       HSV_H_dorsal, HSV_S_dorsal, HSV_V_dorsal,
                       HSV_H_ventral, HSV_S_ventral, HSV_V_ventral)

write.csv(x = melanism_hsv, 'data/melanism_HSV.csv')

## plot
library(ggrepel)

p_S_V <- ggplot(species_hsv, aes(x = mean_S_dorsal, y = mean_V_dorsal)) +
  geom_point(size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, color = "grey40", 
              linewidth = 0.7) +
  ggrepel::geom_label_repel(
    data = filter(species_hsv, mean_S_dorsal > 0.3),
    aes(label = SpeciesName),
    size = 2.8, fontface = "italic",
    box.padding = 0.4, max.overlaps = 20) +
  labs(x = "Mean Saturation (dorsal)", 
       y = "Mean Value / Brightness (dorsal)") +
  theme_classic() +
  theme(axis.text       = element_text(size = 11),
        axis.title      = element_text(size = 12),
        plot.title      = element_text(hjust = 0.5, size = 13),
        plot.subtitle   = element_text(hjust = 0.5, size = 10,
                                       color = "grey40"),
        legend.position = c(0.15, 0.90))

ggsave("figureOutputs/dorsal_S_vs_V_labelled.png",
       p_S_V, dpi = 450, width = 9, height = 7)

###

p_S_V_ventral <- ggplot(species_hsv, aes(x = mean_S_ventral, y = mean_V_ventral)) +
  geom_point(size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, color = "grey40", 
              linewidth = 0.7) +
  ggrepel::geom_label_repel(
    data = filter(species_hsv, mean_S_ventral > 0.38),
    aes(label = SpeciesName),
    size = 2.8, fontface = "italic",
    box.padding = 0.4, max.overlaps = 20) +
  ggrepel::geom_label_repel(
    data = filter(species_hsv, mean_S_ventral > 0.3 & mean_V_ventral<70),
    aes(label = SpeciesName),
    size = 2.8, fontface = "italic",
    box.padding = 0.4, max.overlaps = 20) +
  labs(x = "Mean Saturation (dorsal)", 
       y = "Mean Value / Brightness (dorsal)") +
  theme_classic() +
  theme(axis.text       = element_text(size = 11),
        axis.title      = element_text(size = 12),
        plot.title      = element_text(hjust = 0.5, size = 13),
        plot.subtitle   = element_text(hjust = 0.5, size = 10,
                                       color = "grey40"),
        legend.position = c(0.15, 0.90))

p_S_V_ventral

ggsave("figureOutputs/ventral_S_vs_V_labelled.png",
       p_S_V, dpi = 450, width = 9, height = 7)
