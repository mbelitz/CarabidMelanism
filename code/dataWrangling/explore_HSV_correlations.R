# generate correlation figure of HSV values

## plot
library(tidyverse)
library(ggrepel)
library(patchwork)

# species_hsv is the summarised data frame from your script
# (one row per species, with mean_S_*, mean_V_*)

# correlations to annotate
r_d <- cor(species_hsv$mean_S_dorsal,  species_hsv$mean_V_dorsal)
r_v <- cor(species_hsv$mean_S_ventral, species_hsv$mean_V_ventral)

# species to label (the few most-saturated; edit as you like)
lab_d <- c("Oxypselaphus pusillus", "Galerita bicolor")
lab_v <- c("Brachinus alternans", "Harpalus pensylvanicus", "Euryderus grossus")

theme_sv <- theme_classic() +
  theme(axis.text  = element_text(size = 11),
        axis.title = element_text(size = 12),
        plot.tag   = element_text(size = 16, face = "bold"))

p_dorsal <- ggplot(species_hsv, aes(mean_S_dorsal, mean_V_dorsal)) +
  geom_smooth(method = "lm", se = TRUE, color = "grey40",
              fill = "grey80", linewidth = 0.7) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_label_repel(
    data = filter(species_hsv, SpeciesName %in% lab_d),
    aes(label = SpeciesName),
    size = 2.9, fontface = "italic", box.padding = 0.5,
    min.segment.length = 0, max.overlaps = Inf) +
  annotate("text", x = -Inf, y = Inf, hjust = -0.2, vjust = 1.5,
           label = sprintf("r = %.2f", r_d), size = 4.5) +
  labs(x = "Mean saturation (dorsal)",
       y = "Mean lightness (HSV Value, dorsal)", tag = "A") +
  theme_sv

p_ventral <- ggplot(species_hsv, aes(mean_S_ventral, mean_V_ventral)) +
  geom_smooth(method = "lm", se = TRUE, color = "grey40",
              fill = "grey80", linewidth = 0.7) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_label_repel(
    data = filter(species_hsv, SpeciesName %in% lab_v),
    aes(label = SpeciesName),
    size = 2.9, fontface = "italic", box.padding = 0.5,
    min.segment.length = 0, max.overlaps = Inf) +
  annotate("text", x = -Inf, y = Inf, hjust = -0.2, vjust = 1.5,
           label = sprintf("r = %.2f", r_v), size = 4.5) +
  labs(x = "Mean saturation (ventral)",
       y = "Mean lightness (HSV Value, ventral)", tag = "B") +
  theme_sv

fig_S6 <- p_dorsal + p_ventral

ggsave("figureOutputs/FigureS6_saturation_value.png",
       fig_S6, dpi = 450, width = 12, height = 5.3)
