# =============================================================================
# SENSITIVITY ANALYSIS: within-species variation in lightness
# Tests whether the 14 multi-site species show lightness variation across
# sites consistent with the desiccation hypothesis (darker at drier sites)
# =============================================================================

library(tidyverse)
library(lme4)
library(lmerTest)

# Helper: reduce  subspecies names to binomial species names
strip_subspecies <- function(x) {
  words <- str_split(x, " ")
  map_chr(words, ~ if (length(.x) >= 3) paste(.x[1], .x[2]) else paste(.x, collapse = " "))
}

# Reload unscaled data so we can compute species means on original scale
mdf_raw <- read.csv("data/melanism_modelDF.csv") %>%
  mutate(SpeciesName_match = strip_subspecies(SpeciesName))

# --- Identify multi-site species ---
species_site_counts <- mdf_raw %>%
  group_by(SpeciesName_match) %>%
  summarise(n_sites = n_distinct(IL_Site), .groups = "drop") %>%
  arrange(desc(n_sites))

multi_site_species <- species_site_counts %>%
  filter(n_sites >= 2) %>%
  pull(SpeciesName_match)

cat(sprintf("\n%d species sampled at 2+ sites:\n", length(multi_site_species)))
print(species_site_counts %>% filter(n_sites >= 2))

# --- Approach 1: site-level means per species, then within-species correlations ---

# Compute mean lightness and environment per species-site combination
spp_site_means <- mdf_raw %>%
  filter(SpeciesName_match %in% multi_site_species) %>%
  group_by(SpeciesName_match, IL_Site) %>%
  summarise(
    mean_dorsal_lightness  = mean(ave_melanism_dorsal,  na.rm = TRUE),
    mean_ventral_lightness = mean(ave_melanism_ventral, na.rm = TRUE),
    mean_VPD               = mean(meanVPD,              na.rm = TRUE),
    mean_soil_moisture     = mean(meanSoilMoisture,     na.rm = TRUE),
    n_individuals          = n(),
    .groups = "drop"
  )

# Within-species Spearman correlations between lightness and moisture variables
within_species_cors <- spp_site_means %>%
  group_by(SpeciesName_match) %>%
  summarise(
    n_sites         = n(),
    # VPD: desiccation predicts NEGATIVE correlation (drier = higher VPD = darker = lower lightness)
    cor_VPD_dorsal  = cor(mean_VPD, mean_dorsal_lightness,  method = "spearman"),
    cor_VPD_ventral = cor(mean_VPD, mean_ventral_lightness, method = "spearman"),
    # Soil moisture: desiccation predicts POSITIVE correlation (wetter = higher moisture = lighter)
    cor_SM_dorsal   = cor(mean_soil_moisture, mean_dorsal_lightness,  method = "spearman"),
    cor_SM_ventral  = cor(mean_soil_moisture, mean_ventral_lightness, method = "spearman"),
    .groups = "drop"
  ) %>%
  arrange(desc(n_sites))

cat("\n--- Within-species correlations (Spearman) ---\n")
cat("Desiccation hypothesis predicts: cor_VPD < 0, cor_SM > 0\n\n")
print(within_species_cors)

# Summarise: what proportion of species show the predicted direction?
direction_summary <- within_species_cors %>%
  summarise(
    n_species                     = n(),
    pct_VPD_dorsal_correct_dir    = mean(cor_VPD_dorsal  < 0, na.rm = TRUE) * 100,
    pct_VPD_ventral_correct_dir   = mean(cor_VPD_ventral < 0, na.rm = TRUE) * 100,
    pct_SM_dorsal_correct_dir     = mean(cor_SM_dorsal   > 0, na.rm = TRUE) * 100,
    pct_SM_ventral_correct_dir    = mean(cor_SM_ventral  > 0, na.rm = TRUE) * 100,
    median_cor_VPD_dorsal         = median(cor_VPD_dorsal,  na.rm = TRUE),
    median_cor_VPD_ventral        = median(cor_VPD_ventral, na.rm = TRUE),
    median_cor_SM_dorsal          = median(cor_SM_dorsal,   na.rm = TRUE),
    median_cor_SM_ventral         = median(cor_SM_ventral,  na.rm = TRUE)
  )

cat("\n--- Direction summary ---\n")
print(direction_summary)

# Sign test: are the within-species correlations biased toward the predicted direction?
# Under the null (no within-species effect), ~50% should show each direction
sign_test <- function(cors, predicted_positive = TRUE) {
  cors_clean <- cors[!is.na(cors)]
  n          <- length(cors_clean)
  n_correct  <- if (predicted_positive) sum(cors_clean > 0) else sum(cors_clean < 0)
  # Exact binomial test against p = 0.5
  binom.test(n_correct, n, p = 0.5, alternative = "greater")
}

cat("\n--- Sign tests (binomial, H1: more species show predicted direction than chance) ---\n")
cat("VPD ~ dorsal lightness (predicted negative):\n")
print(sign_test(within_species_cors$cor_VPD_dorsal, predicted_positive = FALSE))

cat("VPD ~ ventral lightness (predicted negative):\n")
print(sign_test(within_species_cors$cor_VPD_ventral, predicted_positive = FALSE))

cat("Soil moisture ~ dorsal lightness (predicted positive):\n")
print(sign_test(within_species_cors$cor_SM_dorsal, predicted_positive = TRUE))

cat("Soil moisture ~ ventral lightness (predicted positive):\n")
print(sign_test(within_species_cors$cor_SM_ventral, predicted_positive = TRUE))

# --- Visualisation: dot plot of within-species correlations ---
cors_long <- within_species_cors %>%
  select(SpeciesName_match, n_sites,
         `VPD ~ dorsal`    = cor_VPD_dorsal,
         `VPD ~ ventral`   = cor_VPD_ventral,
         `Moisture ~ dorsal`  = cor_SM_dorsal,
         `Moisture ~ ventral` = cor_SM_ventral) %>%
  pivot_longer(-c(SpeciesName_match, n_sites),
               names_to = "comparison", values_to = "correlation") %>%
  mutate(
    predicted_direction = if_else(str_detect(comparison, "VPD"), "negative", "positive"),
    consistent          = case_when(
      predicted_direction == "negative" & correlation < 0 ~ "Consistent",
      predicted_direction == "positive" & correlation > 0 ~ "Consistent",
      TRUE                                                 ~ "Inconsistent"
    ),
    SpeciesName_match = fct_reorder(SpeciesName_match, n_sites)
  )

p_within_cors <- ggplot(cors_long,
                        aes(x = correlation, y = SpeciesName_match,
                            color = consistent, shape = consistent)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("Consistent"   = "#2c7bb6",
                                "Inconsistent" = "#d7191c")) +
  scale_shape_manual(values = c("Consistent" = 16, "Inconsistent" = 1)) +
  facet_wrap(~ comparison, nrow = 2) +
  labs(x = "Spearman correlation", y = "",
       color = "Direction", shape = "Direction") +
  theme_classic() +
  theme(axis.text.y  = element_text(size = 9, face = "italic"),
        axis.text.x  = element_text(size = 10),
        strip.text   = element_text(size = 11),
        legend.position = "bottom")

ggsave("figureOutputs/within_species_sensitivity.png",
       p_within_cors, dpi = 450, width = 9, height = 7)

# Save full results table for supplement
write_csv(within_species_cors,
          "modelResults/within_species_correlations_supplement.csv")