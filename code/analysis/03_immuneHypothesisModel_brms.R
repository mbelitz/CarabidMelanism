library(tidyverse)
library(lme4)
library(lmerTest)
library(ape)
library(phytools)
library(brms)
library(tidybayes)
library(cowplot)

# =============================================================================
# DATA & TREE PREPARATION
# (identical to desiccation script - source this section once if running
#  all hypothesis scripts in the same session)
# =============================================================================

mdf <- read.csv("data/melanism_modelDF.csv") %>%
  mutate(
    AnnualPrecipitation = scale(AnnualPrecipitation),
    MajorAxis_Dorsal    = scale(MajorAxis_Dorsal),
    meanSoilMoisture    = scale(meanSoilMoisture),
    beetleAbundance     = scale(beetleAbundance),
    meanSoilTemp        = scale(meanSoilTemp),
    mammalAbundance     = scale(mammalAbundance),
    meanVPD             = scale(meanVPD)
  ) %>%
  mutate(sppSite = paste(SpeciesName, IL_Site, sep = "_"))

# join with HSV values
hsv <- read.csv("data/melanism_HSV.csv")

mdf <- left_join(mdf, hsv)

# read in phylogeny
tree <- read.tree("data/Tree.DNA.droppedseqs.newick")

strip_subspecies <- function(x) {
  words <- str_split(x, " ")
  map_chr(words, ~ if (length(.x) >= 3) paste(.x[1], .x[2]) 
          else paste(.x, collapse = " "))
}

mdf <- mdf %>%
  mutate(SpeciesName_match = strip_subspecies(SpeciesName))

tree$tip.label.clean <- tree$tip.label %>%
  str_replace("_[0-9]+$", "") %>%
  str_replace_all("_", " ")

tip_df <- tibble(
  tip.label       = tree$tip.label,
  tip.label.clean = tree$tip.label.clean
) %>%
  group_by(tip.label.clean) %>%
  slice(1) %>%
  ungroup()

mdf_species <- tibble(SpeciesName_match = unique(mdf$SpeciesName_match))

missing_from_tree <- mdf_species %>%
  filter(!SpeciesName_match %in% tip_df$tip.label.clean)

if (nrow(missing_from_tree) > 0) {
  message("Species still not matched after subspecies stripping:")
  print(missing_from_tree)
} else {
  message("All species matched successfully.")
}

species_to_tip <- tip_df %>%
  filter(tip.label.clean %in% mdf_species$SpeciesName_match) %>%
  select(SpeciesName_match = tip.label.clean, tip.label)

mdf <- mdf %>%
  left_join(species_to_tip, by = "SpeciesName_match")

tips_to_keep <- species_to_tip$tip.label
tree_pruned  <- keep.tip(tree, tips_to_keep)

tree_pruned$tip.label <- tree_pruned$tip.label %>%
  str_replace("_[0-9]+$", "") %>%
  str_replace_all("_", " ")

stopifnot(all(unique(mdf$SpeciesName_match) %in% tree_pruned$tip.label))

A <- ape::vcv.phylo(tree_pruned, corr = TRUE)

# =============================================================================
# SHARED SETTINGS
# =============================================================================

# NOTE on priors: intercept prior normal(130, 40) assumes RGB brightness on a
# 0-255 scale. If Blair et al.'s FIJI output is 0-1 normalised, change to
# normal(0.5, 0.2) and slope prior to normal(0, 0.1).
bayes_priors <- c(
  prior(normal(130, 40),  class = Intercept),
  prior(normal(0,  20),   class = b),
  prior(exponential(0.1), class = sd),
  prior(exponential(0.1), class = sigma)
)

# Shared posterior summary function
summarise_posterior <- function(fit, model_name) {
  as_draws_df(fit) %>%
    select(starts_with("b_")) %>%
    pivot_longer(everything(), names_to = "parameter", values_to = "value") %>%
    group_by(parameter) %>%
    summarise(
      median   = median(value),
      mean     = mean(value),
      lower_95 = quantile(value, 0.025),
      upper_95 = quantile(value, 0.975),
      lower_90 = quantile(value, 0.05),
      upper_90 = quantile(value, 0.95),
      pd       = pmax(mean(value > 0), mean(value < 0)),
      .groups  = "drop"
    ) %>%
    mutate(model = model_name)
}

# Shared lambda analog function
compute_lambda_analog <- function(fit) {
  as_draws_df(fit) %>%
    select(starts_with("sd_"), sigma) %>%
    mutate(across(everything(), ~ .^2)) %>%
    mutate(
      total_var     = rowSums(across(everything())),
      phylo_var     = .data[[names(.)[str_detect(names(.), "SpeciesName_match")][1]]],
      lambda_analog = phylo_var / total_var
    ) %>%
    summarise(
      lambda_median = median(lambda_analog),
      lambda_lower  = quantile(lambda_analog, 0.025),
      lambda_upper  = quantile(lambda_analog, 0.975)
    )
}

# Helper: extract conditional effects cleanly
get_ce <- function(fit, effects, conditions = NULL) {
  conditional_effects(fit, effects = effects, conditions = conditions)[[1]]
}

size_colors <- c(
  "Small (-2 SD)"  = "#e09f3e",
  "Average (0 SD)" = "#9e2a2b",
  "Large (+2 SD)"  = "#540b0e"
)

size_conditions <- data.frame(
  MajorAxis_Dorsal = c(-2, 0, 2),
  cond__           = c("Small (-2 SD)", "Average (0 SD)", "Large (+2 SD)")
)

# =============================================================================
# IMMUNE FUNCTION HYPOTHESIS
# Prediction: higher melanism in wetter environments (more pathogens)
# Predictors: AnnualPrecipitation, meanSoilMoisture, MajorAxis_Dorsal
# No interactions: immune function not expected to scale with body size
# Both dorsal and ventral surfaces examined
# =============================================================================

# --- Dorsal immune model ---
cat("\nFitting Bayesian phylogenetic mixed model: IMMUNE - DORSAL brightness...\n")

bayes_immune_dorsal <- brm(
  formula = HSV_V_dorsal ~
    AnnualPrecipitation +
    meanSoilMoisture +
    MajorAxis_Dorsal +
    (1 | gr(SpeciesName_match, cov = A)) +
    (1 | IL_Site),
  data    = mdf,
  data2   = list(A = A),
  family  = gaussian(),
  prior   = bayes_priors,
  chains  = 4,
  cores   = 4,
  iter    = 4000,
  warmup  = 1000,
  control = list(adapt_delta = 0.95, max_treedepth = 12),
  seed    = 42,
  file    = "modelResults/bayes_immune_dorsal_HSV"
)

cat("\n--- Immune dorsal model summary ---\n")
print(summary(bayes_immune_dorsal))
pp_check(bayes_immune_dorsal, ndraws = 100)

# --- Ventral immune model ---
cat("\nFitting Bayesian phylogenetic mixed model: IMMUNE - VENTRAL brightness...\n")

bayes_immune_ventral <- brm(
  formula = HSV_V_ventral ~
    AnnualPrecipitation +
    meanSoilMoisture +
    MajorAxis_Dorsal +
    (1 | gr(SpeciesName_match, cov = A)) +
    (1 | IL_Site),
  data    = mdf,
  data2   = list(A = A),
  family  = gaussian(),
  prior   = bayes_priors,
  chains  = 4,
  cores   = 4,
  iter    = 4000,
  warmup  = 1000,
  control = list(adapt_delta = 0.95, max_treedepth = 12),
  seed    = 42,
  file    = "modelResults/bayes_immune_ventral_HSV"
)

cat("\n--- Immune ventral model summary ---\n")
print(summary(bayes_immune_ventral))
pp_check(bayes_immune_ventral, ndraws = 100)

# --- Immune: lambda analogs ---
lambda_immune_dorsal  <- compute_lambda_analog(bayes_immune_dorsal)
lambda_immune_ventral <- compute_lambda_analog(bayes_immune_ventral)

cat(sprintf("\nImmune dorsal  lambda analog: median = %.3f, 95%% CrI [%.3f, %.3f]\n",
            lambda_immune_dorsal$lambda_median,
            lambda_immune_dorsal$lambda_lower,
            lambda_immune_dorsal$lambda_upper))
cat(sprintf("Immune ventral lambda analog: median = %.3f, 95%% CrI [%.3f, %.3f]\n",
            lambda_immune_ventral$lambda_median,
            lambda_immune_ventral$lambda_lower,
            lambda_immune_ventral$lambda_upper))

# --- Immune: posterior summaries ---
posterior_immune <- bind_rows(
  summarise_posterior(bayes_immune_dorsal,  "immune_dorsal"),
  summarise_posterior(bayes_immune_ventral, "immune_ventral")
)
print(posterior_immune)
write_csv(posterior_immune, "modelResults/bayesian_posterior_immune.csv")

# --- Immune: figures ---
# Coefficient plot — both surfaces on one panel for easy comparison
coef_immune <- posterior_immune %>%
  filter(parameter != "b_Intercept") %>%
  mutate(
    parameter = case_when(
      parameter == "b_AnnualPrecipitation" ~ "Annual precipitation",
      parameter == "b_meanSoilMoisture"    ~ "Soil moisture",
      parameter == "b_MajorAxis_Dorsal"    ~ "Body size"
    ),
    surface = if_else(str_detect(model, "dorsal"), "Dorsal", "Ventral")
  )

p_immune_coef <- ggplot(coef_immune,
                        aes(x = median, y = parameter, color = surface,
                            xmin = lower_95, xmax = upper_95)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbar(width = 0.15, position = position_dodge(width = 0.5)) +
  geom_point(size = 2.5,    position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("Dorsal" = "#2c7bb6", "Ventral" = "#d7191c")) +
  scale_x_continuous(limits = c(-12, 12)) +
  labs(x = "Posterior median (95% CrI)", y = "", color = "Surface") +
  theme_classic() +
  theme(axis.text  = element_text(size = 12),
        axis.title = element_text(size = 13),
        legend.position = c(0.85, 0.15))

# Conditional effects: dorsal
ap_imm_d  <- get_ce(bayes_immune_dorsal, "AnnualPrecipitation")
msm_imm_d <- get_ce(bayes_immune_dorsal, "meanSoilMoisture")
bs_imm_d  <- get_ce(bayes_immune_dorsal, "MajorAxis_Dorsal")

p_imm_d_ap <- ggplot(ap_imm_d, aes(x = AnnualPrecipitation, y = estimate__)) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = "black", alpha = 0.25) +
  geom_line(color = "black") +
  geom_point(data = mdf, aes(x = AnnualPrecipitation, y = HSV_V_dorsal),
             alpha = 0.08, position = position_jitter(width = 0.05, seed = 1)) +
  labs(x = "Annual precipitation", y = "Dorsal brightness") +
  theme_classic() +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 13))

p_imm_d_msm <- ggplot(msm_imm_d, aes(x = meanSoilMoisture, y = estimate__)) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = "black", alpha = 0.25) +
  geom_line(color = "black") +
  geom_point(data = mdf, aes(x = meanSoilMoisture, y = HSV_V_dorsal),
             alpha = 0.08, position = position_jitter(width = 0.05, seed = 1)) +
  labs(x = "Soil moisture", y = "Dorsal brightness") +
  theme_classic() +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 13))

p_imm_d_bs <- ggplot(bs_imm_d, aes(x = MajorAxis_Dorsal, y = estimate__)) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = "black", alpha = 0.25) +
  geom_line(color = "black") +
  geom_point(data = mdf, aes(x = MajorAxis_Dorsal, y = HSV_V_dorsal),
             alpha = 0.08) +
  labs(x = "Body size", y = "Dorsal brightness") +
  theme_classic() +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 13))

# Conditional effects: ventral
ap_imm_v  <- get_ce(bayes_immune_ventral, "AnnualPrecipitation")
msm_imm_v <- get_ce(bayes_immune_ventral, "meanSoilMoisture")
bs_imm_v  <- get_ce(bayes_immune_ventral, "MajorAxis_Dorsal")

p_imm_v_ap <- ggplot(ap_imm_v, aes(x = AnnualPrecipitation, y = estimate__)) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = "black", alpha = 0.25) +
  geom_line(color = "black") +
  geom_point(data = mdf, aes(x = AnnualPrecipitation, y = HSV_V_ventral),
             alpha = 0.08, position = position_jitter(width = 0.05, seed = 1)) +
  labs(x = "Annual precipitation", y = "Ventral brightness") +
  theme_classic() +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 13))

p_imm_v_msm <- ggplot(msm_imm_v, aes(x = meanSoilMoisture, y = estimate__)) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = "black", alpha = 0.25) +
  geom_line(color = "black") +
  geom_point(data = mdf, aes(x = meanSoilMoisture, y = HSV_V_ventral),
             alpha = 0.08, position = position_jitter(width = 0.05, seed = 1)) +
  labs(x = "Soil moisture", y = "Ventral brightness") +
  theme_classic() +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 13))

p_imm_v_bs <- ggplot(bs_imm_v, aes(x = MajorAxis_Dorsal, y = estimate__)) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = "black", alpha = 0.25) +
  geom_line(color = "black") +
  geom_point(data = mdf, aes(x = MajorAxis_Dorsal, y = HSV_V_ventral),
             alpha = 0.08) +
  labs(x = "Body size", y = "Ventral brightness") +
  theme_classic() +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 13))

# Combined immune figure: coefficient plot + dorsal + ventral conditional effects
fig_immune <- plot_grid(
  p_immune_coef,
  plot_grid(p_imm_d_bs, p_imm_d_ap, p_imm_d_msm,
            p_imm_v_bs, p_imm_v_ap, p_imm_v_msm,
            labels = c("B", "C", "D", "E", "F", "G"),
            nrow = 2, ncol = 3),
  labels = c("A", ""),
  ncol = 1, rel_heights = c(1, 2)
)
ggsave("figureOutputs/bayes_immune.png",
       fig_immune, dpi = 450, width = 10, height = 10)