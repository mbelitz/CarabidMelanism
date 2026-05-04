library(tidyverse)
library(lme4)
library(lmerTest)
library(ape)
library(phytools)  
library(brms)
library(tidybayes)
library(cowplot)
library(ggnewscale)

# =============================================================================
# DATA & TREE PREPARATION
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

# --- Reduce trinomial subspecies names to binomial species names ---
# Leaves binomials unchanged; strips third epithet from any trinomials.
# Five subspecies in mdf (e.g. "Bembidion quadrimaculatum oppositum") are
# matched to their nominal species in the phylogeny. This is noted in methods.
strip_subspecies <- function(x) {
  words <- str_split(x, " ")
  map_chr(words, ~ if (length(.x) >= 3) paste(.x[1], .x[2]) else paste(.x, collapse = " "))
}

mdf <- mdf %>%
  mutate(SpeciesName_match = strip_subspecies(SpeciesName))

# --- Match tip labels to species names ---
# Tree tips: "Genus_species_number" -> strip trailing _number -> "Genus species"
tree$tip.label.clean <- tree$tip.label %>%
  str_replace("_[0-9]+$", "") %>%
  str_replace_all("_", " ")

# Where multiple tips map to the same species, keep first occurrence
tip_df <- tibble(
  tip.label       = tree$tip.label,
  tip.label.clean = tree$tip.label.clean
) %>%
  group_by(tip.label.clean) %>%
  slice(1) %>%
  ungroup()

# Species in mdf (using matched binomial names)
mdf_species <- tibble(SpeciesName_match = unique(mdf$SpeciesName_match))

# Diagnostic: any species still missing after subspecies stripping?
missing_from_tree <- mdf_species %>%
  filter(!SpeciesName_match %in% tip_df$tip.label.clean)

if (nrow(missing_from_tree) > 0) {
  message("Species still not matched after subspecies stripping:")
  print(missing_from_tree)
} else {
  message("All species matched successfully.")
}

# Build species-to-tip lookup and join to mdf
species_to_tip <- tip_df %>%
  filter(tip.label.clean %in% mdf_species$SpeciesName_match) %>%
  select(SpeciesName_match = tip.label.clean, tip.label)

mdf <- mdf %>%
  left_join(species_to_tip, by = "SpeciesName_match")

# Prune tree to species present in mdf
tips_to_keep <- species_to_tip$tip.label
tree_pruned  <- keep.tip(tree, tips_to_keep)

# Rename pruned tree tips to clean binomial names for brms
tree_pruned$tip.label <- tree_pruned$tip.label %>%
  str_replace("_[0-9]+$", "") %>%
  str_replace_all("_", " ")

# Verify all mdf species are in the pruned tree
stopifnot(all(unique(mdf$SpeciesName_match) %in% tree_pruned$tip.label))

# Compute phylogenetic covariance matrix (Brownian motion, correlation form)
A <- ape::vcv.phylo(tree_pruned, corr = TRUE)

# =============================================================================
# PART 1: PHYLOGENETIC SIGNAL IN LMM RESIDUALS
# =============================================================================
# We test for phylogenetic signal in species-averaged LMM residuals using
# Blomberg's K and Pagel's lambda, to motivate the phylogenetic mixed model.

# --- Dorsal LMM ---
dorsal_lmm <- lmer(
  HSV_V_dorsal ~
    AnnualPrecipitation +
    meanSoilMoisture +
    meanVPD +
    MajorAxis_Dorsal +
    MajorAxis_Dorsal:meanVPD +
    MajorAxis_Dorsal:meanSoilMoisture +
    (1 | Tribe/Genus) + (1 | IL_Site),
  data      = mdf,
  REML      = FALSE,
  na.action = "na.fail"
)

residual_df <- mdf %>%
  mutate(lmm_resid = residuals(dorsal_lmm)) %>%
  group_by(SpeciesName_match) %>%
  summarise(mean_resid = mean(lmm_resid, na.rm = TRUE), .groups = "drop") %>%
  filter(SpeciesName_match %in% tree_pruned$tip.label)

resid_vec        <- residual_df$mean_resid
names(resid_vec) <- residual_df$SpeciesName_match

K_result      <- phytools::phylosig(tree_pruned, resid_vec, method = "K",      test = TRUE, nsim = 1000)
lambda_result <- phytools::phylosig(tree_pruned, resid_vec, method = "lambda", test = TRUE)

cat("\n--- Phylogenetic signal in LMM residuals (dorsal desiccation model) ---\n")
cat(sprintf("Blomberg's K   = %.4f,  p = %.4f\n", K_result$K, K_result$P))
cat(sprintf("Pagel's lambda = %.4f,  p = %.4f (LRT vs lambda = 0)\n",
            lambda_result$lambda, lambda_result$P))

# --- Ventral LMM ---
ventral_lmm <- lmer(
  HSV_V_ventral ~
    AnnualPrecipitation +
    meanSoilMoisture +
    meanVPD +
    MajorAxis_Dorsal +
    MajorAxis_Dorsal:meanVPD +
    MajorAxis_Dorsal:meanSoilMoisture +
    (1 | Tribe/Genus) + (1 | IL_Site),
  data      = mdf,
  REML      = FALSE,
  na.action = "na.fail"
)

residual_df_v <- mdf %>%
  mutate(lmm_resid = residuals(ventral_lmm)) %>%
  group_by(SpeciesName_match) %>%
  summarise(mean_resid = mean(lmm_resid, na.rm = TRUE), .groups = "drop") %>%
  filter(SpeciesName_match %in% tree_pruned$tip.label)

resid_vec_v        <- residual_df_v$mean_resid
names(resid_vec_v) <- residual_df_v$SpeciesName_match

K_result_v      <- phytools::phylosig(tree_pruned, resid_vec_v, method = "K",      test = TRUE, nsim = 1000)
lambda_result_v <- phytools::phylosig(tree_pruned, resid_vec_v, method = "lambda", test = TRUE)

cat("\n--- Phylogenetic signal in LMM residuals (ventral desiccation model) ---\n")
cat(sprintf("Blomberg's K   = %.4f,  p = %.4f\n", K_result_v$K, K_result_v$P))
cat(sprintf("Pagel's lambda = %.4f,  p = %.4f (LRT vs lambda = 0)\n",
            lambda_result_v$lambda, lambda_result_v$P))

# =============================================================================
# PART 2: BAYESIAN PHYLOGENETIC MIXED MODELS (brms)
# =============================================================================
# Model structure:
#   Fixed effects : same as original LMM
#   Random effects: (1 | gr(SpeciesName_match, cov = A))  <- phylogenetic signal
#                   (1 | IL_Site)                          <- site
#
# The gr() term uses phylogenetic covariance matrix A (Brownian motion).
# Variance partitioning across these terms yields a Pagel's lambda analog:
#   lambda ~ phylo_var / (phylo_var + all other variances)
#
# NOTE on priors: intercept prior normal(130, 40) assumes RGB brightness on a
# 0-255 scale. If Blair et al.'s FIJI output is 0-1 normalised, change to
# normal(0.5, 0.2) and slope prior to normal(0, 0.1).
# =============================================================================

bayes_priors <- c(
  prior(normal(130, 40),  class = Intercept),
  prior(normal(0,  20),   class = b),
  prior(exponential(0.1), class = sd),
  prior(exponential(0.1), class = sigma)
)

# --- DORSAL model ---
cat("\nFitting Bayesian phylogenetic mixed model: DORSAL brightness...\n")

bayes_dorsal <- brm(
  formula = HSV_V_dorsal ~
    AnnualPrecipitation +
    meanSoilMoisture +
    meanVPD +
    MajorAxis_Dorsal +
    MajorAxis_Dorsal:meanVPD +
    MajorAxis_Dorsal:meanSoilMoisture +
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
  file    = "modelResults/bayes_dorsal_desiccation_HSV"
)

cat("\n--- Dorsal model summary ---\n")
print(summary(bayes_dorsal))
pp_check(bayes_dorsal, ndraws = 100)

# --- VENTRAL model ---
cat("\nFitting Bayesian phylogenetic mixed model: VENTRAL brightness...\n")

bayes_ventral <- brm(
  formula = HSV_V_ventral ~
    AnnualPrecipitation +
    meanSoilMoisture +
    meanVPD +
    MajorAxis_Dorsal +
    MajorAxis_Dorsal:meanVPD +
    MajorAxis_Dorsal:meanSoilMoisture +
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
  file    = "modelResults/bayes_ventral_desiccation_HSV"
)

cat("\n--- Ventral model summary ---\n")
print(summary(bayes_ventral))
pp_check(bayes_ventral, ndraws = 100)

# =============================================================================
# PART 3: LAMBDA ANALOG & VARIANCE PARTITIONING
# =============================================================================

compute_lambda_analog <- function(fit) {
  as_draws_df(fit) %>%
    select(starts_with("sd_"), sigma) %>%
    mutate(across(everything(), ~ .^2)) %>%  # SD -> variance
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

lambda_dorsal  <- compute_lambda_analog(bayes_dorsal)
lambda_ventral <- compute_lambda_analog(bayes_ventral)

cat(sprintf("\nDorsal  lambda analog: median = %.3f, 95%% CrI [%.3f, %.3f]\n",
            lambda_dorsal$lambda_median, lambda_dorsal$lambda_lower, lambda_dorsal$lambda_upper))
cat(sprintf("Ventral lambda analog: median = %.3f, 95%% CrI [%.3f, %.3f]\n",
            lambda_ventral$lambda_median, lambda_ventral$lambda_lower, lambda_ventral$lambda_upper))

partition_variance <- function(fit, model_name) {
  draws <- as_draws_df(fit) %>%
    select(starts_with("sd_"), sigma) %>%
    mutate(across(everything(), ~ .^2))
  
  phylo_col <- names(draws)[str_detect(names(draws), "SpeciesName_match")]
 # tribe_col <- names(draws)[str_detect(names(draws), "Tribe__")]
 # genus_col <- names(draws)[str_detect(names(draws), "Genus__")]
  site_col  <- names(draws)[str_detect(names(draws), "IL_Site")]
  
  draws %>%
    mutate(
      total     = rowSums(across(everything())),
      pct_phylo = .data[[phylo_col[1]]] / total * 100,
    #  pct_tribe = if (length(tribe_col) > 0) .data[[tribe_col[1]]] / total * 100 else NA_real_,
    #  pct_genus = if (length(genus_col) > 0) .data[[genus_col[1]]] / total * 100 else NA_real_,
      pct_site  = if (length(site_col)  > 0) .data[[site_col[1]]]  / total * 100 else NA_real_,
      pct_resid = sigma / total * 100
    ) %>%
    summarise(across(starts_with("pct_"),
                     list(median = median,
                          lower  = ~ quantile(.x, 0.025),
                          upper  = ~ quantile(.x, 0.975)),
                     .names = "{.col}_{.fn}")) %>%
    mutate(model = model_name)
}

var_dorsal  <- partition_variance(bayes_dorsal,  "dorsal")
var_ventral <- partition_variance(bayes_ventral, "ventral")
var_all     <- bind_rows(var_dorsal, var_ventral)

print(var_all)
write_csv(var_all, "modelResults/variance_partitioning.csv")

# =============================================================================
# PART 4: POSTERIOR SUMMARIES FOR MANUSCRIPT REPORTING
# =============================================================================

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
      pd       = pmax(mean(value > 0), mean(value < 0)),  # prob of dominant direction
      .groups  = "drop"
    ) %>%
    mutate(model = model_name)
}

posterior_all <- bind_rows(
  summarise_posterior(bayes_dorsal,  "dorsal"),
  summarise_posterior(bayes_ventral, "ventral")
)

print(posterior_all)
write_csv(posterior_all, "modelResults/bayesian_posterior_summaries.csv")

# =============================================================================
# PART 5: FIGURES
# =============================================================================

size_colors <- c(
  "Small (-2 SD)"   = "#e09f3e",
  "Average (0 SD)"  = "#9e2a2b",
  "Large (+2 SD)"   = "#540b0e"
)

# Helper: extract conditional effects cleanly
get_ce <- function(fit, effects, conditions = NULL) {
  conditional_effects(fit, effects = effects, conditions = conditions)[[1]]
}

size_conditions <- data.frame(
  MajorAxis_Dorsal = c(-2, 0, 2),
  cond__           = c("Small (-2 SD)", "Average (0 SD)", "Large (+2 SD)")
)

# --- DORSAL figures ---
ap_ce  <- get_ce(bayes_dorsal, "AnnualPrecipitation")
msm_ce <- get_ce(bayes_dorsal, "meanSoilMoisture")
bs_ce  <- get_ce(bayes_dorsal, "MajorAxis_Dorsal")
int_ce <- get_ce(bayes_dorsal, "meanVPD:MajorAxis_Dorsal", conditions = size_conditions) %>%
  mutate(body_size_group = factor(cond__, levels = names(size_colors)))

p_bs <- ggplot(bs_ce, aes(x = MajorAxis_Dorsal, y = estimate__)) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = "black", alpha = 0.25) +
  geom_line(color = "black") +
  geom_point(data = mdf, aes(x = MajorAxis_Dorsal, y = HSV_V_dorsal),
             alpha = 0.08) +
  labs(x = "Body size", y = "Dorsal brightness") +
  theme_classic() +
  theme(axis.text = element_text(size = 13), axis.title = element_text(size = 14))

int_ce_plot <- int_ce %>%
  filter(effect2__ %in% c(-1, 0, 1)) %>%           # keep only the three focal body sizes
  mutate(body_size_group = factor(
    case_when(
      effect2__ == -1 ~ "Small (-2 SD)",
      effect2__ ==  0 ~ "Average (0 SD)",
      effect2__ ==  1 ~ "Large (+2 SD)"
    ),
    levels = c("Small (-2 SD)", "Average (0 SD)", "Large (+2 SD)")
  ))

p_vpd <- ggplot(int_ce_plot, 
                aes(x = meanVPD, y = estimate__,
                    color = body_size_group, 
                    fill  = body_size_group)) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.30, color = NA) +
  geom_line() +
  geom_point(data = mdf,
             aes(x = meanVPD, y = HSV_V_dorsal),
             inherit.aes = FALSE, alpha = 0.08,
             position = position_jitter(width = 0.05, seed = 1)) +
  scale_color_manual(values = size_colors) +
  scale_fill_manual(values  = size_colors) +
  labs(x = "Vapour pressure deficit", y = "Dorsal brightness",
       color = "Body size", fill = "Body size") +
  theme_classic() +
  theme(legend.position = c(0.25, 0.90),
        legend.text     = element_text(size = 9),
        axis.text       = element_text(size = 12),
        axis.title      = element_text(size = 14))

p_msm <- ggplot(msm_ce, aes(x = meanSoilMoisture, y = estimate__)) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = "black", alpha = 0.25) +
  geom_line(color = "black") +
  geom_point(data = mdf, aes(x = meanSoilMoisture, y = HSV_V_dorsal),
             alpha = 0.08, position = position_jitter(width = 0.05, seed = 1)) +
  labs(x = "Soil moisture", y = "Dorsal brightness") +
  theme_classic() +
  theme(axis.text = element_text(size = 13), axis.title = element_text(size = 14))

p_ap <- ggplot(ap_ce, aes(x = AnnualPrecipitation, y = estimate__)) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = "black", alpha = 0.25) +
  geom_line(color = "black") +
  geom_point(data = mdf, aes(x = AnnualPrecipitation, y = HSV_V_dorsal),
             alpha = 0.08, position = position_jitter(width = 0.05, seed = 1)) +
  labs(x = "Annual precipitation", y = "Dorsal brightness") +
  theme_classic() +
  theme(axis.text = element_text(size = 13), axis.title = element_text(size = 14))

fig_dorsal <- plot_grid(p_bs, p_vpd, p_msm, p_ap,
                        labels = c("A", "B", "C", "D"), nrow = 2, ncol = 2)
ggsave("figureOutputs/bayes_dorsal_desiccation.png",
       fig_dorsal, dpi = 450, width = 8, height = 8)

# --- VENTRAL figures ---
ap_ce_v  <- get_ce(bayes_ventral, "AnnualPrecipitation")
msm_ce_v <- get_ce(bayes_ventral, "meanSoilMoisture")
bs_ce_v  <- get_ce(bayes_ventral, "MajorAxis_Dorsal")
int_ce_v <- get_ce(bayes_ventral, "meanSoilMoisture:MajorAxis_Dorsal", conditions = size_conditions) %>%
  mutate(body_size_group = factor(cond__, levels = names(size_colors)))

p_bs_v <- ggplot(bs_ce_v, aes(x = MajorAxis_Dorsal, y = estimate__)) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = "black", alpha = 0.25) +
  geom_line(color = "black") +
  geom_point(data = mdf, aes(x = MajorAxis_Dorsal, y = HSV_V_ventral),
             alpha = 0.08) +
  labs(x = "Body size", y = "Ventral brightness") +
  theme_classic() +
  theme(axis.text = element_text(size = 13), axis.title = element_text(size = 14))

p_ap_v <- ggplot(ap_ce_v, aes(x = AnnualPrecipitation, y = estimate__)) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = "black", alpha = 0.25) +
  geom_line(color = "black") +
  geom_point(data = mdf, aes(x = AnnualPrecipitation, y = HSV_V_ventral),
             alpha = 0.08, position = position_jitter(width = 0.05, seed = 1)) +
  labs(x = "Annual precipitation", y = "Ventral brightness") +
  theme_classic() +
  theme(axis.text = element_text(size = 13), axis.title = element_text(size = 14))

p_msm_v <- ggplot(msm_ce_v, aes(x = meanSoilMoisture, y = estimate__)) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = "black", alpha = 0.25) +
  geom_line(color = "black") +
  geom_point(data = mdf, aes(x = meanSoilMoisture, y = HSV_V_ventral),
             alpha = 0.08, position = position_jitter(width = 0.05, seed = 1)) +
  labs(x = "Soil moisture", y = "Ventral brightness") +
  theme_classic() +
  theme(axis.text = element_text(size = 13), axis.title = element_text(size = 14))

int_ce_plot_v <- int_ce_v %>%
  filter(effect2__ %in% c(-1, 0, 1)) %>%           # keep only the three focal body sizes
  mutate(body_size_group = factor(
    case_when(
      effect2__ == -1 ~ "Small (-2 SD)",
      effect2__ ==  0 ~ "Average (0 SD)",
      effect2__ ==  1 ~ "Large (+2 SD)"
    ),
    levels = c("Small (-2 SD)", "Average (0 SD)", "Large (+2 SD)")
  ))

p_int_v <- ggplot(int_ce_plot_v, 
                aes(x = meanSoilMoisture, y = estimate__,
                    color = body_size_group, 
                    fill  = body_size_group)) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.30, color = NA) +
  geom_line() +
  geom_point(data = mdf,
             aes(x = meanSoilMoisture, y = HSV_V_dorsal),
             inherit.aes = FALSE, alpha = 0.08,
             position = position_jitter(width = 0.05, seed = 1)) +
  scale_color_manual(values = size_colors) +
  scale_fill_manual(values  = size_colors) +
  labs(x = "Soil moisture", y = "Dorsal brightness",
       color = "Body size", fill = "Body size") +
  theme_classic() +
  theme(legend.position = c(0.25, 0.90),
        legend.text     = element_text(size = 9),
        axis.text       = element_text(size = 12),
        axis.title      = element_text(size = 14))

fig_ventral <- plot_grid(p_bs_v, p_ap_v, p_msm_v, p_int_v,
                         labels = c("A", "B", "C", "D"), nrow = 2, ncol = 2)
ggsave("figureOutputs/bayes_ventral_desiccation.png",
       fig_ventral, dpi = 450, width = 8, height = 8)
