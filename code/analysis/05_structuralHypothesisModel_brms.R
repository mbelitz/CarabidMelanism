
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
  map_chr(words, ~ if (length(.x) >= 3) paste(.x[1], .x[2]) else paste(.x, collapse = " "))
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
# STRUCTURAL SUPPORT HYPOTHESIS
# Prediction: higher melanism in areas with greater small mammal predation
# Predictors: mammalAbundance, MajorAxis_Dorsal, and their interaction
# Both dorsal and ventral surfaces examined
# Note: original script only examined dorsal; ventral added here per revision
# =============================================================================

# --- Dorsal structural model ---
cat("\nFitting Bayesian phylogenetic mixed model: STRUCTURAL - DORSAL brightness...\n")

bayes_structural_dorsal <- brm(
  formula = HSV_V_dorsal ~
    mammalAbundance +
    MajorAxis_Dorsal +
    MajorAxis_Dorsal:mammalAbundance +
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
  file    = "modelResults/bayes_structural_dorsal_HSV"
)

cat("\n--- Structural dorsal model summary ---\n")
print(summary(bayes_structural_dorsal))
pp_check(bayes_structural_dorsal, ndraws = 100)

# --- Ventral structural model ---
cat("\nFitting Bayesian phylogenetic mixed model: STRUCTURAL - VENTRAL brightness...\n")

bayes_structural_ventral <- brm(
  formula = HSV_V_ventral ~
    mammalAbundance +
    MajorAxis_Dorsal +
    MajorAxis_Dorsal:mammalAbundance +
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
  file    = "modelResults/bayes_structural_ventral_HSV"
)

cat("\n--- Structural ventral model summary ---\n")
print(summary(bayes_structural_ventral))
pp_check(bayes_structural_ventral, ndraws = 100)

# --- Structural: lambda analogs ---
lambda_structural_dorsal  <- compute_lambda_analog(bayes_structural_dorsal)
lambda_structural_ventral <- compute_lambda_analog(bayes_structural_ventral)

cat(sprintf("\nStructural dorsal  lambda analog: median = %.3f, 95%% CrI [%.3f, %.3f]\n",
            lambda_structural_dorsal$lambda_median,
            lambda_structural_dorsal$lambda_lower,
            lambda_structural_dorsal$lambda_upper))
cat(sprintf("Structural ventral lambda analog: median = %.3f, 95%% CrI [%.3f, %.3f]\n",
            lambda_structural_ventral$lambda_median,
            lambda_structural_ventral$lambda_lower,
            lambda_structural_ventral$lambda_upper))

# --- Structural: posterior summaries ---
posterior_structural <- bind_rows(
  summarise_posterior(bayes_structural_dorsal,  "structural_dorsal"),
  summarise_posterior(bayes_structural_ventral, "structural_ventral")
)
print(posterior_structural)
write_csv(posterior_structural, "modelResults/bayesian_posterior_structural.csv")

# --- Structural: figures ---
# Coefficient plot — both surfaces
coef_structural <- posterior_structural %>%
  filter(parameter != "b_Intercept") %>%
  mutate(
    parameter = case_when(
      parameter == "b_mammalAbundance"                    ~ "Mammal abundance",
      parameter == "b_MajorAxis_Dorsal"                   ~ "Body size",
      parameter == "b_mammalAbundance:MajorAxis_Dorsal"   ~ "Mammal abundance\n× Body size",
      parameter == "b_MajorAxis_Dorsal:mammalAbundance"   ~ "Mammal abundance\n× Body size"
    ),
    surface = if_else(str_detect(model, "dorsal"), "Dorsal", "Ventral")
  )

p_struct_coef <- ggplot(coef_structural,
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

# Interaction conditional effects — dorsal
mammal_conditions <- data.frame(
  MajorAxis_Dorsal = c(-2, 0, 2),
  cond__           = c("Small (-2 SD)", "Average (0 SD)", "Large (+2 SD)")
)

int_struct_d <- get_ce(bayes_structural_dorsal,
                       "mammalAbundance:MajorAxis_Dorsal",
                       conditions = mammal_conditions) %>%
  filter(effect2__ %in% c(-1, 0, 1)) %>%
  mutate(body_size_group = factor(
    case_when(
      effect2__ == -1 ~ "Small (-2 SD)",
      effect2__ ==  0 ~ "Average (0 SD)",
      effect2__ ==  1 ~ "Large (+2 SD)"
    ),
    levels = c("Small (-2 SD)", "Average (0 SD)", "Large (+2 SD)")
  ))

p_struct_d_int <- ggplot(int_struct_d,
                         aes(x = mammalAbundance, y = estimate__,
                             color = body_size_group, fill = body_size_group)) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.30, color = NA) +
  geom_line() +
  geom_point(data = mdf,
             aes(x = mammalAbundance, y = HSV_V_dorsal),
             inherit.aes = FALSE, alpha = 0.08,
             position = position_jitter(width = 0.05, seed = 1)) +
  scale_color_manual(values = size_colors) +
  scale_fill_manual(values  = size_colors) +
  labs(x = "Mammal abundance", y = "Dorsal brightness",
       color = "Body size", fill = "Body size") +
  theme_classic() +
  theme(legend.position = c(0.80, 0.85),
        legend.text     = element_text(size = 9),
        axis.text       = element_text(size = 12),
        axis.title      = element_text(size = 13))

# Interaction conditional effects — ventral
int_struct_v <- get_ce(bayes_structural_ventral,
                       "mammalAbundance:MajorAxis_Dorsal",
                       conditions = mammal_conditions) %>%
  filter(effect2__ %in% c(-1, 0, 1)) %>%
  mutate(body_size_group = factor(
    case_when(
      effect2__ == -1 ~ "Small (-2 SD)",
      effect2__ ==  0 ~ "Average (0 SD)",
      effect2__ ==  1 ~ "Large (+2 SD)"
    ),
    levels = c("Small (-2 SD)", "Average (0 SD)", "Large (+2 SD)")
  ))

p_struct_v_int <- ggplot(int_struct_v,
                         aes(x = mammalAbundance, y = estimate__,
                             color = body_size_group, fill = body_size_group)) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.30, color = NA) +
  geom_line() +
  geom_point(data = mdf,
             aes(x = mammalAbundance, y = HSV_V_ventral),
             inherit.aes = FALSE, alpha = 0.08,
             position = position_jitter(width = 0.05, seed = 1)) +
  scale_color_manual(values = size_colors) +
  scale_fill_manual(values  = size_colors) +
  labs(x = "Mammal abundance", y = "Ventral brightness",
       color = "Body size", fill = "Body size") +
  theme_classic() +
  theme(legend.position = c(0.80, 0.85),
        legend.text     = element_text(size = 9),
        axis.text       = element_text(size = 12),
        axis.title      = element_text(size = 13))

# Combined structural figure: coefficient plot + dorsal + ventral interactions
fig_structural <- plot_grid(
  p_struct_coef,
  plot_grid(p_struct_d_int, p_struct_v_int,
            labels = c("B", "C"),
            nrow = 1, ncol = 2),
  labels = c("A", ""),
  ncol = 1, rel_heights = c(1, 1)
)
ggsave("figureOutputs/bayes_structural.png",
       fig_structural, dpi = 450, width = 10, height = 8)
