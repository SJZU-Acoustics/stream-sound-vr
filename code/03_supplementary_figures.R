# Curated supplementary figures. Each item directly supports one main-text RQ.

source("code/00_setup.R")

# -----------------------------------------------------------------------------
# Figure S1: RQ1 outcome-family screen and distributional robustness.
screen <- read_csv(file.path(LOCK_DIR, "rq1_outcome_screen.csv"), show_col_types = FALSE)
robust <- read_csv(file.path(LOCK_DIR, "rq1_robustness.csv"), show_col_types = FALSE)

screen_order <- c("ISO pleasantness", "Annoying - pleasant", "Noisy - calm",
                  "Pleasant", "Noisy/chaotic", "Lively", "Bland", "Calm",
                  "Annoying", "Immersive", "Dull")
screen <- screen |>
  mutate(
    label = factor(label, levels = rev(screen_order)),
    family = factor(family, levels = c("Primary", "Convergent", "Attribute"))
  )
pS1a <- ggplot(screen, aes(dz, label)) +
  geom_vline(xintercept = 0, colour = COL_MID, linewidth = 0.35) +
  geom_errorbar(aes(xmin = lo, xmax = hi, colour = family), orientation = "y", width = 0, linewidth = 0.7) +
  geom_point(aes(colour = family, shape = holm_significant), size = 2.5, stroke = 0.75, fill = "white") +
  scale_colour_manual(values = c("Primary" = COL_BLUE, "Convergent" = COL_GREEN,
                                 "Attribute" = COL_MID)) +
  scale_shape_manual(values = c(`TRUE` = 19, `FALSE` = 21),
                     labels = c(`TRUE` = "Holm P < .05", `FALSE` = "Holm P ≥ .05")) +
  scale_x_continuous(limits = c(-0.82, 0.82), breaks = c(-0.8, -0.4, 0, 0.4, 0.8)) +
  # Extra headroom above the top category so the panel tag "a" does not
  # collide with the "ISO pleasantness" label.
  scale_y_discrete(expand = expansion(add = c(0.6, 1.2))) +
  labs(x = "Paired standardised stream effect", y = NULL, shape = NULL) +
  theme_p17() +
  theme(legend.position = "bottom", legend.box = "vertical")

robust <- robust |>
  mutate(method = factor(method, levels = rev(c("Participant mean", "Median", "20% trimmed mean", "Leave-one-out mean"))))
pS1b <- ggplot(robust, aes(estimate, method)) +
  geom_vline(xintercept = 0, colour = COL_MID, linewidth = 0.35) +
  geom_errorbar(aes(xmin = lo, xmax = hi, linetype = interval), orientation = "y", width = 0, linewidth = 0.8, colour = COL_DARK) +
  geom_point(size = 2.7, colour = COL_BLUE) +
  scale_linetype_manual(values = c("95% bootstrap CI" = "solid", "range" = "dashed")) +
  scale_x_continuous(limits = c(-0.2, 4.2), breaks = c(0, 1, 2, 3, 4)) +
  labs(x = "Pooled benefit (ISO points)", y = NULL, linetype = NULL) +
  theme_p17() + theme(legend.position = "bottom")

figS1 <- (pS1a | pS1b) +
  plot_layout(widths = c(1.28, 0.9)) +
  plot_annotation(tag_levels = "a") & tag_theme
save_figure(figS1, file.path(SI_FIG_DIR, "Figure_S1_rq1_robustness.png"), 178, 95)

# -----------------------------------------------------------------------------
# Figure S2: RQ2 loadings and sensitivity to a transparent activation composite.
loadings <- read_csv(file.path(LOCK_DIR, "pca_loadings.csv"), show_col_types = FALSE) |>
  mutate(label = factor(label, levels = rev(c("Pleasant", "Noisy/chaotic", "Lively", "Bland",
                                              "Calm", "Annoying", "Immersive", "Dull"))))
dim_eff <- read_csv(file.path(LOCK_DIR, "rq2_dimension_effects.csv"), show_col_types = FALSE)

pS2a <- ggplot(loadings, aes(component, label, fill = loading)) +
  geom_tile(colour = "white", linewidth = 0.6) +
  geom_text(aes(label = sprintf("%+.2f", loading)), family = "Arial", size = 2.45,
            colour = if_else(abs(loadings$loading) > 0.38, "white", COL_DARK)) +
  scale_fill_gradient2(low = COL_VERMILION, mid = "white", high = COL_BLUE,
                       midpoint = 0, limits = c(-0.5, 0.5), name = "Loading") +
  labs(x = "Orthogonal component", y = NULL) +
  theme_p17() +
  theme(axis.ticks.y = element_blank(), legend.position = "bottom",
        legend.key.width = unit(15, "mm"))

effect_data <- dim_eff |>
  filter(metric %in% c("PC1", "PC2", "ISO", "activation_proxy")) |>
  mutate(
    label = recode(metric, PC1 = "Valence PC1", PC2 = "Activation PC2",
                   ISO = "ISO valence", activation_proxy = "Activation composite"),
    label = factor(label, levels = rev(c("Valence PC1", "Activation PC2", "ISO valence", "Activation composite"))),
    definition = if_else(metric %in% c("PC1", "PC2"), "Orthogonal PCA", "Simple composite")
  )
pS2b <- ggplot(effect_data, aes(estimate, label)) +
  geom_vline(xintercept = 0, colour = COL_MID, linewidth = 0.35) +
  geom_errorbar(aes(xmin = lo, xmax = hi, colour = definition), orientation = "y", width = 0, linewidth = 0.75) +
  geom_point(aes(colour = definition), size = 2.7) +
  scale_colour_manual(values = c("Orthogonal PCA" = COL_BLUE, "Simple composite" = COL_ORANGE)) +
  scale_x_continuous(limits = c(-0.2, 0.48), breaks = c(-0.2, 0, 0.2, 0.4)) +
  labs(x = "Standardised stream effect", y = NULL) +
  theme_p17() + theme(legend.position = "bottom")

contrast_data <- dim_eff |>
  filter(metric %in% c("PCA_contrast", "transparent_contrast")) |>
  mutate(
    label = recode(metric, PCA_contrast = "PC1 - PC2",
                   transparent_contrast = "ISO - activation composite"),
    label = factor(label, levels = rev(c("PC1 - PC2", "ISO - activation composite")))
  )
pS2c <- ggplot(contrast_data, aes(estimate, label)) +
  geom_vline(xintercept = 0, colour = COL_MID, linewidth = 0.35) +
  geom_errorbar(aes(xmin = lo, xmax = hi), orientation = "y", width = 0, linewidth = 0.75, colour = COL_DARK) +
  geom_point(size = 2.8, colour = COL_BLUE) +
  scale_x_continuous(limits = c(-0.16, 0.46), breaks = c(-0.1, 0.1, 0.3)) +
  labs(x = "Valence - activation (SD)", y = NULL) +
  theme_p17()

figS2 <- (pS2a | pS2b | pS2c) +
  plot_layout(widths = c(1.0, 1.1, 0.9)) +
  plot_annotation(tag_levels = "a") & tag_theme
save_figure(figS2, file.path(SI_FIG_DIR, "Figure_S2_rq2_dimension_sensitivity.png"), 178, 92)

# -----------------------------------------------------------------------------
# Figure S3: RQ3 correlation and held-out prediction diagnostics.
corr <- read_csv(file.path(LOCK_DIR, "rq3_pairwise_correlations.csv"), show_col_types = FALSE)
pred <- read_csv(file.path(LOCK_DIR, "rq3_predictions.csv"), show_col_types = FALSE)
by_spl <- read_csv(file.path(LOCK_DIR, "rq3_prediction_by_spl.csv"), show_col_types = FALSE)

levels_spl <- c(54, 57, 60, 63)
corr_mat <- expand_grid(level_1 = levels_spl, level_2 = levels_spl) |>
  mutate(rho = case_when(
    level_1 == level_2 ~ 1,
    TRUE ~ NA_real_
  ))
for (i in seq_len(nrow(corr))) {
  a <- corr$level_1[i]; b <- corr$level_2[i]; r <- corr$rho[i]
  corr_mat$rho[corr_mat$level_1 == a & corr_mat$level_2 == b] <- r
  corr_mat$rho[corr_mat$level_1 == b & corr_mat$level_2 == a] <- r
}
corr_mat <- corr_mat |>
  mutate(across(c(level_1, level_2), ~factor(.x, levels = levels_spl)))
pS3a <- ggplot(corr_mat, aes(level_1, level_2, fill = rho)) +
  geom_tile(colour = "white", linewidth = 0.7) +
  geom_text(aes(label = sprintf("%.2f", rho)), family = "Arial", size = 2.35,
            colour = if_else(corr_mat$rho > 0.65, "white", COL_DARK)) +
  scale_fill_gradient2(low = COL_VERMILION, mid = "white", high = COL_BLUE,
                       midpoint = 0, limits = c(-1, 1), name = "Spearman rho") +
  labs(x = "Traffic level, dB(A)", y = "Traffic level, dB(A)") +
  coord_fixed() +
  theme_p17() + theme(legend.position = "bottom", axis.ticks = element_blank())

trial <- pred |> filter(model == "Personal trial")
pS3b <- ggplot(trial, aes(observed, predicted)) +
  geom_abline(slope = 1, intercept = 0, linewidth = 0.4, linetype = "dashed", colour = COL_MID) +
  geom_point(shape = 21, fill = COL_SKY, colour = COL_BLUE, alpha = 0.48, size = 1.45, stroke = 0.25) +
  scale_x_continuous(limits = c(-18, 19), breaks = c(-15, -5, 5, 15)) +
  scale_y_continuous(limits = c(-18, 19), breaks = c(-15, -5, 5, 15)) +
  labs(x = "Observed benefit (ISO points)", y = "Held-out personal-trial prediction") +
  coord_fixed() +
  theme_p17()

by_spl <- by_spl |>
  mutate(
    model = factor(model, levels = c("Personal trial", "Traits", "Baseline state", "Combined profile")),
    skill_pct = 100 * skill
  )
pS3c <- ggplot(by_spl, aes(spl_db, skill_pct, colour = model, group = model)) +
  geom_hline(yintercept = 0, linewidth = 0.4, colour = COL_DARK) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 2.1) +
  scale_colour_manual(values = c("Personal trial" = COL_BLUE, "Traits" = COL_VERMILION,
                                 "Baseline state" = COL_GREEN, "Combined profile" = COL_PURPLE),
                      labels = c("Personal trial", "Traits", "Baseline", "Combined")) +
  scale_x_continuous(breaks = levels_spl) +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(-75, 45),
                     breaks = c(-60, -30, 0, 30)) +
  labs(x = "Held-out traffic level, dB(A)", y = "Skill vs universal mean") +
  theme_p17() +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE)) +
  theme(legend.position = "bottom", legend.box = "vertical", legend.text = element_text(size = 7.5))

figS3 <- (pS3a | pS3b | pS3c) +
  plot_layout(widths = c(0.82, 0.92, 1.15)) +
  plot_annotation(tag_levels = "a") & tag_theme
save_figure(figS3, file.path(SI_FIG_DIR, "Figure_S3_rq3_prediction_diagnostics.png"), 178, 88)

message("Supplementary figures complete.")
