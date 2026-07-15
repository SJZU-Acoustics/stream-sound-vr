# Locked display tables for the P17 writing package.

source("code/00_setup.R")

signed_num <- function(x, digits = 2) {
  ifelse(is.na(x), "--", sprintf(paste0("%+.", digits, "f"), x))
}

plain_num <- function(x, digits = 2) {
  ifelse(is.na(x), "--", sprintf(paste0("%.", digits, "f"), x))
}

p_text <- function(x) {
  ifelse(
    is.na(x), "--",
    ifelse(x < 0.001, "<.001", sub("^0", "", sprintf("%.3f", x)))
  )
}

ci_text <- function(lo, hi, digits = 2, signed = FALSE) {
  mapply(
    function(l, h, d, s) {
      formatter <- if (s) signed_num else plain_num
      paste0("[", formatter(l, d), ", ", formatter(h, d), "]")
    },
    lo, hi, digits, signed,
    USE.NAMES = FALSE
  )
}

write_display_table <- function(df, stem, widths_cm) {
  stopifnot(ncol(df) == length(widths_cm))
  write.csv(df, file.path(TABLE_DIR, paste0(stem, ".csv")), row.names = FALSE)

  column_spec <- paste0(
    ">{\\raggedright\\arraybackslash}p{", widths_cm, "cm}",
    collapse = ""
  )
  header <- paste(tex_escape(str_replace_all(names(df), "_", " ")), collapse = " & ")
  body <- apply(df, 1, function(row) {
    paste0(paste(tex_escape(row), collapse = " & "), " \\\\")
  })
  lines <- c(
    paste0("\\begin{longtable}{@{}", column_spec, "@{}}"),
    "\\toprule",
    paste0(header, " \\\\"),
    "\\midrule",
    "\\endfirsthead",
    "\\toprule",
    paste0(header, " \\\\"),
    "\\midrule",
    "\\endhead",
    "\\midrule",
    paste0("\\multicolumn{", ncol(df), "}{r}{Continued on next page} \\\\"),
    "\\endfoot",
    "\\bottomrule",
    "\\endlastfoot",
    body,
    "\\end{longtable}"
  )
  writeLines(lines, file.path(TABLE_DIR, paste0(stem, ".tex")))
}

# Table S1: the design and inferential contract.
s1 <- tribble(
  ~Component, ~Locked_specification, ~Role_in_the_paper,
  "Sample", "50 participants for subjective outcomes; 39 with usable EEG; 43 with usable EDA", "Defines the denominator for each evidence stream",
  "Design", "Within-participant 4 x 2 experiment: traffic at 54, 57, 60, and 63 dB(A), each without and with the same stream recording", "Tests the intervention across the observed traffic gradient",
  "Stream treatment", "One stream recording presented at -3 dB signal-to-noise ratio relative to traffic", "Fixed augmentation; not a dose-response comparison",
  "Context", "One Zhongshan Square virtual environment, short controlled exposures, and a young-adult sample", "Defines the boundary of inference",
  "RQ1 primary outcome", "ISO pleasantness score; pooled stream effect from a mixed model with participant intercepts", "Answers whether augmentation improves affective appraisal",
  "RQ1 level dependence", "One continuous stream-by-SPL interaction; participant-cluster bootstrap for the pooled interval", "Tests whether benefit changes over the observed gradient",
  "Multiplicity", "Holm correction across the frozen family of 11 subjective outcomes: ISO score, two convergent ratings, and eight attributes", "Prevents exploratory significance from becoming separate storylines",
  "RQ2 structure", "PCA of eight attributes, with signs fixed so PC1 is valence-like and PC2 activation-like; direct paired PC1-PC2 contrast", "Tests which experiential dimension changes primarily",
  "RQ2 sensitivity", "Transparent ISO-derived valence and activation proxies reported alongside the data-derived axes", "Shows that the dimensional conclusion depends on representation",
  "RQ3 reach", "Participant mean effects and exact binomial interval", "Quantifies how widely benefit is observed",
  "RQ3 predictability", "Participant-held-out predictions for universal, personal-trial, trait, baseline-state, and combined-profile strategies", "Tests whether selective delivery improves on universal provision",
  "Resampling", "1,000 participant-cluster bootstrap replicates; random seed 170715", "Preserves within-participant dependence and reproducibility",
  "Physiology", "Paired standardised stream effects with 90% intervals, calibrated against SPL sensitivity", "Supporting evidence only; not used to adjudicate mechanism"
)
write_display_table(s1, "Table_S1_design_and_inference", c(3.0, 7.8, 4.6))

# Table S2: RQ1 estimates, robustness, and multiplicity screen.
rq1_summary <- read.csv(file.path(LOCK_DIR, "rq1_summary.csv"), check.names = FALSE)
rq1_level <- read.csv(file.path(LOCK_DIR, "rq1_effects_by_spl.csv"), check.names = FALSE)
rq1_screen <- read.csv(file.path(LOCK_DIR, "rq1_outcome_screen.csv"), check.names = FALSE)
rq1_robust <- read.csv(file.path(LOCK_DIR, "rq1_robustness.csv"), check.names = FALSE)

s2_primary <- bind_rows(
  tibble(
    Section = "Primary",
    Quantity = "Pooled stream effect",
    Estimate = paste0(signed_num(rq1_summary$estimate[1]), " ISO points"),
    Interval = paste0("95% participant-bootstrap CI ", ci_text(rq1_summary$lo[1], rq1_summary$hi[1])),
    P = p_text(rq1_summary$p[1]),
    Holm_P = "--"
  ),
  tibble(
    Section = "Primary",
    Quantity = "Stream-by-SPL linear interaction",
    Estimate = paste0(signed_num(rq1_summary$estimate[2]), " points/dB"),
    Interval = paste0("95% participant-bootstrap CI ", ci_text(rq1_summary$lo[2], rq1_summary$hi[2], signed = TRUE)),
    P = p_text(rq1_summary$p[2]),
    Holm_P = "--"
  ),
  tibble(
    Section = "Primary",
    Quantity = "Change in positive ratings",
    Estimate = paste0(signed_num(100 * rq1_summary$estimate[3], 0), " percentage points"),
    Interval = paste0("95% participant-bootstrap CI ", ci_text(100 * rq1_summary$lo[3], 100 * rq1_summary$hi[3], digits = 0)),
    P = "--",
    Holm_P = "--"
  )
)

s2_levels <- rq1_level |>
  filter(scope != "Pooled") |>
  transmute(
    Section = "By level",
    Quantity = paste0(spl_db, " dB(A)"),
    Estimate = paste0(signed_num(estimate), " ISO points"),
    Interval = paste0("95% participant-bootstrap CI ", ci_text(boot_lo, boot_hi)),
    P = p_text(p),
    Holm_P = "--"
  )

s2_robust <- rq1_robust |>
  transmute(
    Section = "Robustness",
    Quantity = method,
    Estimate = paste0(signed_num(estimate), " ISO points"),
    Interval = paste0(interval, " ", ci_text(lo, hi)),
    P = "--",
    Holm_P = "--"
  )

s2_screen <- rq1_screen |>
  transmute(
    Section = family,
    Quantity = label,
    Estimate = paste0(signed_num(raw_estimate), " raw; dz ", signed_num(dz)),
    Interval = paste0("95% CI for dz ", ci_text(lo, hi, signed = TRUE)),
    P = p_text(p),
    Holm_P = p_text(p_holm)
  )

s2 <- bind_rows(s2_primary, s2_levels, s2_robust, s2_screen)
write_display_table(s2, "Table_S2_rq1_estimates", c(1.8, 3.8, 3.0, 4.0, 1.1, 1.1))

# Table S3: RQ2 structure, direct tests, sensitivity, and bounded translation.
pca_variance <- read.csv(file.path(LOCK_DIR, "pca_variance.csv"), check.names = FALSE)
pca_loadings <- read.csv(file.path(LOCK_DIR, "pca_loadings.csv"), check.names = FALSE)
rq2_effects <- read.csv(file.path(LOCK_DIR, "rq2_dimension_effects.csv"), check.names = FALSE)
rq2_geometry <- read.csv(file.path(LOCK_DIR, "rq2_geometry.csv"), check.names = FALSE)

loadings_wide <- pca_loadings |>
  filter(component %in% c("PC1", "PC2")) |>
  select(label, component, loading) |>
  pivot_wider(names_from = component, values_from = loading)

s3_structure <- bind_rows(
  tibble(
    Section = "PCA structure",
    Quantity = c("Variance explained by PC1", "Variance explained by PC2"),
    Estimate = paste0(plain_num(100 * pca_variance$variance_proportion[1:2], 1), "%"),
    Interval = "--",
    P = "--"
  ),
  loadings_wide |>
    transmute(
      Section = "PCA loadings",
      Quantity = label,
      Estimate = paste0("PC1 ", signed_num(PC1), "; PC2 ", signed_num(PC2)),
      Interval = "--",
      P = "--"
    )
)

effect_labels <- c(
  PC1 = "Valence-like axis (PC1)",
  PC2 = "Activation-like axis (PC2)",
  PCA_contrast = "Direct PC1-PC2 contrast",
  ISO = "Transparent ISO valence proxy",
  activation_proxy = "Transparent activation proxy",
  transparent_contrast = "Direct transparent-proxy contrast"
)
s3_effects <- rq2_effects |>
  mutate(
    Section = if_else(metric %in% c("PC1", "PC2", "PCA_contrast"), "Effects", "Proxy sensitivity"),
    Quantity = unname(effect_labels[metric])
  ) |>
  transmute(
    Section,
    Quantity,
    Estimate = paste0(signed_num(estimate), " SD"),
    Interval = paste0("95% participant-bootstrap CI ", ci_text(lo, hi, signed = TRUE)),
    P = p_text(p)
  )

geometry_row <- function(metric, section, quantity, unit, multiplier = 1) {
  x <- rq2_geometry[rq2_geometry$metric == metric, ]
  tibble(
    Section = section,
    Quantity = quantity,
    Estimate = paste0(signed_num(multiplier * x$estimate), unit),
    Interval = paste0("95% participant-bootstrap CI ", ci_text(multiplier * x$lo, multiplier * x$hi, signed = TRUE)),
    P = "--"
  )
}
s3_translation <- bind_rows(
  geometry_row("traffic3_valence", "Translation", "+3 dB traffic vector: valence", " SD"),
  geometry_row("traffic3_activation", "Translation", "+3 dB traffic vector: activation", " SD"),
  geometry_row("pc1_water", "Translation", "Stream vector: valence", " SD"),
  geometry_row("pc2_water", "Translation", "Stream vector: activation", " SD"),
  geometry_row("valence_offset_3db", "Translation", "Valence offset of a +3 dB traffic change", "%", multiplier = 100),
  geometry_row("activation_offset_3db", "Translation", "Activation offset of a +3 dB traffic change", "%", multiplier = 100),
  geometry_row("iso_db_credit", "Translation", "ISO valence-equivalent credit", " dB")
)

s3 <- bind_rows(s3_structure, s3_effects, s3_translation)
write_display_table(s3, "Table_S3_rq2_dimensions", c(2.2, 5.2, 3.0, 4.1, 1.1))

# Table S4: RQ3 reach, stability, held-out prediction, and relevant checks.
reach <- read.csv(file.path(LOCK_DIR, "rq3_reach.csv"), check.names = FALSE)
stability <- read.csv(file.path(LOCK_DIR, "rq3_stability.csv"), check.names = FALSE)
prediction <- read.csv(file.path(LOCK_DIR, "rq3_prediction_performance.csv"), check.names = FALSE)
reliability <- read.csv(file.path(LOCK_DIR, "rq3_reliability.csv"), check.names = FALSE)
psychometric <- read.csv(file.path(LOCK_DIR, "rq3_psychometric_checks.csv"), check.names = FALSE)

s4_reach <- tribble(
  ~Section, ~Quantity, ~Estimate, ~Interval, ~P,
  "Reach", "Participants with a positive mean benefit", paste0(reach$positive_mean_n, "/", reach$n, " (", plain_num(100 * reach$positive_mean_prop, 0), "%)"), paste0("Exact 95% CI ", ci_text(100 * reach$binomial_lo, 100 * reach$binomial_hi, 0), "%"), "--",
  "Reach", "Participants positive at three or four levels", paste0(reach$positive_3plus_n, "/", reach$n, " (", plain_num(100 * reach$positive_3plus_prop, 0), "%)"), "--", "--",
  "Reach", "Participants never positive", paste0(reach$never_positive_n, "/", reach$n), "--", "--"
)

s4_stability <- stability |>
  transmute(
    Section = "Stability",
    Quantity = metric,
    Estimate = plain_num(estimate),
    Interval = paste0("95% participant-bootstrap CI ", ci_text(lo, hi, signed = TRUE)),
    P = "--"
  )

prediction_units <- c(rmse = " ISO points", skill = "%")
s4_prediction <- prediction |>
  filter(metric %in% c("rmse", "skill")) |>
  mutate(
    Quantity = if_else(metric == "rmse", paste0(model, ": RMSE"), paste0(model, ": skill versus universal")),
    scale = if_else(metric == "skill", 100, 1),
    suffix = if_else(metric == "skill", "%", " ISO points")
  ) |>
  transmute(
    Section = "Prediction",
    Quantity,
    Estimate = paste0(if_else(metric == "skill", signed_num(scale * estimate, 1), plain_num(estimate, 2)), suffix),
    Interval = paste0("95% participant-bootstrap CI ", ci_text(scale * lo, scale * hi, if_else(metric == "skill", 1, 2), signed = metric == "skill")),
    P = "--"
  )

s4_reliability <- reliability |>
  transmute(
    Section = "Reliability",
    Quantity = paste0(scale, " (", items, " items)"),
    Estimate = paste0("alpha = ", plain_num(alpha)),
    Interval = "--",
    P = "--"
  )

s4_psychometric <- psychometric |>
  transmute(
    Section = "Check",
    Quantity = check,
    Estimate = signed_num(estimate),
    Interval = paste0("95% model CI ", ci_text(lo, hi, signed = TRUE)),
    P = p_text(p)
  )

s4 <- bind_rows(s4_reach, s4_stability, s4_prediction, s4_reliability, s4_psychometric)
write_display_table(s4, "Table_S4_rq3_delivery", c(2.2, 5.3, 2.6, 4.2, 1.1))

# Table S5: physiology calibration. This table is landscape in the SI document.
physiology <- read.csv(file.path(LOCK_DIR, "physiology_calibration.csv"), check.names = FALSE)
s5 <- physiology |>
  transmute(
    Measure = measure,
    Kind = kind,
    N = n,
    Stream_dz = signed_num(water_dz),
    `90%_CI` = ci_text(water_lo90, water_hi90, signed = TRUE),
    `SPL_partial_eta2` = plain_num(spl_partial_eta2, 3),
    `SPL_P` = p_text(spl_p),
    Interpretation = interpretation
  )
write_display_table(s5, "Table_S5_physiology_calibration", c(3.9, 2.4, 0.7, 1.3, 2.3, 1.7, 1.3, 7.0))

message("Locked Tables S1-S5 written to: ", TABLE_DIR)
