# Fixed synthesis for all estimands entering the manuscript or supplementary file.
# This script performs no outcome search. It writes the sole data lock used by plots.

source("code/00_setup.R")

ATTR <- paste0("a", 1:8)
ATTR_LABELS <- c(
  a1 = "Pleasant", a2 = "Noisy/chaotic", a3 = "Lively", a4 = "Bland",
  a5 = "Calm", a6 = "Annoying", a7 = "Immersive", a8 = "Dull"
)

d <- readr::read_csv(DATA_FILE, show_col_types = FALSE) |>
  mutate(
    id = factor(id),
    water_f = factor(water, levels = c(0, 1), labels = c("No stream", "Stream")),
    spl_f = factor(spl_db, levels = c(54, 57, 60, 63)),
    spl_c = spl_db - 60,
    lba_global = log(ba_global),
    lba_prefrontal = log(ba_prefrontal)
  )

stopifnot(nrow(d) == 400, n_distinct(d$id) == 50,
          identical(sort(unique(d$spl_db)), c(54, 57, 60, 63)))

# -----------------------------------------------------------------------------
# Orthogonal attribute dimensions. Signs are fixed to pleasant (PC1) and lively
# (PC2), so higher scores mean more positive valence and greater activation.
pc <- prcomp(d[, ATTR], center = TRUE, scale. = TRUE)
s1 <- sign(pc$rotation["a1", 1])
s2 <- sign(pc$rotation["a3", 2])
rot <- pc$rotation[, 1:2]
rot[, 1] <- rot[, 1] * s1
rot[, 2] <- rot[, 2] * s2

d <- d |>
  mutate(
    PC1 = pc$x[, 1] * s1,
    PC2 = pc$x[, 2] * s2,
    PC1z = as.numeric(scale(PC1)),
    PC2z = as.numeric(scale(PC2)),
    activation = ((a3 + a7) / 2) - ((a4 + a8) / 2),
    activation_z = as.numeric(scale(activation)),
    iso_z = as.numeric(scale(iso_pleasant))
  )

pca_loadings <- as_tibble(rot, rownames = "attribute") |>
  rename(PC1 = 2, PC2 = 3) |>
  mutate(label = unname(ATTR_LABELS[attribute])) |>
  pivot_longer(c(PC1, PC2), names_to = "component", values_to = "loading")

pca_variance <- tibble(
  component = paste0("PC", seq_along(pc$sdev)),
  variance_proportion = pc$sdev^2 / sum(pc$sdev^2)
)

write_csv(pca_loadings, file.path(LOCK_DIR, "pca_loadings.csv"))
write_csv(pca_variance, file.path(LOCK_DIR, "pca_variance.csv"))

# Compact, deidentified plot data. The source table remains the analysis authority.
d |>
  transmute(
    participant = as.integer(id), spl_db, water, water_label = water_f,
    iso_pleasant, e, f, across(all_of(ATTR)), PC1z, PC2z, activation_z, iso_z
  ) |>
  write_csv(file.path(LOCK_DIR, "condition_plot_data.csv"))

# -----------------------------------------------------------------------------
# RQ1: primary pooled effect, exposure pattern, interaction and positive ratings.
m_iso_cat <- lmm(iso_pleasant ~ spl_f * water_f + (1 | id), d)
m_iso_cont <- lmm(iso_pleasant ~ spl_c * water_f + (1 | id), d)

cell_emm <- as.data.frame(confint(emmeans(m_iso_cat, ~ spl_f * water_f))) |>
  as_tibble() |>
  transmute(
    spl_db = as.numeric(as.character(spl_f)),
    water_label = as.character(water_f), estimate = emmean,
    se = SE, lo = lower.CL, hi = upper.CL
  )

pooled_emm <- as.data.frame(summary(
  pairs(emmeans(m_iso_cat, ~ water_f), reverse = TRUE), infer = c(TRUE, TRUE)
)) |>
  as_tibble()

level_emm <- as.data.frame(summary(
  pairs(emmeans(m_iso_cat, ~ water_f | spl_f), reverse = TRUE), infer = c(TRUE, TRUE)
)) |>
  as_tibble() |>
  transmute(
    scope = as.character(spl_f), spl_db = as.numeric(as.character(spl_f)),
    estimate, se = SE, df, lo = lower.CL, hi = upper.CL,
    t = t.ratio, p = p.value
  )

pair_iso <- d |>
  select(id, spl_db, spl_c, water, iso_pleasant) |>
  pivot_wider(names_from = water, values_from = iso_pleasant, names_prefix = "w") |>
  mutate(diff = w1 - w0)

iso_by_id <- pair_iso |>
  group_by(id) |>
  summarise(
    mean_diff = mean(diff),
    interaction_slope = coef(lm(diff ~ spl_c))["spl_c"],
    .groups = "drop"
  )

boot_idx <- replicate(B, sample(seq_len(nrow(iso_by_id)), replace = TRUE))
boot_pool <- apply(boot_idx, 2, function(i) mean(iso_by_id$mean_diff[i]))
boot_interaction <- apply(boot_idx, 2, function(i) mean(iso_by_id$interaction_slope[i]))
pool_boot_ci <- q_ci(boot_pool)
interaction_boot_ci <- q_ci(boot_interaction)

positive_pair <- d |>
  transmute(id, spl_db, water, positive = as.integer(iso_pleasant > 0)) |>
  pivot_wider(names_from = water, values_from = positive, names_prefix = "w") |>
  mutate(change = w1 - w0)

positive_by_id <- positive_pair |>
  group_by(id) |>
  summarise(no_stream = mean(w0), stream = mean(w1), change = mean(change), .groups = "drop")
boot_positive <- apply(boot_idx, 2, function(i) mean(positive_by_id$change[i]))
positive_ci <- q_ci(boot_positive)

level_boot <- map_dfr(c(54, 57, 60, 63), function(level) {
  x <- pair_iso |> filter(spl_db == level) |> pull(diff)
  bs <- replicate(B, mean(sample(x, replace = TRUE)))
  tibble(spl_db = level, boot_lo = q_ci(bs)[1], boot_hi = q_ci(bs)[2])
})

pooled_row <- tibble(
  scope = "Pooled", spl_db = NA_real_, estimate = pooled_emm$estimate,
  se = pooled_emm$SE, df = pooled_emm$df,
  lo = pooled_emm$lower.CL, hi = pooled_emm$upper.CL,
  t = pooled_emm$t.ratio, p = pooled_emm$p.value,
  boot_lo = pool_boot_ci[1], boot_hi = pool_boot_ci[2]
)

rq1_effects <- bind_rows(
  level_emm |> left_join(level_boot, by = "spl_db"),
  pooled_row
) |>
  mutate(scope = factor(scope, levels = c("54", "57", "60", "63", "Pooled")))

co_cont <- summary(m_iso_cont)$coefficients
int_term <- "spl_c:water_fStream"
int_wald <- co_cont[int_term, "Estimate"] + c(-1, 1) *
  qt(0.975, df = co_cont[int_term, "df"]) * co_cont[int_term, "Std. Error"]

rq1_summary <- tibble::tribble(
  ~metric, ~estimate, ~lo, ~hi, ~p, ~units,
  "Pooled stream effect", pooled_emm$estimate, pool_boot_ci[1], pool_boot_ci[2], pooled_emm$p.value, "ISO pleasantness points",
  "Stream-by-SPL linear interaction", co_cont[int_term, "Estimate"], interaction_boot_ci[1], interaction_boot_ci[2], co_cont[int_term, "Pr(>|t|)"], "points per dB",
  "Positive-rating change", mean(positive_by_id$change), positive_ci[1], positive_ci[2], NA_real_, "proportion"
)

positive_summary <- tibble(
  condition = c("No stream", "Stream"),
  proportion = c(mean(positive_by_id$no_stream), mean(positive_by_id$stream))
)

write_csv(cell_emm, file.path(LOCK_DIR, "rq1_cell_estimates.csv"))
write_csv(rq1_effects, file.path(LOCK_DIR, "rq1_effects_by_spl.csv"))
write_csv(rq1_summary, file.path(LOCK_DIR, "rq1_summary.csv"))
write_csv(positive_summary, file.path(LOCK_DIR, "rq1_positive_summary.csv"))

# Outcome-family screen, with one Holm adjustment across the frozen 11 outcomes.
screen_vars <- c("iso_pleasant", "e", "f", ATTR)
screen_labels <- c(
  iso_pleasant = "ISO pleasantness", e = "Annoying - pleasant", f = "Noisy - calm",
  a1 = "Pleasant", a2 = "Noisy/chaotic", a3 = "Lively", a4 = "Bland",
  a5 = "Calm", a6 = "Annoying", a7 = "Immersive", a8 = "Dull"
)

outcome_screen <- map_dfr(screen_vars, function(y) {
  dd <- d |>
    select(id, spl_db, water, value = all_of(y)) |>
    pivot_wider(names_from = water, values_from = value, names_prefix = "w") |>
    mutate(diff = w1 - w0) |>
    group_by(id) |>
    summarise(diff = mean(diff), .groups = "drop") |>
    pull(diff)
  dz <- mean(dd) / sd(dd)
  dz_boot <- replicate(B, mean(sample(dd, replace = TRUE)) / sd(dd))
  mm <- lmm(reformulate("spl_f * water_f + (1 | id)", response = y), d)
  cc <- as.data.frame(pairs(emmeans(mm, ~ water_f), reverse = TRUE))
  tibble(
    outcome = y, label = unname(screen_labels[y]), raw_estimate = cc$estimate,
    dz = dz, lo = q_ci(dz_boot)[1], hi = q_ci(dz_boot)[2], p = cc$p.value
  )
}) |>
  mutate(
    p_holm = p.adjust(p, method = "holm"),
    family = case_when(
      outcome == "iso_pleasant" ~ "Primary",
      outcome %in% c("e", "f") ~ "Convergent",
      TRUE ~ "Attribute"
    ),
    holm_significant = p_holm < 0.05
  )

write_csv(outcome_screen, file.path(LOCK_DIR, "rq1_outcome_screen.csv"))

# Robust location summaries for the same participant-level pooled contrast.
x_iso <- iso_by_id$mean_diff
boot_location <- replicate(B, {
  s <- sample(x_iso, replace = TRUE)
  c(mean = mean(s), median = median(s), trimmed = mean(s, trim = 0.2))
}) |>
  t()
loo <- map_dbl(seq_along(x_iso), ~mean(x_iso[-.x]))
rq1_robustness <- tibble(
  method = c("Participant mean", "Median", "20% trimmed mean", "Leave-one-out mean"),
  estimate = c(mean(x_iso), median(x_iso), mean(x_iso, trim = 0.2), mean(x_iso)),
  lo = c(q_ci(boot_location[, "mean"])[1], q_ci(boot_location[, "median"])[1],
         q_ci(boot_location[, "trimmed"])[1], min(loo)),
  hi = c(q_ci(boot_location[, "mean"])[2], q_ci(boot_location[, "median"])[2],
         q_ci(boot_location[, "trimmed"])[2], max(loo)),
  interval = c("95% bootstrap CI", "95% bootstrap CI", "95% bootstrap CI", "range")
)
write_csv(rq1_robustness, file.path(LOCK_DIR, "rq1_robustness.csv"))

# -----------------------------------------------------------------------------
# RQ2: direct orthogonal-dimension contrast and bounded design translation.
dim_pair <- d |>
  select(id, spl_db, water, PC1z, PC2z, activation_z, iso_z) |>
  pivot_wider(names_from = water, values_from = c(PC1z, PC2z, activation_z, iso_z)) |>
  mutate(
    d_pc1 = PC1z_1 - PC1z_0,
    d_pc2 = PC2z_1 - PC2z_0,
    d_activation = activation_z_1 - activation_z_0,
    d_iso = iso_z_1 - iso_z_0
  ) |>
  group_by(id) |>
  summarise(across(starts_with("d_"), mean), .groups = "drop")

boot_dim_idx <- replicate(B, sample(seq_len(nrow(dim_pair)), replace = TRUE))
dim_boot <- apply(boot_dim_idx, 2, function(i) {
  z <- dim_pair[i, ]
  c(
    PC1 = mean(z$d_pc1), PC2 = mean(z$d_pc2),
    PCA_contrast = mean(z$d_pc1 - z$d_pc2),
    ISO = mean(z$d_iso), activation_proxy = mean(z$d_activation),
    transparent_contrast = mean(z$d_iso - z$d_activation)
  )
}) |>
  t()

dim_points <- c(
  PC1 = mean(dim_pair$d_pc1), PC2 = mean(dim_pair$d_pc2),
  PCA_contrast = mean(dim_pair$d_pc1 - dim_pair$d_pc2),
  ISO = mean(dim_pair$d_iso), activation_proxy = mean(dim_pair$d_activation),
  transparent_contrast = mean(dim_pair$d_iso - dim_pair$d_activation)
)

dimension_effects <- tibble(
  metric = names(dim_points), estimate = as.numeric(dim_points),
  lo = apply(dim_boot, 2, q_ci)[1, ], hi = apply(dim_boot, 2, q_ci)[2, ],
  p = c(
    t.test(dim_pair$d_pc1)$p.value, t.test(dim_pair$d_pc2)$p.value,
    t.test(dim_pair$d_pc1 - dim_pair$d_pc2)$p.value,
    t.test(dim_pair$d_iso)$p.value, t.test(dim_pair$d_activation)$p.value,
    t.test(dim_pair$d_iso - dim_pair$d_activation)$p.value
  )
)
write_csv(dimension_effects, file.path(LOCK_DIR, "rq2_dimension_effects.csv"))

# Balanced within-person coefficients for response geometry.
participant_coefficients <- function(data, y) {
  data |>
    select(id, spl_db, spl_c, water, value = all_of(y)) |>
    group_by(id) |>
    summarise(
      water_effect = mean(value[water == 1]) - mean(value[water == 0]),
      slope = coef(lm(value ~ spl_c + water))["spl_c"],
      .groups = "drop"
    )
}

coef_iso <- participant_coefficients(d, "iso_pleasant") |> rename(iso_water = water_effect, iso_slope = slope)
coef_pc1 <- participant_coefficients(d, "PC1z") |> rename(pc1_water = water_effect, pc1_slope = slope)
coef_pc2 <- participant_coefficients(d, "PC2z") |> rename(pc2_water = water_effect, pc2_slope = slope)
coef_all <- coef_iso |>
  left_join(coef_pc1, by = "id") |>
  left_join(coef_pc2, by = "id")

extract_geometry <- function(z) {
  b <- colMeans(z |> select(-id))
  c(
    iso_water = unname(b["iso_water"]), iso_per_db = unname(b["iso_slope"]),
    iso_db_credit = unname(b["iso_water"] / -b["iso_slope"]),
    pc1_water = unname(b["pc1_water"]), pc1_per_db = unname(b["pc1_slope"]),
    pc2_water = unname(b["pc2_water"]), pc2_per_db = unname(b["pc2_slope"]),
    traffic3_valence = unname(3 * b["pc1_slope"]),
    traffic3_activation = unname(3 * b["pc2_slope"]),
    valence_offset_3db = unname(b["pc1_water"] / (-3 * b["pc1_slope"])),
    activation_offset_3db = unname(b["pc2_water"] / (-3 * b["pc2_slope"]))
  )
}

geometry_point <- extract_geometry(coef_all)
geometry_boot <- apply(boot_dim_idx, 2, function(i) extract_geometry(coef_all[i, ])) |>
  t()
geometry <- tibble(
  metric = names(geometry_point), estimate = as.numeric(geometry_point),
  lo = apply(geometry_boot, 2, q_ci)[1, ],
  hi = apply(geometry_boot, 2, q_ci)[2, ]
)
write_csv(geometry, file.path(LOCK_DIR, "rq2_geometry.csv"))

# -----------------------------------------------------------------------------
# RQ3: reach, cross-level stability and genuinely held-out prediction.
benefit <- pair_iso |>
  select(id, spl_db, diff) |>
  group_by(id) |>
  mutate(mean_benefit = mean(diff), positive_levels = sum(diff > 0)) |>
  ungroup()

coverage <- benefit |>
  distinct(id, mean_benefit, positive_levels) |>
  arrange(desc(mean_benefit)) |>
  mutate(rank = row_number(), positive_mean = mean_benefit > 0)

reach_test <- binom.test(sum(coverage$positive_mean), nrow(coverage))
reach <- tibble(
  n = nrow(coverage), positive_mean_n = sum(coverage$positive_mean),
  positive_mean_prop = mean(coverage$positive_mean),
  binomial_lo = reach_test$conf.int[1], binomial_hi = reach_test$conf.int[2],
  positive_3plus_n = sum(coverage$positive_levels >= 3),
  positive_3plus_prop = mean(coverage$positive_levels >= 3),
  never_positive_n = sum(coverage$positive_levels == 0)
)
write_csv(coverage, file.path(LOCK_DIR, "rq3_coverage.csv"))
write_csv(reach, file.path(LOCK_DIR, "rq3_reach.csv"))

wide_benefit <- benefit |>
  select(id, spl_db, diff) |>
  pivot_wider(names_from = spl_db, values_from = diff) |>
  arrange(as.integer(id))
M <- as.matrix(wide_benefit |> select(`54`, `57`, `60`, `63`))

icc_mom <- function(mat) {
  mat <- sweep(mat, 2, colMeans(mat), "-")
  n <- nrow(mat); k <- ncol(mat)
  rm <- rowMeans(mat); gm <- mean(mat)
  ms_between <- k * sum((rm - gm)^2) / (n - 1)
  ms_within <- sum((mat - rm)^2) / (n * (k - 1))
  (ms_between - ms_within) / (ms_between + (k - 1) * ms_within)
}

split_defs <- list(
  "Adjacent halves" = list(c("54", "57"), c("60", "63")),
  "Interleaved halves" = list(c("54", "60"), c("57", "63")),
  "Outer/middle halves" = list(c("54", "63"), c("57", "60"))
)

stability_point <- c(
  "Single-level ICC" = icc_mom(M),
  map_dbl(split_defs, function(s) {
    cor(rowMeans(M[, s[[1]], drop = FALSE]),
        rowMeans(M[, s[[2]], drop = FALSE]), method = "spearman")
  })
)

stability_boot <- replicate(B, {
  i <- sample(seq_len(nrow(M)), replace = TRUE)
  Mb <- M[i, , drop = FALSE]
  c(
    "Single-level ICC" = icc_mom(Mb),
    map_dbl(split_defs, function(s) {
      suppressWarnings(cor(rowMeans(Mb[, s[[1]], drop = FALSE]),
                           rowMeans(Mb[, s[[2]], drop = FALSE]), method = "spearman"))
    })
  )
}) |>
  t()

stability <- tibble(
  metric = names(stability_point), estimate = as.numeric(stability_point),
  lo = apply(stability_boot, 2, q_ci)[1, ],
  hi = apply(stability_boot, 2, q_ci)[2, ]
)
write_csv(stability, file.path(LOCK_DIR, "rq3_stability.csv"))

pairwise_defs <- combn(colnames(M), 2, simplify = FALSE)
pairwise_corr <- map_dfr(pairwise_defs, function(z) {
  point <- cor(M[, z[1]], M[, z[2]], method = "spearman")
  bs <- replicate(B, {
    i <- sample(seq_len(nrow(M)), replace = TRUE)
    suppressWarnings(cor(M[i, z[1]], M[i, z[2]], method = "spearman"))
  })
  tibble(level_1 = as.numeric(z[1]), level_2 = as.numeric(z[2]),
         rho = point, lo = q_ci(bs)[1], hi = q_ci(bs)[2])
})
write_csv(pairwise_corr, file.path(LOCK_DIR, "rq3_pairwise_correlations.csv"))

# Leave-one-condition-out personal-trial predictions.
pair_dim <- d |>
  select(id, spl_db, spl_f, water, iso_pleasant, PC2,
         noise_sens, who5, vr_comfort, age, gender) |>
  pivot_wider(names_from = water, values_from = c(iso_pleasant, PC2)) |>
  mutate(diff = iso_pleasant_1 - iso_pleasant_0)

trial_pred <- map_dfr(seq_len(nrow(pair_dim)), function(j) {
  tr <- pair_dim[-j, ]
  te <- pair_dim[j, ]
  universal <- mean(tr$diff[tr$spl_db == te$spl_db])
  mm <- lmer(
    diff ~ spl_f + (1 | id), data = tr,
    control = lmerControl(calc.derivs = FALSE)
  )
  tibble(
    id = te$id, spl_db = te$spl_db, observed = te$diff,
    predicted = as.numeric(predict(mm, newdata = te, allow.new.levels = TRUE)),
    universal = universal, model = "Personal trial"
  )
})

# Trait/state predictors never see the held-out water response. Baseline state uses
# no-stream observations at the other three levels only.
target <- pair_dim |>
  group_by(id) |>
  mutate(
    baseline_iso_other = (sum(iso_pleasant_0) - iso_pleasant_0) / 3,
    baseline_activation_other = (sum(PC2_0) - PC2_0) / 3
  ) |>
  ungroup()

forms <- list(
  "Traits" = diff ~ noise_sens + who5 + vr_comfort + age + factor(gender),
  "Baseline state" = diff ~ baseline_iso_other + baseline_activation_other,
  "Combined profile" = diff ~ noise_sens + who5 + vr_comfort + age + factor(gender) +
    baseline_iso_other + baseline_activation_other
)

profile_pred <- map_dfr(seq_len(nrow(target)), function(j) {
  te <- target[j, ]
  tr <- target |> filter(spl_db == te$spl_db, id != te$id)
  universal <- mean(tr$diff)
  imap_dfr(forms, function(fm, nm) {
    tibble(
      id = te$id, spl_db = te$spl_db, observed = te$diff,
      predicted = as.numeric(predict(lm(fm, data = tr), newdata = te)),
      universal = universal, model = nm
    )
  })
})

all_predictions <- bind_rows(trial_pred, profile_pred) |>
  mutate(model = factor(model, levels = c("Personal trial", "Traits", "Baseline state", "Combined profile")))
write_csv(all_predictions, file.path(LOCK_DIR, "rq3_predictions.csv"))

performance <- function(z, w = rep(1, nrow(z))) {
  err <- z$observed - z$predicted
  err_u <- z$observed - z$universal
  c(
    rmse = sqrt(weighted.mean(err^2, w)), mae = weighted.mean(abs(err), w),
    skill = 1 - sum(w * err^2) / sum(w * err_u^2),
    rho = suppressWarnings(cor(z$observed[rep(seq_len(nrow(z)), w)],
                               z$predicted[rep(seq_len(nrow(z)), w)], method = "spearman"))
  )
}

prediction_performance <- map_dfr(levels(all_predictions$model), function(nm) {
  z <- all_predictions |> filter(model == nm)
  point <- performance(z)
  ids <- unique(z$id)
  bs <- replicate(B, {
    counts <- tabulate(sample(seq_along(ids), length(ids), replace = TRUE),
                       nbins = length(ids))
    performance(z, counts[match(z$id, ids)])
  }) |>
    t()
  tibble(
    model = nm, metric = names(point), estimate = as.numeric(point),
    lo = apply(bs, 2, q_ci)[1, ], hi = apply(bs, 2, q_ci)[2, ]
  )
})
write_csv(prediction_performance, file.path(LOCK_DIR, "rq3_prediction_performance.csv"))

prediction_by_spl <- all_predictions |>
  group_by(model, spl_db) |>
  summarise(
    n = n(), rmse = sqrt(mean((observed - predicted)^2)),
    skill = 1 - sum((observed - predicted)^2) / sum((observed - universal)^2),
    rho = suppressWarnings(cor(observed, predicted, method = "spearman")),
    .groups = "drop"
  )
write_csv(prediction_by_spl, file.path(LOCK_DIR, "rq3_prediction_by_spl.csv"))

# Minimal psychometric calibration for predictors used in the targeting models.
subj <- d |>
  distinct(id, b1, b2, b3, b4, b5, c1, c2, c3, c4, c5, d1, d2, d3, d4, d5, d6,
           noise_sens, who5, vr_comfort)
reliability <- tibble(
  scale = c("Noise sensitivity", "WHO-5 wellbeing", "VR comfort"),
  items = c(5, 5, 6),
  alpha = c(
    alpha_manual(subj |> transmute(b1, b2 = -b2, b3, b4, b5)),
    alpha_manual(subj |> select(c1:c5)),
    alpha_manual(subj |> select(d1:d6))
  )
)

d_psych <- d |> mutate(noise_z = as.numeric(scale(noise_sens)))
m_criterion <- lmm(e ~ spl_f + water_f + noise_z + (1 | id), d_psych)
m_moderation <- lmm(iso_pleasant ~ spl_f + water_f * noise_z + (1 | id), d_psych)
criterion_co <- summary(m_criterion)$coefficients["noise_z", ]
moderation_co <- summary(m_moderation)$coefficients["water_fStream:noise_z", ]
psychometric_checks <- tibble(
  check = c("Noise sensitivity predicts annoying-pleasant contrast",
            "Noise sensitivity moderates stream benefit"),
  estimate = c(criterion_co["Estimate"], moderation_co["Estimate"]),
  se = c(criterion_co["Std. Error"], moderation_co["Std. Error"]),
  lo = estimate - 1.96 * se, hi = estimate + 1.96 * se,
  p = c(criterion_co["Pr(>|t|)"], moderation_co["Pr(>|t|)"])
)
write_csv(reliability, file.path(LOCK_DIR, "rq3_reliability.csv"))
write_csv(psychometric_checks, file.path(LOCK_DIR, "rq3_psychometric_checks.csv"))

# -----------------------------------------------------------------------------
# Outcome-reporting transparency: calibration of the physiology nulls.
marker_defs <- tribble(
  ~variable, ~measure, ~kind,
  "iso_pleasant", "Subjective valence (ISO pleasantness)", "Subjective reference",
  "PC2", "Subjective activation (PC2)", "Subjective reference",
  "ms", "EEG mental stress", "EEG",
  "lba_global", "EEG log beta/alpha, global", "EEG",
  "lba_prefrontal", "EEG log beta/alpha, prefrontal", "EEG",
  "ta_global", "EEG theta/alpha, global", "EEG",
  "eda_slope", "EDA arousal slope", "EDA",
  "eda_scl_stim", "EDA stimulus SCL", "EDA"
)

physiology <- pmap_dfr(marker_defs, function(variable, measure, kind) {
  dd <- d |> filter(!is.na(.data[[variable]]))
  x <- dd |>
    select(id, spl_db, water, value = all_of(variable)) |>
    pivot_wider(names_from = water, values_from = value, names_prefix = "w") |>
    mutate(diff = w1 - w0) |>
    group_by(id) |>
    summarise(diff = mean(diff), .groups = "drop") |>
    pull(diff)
  dz <- mean(x) / sd(x)
  dz_bs <- replicate(B, mean(sample(x, replace = TRUE)) / sd(x))
  mm <- lmm(reformulate("spl_f + water_f + (1 | id)", response = variable), dd)
  aa <- anova(mm)
  Fspl <- aa["spl_f", "F value"]
  df1 <- aa["spl_f", "NumDF"]
  df2 <- aa["spl_f", "DenDF"]
  peta <- (Fspl * df1) / (Fspl * df1 + df2)
  tibble(
    measure, kind, n = length(x), water_dz = dz,
    water_lo90 = q_ci(dz_bs, 0.90)[1], water_hi90 = q_ci(dz_bs, 0.90)[2],
    spl_p = aa["spl_f", "Pr(>F)"], spl_partial_eta2 = peta,
    interpretation = case_when(
      kind == "Subjective reference" & spl_p < 0.05 & variable == "iso_pleasant" ~ "Sensitive to SPL; responds to the stream",
      kind == "Subjective reference" & spl_p < 0.05 ~ "Sensitive to SPL; stream estimate near zero",
      TRUE ~ "Not sensitive to SPL here, so the stream estimate cannot inform mechanism"
    )
  )
})
write_csv(physiology, file.path(LOCK_DIR, "physiology_calibration.csv"))

# -----------------------------------------------------------------------------
# Frozen multiplicity/estimand map and manuscript macros.
multiplicity_map <- tribble(
  ~rq, ~role, ~family, ~inferential_rule,
  "RQ1", "Primary", "ISO pleasantness", "Single primary outcome; pooled mixed-model contrast with participant bootstrap CI",
  "RQ1", "Convergent/screened", "e, f and eight attributes", "Holm adjustment across all 11 subjective outcomes; full screen in SI only",
  "RQ1", "Level dependence", "One continuous interaction", "Estimate and participant-bootstrap CI; no four-level discovery claims",
  "RQ2", "Dimension contrast", "PC1 versus PC2", "Direct within-participant standardised contrast with participant bootstrap CI",
  "RQ2", "Sensitivity", "Transparent activation composite", "Reported in SI to delimit the orthogonal-axis interpretation",
  "RQ3", "Reach", "Positive participant means", "Exact binomial CI",
  "RQ3", "Targeting", "Four held-out strategies", "Skill versus universal mean with participant-cluster bootstrap CIs",
  "Supporting", "Physiology", "Prespecified compact marker set", "90% effect intervals plus SPL positive-control sensitivity; no mechanism claim"
)
write_csv(multiplicity_map, file.path(LOCK_DIR, "multiplicity_map.csv"))

get_geom <- function(metric) geometry |> filter(.data$metric == .env$metric)
get_dim <- function(metric) dimension_effects |> filter(.data$metric == .env$metric)
get_skill <- function(model) prediction_performance |>
  filter(.data$model == .env$model, .data$metric == "skill")

macro_lines <- c(
  "% Generated by R/01_synthesis_lock.R. Do not edit manually.",
  sprintf("\\newcommand{\\PooledBenefit}{%.2f}", pooled_emm$estimate),
  sprintf("\\newcommand{\\PooledBenefitLo}{%.2f}", pool_boot_ci[1]),
  sprintf("\\newcommand{\\PooledBenefitHi}{%.2f}", pool_boot_ci[2]),
  sprintf("\\newcommand{\\PooledBenefitP}{%s}", fmt_p(pooled_emm$p.value)),
  sprintf("\\newcommand{\\InteractionBeta}{%+.2f}", co_cont[int_term, "Estimate"]),
  sprintf("\\newcommand{\\InteractionLo}{%+.2f}", interaction_boot_ci[1]),
  sprintf("\\newcommand{\\InteractionHi}{%+.2f}", interaction_boot_ci[2]),
  sprintf("\\newcommand{\\PositiveNoStream}{%.0f\\%%}", 100 * mean(positive_by_id$no_stream)),
  sprintf("\\newcommand{\\PositiveStream}{%.0f\\%%}", 100 * mean(positive_by_id$stream)),
  sprintf("\\newcommand{\\PositiveChange}{%.0f}", 100 * mean(positive_by_id$change)),
  sprintf("\\newcommand{\\PCOneEffect}{%.2f}", get_dim("PC1")$estimate),
  sprintf("\\newcommand{\\PCTwoEffect}{%.2f}", get_dim("PC2")$estimate),
  sprintf("\\newcommand{\\DimensionContrast}{%.2f}", get_dim("PCA_contrast")$estimate),
  sprintf("\\newcommand{\\ValenceOffset}{%.0f\\%%}", 100 * get_geom("valence_offset_3db")$estimate),
  sprintf("\\newcommand{\\ValenceCredit}{%.2f}", get_geom("iso_db_credit")$estimate),
  sprintf("\\newcommand{\\ReachN}{%d}", reach$positive_mean_n),
  sprintf("\\newcommand{\\ReachPct}{%.0f\\%%}", 100 * reach$positive_mean_prop),
  sprintf("\\newcommand{\\ReachLo}{%.0f\\%%}", 100 * reach$binomial_lo),
  sprintf("\\newcommand{\\ReachHi}{%.0f\\%%}", 100 * reach$binomial_hi),
  sprintf("\\newcommand{\\ICC}{%.2f}", stability |> filter(metric == "Single-level ICC") |> pull(estimate)),
  sprintf("\\newcommand{\\PersonalSkill}{%+.0f\\%%}", 100 * get_skill("Personal trial")$estimate),
  sprintf("\\newcommand{\\TraitSkill}{%+.0f\\%%}", 100 * get_skill("Traits")$estimate),
  sprintf("\\newcommand{\\BaselineSkill}{%+.0f\\%%}", 100 * get_skill("Baseline state")$estimate),
  sprintf("\\newcommand{\\CombinedSkill}{%+.0f\\%%}", 100 * get_skill("Combined profile")$estimate)
)
writeLines(macro_lines, file.path(LOCK_DIR, "results_macros.tex"))

# Hard checks prevent silent drift of the paper's three answers.
checks <- c(
  pooled_effect = pooled_emm$estimate,
  positive_rating_change = mean(positive_by_id$change),
  pca_direct_contrast = get_dim("PCA_contrast")$estimate,
  positive_mean_n = reach$positive_mean_n,
  icc = stability |> filter(metric == "Single-level ICC") |> pull(estimate),
  personal_trial_skill = get_skill("Personal trial")$estimate,
  traits_skill = get_skill("Traits")$estimate,
  combined_skill = get_skill("Combined profile")$estimate
)
stopifnot(
  abs(checks["pooled_effect"] - 1.836449) < 1e-5,
  abs(checks["positive_rating_change"] - 0.12) < 1e-8,
  checks["positive_mean_n"] == 38,
  checks["pca_direct_contrast"] > 0,
  checks["traits_skill"] < 0,
  checks["combined_skill"] < 0
)
writeLines(
  c(
    "P17 synthesis lock passed.",
    paste0("Seed: ", SEED), paste0("Participant bootstrap resamples: ", B),
    capture.output(print(round(checks, 5)))
  ),
  file.path(LOCK_DIR, "LOCK_CHECKS.txt")
)
writeLines(capture.output(sessionInfo()), file.path(LOCK_DIR, "sessionInfo.txt"))

message("P17 synthesis lock complete.")
