# Four locked main figures for the journal-neutral manuscript.

source("code/00_setup.R")

plot_data <- read_csv(file.path(LOCK_DIR, "condition_plot_data.csv"), show_col_types = FALSE)

# -----------------------------------------------------------------------------
# Figure 1: design problem and controlled experiment (schematic, not a site photo).
box_layer <- function(xmin, xmax, ymin, ymax, fill, colour = COL_DARK) {
  annotate("rect", xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
           fill = fill, colour = colour, linewidth = 0.45)
}

p1a <- ggplot() +
  box_layer(0.2, 2.7, 6.5, 8.8, "#FBE7DF", COL_VERMILION) +
  box_layer(3.8, 6.3, 6.5, 8.8, COL_PALE, COL_DARK) +
  box_layer(7.4, 9.8, 6.5, 8.8, "#E8E8E8", COL_MID) +
  box_layer(3.8, 6.3, 1.6, 3.9, "#E2F2FA", COL_BLUE) +
  box_layer(7.4, 9.8, 1.6, 3.9, "#E2F3ED", COL_GREEN) +
  annotate("segment", x = 2.75, xend = 3.65, y = 7.65, yend = 7.65,
           arrow = arrow(length = unit(2.2, "mm")), linewidth = 0.55) +
  annotate("segment", x = 6.35, xend = 7.25, y = 7.65, yend = 7.65,
           arrow = arrow(length = unit(2.2, "mm")), linewidth = 0.55) +
  annotate("segment", x = 8.6, xend = 8.6, y = 6.35, yend = 4.05,
           arrow = arrow(length = unit(2.2, "mm")), linewidth = 0.55) +
  annotate("segment", x = 6.35, xend = 7.25, y = 2.75, yend = 2.75,
           arrow = arrow(length = unit(2.2, "mm")), linewidth = 0.55,
           colour = COL_BLUE) +
  annotate("text", x = 1.45, y = 7.65, label = "Traffic-sound\nload",
           family = "Arial", size = 2.8, fontface = "bold", colour = COL_VERMILION) +
  annotate("text", x = 5.05, y = 7.65, label = "Source/path\ncontrol",
           family = "Arial", size = 2.8, fontface = "bold") +
  annotate("text", x = 8.6, y = 7.65, label = "Residual\ntraffic sound",
           family = "Arial", size = 2.8, colour = COL_MID) +
  annotate("text", x = 5.05, y = 2.75, label = "Stream-sound\naugmentation",
           family = "Arial", size = 2.8, fontface = "bold", colour = COL_BLUE) +
  annotate("text", x = 8.6, y = 2.75, label = "Affective\nappraisal",
           family = "Arial", size = 2.8, fontface = "bold", colour = COL_GREEN) +
  annotate("text", x = 5.05, y = 0.55, label = "Complement, not acoustic substitution",
           family = "Arial", size = 2.55, colour = COL_MID) +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 10), clip = "off") +
  theme_void(base_family = "Arial") + theme(plot.margin = margin(3, 3, 3, 3, "mm"))

# Figure 1b is a schematic (not a photograph): a flat-vector illustration of the
# single held-constant VR square, a VR participant, and the two sound layers --
# traffic and the added stream -- drawn as colour-coded sound waves. Nothing here
# implies a photographed site or a tested built water feature.
circle_df <- function(cx, cy, r, n = 80) {
  t <- seq(0, 2 * pi, length.out = n)
  data.frame(x = cx + r * cos(t), y = cy + r * sin(t))
}
ellipse_df <- function(cx, cy, rx, ry, n = 100) {
  t <- seq(0, 2 * pi, length.out = n)
  data.frame(x = cx + rx * cos(t), y = cy + ry * sin(t))
}
arc_df <- function(cx, cy, r, a0, a1, n = 140) {
  t <- seq(a0, a1, length.out = n) * pi / 180
  data.frame(x = cx + r * cos(t), y = cy + r * sin(t))
}
SKY_TOP <- "#D3E2EE"; SKY_BOT <- "#EEF4F9"
BLD_FAR <- "#C6D2DA"; BLD_NEAR <- "#A7B7C1"; BLD_LINE <- "#B9C7D0"
GROUND <- "#EAE2D5"; PAVING <- "#D7CDBB"
TREE_DK <- "#7FA579"; TREE_LT <- "#9CBE93"; TRUNK <- "#7C5B42"
HEADSET <- "#3E3E3E"; HEADSET_HI <- "#6E6E6E"

# A small deciduous tree drawn as a cluster crown on a trunk.
tree_layers <- function(cx, base_y, r, trunk_h) {
  cy <- base_y + trunk_h
  crown <- list(c(cx, cy + r * 0.35, r * 0.90), c(cx - r * 0.72, cy + r * 0.02, r * 0.62),
                c(cx + r * 0.72, cy + r * 0.02, r * 0.62), c(cx, cy + r * 1.02, r * 0.62))
  c(list(annotate("rect", xmin = cx - r * 0.14, xmax = cx + r * 0.14,
                  ymin = base_y, ymax = cy + r * 0.2, fill = TRUNK, colour = NA)),
    lapply(crown, function(p)
      geom_polygon(data = circle_df(p[1], p[2], p[3]), aes(x, y), fill = TREE_DK, colour = NA)),
    list(geom_polygon(data = circle_df(cx - r * 0.28, cy + r * 0.55, r * 0.42), aes(x, y),
                      fill = TREE_LT, colour = NA)))
}

sky_n <- 44
sky_y <- seq(40, 78, length.out = sky_n + 1)
sky_cols <- colorRampPalette(c(SKY_BOT, SKY_TOP))(sky_n)
sky_layer <- lapply(seq_len(sky_n), function(i)
  annotate("rect", xmin = 2, xmax = 98, ymin = sky_y[i], ymax = sky_y[i + 1],
           fill = sky_cols[i], colour = NA))
far_sky <- data.frame(
  x = c(2, 2, 9, 9, 16, 16, 23, 23, 31, 31, 39, 39, 47, 47, 55, 55, 63, 63,
        71, 71, 79, 79, 87, 87, 94, 94, 98, 98),
  y = c(40, 55, 55, 60, 60, 56, 56, 62, 62, 57, 57, 61, 61, 56, 56, 60, 60, 55,
        55, 61, 61, 57, 57, 62, 62, 57, 57, 40))
near_blocks <- data.frame(
  xmin = c(5, 17, 66, 82), xmax = c(16, 30, 79, 95), ymax = c(58, 67, 64, 56))
storey_lines <- do.call(rbind, lapply(seq_len(nrow(near_blocks)), function(i) {
  ys <- seq(44, near_blocks$ymax[i] - 3, by = 4)
  data.frame(x = near_blocks$xmin[i] + 1, xend = near_blocks$xmax[i] - 1, y = ys, yend = ys)
}))
paving <- data.frame(x = c(6, 20, 34, 50, 66, 80, 94), y = 2, xend = 50, yend = 40)
traffic_src <- c(13, 53); stream_src <- c(84, 11)
traffic_arcs <- do.call(rbind, Map(function(r, g)
  cbind(arc_df(traffic_src[1], traffic_src[2], r, -98, 14), g = g),
  r = c(7, 11.5, 16, 20.5), g = 1:4))
stream_arcs <- do.call(rbind, Map(function(r, g)
  cbind(arc_df(stream_src[1], stream_src[2], r, 122, 220), g = g),
  r = c(6, 10, 14), g = 1:3))
torso <- data.frame(
  x = c(45.5, 44, 42.8, 45, 47.6, 48, 52, 52.4, 55, 57.2, 56, 54.5),
  y = c(5, 10, 14.5, 16.8, 17.9, 18.6, 18.6, 17.9, 16.8, 14.5, 10, 5))
head_c <- circle_df(50, 23, 4.7)

p1b <- ggplot() +
  sky_layer +
  geom_polygon(data = far_sky, aes(x, y), fill = BLD_FAR, colour = NA) +
  lapply(seq_len(nrow(near_blocks)), function(i)
    annotate("rect", xmin = near_blocks$xmin[i], xmax = near_blocks$xmax[i],
             ymin = 40, ymax = near_blocks$ymax[i], fill = BLD_NEAR, colour = NA)) +
  geom_segment(data = storey_lines, aes(x, y, xend = xend, yend = yend),
               colour = BLD_LINE, linewidth = 0.25) +
  annotate("rect", xmin = 2, xmax = 98, ymin = 2, ymax = 40, fill = GROUND, colour = NA) +
  geom_path(data = ellipse_df(50, 12, 30, 6), aes(x, y), colour = PAVING, linewidth = 0.4) +
  geom_segment(data = paving, aes(x, y, xend = xend, yend = yend), colour = PAVING, linewidth = 0.3) +
  tree_layers(15, 6, 7.4, 9) +
  tree_layers(86, 17, 6.0, 8) +
  geom_path(data = traffic_arcs, aes(x, y, group = g, alpha = g), colour = COL_VERMILION,
            linewidth = 0.85, lineend = "round") +
  geom_path(data = stream_arcs, aes(x, y, group = g, alpha = g), colour = COL_BLUE,
            linewidth = 0.85, lineend = "round") +
  scale_alpha_continuous(range = c(0.95, 0.42), guide = "none") +
  geom_polygon(data = ellipse_df(50, 4.4, 8, 1.4), aes(x, y), fill = COL_DARK, alpha = 0.13, colour = NA) +
  geom_polygon(data = torso, aes(x, y), fill = COL_DARK, colour = NA) +
  geom_polygon(data = head_c, aes(x, y), fill = COL_DARK, colour = NA) +
  annotate("rect", xmin = 44.7, xmax = 55.3, ymin = 21.4, ymax = 24.6, fill = HEADSET) +
  annotate("segment", x = 44.7, xend = 55.3, y = 24.4, yend = 24.4, colour = HEADSET_HI, linewidth = 0.4) +
  annotate("rect", xmin = 2, xmax = 98, ymin = 2, ymax = 78, fill = NA, colour = COL_MID, linewidth = 0.5) +
  annotate("text", x = 50, y = 74, label = "VR reconstruction of Zhongshan Square",
           family = "Arial", fontface = "bold", size = 2.9, colour = COL_DARK) +
  annotate("text", x = 50, y = 69.5, label = "visual scene held constant across conditions",
           family = "Arial", size = 2.2, colour = COL_MID) +
  annotate("text", x = 24, y = 61, label = "Traffic sound", hjust = 0,
           family = "Arial", fontface = "bold", size = 2.5, colour = COL_VERMILION) +
  annotate("text", x = 68, y = 31, label = "Stream sound (added)", hjust = 0.5,
           family = "Arial", fontface = "bold", size = 2.5, colour = COL_BLUE) +
  coord_fixed(ratio = 1, xlim = c(0, 100), ylim = c(0, 80), expand = FALSE, clip = "off") +
  theme_void(base_family = "Arial") + theme(plot.margin = margin(3, 3, 3, 3, "mm"))

design_grid <- expand_grid(
  spl = c(54, 57, 60, 63),
  stream = factor(c("Off", "On"), levels = c("Off", "On"))
) |>
  mutate(y = recode(spl, `54` = 4, `57` = 3, `60` = 2, `63` = 1))
p1c <- ggplot(design_grid, aes(stream, y, fill = stream)) +
  geom_tile(colour = "white", linewidth = 1.0, width = 0.88, height = 0.86) +
  geom_text(aes(label = paste0(as.character(spl), " dB(A)")), family = "Arial",
            size = 2.55, colour = COL_DARK) +
  annotate("text", x = 1.5, y = 5.15, label = "N = 50; all eight conditions",
           family = "Arial", size = 2.8, fontface = "bold") +
  annotate("text", x = 1.5, y = 0.15, label = "Within person; Latin-square order",
           family = "Arial", size = 2.45, colour = COL_MID) +
  scale_fill_manual(values = c(Off = "#E3E3E3", On = "#B9DFF2"), guide = "none") +
  scale_x_discrete(position = "top", labels = c("No stream", "Stream")) +
  scale_y_continuous(breaks = NULL) +
  coord_cartesian(ylim = c(-0.5, 5.35), clip = "off") +
  theme_void(base_family = "Arial") +
  theme(
    axis.text.x = element_text(size = 8, face = "bold", colour = COL_DARK,
                               margin = margin(b = 1.5, unit = "mm")),
    plot.margin = margin(3, 3, 3, 3, "mm")
  )

fig1 <- (p1a / (p1b | p1c)) +
  plot_layout(heights = c(0.58, 1)) +
  plot_annotation(tag_levels = "a") & tag_theme
save_figure(fig1, file.path(MAIN_FIG_DIR, "Figure_01_design.png"), 178, 112)

# -----------------------------------------------------------------------------
# Figure 2: average affective benefit across the tested gradient.
cell <- read_csv(file.path(LOCK_DIR, "rq1_cell_estimates.csv"), show_col_types = FALSE)
effects <- read_csv(file.path(LOCK_DIR, "rq1_effects_by_spl.csv"), show_col_types = FALSE)
positive <- read_csv(file.path(LOCK_DIR, "rq1_positive_summary.csv"), show_col_types = FALSE)
rq1 <- read_csv(file.path(LOCK_DIR, "rq1_summary.csv"), show_col_types = FALSE)

plot_data <- plot_data |>
  mutate(water_label = factor(water_label, levels = c("No stream", "Stream")))
cell <- cell |> mutate(water_label = factor(water_label, levels = c("No stream", "Stream")))

p2a <- ggplot() +
  geom_line(
    data = plot_data,
    aes(spl_db, iso_pleasant, group = interaction(participant, water_label), colour = water_label),
    alpha = 0.055, linewidth = 0.25
  ) +
  geom_ribbon(
    data = cell, aes(spl_db, ymin = lo, ymax = hi, fill = water_label, group = water_label),
    alpha = 0.18, colour = NA
  ) +
  geom_line(data = cell, aes(spl_db, estimate, colour = water_label), linewidth = 1.0) +
  geom_point(data = cell, aes(spl_db, estimate, colour = water_label), size = 2.0) +
  geom_hline(yintercept = 0, linewidth = 0.35, linetype = "dashed", colour = COL_MID) +
  scale_colour_manual(values = c("No stream" = COL_MID, "Stream" = COL_BLUE)) +
  scale_fill_manual(values = c("No stream" = COL_MID, "Stream" = COL_BLUE)) +
  scale_x_continuous(breaks = c(54, 57, 60, 63)) +
  scale_y_continuous(breaks = seq(-15, 15, 5), limits = c(-18, 18), expand = expansion(mult = 0.02)) +
  labs(x = "Traffic-sound level, dB(A)", y = "ISO pleasantness") +
  theme_p17() +
  theme(legend.position = "inside", legend.position.inside = c(0.25, 0.12),
        legend.direction = "vertical")

effects_plot <- effects |>
  mutate(
    label = if_else(scope == "Pooled", "Pooled", paste0(scope, " dB(A)")),
    label = factor(label, levels = c("54 dB(A)", "57 dB(A)", "60 dB(A)", "63 dB(A)", "Pooled")),
    plot_lo = if_else(scope == "Pooled", boot_lo, lo),
    plot_hi = if_else(scope == "Pooled", boot_hi, hi),
    pooled = scope == "Pooled"
  )
p2b <- ggplot(effects_plot, aes(estimate, label)) +
  geom_vline(xintercept = 0, linewidth = 0.35, colour = COL_MID) +
  geom_errorbar(aes(xmin = plot_lo, xmax = plot_hi, colour = pooled), orientation = "y", width = 0, linewidth = 0.7) +
  geom_point(aes(colour = pooled, size = pooled), shape = 21, fill = "white", stroke = 0.8) +
  scale_colour_manual(values = c(`FALSE` = COL_MID, `TRUE` = COL_BLUE), guide = "none") +
  scale_size_manual(values = c(`FALSE` = 2.0, `TRUE` = 2.8), guide = "none") +
  scale_x_continuous(breaks = c(-2, 0, 2, 4, 6), limits = c(-2.1, 6.1)) +
  labs(x = "Stream - no stream (ISO points)", y = NULL) +
  theme_p17() +
  theme(axis.text.y = element_text(face = "plain"))

pos_ci <- rq1 |> filter(metric == "Positive-rating change")
p2c <- ggplot(positive, aes(condition, proportion, group = 1)) +
  geom_line(colour = COL_LIGHT, linewidth = 1.2) +
  geom_point(aes(colour = condition), size = 3.2) +
  geom_text(aes(label = percent(proportion, accuracy = 1)), vjust = -1.0,
            family = "Arial", size = 2.8, fontface = "bold") +
  annotate("text", x = 1.5, y = 0.66,
           label = sprintf("+%.0f pp\n95%% CI %.0f to %.0f", 100 * pos_ci$estimate,
                           100 * pos_ci$lo, 100 * pos_ci$hi),
           family = "Arial", size = 2.55, colour = COL_BLUE) +
  scale_colour_manual(values = c("No stream" = COL_MID, "Stream" = COL_BLUE), guide = "none") +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0.35, 0.72),
                     breaks = c(0.4, 0.5, 0.6, 0.7), expand = expansion(mult = c(0, 0.02))) +
  labs(x = NULL, y = "Ratings above neutral") +
  theme_p17() +
  theme(axis.text.x = element_text(face = "bold"))

fig2 <- (p2a | p2b | p2c) +
  plot_layout(widths = c(1.15, 1, 0.72)) +
  plot_annotation(tag_levels = "a") & tag_theme
save_figure(fig2, file.path(MAIN_FIG_DIR, "Figure_02_rq1_affective_benefit.png"), 178, 75)

# -----------------------------------------------------------------------------
# Figure 3: orthogonal valence/activation distinction and bounded translation.
dim_eff <- read_csv(file.path(LOCK_DIR, "rq2_dimension_effects.csv"), show_col_types = FALSE)
geom <- read_csv(file.path(LOCK_DIR, "rq2_geometry.csv"), show_col_types = FALSE)

p3a_data <- dim_eff |>
  filter(metric %in% c("PC1", "PC2", "PCA_contrast")) |>
  mutate(
    label = recode(metric,
                   PC1 = "Valence (PC1)", PC2 = "Activation (PC2)",
                   PCA_contrast = "Direct contrast"),
    label = factor(label, levels = c("Activation (PC2)", "Valence (PC1)", "Direct contrast")),
    kind = if_else(metric == "PCA_contrast", "contrast", "axis")
  )
p3a <- ggplot(p3a_data, aes(estimate, label)) +
  geom_vline(xintercept = 0, colour = COL_MID, linewidth = 0.35) +
  geom_errorbar(aes(xmin = lo, xmax = hi, colour = kind), orientation = "y", width = 0, linewidth = 0.75) +
  geom_point(aes(colour = kind, shape = kind), size = 2.6, stroke = 0.8, fill = "white") +
  scale_colour_manual(values = c(axis = COL_BLUE, contrast = COL_DARK), guide = "none") +
  scale_shape_manual(values = c(axis = 21, contrast = 18), guide = "none") +
  scale_x_continuous(limits = c(-0.22, 0.48), breaks = c(-0.2, 0, 0.2, 0.4)) +
  labs(x = "Standardised stream effect", y = NULL) +
  theme_p17()

gval <- function(metric) geom |> filter(.data$metric == .env$metric) |> pull(estimate)
vector_data <- tibble(
  label = c("+3 dB traffic", "Add stream"),
  x = c(gval("traffic3_valence"), gval("pc1_water")),
  y = c(gval("traffic3_activation"), gval("pc2_water")),
  colour = c("traffic", "stream"),
  # Labels sit clear of, and inside, the panel, each centred above its
  # arrowhead. Anchoring to the arrow tips (as before) pushed "+3 dB " off the
  # left edge and clipped the final letter of "Add stream".
  lx = c(gval("traffic3_valence") + 0.03, gval("pc1_water") + 0.02),
  ly = c(gval("traffic3_activation") + 0.085, gval("pc2_water") + 0.075),
  lhjust = c(0.5, 0.5)
)
p3b <- ggplot(vector_data) +
  geom_hline(yintercept = 0, linewidth = 0.3, linetype = "dashed", colour = COL_MID) +
  geom_vline(xintercept = 0, linewidth = 0.3, linetype = "dashed", colour = COL_MID) +
  geom_segment(aes(x = 0, y = 0, xend = x, yend = y, colour = colour),
               linewidth = 1.1, arrow = arrow(length = unit(2.7, "mm"))) +
  geom_text(aes(x = lx, y = ly, label = label, colour = colour, hjust = lhjust),
            family = "Arial", size = 2.55, vjust = 0) +
  scale_colour_manual(values = c(traffic = COL_VERMILION, stream = COL_BLUE), guide = "none") +
  scale_x_continuous(limits = c(-0.80, 0.60), breaks = c(-0.6, -0.3, 0, 0.3)) +
  scale_y_continuous(limits = c(-0.12, 0.44), breaks = c(-0.1, 0, 0.1, 0.2, 0.3, 0.4)) +
  labs(x = "Valence change (SD)", y = "Activation change (SD)") +
  theme_p17()

offset <- geom |> filter(metric == "valence_offset_3db") |>
  mutate(across(c(estimate, lo, hi), ~100 * .x), label = "Valence loss offset")
p3c <- ggplot(offset, aes(estimate, label)) +
  geom_segment(aes(x = 0, xend = 100, yend = label), colour = COL_LIGHT, linewidth = 2.3) +
  geom_segment(aes(x = 0, xend = estimate, yend = label), colour = COL_BLUE, linewidth = 2.3) +
  geom_errorbar(aes(xmin = lo, xmax = hi), orientation = "y", width = 0.16, linewidth = 0.75, colour = COL_DARK) +
  geom_point(size = 3.0, colour = COL_BLUE) +
  geom_text(aes(label = sprintf("%.0f%%", estimate)), nudge_y = 0.18,
            family = "Arial", size = 3.0, fontface = "bold", colour = COL_BLUE) +
  geom_vline(xintercept = 100, linetype = "dashed", linewidth = 0.35, colour = COL_MID) +
  scale_x_continuous(limits = c(0, 108), breaks = c(0, 25, 50, 75, 100), labels = label_percent(scale = 1)) +
  labs(x = "Share of +3 dB valence loss", y = NULL) +
  theme_p17() + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

fig3 <- (p3a | p3b | p3c) +
  plot_layout(widths = c(1.05, 1.1, 0.85)) +
  plot_annotation(tag_levels = "a") & tag_theme
save_figure(fig3, file.path(MAIN_FIG_DIR, "Figure_03_rq2_dimensions.png"), 178, 72)

# -----------------------------------------------------------------------------
# Figure 4: broad reach and weak basis for selective targeting.
coverage <- read_csv(file.path(LOCK_DIR, "rq3_coverage.csv"), show_col_types = FALSE)
stability <- read_csv(file.path(LOCK_DIR, "rq3_stability.csv"), show_col_types = FALSE)
performance <- read_csv(file.path(LOCK_DIR, "rq3_prediction_performance.csv"), show_col_types = FALSE)

heat <- plot_data |>
  select(participant, spl_db, water, iso_pleasant) |>
  pivot_wider(names_from = water, values_from = iso_pleasant, names_prefix = "w") |>
  mutate(value = w1 - w0) |>
  select(participant, spl_db, value) |>
  left_join(coverage |> transmute(participant = as.integer(str_remove(id, "A-")), rank, mean_benefit), by = "participant") |>
  transmute(rank, column = as.character(spl_db), value)
heat_mean <- coverage |>
  transmute(rank, column = "Mean", value = mean_benefit)
heat <- bind_rows(heat, heat_mean) |>
  mutate(column = factor(column, levels = c("54", "57", "60", "63", "Mean")))

p4a <- ggplot(heat, aes(column, rank, fill = value)) +
  geom_tile(colour = "white", linewidth = 0.15) +
  geom_vline(xintercept = 4.5, colour = COL_DARK, linewidth = 0.45) +
  scale_fill_gradient2(
    low = COL_VERMILION, mid = "white", high = COL_BLUE, midpoint = 0,
    limits = c(-18, 18), oob = squish,
    name = "Stream benefit (ISO points)",
    guide = guide_colourbar(title.position = "top", title.hjust = 0.5)
  ) +
  scale_y_reverse(breaks = c(1, 10, 20, 30, 40, 50), expand = c(0, 0)) +
  labs(x = "Traffic-sound level, dB(A)", y = "Participants (sorted by mean benefit)") +
  theme_p17() +
  theme(legend.position = "bottom", legend.key.width = unit(16, "mm"),
        legend.title = element_text(size = 8),
        axis.ticks.x = element_blank())

stability_plot <- stability |>
  mutate(metric = factor(metric, levels = rev(c("Single-level ICC", "Adjacent halves",
                                                "Interleaved halves", "Outer/middle halves"))))
p4b <- ggplot(stability_plot, aes(estimate, metric)) +
  geom_vline(xintercept = 0, colour = COL_MID, linewidth = 0.35) +
  geom_errorbar(aes(xmin = lo, xmax = hi), orientation = "y", width = 0, colour = COL_MID, linewidth = 0.7) +
  geom_point(size = 2.5, colour = COL_BLUE) +
  scale_x_continuous(limits = c(-0.15, 0.62), breaks = c(-0.1, 0.1, 0.3, 0.5)) +
  labs(x = "Cross-level stability", y = NULL) +
  theme_p17()

skill <- performance |>
  filter(metric == "skill") |>
  mutate(
    model = factor(model, levels = rev(c("Personal trial", "Traits", "Baseline state", "Combined profile"))),
    across(c(estimate, lo, hi), ~100 * .x),
    supported = hi < 0
  )
p4c <- ggplot(skill, aes(estimate, model)) +
  geom_vline(xintercept = 0, colour = COL_DARK, linewidth = 0.45) +
  geom_errorbar(aes(xmin = lo, xmax = hi, colour = supported), orientation = "y", width = 0, linewidth = 0.75) +
  geom_point(aes(colour = supported), size = 2.6) +
  scale_colour_manual(values = c(`FALSE` = COL_BLUE, `TRUE` = COL_VERMILION), guide = "none") +
  scale_x_continuous(limits = c(-45, 28), breaks = c(-40, -20, 0, 20), labels = label_percent(scale = 1)) +
  labs(x = "Held-out skill vs universal mean", y = NULL) +
  theme_p17()

fig4 <- (p4a | (p4b / p4c)) +
  plot_layout(widths = c(1.18, 1)) +
  plot_annotation(tag_levels = "a") & tag_theme
save_figure(fig4, file.path(MAIN_FIG_DIR, "Figure_04_rq3_delivery_strategy.png"), 178, 112)

message("Main figures complete.")
