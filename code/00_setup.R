# Shared setup for the locked P17 writing package.

suppressPackageStartupMessages({
  library(tidyverse)
  library(lme4)
  library(lmerTest)
  library(emmeans)
  library(patchwork)
  library(scales)
  library(grid)
})

emm_options(lmerTest.limit = 5000, pbkrtest.limit = 5000)

# All paths are relative to the repository root. Run every script with the
# working directory set to the repo root (run_all.R does this). load_data.R first
# reads the Mendeley Data workbook and writes the analysis-ready table here.
DATA_FILE <- "intermediate/analysis_wide.csv"
# output/ holds the R-pipeline products: the frozen numeric lock, result macros,
# and the generated table fragments and CSVs. Figures go to figures/ (main) and
# figures/si/ (supplementary).
OUTPUT_DIR <- "output"
LOCK_DIR <- OUTPUT_DIR
TABLE_DIR <- OUTPUT_DIR
MAIN_FIG_DIR <- "figures"
SI_FIG_DIR <- "figures/si"

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(MAIN_FIG_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(SI_FIG_DIR, recursive = TRUE, showWarnings = FALSE)

SEED <- 170715L
B <- 1000L
set.seed(SEED)

# Okabe--Ito plus restrained neutral tones.
COL_BLUE <- "#0072B2"
COL_SKY <- "#56B4E9"
COL_GREEN <- "#009E73"
COL_ORANGE <- "#E69F00"
COL_VERMILION <- "#D55E00"
COL_PURPLE <- "#CC79A7"
COL_DARK <- "#262626"
COL_MID <- "#737373"
COL_LIGHT <- "#D9D9D9"
COL_PALE <- "#F2F2F2"

theme_p17 <- function(base_size = 8.5) {
  theme_classic(base_size = base_size, base_family = "Arial") +
    theme(
      text = element_text(colour = COL_DARK),
      axis.line = element_line(colour = "black", linewidth = 0.35),
      axis.ticks = element_line(colour = "black", linewidth = 0.35),
      axis.ticks.length = unit(1.2, "mm"),
      axis.text = element_text(colour = COL_DARK),
      axis.title = element_text(colour = COL_DARK),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      plot.caption = element_blank(),
      plot.margin = margin(3, 3, 3, 3, unit = "mm"),
      legend.title = element_blank(),
      legend.key.height = unit(3.2, "mm"),
      legend.key.width = unit(4.2, "mm"),
      legend.spacing.x = unit(1.2, "mm"),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold", margin = margin(b = 1.5, unit = "mm"))
    )
}

tag_theme <- theme(
  plot.tag = element_text(family = "Arial", face = "bold", size = 10,
                          colour = COL_DARK, hjust = 0, vjust = 1),
  plot.tag.position = c(0, 1),
  plot.tag.location = "plot"
)

save_figure <- function(plot, filename, width_mm = 178, height_mm = 90) {
  ragg::agg_png(
    filename = filename, width = width_mm, height = height_mm,
    units = "mm", res = 600, background = "white", scaling = 1
  )
  print(plot)
  invisible(dev.off())
}

q_ci <- function(x, conf = 0.95) {
  a <- (1 - conf) / 2
  unname(quantile(x, c(a, 1 - a), na.rm = TRUE))
}

fmt_p <- function(p) {
  ifelse(is.na(p), "--",
         ifelse(p < 0.001, "<0.001", sub("^0", "", sprintf("%.3f", p))))
}

fmt_num <- function(x, digits = 2) {
  ifelse(is.na(x), "--", sprintf(paste0("%.", digits, "f"), x))
}

fmt_ci <- function(lo, hi, digits = 2) {
  ifelse(is.na(lo) | is.na(hi), "--",
         sprintf(paste0("[%.", digits, "f, %.", digits, "f]"), lo, hi))
}

alpha_manual <- function(M) {
  M <- as.matrix(M)
  k <- ncol(M)
  (k / (k - 1)) * (1 - sum(apply(M, 2, var)) / var(rowSums(M)))
}

lmm <- function(formula, data) {
  lmerTest::lmer(
    formula, data = data,
    control = lmerControl(calc.derivs = FALSE)
  )
}

tex_escape <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, fixed("\\"), "\\textbackslash{}")
  x <- str_replace_all(x, "([%&#_$])", "\\\\\\1")
  x <- str_replace_all(x, fixed("~"), "\\textasciitilde{}")
  x
}

