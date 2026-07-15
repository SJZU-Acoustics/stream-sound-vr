#!/usr/bin/env Rscript
# =============================================================================
# run_all.R -- one-command reproduction of every figure, table and reported
# number in the manuscript and its Supplementary Information.
#
#   Rscript run_all.R                       (run from the repository root)
#
# Pipeline:  data/<Mendeley .xlsx>
#              -> intermediate/analysis_wide.csv   (load_data.R)
#              -> output/    (numeric lock, result macros, Tables S1-S5)
#              -> figures/   (Figures 1-4) and figures/si/ (Figures S1-S3)
#
# The intermediate/ folder holds the regenerated analysis table and is safe to
# delete. The synthesis uses 1,000 participant-cluster bootstrap replicates with
# random seed 170715.
# =============================================================================
t0 <- Sys.time()
if (!dir.exists("code"))
  stop("Run from the repository root:  Rscript run_all.R")

# Fresh run, so only the manuscript's items remain.
unlink(c("output", "figures", "intermediate"), recursive = TRUE)

message("== 1/5  Load Mendeley workbook ==")
source("load_data.R")

message("\n== 2/5  Fixed synthesis lock (numbers, macros) ==")
source("code/01_synthesis_lock.R")

message("\n== 3/5  Main figures (Figures 1-4) ==")
source("code/02_main_figures.R")

message("\n== 4/5  Supplementary figures (Figures S1-S3) ==")
source("code/03_supplementary_figures.R")

message("\n== 5/5  Tables S1-S5 ==")
source("code/04_tables.R")

message(sprintf("\n== Done in %.0f s.  Outputs: figures/  figures/si/  output/ ==",
                as.numeric(difftime(Sys.time(), t0, units = "secs"))))
