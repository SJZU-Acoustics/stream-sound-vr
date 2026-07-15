# =============================================================================
# load_data.R -- read the Mendeley Data workbook and write the analysis-ready
# table as a CSV into intermediate/, which the analysis pipeline consumes.
#
# Input  : data/Stream_sound_augmentation_traffic_VR_data.xlsx
#          Download it from Mendeley Data and place it in data/ -- see
#          data/README.md.
# Output : intermediate/analysis_wide.csv  (400 rows x 72 cols)
#
# The single `analysis_data` sheet is read as text (so no reader-side numeric
# reformatting is applied) and written straight back out; each analysis module
# then re-infers column types via its own read_csv(), exactly as the working
# pipeline did on the original clean CSV. readr maps both "" and "NA" to NA, so
# the empty EEG/EDA cells survive the round trip.
# =============================================================================
suppressPackageStartupMessages({ library(readxl); library(readr) })

xlsx <- list.files("data", pattern = "\\.xlsx$", full.names = TRUE)
if (length(xlsx) == 0L)
  stop("No .xlsx in data/. Download the workbook from Mendeley Data and place ",
       "it in data/. See data/README.md.")
if (length(xlsx) > 1L)
  stop("Multiple .xlsx in data/; keep only the Mendeley workbook.")

dir.create("intermediate", recursive = TRUE, showWarnings = FALSE)

df <- read_excel(xlsx, sheet = "analysis_data", col_types = "text",
                 .name_repair = "minimal")
write_csv(df, "intermediate/analysis_wide.csv")
message(sprintf("  analysis_data  %5d rows x %3d cols", nrow(df), ncol(df)))
