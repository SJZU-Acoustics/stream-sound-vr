R code for reproducing the statistical analyses, figures and tables for the manuscript "Stream-sound augmentation improves affective appraisal without replacing traffic-noise control in virtual reality".

## Requirements

- R 4.5+ (developed and verified on R 4.5.3)

- CRAN packages: `tidyverse`, `lme4`, `lmerTest`, `emmeans`, `patchwork`, `scales`, `ragg`, `readxl`

- Install:

  ```r
  install.packages(c("tidyverse", "lme4", "lmerTest", "emmeans",
                     "patchwork", "scales", "ragg", "readxl"))
  ```

- Typical install time: a few minutes on a normal desktop with binary CRAN packages (longer if compiled from source). No non-standard hardware is required.

## Data

The analysis reads a single input: the Mendeley Data workbook
`Stream_sound_augmentation_traffic_VR_data.xlsx`.

1. Download it from Mendeley Data, **DOI [10.17632/v77ydkjwvn](https://doi.org/10.17632/v77ydkjwvn)** (CC BY 4.0).
2. Place the `.xlsx` file in the `data/` folder (see `data/README.md`).

The workbook's `analysis_data` sheet is the analysis-ready table (400 rows =
50 participants × 8 conditions, 72 columns) that underlies every figure and
table. The raw EEG/EDA recordings, the window-level EEG series and the VR
scene/audio stimuli are not redistributed (available from the authors on
reasonable request).

## File structure

- `run_all.R` — master script: loads the workbook, runs the synthesis, and writes every manuscript figure and table.
- `load_data.R` — reads the `analysis_data` sheet from the workbook into `intermediate/`.
- `code/00_setup.R` — shared paths, plotting theme, helper functions, the random seed and the bootstrap replicate count.
- `code/01_synthesis_lock.R` — the fixed synthesis: PCA of the attribute space and every RQ1–RQ3 estimand (pooled and per-level stream effects, outcome-family screen, dimensional contrasts and bounded design translation, reach/stability/held-out-prediction, physiology calibration), plus the manuscript result macros. Performs no outcome search.
- `code/02_main_figures.R` — Figures 1–4.
- `code/03_supplementary_figures.R` — Supplementary Figures S1–S3.
- `code/04_tables.R` — Supplementary Tables S1–S5 (LaTeX fragments and CSV).

## Usage

From the repository root:

```bash
Rscript run_all.R
```

The full pipeline runs in about 15 seconds on a normal desktop. Outputs are written to:

- `figures/` — Figures 1–4, and `figures/si/` — Supplementary Figures S1–S3 (PNG, 600 dpi)
- `output/` — the frozen numeric lock (`rq*.csv`, `pca_*.csv`, …), `results_macros.tex`, the generated `Table_S1..S5.{tex,csv}`, `LOCK_CHECKS.txt` and `sessionInfo.txt`

## Notes

- The synthesis is deterministic: it uses **1,000** participant-cluster bootstrap replicates with random seed **170715**, set in `code/00_setup.R`. `code/01_synthesis_lock.R` ends with hard checks that stop the run if the paper's headline answers drift.
- Mixed models use `lme4`/`lmerTest` (Satterthwaite degrees of freedom via `emmeans`); the attribute-space decomposition uses base-R `prcomp`.
- Figures are rendered with `ragg` at 600 dpi in the Arial family; if Arial is unavailable the renderer substitutes a default sans-serif font (this affects glyph rendering only, not any reported value).
- `intermediate/`, `output/` and `figures/` are produced at run time and are safe to delete.

## License

Code in this repository is released under the MIT License (see `LICENSE`). The input data are archived separately under CC BY 4.0 at Mendeley Data (DOI as above).
