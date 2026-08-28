# Data

This folder holds the single input to the analysis: the Mendeley Data workbook.

1. Download `Stream_sound_augmentation_traffic_VR_data.xlsx` from Mendeley Data —
   **DOI [10.17632/v77ydkjwvn](https://doi.org/10.17632/v77ydkjwvn)** (CC BY 4.0).
2. Place the `.xlsx` file in this `data/` folder.
3. From the repository root, run `Rscript run_all.R`.

The workbook has seven sheets: a README sheet, a sheet summary, the
analysis-ready data table (`analysis_data`, 400 rows × 72 columns — one row per
participant × condition), the window-level EEG series (`eeg_window_series`,
404,976 rows) and the EDA signal series (`eda_signal_series`, 165,120 rows)
from which the per-condition physiological summaries were derived, a variable
dictionary and a value-code map. `load_data.R` reads the `analysis_data` sheet
only; the other sheets are documentation and provenance.

The workbook itself is not stored in this repository — it is openly archived at
the DOI above. Raw EEG/EDA recordings and the VR scene/audio stimuli are not
part of this deposit (available from the authors on reasonable request); the
per-condition summaries needed to reproduce every display item are all in the
workbook.
