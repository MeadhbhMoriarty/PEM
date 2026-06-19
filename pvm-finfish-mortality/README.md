# Finfish Mortality REM/PEM Analysis

This repository contains an R workflow and Shiny decision-support app for analysing monthly finfish mortality, recurrent elevated mortality (REM), and persistent elevated mortality (PEM) signals.

The code was modularised from the original draft analysis into separate scripts for readability and GitHub use.

## Project structure

```text
.
├── R/
│   ├── 00_packages.R              # package loading and shared output helpers
│   ├── 01_data_ingest_prepare.R   # SEPA, FHI/SGMD, and Salmon Scotland data preparation
│   ├── 02_data_exploration.R      # baseline summaries, QA summaries, and exploratory outputs
│   ├── 03_data_analysis.R         # REM/PEM analysis logic from the draft workflow
│   ├── 04_model_95_percent.R      # reusable 95th-percentile model helpers
│   └── 05_plotting.R              # reusable plotting helpers
├── PVM_analysis_original.Rmd      # original notebook-style draft, with project-relative paths
├── PVM_analysis_modular.Rmd       # R Markdown wrapper around the split scripts
├── app.R                          # Shiny REM/PEM decision-support tool
├── run_analysis.R                 # end-to-end analysis runner
├── data/README.md                 # expected raw input files
└── outputs/                       # generated figures, tables, and QA checks
```

## Data inputs

Add the required CSV files to `data/` before running the workflow. See `data/README.md` for the expected filenames. Raw data are intentionally excluded from GitHub via `.gitignore`.

## Install packages

```r
source("R/00_packages.R")
install.packages(missing_packages)
```

## Run the analysis

Use the split-script runner:

```r
source("run_analysis.R")
```

Or render the R Markdown workflow:

```r
rmarkdown::render("PVM_analysis_modular.Rmd")
```

The original draft is also included as `PVM_analysis_original.Rmd` for traceability.

Generated tables and QA checks are written to `outputs/tables/` and `outputs/QA_checks/`; generated figures are written to `outputs/figures/`.

## Run the Shiny app

```r
shiny::runApp()
```

By default, the app reads `data/data_for_app_development.csv`. To point it at another prepared dataset, set `PVM_APP_DATA` before starting Shiny:

```r
Sys.setenv(PVM_APP_DATA = "path/to/data_for_app_development.csv")
shiny::runApp()
```

## Model definition

The reusable model in `R/04_model_95_percent.R` calculates a 95th-percentile mortality threshold from a baseline dataset, flags high monthly mortality events, identifies within-cycle streaks, and then flags sites with recurrent elevated mortality across the requested number of cycles.

## Notes for maintainers

- The scripts preserve the original draft workflow as closely as possible, but file paths have been made project-relative.
- Keep raw data out of GitHub unless licensing and disclosure checks are complete.
- The Shiny app is included as `app.R` so it can be deployed directly to shinyapps.io, Posit Connect, or run locally.

