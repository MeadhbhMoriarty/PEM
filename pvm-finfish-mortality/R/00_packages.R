# Package loading for the finfish mortality analysis project.
# Install any missing packages before sourcing the analysis scripts.
required_packages <- c(
  "tidyverse", "lubridate", "kableExtra", "zoo", "forecast", "scales",
  "webshot", "data.table", "gt", "flextable", "ggplot2", "qcc",
  "forcats", "tidyr", "stringr", "mgcv", "purrr", "DT", "openxlsx",
  "shiny"
)

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  message("Missing packages: ", paste(missing_packages, collapse = ", "))
  message("Install them with: install.packages(c(", paste(sprintf('\"%s\"', missing_packages), collapse = ", "), "))")
}

invisible(lapply(required_packages, function(pkg) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }
}))

ensure_project_dirs <- function() {
  dirs <- c("data", "outputs", "outputs/figures", "outputs/tables", "outputs/QA_checks")
  invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))
}

write_output_csv <- function(x, filename, subdir = "tables", row.names = FALSE) {
  ensure_project_dirs()
  out <- file.path("outputs", subdir, filename)
  write.csv(x, out, row.names = row.names)
  invisible(out)
}

