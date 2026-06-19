# End-to-end analysis runner.
# Place required input CSVs in data/, then run: source("run_analysis.R")
source("R/00_packages.R")
source("R/01_data_ingest_prepare.R")
source("R/02_data_exploration.R")
source("R/03_data_analysis.R")
source("R/04_model_95_percent.R")
source("R/05_plotting.R")

if (exists("salmon_df_joined_2018")) {
  salmon_df_joined_2018 <- calculate_final_mortality_rate(salmon_df_joined_2018)
  rem_model <- fit_rem_model_95(salmon_df_joined_2018, baseline_df = salmon_df_active)
  write_output_csv(rem_model$site_summary, "rem_site_summary_95_percent.csv")
  write_output_csv(rem_model$cycle_summary, "rem_cycle_summary_95_percent.csv")
  ggsave(file.path("outputs", "figures", "rem_sites_95_percent.png"), plot_rem_sites(rem_model), width = 12, height = 8, dpi = 300)
}
