# Plotting helpers for analysis outputs.
source("R/00_packages.R")
ensure_project_dirs()

plot_mortality_heatmap <- function(df, mortality_col = "mortality_rate") {
  df %>%
    group_by(year, month) %>%
    summarise(avg_mortality = mean(.data[[mortality_col]], na.rm = TRUE), .groups = "drop") %>%
    ggplot(aes(x = factor(month), y = factor(year), fill = avg_mortality)) +
    geom_tile(color = "white") +
    scale_fill_viridis_c(option = "C", name = "Avg mortality (%)") +
    labs(x = "Month", y = "Year", title = "Monthly mortality heatmap") +
    theme_minimal()
}

plot_reported_vs_final <- function(df, threshold = 5.5) {
  df %>%
    mutate(category = if_else(mortality_rate > threshold & mortality_rate_final < threshold,
                              "Reported above threshold; final below", "Other")) %>%
    ggplot(aes(x = mortality_rate, y = mortality_rate_final, color = category)) +
    geom_point(alpha = 0.7, size = 2) +
    geom_vline(xintercept = threshold, linetype = "dashed") +
    geom_hline(yintercept = threshold, linetype = "dashed") +
    scale_color_manual(values = c("Reported above threshold; final below" = "red", "Other" = "grey70")) +
    labs(title = "Reported vs final mortality rates", x = "Reported monthly mortality (%)", y = "Final monthly mortality (%)", color = NULL) +
    theme_minimal()
}

plot_rem_sites <- function(model_result) {
  model_result$modelled_data %>%
    filter(rem_cycle_flag) %>%
    ggplot(aes(x = date, y = mortality_rate_final, color = as.factor(stock_cycle))) +
    geom_point() +
    geom_hline(yintercept = model_result$threshold_95, linetype = "dashed", color = "red") +
    facet_wrap(~ site_number, scales = "free_x") +
    labs(title = "REM cycles using 95th percentile threshold", x = "Date", y = "Final monthly mortality (%)", color = "Cycle") +
    theme_minimal() +
    theme(legend.position = "none")
}
