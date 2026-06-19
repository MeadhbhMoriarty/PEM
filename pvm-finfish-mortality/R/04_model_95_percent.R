# 95th percentile REM/PEM model helpers.
# These functions are extracted from the draft logic and can be reused by scripts or Shiny.
source("R/00_packages.R")

calculate_final_mortality_rate <- function(df) {
  df %>%
    mutate(
      mortality_rate_final = case_when(
        biomass_decrease_flag == FALSE ~ mortality_rate,
        biomass_decrease_flag == TRUE & action %in% c("Cull", "Harvest", "Fallow", "Harvest; Fallow", "Fallow; Harvest") ~ adjusted_mortality_rate,
        biomass_decrease_flag == TRUE & unexplained_biomass_drop == TRUE ~ adjusted_mortality_rate,
        TRUE ~ mortality_rate
      )
    )
}

detect_streaks <- function(x, min_len = 2) {
  r <- rle(x)
  rep(r$lengths >= min_len & r$values, r$lengths)
}

fit_rem_model_95 <- function(df, baseline_df = df, mortality_col = "mortality_rate_final", baseline_mortality_col = NULL, min_streak = 2, min_cycles = 2) {
  stopifnot(all(c("site_number", "stock_cycle", "date") %in% names(df)))
  if (is.null(baseline_mortality_col)) {
    baseline_mortality_col <- if (mortality_col %in% names(baseline_df)) mortality_col else "mortality_rate"
  }
  threshold <- quantile(baseline_df[[baseline_mortality_col]], 0.95, na.rm = TRUE)

  modelled <- df %>%
    mutate(high_event = .data[[mortality_col]] >= threshold) %>%
    arrange(site_number, stock_cycle, date) %>%
    group_by(site_number, stock_cycle) %>%
    mutate(within_cycle_streak = detect_streaks(high_event, min_len = min_streak)) %>%
    ungroup()

  cycle_summary <- modelled %>%
    group_by(site_number, stock_cycle) %>%
    summarise(has_streak = any(within_cycle_streak, na.rm = TRUE), .groups = "drop") %>%
    arrange(site_number, stock_cycle) %>%
    group_by(site_number) %>%
    mutate(
      cycle_num = as.numeric(factor(stock_cycle, levels = sort(unique(stock_cycle)))),
      streak_group = cumsum(!(has_streak & lag(has_streak, default = FALSE) &
                               (cycle_num - lag(cycle_num, default = cycle_num[1])) == 1)),
      streak_len = ave(has_streak, streak_group, FUN = function(x) if (all(!x)) 0 else sum(x)),
      rem_cycle_flag = has_streak & streak_len >= min_cycles
    ) %>%
    ungroup()

  site_summary <- cycle_summary %>%
    group_by(site_number) %>%
    summarise(
      n_cycles = n(),
      n_rem_cycles = sum(rem_cycle_flag, na.rm = TRUE),
      rem_site_flag = any(rem_cycle_flag, na.rm = TRUE),
      .groups = "drop"
    )

  list(
    threshold_95 = threshold,
    modelled_data = modelled %>% left_join(cycle_summary, by = c("site_number", "stock_cycle")),
    cycle_summary = cycle_summary,
    site_summary = site_summary
  )
}

