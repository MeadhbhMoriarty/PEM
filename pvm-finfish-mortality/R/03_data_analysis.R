# Main analysis for recurrent and persistent elevated mortality.
# Run after R/01_data_ingest_prepare.R and R/02_data_exploration.R.
source("R/00_packages.R")
ensure_project_dirs()
# ---- calculate-monthly-mort-rate, echo=FALSE ----

str(salmon_df_joined_2018)

salmon_df_joined_2018 <- salmon_df_joined_2018 %>%
  mutate(
    mortality_rate_final = case_when(
      # 1. Biomass has increased compared to previous month
      biomass_decrease_flag == FALSE ~ mortality_rate,

      # 2. Biomass decreased, but decrease is explained by reported mortalities
     # biomass_decrease_flag == TRUE & !is.na(mortality_rate) ~ mortality_rate,

      # 3. Biomass decreased, not explained by mortalities, 
      # but harvesting/cull/transfer has been reported
      biomass_decrease_flag == TRUE & action %in% c("Cull", "Harvest", "Fallow", "	Harvest; Fallow", "Fallow; Harvest") ~ adjusted_mortality_rate,

      # 4. Biomass decreased, no action reported, but SEPA data indicates unexplained drop
      biomass_decrease_flag == TRUE & unexplained_biomass_drop == TRUE ~ adjusted_mortality_rate,

      # 5. Fallback: use reported mortality rate
      TRUE ~ mortality_rate
    )
  )
salmon_df_joined_2018 %>%
  select(sepa_site, date, biomass_decrease_flag, action, unexplained_biomass_drop, mortality_rate, adjusted_mortality_rate, mortality_rate_final) %>%
  head(100)




# ---- mortality_final echo=FALSE ----
mort_plot_2 <- salmon_df_joined_2018 %>%
  ggplot(aes(x = (mortality_rate), y=(mortality_rate_final))) +
  geom_point(fill = "skyblue", alpha = 0.5) +
  geom_vline(xintercept = 5.5, linetype = "dashed", color = c("red")) +
  geom_hline(yintercept = 5.5, linetype = "dashed", color = c("red")) +
  annotate("text", x = 5.67, y = 5.65, label = "5.5%", angle = 0, vjust = -0.5, color = c("red")) +
  labs(title = "Monthly Mortality Rates",
       x = "Reported Monthly Mortality (%)", y = "Final Monthly Mortality (%)") +
  theme_minimal()




salmon_df_joined_2018 <- salmon_df_joined_2018 %>%
  mutate(category = case_when(
    mortality_rate > 5.5 & mortality_rate_final < 5.5 ~ "Above X, Below Y",
    TRUE ~ "Other"
  ))

n_flagged <- salmon_df_joined_2018 %>%
  filter(mortality_rate > 5.5,
         mortality_rate_final < 5.5) %>%
  nrow()

mort_plot_2 <- ggplot(salmon_df_joined_2018,
       aes(x = mortality_rate,
           y = mortality_rate_final,
           color = category)) +

  geom_point(alpha = 0.7, size = 2) +

  geom_vline(xintercept = 5.5, linetype = "dashed") +
  geom_hline(yintercept = 5.5, linetype = "dashed") +

  scale_color_manual(values = c(
    "Above X, Below Y" = "red",
    "Other" = "grey70"
  )) +

  labs(
    title = "Comparison of Reported vs Final Mortality Rates",
    subtitle = "Highlighted: Reported > 5.5% and Final < 5.5%",
    x = "Reported Monthly Mortality (%)",
    y = "Final Monthly Mortality (%)",
    color = "Category"
  ) +

  theme_minimal() + annotate("text",
           x = Inf, y = -Inf,
           label = paste("Flagged:", n_flagged, "of 11940"),
           hjust = 1.1, vjust = -1,
           size = 4)

# Save as PNG (300 dpi, 8x6 inches)
ggsave("figXX_mortality_rates_alt.png", plot = mort_plot_2,
       width = 8, height = 6, dpi = 300, units = "in")

# Save as TIFF (publication-grade)
ggsave("figXX_mortality_rates_alt.tiff", plot = mort_plot_2,
       width = 8, height = 6, dpi = 600, units = "in", compression = "lzw")

mort_diff <-salmon_df_joined_2018 %>%
  mutate(diff = mortality_rate_final - mortality_rate) %>%

  ggplot(aes(x = mortality_rate, y = diff)) +

  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(alpha = 0.6) +

  labs(
    title = "Difference Between Final and Reported Mortality",
    y = "Final - Reported (%)",
    x = "Reported Mortality (%)"
  ) +

  theme_minimal()

# Save as PNG (300 dpi, 8x6 inches)
ggsave("figXX_mortality_diff_alt.png", plot = mort_diff,
       width = 8, height = 6, dpi = 300, units = "in")

# Save as TIFF (publication-grade)
ggsave("figXX_mortality_diff_alt.tiff", plot = mort_diff,
       width = 8, height = 6, dpi = 600, units = "in", compression = "lzw")
# ---- highlight-high-cycles-SEPA,fig.width=12, fig.height=12, echo=FALSE ----
national_average <-mean(salmon_df_active$mortality_rate,  na.rm = TRUE)
#mid_thresh <- quantile(salmon_df_active$mortality_rate, 0.75, na.rm = TRUE)
high_thresh <- quantile(salmon_df_active$mortality_rate, 0.95, na.rm = TRUE)
very_high_thresh <- quantile(salmon_df_active$mortality_rate, 0.99, na.rm = TRUE)


# --- Step 1: Detect within-cycle monthly streaks (â‰¥2 consecutive TRUEs) ---
detect_streaks <- function(x) {
  r <- rle(x)
  rep(r$lengths >= 2 & r$values, r$lengths)
}


high_thresh <- quantile(salmon_df_active$mortality_rate, 0.95, na.rm = TRUE)

# Step 2 - classify data
salmon_df_joined_2018 <- salmon_df_joined_2018 %>%
  mutate(high_event = mortality_rate_final >= high_thresh) %>% 
  arrange(site_number, date) %>%
  group_by(site_number) %>%
  mutate(
    month_id = row_number(),  # helps with indexing
    high_event_numeric = as.integer(high_event),
  ) %>%
  ungroup()

salmon_df_joined_2018 <- salmon_df_joined_2018 %>%
  arrange(site_number, stock_cycle, stock_cycle_month_n) %>%
  group_by(site_number, stock_cycle) %>%
  mutate(within_cycle_streak = detect_streaks(high_event)) %>%
  ungroup()

# --- Step 2: Flag cycles that have any within-cycle streak ---
cycle_summary <- salmon_df_joined_2018 %>%
  group_by(site_number, stock_cycle) %>%
  summarise(has_streak = any(within_cycle_streak, na.rm = TRUE), .groups = "drop")

# --- Step 3: Detect consecutive cycles with streaks ---
cycle_summary <- cycle_summary %>%
  arrange(site_number, stock_cycle) %>%
  group_by(site_number) %>%
  mutate(
    # Convert stock_cycle to numeric if possible
    cycle_num = as.numeric(factor(stock_cycle, levels = sort(unique(stock_cycle)))),
    
    # Group consecutive cycles with streaks
    streak_group = cumsum(!(has_streak & dplyr::lag(has_streak, default = FALSE) &
                              (cycle_num - dplyr::lag(cycle_num, default = cycle_num[1])) == 1)),
    
    streak_len = ave(has_streak, streak_group, FUN = function(x) if (all(!x)) 0 else sum(x)),
    consec_cycle_flag = has_streak & streak_len >= 2
  ) %>%
  ungroup()

# --- Step 4: Join back to main dataframe ---
salmon_df_joined_2018 <- salmon_df_joined_2018 %>%
  left_join(cycle_summary %>% select(site_number, stock_cycle, consec_cycle_flag),
            by = c("site_number", "stock_cycle"))

# --- Step 5: Summaries at site level ---
summary_table <- salmon_df_joined_2018 %>%
  # Create the category
  mutate(
    category = case_when(
      consec_cycle_flag ~ "b. sites with REM cycles",
      !consec_cycle_flag ~ "a. sites with no REM cycles"
    )
  ) %>%
  # Summarize by site_number to avoid double-counting rows
  group_by(site_number, category) %>%
  summarise(.groups = "drop") #%>%
# Count how many sites fall into each category
# count(category, name = "num_sites") %>%
# arrange(category)

REM_sites <- summary_table %>%
  filter(category %in% c("b. sites with REM cycles")) %>%
  pull(site_number)

summary_table %>%
  count(category, name = "num_sites") %>%
  arrange(category) %>%
  gt() %>%
  tab_header(title = "Number of sites in each recurring classification level 2018-2024")


# View the summary table
summary_table


# Choose top high-mortality cycles
ggplot(filter(salmon_df_joined_2018, site_number %in% REM_sites), aes(x = date, y = mortality_rate)) +
  geom_point(aes(color = stock_cycle)) +
  # Add threshold line
    geom_hline(yintercept = high_thresh, linetype = "dashed", color = "red") +
    geom_hline(yintercept = national_average, linetype = "solid", color = "black", linewidth = .75) +
    geom_hline(yintercept = very_high_thresh, linetype = "dashed", color = "darkred", linewidth = .75) +
  facet_wrap(~site_number) +
  labs(title = "Mortality Trends in SEPA sites breaching high threshold (2018-2014)",
       x = "Date",
       y = "Monthly Mortality (%)") +
  theme_minimal()+
      theme(legend.position = "none")

# ---- towards-persistent-reasons-sepa-plot,fig.width=12, fig.height=12, echo=FALSE, warning=FALSE, message=FALSE ----
#ï¸ Step-by-Step Processing
# clean actions
salmon_df_joined_2018 <- salmon_df_joined_2018 %>%
   arrange(date) %>%
   mutate(
     action_cleaned = case_when(grepl("harvest", tolower(action)) ~ "Harvest Reported",
       is.na(action) | action == "0" | trimws(action) == "" ~ "No Action Reported",
        TRUE ~ "Other"
     ),
     action_cleaned = factor(action_cleaned, levels = c("Harvest Reported", "No Action Reported", "Other"))
    )
#1. combine reasons 
#FHI reasons and reasons_1 likely contain overlapping or split data.

salmon_df_joined_2018 <- salmon_df_joined_2018 %>%
  mutate(
    reason_combined = paste(top_cause_1, top_cause_2, top_cause_3, sep = "; "),
    reason_combined = gsub("NA|^; |; NA", "", reason_combined),
    reason_combined = tolower(trimws(reason_combined))
  )
#2. Clean & Recombine Reason Fields
# Helper function to clean, split, sort and recombine causes
clean_reason <- function(x) {
  x %>%
    str_split(";") %>%                      # Split into individual causes
    map(~ str_trim(.x)) %>%                # Trim white space
    map(~ tolower(.x)) %>%                 # Lowercase
    map(~ .x[!is.na(.x) & .x != ""]) %>%    # Remove NA and empty strings
    map(~ sort(unique(.x))) %>%            # De-duplicate and sort
    map_chr(~ paste(.x, collapse = "; "))  # Recombine
}

# Apply the cleaner
salmon_df_joined_2018 <- salmon_df_joined_2018 %>%
  mutate(reason_combined_clean = clean_reason(reason_combined))

#2. Aggregate Reasons per Stock Cycle
#This step allows us to see patterns within cycles:

cycle_mortality_pers <- salmon_df_joined_2018 %>%
  filter(site_number %in% REM_sites, mortality_rate_final > high_thresh & high_event==TRUE & within_cycle_streak==TRUE & consec_cycle_flag==TRUE) %>%
  group_by(site_number, stock_cycle, date) %>%
  select(
    site_number,
    stock_cycle,
    stock_cycle_month_n,
    date,
    month,
    year,
    mortality_rate,
    adjusted_mortality_rate,
    mortality_rate_final,
    action,
    reason_combined_clean
     ) %>%
  #filter(!is.na(reason_combined_clean) & reason_combined_clean != "") %>%
  arrange(site_number, stock_cycle, desc(date))
#write.csv(cycle_mortality_pers, "Expert_check_PEM_SEPAdata2018to2024.csv")
#write.csv(salmon_df_joined_2018, "salmon_all_df_SEPAFHI_2018to2024.csv")


#3. Compare Causes Between Cycles at a Site
#This helps detect persistent vs shifting causes:

#cycles_to_check_reason_towards_persistent <- cycle_mortality_pers %>%
#  filter(site_number %in% reccurent_sites_2018_v1)

high_mortality_summary <- cycle_mortality_pers %>%
  group_by(site_number, stock_cycle, date, reason_combined_clean) %>%
  summarise(
    mortality_rate = sum(mortality_rate, na.rm=TRUE),
    adjusted_mortality = sum(adjusted_mortality_rate, na.rm = TRUE),
    mortality_rate_final =sum(mortality_rate_final, na.rm = TRUE),
    any_action = any(action != "0"),
    .groups = "drop"
  )

high_mortality_summary%>% gt() %>%
  tab_header(title = "Recurrent Mortality - Reason Reported") %>%
  fmt_number(columns = c(mortality_rate, adjusted_mortality), decimals = 2)

write.csv(high_mortality_summary, "EC_Recurrent_Mortality_with_Reason_Reported_summary_data_SEPA_2018-2024.csv")

# ---- , category-mapping,fig.width=12, fig.height=12, echo=FALSE, warning=FALSE, message=FALSE  ----


#1. Function to map combined reasons to a set of high-level categories:
# Clean and reclassify into main buckets
# Function to map a single reason string to high-level categories
map_reason_to_category <- function(reason_string) {
  if (is.na(reason_string) || reason_string == "") return("Unknown")
  
  reasons <- unlist(str_split(reason_string, ";\\s*"))
  categories <- unique(unlist(map(reasons, function(r) {
    key <- str_trim(tolower(r))
    matched <- reason_category_map[[key]]
    if (is.null(matched)) return("other") else return(matched)
  })))
  
  paste(sort(categories), collapse = "; ")
}

#2. Define a mapping from sub-reason phrases to 5 categories:
# High-level category mapping table
reason_category_map <- list(
  "environmental" = "environmental",
   "Environmental" = "environmental",
  "Infectious diseases" = "infectious diseases",
  "Handling and treatment" = "handling and treatment",
  "Developmental"= "developmental",
  "Gill health related" = "gill health related",
  "Other" = "other",
  "infectious diseases" = "infectious diseases",
  "handling and treatment" = "handling and treatment",
  "developmental"= "developmental",
  "gill health related" = "gill health related",
  "other" = "other"
)
# 3. Apply row-wise using `map_chr`
towards_PEM <- cycle_mortality_pers %>%
  mutate(
    reason_category_combined = map_chr(reason_combined_clean, map_reason_to_category)
  )


# Step 4: Clean and un nest causes
towards_PEM_unnested <- towards_PEM %>%
  # Split reason_combined_clean into individual reasons
  mutate(reason_split = str_split(reason_combined_clean, ";\\s*"),
  n_causes = map_int(reason_split, length)) %>%
  unnest(reason_split) %>%
  mutate(
    reason_split = tolower(str_trim(reason_split)),
    high_level_reason = map_chr(reason_split, ~ {
      reason_category_map[[.x]] %||% "other"
    }), 
    shared_mortality_rate = mortality_rate_final / n_causes  # distribute evenly across causes
  ) %>%
  group_by(site_number, stock_cycle, stock_cycle_month_n, date, high_level_reason) %>%
  summarise(
    total_mortality_rate = sum(mortality_rate_final),
    avg_mortality_rate = mean(shared_mortality_rate),  # equally split contribution
    .groups = "drop"
  )


str(towards_PEM_unnested)


filtered_reasons <- towards_PEM_unnested %>%
  # Count how many distinct cycles each high_level_reason appears in per site
  group_by(site_number, high_level_reason) %>%
  summarise(
    num_cycles = n_distinct(stock_cycle),
    .groups = "drop"
  ) %>%
  # Keep only those that appear in 2 or more cycles
  filter(num_cycles >= 2)

towards_PEM_filtered <- towards_PEM_unnested %>%
  inner_join(
    filtered_reasons %>% select(site_number, high_level_reason),
    by = c("site_number", "high_level_reason")
  )

write.csv(towards_PEM_filtered, "ExpertChecking_Persistent_Mortality_with_Reason_Reported_summary_data_SEPA_2018-2024.csv")
# Step 3: Plot stacked bars
stacked_plot <- ggplot(towards_PEM_filtered, aes(
  x = factor(stock_cycle),
  y = avg_mortality_rate,
  fill = high_level_reason
)) +
  geom_col() +
  facet_wrap(~site_number, scales = "free_x") +
  scale_fill_brewer(palette = "Set2", name = "Mortality Cause (High Level)") +
  labs(
    title = "Average Mortality by High-Level Cause per Stock Cycle",
    x = "Stock Cycle",
    y = "Avg Mortality Rate"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5)) +
      theme(legend.position = "right") +
  theme(legend.position = "bottom")

ggsave("SEPA_mortality_by_cause_stacked.png", stacked_plot, width = 14, height = 10)
stacked_plot

# Save as TIFF (publication-grade)
ggsave("SEPA_mortality_by_cause_stacked.tiff", plot = stacked_plot,
       width = 8, height = 6, dpi = 600, units = "in", compression = "lzw")
stacked_plot_1 <- towards_PEM_filtered %>%
   filter((site_number) ==  "FS0853")

plot_site_C <-  ggplot(stacked_plot_1, aes(
  x = factor(stock_cycle),
  y = avg_mortality_rate,
  fill = high_level_reason
)) +
  geom_col() +
  #facet_wrap(~site_number, scales = "free_x") +
  scale_fill_grey( name = "Mortality Cause (High Level)") +
  labs(
    title = "Site C",
    x = "Stock Cycle",
    y = "Avg Mortality Rate"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5)) +
      theme(legend.position = "right") +
  theme(legend.position = "bottom")

ggsave("SEPA_mortality_by_cause_stacked_siteC.png", plot_site_C, width = 14, height = 10)
ggsave("SEPA_mortality_by_cause_stacked_siteC.tiff", plot = plot_site_C,
       width = 8, height = 6, dpi = 600, units = "in", compression = "lzw")
stacked_plot_2 <- towards_PEM_filtered %>%
   filter((site_number) ==  "FS0656")
plot_site_B <-  ggplot(stacked_plot_2, aes(
  x = factor(stock_cycle),
  y = avg_mortality_rate,
  fill = high_level_reason
)) +
  geom_col() +
  #facet_wrap(~site_number, scales = "free_x") +
  scale_fill_grey(name = "Mortality Cause (High Level)") +
  labs(
    title = "Site B",
    x = "Stock Cycle",
    y = "Avg Mortality Rate"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5)) +
      theme(legend.position = "right") +
  theme(legend.position = "bottom")

ggsave("SEPA_mortality_by_cause_stacked_siteB.png", plot_site_B, width = 14, height = 10)
ggsave("SEPA_mortality_by_cause_stacked_siteB.tiff", plot = plot_site_B,
       width = 8, height = 6, dpi = 600, units = "in", compression = "lzw")


# Step 1: flag consecutive months within the same stock cycle
towards_PEM_flagged <- towards_PEM_unnested %>%
  arrange(site_number, high_level_reason, stock_cycle, stock_cycle_month_n) %>%
  group_by(site_number, high_level_reason, stock_cycle) %>%
  mutate(
    lag_month  = lag(stock_cycle_month_n),
    lead_month = lead(stock_cycle_month_n),
    consecutive_month_same_cycle =
      (!is.na(lag_month)  & stock_cycle_month_n == lag_month + 1) |
      (!is.na(lead_month) & lead_month == stock_cycle_month_n + 1)
  ) %>%
  ungroup() %>%
  select(-lag_month, -lead_month)

# Step 2: identify cycles that have consecutive months and appear in the next cycle
# Build a table of cycles per site/reason
cycle_table <- towards_PEM_flagged %>%
  distinct(site_number, high_level_reason, stock_cycle, consecutive_month_same_cycle) %>%
  filter(consecutive_month_same_cycle == TRUE) %>%  # only consider cycles with consecutive months
  arrange(site_number, high_level_reason, stock_cycle) %>%
  group_by(site_number, high_level_reason) %>%
  mutate(
    next_cycle = lead(stock_cycle),
    consecutive_cycle = ifelse(!is.na(next_cycle) & (next_cycle - stock_cycle == 1), TRUE, FALSE)
  ) %>%
  # propagate the flag to both cycles in the pair
  mutate(
    consecutive_cycle = consecutive_cycle | lag(consecutive_cycle, default = FALSE)
  ) %>%
  ungroup() %>%
  select(site_number, high_level_reason, stock_cycle, consecutive_cycle)

# Step 3: join back to all rows
towards_PEM_flagged <- towards_PEM_flagged %>%
  left_join(cycle_table,
            by = c("site_number", "high_level_reason", "stock_cycle")) %>%
  mutate(
    # if no match in cycle_table, consecutive_cycle is FALSE
    consecutive_cycle = ifelse(is.na(consecutive_cycle), FALSE, consecutive_cycle),
    consecutive_reason_flag = consecutive_month_same_cycle & consecutive_cycle
  )

PEM_sites_1<- unique(towards_PEM_flagged$site_number[towards_PEM_flagged$consecutive_reason_flag==TRUE])

towards_PEM_flagged_dat <- towards_PEM_flagged %>% filter(consecutive_reason_flag==TRUE) %>%  select(
    site_number,
    stock_cycle,
    stock_cycle_month_n,
    date, high_level_reason,
    total_mortality_rate, avg_mortality_rate)
towards_PEM_flagged_dat_unique<-unique(towards_PEM_flagged_dat)
write.csv(towards_PEM_flagged_dat_unique,"Expert_check_PEM_unnested.csv")

##########################TRUE


towards_cycle_mortality_PEM <- salmon_df_joined_2018 %>% 
  inner_join(
    towards_PEM_flagged_dat_unique %>%
      select(site_number, stock_cycle, stock_cycle_month_n, date,avg_mortality_rate),
    by = c("site_number", "stock_cycle", "stock_cycle_month_n", "date")
  ) %>%
  select(
    site_number,
    stock_cycle,
    stock_cycle_month_n,
    date,
    month,
    year,
    mortality_rate_final,
    avg_mortality_rate,
    action,
    reason_combined_clean,
    Freetext_reasons,
    Freetext_unexplained_obs,
    Freetext_Additional_info,
    Freetext_action_taken_fhi,
    All_Mortality_Event_No
  ) %>%
  arrange(site_number, stock_cycle, desc(date))

write.csv(towards_cycle_mortality_PEM,"Expert_check_PEM_nested_SEPA_FHIdata2018to2024.csv")



# Step 4: Clean and un nest causes
towards_cycle_mortality_PEM_unnested <- salmon_df_joined_2018 %>%
  right_join(
    towards_PEM_flagged_dat,
    by = c("site_number", "stock_cycle", "stock_cycle_month_n", "date")
  ) %>%
  select(
    site_number,
    stock_cycle,
    stock_cycle_month_n,
    date,
    month,
    year,
    mortality_rate_final,
    avg_mortality_rate,
    total_mortality_rate,    
    high_level_reason,       
    action,
    reason_combined_clean,
    Freetext_reasons,
    Freetext_unexplained_obs,
    Freetext_Additional_info,
    Freetext_action_taken_fhi,
    All_Mortality_Event_No
  ) %>%
  arrange(site_number, stock_cycle, desc(date))



write.csv(towards_cycle_mortality_PEM_unnested,"Expert_check_PEM_unnested_SEPA_FHIdata2018to2024.csv")



# ---- persistent-in-consecutive-cycles-reasons-sepa-plot,fig.width=12, fig.height=12, echo=FALSE, warning=FALSE, message=FALSE ----
# Ensure data is sorted and consecutive cycle logic applied
consecutive_persistence <- towards_cycle_mortality_PEM_unnested %>%
   filter(total_mortality_rate > high_thresh) %>% # Adjust threshold if needed
  select(site_number, stock_cycle, high_level_reason, avg_mortality_rate, total_mortality_rate) %>%
  arrange(site_number, high_level_reason, stock_cycle) %>%
  group_by(site_number, high_level_reason) %>%
  mutate(
    cycle_diff = stock_cycle - lag(stock_cycle),
    is_consecutive = ifelse(cycle_diff == 1, TRUE, FALSE)
  ) %>%
  summarise(
    total_consecutive_pairs = sum(is_consecutive, na.rm = TRUE),
    n_cycles = n_distinct(stock_cycle),
    cycle_vector = list(sort(unique(stock_cycle))),
    list_mortality_rates = list(sort(unique(avg_mortality_rate))),
    average_mortality_rate = mean(avg_mortality_rate, na.rm = TRUE),
    total_mortality_rate = sum(avg_mortality_rate, na.rm=TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    is_persistent = total_consecutive_pairs > 0   
  )
#write.csv(stacked_causes, "stacked_causes_check_oct25.csv") 

#  Plot updated results
persistent_flag_plot<-ggplot(consecutive_persistence, aes(x = site_number, y = reorder(high_level_reason, total_mortality_rate))) +
  geom_tile(aes(fill = is_persistent)) +
  scale_fill_manual(values = c("TRUE" = "darkred", "FALSE" = "grey80")) +
  labs(
    title = "Flagged Persistent Mortality Causes (â‰¥2 Consecutive Cycles)",
    x = "Site",
    y = "Mortality Cause",
    fill = "Persistent"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("SEPA_persistent_flag_plot.png", plot =persistent_flag_plot, width = 15, height = 12, units = "in")
ggsave("SEPA_persistent_flag_plot.tiff", plot =persistent_flag_plot, width = 15, height = 12, units = "in")
persistent_flag_plot


RC_Cause<-consecutive_persistence %>%
  select(site_number, high_level_reason, average_mortality_rate, cycle_vector) %>%
  mutate(
    cycle_vector = sapply(cycle_vector, paste, collapse = ", ")
  ) 

write.csv(RC_Cause, "Expert_check_summary_of_model_identified_PEM_sites_cycles_causes.csv")
 
 # RC_Cause%>% gt() %>%
#  tab_header(title = "Recurrent Persistent Mortality Causes") %>%
#  fmt_number(columns = average_mortality_rate, decimals = 2)


consecutive_persistence %>%
  filter(is_persistent) %>%
  arrange(desc(n_cycles), desc(average_mortality_rate)) %>%
  gt() %>%
  tab_header(title = "Persistent Elevated Mortality Causes") %>%
  fmt_number(columns = average_mortality_rate, decimals = 2)

length(unique(consecutive_persistence$site_number))

consecutive_persistence_only <- consecutive_persistence %>%
  filter(is_persistent==TRUE)


pers_plot<-ggplot(consecutive_persistence_only, aes(x = site_number, y = reorder(high_level_reason, average_mortality_rate))) +
  geom_tile(aes(fill = average_mortality_rate)) +
  scale_fill_viridis_c() +
  labs(
    title = "Persistent and Recurrent Elevated Mortality Causes by Site",
    x = "Site",
    y = "Cause",
    fill = "Avg Mortality"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggsave("SEPA_2018_2024_pers_plot_95_Oct2025.png", plot = pers_plot, width = 25, height = 20, units = "in")
pers_plot

library(gt)

persistent_table_SEPA2018_2024 <- consecutive_persistence_only %>%
  select(site_number, high_level_reason, #average_mortality_rate, 
         n_cycles, cycle_vector, is_persistent, total_consecutive_pairs) %>%
  mutate(
    cycle_vector = sapply(cycle_vector, paste, collapse = ", ")
  #  total_consecutive_pairs = sum(total_consecutive_pairs),
   # is_persistent = sapply(is_persistent, paste, collapse = ", ")
  ) 
write.csv(persistent_table_SEPA2018_2024, "Expert_check_summary_of_model_identified_PEM_sites_cycles_SEPA_morts.csv")

  persistent_table_SEPA2018_2024 %>% gt() %>%
  tab_header(title = "Recurrent Persistent Elevated Mortality Causes SEPA 2018-2024") %>%
  fmt_number(columns = is_persistent)

  length(unique(persistent_table_SEPA2018_2024$site_number))
  persistent_recurrent_flags_1 <-unique(persistent_table_SEPA2018_2024$site_number)
# ---- , persistent-reasons-sepa-plot, fig.width=12, fig.height=12,echo=FALSE, warning=FALSE, message=FALSE ----

str(fhi_monthly_summary_plus)
str(salmon_df_joined_2018)
#str(persistent_recurrent_flags_1)

# Combine using full joins on 'site_number', 'year', and 'month' where appropriate
# Join datasets
combined_SEPA_2018_df <- fhi_monthly_summary_plus %>%
  full_join(salmon_df_joined_2018, by = c("site_number", "year", "month"))

# Sites to keep
site_data_list_to_check <- unique(persistent_recurrent_flags_1)
#site_data_list_to_check %in% salmon_df_joined_2018$site_name
companies <- subset(salmon_df_joined_2018, site_number == site_data_list_to_check, select = operator)
unique(companies)
companies <- salmon_df_joined_2018 %>%
  filter(site_name == site_data_list_to_check) %>%
  select(operator)

# Filter before splitting
filtered_combined_df <- combined_SEPA_2018_df %>%
  filter(site_number %in% site_data_list_to_check)

# Split by site
site_data_list <- filtered_combined_df %>%
  group_split(site_number, .keep = TRUE) %>%
  set_names(filtered_combined_df %>% pull(site_number) %>% unique())



# Create directory
dir.create("site_data_exports_check_wc21July", showWarnings = FALSE)

# Export only selected sites
walk2(site_data_list, names(site_data_list), ~ {
  write.csv(.x, file = file.path("site_data_exports_check_wc21July", paste0("site_", .y, ".csv")), row.names = FALSE)
})
print(names(site_data_list))
 #site_data_list[["FS1262"]]

# ---- , category-mapping-salscot,fig.width=12, fig.height=12, echo=FALSE, warning=FALSE, message=FALSE  ----

# Define the mapping dictionary
reason_category_map <- list(
  "environmental" = "environmental",
   "Environmental" = "environmental",
  "Infectious diseases" = "infectious diseases",
  "Handling and treatment" = "handling and treatment",
  "Developmental"= "developmental",
  "Gill health related" = "gill health related",
  "Other" = "other",
  "infectious diseases" = "infectious diseases",
  "handling and treatment" = "handling and treatment",
  "developmental"= "developmental",
  "gill health related" = "gill health related",
  "other" = "other"
)

# Helper function for mapping with fallback
map_reason_to_category <- function(reason_string) {
  reason_string <- tolower(str_trim(reason_string))
  matched <- reason_category_map[[reason_string]]
  if (is.null(matched)) return("Other") else return(matched)
}


# Combine, clean, and split causes

stacked_reasons_pc <- pc.dat %>%
  mutate(
    combined_reason = paste(Grouped_Mortality_Cause_1_Use, Grouped_Mortality_Cause_2_Use,
                            Grouped_Mortality_Cause_3_Use, sep = "; "),
    combined_reason = str_replace_all(combined_reason, "\\bNA\\b|^; |; NA|^;|;$", ""),
    combined_reason = str_trim(combined_reason),
    reason_split = str_split(combined_reason, ";\\s*"),  # Split on ";"
    n_causes = map_int(reason_split, length)
  ) %>%
  unnest(reason_split) %>%  # Stack into multiple rows
  filter(reason_split != "", !is.na(reason_split)) %>%
  mutate(reason_split = str_to_title(str_trim(reason_split)),
         high_level_reason = map_chr(reason_split, ~ reason_category_map[[.x]] %||% "Other"),
    shared_mortality_rate = monthly_mortality / n_causes  # distribute evenly across causes
  ) %>%
  group_by(site_number, id, date, high_level_reason, reason_split) %>%
  summarise(
    total_mortality_rate = sum(shared_mortality_rate, na.rm = TRUE),
    avg_mortality_rate = mean(shared_mortality_rate, na.rm = TRUE),
    count = n(),
    .groups = "drop"
  )  # Optional: Title-case

# ---- control-charts-ggplot, echo=FALSE, warning=FALSE, message=FALSE ----

national_average <-mean(salmon_df_active$mortality_rate,  na.rm = TRUE)
#mid_thresh <- quantile(salmon_df_active$mortality_rate, 0.75, na.rm = TRUE)
high_thresh <- quantile(salmon_df_active$mortality_rate, 0.95, na.rm = TRUE)
very_high_thresh <- quantile(salmon_df_active$mortality_rate, 0.99, na.rm = TRUE)

output_dir <- "SEPA_mortality_plots_finalrun_Oct25"  # folder to save plots
dir.create(output_dir, showWarnings = FALSE)  # create folder if not exists
#site_list <- reccurent_sites_2018_v1
site_list <- unique(salmon_df_joined_2018$site_number)
plots <- list()

for (i in seq_along(site_list)) {
  site <- site_list[i]
  
 temp <- salmon_df_joined_2018 %>%
    filter(site_number == site & !is.na(mortality_rate)) %>%
    arrange(date) %>%
    mutate(
      action_cleaned = case_when(
        grepl("harvest", tolower(action)) ~ "Harvesting",
        is.na(action) | action == "0" | trimws(action) == "" ~ "No Action Reported",
        TRUE ~ "Other"
      ),
      action_cleaned = factor(action_cleaned, levels = c("Harvesting", "No Action Reported", "Other"))
    )
  
  if (nrow(temp) > 24) {
    plots[[i]] <- ggplot(temp, aes(x = date, y = mortality_rate)) +
      geom_point(aes(shape = action_cleaned, color = factor(stock_cycle)), size = 2) +
      geom_point(aes(x = date, y = adjusted_mortality_rate,  shape = "adjusted mortality rate", size = 2, alpha = 0.6,  color = factor(stock_cycle))) +
      geom_hline(yintercept = high_thresh, linetype = "dashed", color = "red", size = 1) +
      geom_hline(yintercept = national_average, linetype = "solid", color = "black", size = 0.75) +
      geom_hline(yintercept = very_high_thresh, linetype = "dashed", color = "darkred", size = 0.75) +
      labs(
        title = paste("Mortality Control Chart -", site),
        x = "Date", y = "Mortality Rate (%)",
        shape = "Action Reported",
        color = "Stock Cycle"
      ) +
      theme_minimal() +
      theme(legend.position = "right")
    
    # Save each plot
    ggsave(
      filename = file.path(output_dir, paste0("mortality_chart_", site, ".png")),
      plot = plots[[i]],
      width = 14, height = 10, dpi = 300
    )
 ggsave(
      filename = file.path(output_dir, paste0("mortality_chart_", site, ".tiff")),
      plot = plots[[i]],
    width = 8, height = 6, dpi = 600, units = "in", compression = "lzw"
    )   
    
  }
}


# Display all plots
#plots
# ---- control-charts-ggplot-BW, echo=FALSE, warning=FALSE, message=FALSE ----

national_average <-mean(salmon_df_active$mortality_rate,  na.rm = TRUE)
mid_thresh <- quantile(salmon_df_active$mortality_rate, 0.75, na.rm = TRUE)
high_thresh <- quantile(salmon_df_active$mortality_rate, 0.95, na.rm = TRUE)
very_high_thresh <- quantile(salmon_df_active$mortality_rate, 0.99, na.rm = TRUE)

output_dir <- "SEPA_mortality_plots_BW_for_SVEPM"  # folder to save plots
dir.create(output_dir, showWarnings = FALSE)  # create folder if not exists
#site_list <- reccurent_sites_2018_v1
#site_list <- unique(salmon_df_joined_2018$site_number)
site_list <- c("FS0088", "FS0853", "FS0656")
plots <- list()

for (i in seq_along(site_list)) {
  site <- site_list[i]
  
 temp <- salmon_df_joined_2018 %>%
    filter(site_number == site & !is.na(mortality_rate)) %>%
    arrange(date) %>%
    mutate(
      action_cleaned = case_when(
        grepl("harvest", tolower(action)) ~ "Harvesting",
        is.na(action) | action == "0" | trimws(action) == "" ~ "No Action Reported",
        TRUE ~ "Other"
      ),
      action_cleaned = factor(action_cleaned, levels = c("Harvesting", "No Action Reported", "Other"))
    )
  
  if (nrow(temp) > 24) {
    plots[[i]] <- ggplot(temp, aes(x = date, y = mortality_rate)) +
      geom_point(aes(shape=factor(stock_cycle))) +
      geom_hline(yintercept = high_thresh, linetype = "dotdash", color = "grey5", size = 1) +
      geom_hline(yintercept = national_average, linetype = "solid", color = "grey5", size = 0.75) +
      geom_hline(yintercept = very_high_thresh, linetype = "dashed", color = "grey5", size = 0.75) +
      labs(
        title = paste("Mortality Control Chart -", site),
        x = "Date", y = "Mortality Rate (%)",
        shape = "Stock Cycle"#,
        #color = "Stock Cycle"
      ) +
      theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
      theme(legend.position = "right")
    
    # Save each plot
    ggsave(
      filename = file.path(output_dir, paste0("BW_mortality_chart_", site, ".png")),
      plot = plots[[i]],
      width = 14, height = 10, dpi = 300
    )
     ggsave(
      filename = file.path(output_dir, paste0("mortality_chart_", site, ".tiff")),
      plot = plots[[i]],
    width = 8, height = 6, dpi = 600, units = "in", compression = "lzw"
    )   
  }
}

# Display all plots
#plots
# ---- recurring-high-mortality, fig.width=12, fig.height=12, echo=FALSE ----
mort_p95 <- quantile(pc.dat$monthly_mortality, 0.95, na.rm = TRUE)
print(mort_p95)
mid_thresh <- quantile(salmon_df_active$mortality_rate, 0.75, na.rm = TRUE)
high_thresh <- quantile(salmon_df_active$mortality_rate, 0.95, na.rm = TRUE)
very_high_thresh <- quantile(salmon_df_active$mortality_rate, 0.99, na.rm = TRUE)

 # Step 2 - classify data
pc_salmon_flagged_18_24 <- pc.dat %>%
  mutate(high_event = monthly_mortality >= high_thresh) %>% 
  arrange(site_number, date) %>%
  group_by(site_number) %>%
  mutate(
    month_id = row_number(),  # helps with indexing
    high_event_numeric = as.integer(high_event),
  ) %>%
  ungroup()

  
detect_streaks <- function(x) {
  r <- rle(x)
  streaks <- rep(r$lengths >= 2 & r$values, r$lengths)
  return(streaks)
}

pc_salmon_flagged_18_24 <- pc_salmon_flagged_18_24 %>%
  group_by(site_number) %>%
  mutate(consec_streak = detect_streaks(high_event)) %>%
  ungroup()

# Step 3: Count exceedances per stock_cycle
pc_cycle_level_flags_18_24 <- pc_salmon_flagged_18_24 %>%
  group_by(site_number, id) %>%
  summarise(
    cycle_exceed_high = any(high_event, na.rm = TRUE),
    cycle_has_streak = any(consec_streak, na.rm = TRUE),
    .groups = "drop"
  )

# Step 4: Summarise at site level
pc_classification_summary_18_24 <- pc_cycle_level_flags_18_24 %>%
  arrange(site_number, id) %>%
  group_by(site_number) %>%
  mutate(
    # Flag whether current and previous cycles both had streaks
    has_streak = cycle_has_streak,
    streak_lag = lag(cycle_has_streak, default = FALSE),
    consecutive_streak = has_streak & streak_lag
  ) %>%
  summarise(
    total_cycles_high = sum(cycle_exceed_high, na.rm = TRUE),
    recurring_streak_cycles = sum(cycle_has_streak, na.rm = TRUE),
    has_consecutive_recurring_streaks = any(consecutive_streak, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    category = case_when(
      has_consecutive_recurring_streaks ~ "b. sites with consecutive threshold breaches",
      !has_consecutive_recurring_streaks ~ "a. sites which do not have consecutive cycles with recurring threshold breaches",
      TRUE ~ "Unclassified"
    )
  )  
#summary(classification_summary)
pc_classification_summary_18_24 %>%
  count(category) %>%
  gt() %>%
  tab_header(title = "Number of sites in each recurring classification level SalScot 2018-2024")

#print(summary(as.factor((classification_summary$total_exceed))))

print(summary(as.factor(pc_classification_summary_18_24$total_exceed_high)))

# Filter classification
pc_check_for_persistent_sites_18_24 <- pc_classification_summary_18_24 %>%
  filter(category %in% c("b. sites with consecutive threshold breaches")) %>%
  pull(site_number)

# ---- data-dot-plots, echo=FALSE ----
# Join datasets
combined_SEPA_SALSCOT_2018_df <- pc_salmon_flagged_18_24 %>%
  full_join(salmon_df_joined_2018, by = c("site_number", "year", "date"))

plot.new()
plot(combined_SEPA_SALSCOT_2018_df$monthly_mortality, combined_SEPA_SALSCOT_2018_df$mortality_rate, xlab="SAL SCOT reported mortality (%)", ylab= "SEPA reported mortality (%)" )
abline(v=5.684227, add=T, col="red")
abline(h=5.684227, add=T, col="red")
abline(0,1, add=T, col="yellow")
points(combined_SEPA_SALSCOT_2018_df$total_mortality_rate_fhi, combined_SEPA_SALSCOT_2018_df$mortality_rate, col = "blue", add=T)
points(combined_SEPA_SALSCOT_2018_df$monthly_mortality, combined_SEPA_SALSCOT_2018_df$total_mortality_rate_fhi, col = "lightblue", add=T)
res <- combined_SEPA_SALSCOT_2018_df %>% mutate("CorrectOrNot" = ifelse(monthly_mortality == mortality_rate, "Correct","Incorrect"))

res <- combined_SEPA_SALSCOT_2018_df %>% mutate("CorrectOrNot" = ifelse(round(monthly_mortality,0) == round(mortality_rate,0), "Correct","Incorrect"))

summary(as.factor(res$CorrectOrNot))

res$diff_morts <- res$monthly_mortality - res$mortality_rate
summary(res$diff_morts[res$CorrectOrNot=="Incorrect"])
plot(res$diff_morts[res$CorrectOrNot=="Incorrect" & res$diff_morts > 1 | res$diff_morts < - 1 ])
summary(res$diff_morts[res$CorrectOrNot=="Incorrect" & res$diff_morts > 2 | res$diff_morts < - 2 ])
length((res$diff_morts[res$CorrectOrNot=="Incorrect" & res$diff_morts > 2 | res$diff_morts < - 2 ]))
site_list <- unique(salmon_df_joined_2018$site_number)

combined_SEPA_SALSCOT_2018_df$diff_morts <- combined_SEPA_SALSCOT_2018_df$monthly_mortality - combined_SEPA_SALSCOT_2018_df$mortality_rate 
res <- combined_SEPA_SALSCOT_2018_df %>% mutate("CorrectOrNot" = ifelse(combined_SEPA_SALSCOT_2018_df$diff_morts > 2 | combined_SEPA_SALSCOT_2018_df$diff_morts < - 2, "Incorrect","Correct"))

summary(as.factor(res$CorrectOrNot))
summary(res$diff_morts[res$CorrectOrNot=="Incorrect"])
dev.off()

write.csv(res, "./fulldatasetdump_salscot_sepa_fhi.csv")
write.csv(salmon_df_joined_2018, "./SEPA_FhiDatadump.csv")
write.csv(pc_salmon_flagged_18_24, "./Salscotdatadump.csv")

# ---- control-charts-ggplot, echo=FALSE, warning=FALSE, message=FALSE ----

national_average <-mean(salmon_df_active$mortality_rate,  na.rm = TRUE)
mid_thresh <- quantile(salmon_df_active$mortality_rate, 0.75, na.rm = TRUE)
high_thresh <- quantile(salmon_df_active$mortality_rate, 0.95, na.rm = TRUE)
very_high_thresh <- quantile(salmon_df_active$mortality_rate, 0.99, na.rm = TRUE)

output_dir <- "SEPA_SALSCOT_mortality_plots_allsites"  # folder to save plots
dir.create(output_dir, showWarnings = FALSE)  # create folder if not exists
#site_list <- reccurent_sites_2018_v1

plots <- list()

for (i in seq_along(site_list)) {
  site <- site_list[i]

  temp <- combined_SEPA_SALSCOT_2018_df %>%
    filter(site_number == site & !is.na(mortality_rate)) %>%
    arrange(date) %>%
    mutate(
      action_cleaned = case_when(
        grepl("harvest", tolower(action)) ~ "Harvesting",
        is.na(action) | action == "0" | trimws(action) == "" ~ "No Action Reported",
        TRUE ~ "Other"
      ),
      action_cleaned = factor(action_cleaned, levels = c("Harvesting", "No Action Reported", "Other"))
    )  

  if (nrow(temp) > 24) {
    plots[[i]] <- ggplot(temp, aes(x = date, y = mortality_rate)) +
      geom_point(aes(shape = action_cleaned, color = factor(stock_cycle)), size = 2) +
      geom_point(aes(x = date, y = adjusted_mortality_rate,  shape = "adjusted mortality rate", size = 2, alpha = 0.6,  color = factor(stock_cycle))) +
      geom_point( aes(x = date, y = monthly_mortality, color = id))  +
       geom_hline(yintercept = high_thresh, linetype = "dashed", color = "red", size = 1) +
      geom_hline(yintercept = national_average, linetype = "solid", color = "black", size = 0.75) +
      geom_hline(yintercept = very_high_thresh, linetype = "dashed", color = "darkred", size = 0.75) +
      labs(
        title = paste("Mortality Control Chart -", site),
        x = "Date", y = "Mortality Rate (%)",
        shape = "Action Reported",
        color = "Stock Cycle"
      ) +
      theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5)) +
      theme(legend.position = "right") +
      theme(legend.position = "right")
    
    # Save each plot
    ggsave(
      filename = file.path(output_dir, paste0("mortality_chart_", site, ".png")),
      plot = plots[[i]],
      width = 14, height = 10, dpi = 300
    )
  }
}

# Display all plots
plots
# ---- compare-high-cycles, fig.width=12, fig.height=12,echo=FALSE ----
#salscot_high_cycles <- pc_check_for_persistent_sites_18_24 %>%
#  count(site_number) %>%
#  filter(n > 1) %>%
#  pull(site_number) # Salcot
print(paste0("SalScot Site numbers with reccurent mortality in more than 1 production cycle (2018 - 2024)"))
pc_check_for_persistent_sites_18_24

