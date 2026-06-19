# Data exploration and baseline summaries.
# Run after R/01_data_ingest_prepare.R.
source("R/00_packages.R")
ensure_project_dirs()
# ---- seasonality-heatmap,  echo=FALSE ----
print(nrow(salmon_df_active))
unique_sites_active <- unique(salmon_df_active$site_number)
print(nrow(as.data.frame(unique_sites_active)))

print(nrow(salmon_df_joined_2018))

unique_sites_active_2018 <- unique(salmon_df_joined_2018$site_number)
print(nrow(as.data.frame(unique_sites_active_2018)))

summary_stats_salmon <- salmon_df_active %>%
    dplyr::reframe( n_observations = n(),
                    mean = round(mean(mortality_rate, na.rm = TRUE),2),
                     median = round(median(mortality_rate, na.rm = TRUE),2),
                    IQR = round(IQR(mortality_rate, na.rm = TRUE),2),
                     p95 = round(quantile(mortality_rate, 0.95, na.rm = TRUE),2),
                     p99 = round(quantile(mortality_rate, 0.99, na.rm = TRUE),2)) %>%
  gt() %>%
  tab_header(title = "Mortality Summary - 2003-2024 SEPA data")

summary_stats_salmon_2018 <- salmon_df_joined_2018 %>%
    dplyr::reframe( n_observations = n(),
                    mean = round(mean(mortality_rate, na.rm = TRUE),2),
                    median = round(median(mortality_rate, na.rm = TRUE),2),
                    IQR = round(IQR(mortality_rate, na.rm = TRUE),2),
                    p95 = round(quantile(mortality_rate, 0.95, na.rm = TRUE),2),
                    p99 = round(quantile(mortality_rate, 0.99, na.rm = TRUE),2)) %>%
  gt() %>%
  tab_header(title = "Mortality Summary - SEPA data 2018-2024")


# Ensure month is ordered factor for correct plotting
salmon_df_active$month_f <- month.abb[salmon_df_active$month]
summary(salmon_df_active)
# Aggregate average mortality by year and month
monthly_heatmap <- salmon_df_active %>%
  group_by(year, month, month_f) %>%
  summarise(avg_mortality = mean(mortality_rate, na.rm = TRUE))
monthly_heatmap$Month <- factor(monthly_heatmap$month_f, levels = c("Jan", "Feb", "Mar", "Apr",  "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
# Heatmap
heatmap <-   ggplot(monthly_heatmap, aes(x = factor(Month), 
                                         y = factor(year), 
                                         fill = avg_mortality)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "C", name = "Avg Mortality (%)") +
  labs(title = "Heatmap of Average Monthly Mortality (Salmon)",
       x = "Month",
       y = "Year") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5))

# Save as PNG (300 dpi, 8x6 inches)
ggsave("fig1_mortality_heatmap.png", plot = heatmap,
       width = 8, height = 6, dpi = 300, units = "in")

# Save as TIFF (publication-grade)
ggsave("fig1_mortality_heatmap.tiff", plot = heatmap,
       width = 8, height = 6, dpi = 600, units = "in", compression = "lzw")
heatmap


greyscale_heatmap <-  ggplot(monthly_heatmap, aes(x = factor(Month), y = factor(year), fill = avg_mortality)) +
  geom_tile(color = "white") +
  scale_fill_gradient(
    low = "grey90", high = "black",
    name = "Avg Mortality (%)"
  ) +
  labs(
    title = "Heatmap of Average Monthly Mortality (Salmon)",
    x = "Month",
    y = "Year"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
  )


heatmapV1 <- ggplot(monthly_heatmap, aes(x = factor(Month), y = factor(year), fill = avg_mortality)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(
    colours = c("grey95", "grey75", "grey55", "grey35", "grey15"),
    name = "Avg Mortality (%)"
  ) +
  labs(
    title = "Heatmap of Average Monthly Mortality (Salmon)",
    x = "Month",
    y = "Year"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    panel.grid = element_blank()
  )

# Save as PNG (300 dpi, 8x6 inches)
ggsave("fig1_mortality_heatmap_BW.png", plot = heatmapV1,
       width = 8, height = 6, dpi = 300, units = "in")

# Save as TIFF (publication-grade)
ggsave("fig1_mortality_heatmapBW.tiff", plot = heatmapV1,
       width = 8, height = 6, dpi = 600, units = "in", compression = "lzw")

summary_stats_salmon
summary_stats_salmon_2018


# Aggregate average mortality by year and month
yearly_heatmap <- salmon_df_active %>%
  group_by(year) %>%
  summarise(avg_mortality =round(mean(mortality_rate, na.rm = TRUE), 2),
            sd_mortality = round(sd(mortality_rate, na.rm=TRUE), 2))

write.csv(yearly_heatmap, "mortality_summary_by_year_SEPA.csv", row.names = FALSE)



yearly_summary<-salmon_df_active %>%
  group_by(year) %>%
  summarise(
    Mean = round(mean(mortality_rate, na.rm = TRUE),2),
    Median = round(median(mortality_rate, na.rm = TRUE),2),
    IQR = round(IQR(mortality_rate, na.rm = TRUE),2),
    P95 = round(quantile(mortality_rate, 0.95, na.rm = TRUE),2),
    P99 = round(quantile(mortality_rate, 0.99, na.rm = TRUE),2),
    Observations = n()
  ) 

write.csv(yearly_summary, "mortality_summary_by_year_SEPA.csv", row.names = FALSE)

# ---- survey-region-table, echo=FALSE ----
# Combine regions
salmon_df_active <- salmon_df_active %>%
  mutate(region_grouped = case_when(
    survey_region %in% c("Highland", "North West") ~ "Highland & NW",
    survey_region %in% c("South West", "Strathclyde") ~ "South West & Strathclyde",
    TRUE ~ survey_region
  ))

salmon_df_joined_2018<- salmon_df_joined_2018 %>%
  mutate(region_grouped = case_when(
    survey_region %in% c("Highland", "North West") ~ "Highland & NW",
    survey_region %in% c("South West", "Strathclyde") ~ "South West & Strathclyde",
    TRUE ~ survey_region
  ))
# Step 1: Create the summary table (as a regular data frame or tibble)
mortality_summary <- salmon_df_active %>%
  group_by(region_grouped, site_number) %>%
  summarise(
    Mean = round(mean(mortality_rate, na.rm = TRUE), 2),
    Median = round(median(mortality_rate, na.rm = TRUE), 2),
    IQR = round(IQR(mortality_rate, na.rm = TRUE), 2),
    P95 = round(quantile(mortality_rate, 0.95, na.rm = TRUE), 2),
    P99 = round(quantile(mortality_rate, 0.99, na.rm = TRUE), 2),
    start_year = min(year), 
    final_year = max(year),
    Observations = n(),
    .groups = "drop"
  )

# Step 2: Save it to a CSV file
write.csv(mortality_summary, "mortality_summary_by_region_site.csv", row.names = FALSE)

# Step 3: (Optional) Create the gt table for display in the Markdown output
#mortality_summary %>%
#  gt() %>%
#  tab_header(title = "Monthly Mortality % Summary by Survey #Region/Site 2003â€“2024")

salmon_df_active %>%
  group_by(region_grouped) %>%
  summarise(
    Mean = round(mean(mortality_rate, na.rm = TRUE),2),
    Median = round(median(mortality_rate, na.rm = TRUE),2),
    IQR = round(IQR(mortality_rate, na.rm = TRUE),2),
    P95 = round(quantile(mortality_rate, 0.95, na.rm = TRUE),2),
    P99 = round(quantile(mortality_rate, 0.99, na.rm = TRUE),2),
    Observations = n()
  ) %>%
  gt() %>%
  tab_header(title = "Montly Mortality % Summary by Survey Region 2003-2024")


# national stats 
salmon_df_active %>%
  summarise(
    Mean = round(mean(mortality_rate, na.rm = TRUE),2),
    Median = round(median(mortality_rate, na.rm = TRUE),2),
    IQR = round(IQR(mortality_rate, na.rm = TRUE),2),
    P95 = round(quantile(mortality_rate, 0.95, na.rm = TRUE),2),
    P99 = round(quantile(mortality_rate, 0.99, na.rm = TRUE),2),
    Observations = n()
  ) %>%
  gt() %>%
  tab_header(title = "Montly Mortality % Summary (national average  2003-2024")
#Step 1: Create the summary table (as a regular data frame or tibble)
salmon_df_joined_2018 %>%
  group_by(region_grouped) %>%
  summarise(
    Mean = round(mean(mortality_rate, na.rm = TRUE),2),
    Median = round(median(mortality_rate, na.rm = TRUE),2),
    IQR = round(IQR(mortality_rate, na.rm = TRUE),2),
    P95 = round(quantile(mortality_rate, 0.95, na.rm = TRUE),2),
    P99 = round(quantile(mortality_rate, 0.99, na.rm = TRUE),2),
    Observations = n()
  ) %>%
  gt() %>%
  tab_header(title = "Monthly Mortality % Summary (regional 2018-2024)")
# Step 2: Save it to a CSV file


yearly_summary<-salmon_df_joined_2018 %>%
  #group_by(year) %>%
  summarise(
    Mean = round(mean(mortality_rate, na.rm = TRUE),2),
    Median = round(median(mortality_rate, na.rm = TRUE),2),
    IQR = round(IQR(mortality_rate, na.rm = TRUE),2),
    P95 = round(quantile(mortality_rate, 0.95, na.rm = TRUE),2),
    P99 = round(quantile(mortality_rate, 0.99, na.rm = TRUE),2),
    Observations = n()
  ) 

# Step 2: Save it to a CSV file
write.csv(yearly_summary, "mortality_summary_by_year.csv", row.names = FALSE)

# Step 3: (Optional) Create the gt table for display in the Markdown output
yearly_summary%>%
 gt() %>%
  tab_header(title = "Monthly Mortality % Summary by Year #(2018-2024)")



yearly_summary<-salmon_df_joined_2018 %>%
  group_by(year) %>%
  summarise(
    Mean = round(mean(mortality_rate, na.rm = TRUE),2),
    Median = round(median(mortality_rate, na.rm = TRUE),2),
    IQR = round(IQR(mortality_rate, na.rm = TRUE),2),
    P95 = round(quantile(mortality_rate, 0.95, na.rm = TRUE),2),
    P99 = round(quantile(mortality_rate, 0.99, na.rm = TRUE),2),
    Observations = n()
  ) 
write.csv(yearly_summary, "mortality_summary_by_year_SalScot.csv", row.names = FALSE)

# ---- baseline-kde-SEPA, echo=FALSE ----
quantiles <- quantile(salmon_df_active$mortality_rate, probs = c(0.5, 0.75, 0.95, 0.99), na.rm = TRUE)

length(salmon_df_active$mortality_rate[salmon_df_active$mortality_rate>1.47])/length(salmon_df_active$mortality_rate)



length(salmon_df_joined_2018$mortality_rate[salmon_df_joined_2018$mortality_rate>1.47])/length(salmon_df_joined_2018$mortality_rate)

mort_plot <- salmon_df_active %>%
  ggplot(aes(x = log(mortality_rate))) +
  geom_density(fill = "lightblue", alpha = 0.6) +
  geom_vline(xintercept = log(quantiles), 
             linetype = "dashed", 
             color = c("blue", "green", "orange", "red")) +
  annotate("text", x = log(quantiles), y = 0.01, 
           label = names(quantiles), angle = 90, vjust = -0.5) +
  labs(title = "a.) Kernel Density of Monthly Mortality Rates (SEPA, Salmon, 2003â€“2024)",
       x = "Logged Mortality Rate (%)", y = "Density") +
  theme_minimal()

# Save as PNG (300 dpi, 8x6 inches)
ggsave("fig2a_mortality_density.png", plot = mort_plot,
       width = 8, height = 6, dpi = 300, units = "in")

# Save as TIFF (publication-grade)
ggsave("fig2a_mortality_density.tiff", plot = mort_plot,
       width = 8, height = 6, dpi = 600, units = "in", compression = "lzw")
mort_plot
# ---- kde-quantiles-SalScot, echo=FALSE ----
quantiles <- quantile(pc.dat$monthly_mortality, probs = c(0.5, 0.75, 0.95, 0.99), na.rm = TRUE)


length(pc.dat$monthly_mortality[pc.dat$monthly_mortality>1.47])/length(pc.dat$monthly_mortality)

mort_plot_salscot <- pc.dat %>%
  ggplot(aes(x = log(monthly_mortality))) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  geom_vline(xintercept = log(quantiles), linetype = "dashed", color = c("blue", "green", "orange", "red")) +
  annotate("text", x = log(quantiles), y = 0.01, label = names(quantiles), angle = 90, vjust = -0.5) +
  labs(title = "b.) Kernel Density of Monthly Mortality Rates (Salmon Scotland, 2018â€“2024)",
       x = "Logged Monthly Mortality (%)", y = "Density") +
  theme_minimal()

# Save as PNG (300 dpi, 8x6 inches)
ggsave("fig2b_mortality_density.png", plot = mort_plot_salscot,
       width = 8, height = 6, dpi = 300, units = "in")

# Save as TIFF (publication-grade)
ggsave("fig2n_mortality_density.tiff", plot = mort_plot_salscot,
       width = 8, height = 6, dpi = 600, units = "in", compression = "lzw")
mort_plot_salscot

summary_stats_salmonscot <- pc.dat %>%
    dplyr::reframe( n_observations = n(),
                    mean = round(mean(monthly_mortality, na.rm = TRUE),2),
                     median = round(median(monthly_mortality, na.rm = TRUE),2),
                    IQR = round(IQR(monthly_mortality, na.rm = TRUE),2),
                     p95 = round(quantile(monthly_mortality, 0.95, na.rm = TRUE),2),
                     p99 = round(quantile(monthly_mortality, 0.99, na.rm = TRUE),2)) %>%
  gt() %>%
  tab_header(title = "Mortality Summary - Salmon Scot data")
summary_stats_salmonscot

pc.dat %>%
  group_by(survey_region) %>%
  summarise(
    Mean = round(mean(monthly_mortality, na.rm = TRUE),2),
    Median = round(median(monthly_mortality, na.rm = TRUE),2),
    IQR = round(IQR(monthly_mortality, na.rm = TRUE),2),
    P95 = round(quantile(monthly_mortality, 0.95, na.rm = TRUE),2),
    P99 = round(quantile(monthly_mortality, 0.99, na.rm = TRUE),2),
    Observations = n()
  ) %>%
  gt() %>%
  tab_header(title = "Monthly Mortality % Summary by Survey Region (SalScot)")


yearly_summary_salscot<-pc.dat %>%
  group_by(year) %>%
  summarise(
    Mean = round(mean(monthly_mortality, na.rm = TRUE),2),
    Median = round(median(monthly_mortality, na.rm = TRUE),2),
    IQR = round(IQR(monthly_mortality, na.rm = TRUE),2),
    P95 = round(quantile(monthly_mortality, 0.95, na.rm = TRUE),2),
    P99 = round(quantile(monthly_mortality, 0.99, na.rm = TRUE),2),
    Observations = n()
  ) 
# Step 2: Save it to a CSV file
write.csv(yearly_summary_salscot, "mortality_summary_by_year_salscot.csv", row.names = FALSE)

# Step 3: (Optional) Create the gt table for display in the Markdown output
#yearly_summary_salscot%>%
#  gt() %>%
#  tab_header(title = "Monthly Mortality % Summary by Year (SalScot #2018-2024)")


# ---- alt_kde_salscot echo=FALSE ----
mort_plot_salscot <- pc.dat %>%
  ggplot(aes(x = (monthly_mortality))) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  geom_vline(xintercept = quantiles, linetype = "dashed", color = c("blue", "green", "orange", "red")) +
  annotate("text", x = quantiles, y = 0.01, label = names(quantiles), angle = 90, vjust = -0.5) +
  labs(title = "b.) Kernel Density of Monthly Mortality Rates (Salmon Scotland, 2018â€“2024)",
       x = "Monthly Mortality (%)", y = "Density") +
  theme_minimal()

# Save as PNG (300 dpi, 8x6 inches)
ggsave("fig2b_mortality_density_alt.png", plot = mort_plot_salscot,
       width = 8, height = 6, dpi = 300, units = "in")

# Save as TIFF (publication-grade)
ggsave("fig2n_mortality_density_alt.tiff", plot = mort_plot_salscot,
       width = 8, height = 6, dpi = 600, units = "in", compression = "lzw")
mort_plot_salscot

# ---- clean-up-global-env, echo=FALSE ----
#rm(number_of_sites, quantiles,summary_stats_salmon, summary_stats_salmon_2018, summary_stats_salmonscot, unique_sites_active, unique_sites_active_2018, yearly_summary, yearly_summary_salscot, greyscale_heatmap, heatmap, heatmapV1, mort_plot, mort_plot_salscot)
write.csv(mortality_summary, "Active_SEPA_Site_Summary_Mortality_Stats.csv")
#rm(mortality_summary)
