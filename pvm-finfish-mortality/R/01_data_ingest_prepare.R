# Data ingest and preparation.
# Expected input CSVs live in data/. See data/README.md for filenames.
source("R/00_packages.R")
ensure_project_dirs()
# ---- load-salmon-scot-data, echo=FALSE ----
#SalScot - 
pc.dat <- read.csv("data/Mortality_data_for_full_production_cycles_2018-2024_v2.csv")
pc.dat$date <- dmy(pc.dat$date)
#pc.dat <- pc.dat %>% filter(prod_cycle_len >11 | prod_cycle_len <21 ) #SI recokns this is not required

# ---- load-and-prep-fhi-data, echo=FALSE ----

# fhi data

fhi_data <-  read.csv("data/FHI 066 Mortality events 16092025_grouped_causes.csv")
                      
                      #MRT_copy_17-6-10.40_withAIinference_on_MortCat_weights_fixed.csv")

# drop irrelevan cols
cols_to_drop <- c(
 "Entered.by.Inspector" , "Data.checked..2025.onwards.", "Reporting.Business.No")

# sort out chr cols

fhi_data <- fhi_data %>%
  mutate(
    Mortality_rate_clean = Mortality.rate.recorded... %>%
      gsub("%", "", .) %>%               # remove % if present
      trimws() %>%                       # remove leading/trailing whitespace
      na_if("") %>%                      # convert empty strings to NA
      as.numeric()                       # convert to numeric
  )

fhi_data <- fhi_data %>%
  mutate(
    Total_mortality_clean = Total.mortality.during.event %>%
      gsub(",", "", .) %>%              # remove thousands separator
      trimws() %>%                      # trim whitespace
      na_if("") %>%                     # convert empty strings to NA
      as.numeric()                      # convert to numeric
  )

fhi_clean <- fhi_data %>%
  select(-all_of(cols_to_drop))

#filter for sal and seawater
fhi_clean <- fhi_clean %>%
  filter(Species == "SAL" & Water.Type =="SW")

fhi_clean <- fhi_clean %>%
  mutate(
    Total_mortality_during_event = gsub(",", "", Total.mortality.during.event),
    Total_mortality_during_event = as.numeric(trimws(Total.mortality.during.event))
  )

extract_numeric_grams <- function(x) {
  x <- tolower(x)
  x <- gsub("[^0-9\\.kgmg<>]", "", x)  # Remove unexpected text
  x <- gsub("<", "", x)
  # Convert kg â†’ g
  if (grepl("kg", x)) {
    as.numeric(gsub("kg", "", x)) * 1000
  } else if (grepl("g", x)) {
    as.numeric(gsub("g", "", x))
  } else {
    as.numeric(x)
  }
}

fhi_clean <- fhi_clean %>%
  mutate(
    Size_of_fish_g = sapply(Size.of.fish, extract_numeric_grams),
    Avg_weight_affected_pop_g = sapply(Average.weight.of.affected.population, extract_numeric_grams)
  )
#glimpse(fhi_clean)
#summary(fhi_clean$Total_mortality_during_event)
fhi_clean$Date.reported <- dmy(fhi_clean$Date.reported)
fhi_clean$Start.Date <- dmy(fhi_clean$Start.Date)
fhi_clean$End.Date <- dmy(fhi_clean$End.Date)
str(fhi_clean$Date.reported)
fhi_clean <- fhi_clean %>%
  mutate(
    #Date_reported = dmy(Date.reported),
    year_RD = year(Date.reported),
    month_RD = month(Date.reported),
    site_number = SiteNo
  )
fhi_clean <- fhi_clean %>%
  mutate(
   # Start_Date = dmy(Start.Date),
    year_SD = year(Start.Date),
    month_SD = month(Start.Date),
  )
str(fhi_clean[, c("site_number", "year_SD", "month_SD")])
fhi_clean <- fhi_clean %>%
  mutate(
   # End_Date = dmy(End.Date),
    year_ED = year(End.Date),
    month_ED = month(End.Date),
  )

fhi_clean <- fhi_clean %>%
  mutate(
   # Start_Date = dmy(Start.Date),
    year = year(Start.Date),
    month = month(Start.Date),
  )

str(fhi_clean[, c("site_number", "year", "month")])
#plot(fhi_clean$Start_Date, fhi_clean$Date_reported)
#plot(fhi_clean$Start_Date, fhi_clean$End_Date, col="red")
#print(summary(fhi_clean$year_ED == fhi_clean$year_SD))
end_year_is_different<-subset(fhi_clean, !(fhi_clean$year_ED == fhi_clean$year_SD))
write.csv(end_year_is_different, "end_year_is_different_fhi.csv")
#print(summary(fhi_clean$month_ED == fhi_clean$month_SD))
end_month_is_different<-subset(fhi_clean, !(fhi_clean$month_ED == fhi_clean$month_SD))
write.csv(end_month_is_different, "end_month_is_different_fhi.csv")
end_date_earlier_than_start_date<- subset(fhi_clean, End.Date<Start.Date)
write.csv(end_date_earlier_than_start_date, "end_date_earlier_than_start_date.csv")

#summary(fhi_clean)
fhi_monthly_summary <- fhi_clean %>%
  group_by(site_number, year, month) %>%
  summarise(
    n_events_fhi = n(),
    average_size_of_fish_g_fhi = mean(Avg_weight_affected_pop_g, na.rm=TRUE),
    mortality_rate_average_fhi = mean(Mortality_rate_clean, na.rm=TRUE),
    mortality_rate_cumulative_fhi = sum(Mortality_rate_clean, na.rm=TRUE),
    total_mortality_rate_fhi = sum(Mortality_rate_clean, na.rm=TRUE),
    total_reported_mortality_fhi = sum(Total_mortality_during_event, na.rm = TRUE),
    top_cause_1 = paste0(unique(Grouped_Mortality_Cause_1[Grouped_Mortality_Cause_1!= ""]), collapse = "; "),
    top_cause_2 = paste0(unique(Grouped_Mortality_Cause_2[Grouped_Mortality_Cause_2!= ""]), collapse = "; "),
    top_cause_3 = paste0(unique(Grouped_Mortality_Cause_3[Grouped_Mortality_Cause_3!= ""]), collapse = "; "),
    action = paste0(unique(Production_to_Harvest_or_Cull[Production_to_Harvest_or_Cull!= ""]), collapse = "; "),
    .groups = "drop"
  )

fhi_data_clean <- fhi_clean %>%
 select(SiteNo,Date.reported, Start.Date, End.Date, reasons_1 = Grouped_Mortality_Cause_1, reasons_2 = Grouped_Mortality_Cause_2, reasons_3 = Grouped_Mortality_Cause_3,action= Production_to_Harvest_or_Cull)

fhi_clean$site_number <- fhi_clean$SiteNo
fhi_clean$startdate <- dmy(fhi_clean$Start.Date) # Convert factor to date
fhi_clean$year <- (fhi_clean$year_SD)
fhi_clean$month <- (fhi_clean$month_SD)


# create extra output with further FHI information for SJ's checks
fhi_monthly_summary_plus <- fhi_clean %>%
  group_by(site_number, year, month) %>%
  summarise(
    n_events_fhi = n(),
    average_size_of_fish_g_fhi = mean(Avg_weight_affected_pop_g, na.rm=TRUE),
    mortality_rate_average_fhi = mean(Mortality_rate_clean, na.rm=TRUE),
    mortality_rate_cumulative_fhi = sum(Mortality_rate_clean, na.rm=TRUE),
    total_mortality_rate_fhi = sum(Mortality_rate_clean, na.rm=TRUE),
    total_reported_mortality_fhi = sum(Total_mortality_during_event, na.rm = TRUE),
    top_cause_1 = paste0(unique(Grouped_Mortality_Cause_1[Grouped_Mortality_Cause_1 != ""]), collapse = "; "),
    top_cause_2 = paste0(unique(Grouped_Mortality_Cause_2[Grouped_Mortality_Cause_2 != ""]), collapse = "; "),
    top_cause_3 = paste0(unique(Grouped_Mortality_Cause_3[Grouped_Mortality_Cause_3 != ""]), collapse = "; "),
    action = paste0(unique(Production_to_Harvest_or_Cull[Production_to_Harvest_or_Cull != ""]), collapse = "; "),
    All_Mortality_Event_No = paste0(unique(Mortality.Event.No[Mortality.Event.No != ""]), collapse = "; "),
    Weekly_or_5_weekly = paste0(unique(Weekly.or.5.weekly.[Weekly.or.5.weekly. != ""]), collapse = "; "),
    Freetext_reasons = paste0(unique(Explained..reasons[Explained..reasons != ""]), collapse = "; "),
    Freetext_unexplained_obs = paste0(unique(Unexplained.observations[Unexplained.observations != ""]), collapse = "; "),
    Freetext_Additional_info = paste0(unique(Additional.information[Additional.information != ""]), collapse = "; "),
    Freetext_action_taken_fhi = paste0(unique(Action.taken.by.FHI[Action.taken.by.FHI != ""]), collapse = "; "),
    .groups = "drop"
  )

rm(fhi_data, end_date_earlier_than_start_date, end_month_is_different,     end_year_is_different, fhi_data_clean, fhi_monthly_summary, cols_to_drop)
# ---- load-and-prep-sepa-data, echo=FALSE ----
dat <- read.csv("data/SEPA_mortality_all_data.csv")
dat$date <- dmy(dat$year) # Convert factor to date
dat$year <- year(dat$date)
dat$month <- month(dat$date)#, label = TRUE, abbr = TRUE)
dat <- dat %>%
  mutate(mortality_rate = ifelse(actual_biomass_on_site_tonnes > 0,
                                 (mortalities_kilograms / (actual_biomass_on_site_tonnes * 1000)) * 100,
                                 NA))


# Count distinct licence_active_at_report per site
licence_conflicts <- dat %>%
  group_by(site_number) %>%
  summarise(
    n_licences = n_distinct(licence_active_at_report),
    licences = paste(unique(licence_active_at_report), collapse = "; ")
  ) %>%
  filter(n_licences > 1)

# View any sites with >1 licence reported
print(licence_conflicts)


# subest salmon
salmon_df <- dat %>% filter((species_farmed == "SAL" | 
                             species_farmed == "SAL/COD/HAL/HAD/TR" | 
                             species_farmed == "SAL/COD/HAL/TRO" | 
                             species_farmed == "SAL/LUM/WRS"))
print(nrow(salmon_df)) # subset for farmed species

salmon_df_1 <- salmon_df %>% filter((mortality_rate < 100 | is.na(mortality_rate))                               & year > 2002)
# provide some info for write up
print(paste0("number of data rows ", min(salmon_df_1$year), "-", max(salmon_df_1$year)))
print(nrow(salmon_df_1)) # subset for 2003 onwards removing anomilies
print(length(unique(salmon_df_1$site_name))) 
#find "no number sites"
check_number<- salmon_df_1 %>% filter((site_number == "no number"|site_number =="No number"))
#22 years 1 vaule per site per month 
22*12*355
#print(nrow(check_number)) # subset for farmed species

summary(as.factor(check_number$sepa_site))
summary(as.factor(check_number$local_authority))

salmon_df_1$site_number[salmon_df_1$sepa_site=="ARDM1"] <- "XX001"
salmon_df_1$site_number[salmon_df_1$sepa_site=="BLMQ1"] <- "XX002"
salmon_df_1$site_number[salmon_df_1$sepa_site=="BTW1"] <- "XX003"
salmon_df_1$site_number[salmon_df_1$sepa_site=="FFMC17"] <- "XX004"
salmon_df_1$site_number[salmon_df_1$sepa_site=="FFMC78"] <- "XX005"
salmon_df_1$site_number[salmon_df_1$sepa_site=="FFMC79"] <- "XX006"
salmon_df_1$site_number[salmon_df_1$sepa_site=="GON1"] <- "XX007"
salmon_df_1$site_number[salmon_df_1$sepa_site=="GVRW1"] <- "FS0242"
salmon_df_1$site_number[salmon_df_1$sepa_site=="HMR1"] <- "XX009"
salmon_df_1$site_number[salmon_df_1$sepa_site=="MIL1"] <- "XX010"
salmon_df_1$site_number[salmon_df_1$sepa_site=="NKBN1"] <- "FS1365"
salmon_df_1$site_number[salmon_df_1$sepa_site=="NRAS1"] <- "XX012"
salmon_df_1$site_number[salmon_df_1$sepa_site=="RUIN1"] <- "XX013"
salmon_df_1$site_number[salmon_df_1$sepa_site=="SNIZ1"] <- "XX014"
salmon_df_1$site_number[salmon_df_1$sepa_site=="TOL1"] <- "XX015"
salmon_df_1$site_number[salmon_df_1$sepa_site=="USH1"] <- "XX016"
salmon_df_1$site_number[salmon_df_1$sepa_site=="VUMN1"] <- "XX017"
salmon_df_1$site_number[salmon_df_1$sepa_site=="WGH1"] <- "XX018"
salmon_df_1$site_number[salmon_df_1$sepa_site=="WGR1"] <- "XX019"

# provide some info for write up
# Count number of records per site per year-month
duplicate_licence <- salmon_df_1 %>%
  group_by(site_number, year, month) %>%
  tally() %>%
  filter(n > 1)

# View duplicates
print(duplicate_licence)

dat_summarised <- salmon_df_1 %>%
  group_by(site_number, year, month) %>%
  summarise(
    date = min(date),
    sepa_site = first(sepa_site),
    site_name = first(site_name),
    operator = first(operator),
    receiving_water = first(receiving_water),
    region = first(survey_region),
    active_licence_number = paste(unique(active_licence_number), collapse = "; "),
    licence_active_at_report = paste(unique(licence_active_at_report), collapse = "; "),
    actual_biomass_on_site_tonnes = sum(actual_biomass_on_site_tonnes, na.rm = TRUE),
    mortalities_kilograms = sum(mortalities_kilograms, na.rm = TRUE),
    feed_kilograms = sum(feed_kilograms, na.rm = TRUE),
    deltamethrin_grams = sum(deltamethrin_grams, na.rm = TRUE),
    cypermethrin_grams = sum(cypermethrin_grams, na.rm = TRUE),
    azamethiphos_grams = sum(azamethiphos_grams, na.rm = TRUE),
    emamectin_benzoate_grams = sum(emamectin_benzoate_grams, na.rm = TRUE),
    mortality_rate = mean(mortality_rate, na.rm = TRUE),  # or weighted avg if needed
    n_records = n(),
    .groups = "drop"
  )
# Add QA flag
dat_summarised <- dat_summarised %>%
  mutate(
    multiple_licences = grepl(";", licence_active_at_report),
    anomaly_flag = ifelse(n_records > 1 | multiple_licences, TRUE, FALSE)
  )
summary(dat_summarised$multiple_licences)

# print QA
write.csv(licence_conflicts, "outputs/QA_checks/qa_flag_licence_conflicts.csv", row.names = FALSE)
write.csv(duplicate_licence, "outputs/QA_checks/qa_flag_month_duplicates.csv", row.names = FALSE)

dat_summarised <- dat_summarised %>%
  mutate(mortality_rate = ifelse(actual_biomass_on_site_tonnes > 0,
                                 (mortalities_kilograms / (actual_biomass_on_site_tonnes * 1000)) * 100,
                                 NA))


# create extra output with SEPA data as monthly values no dubfor SJ's checks
dat_one_value_per_month <- salmon_df_1 %>%
  group_by(site_number, year, month) %>%
  summarise(
    date = min(date),
    sepa_site = paste(unique(sepa_site), collapse = "; "),
    site_name = paste(unique(site_name), collapse = "; "),
    operator = paste(unique(operator), collapse = "; "),
    receiving_water = paste(unique(receiving_water), collapse = "; "),
    survey_region = paste(unique(survey_region), collapse = "; "),
    active_licence_number = paste(unique(active_licence_number), collapse = "; "),
    licence_active_at_report = paste(unique(licence_active_at_report), collapse = "; "),
    actual_biomass_on_site_tonnes = sum(actual_biomass_on_site_tonnes, na.rm = TRUE),
    mortalities_kilograms = sum(mortalities_kilograms, na.rm = TRUE),
    #feed_kilograms = sum(feed_kilograms, na.rm = TRUE),
    #deltamethrin_grams = sum(deltamethrin_grams, na.rm = TRUE),
    #cypermethrin_grams = sum(cypermethrin_grams, na.rm = TRUE),
    #azamethiphos_grams = sum(azamethiphos_grams, na.rm = TRUE),
    #emamectin_benzoate_grams = sum(emamectin_benzoate_grams, na.rm = TRUE),
    #mortality_rate = mean(mortality_rate, na.rm = TRUE),  # or weighted avg if needed
    n_records = n(),
    .groups = "drop"
  )

dat_one_value_per_month <- dat_one_value_per_month %>%
  mutate(mortality_rate = ifelse(actual_biomass_on_site_tonnes > 0,
                                 (mortalities_kilograms / (actual_biomass_on_site_tonnes * 1000)) * 100,
                                 NA))
#print(nrow(salmon_df_1))  # subset for year above 2002 and morts less than 100%
print(paste0("number of data rows containing biomass greater than 0 (FALSE)", min(salmon_df_1$year), "-", max(salmon_df_1$year)))
print(summary(as.factor(salmon_df_1$actual_biomass_on_site_tonnes==0)))

#print((salmon_df_1)) # subset for 2003 onwards removing anomilies
number_of_sites <- (unique(salmon_df_1$site_number))
print(paste0("number of unique sites ", min(salmon_df_1$year), "-", max(salmon_df_1$year)))
print(length(number_of_sites))  # subset for year above 2002 and morts less than 100%

# add in estimated stocking cycle info

salmon_df_2 <- dat_one_value_per_month %>%
  arrange(site_number, date) %>%
  group_by(site_number) %>%
  mutate(
    biomass_zero = actual_biomass_on_site_tonnes == 0,
    biomass_lag  = lag(biomass_zero, default = TRUE),
    cycle_boundary = biomass_lag & !biomass_zero,
    stock_cycle = cumsum(cycle_boundary)
  ) %>%
  ungroup() %>%
  group_by(site_number, stock_cycle) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(
    stock_cycle_month_n = row_number(),
    cycle_start_date = min(date[actual_biomass_on_site_tonnes > 0], na.rm = TRUE),
    cycle_end_date = max(date[actual_biomass_on_site_tonnes > 0], na.rm = TRUE)
  ) %>%
  ungroup()

###############################################
# check this estimate against data JF provided 
##################################################
StockingFallowMorts<- read.csv("./data/StockingFallowMorts-SEPA_checked.csv")

str(StockingFallowMorts)

#Step 1: Preprocess StockingFallowMorts
#Make sure dates are in proper format, and extract #month/year:
# Clean and parse date columns
StockingFallowMorts_clean <- StockingFallowMorts %>%
  filter(!is.na(Month.beginning), Month.beginning >= "01/01/2019") %>%
  mutate(
    sepa_site = Site.ID,
    month_date = dmy(Month.beginning),
    stocking_date = dmy(Stocking.Date),
    fallow_date = dmy(Fallow.Date)
  )

# Ensure salmon_df_2 dates are Date class and minimal per cycle
salmon_check <- salmon_df_2 %>%
  filter(year >= "2019") %>%
  group_by(site_number,sepa_site, stock_cycle) %>%
  summarise(
    cycle_start_date = min(date, na.rm = TRUE),
    cycle_end_date = max(date, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(sepa_site = sepa_site)  # Align site name key

# Join by site + date proximity (same month)
stock_cycle_check <- StockingFallowMorts_clean %>%
  inner_join(salmon_check, by = "sepa_site") %>%
  filter(month(month_date) == month(cycle_start_date) & year(month_date) == year(cycle_start_date)) %>%
  mutate(
    match_stocking = !is.na(stocking_date) & floor_date(stocking_date, "month") == floor_date(cycle_start_date, "month"),
    match_fallow = !is.na(fallow_date) & floor_date(fallow_date, "month") == floor_date(cycle_end_date, "month")
  )

cycle_match_summary <- stock_cycle_check %>%
  summarise(
    total_checked = n(),
    stocking_matches = sum(match_stocking, na.rm = TRUE),
    fallow_matches = sum(match_fallow, na.rm = TRUE),
    full_matches = sum(match_stocking & match_fallow, na.rm = TRUE)
  )

mismatch_flags <- stock_cycle_check %>%
  filter(!(match_stocking & match_fallow)) %>%
  select(sepa_site, month_date, cycle_start_date, cycle_end_date, stocking_date, fallow_date, match_stocking, match_fallow)

write.csv(mismatch_flags, "outputs/QA_checks/qa_flag_cycle_mismatch_SEPA_reportedand_MM_Estimated.csv", row.names = FALSE)

#add flag for  decreasing biomass within a cycle
#This logic checks whether biomass ever decreases #month-to-month in a given cycle
salmon_df_2 <- salmon_df_2 %>%
  arrange(site_number, stock_cycle, date) %>%
  group_by(site_number, stock_cycle) %>%
  mutate(
    previous_biomass = lag(actual_biomass_on_site_tonnes),
    biomass_change = actual_biomass_on_site_tonnes - previous_biomass,
    biomass_decrease_flag = !is.na(previous_biomass) & biomass_change < 0
  ) %>%
  ungroup()
# For each cycle, weâ€™ll create a flag if biomass drops more than mortality alone could explain, suggesting data inconsistency, unreported harvest, or error.


# First, calculate lagged biomass and month-over-month change
#salmon_df_2 <- salmon_df_2 %>%
#  arrange(site_number, stock_cycle, date) %>%
#  group_by(site_number, stock_cycle) %>%
#  mutate(
#    biomass_lag = lag(actual_biomass_on_site_tonnes),
#    biomass_change = actual_biomass_on_site_tonnes - biomass_lag
#  ) %>%
#  ungroup()


# 2. Calculate average biomass growth (positive changes only)

cycle_growth_buffer <- salmon_df_2 %>%
  filter(biomass_change > 0) %>%
  group_by(site_number, stock_cycle) %>%
  summarise(
    avg_growth_tonnes = mean(biomass_change, na.rm = TRUE),
    .groups = "drop"
  )

#3. Join buffer back into main data
salmon_df_2 <- salmon_df_2 %>%
  left_join(cycle_growth_buffer, by = c("site_number", "stock_cycle"))


# 4. Flag unexplained drops using mortality and dynamic buffer

salmon_df_2 <- salmon_df_2 %>%
  group_by(site_number, stock_cycle) %>%
  mutate(
    biomass_tonnes_lag = lag(actual_biomass_on_site_tonnes),
    mortality_kg_lag = lag(mortalities_kilograms),
    mortality_tonnes_lag = mortality_kg_lag / 1000,

    # Allow drop equal to mortality + average growth
    unexplained_biomass_drop = biomass_change < (-mortality_tonnes_lag - avg_growth_tonnes),
    unexplained_biomass_drop = ifelse(is.na(unexplained_biomass_drop), FALSE, unexplained_biomass_drop)
  ) %>%
  ungroup()

#5. Flag cycles with any anomaly

cycle_flags <- salmon_df_2 %>%
  group_by(site_number, stock_cycle) %>%
  summarise(
    cycle_has_unexplained_drop = any(unexplained_biomass_drop, na.rm = TRUE),
    n_flags = sum(unexplained_biomass_drop, na.rm = TRUE),
    .groups = "drop"
  )

write.csv(cycle_flags, "outputs/QA_checks/qa_flag_cylce_unexplained_biomass_drop_SEPA_Data.csv", row.names = FALSE)
# calculate adjusted mortality 

salmon_df_2 <- salmon_df_2 %>%
  group_by(site_number, stock_cycle) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(
    previous_biomass = lag(actual_biomass_on_site_tonnes),
    biomass_decrease_flag == TRUE & !is.na(previous_biomass) & previous_biomass > 0 & actual_biomass_on_site_tonnes < previous_biomass,
    adjusted_mortality_rate = case_when(
      biomass_decrease_flag &
      !is.na(mortality_rate) &
      actual_biomass_on_site_tonnes > 0 ~ 
      (mortalities_kilograms / (previous_biomass * 1000) * 100),
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()


salmon_df_3 <- salmon_df_2 %>% filter(!actual_biomass_on_site_tonnes == 0)
# Find inactive sites with no data after 2018
active_sites <- salmon_df_3 %>%
  filter(year > 2017) %>%
  distinct(site_number) %>%
  pull(site_number)

#active_sites<-(unique(active_sites))

salmon_df_active <- salmon_df_2 %>% 
  filter(site_number %in% active_sites)
# check 
all_sites <- unique(salmon_df_2$site_number)
inactive_sites <- setdiff(all_sites, active_sites)
length(inactive_sites)  # How many dropped
length(active_sites)
#check
salmon_df_active %>%
  group_by(site_number) %>%
  summarise(min_year = min(year), max_year = max(year), .groups = "drop") %>%
  arrange(desc(max_year))


#print(summary(!is.na(salmon_df_active$mortality_rate)))
salmon_df_active <- salmon_df_active  %>% filter(!is.na(mortality_rate))

print(unique(salmon_df_active$site_number))

print(unique(fhi_monthly_summary_plus$site_number))

str(salmon_df_active[, c("site_number", "year", "month")])
str(fhi_monthly_summary_plus[, c("site_number", "year", "month")])

#clean up global environment
rm(salmon_check, salmon_df, salmon_df_1, salmon_df_2, salmon_df_3, stock_cycle_check, mismatch_flags, licence_conflicts, StockingFallowMorts)
rm(check_number, cycle_flags, cycle_growth_buffer, cycle_match_summary, dat_summarised, dat_one_value_per_month, inactive_sites, number_of_sites, duplicate_licence)
rm(active_sites, all_sites, fhi_clean, StockingFallowMorts_clean, dat)
# ---- join SEPA-and-FHI-dat, echo=FALSE ----
salmon_df_joined <- salmon_df_active %>%
  left_join(fhi_monthly_summary_plus, by = c("site_number", "year", "month"))

# make an estimate of number of fish 
salmon_df_joined <- salmon_df_joined %>%
  mutate(
    est_total_fish_count_COGP = actual_biomass_on_site_tonnes * 1e6 / 3300,  # 3300g per fish (COGP values)
    calculated_mortality_rate_COGP = mortality_rate / est_total_fish_count_COGP * 100
  )

salmon_df_joined <- salmon_df_joined %>%
  mutate(est_fish_lost_cogp_size = mortalities_kilograms * 1000 / 3300)
#check cogp V fhi fish lost numbers
salmon_df_joined <- salmon_df_joined %>%
  mutate(est_fish_lost_fhi_size = mortalities_kilograms * 1000 / average_size_of_fish_g_fhi)
#plot.new()
#plot(salmon_df_joined$average_size_of_fish_g, salmon_df_joined$est_fish_lost_fhi_size)
#abline(0,1, col="red", add=TRUE)

# make this salmon joined data from 2018 onwards

salmon_df_joined_2018 <-salmon_df_joined %>% filter(year > 2017)

# provide some info for write up
print(paste0("number of data rows ", min(salmon_df_joined_2018$year), "-", max(salmon_df_joined_2018$year)))
print(nrow(salmon_df_joined_2018)) # subset for 2003 onwards removing anomilies
number_of_sites_1 <- (unique(salmon_df_joined_2018$site_number))
print(paste0("number of unique sites ", min(salmon_df_joined_2018$year), "-", max(salmon_df_joined_2018$year)))
print(length(number_of_sites_1))  # subset for year above 2018 and morts less than 100%


rm(salmon_df, salmon_df_1, salmon_df_2, salmon_df_3, salmon_check, check_number, 
   cols_to_drop, StockingFallowMorts, StockingFallowMorts_clean, inactive_sites,
   mismatch_flags, stock_cycle_check, licence_conflicts, end_year_is_different,
   end_month_is_different, end_date_earlier_than_start_date, duplicate_licence)

print(summary(as.factor(salmon_df_joined_2018$site_number)))

