library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(DT)
library(openxlsx)

#library(shinydashboard)

# ---- BACKGROUND DATA LOAD ----
DATA_PATH <- Sys.getenv("PVM_APP_DATA", unset = file.path("data", "data_for_app_development.csv"))
raw_data_bg <- reactiveVal(NULL)

# ---- HELPER FUNCTION: DETECT STREAKS ----
detect_streaks <- function(x, min_len = 2) {
  r <- rle(x)
  rep(r$lengths >= min_len & r$values, r$lengths)
}
# ---- HELPER FUNCTION: DETECT CAUSE STREAKS ----
detect_cause_streaks <- function(x, min_len = 2) {
  r <- rle(x)
  rep(r$lengths >= min_len & r$values, r$lengths)
}
# ----- HELPER FUNCTION CAUSE-LEVEL STREAK DECTOR ----
#04/02/2026#detect_streaks_cause <- function(x, min_len = 2) {
#  r <- rle(x)
#  rep(r$lengths >= min_len & r$values, r$lengths)
#}
# ----- HELPER FUNCTION CAUSE-LEVEL STREAK DETECTION - GAP AWARE----
detect_streaks_cause <- function(flag, month_n, min_len = 2) {
  
  n <- length(flag)
  out <- rep(FALSE, n)
  
  run_len <- 0
  
  for (i in seq_len(n)) {
    
    if (flag[i]) {
      
      # First TRUE
      if (i == 1) {
        run_len <- 1
        
      } else {
        
        # Check if previous month is consecutive
        if (month_n[i] - month_n[i - 1] == 1) {
          run_len <- run_len + 1
        } else {
          run_len <- 1
        }
      }
      
    } else {
      run_len <- 0
    }
    
    # Mark valid streaks
    if (run_len >= min_len) {
      
      # Mark last min_len positions TRUE
      idx <- (i - min_len + 1):i
      out[idx] <- TRUE
    }
  }
  
  out
}

# ---- UI ----
ui <- fluidPage(
  titlePanel("Persistent Elevated Mortality (PEM) Decision Support Tool (Beta Version)"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "mortality_thresh",
        "Elevated Monthly Mortality Threshold (%)",
        min = 0,
        max = 20,
        value = 5.5,
        step = 0.25
      ),
      
      sliderInput(
        "streak_len",
        "Minimum Consecutive Months (Streak Length)",
        min = 2,
        max = 6,
        value = 2,
        step = 1
      ),
      
      sliderInput(
        "min_pem_cycles",
        "Minimum Cycles for REM/PEM",
        min = 1,
        max = 6,
        value = 2,
        step = 1
      ),
      
      tags$hr(),
      
      downloadButton(
        "export_regulator",
        "Download Report"
      )
    ),
    
    mainPanel(
      
      fluidRow(
        #column(4, wellPanel(h4("Sites Flagged as Recurrent Elevated Mortality (REM)"),
        #                    textOutput("n_REM_sites"))),
        column(4, wellPanel(h4("Total Sites"),
                            textOutput("n_total_sites"))),
        column(4, wellPanel(h4("REM Rate (%)"),
                            textOutput("rem_rate"))), 
        column(4, wellPanel(h4("PEM Rate (%)"),
                            textOutput("pem_rate"))), 
      ),
      fluidRow(
        column(3, uiOutput("rem_kpi")),
        column(3, uiOutput("pem_kpi"))
      ),
      
      
      tabsetPanel(
        tabPanel("REM Summary Table", tableOutput("summary_table")),
        tabPanel("REM Mortality Plot", plotOutput("mortality_plot", height = "500px")),
        #tabPanel("PEM Causes", plotOutput("pem_plot", height = "600px")),
        #tabPanel("PEM Summary Table", tableOutput("pem_summary_table")), 
        
        #tabPanel("PEM by Operator", tableOutput("pem_operator_table")),
        #tabPanel("PEM by Region", tableOutput("pem_region_table")),
        
        tabPanel("PEM Site Summary of Multifactorial Causes",
                 
                 h4("Persistent Causes (Filtered by Cycles)"),
                 
                 plotOutput("pem_plot", height = "500px"),
                 
                 tags$hr(),
                 
                 h4("Cause Summary"),
                 tableOutput("pem_causes_table")
        ),
        
        tabPanel("PEM Summary by Cause",
                 
                 h4("PEM Cause Classification"),
                 tableOutput("pem_summary_table"),
                 plotOutput("pem_summary_plot", height = "350px")
        ),
        
        tabPanel("PEM Summary by Operator",
                 
                 tableOutput("pem_operator_table"),
                 plotOutput("pem_operator_plot", height = "400px")
        ),
        
        tabPanel("PEM Summary by Region",
                 
                 tableOutput("pem_region_table"),
                 plotOutput("pem_region_plot", height = "400px")
        ),
        
        
        tabPanel("PEM Summary by Year",
                 
                 h4("PEM Cases per Year"),
                 
                 tableOutput("pem_year_table"),
                 
                 tags$hr(),
                 
                 plotOutput("pem_year_plot", height = "400px")
        ),
        
        tabPanel("PEM Table", DT::DTOutput("pem_debug_table"))#,
        #  tabPanel("PEM Debug", tableOutput("debug_pem"))
        
        
        
        
      )
      
    )
  )
  
)

# ---- SERVER ----
server <- function(input, output, session) {
  
  # Load data once
  #observe({
  #  df <- read.csv(DATA_PATH, stringsAsFactors = FALSE)
  
  #  df <- df %>%
  #    mutate(
  #      date = as.Date(date, format = "%d/%m/%Y"),
  #      mortality_rate_final = as.numeric(mortality_rate_final)
  #    )
  
  #  raw_data_bg(df)
  #})
  
  observeEvent(TRUE, {
    df <- read.csv(DATA_PATH, stringsAsFactors = FALSE)
    
    df <- df %>%
      mutate(
        date = as.Date(date, format = "%d/%m/%Y"),
        mortality_rate_final = as.numeric(mortality_rate_final)
      )
    
    raw_data_bg(df)
  }, once = TRUE)
  
  
  # ---- CORE REM CLASSIFICATION ----
  classified_data <- reactive({
    req(raw_data_bg())
    
    data <- raw_data_bg()
    
    # Step 1: Flag high events
    data <- data %>%
      mutate(high_event = mortality_rate_final >= input$mortality_thresh) %>% 
      arrange(site_number, date) %>%
      group_by(site_number) %>%
      mutate(month_id = row_number()) %>%
      ungroup()
    
    # Step 2: Within-cycle streaks (user-defined length)
    data <- data %>%
      arrange(site_number, stock_cycle, stock_cycle_month_n) %>%
      group_by(site_number, stock_cycle) %>%
      mutate(within_cycle_streak = detect_streaks(high_event, input$streak_len)) %>%
      ungroup()
    
    # Step 3: Cycle-level summary
    cycle_summary <- data %>%
      group_by(site_number, stock_cycle) %>%
      summarise(has_streak = any(within_cycle_streak, na.rm = TRUE),
                .groups = "drop")
    
    # Step 4: Consecutive cycles with streaks (fixed at â‰¥2 for now)
    cycle_summary <- cycle_summary %>%
      arrange(site_number, stock_cycle) %>%
      group_by(site_number) %>%
      mutate(
        cycle_num = as.numeric(factor(stock_cycle, levels = sort(unique(stock_cycle)))),
        
        streak_group = cumsum(!(has_streak & lag(has_streak, default = FALSE) &
                                  (cycle_num - lag(cycle_num, default = cycle_num[1])) == 1)),
        
        streak_len = ave(has_streak, streak_group,
                         FUN = function(x) if (all(!x)) 0 else sum(x)),
        
        consec_cycle_flag = has_streak & streak_len >= input$min_pem_cycles #2
      ) %>%
      ungroup()
    
    
      # Step 5: Join back
    data <- data %>%
      left_join(
        cycle_summary %>%
          select(site_number, stock_cycle, consec_cycle_flag),
        by = c("site_number", "stock_cycle")
      )
    
    data
  })
  
  # ---- SITE-LEVEL SUMMARY ----
  summary_table_reactive <- reactive({
    classified_data() %>%
      
      # Collapse to site level FIRST
      group_by(site_number) %>%
      summarise(
        has_REM = any(consec_cycle_flag, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      
      # Then assign category
      mutate(
        category = if_else(
          has_REM,
          "b. sites with REM cycles",
          "a. sites with no REM cycles"
        )
      )
  })
  
  REM_sites <- reactive({
    summary_table_reactive() %>%
      filter(category == "b. sites with REM cycles") %>%
      pull(site_number) #%>%
    # unique()
  })
  
  
  # ---- REM KPI OUTPUTS ----
  
  output$n_REM_sites <- renderText({
    length(REM_sites())
  })
  
  #  output$n_total_sites <- renderText({
  #    length(unique(raw_data_bg()$site_number))
  #  })
  output$n_total_sites <- renderText({
    req(raw_data_bg())
    dplyr::n_distinct(raw_data_bg()$site_number)
  })
  
  output$rem_rate <- renderText({
    total <- length(unique(raw_data_bg()$site_number))
    rem   <- length(REM_sites())
    paste0(round(100 * rem / total, 1), "%")
  })
  
  
  
  # ---- SUMMARY TABLE ----
  
  output$summary_table <- renderTable({
    summary_table_reactive() %>%
      count(category, name = "num_sites")
  })
  
  # ---- PLOT ----
  
  output$mortality_plot <- renderPlot({
    req(classified_data())
    
    plot_data <- classified_data() %>%
      filter(site_number %in% REM_sites())
    
    ggplot(plot_data, aes(x = date, y = mortality_rate_final,
                          color = factor(stock_cycle))) +
      geom_point() +
      geom_hline(yintercept = input$mortality_thresh,
                 linetype = "dashed", color = "red") +
      facet_wrap(~ site_number, scales = "free_x") +
      labs(
        title = "Sites Flagged with Recurring Elevated Mortality",
        x = "Date",
        y = "Monthly Mortality (%)",
        color = "Stock Cycle"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # ---- HELPER: CLEAN & NORMALISE REASONS ----
  clean_reason <- function(x) {
    x %>%
      stringr::str_split(";") %>%
      purrr::map(~ stringr::str_trim(.x)) %>%
      purrr::map(~ tolower(.x)) %>%
      purrr::map(~ .x[!is.na(.x) & .x != ""]) %>%
      purrr::map(~ sort(unique(.x))) %>%
      purrr::map_chr(~ paste(.x, collapse = "; "))
  }
  
  # ---- HIGH-LEVEL CATEGORY MAP ----
  reason_category_map <- list(
    "environmental" = "environmental",
    "infectious diseases" = "infectious diseases",
    "handling and treatment" = "handling and treatment",
    "developmental" = "developmental",
    "gill health related" = "gill health related",
    "other" = "other"
  )
  
  
  
  
  # ---- PEM data ----
  pem_data <- reactive({
    req(classified_data(), REM_sites())
    
    classified_data() %>%
      filter(
        site_number %in% REM_sites(),
        high_event == TRUE,
        #within_cycle_streak == TRUE,
        consec_cycle_flag == TRUE| 
          lag(consec_cycle_flag, default = FALSE)
      ) %>%
      
      # --- Combine & clean reasons (exactly same logic as fianl methods run) ---
      mutate(
        reason_combined = paste(top_cause_1, top_cause_2, top_cause_3, sep = "; "),
        reason_combined = gsub("NA|^; |; NA", "", reason_combined),
        reason_combined = tolower(trimws(reason_combined)),
        reason_combined_clean = clean_reason(reason_combined)
      ) %>%
      
      # --- Unnest causes + distribute mortality evenly ---
      mutate(
        reason_split = stringr::str_split(reason_combined_clean, ";\\s*"),
        n_causes     = purrr::map_int(reason_split, length)
      ) %>%
      tidyr::unnest(reason_split) %>%
      
      mutate(
        reason_split = tolower(stringr::str_trim(reason_split)),
        high_level_reason = purrr::map_chr(
          reason_split,
          ~ reason_category_map[[.x]] %||% "other"
        ),
        shared_mortality_rate = mortality_rate_final / n_causes
      ) %>%
      
      arrange(site_number, stock_cycle, date) %>%
      group_by(site_number, stock_cycle, high_level_reason) %>%
      mutate(
        #cause_high_event = shared_mortality_rate > 0,
        cause_high_event = mortality_rate_final >= input$mortality_thresh,
        cause_streak = detect_cause_streaks(cause_high_event, input$streak_len)
      ) %>%
      ungroup()
  })
  # Identify cycles where each cause truely qualifies
  pem_cause_cycles <- reactive({
    req(pem_data())
    
    pem_data() %>%
      group_by(site_number, stock_cycle, high_level_reason) %>%
      summarise(
        has_cause_streak = any(cause_streak, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(has_cause_streak)
  })
  
  # create threshold PEM base (mortality level + number of months)
  #04/02/2026###
  pem_data_thresh <- reactive({
    req(pem_data(), input$mortality_thresh, input$streak_len)
    
    pem_data() %>%
      mutate(
        above_thresh = mortality_rate_final >= input$mortality_thresh
      ) %>%
      
      arrange(site_number, stock_cycle, high_level_reason, stock_cycle_month_n) %>%
      
      group_by(site_number, stock_cycle, high_level_reason) %>%
      
      mutate(
        cause_streak = detect_streaks_cause(
          above_thresh,
          stock_cycle_month_n,
          input$streak_len
        )
      ) %>%
      
      ungroup() %>%
      
      filter(cause_streak == TRUE)
  })
  
  #pem_data_thresh <- reactive({
  # req(pem_data(), input$mortality_thresh, input$streak_len)
  
  #  pem_data() %>%
  #   mutate(
  #    above_thresh = shared_mortality_rate >= input$mortality_thresh
  #  ) %>%
  #   above_thresh = mortality_rate_final >= input$mortality_thresh
  #) %>%
  #    arrange(site_number, stock_cycle, high_level_reason, date) %>%
  
  #    group_by(site_number, stock_cycle, high_level_reason) %>%
  
  #   mutate(
  #    cause_streak = detect_streaks_cause(above_thresh, input$streak_len)
  # ) %>%
  
  #ungroup() %>%
  
  # Keep only months that are in valid streaks
  #filter(cause_streak == TRUE)
  #})
  
  
  #04/02/2026#pem_data_thresh <- reactive({
  # req(pem_data(), input$mortality_thresh)
  
  #pem_data() %>%
  # filter(shared_mortality_rate >= input$mortality_thresh)
  #})
  
  
  # recompute persistent cause on threshold data 
  pem_persistent_causes <- reactive({
    req(pem_data_thresh())
    
    pem_data_thresh() %>%
      group_by(site_number, high_level_reason) %>%
      summarise(
        num_cycles = n_distinct(stock_cycle),
        .groups = "drop"
      ) %>%
      filter(num_cycles >= 2)
  })
  
  # make sure consecutive cycles by cause
  #04/02/2026# pem_persistent_causes <- reactive({
  #  req(pem_cause_cycles())
  
  #  pem_cause_cycles() %>%
  #    arrange(site_number, high_level_reason, stock_cycle) %>%
  #    group_by(site_number, high_level_reason) %>%
  #    mutate(
  #      cycle_num = as.numeric(factor(stock_cycle, levels = sort(unique(stock_cycle)))),
  #      cycle_gap = cycle_num - lag(cycle_num),
  #      consec_cycle = if_else(!is.na(cycle_gap) & cycle_gap == 1, TRUE, FALSE)
  #    ) %>%
  #    summarise(
  #      num_consec_pairs = sum(consec_cycle, na.rm = TRUE),
  #      .groups = "drop"
  #    ) %>%
  #    filter(num_consec_pairs >= 1)
  # })
  # final pen site flags
  pem_site_flags <- reactive({
    req(pem_persistent_causes())
    
    pem_persistent_causes() %>%
      group_by(site_number) %>%
      summarise(
        pem_flag = TRUE,
        dominant_persistent_cause = paste(unique(high_level_reason), collapse = "; "),
        .groups = "drop"
      )
  })
  
  
  # ---- PEM summary ----
  # pem_summary <- reactive({
  #    pem_data() %>%
  #      group_by(
  #        site_number,
  #        stock_cycle,
  #        stock_cycle_month_n,
  #        date,
  #        high_level_reason
  #      ) %>%
  #      summarise(
  #        total_mortality_rate = sum(mortality_rate_final, na.rm = TRUE),
  #        cause_mortality_rate   = sum(shared_mortality_rate, na.rm = TRUE),
  #        .groups = "drop"
  #      )
  # })
  
  # pem_summary <- reactive({
  #    req(pem_summary())
  
  #    recurring_causes <- pem_summary() %>%
  #      group_by(site_number, high_level_reason) %>%
  #      summarise(
  #        num_cycles = n_distinct(stock_cycle),
  #        .groups = "drop"
  #      ) %>%
  #      filter(num_cycles >= 2)
  #    
  
  #  pem_summary() %>%
  #      inner_join(
  #        recurring_causes %>%
  #          select(site_number, high_level_reason),
  #        by = c("site_number", "high_level_reason")
  #      )
  #  })
  
  
  pem_display_data <- reactive({
    req(pem_data(), pem_persistent_causes(), input$mortality_thresh)
    
    pem_data() %>%
      inner_join(
        pem_persistent_causes(),
        by = c("site_number", "high_level_reason")
      ) %>%
      filter(mortality_rate_final >= input$mortality_thresh)
  })
  
  # build pem summary for threshold data 
  pem_persistent_causes <- reactive({
    req(pem_data_thresh())
    
    pem_data_thresh() %>%
      group_by(site_number, high_level_reason) %>%
      summarise(
        num_cycles = n_distinct(stock_cycle),
        .groups = "drop"
      ) %>%
      filter(num_cycles >= 2)
  })
  
  ## 04/02/2026 pem_summary <- reactive({
  #req(pem_display_data())
  
  #  pem_display_data() %>%
  #    group_by(
  #      site_number,
  #      stock_cycle,
  #      stock_cycle_month_n,
  #      high_level_reason
  #    ) %>%
  #    summarise(
  #      avg_mortality_rate = mean(shared_mortality_rate, na.rm = TRUE),
  #      total_mortality_rate = sum(shared_mortality_rate, na.rm = TRUE),
  #      n_months             = dplyr::n(),
  #      .groups = "drop"
  #    )
  #})
  
  
  #---- PEM plot ---- not using this version as it has all causes not just PEM related ones
  # output$pem_plot <- renderPlot({
  #    req(pem_summary())
  
  #    ggplot(pem_summary(), aes(
  #      x    = factor(stock_cycle),
  #      y    = avg_mortality_rate,
  #      fill = high_level_reason
  #    )) +
  #      geom_col() +
  #      facet_wrap(~ site_number, scales = "free_x") +
  #     scale_fill_brewer(palette = "Set2", name = "Mortality Cause (High Level)") +
  #      labs(
  #        title = "High-Level Causes per Stock Cycle (PEM Layer)",
  #        x     = "Stock Cycle",
  #        y     = "Avg Mortality Rate (%)"
  #      ) +
  #      theme_minimal(base_size = 12) +
  #      theme(
  #        axis.text.x   = element_text(angle = 45, hjust = 1),
  #        legend.position = "bottom"
  #      )
  #  })
  
  #---- PEM TABLE ----
  ###  output$pem_table <- renderTable({
  ###    pem_summary() %>%
  ###      arrange(site_number, desc(cause_mortality_rate))
  ###  })
  #--- Dominant case in PEM Cycles -----
  #  pem_dominant_cause <- reactive({
  #   req(pem_summary())
  
  #    pem_summary() %>%
  #     group_by(site_number, stock_cycle, high_level_reason) %>%
  #     summarise(
  #      cycle_mortality = sum(cause_mortality_rate , na.rm = TRUE),
  #      .groups = "drop"
  #    ) %>%
  #    group_by(site_number, stock_cycle) %>%
  #    slice_max(order_by = cycle_mortality, n = 1, with_ties = TRUE) %>%
  #    ungroup()
  # })
  
  # update PEM summary 
  pem_summary <- reactive({
    req(pem_data_thresh(), pem_persistent_causes())
    
    pem_data_thresh() %>%
      inner_join(
        pem_persistent_causes(),
        by = c("site_number", "high_level_reason")
      ) %>%
      
      group_by(
        site_number,
        year,
        stock_cycle,
        high_level_reason
      ) %>%
      
      summarise(
        avg_mortality_rate   = mean(mortality_rate_final), # check this works 04/02/2026
        total_mortality_rate = sum(shared_mortality_rate), ## check this works 04/02/2026
        n_months             = n(),
        .groups = "drop"
      )
  })
  
  
  
  # rebuild dominant cause from summary data
  pem_dominant_cause <- reactive({
    req(pem_summary())
    
    pem_summary() %>%
      group_by(site_number, stock_cycle, high_level_reason) %>%
      summarise(
        cycle_mortality = sum(total_mortality_rate),
        .groups = "drop"
      ) %>%
      group_by(site_number, stock_cycle) %>%
      slice_max(cycle_mortality, n = 1, with_ties = FALSE) %>%
      ungroup()
  })
  
  
  # ---- PEM Logic ------ "A cause is persistent if it appears with meaningful mortality in â‰¥ 2 REM cycles"
  
  pem_site_flags <- reactive({
    req(pem_dominant_cause())
    
    pem_dominant_cause() %>%
      group_by(site_number, high_level_reason) %>%
      summarise(
        num_cycles = n_distinct(stock_cycle),
        .groups = "drop"
      ) %>%
      group_by(site_number) %>%
      summarise(
        pem_flag = any(num_cycles >= 2),
        dominant_persistent_cause = paste(
          high_level_reason[num_cycles >= 2],
          collapse = "; "
        ),
        .groups = "drop"
      )
  })
  
  
  # o4/02/2024 pem_site_flags <- reactive({
  #    req(pem_persistent_causes())
  
  #   pem_persistent_causes() %>%
  #    group_by(site_number) %>%
  #   summarise(
  #    pem_flag = TRUE,
  #   dominant_persistent_cause = paste(unique(high_level_reason), collapse = "; "),
  #  .groups = "drop"
  #  )
  #  })
  
  # pem_site_flags <- reactive({
  #   req(pem_summary())
  
  #   pem_summary() %>%
  #     group_by(site_number, stock_cycle, high_level_reason) %>%
  #    summarise(
  #      cycle_mortality = sum(cause_mortality_rate, na.rm = TRUE),
  #      .groups = "drop"
  #    ) %>%
  #    filter(cycle_mortality > 0) %>%
  #    group_by(site_number, high_level_reason) %>%
  #    summarise(
  #     num_cycles = n_distinct(stock_cycle),
  #      .groups = "drop"
  #    ) %>%
  #    group_by(site_number) %>%
  #    summarise(
  #      pem_flag = any(num_cycles >= 2),
  #      dominant_persistent_cause = paste(
  #        high_level_reason[num_cycles >= 2],
  #       collapse = "; "
  #      ),
  #     .groups = "drop"
  #    )
  # })
  
  
  
  
  # ---- PEM Join to main df -----
  #classified_with_pem <- reactive({
  #  req(classified_data(), pem_site_flags())
  
  #   classified_data() %>%
  #     left_join(pem_site_flags(), by = "site_number") %>%
  #     mutate(
  #       pem_flag = ifelse(is.na(pem_flag), FALSE, pem_flag),
  #       pem_category = case_when(
  #          pem_flag ~ "c. sites with PEM (persistent cause)",
  #         consec_cycle_flag ~ "b. sites with REM cycles",
  #         TRUE ~ "a. sites with no REM cycles"
  #       )
  #     )
  #  })
  classified_with_pem <- reactive({
    req(classified_data(), pem_site_flags())
    
    classified_data() %>%
      left_join(pem_site_flags(), by = "site_number") %>%
      mutate(
        pem_flag = if_else(is.na(pem_flag), FALSE, pem_flag),
        pem_category = case_when(
          pem_flag ~ "c. sites with PEM (persistent cause)",
          consec_cycle_flag ~ "b. sites with REM cycles",
          TRUE ~ "a. sites with no REM cycles"
        )
      )
  })
  
  # ---- PEM Summary Table (No Double Counting) -----
  #  pem_summary_table <- reactive({
  #    classified_with_pem() %>%
  #      distinct(site_number, pem_category, pem_flag) %>%
  #      count(pem_category, name = "num_sites") %>%
  #      arrange(pem_category)
  #  })
  
  #output$pem_summary_table <- renderTable({
  #req(pem_site_flags())
  
  #  pem_site_flags() %>%
  #    count(pem_flag, name = "num_sites") %>%
  #    mutate(
  #      pem_category = if_else(pem_flag, "c. sites with PEM", "other")
  #    )
  #})
  pem_summary_table <- reactive({
    req(pem_summary())
    
    pem_summary() %>%
      distinct(site_number, high_level_reason) %>%
      count(high_level_reason, name = "num_site_cause_pairs") %>%
      arrange(desc(num_site_cause_pairs))
  })
  
  output$pem_summary_table <- renderTable({
    pem_summary_table()
  })
  
  
  #----   PEM Sites Table (For Expert Review) ----
  # output$pem_sites_table <- DT::renderDT({
  #    req(pem_site_flags())
  
  #    pem_site_flags() %>%
  #      filter(pem_flag == TRUE) %>%
  #     arrange(site_number) %>%
  #    datatable(
  #     options = list(pageLength = 10),
  #    rownames = FALSE
  #  )
  #})
  
  # ---- PEM Plot (Only Persistent-Cause Cycles) ----
  #output$pem_plot <- renderPlot({
  #  req(pem_summary(), pem_site_flags())
  
  #  pem_sites <- pem_site_flags() %>%
  #    filter(pem_flag) %>%
  #    pull(site_number)
  
  #  persistent_causes <- pem_site_flags() %>%
  #   filter(pem_flag) %>%
  #  separate_rows(dominant_persistent_cause, sep = ";\\s*") %>%
  # rename(high_level_reason = dominant_persistent_cause)
  
  #plot_data <- pem_summary() %>%
  #  filter(site_number %in% pem_sites) %>%
  #  inner_join(
  #    persistent_causes,
  #    by = c("site_number", "high_level_reason")
  #  )
  
  #  ggplot(plot_data, aes(
  #   x = factor(stock_cycle),
  #  y = cause_mortality_rate,
  #  fill = high_level_reason
  #)) +
  #  geom_col() +
  #  facet_wrap(~ site_number, scales = "free_x") +
  #  scale_fill_brewer(palette = "Set2", name = "Persistent Cause") +
  #  labs(
  #    title = "Persistent Elevated Mortality (PEM) â€” Dominant Causes",
  #    x = "Stock Cycle",
  #    y = "Avg Mortality Rate (%)"
  #  ) +
  #  theme_minimal(base_size = 12) +
  #  theme(
  #    axis.text.x = element_text(angle = 45, hjust = 1),
  #    legend.position = "bottom"
  #  )
  #  })
  
  output$pem_plot <- renderPlot({
    req(pem_summary())
    
    ggplot(pem_summary(), aes(
      x = factor(stock_cycle),
      y = avg_mortality_rate,
      fill = high_level_reason
    )) +
      geom_col() +
      facet_wrap(~ site_number, scales = "free_x") +
      scale_fill_brewer(palette = "Set2", name = "Persistent Cause") +
      labs(
        title = "Persistent Elevated Mortality (PEM) â€” Causes",
        x = "Stock Cycle",
        y = "Avg Mortality Rate (%)"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom"
      )
  })
  # --- add summary table --- 05/02/2026
  pem_causes_summary <- reactive({
    req(pem_summary())
    
    pem_summary() %>%
      group_by(high_level_reason) %>%
      summarise(
        num_cases = n(),
        num_sites = n_distinct(site_number),
        .groups = "drop"
      ) %>%
      arrange(desc(num_cases))
  })
  
  output$pem_causes_table <- renderTable({
    pem_causes_summary()
  })
  # --- create yearly summary table ----
  pem_by_year <- reactive({
    req(pem_summary())
    
    pem_summary() %>%
      group_by(year) %>%
      summarise(
        num_cases = n(),
        num_sites = n_distinct(site_number),
        .groups = "drop"
      ) %>%
      arrange(year)
  })
  
  output$pem_year_table <- renderTable({
    pem_by_year()
  })
  
  pem_by_year_cause <- reactive({
    req(pem_summary())
    
    pem_summary() %>%
      group_by(year, high_level_reason) %>%
      summarise(
        num_cases = n(),
        num_sites = n_distinct(site_number),
        .groups = "drop"
      ) %>%
      arrange(year)
  })
  
  
  output$pem_year_plot <- renderPlot({
    req(pem_by_year_cause())
    
    ggplot(pem_by_year_cause(),
           aes(x = year, y = num_cases, fill = high_level_reason)) +
      geom_col() +
      labs(
        title = "PEM Cases per Year",
        x = "Year",
        y = "Number of PEM Cases"
      ) +
      theme_minimal(base_size = 20)
  })
  
  
  #--- temp debug PEM ---
  
  pem_debug_cycles <- reactive({
    req(pem_summary())
    
    pem_summary() %>%
      group_by(site_number, stock_cycle, high_level_reason) %>%
      summarise(
        total_cycle_mortality = sum(cause_mortality_rate, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(site_number, stock_cycle, desc(total_cycle_mortality))
  })
  
  # ---- KPI Value Boxes (REM + PEM)
  output$rem_kpi <- renderUI({
    div(
      style = "
      background-color: #f0ad4e;
      color: white;
      padding: 15px;
      border-radius: 8px;
      text-align: center;
      font-size: 18px;",
      h3(length(REM_sites())),
      p("Sites with model indicating REM")
    )
  })
  
  output$pem_kpi <- renderUI({
    req(pem_site_flags())
    
    div(
      style = "
      background-color: #d9534f;
      color: white;
      padding: 15px;
      border-radius: 8px;
      text-align: center;
      font-size: 18px;",
      h3(sum(pem_site_flags()$pem_flag, na.rm = TRUE)),
      p("Sites with model indicating PEM")
    )
  })
  # debug render PEM
  # output$pem_debug_table <- DT::renderDT({
  #   req(pem_debug_cycles())
  
  #   pem_debug_cycles() %>%
  #     datatable(
  #       options = list(pageLength = 25),
  #       rownames = FALSE
  #    )
  # })
  output$pem_debug_table <- DT::renderDT({
    req(pem_data())
    
    pem_data() %>%
      filter(cause_streak) %>%
      select(
        site_number,
        stock_cycle,
        stock_cycle_month_n,
        date,
        high_level_reason,
        shared_mortality_rate,
        mortality_rate_final#,
        # cause_streak
      ) %>%
      arrange(site_number, stock_cycle,stock_cycle_month_n, high_level_reason, date) %>%
      datatable(options = list(pageLength = 25), rownames = FALSE)
  })
  
  output$debug_pem <- renderTable({
    
    pem_data() %>%
      mutate(
        above = shared_mortality_rate >= input$mortality_thresh
      ) %>%
      
      arrange(site_number, stock_cycle, high_level_reason, stock_cycle_month_n) %>%
      
      group_by(site_number, stock_cycle, high_level_reason) %>%
      
      mutate(
        cause_streak = detect_streaks_cause(
          above,
          stock_cycle_month_n,
          input$streak_len
        )
      ) %>%
      
      ungroup() %>%
      
      select(
        site_number,
        stock_cycle,
        stock_cycle_month_n,
        date,
        high_level_reason,
        shared_mortality_rate,
        above,
        cause_streak
      )
  })
  
  # observe({
  #    cat("PEM sites (truth):", nrow(pem_site_flags()), "\n")
  #   cat("PEM sites (display):",
  #        dplyr::n_distinct(pem_summary()$site_number), "\n")
  #  })
  observe({
    cat("PEM sites (dynamic):",
        sum(pem_site_flags()$pem_flag, na.rm = TRUE),
        "\n")
  })
  # temp debug table 2 
  #02/04/2026
  #output$debug_pem <- renderTable({
  #   pem_data_thresh() %>%
  #     count(site_number, stock_cycle, high_level_reason)
  # })
  # ----additional PEM inference ----
  #  classified_with_pem() %>%
  #   filter(pem_flag == TRUE) %>%
  #  distinct(site_number, operator, region_grouped)
  
  pem_by_operator <- reactive({
    req(classified_with_pem())
    
    classified_with_pem() %>%
      filter(pem_flag == TRUE) %>%
      distinct(site_number, operator) %>%
      count(operator, name = "num_sites") %>%
      arrange(desc(num_sites))
  })
  
  pem_by_region <- reactive({
    req(classified_with_pem())
    
    classified_with_pem() %>%
      filter(pem_flag == TRUE) %>%
      distinct(site_number, region_grouped) %>%
      count(region_grouped, name = "num_sites") %>%
      arrange(desc(num_sites))
  })
  
  output$pem_operator_table <- renderTable({
    pem_by_operator()
  })
  
  output$pem_region_table <- renderTable({
    pem_by_region()
  })
  # ---- visualise PEM tables ----
  output$pem_summary_plot <- renderPlot({
    req(pem_summary_table())
    
    ggplot(pem_summary_table(),
           # aes(x = pem_category, y = num_sites)) +
           aes(x=high_level_reason, y=num_site_cause_pairs))+
      geom_col() +
      labs(
        title = "Sites by PEM Classification",
        x = "Classification",
        y = "Number of Sites"
      ) +
      theme_minimal(base_size = 20) +
      theme(
        axis.text.x = element_text(angle = 30, hjust = 1)
      )
  })
  
  output$pem_operator_plot <- renderPlot({
    req(pem_by_operator())
    
    ggplot(pem_by_operator(),
           aes(x = reorder(operator, num_sites),
               y = num_sites)) +
      geom_col() +
      coord_flip() +
      labs(
        title = "PEM Sites by Operator",
        x = "Operator",
        y = "Number of Sites"
      ) +
      theme_minimal(base_size = 20)
  })
  output$pem_region_plot <- renderPlot({
    req(pem_by_region())
    
    ggplot(pem_by_region(),
           aes(x = reorder(region_grouped, num_sites),
               y = num_sites)) +
      geom_col() +
      coord_flip() +
      labs(
        title = "PEM Sites by Region",
        x = "Region",
        y = "Number of Sites"
      ) +
      theme_minimal(base_size = 20)
  })
  
  
  # ----PEM KPI OUTPUTS ----
  PEM_sites <- reactive({
    classified_with_pem() %>%
      filter(pem_category == "c. sites with PEM (persistent cause)") %>%
      pull(site_number) %>%
      unique()
  })
  output$n_PEM_sites <- renderText({
    length(PEM_sites())
  })
  output$pem_rate <- renderText({
    total <- length(unique(raw_data_bg()$site_number))
    pem   <- length(PEM_sites())
    paste0(round(100 * pem / total, 1), "%")
  })
  
  output$export_regulator <- downloadHandler(
    
    filename = function() {
      paste0(
        "PEM_Report_",
        Sys.Date(),
        ".xlsx"
      )
    },
    
    content = function(file) {
      
      wb <- openxlsx::createWorkbook()
      
      
      # ---- Sheet 1: Site Classifications ----
      openxlsx::addWorksheet(wb, "Site_Classification")
      
      openxlsx::writeData(
        wb,
        "Site_Classification",
        classified_with_pem() %>%
          distinct(
            site_number,
            operator,
            region_grouped,
            pem_category,
            pem_flag
          )
      )
      
      
      # ---- Sheet 2: PEM Summary ----
      openxlsx::addWorksheet(wb, "PEM_Summary")
      
      openxlsx::writeData(
        wb,
        "PEM_Summary",
        pem_summary_table()
      )
      
      
      # ---- Sheet 3: Operator Summary ----
      openxlsx::addWorksheet(wb, "By_Operator")
      
      openxlsx::writeData(
        wb,
        "By_Operator",
        pem_by_operator()
      )
      
      
      # ---- Sheet 4: Region Summary ----
      openxlsx::addWorksheet(wb, "By_Region")
      
      openxlsx::writeData(
        wb,
        "By_Region",
        pem_by_region()
      )
      
      
      # ---- Sheet 5: Persistent Causes ----
      openxlsx::addWorksheet(wb, "Persistent_Causes")
      
      openxlsx::writeData(
        wb,
        "Persistent_Causes",
        pem_site_flags() %>%
          filter(pem_flag) %>%
          arrange(site_number)
      )
      
      
      # Save
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
}

shinyApp(ui, server)

