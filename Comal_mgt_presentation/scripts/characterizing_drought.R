# ============================================================
# COMAL / SAN MARCOS SPRINGFLOW HISTORICAL CONTEXT
#
# Daily springflow -> monthly means -> rolling monthly means
#
# Rolling windows:
#   3, 6, 9, 12, 18, 24 months
#
# Special data rule:
#   San Marcos 1956 is retained even though it is a partial
#   year because the gage record begins May 26, 1956 during
#   the drought of record.
#
# Outputs:
#   1. annual completeness
#   2. monthly completeness + monthly means
#   3. rolling monthly means
#   4. 2025 drought vs July 2026 context
#   5. driest individual rolling periods
#   6. driest rolling period by year
# ============================================================


library(tidyverse)
library(lubridate)
library(slider)


# ------------------------------------------------------------
# PATHS
# ------------------------------------------------------------

project_root <- paste0(
  "C:/Users/cfurl/OneDrive - Edwards Aquifer Authority/",
  "SC_meetings/sep_2026/fall_meeting/",
  "EAHCP-Fall-2026-Sci-Comm"
)

data_path <- file.path(
  project_root,
  "springflow_protection_presentation",
  "data",
  "HistoricDailyMeanSpringflow.csv"
)

output_dir <- file.path(
  project_root,
  "Comal_mgt_presentation",
  "output",
  "flow_statistics"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


cat("\nInput file:\n")
cat(data_path, "\n")

cat("\nOutput directory:\n")
cat(output_dir, "\n\n")


# ------------------------------------------------------------
# SETTINGS
# ------------------------------------------------------------

analysis_end_date <- as.Date("2026-07-31")


# Historical years with at least this many observations
# are accepted as sufficiently complete.
#
# Example:
# 363-day years remain in the analysis.
min_days_valid_year <- 360


# Monthly means need at least 90% daily coverage to be used
# in rolling-average calculations.
#
# IMPORTANT:
# Partial monthly means are still calculated and written to
# the monthly CSV. This setting only controls whether they
# are used in rolling calculations.
min_month_coverage <- 0.90


rolling_windows <- c(
  3,
  6,
  9,
  12,
  18,
  24
)


# Current working definition of the 2025 drought minimum
# search period.
drought_2025_start <- as.Date("2025-05-01")
drought_2025_end   <- as.Date("2025-09-30")


# ------------------------------------------------------------
# EXPLICIT PARTIAL-YEAR EXCEPTIONS
#
# San Marcos begins May 26, 1956.
#
# We intentionally retain this year because it captures the
# drought-of-record period at the beginning of the gage record.
# ------------------------------------------------------------

forced_year_inclusions <- tibble(
  station = "San Marcos Springs",
  year = 1956L,
  inclusion_reason =
    "Gage record begins 1956-05-26 during drought of record"
)


# ------------------------------------------------------------
# READ DAILY DATA
# ------------------------------------------------------------

daily <- read_csv(
  data_path,
  na = c("", "NA"),
  show_col_types = FALSE
) |>
  transmute(
    date = as.Date(Day),
    
    Comal =
      `Comal Springs (cfs)`,
    
    San_Marcos =
      `San Marcos Springs (cfs)`
  ) |>
  filter(
    date <= analysis_end_date
  ) |>
  pivot_longer(
    cols = c(
      Comal,
      San_Marcos
    ),
    names_to = "station",
    values_to = "flow_cfs"
  ) |>
  mutate(
    station = recode(
      station,
      Comal = "Comal Springs",
      San_Marcos = "San Marcos Springs"
    )
  ) |>
  arrange(
    station,
    date
  )


# ------------------------------------------------------------
# BASIC RECORD CHECK
# ------------------------------------------------------------

cat("\n========================================\n")
cat("DATA DATE RANGE\n")
cat("========================================\n")

print(
  range(daily$date)
)


cat("\n========================================\n")
cat("FIRST AND LAST OBSERVATION BY STATION\n")
cat("========================================\n")

daily |>
  filter(
    !is.na(flow_cfs)
  ) |>
  group_by(
    station
  ) |>
  summarise(
    first_date = min(date),
    last_date = max(date),
    total_observations = n(),
    .groups = "drop"
  ) |>
  print()


# ------------------------------------------------------------
# YEARLY DATA COMPLETENESS
# ------------------------------------------------------------

yearly_completeness <- daily |>
  filter(
    !is.na(flow_cfs)
  ) |>
  mutate(
    year = year(date)
  ) |>
  group_by(
    station,
    year
  ) |>
  summarise(
    
    observed_days =
      n_distinct(date),
    
    first_observation =
      min(date),
    
    last_observation =
      max(date),
    
    .groups = "drop"
  ) |>
  
  # Mark explicit exceptions
  left_join(
    forced_year_inclusions,
    by = c(
      "station",
      "year"
    )
  ) |>
  
  mutate(
    
    forced_include =
      !is.na(inclusion_reason),
    
    expected_days_full_year =
      if_else(
        leap_year(year),
        366L,
        365L
      ),
    
    is_current_year =
      year ==
      year(analysis_end_date),
    
    # Current year is only expected to extend through
    # July 31, 2026.
    expected_days_available =
      if_else(
        is_current_year,
        yday(analysis_end_date),
        expected_days_full_year
      ),
    
    coverage_pct =
      100 *
      observed_days /
      expected_days_available,
    
    literally_complete =
      observed_days ==
      expected_days_available,
    
    
    # ----------------------------------------------
    # YEAR-INCLUSION LOGIC
    # ----------------------------------------------
    
    year_used = case_when(
      
      # Explicit scientific/data-history exception
      forced_include ~
        TRUE,
      
      # Current partial calendar year
      is_current_year ~
        TRUE,
      
      # Normal historical completeness rule
      observed_days >=
        min_days_valid_year ~
        TRUE,
      
      # Otherwise exclude
      TRUE ~
        FALSE
    ),
    
    
    # ----------------------------------------------
    # HUMAN-READABLE STATUS
    # ----------------------------------------------
    
    year_status = case_when(
      
      forced_include ~
        "PARTIAL, RETAINED - RECORD START",
      
      is_current_year &
        observed_days ==
        expected_days_available ~
        "CURRENT YEAR - COMPLETE THROUGH DATA END",
      
      is_current_year ~
        "CURRENT YEAR - PARTIAL, RETAINED",
      
      observed_days ==
        expected_days_full_year ~
        "COMPLETE",
      
      observed_days >=
        min_days_valid_year ~
        "PARTIAL, RETAINED",
      
      TRUE ~
        "PARTIAL, EXCLUDED"
    )
  ) |>
  arrange(
    station,
    year
  )


# ------------------------------------------------------------
# DISPLAY NON-STANDARD YEARS
# ------------------------------------------------------------

cat("\n========================================\n")
cat("PARTIAL / SPECIAL YEARS\n")
cat("========================================\n")

yearly_completeness |>
  filter(
    year_status != "COMPLETE"
  ) |>
  select(
    station,
    year,
    observed_days,
    first_observation,
    last_observation,
    expected_days_full_year,
    coverage_pct,
    forced_include,
    year_used,
    year_status,
    inclusion_reason
  ) |>
  print(
    n = Inf
  )


write_csv(
  yearly_completeness,
  file.path(
    output_dir,
    "01_yearly_data_completeness.csv"
  )
)


# ------------------------------------------------------------
# MONTHLY MEANS + MONTHLY COMPLETENESS
# ------------------------------------------------------------

monthly <- daily |>
  mutate(
    
    year =
      year(date),
    
    month =
      floor_date(
        date,
        "month"
      )
    
  ) |>
  group_by(
    station,
    year,
    month
  ) |>
  summarise(
    
    observed_days =
      sum(
        !is.na(flow_cfs)
      ),
    
    monthly_mean_cfs =
      if_else(
        
        observed_days > 0,
        
        mean(
          flow_cfs,
          na.rm = TRUE
        ),
        
        NA_real_
      ),
    
    .groups = "drop"
  ) |>
  mutate(
    
    expected_days =
      days_in_month(month),
    
    coverage_pct =
      100 *
      observed_days /
      expected_days,
    
    complete_month =
      observed_days ==
      expected_days
    
  ) |>
  left_join(
    
    yearly_completeness |>
      select(
        station,
        year,
        year_status,
        year_used,
        forced_include
      ),
    
    by = c(
      "station",
      "year"
    )
    
  ) |>
  mutate(
    
    month_status = case_when(
      
      observed_days == 0 ~
        "MISSING",
      
      observed_days ==
        expected_days ~
        "COMPLETE",
      
      observed_days /
        expected_days >=
        min_month_coverage ~
        "PARTIAL, USABLE",
      
      TRUE ~
        "PARTIAL, LOW COVERAGE"
    ),
    
    
    # Month must belong to a retained year AND have
    # adequate daily coverage.
    #
    # Thus:
    # San Marcos May 1956 is retained in the monthly
    # output but is NOT used in rolling calculations.
    month_usable_for_rolling =
      coalesce(
        year_used,
        FALSE
      ) &
      coverage_pct >=
      100 *
      min_month_coverage
    
  ) |>
  arrange(
    station,
    month
  )


# ------------------------------------------------------------
# DISPLAY PARTIAL MONTHS
# ------------------------------------------------------------

cat("\n========================================\n")
cat("PARTIAL MONTHS\n")
cat("========================================\n")

monthly |>
  filter(
    observed_days > 0,
    !complete_month
  ) |>
  select(
    station,
    month,
    observed_days,
    expected_days,
    coverage_pct,
    month_status,
    year_status,
    month_usable_for_rolling
  ) |>
  print(
    n = Inf
  )


write_csv(
  monthly,
  file.path(
    output_dir,
    "02_monthly_springflow.csv"
  )
)


# ------------------------------------------------------------
# SPECIFIC CHECK:
# SAN MARCOS 1956
# ------------------------------------------------------------

cat("\n========================================\n")
cat("SAN MARCOS 1956 CHECK\n")
cat("========================================\n")

monthly |>
  filter(
    station == "San Marcos Springs",
    year == 1956
  ) |>
  select(
    station,
    month,
    observed_days,
    expected_days,
    coverage_pct,
    monthly_mean_cfs,
    month_status,
    month_usable_for_rolling
  ) |>
  print(
    n = Inf
  )


# ------------------------------------------------------------
# BUILD CONTINUOUS MONTHLY SERIES
#
# Only retained years enter the initial rolling dataset.
#
# Missing months inside the retained record are then inserted
# explicitly so rolling windows cannot silently skip them.
# ------------------------------------------------------------

monthly_roll_base <- monthly |>
  filter(
    coalesce(
      year_used,
      FALSE
    )
  ) |>
  select(
    station,
    month,
    monthly_mean_cfs,
    observed_days,
    expected_days,
    coverage_pct,
    complete_month,
    month_status,
    month_usable_for_rolling
  ) |>
  group_by(
    station
  ) |>
  complete(
    
    month = seq.Date(
      min(month),
      max(month),
      by = "month"
    )
    
  ) |>
  ungroup() |>
  mutate(
    
    expected_days =
      if_else(
        is.na(expected_days),
        days_in_month(month),
        expected_days
      ),
    
    observed_days =
      replace_na(
        observed_days,
        0L
      ),
    
    coverage_pct =
      100 *
      observed_days /
      expected_days,
    
    complete_month =
      observed_days ==
      expected_days,
    
    month_status = case_when(
      
      observed_days == 0 ~
        "MISSING",
      
      complete_month ~
        "COMPLETE",
      
      coverage_pct >=
        100 *
        min_month_coverage ~
        "PARTIAL, USABLE",
      
      TRUE ~
        "PARTIAL, LOW COVERAGE"
    ),
    
    month_usable_for_rolling =
      coverage_pct >=
      100 *
      min_month_coverage,
    
    monthly_mean_for_rolling =
      if_else(
        month_usable_for_rolling,
        monthly_mean_cfs,
        NA_real_
      )
    
  ) |>
  arrange(
    station,
    month
  )


# ------------------------------------------------------------
# FUNCTION: ROLLING MONTHLY MEANS
# ------------------------------------------------------------

make_rolling <- function(
    dat,
    n_months
) {
  
  dat |>
    group_by(
      station
    ) |>
    arrange(
      month,
      .by_group = TRUE
    ) |>
    mutate(
      
      rolling_mean_cfs =
        slide_dbl(
          
          monthly_mean_for_rolling,
          
          .f = function(x) {
            
            # Every month in the requested rolling window
            # must be usable.
            if (any(is.na(x))) {
              return(NA_real_)
            }
            
            mean(x)
          },
          
          .before =
            n_months - 1,
          
          .complete =
            TRUE
        ),
      
      
      partial_months_in_window =
        slide_int(
          
          complete_month,
          
          .f = function(x) {
            
            if (any(is.na(x))) {
              return(NA_integer_)
            }
            
            sum(!x)
          },
          
          .before =
            n_months - 1,
          
          .complete =
            TRUE
        ),
      
      
      unusable_months_in_window =
        slide_int(
          
          month_usable_for_rolling,
          
          .f = function(x) {
            
            if (any(is.na(x))) {
              return(NA_integer_)
            }
            
            sum(!x)
          },
          
          .before =
            n_months - 1,
          
          .complete =
            TRUE
        )
      
    ) |>
    ungroup() |>
    transmute(
      
      station,
      
      window_months =
        n_months,
      
      window_start_month =
        month %m-%
        months(
          n_months - 1
        ),
      
      window_end_month =
        month,
      
      rolling_mean_cfs,
      
      partial_months_in_window,
      
      unusable_months_in_window
    )
}


# ------------------------------------------------------------
# CALCULATE ALL ROLLING WINDOWS
# ------------------------------------------------------------

rolling <- map_dfr(
  
  rolling_windows,
  
  ~make_rolling(
    monthly_roll_base,
    .x
  )
  
) |>
  arrange(
    station,
    window_months,
    window_end_month
  )


write_csv(
  rolling,
  file.path(
    output_dir,
    "03_rolling_monthly_springflow.csv"
  )
)


# ------------------------------------------------------------
# HISTORICAL RANKS AND PERCENTILES
#
# Rank 1 = lowest springflow rolling period
#
# Low percentile = historically severe low flow
# ------------------------------------------------------------

rolling_ranked <- rolling |>
  filter(
    !is.na(
      rolling_mean_cfs
    )
  ) |>
  group_by(
    station,
    window_months
  ) |>
  arrange(
    rolling_mean_cfs,
    window_end_month,
    .by_group = TRUE
  ) |>
  mutate(
    
    historical_rank_lowest =
      min_rank(
        rolling_mean_cfs
      ),
    
    historical_n =
      n(),
    
    low_flow_percentile =
      100 *
      cume_dist(
        rolling_mean_cfs
      )
    
  ) |>
  ungroup()


# ------------------------------------------------------------
# ALL-TIME LOW
# ------------------------------------------------------------

historical_low <- rolling_ranked |>
  group_by(
    station,
    window_months
  ) |>
  slice_min(
    rolling_mean_cfs,
    n = 1,
    with_ties = FALSE
  ) |>
  ungroup() |>
  select(
    
    station,
    
    window_months,
    
    historical_low_cfs =
      rolling_mean_cfs,
    
    historical_low_month =
      window_end_month
  )


# ------------------------------------------------------------
# 2025 DROUGHT LOW
# ------------------------------------------------------------

drought_2025 <- rolling_ranked |>
  filter(
    
    window_end_month >=
      floor_date(
        drought_2025_start,
        "month"
      ),
    
    window_end_month <=
      floor_date(
        drought_2025_end,
        "month"
      )
    
  ) |>
  group_by(
    station,
    window_months
  ) |>
  slice_min(
    rolling_mean_cfs,
    n = 1,
    with_ties = FALSE
  ) |>
  ungroup() |>
  select(
    
    station,
    
    window_months,
    
    drought_2025_low_cfs =
      rolling_mean_cfs,
    
    drought_2025_low_month =
      window_end_month,
    
    drought_2025_rank =
      historical_rank_lowest,
    
    drought_2025_percentile =
      low_flow_percentile,
    
    drought_2025_historical_n =
      historical_n
  )


# ------------------------------------------------------------
# CURRENT VALUES
#
# Latest valid rolling value for each station/window.
# With this dataset that should correspond to July 2026.
# ------------------------------------------------------------

current <- rolling_ranked |>
  group_by(
    station,
    window_months
  ) |>
  slice_max(
    window_end_month,
    n = 1,
    with_ties = FALSE
  ) |>
  ungroup() |>
  select(
    
    station,
    
    window_months,
    
    current_month =
      window_end_month,
    
    current_rolling_cfs =
      rolling_mean_cfs,
    
    current_rank =
      historical_rank_lowest,
    
    current_percentile =
      low_flow_percentile,
    
    current_historical_n =
      historical_n
  )


# ------------------------------------------------------------
# DROUGHT CONTEXT TABLE
# ------------------------------------------------------------

drought_context <- drought_2025 |>
  left_join(
    current,
    by = c(
      "station",
      "window_months"
    )
  ) |>
  left_join(
    historical_low,
    by = c(
      "station",
      "window_months"
    )
  ) |>
  mutate(
    
    recovery_cfs =
      current_rolling_cfs -
      drought_2025_low_cfs,
    
    recovery_pct =
      100 *
      (
        current_rolling_cfs /
          drought_2025_low_cfs -
          1
      )
    
  ) |>
  arrange(
    station,
    window_months
  )


cat("\n\n")
cat("=================================================\n")
cat("2025 DROUGHT VS CURRENT CONDITIONS\n")
cat("=================================================\n\n")


drought_context |>
  mutate(
    
    across(
      contains("cfs"),
      ~round(.x, 1)
    ),
    
    across(
      contains("percentile"),
      ~round(.x, 1)
    ),
    
    recovery_pct =
      round(
        recovery_pct,
        1
      )
    
  ) |>
  print(
    n = Inf
  )


write_csv(
  drought_context,
  file.path(
    output_dir,
    "04_drought_2025_vs_current.csv"
  )
)


# ------------------------------------------------------------
# 10 LOWEST INDIVIDUAL ROLLING PERIODS
#
# IMPORTANT:
# These are overlapping rolling windows.
#
# Therefore several rows may represent the same drought event.
# ------------------------------------------------------------

driest_10 <- rolling_ranked |>
  group_by(
    station,
    window_months
  ) |>
  slice_min(
    rolling_mean_cfs,
    n = 10,
    with_ties = FALSE
  ) |>
  ungroup() |>
  arrange(
    station,
    window_months,
    rolling_mean_cfs
  )


write_csv(
  driest_10,
  file.path(
    output_dir,
    "05_driest_10_rolling_periods.csv"
  )
)


# ------------------------------------------------------------
# ANNUAL MINIMUM ROLLING VALUE
#
# Find the lowest rolling flow ending in each calendar year.
#
# This reduces repeated representation of the same drought
# compared with the raw top-10 rolling-period table.
# ------------------------------------------------------------

annual_minimum_rolling <- rolling_ranked |>
  mutate(
    
    ending_year =
      year(
        window_end_month
      )
    
  ) |>
  group_by(
    station,
    window_months,
    ending_year
  ) |>
  slice_min(
    rolling_mean_cfs,
    n = 1,
    with_ties = FALSE
  ) |>
  ungroup() |>
  group_by(
    station,
    window_months
  ) |>
  arrange(
    rolling_mean_cfs,
    .by_group = TRUE
  ) |>
  mutate(
    
    drought_year_rank =
      row_number()
    
  ) |>
  ungroup()


write_csv(
  annual_minimum_rolling,
  file.path(
    output_dir,
    "06_annual_minimum_rolling_flow.csv"
  )
)


# ------------------------------------------------------------
# DISPLAY 10 LOWEST ANNUAL MINIMA
# ------------------------------------------------------------

cat("\n\n")
cat("=================================================\n")
cat("10 LOWEST ANNUAL MINIMA BY ROLLING WINDOW\n")
cat("=================================================\n\n")


annual_minimum_rolling |>
  filter(
    drought_year_rank <= 10
  ) |>
  select(
    
    station,
    
    window_months,
    
    drought_year_rank,
    
    ending_year,
    
    window_end_month,
    
    rolling_mean_cfs
    
  ) |>
  mutate(
    
    rolling_mean_cfs =
      round(
        rolling_mean_cfs,
        1
      )
    
  ) |>
  print(
    n = Inf
  )


# ------------------------------------------------------------
# EXTRA CHECK:
# EARLIEST VALID SAN MARCOS ROLLING VALUES
# ------------------------------------------------------------

cat("\n\n")
cat("=================================================\n")
cat("EARLIEST VALID SAN MARCOS ROLLING VALUES\n")
cat("=================================================\n\n")


rolling_ranked |>
  filter(
    station ==
      "San Marcos Springs"
  ) |>
  group_by(
    window_months
  ) |>
  slice_min(
    window_end_month,
    n = 3,
    with_ties = FALSE
  ) |>
  ungroup() |>
  select(
    station,
    window_months,
    window_start_month,
    window_end_month,
    rolling_mean_cfs
  ) |>
  arrange(
    window_months,
    window_end_month
  ) |>
  mutate(
    rolling_mean_cfs =
      round(
        rolling_mean_cfs,
        1
      )
  ) |>
  print(
    n = Inf
  )


# ------------------------------------------------------------
# FINISHED
# ------------------------------------------------------------

cat("\n\nAnalysis complete.\n")

cat(
  "\nInput:\n",
  data_path,
  "\n"
)

cat(
  "\nOutput files written to:\n",
  output_dir,
  "\n"
)