# ============================================================
# WATER TEMPERATURE DATA GAP CHECK
#
# Combined files:
#   HCP1_SPRING7_2025_2026.csv
#   HCP2_SPRING3_2025_2026.csv
#   HCP3_OLDCHAN_2025_2026.csv
#
# A record is considered missing when:
#
#   1. The expected 15-minute timestamp is absent
#
#      OR
#
#   2. The timestamp exists but val_Fin is:
#        NA
#        blank
#        "NULL"
#
# Only continuous gaps >= 1 hour are reported.
#
# This is important because:
#   - 2025 data have already been corrected and invalid
#     values may be represented by NULL/blank val_Fin.
#   - 2026 data are raw, so outages may appear primarily
#     as missing timestamps.
#
# dt_CST is treated as fixed clock time.
# UTC is used only as a neutral parsing container.
# ============================================================

library(tidyverse)
library(lubridate)


# ------------------------------------------------------------
# DATA DIRECTORY
# ------------------------------------------------------------

data_dir <- paste0(
  "C:/Users/cfurl/OneDrive - Edwards Aquifer Authority/",
  "SC_meetings/sep_2026/fall_meeting/",
  "EAHCP-Fall-2026-Sci-Comm/",
  "Comal_mgt_presentation/data"
)


# ------------------------------------------------------------
# COMBINED FILES
# ------------------------------------------------------------

files <- c(
  "HCP1_SPRING7" = "HCP1_SPRING7_2025_2026.csv",
  "HCP2_SPRING3" = "HCP2_SPRING3_2025_2026.csv",
  "HCP3_OLDCHAN" = "HCP3_OLDCHAN_2025_2026.csv"
)


# ------------------------------------------------------------
# READ ONE STATION
# ------------------------------------------------------------

read_station <- function(
    station_name,
    file_name
) {
  
  dat <- read_csv(
    file.path(
      data_dir,
      file_name
    ),
    col_types = cols(
      .default = col_character()
    ),
    show_col_types = FALSE
  ) |>
    
    filter(
      SensorName == "WATTEMP"
    ) |>
    
    mutate(
      
      # Neutral timezone: preserve source clock time
      dt_CST = ymd_hm(
        dt_CST,
        tz = "UTC",
        quiet = TRUE
      ),
      
      # Identify missing/NULL values BEFORE numeric conversion
      value_missing =
        is.na(val_Fin) |
        str_trim(coalesce(val_Fin, "")) == "" |
        toupper(str_trim(coalesce(val_Fin, ""))) == "NULL",
      
      value_numeric = suppressWarnings(
        as.numeric(val_Fin)
      )
    ) |>
    
    arrange(dt_CST)
  
  
  # ----------------------------------------------------------
  # DATETIME CHECK
  # ----------------------------------------------------------
  
  if (any(is.na(dat$dt_CST))) {
    
    stop(
      paste0(
        "Some dt_CST values failed to parse for ",
        station_name
      )
    )
  }
  
  
  dat
}


# ------------------------------------------------------------
# READ ALL THREE STATIONS
# ------------------------------------------------------------

station_data <- imap(
  files,
  ~ read_station(
    station_name = .y,
    file_name = .x
  )
)


# ------------------------------------------------------------
# STORAGE
# ------------------------------------------------------------

all_reportable_gaps <- list()
all_null_summary <- list()
all_year_summary <- list()


# ------------------------------------------------------------
# ANALYZE EACH STATION
# ------------------------------------------------------------

for (station_name in names(station_data)) {
  
  dat <- station_data[[station_name]]
  
  
  cat("\n")
  cat("============================================================\n")
  cat(station_name, "\n")
  cat("============================================================\n\n")
  
  
  # ----------------------------------------------------------
  # DATA RANGE
  # ----------------------------------------------------------
  
  first_time <- min(
    dat$dt_CST,
    na.rm = TRUE
  )
  
  last_time <- max(
    dat$dt_CST,
    na.rm = TRUE
  )
  
  
  cat(
    "First observation:",
    format(
      first_time,
      "%Y-%m-%d %H:%M",
      tz = "UTC"
    ),
    "\n"
  )
  
  cat(
    "Last observation: ",
    format(
      last_time,
      "%Y-%m-%d %H:%M",
      tz = "UTC"
    ),
    "\n\n"
  )
  
  
  # ----------------------------------------------------------
  # CHECK FOR DUPLICATE TIMESTAMPS
  # ----------------------------------------------------------
  
  duplicates <- dat |>
    count(
      dt_CST,
      name = "n"
    ) |>
    filter(
      n > 1
    )
  
  if (nrow(duplicates) > 0) {
    
    cat(
      "WARNING:",
      nrow(duplicates),
      "duplicate timestamps found.\n\n"
    )
  }
  
  
  # ----------------------------------------------------------
  # BUILD COMPLETE EXPECTED 15-MINUTE SERIES
  # ----------------------------------------------------------
  
  expected <- tibble(
    dt_CST = seq(
      from = first_time,
      to = last_time,
      by = "15 min"
    )
  )
  
  
  # ----------------------------------------------------------
  # REDUCE ORIGINAL DATA TO ONE ROW PER TIMESTAMP
  # ----------------------------------------------------------
  
  observed <- dat |>
    group_by(dt_CST) |>
    summarise(
      
      timestamp_present = TRUE,
      
      # If any row at this timestamp contains a valid result,
      # treat the timestamp as having data.
      value_missing = all(value_missing),
      
      .groups = "drop"
    )
  
  
  # ----------------------------------------------------------
  # JOIN EXPECTED + OBSERVED
  # ----------------------------------------------------------
  
  coverage <- expected |>
    
    left_join(
      observed,
      by = "dt_CST"
    ) |>
    
    mutate(
      
      timestamp_missing =
        is.na(timestamp_present),
      
      value_missing =
        if_else(
          timestamp_missing,
          FALSE,
          replace_na(
            value_missing,
            FALSE
          )
        ),
      
      # Overall missing condition
      missing =
        timestamp_missing |
        value_missing,
      
      year =
        year(dt_CST),
      
      gap_type = case_when(
        
        timestamp_missing ~
          "Missing timestamp",
        
        value_missing ~
          "Missing/NULL val_Fin",
        
        TRUE ~
          "Observed"
      )
    )
  
  
  # ----------------------------------------------------------
  # YEAR-BY-YEAR SUMMARY
  # ----------------------------------------------------------
  
  year_summary <- coverage |>
    group_by(year) |>
    summarise(
      
      expected_records = n(),
      
      missing_timestamps =
        sum(timestamp_missing),
      
      missing_values =
        sum(value_missing),
      
      total_missing_records =
        sum(missing),
      
      completeness_pct =
        100 *
        (1 - sum(missing) / n()),
      
      .groups = "drop"
    ) |>
    
    mutate(
      station = station_name,
      .before = 1
    )
  
  
  all_year_summary[[station_name]] <-
    year_summary
  
  
  # ----------------------------------------------------------
  # SPECIFICALLY CHECK NULL / BLANK / NA val_Fin BY YEAR
  # ----------------------------------------------------------
  
  null_summary <- dat |>
    mutate(
      year = year(dt_CST)
    ) |>
    group_by(year) |>
    summarise(
      
      rows = n(),
      
      missing_or_null_val_Fin =
        sum(value_missing),
      
      .groups = "drop"
    ) |>
    
    mutate(
      station = station_name,
      .before = 1
    )
  
  
  all_null_summary[[station_name]] <-
    null_summary
  
  
  # ----------------------------------------------------------
  # IDENTIFY CONTINUOUS MISSING PERIODS
  # ----------------------------------------------------------
  
  missing_only <- coverage |>
    filter(missing) |>
    arrange(dt_CST)
  
  
  if (nrow(missing_only) == 0) {
    
    gaps <- tibble()
    
  } else {
    
    gaps <- missing_only |>
      
      mutate(
        
        minutes_from_previous =
          as.numeric(
            difftime(
              dt_CST,
              lag(dt_CST),
              units = "mins"
            )
          ),
        
        new_gap =
          is.na(minutes_from_previous) |
          minutes_from_previous > 15,
        
        gap_id =
          cumsum(new_gap)
      ) |>
      
      group_by(gap_id) |>
      
      summarise(
        
        gap_start =
          min(dt_CST),
        
        gap_end =
          max(dt_CST),
        
        missing_records =
          n(),
        
        # Four missing 15-minute observations = 1 hour
        gap_hours =
          missing_records * 0.25,
        
        missing_timestamp_records =
          sum(timestamp_missing),
        
        missing_value_records =
          sum(value_missing),
        
        .groups = "drop"
      ) |>
      
      mutate(
        
        station = station_name,
        
        gap_reason = case_when(
          
          missing_timestamp_records > 0 &
            missing_value_records > 0 ~
            "Missing timestamps + missing values",
          
          missing_timestamp_records > 0 ~
            "Missing timestamps",
          
          missing_value_records > 0 ~
            "Missing/NULL val_Fin",
          
          TRUE ~
            "Unknown"
        ),
        
        .before = 1
      ) |>
      
      select(
        station,
        gap_start,
        gap_end,
        missing_records,
        gap_hours,
        gap_reason,
        missing_timestamp_records,
        missing_value_records
      )
    
    
    # --------------------------------------------------------
    # ONLY KEEP GAPS >= 1 HOUR
    # --------------------------------------------------------
    
    gaps <- gaps |>
      filter(
        gap_hours >= 1
      )
  }
  
  
  all_reportable_gaps[[station_name]] <-
    gaps
  
  
  # ----------------------------------------------------------
  # PRINT STATION SUMMARY
  # ----------------------------------------------------------
  
  cat("MISSING / NULL VALUES BY YEAR:\n\n")
  
  null_summary |>
    print(
      n = Inf
    )
  
  
  cat("\nDATA COVERAGE BY YEAR:\n\n")
  
  year_summary |>
    mutate(
      completeness_pct =
        round(
          completeness_pct,
          3
        )
    ) |>
    print(
      n = Inf
    )
  
  
  cat("\nGAPS >= 1 HOUR:\n\n")
  
  
  if (nrow(gaps) == 0) {
    
    cat(
      "None.\n"
    )
    
  } else {
    
    gaps |>
      mutate(
        
        gap_start =
          format(
            gap_start,
            "%Y-%m-%d %H:%M",
            tz = "UTC"
          ),
        
        gap_end =
          format(
            gap_end,
            "%Y-%m-%d %H:%M",
            tz = "UTC"
          )
      ) |>
      
      print(
        n = Inf,
        width = Inf
      )
  }
}


# ============================================================
# COMBINE RESULTS
# ============================================================


# ------------------------------------------------------------
# YEAR SUMMARY
# ------------------------------------------------------------

year_summary_all <- bind_rows(
  all_year_summary
) |>
  arrange(
    station,
    year
  )


cat("\n\n")
cat("============================================================\n")
cat("OVERALL COVERAGE BY STATION AND YEAR\n")
cat("============================================================\n\n")

year_summary_all |>
  mutate(
    completeness_pct =
      round(
        completeness_pct,
        3
      )
  ) |>
  print(
    n = Inf,
    width = Inf
  )


# ------------------------------------------------------------
# NULL / BLANK / NA CHECK
# ------------------------------------------------------------

null_summary_all <- bind_rows(
  all_null_summary
) |>
  arrange(
    station,
    year
  )


cat("\n\n")
cat("============================================================\n")
cat("NULL / BLANK / NA val_Fin CHECK\n")
cat("============================================================\n\n")

null_summary_all |>
  print(
    n = Inf,
    width = Inf
  )


# ------------------------------------------------------------
# REPORTABLE GAPS
# ------------------------------------------------------------

reportable_gaps <- bind_rows(
  all_reportable_gaps
) |>
  arrange(
    station,
    gap_start
  )


cat("\n\n")
cat("============================================================\n")
cat("REPORTABLE DATA GAPS — 1 HOUR OR LONGER\n")
cat("============================================================\n\n")


if (nrow(reportable_gaps) == 0) {
  
  cat(
    "No gaps of 1 hour or longer were found.\n"
  )
  
} else {
  
  reportable_gaps |>
    mutate(
      
      gap_start =
        format(
          gap_start,
          "%Y-%m-%d %H:%M",
          tz = "UTC"
        ),
      
      gap_end =
        format(
          gap_end,
          "%Y-%m-%d %H:%M",
          tz = "UTC"
        ),
      
      gap_hours =
        round(
          gap_hours,
          2
        )
    ) |>
    
    print(
      n = Inf,
      width = Inf
    )
}