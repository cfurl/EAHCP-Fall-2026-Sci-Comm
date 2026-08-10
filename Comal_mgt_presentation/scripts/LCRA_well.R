# ============================================================
# LCRA WELL WATER LEVEL ELEVATION TIME SERIES
#
# Source:
#   LCRA304_WLE.csv
#
# Output:
#   lcra304_wle_daily_timeseries.png
#
# Plot:
#   Daily average water level elevation (WLE) from 2025-01-01
#   through the most recent available record.
#
# Additions:
#   - thin horizontal reference line at 621.5 ft amsl
#   - small label near right side of plot
#
# Style:
#   Consistent with other Landa Lake / Comal management plots
# ============================================================

library(tidyverse)
library(lubridate)
library(scales)
library(grid)

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
  "Comal_mgt_presentation",
  "data",
  "LCRA304_WLE.csv"
)

output_dir <- file.path(
  project_root,
  "Comal_mgt_presentation",
  "images"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

output_file <- file.path(
  output_dir,
  "lcra304_wle_daily_timeseries.png"
)

# ------------------------------------------------------------
# COLORS
# ------------------------------------------------------------

LANDA_COLORS <- c(
  lake_deep      = "#123240",
  lake           = "#1E6F73",
  shallow        = "#6CB7AF",
  aquatic        = "#4F7D4F",
  aquatic_light  = "#8BAE66",
  cypress        = "#3B5B45",
  limestone      = "#D8CFBA",
  limestone_dark = "#A99B80",
  sand           = "#F6F2E8",
  paper          = "#FBFAF4",
  ink            = "#243036",
  muted          = "#65737A",
  gold           = "#D6A13A",
  coral          = "#B86156"
)

# ------------------------------------------------------------
# SETTINGS
# ------------------------------------------------------------

start_date <- as.Date("2025-01-01")
reference_elevation <- 621.5

# ------------------------------------------------------------
# READ DATA
# ------------------------------------------------------------

wle_raw <- read_csv(
  data_path,
  show_col_types = FALSE
) |>
  mutate(
    datetime_cst = mdy_hm(
      dt_CST,
      tz = "America/Chicago"
    ),
    WLE = as.numeric(WLE)
  ) |>
  filter(
    !is.na(datetime_cst),
    !is.na(WLE)
  ) |>
  arrange(datetime_cst)

# ------------------------------------------------------------
# FILTER DATE RANGE
# ------------------------------------------------------------

latest_date <- max(
  as.Date(wle_raw$datetime_cst),
  na.rm = TRUE
)

# ------------------------------------------------------------
# AGGREGATE TO DAILY MEAN
# ------------------------------------------------------------

wle_daily <- wle_raw |>
  mutate(date = as.Date(datetime_cst)) |>
  filter(
    date >= start_date,
    date <= latest_date
  ) |>
  group_by(date) |>
  summarise(
    daily_mean_wle = mean(WLE, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  ) |>
  arrange(date)

# ------------------------------------------------------------
# BASIC CHECKS
# ------------------------------------------------------------

cat("\nDaily date range in plot data:\n")
print(range(wle_daily$date))

cat("\nNumber of daily records:\n")
print(nrow(wle_daily))

cat("\nDaily observation counts summary:\n")
print(summary(wle_daily$n_obs))

# ------------------------------------------------------------
# AXIS LIMITS
# ------------------------------------------------------------

y_min <- floor(min(c(
  wle_daily$daily_mean_wle,
  reference_elevation
), na.rm = TRUE) * 2) / 2

y_max <- ceiling(max(c(
  wle_daily$daily_mean_wle,
  reference_elevation
), na.rm = TRUE) * 2) / 2

# ------------------------------------------------------------
# LABEL POSITION
# ------------------------------------------------------------

label_x <- latest_date - days(18)

# ------------------------------------------------------------
# THEME
# ------------------------------------------------------------

theme_landa_small <- function() {
  theme_minimal(
    base_size = 18,
    base_family = "Aptos"
  ) +
    theme(
      plot.background = element_rect(
        fill = LANDA_COLORS[["sand"]],
        color = NA
      ),
      panel.background = element_rect(
        fill = LANDA_COLORS[["sand"]],
        color = NA
      ),
      plot.title = element_blank(),
      plot.subtitle = element_text(
        size = 14,
        color = LANDA_COLORS[["muted"]],
        margin = margin(b = 8)
      ),
      axis.title.x = element_blank(),
      axis.title.y = element_text(
        face = "bold",
        size = 15,
        color = LANDA_COLORS[["ink"]],
        margin = margin(r = 8)
      ),
      axis.text = element_text(
        size = 12,
        color = LANDA_COLORS[["muted"]]
      ),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(
        color = "#DCD7C8",
        linewidth = 0.45
      ),
      axis.line.x = element_line(
        color = LANDA_COLORS[["muted"]],
        linewidth = 0.45
      ),
      axis.ticks = element_blank(),
      legend.position = "none",
      plot.margin = margin(
        8,   # top
        14,  # right
        10,  # bottom
        10   # left
      )
    )
}

# ------------------------------------------------------------
# MAKE PLOT
# ------------------------------------------------------------

wle_plot <- ggplot(
  wle_daily,
  aes(
    x = date,
    y = daily_mean_wle
  )
) +
  geom_hline(
    yintercept = reference_elevation,
    color = LANDA_COLORS[["muted"]],
    linewidth = 0.4,
    linetype = "22"
  ) +
  annotate(
    "label",
    x = label_x,
    y = reference_elevation,
    label = "621.5 ft",
    hjust = 0,
    vjust = -0.4,
    size = 3.2,
    label.size = 0,
    label.padding = unit(0.08, "lines"),
    fill = alpha(LANDA_COLORS[["sand"]], 0.85),
    color = LANDA_COLORS[["muted"]],
    family = "Aptos"
  ) +
  geom_line(
    color = LANDA_COLORS[["lake"]],
    linewidth = 0.9,
    alpha = 0.95,
    lineend = "round"
  ) +
  scale_x_date(
    limits = c(start_date, latest_date),
    date_breaks = "2 months",
    date_labels = "%b\n%Y",
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    limits = c(y_min, y_max),
    labels = label_number(accuracy = 0.1),
    expand = expansion(mult = c(0.01, 0.03))
  ) +
  labs(
    title = NULL,
    subtitle = "Water Level Elevation at LCRA Well",
    y = "Water level elevation (ft amsl)"
  ) +
  theme_landa_small()

print(wle_plot)

# ------------------------------------------------------------
# SAVE PLOT
# ------------------------------------------------------------

ggsave(
  filename = output_file,
  plot = wle_plot,
  width = 6.5,
  height = 4.75,
  units = "in",
  dpi = 300,
  bg = LANDA_COLORS[["sand"]]
)

# ------------------------------------------------------------
# FINISHED
# ------------------------------------------------------------

cat("\nPlot written to:\n")
cat(output_file, "\n")

cat("\nImage dimensions:\n")
cat("6.5 inches wide x 4.75 inches high at 300 dpi\n")