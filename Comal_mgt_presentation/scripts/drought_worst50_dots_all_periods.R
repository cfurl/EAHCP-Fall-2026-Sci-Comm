# ============================================================
# DRIEST 50 ROLLING PERIODS — JITTERED DOT PLOTS
#
# Source:
#   05_driest_50_rolling_periods.csv
#
# Output:
#   - comal_driest50_dotplot.png
#   - san_marcos_driest50_dotplot.png
#
# Goal:
#   Show the 50 lowest rolling-average periods for each
#   time window, highlighting:
#     - 1950s in red circles
#     - 2020s in blue circles
#     - all other years as smaller black triangles
#
# PowerPoint figure size:
#   Width  = 9.53 inches
#   Height = 5.98 inches
#
# Design intent:
#   Keep the slide title as the dominant headline.
#   The PNG behaves like a figure panel:
#   - no main plot title
#   - station name included in subtitle
#   - slightly larger labels for readability on slide
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
  "output",
  "flow_statistics",
  "05_driest_50_rolling_periods.csv"
)

output_dir <- file.path(
  project_root,
  "Comal_mgt_presentation",
  "images",
  "dot_plots"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
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

period_colors <- c(
  "1950s"       = LANDA_COLORS[["coral"]],
  "2020s"       = LANDA_COLORS[["lake"]],
  "Other years" = "#000000"
)

# ------------------------------------------------------------
# WINDOW LABELS
# ------------------------------------------------------------

window_levels_num <- c(
  1, 3, 6, 9, 12, 18, 24, 36, 48, 60
)

window_labels <- c(
  "1 mo",
  "3 mo",
  "6 mo",
  "9 mo",
  "12 mo",
  "18 mo",
  "24 mo",
  "36 mo",
  "48 mo",
  "60 mo"
)

# ------------------------------------------------------------
# SUBTITLE LOOKUP
# ------------------------------------------------------------

station_subtitle_lookup <- c(
  "Comal Springs" =
    "Comal driest 50 rolling-average periods by time window",
  "San Marcos Springs" =
    "San Marcos driest 50 rolling-average periods by time window"
)

# ------------------------------------------------------------
# READ AND PREP DATA
# ------------------------------------------------------------

dot_df <- read_csv(
  data_path,
  show_col_types = FALSE
) |>
  mutate(
    window_start_month = as.Date(window_start_month),
    window_end_month   = as.Date(window_end_month),
    end_year           = year(window_end_month),
    
    period_group = case_when(
      end_year >= 1950 & end_year <= 1959 ~ "1950s",
      end_year >= 2020 & end_year <= 2029 ~ "2020s",
      TRUE ~ "Other years"
    ),
    
    period_group = factor(
      period_group,
      levels = c("1950s", "2020s", "Other years")
    ),
    
    window_months = factor(
      window_months,
      levels = window_levels_num,
      labels = window_labels
    )
  ) |>
  arrange(
    station,
    window_months,
    rolling_mean_cfs
  )

# ------------------------------------------------------------
# FIXED Y LIMITS BY STATION
# ------------------------------------------------------------

station_limits <- dot_df |>
  group_by(station) |>
  summarise(
    ymin = 0,
    ymax = max(rolling_mean_cfs, na.rm = TRUE) * 1.04,
    .groups = "drop"
  )

# ------------------------------------------------------------
# THEME
# ------------------------------------------------------------

theme_landa_dot <- function() {
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
        size = 16,
        color = LANDA_COLORS[["ink"]],
        margin = margin(r = 10)
      ),
      
      axis.text = element_text(
        size = 13,
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
      
      legend.position = "top",
      legend.justification = "left",
      legend.title = element_blank(),
      
      legend.text = element_text(
        size = 11,
        color = LANDA_COLORS[["ink"]]
      ),
      
      legend.key.width = unit(0.9, "cm"),
      legend.key.height = unit(0.32, "cm"),
      legend.spacing.x = unit(0.18, "cm"),
      
      plot.margin = margin(
        8,   # top
        14,  # right
        10,  # bottom
        10   # left
      )
    )
}

# ------------------------------------------------------------
# PLOT FUNCTION
# ------------------------------------------------------------

make_driest50_dotplot <- function(station_name) {
  
  plot_data <- dot_df |>
    filter(station == station_name)
  
  lims <- station_limits |>
    filter(station == station_name)
  
  subtitle_text <- station_subtitle_lookup[[station_name]]
  
  ggplot(
    plot_data,
    aes(
      x = window_months,
      y = rolling_mean_cfs,
      fill = period_group,
      color = period_group,
      shape = period_group,
      size  = period_group,
      alpha = period_group
    )
  ) +
    geom_jitter(
      width = 0.16,
      height = 0,
      stroke = 0.2
    ) +
    scale_fill_manual(
      values = period_colors,
      drop = FALSE
    ) +
    scale_color_manual(
      values = period_colors,
      drop = FALSE
    ) +
    scale_shape_manual(
      values = c(
        "1950s" = 21,
        "2020s" = 21,
        "Other years" = 17
      ),
      drop = FALSE
    ) +
    scale_size_manual(
      values = c(
        "1950s" = 2.9,
        "2020s" = 2.9,
        "Other years" = 1.6
      ),
      guide = "none"
    ) +
    scale_alpha_manual(
      values = c(
        "1950s" = 0.68,
        "2020s" = 0.68,
        "Other years" = 0.60
      ),
      guide = "none"
    ) +
    scale_y_continuous(
      limits = c(lims$ymin, lims$ymax),
      labels = label_number(
        accuracy = 1,
        big.mark = ","
      ),
      expand = expansion(mult = c(0, 0))
    ) +
    labs(
      title = NULL,
      subtitle = subtitle_text,
      y = "Rolling springflow (cfs)"
    ) +
    guides(
      fill = "none",
      color = "none",
      shape = guide_legend(
        override.aes = list(
          fill = c(
            period_colors[["1950s"]],
            period_colors[["2020s"]],
            NA
          ),
          color = c(
            period_colors[["1950s"]],
            period_colors[["2020s"]],
            period_colors[["Other years"]]
          ),
          size = c(2.9, 2.9, 1.9),
          alpha = c(0.68, 0.68, 0.85)
        )
      )
    ) +
    theme_landa_dot()
}

# ------------------------------------------------------------
# MAKE PLOTS
# ------------------------------------------------------------

comal_plot <- make_driest50_dotplot(
  station_name = "Comal Springs"
)

san_marcos_plot <- make_driest50_dotplot(
  station_name = "San Marcos Springs"
)

# Optional preview in session
print(comal_plot)
print(san_marcos_plot)

# ------------------------------------------------------------
# SAVE PLOTS
# ------------------------------------------------------------

ggsave(
  filename = file.path(
    output_dir,
    "comal_driest50_dotplot.png"
  ),
  plot = comal_plot,
  width = 9.53,
  height = 5.98,
  units = "in",
  dpi = 300,
  bg = LANDA_COLORS[["sand"]]
)

ggsave(
  filename = file.path(
    output_dir,
    "san_marcos_driest50_dotplot.png"
  ),
  plot = san_marcos_plot,
  width = 9.53,
  height = 5.98,
  units = "in",
  dpi = 300,
  bg = LANDA_COLORS[["sand"]]
)

# ------------------------------------------------------------
# OPTIONAL CONSOLE COUNTS
# ------------------------------------------------------------

cat("\nCounts by station, window, and period group:\n\n")

dot_df |>
  count(
    station,
    window_months,
    period_group
  ) |>
  pivot_wider(
    names_from = period_group,
    values_from = n,
    values_fill = 0
  ) |>
  print(n = Inf)

cat("\nPlots written to:\n")
cat(output_dir, "\n")

cat("\nImage dimensions:\n")
cat("9.53 inches wide x 5.98 inches high at 300 dpi\n")