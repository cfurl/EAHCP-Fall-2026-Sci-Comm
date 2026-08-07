# ============================================================
# HISTORICAL ROLLING SPRINGFLOW
#
# Creates separate plots for:
#   - Comal Springs
#   - San Marcos Springs
#
# Variants for each station:
#   - 1 month only
#   - 3 months only
#   - 6 months only
#   - 9 months only
#   - 12 months only
#   - 18 months only
#   - 24 months only
#   - 36 months only
#   - 48 months only
#   - 60 months only
#   - all windows
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
#   - reduced legend / top text footprint
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
  "03_rolling_monthly_springflow.csv"
)

output_dir <- file.path(
  project_root,
  "Comal_mgt_presentation",
  "images",
  "rolling_line_plots"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# ------------------------------------------------------------
# LANDA LAKE PRESENTATION COLORS
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
  coral          = "#B86156",
  legend_fade    = "#C7C2B5"
)

# ------------------------------------------------------------
# WINDOW DEFINITIONS
# ------------------------------------------------------------

window_levels <- c(
  "1 month",
  "3 months",
  "6 months",
  "9 months",
  "12 months",
  "18 months",
  "24 months",
  "36 months",
  "48 months",
  "60 months"
)

plot_colors <- c(
  "1 month"   = LANDA_COLORS[["coral"]],
  "3 months"  = LANDA_COLORS[["gold"]],
  "6 months"  = LANDA_COLORS[["aquatic_light"]],
  "9 months"  = LANDA_COLORS[["shallow"]],
  "12 months" = LANDA_COLORS[["lake"]],
  "18 months" = LANDA_COLORS[["aquatic"]],
  "24 months" = LANDA_COLORS[["cypress"]],
  "36 months" = LANDA_COLORS[["limestone_dark"]],
  "48 months" = "#566E72",
  "60 months" = LANDA_COLORS[["lake_deep"]]
)

plot_widths <- c(
  "1 month"   = 0.55,
  "3 months"  = 0.65,
  "6 months"  = 0.75,
  "9 months"  = 0.80,
  "12 months" = 0.85,
  "18 months" = 0.90,
  "24 months" = 0.95,
  "36 months" = 1.00,
  "48 months" = 1.05,
  "60 months" = 1.15
)

window_lookup <- tibble(
  window_months_num = c(1, 3, 6, 9, 12, 18, 24, 36, 48, 60),
  window_label = window_levels,
  slug = c("01", "03", "06", "09", "12", "18", "24", "36", "48", "60")
)

# ------------------------------------------------------------
# READ DATA
# ------------------------------------------------------------

rolling <- read_csv(
  data_path,
  show_col_types = FALSE
) |>
  mutate(
    window_start_month = as.Date(window_start_month),
    window_end_month   = as.Date(window_end_month)
  ) |>
  left_join(
    window_lookup,
    by = c("window_months" = "window_months_num")
  ) |>
  mutate(
    window_months = factor(
      window_label,
      levels = window_levels
    )
  ) |>
  filter(!is.na(rolling_mean_cfs))

# ------------------------------------------------------------
# FIXED AXIS LIMITS BY STATION
# ------------------------------------------------------------

station_limits <- rolling |>
  group_by(station) |>
  summarise(
    xmin = min(window_end_month, na.rm = TRUE),
    xmax = max(window_end_month, na.rm = TRUE),
    ymin = 0,
    ymax = max(rolling_mean_cfs, na.rm = TRUE) * 1.03,
    .groups = "drop"
  )

# ------------------------------------------------------------
# STATION LABELS FOR SUBTITLE
# ------------------------------------------------------------

station_subtitle_lookup <- c(
  "Comal Springs" =
    "Comal monthly springflow shown as rolling averages from 1 month through 60 months",
  "San Marcos Springs" =
    "San Marcos monthly springflow shown as rolling averages from 1 month through 60 months"
)

# ------------------------------------------------------------
# THEME
# ------------------------------------------------------------

theme_landa_flow <- function() {
  
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
        size = 13.5,
        color = LANDA_COLORS[["muted"]],
        margin = margin(b = 8)
      ),
      
      axis.title.y = element_text(
        face = "bold",
        size = 15,
        color = LANDA_COLORS[["ink"]],
        margin = margin(r = 8)
      ),
      
      axis.title.x = element_blank(),
      
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
      
      legend.position = "top",
      legend.justification = "left",
      legend.title = element_blank(),
      
      legend.text = element_text(
        size = 10.5,
        color = LANDA_COLORS[["ink"]]
      ),
      
      legend.key.width = unit(0.90, "cm"),
      legend.key.height = unit(0.28, "cm"),
      legend.spacing.x = unit(0.12, "cm"),
      
      plot.margin = margin(
        8,   # top
        14,  # right
        10,  # bottom
        10   # left
      )
    )
}

# ------------------------------------------------------------
# LEGEND OVERRIDE
# ------------------------------------------------------------

make_legend_override <- function(highlight_windows = NULL) {
  
  highlight_flag <- window_levels %in% highlight_windows
  
  list(
    colour = ifelse(
      highlight_flag,
      unname(plot_colors[window_levels]),
      LANDA_COLORS[["legend_fade"]]
    ),
    linewidth = ifelse(
      highlight_flag,
      unname(plot_widths[window_levels]),
      0.50
    ),
    alpha = rep(1, length(window_levels))
  )
}

# ------------------------------------------------------------
# PLOT FUNCTION
# ------------------------------------------------------------

make_flow_plot <- function(
    station_name,
    active_windows = window_levels
) {
  
  station_data <- rolling |>
    filter(station == station_name)
  
  limits_row <- station_limits |>
    filter(station == station_name)
  
  plot_data <- station_data |>
    filter(window_months %in% active_windows)
  
  subtitle_text <- station_subtitle_lookup[[station_name]]
  
  ggplot() +
    geom_line(
      data = plot_data,
      aes(
        x = window_end_month,
        y = rolling_mean_cfs,
        color = window_months,
        linewidth = window_months,
        group = window_months
      ),
      alpha = 0.88,
      lineend = "round"
    ) +
    scale_color_manual(
      values = plot_colors,
      breaks = window_levels,
      limits = window_levels,
      drop = FALSE
    ) +
    scale_linewidth_manual(
      values = plot_widths,
      breaks = window_levels,
      limits = window_levels,
      drop = FALSE
    ) +
    scale_x_date(
      limits = c(limits_row$xmin, limits_row$xmax),
      date_breaks = "10 years",
      date_minor_breaks = "5 years",
      date_labels = "%Y",
      expand = expansion(mult = c(0, 0))
    ) +
    scale_y_continuous(
      limits = c(limits_row$ymin, limits_row$ymax),
      labels = label_number(
        accuracy = 1,
        big.mark = ","
      ),
      expand = expansion(mult = c(0, 0))
    ) +
    guides(
      color = guide_legend(
        nrow = 2,
        byrow = TRUE,
        override.aes = make_legend_override(active_windows)
      ),
      linewidth = "none"
    ) +
    labs(
      title = NULL,
      subtitle = subtitle_text,
      y = "Springflow (cfs)"
    ) +
    theme_landa_flow()
}

# ------------------------------------------------------------
# SAVE FUNCTION
#
# EXACT POWERPOINT DISPLAY SIZE
# ------------------------------------------------------------

save_flow_plot <- function(plot_obj, filename) {
  
  ggsave(
    filename = file.path(output_dir, filename),
    plot = plot_obj,
    width = 9.53,
    height = 5.98,
    units = "in",
    dpi = 300,
    bg = LANDA_COLORS[["sand"]]
  )
}

# ------------------------------------------------------------
# STATION LOOKUP
# ------------------------------------------------------------

station_lookup <- tibble(
  station = c(
    "Comal Springs",
    "San Marcos Springs"
  ),
  station_slug = c(
    "comal",
    "san_marcos"
  )
)

# ------------------------------------------------------------
# EXPORT INDIVIDUAL WINDOWS + ALL-LINES
# ------------------------------------------------------------

for (i in seq_len(nrow(station_lookup))) {
  
  station_name <- station_lookup$station[i]
  station_slug <- station_lookup$station_slug[i]
  
  # individual window plots
  for (j in seq_len(nrow(window_lookup))) {
    
    active_window <- window_lookup$window_label[j]
    active_slug   <- window_lookup$slug[j]
    
    p <- make_flow_plot(
      station_name = station_name,
      active_windows = active_window
    )
    
    file_name <- paste0(
      station_slug,
      "_rolling_",
      active_slug,
      "_month",
      ifelse(active_slug == "01", "", "s"),
      ".png"
    )
    
    save_flow_plot(
      plot_obj = p,
      filename = file_name
    )
  }
  
  # all lines plot
  p_all <- make_flow_plot(
    station_name = station_name,
    active_windows = window_levels
  )
  
  file_name_all <- paste0(
    station_slug,
    "_rolling_all_windows.png"
  )
  
  save_flow_plot(
    plot_obj = p_all,
    filename = file_name_all
  )
}

# ------------------------------------------------------------
# PREVIEW
# ------------------------------------------------------------

example_plot <- make_flow_plot(
  station_name = "Comal Springs",
  active_windows = "1 month"
)

print(example_plot)

# ------------------------------------------------------------
# FINISHED
# ------------------------------------------------------------

cat("\nPlots written to:\n")
cat(output_dir, "\n")

cat("\nImage dimensions:\n")
cat("9.53 inches wide x 5.98 inches high at 300 dpi\n")