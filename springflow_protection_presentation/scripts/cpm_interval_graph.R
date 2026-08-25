# =============================================================================
# EAHCP Fall 2026 Science Committee
# Historic Comal and San Marcos springflow with CPM stage shading, 2008-2025
#
# The shaded intervals are limited to the San Antonio Pool because Comal
# Springs and San Marcos Springs are San Antonio Pool CPM indicators.
#
# Output:
#   historical_springflow_with_cpm_stages_2008_2025.png
# =============================================================================

library(tidyverse)
library(scales)
library(ragg)
library(grid)

# -----------------------------------------------------------------------------
# Paths
# -----------------------------------------------------------------------------

project_root <- paste0(
  "C:/Users/cfurl/OneDrive - Edwards Aquifer Authority/",
  "SC_meetings/sep_2026/fall_meeting/",
  "EAHCP-Fall-2026-Sci-Comm/",
  "springflow_protection_presentation"
)

springflow_file <- file.path(
  project_root,
  "data",
  "HistoricDailyMeanSpringflow.csv"
)

cpm_file <- file.path(
  project_root,
  "data",
  "cpm_stage_intervals_2008_2025_with_days.csv"
)

theme_file <- file.path(
  project_root,
  "scripts",
  "eahcp_plot_theme.R"
)

output_dir <- file.path(
  project_root,
  "output"
)

output_file <- file.path(
  output_dir,
  "historical_springflow_with_cpm_stages_2008_2025.png"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

source(theme_file)

# -----------------------------------------------------------------------------
# Analysis period
# -----------------------------------------------------------------------------

start_date <- as.Date("2008-01-01")
end_date   <- as.Date("2025-12-31")

# Clean x-axis labeling:
# every 2 years through 2022, then 2025 as the final year.
year_breaks <- as.Date(
  c(
    paste0(seq(2008, 2022, by = 2), "-01-01"),
    "2025-01-01"
  )
)

# -----------------------------------------------------------------------------
# Read historic daily mean springflow
# -----------------------------------------------------------------------------

springflow <- read_csv(
  springflow_file,
  col_types = cols(
    Day = col_date(),
    `Comal Springs (cfs)` = col_double(),
    `San Marcos Springs (cfs)` = col_double()
  )
) %>%
  filter(
    Day >= start_date,
    Day <= end_date
  ) %>%
  rename(
    Comal = `Comal Springs (cfs)`,
    `San Marcos` = `San Marcos Springs (cfs)`
  ) %>%
  pivot_longer(
    cols = c(
      Comal,
      `San Marcos`
    ),
    names_to = "spring",
    values_to = "springflow_cfs"
  ) %>%
  mutate(
    spring = factor(
      spring,
      levels = c(
        "Comal",
        "San Marcos"
      )
    )
  )

# -----------------------------------------------------------------------------
# Read CPM stage intervals
#
# Only San Antonio Pool intervals are used. Comal and San Marcos springflow
# are indicators for the San Antonio Pool, while the Uvalde Pool is managed
# using the J-27 index well.
# -----------------------------------------------------------------------------

cpm_intervals <- read_csv(
  cpm_file,
  col_types = cols(
    year      = col_integer(),
    pool      = col_character(),
    startdate = col_date(),
    enddate   = col_date(),
    stage     = col_integer(),
    days      = col_integer()
  )
) %>%
  mutate(
    pool = str_to_lower(
      str_squish(pool)
    )
  ) %>%
  filter(
    pool == "san antonio",
    startdate <= end_date,
    enddate >= start_date,
    stage %in% 1:5
  ) %>%
  transmute(
    startdate = pmax(
      startdate,
      start_date
    ),
    
    # Add one day so inclusive intervals shade through their listed end date.
    # Cap at 2025-12-31 so the plot does not extend into 2026.
    enddate = pmin(
      enddate + days(1),
      end_date
    ),
    
    stage = factor(
      stage,
      levels = 1:5,
      labels = paste(
        "Stage",
        1:5
      )
    )
  ) %>%
  arrange(
    startdate
  )

# -----------------------------------------------------------------------------
# Validate input data
# -----------------------------------------------------------------------------

if (nrow(springflow) == 0) {
  stop(
    "No springflow records were found between ",
    start_date,
    " and ",
    end_date,
    "."
  )
}

if (nrow(cpm_intervals) == 0) {
  stop(
    "No San Antonio Pool CPM intervals were found between ",
    start_date,
    " and ",
    end_date,
    "."
  )
}

# -----------------------------------------------------------------------------
# EAHCP presentation colors
# -----------------------------------------------------------------------------

spring_colors <- c(
  "Comal"      = EAHCP_COLORS[["aquifer"]],
  "San Marcos" = EAHCP_COLORS[["spring"]]
)

stage_colors <- c(
  "Stage 1" = EAHCP_COLORS[["flow"]],
  "Stage 2" = "#8FAF7D",
  "Stage 3" = "#D6A85B",
  "Stage 4" = EAHCP_COLORS[["alert"]],
  "Stage 5" = "#7A63A8"
)

# -----------------------------------------------------------------------------
# Create chart
# -----------------------------------------------------------------------------

springflow_plot <- ggplot() +
  
  # CPM intervals drawn first so springflow remains visible.
  geom_rect(
    data = cpm_intervals,
    aes(
      xmin = startdate,
      xmax = enddate,
      ymin = -Inf,
      ymax = Inf,
      fill = stage
    ),
    inherit.aes = FALSE,
    alpha = 0.30,
    color = NA
  ) +
  
  geom_line(
    data = springflow,
    aes(
      x = Day,
      y = springflow_cfs,
      color = spring,
      group = spring
    ),
    linewidth = 0.55,
    alpha = 0.96,
    na.rm = TRUE
  ) +
  
  scale_color_manual(
    values = spring_colors,
    breaks = c(
      "Comal",
      "San Marcos"
    ),
    labels = c(
      "Comal Springs",
      "San Marcos Springs"
    ),
    name = NULL
  ) +
  
  scale_fill_manual(
    values = stage_colors,
    breaks = paste(
      "Stage",
      1:5
    ),
    name = "San Antonio Pool CPM stage",
    drop = FALSE
  ) +
  
  scale_x_date(
    limits = c(
      start_date,
      end_date
    ),
    breaks = year_breaks,
    date_labels = "%Y",
    expand = expansion(
      mult = c(0, 0)
    )
  ) +
  
  scale_y_continuous(
    labels = label_comma(
      accuracy = 1
    ),
    expand = expansion(
      mult = c(0.02, 0.07)
    )
  ) +
  
  labs(
    x = "Year",
    y = "Daily mean springflow (cfs)"
  ) +
  
  guides(
    color = guide_legend(
      order = 1,
      nrow = 1,
      byrow = TRUE,
      override.aes = list(
        linewidth = 1.5,
        alpha = 1
      )
    ),
    fill = guide_legend(
      order = 2,
      nrow = 1,
      byrow = TRUE,
      override.aes = list(
        alpha = 0.60
      )
    )
  ) +
  
  theme_eahcp(
    base_size = 16
  ) +
  
  theme(
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    plot.caption = element_blank(),
    
    legend.position = "top",
    legend.justification = "center",
    legend.box = "vertical",
    legend.box.just = "center",
    
    legend.title = element_text(
      face = "bold",
      size = 10.8,
      color = EAHCP_COLORS[["ink"]]
    ),
    
    legend.text = element_text(
      size = 10.8,
      color = EAHCP_COLORS[["ink"]]
    ),
    
    legend.key.width = unit(
      0.82,
      "cm"
    ),
    
    legend.key.height = unit(
      0.38,
      "cm"
    ),
    
    legend.spacing.x = unit(
      0.18,
      "cm"
    ),
    
    legend.spacing.y = unit(
      0.08,
      "cm"
    ),
    
    axis.text.x = element_text(
      size = 10,
      face = "bold",
      angle = 0,
      hjust = 0.5
    ),
    
    axis.text.y = element_text(
      size = 10.5
    ),
    
    axis.title = element_text(
      size = 12,
      face = "bold"
    ),
    
    panel.grid.major.x = element_line(
      color = "#E3E6E4",
      linewidth = 0.35
    ),
    
    plot.margin = margin(
      t = 8,
      r = 18,
      b = 12,
      l = 12
    )
  )

# -----------------------------------------------------------------------------
# Display
# -----------------------------------------------------------------------------

print(springflow_plot)

# -----------------------------------------------------------------------------
# Save slide-ready PNG
# -----------------------------------------------------------------------------

ggsave(
  filename = output_file,
  plot = springflow_plot,
  device = ragg::agg_png,
  width = 12.2,
  height = 6.4,
  units = "in",
  dpi = 320,
  bg = EAHCP_COLORS[["paper"]]
)

message(
  "Plot written to: ",
  normalizePath(
    output_file,
    winslash = "\\",
    mustWork = FALSE
  )
)