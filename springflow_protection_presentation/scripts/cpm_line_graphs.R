# =============================================================================
# EAHCP Fall 2026 Science Committee
# Permitted pumping, CPM-adjusted pumping, and actual pumping
#
# Outputs:
#   1. permitted_after_cpm_actual_pumped_total.png
#   2. permitted_after_cpm_actual_pumped_by_pool_type.png
# =============================================================================

library(tidyverse)
library(scales)
library(patchwork)
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

input_file <- file.path(
  project_root,
  "data",
  "permitted_pumped_by_type_pool.csv"
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

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

source(theme_file)

# -----------------------------------------------------------------------------
# Read and validate data
# -----------------------------------------------------------------------------

pumping_data <- read_csv(
  input_file,
  col_types = cols(
    year                  = col_integer(),
    permitted_amt         = col_double(),
    after_cpm             = col_double(),
    pumped                = col_double(),
    unpumped              = col_double(),
    pool                  = col_character(),
    permit_type           = col_character(),
    cpm_reduction_percent = col_double()
  )
)

required_columns <- c(
  "year",
  "permitted_amt",
  "after_cpm",
  "pumped",
  "pool",
  "permit_type"
)

missing_columns <- setdiff(
  required_columns,
  names(pumping_data)
)

if (length(missing_columns) > 0) {
  stop(
    "Missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

pumping_data <- pumping_data %>%
  mutate(
    pool = str_to_lower(str_squish(pool)),
    permit_type = str_to_lower(str_squish(permit_type))
  )

expected_pools <- c(
  "sanantonio",
  "uvalde"
)

expected_permit_types <- c(
  "mandi",
  "irrigation"
)

unexpected_pools <- setdiff(
  unique(pumping_data$pool),
  expected_pools
)

unexpected_permit_types <- setdiff(
  unique(pumping_data$permit_type),
  expected_permit_types
)

if (length(unexpected_pools) > 0) {
  stop(
    "Unexpected pool values: ",
    paste(unexpected_pools, collapse = ", ")
  )
}

if (length(unexpected_permit_types) > 0) {
  stop(
    "Unexpected permit_type values: ",
    paste(unexpected_permit_types, collapse = ", ")
  )
}

# -----------------------------------------------------------------------------
# Shared plot settings
# -----------------------------------------------------------------------------

series_levels <- c(
  "permitted_amt",
  "after_cpm",
  "pumped"
)

series_colors <- c(
  "permitted_amt" = EAHCP_COLORS[["aquifer"]],
  "after_cpm"     = EAHCP_COLORS[["alert"]],
  "pumped"        = EAHCP_COLORS[["spring"]]
)

series_linetypes <- c(
  "permitted_amt" = "longdash",
  "after_cpm"     = "22",
  "pumped"        = "solid"
)

# Clean x-axis labels:
# every 2 years through 2022, then 2025 as the final year
year_breaks <- c(
  seq(2008, 2022, by = 2),
  2025
)

# =============================================================================
# GRAPH 1
# Total permitted, CPM-adjusted permitted, and actual pumping
# =============================================================================

total_legend_labels <- c(
  "permitted_amt" = "Permitted Pumping",
  "after_cpm"     = "Permitted Pumping after CPM",
  "pumped"        = "Actual Pumped"
)

annual_total_data <- pumping_data %>%
  group_by(year) %>%
  summarise(
    permitted_amt = sum(
      permitted_amt,
      na.rm = TRUE
    ),
    after_cpm = sum(
      after_cpm,
      na.rm = TRUE
    ),
    pumped = sum(
      pumped,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = all_of(series_levels),
    names_to = "series",
    values_to = "acre_feet"
  ) %>%
  mutate(
    series = factor(
      series,
      levels = series_levels
    )
  )

graph_1 <- ggplot(
  annual_total_data,
  aes(
    x = year,
    y = acre_feet,
    color = series,
    linetype = series,
    group = series
  )
) +
  geom_line(
    linewidth = 1.35
  ) +
  geom_point(
    data = annual_total_data %>%
      filter(series == "pumped"),
    size = 2.4,
    stroke = 0,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = series_colors,
    breaks = series_levels,
    labels = total_legend_labels,
    name = NULL
  ) +
  scale_linetype_manual(
    values = series_linetypes,
    breaks = series_levels,
    labels = total_legend_labels,
    name = NULL
  ) +
  scale_x_continuous(
    breaks = year_breaks,
    limits = c(2008, 2025)
  ) +
  scale_y_continuous(
    labels = label_comma(
      accuracy = 1
    ),
    expand = expansion(
      mult = c(0.02, 0.08)
    )
  ) +
  labs(
    x = "Year",
    y = "Acre-feet"
  ) +
  guides(
    color = guide_legend(
      nrow = 1,
      byrow = TRUE,
      override.aes = list(
        linewidth = 1.5
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
    legend.direction = "horizontal",
    legend.text = element_text(
      size = 11.5,
      color = EAHCP_COLORS[["ink"]]
    ),
    legend.key.width = unit(
      1.05,
      "cm"
    ),
    legend.spacing.x = unit(
      0.30,
      "cm"
    ),
    
    axis.text.x = element_text(
      face = "bold"
    ),
    
    plot.margin = margin(
      t = 8,
      r = 18,
      b = 12,
      l = 12
    )
  )

graph_1_output <- file.path(
  output_dir,
  "permitted_after_cpm_actual_pumped_total.png"
)

ggsave(
  filename = graph_1_output,
  plot = graph_1,
  device = ragg::agg_png,
  width = 11.5,
  height = 6.2,
  units = "in",
  dpi = 320,
  bg = EAHCP_COLORS[["paper"]]
)

# =============================================================================
# GRAPH 2
# Two permit types x two pools
# =============================================================================

pool_type_data <- pumping_data %>%
  group_by(
    year,
    pool,
    permit_type
  ) %>%
  summarise(
    permitted_amt = sum(
      permitted_amt,
      na.rm = TRUE
    ),
    after_cpm = sum(
      after_cpm,
      na.rm = TRUE
    ),
    pumped = sum(
      pumped,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = all_of(series_levels),
    names_to = "series",
    values_to = "acre_feet"
  ) %>%
  mutate(
    series = factor(
      series,
      levels = series_levels
    ),
    pool_label = factor(
      pool,
      levels = c(
        "sanantonio",
        "uvalde"
      ),
      labels = c(
        "San Antonio Pool",
        "Uvalde Pool"
      )
    )
  )

make_pool_type_row <- function(
    data,
    selected_permit_type,
    row_title,
    legend_labels
) {
  
  row_data <- data %>%
    filter(
      permit_type == selected_permit_type
    )
  
  actual_pumped_data <- row_data %>%
    filter(
      series == "pumped"
    )
  
  ggplot(
    row_data,
    aes(
      x = year,
      y = acre_feet,
      color = series,
      linetype = series,
      group = series
    )
  ) +
    geom_line(
      linewidth = 1.15
    ) +
    geom_point(
      data = actual_pumped_data,
      size = 2.0,
      stroke = 0,
      show.legend = FALSE
    ) +
    facet_wrap(
      vars(pool_label),
      nrow = 1,
      scales = "free_y"
    ) +
    scale_color_manual(
      values = series_colors,
      breaks = series_levels,
      labels = legend_labels,
      name = NULL
    ) +
    scale_linetype_manual(
      values = series_linetypes,
      breaks = series_levels,
      labels = legend_labels,
      name = NULL
    ) +
    scale_x_continuous(
      breaks = year_breaks,
      limits = c(2008, 2025)
    ) +
    scale_y_continuous(
      labels = label_comma(
        accuracy = 1
      ),
      expand = expansion(
        mult = c(0.03, 0.10)
      )
    ) +
    labs(
      title = row_title,
      x = "Year",
      y = "Acre-feet"
    ) +
    guides(
      color = guide_legend(
        nrow = 1,
        byrow = TRUE,
        override.aes = list(
          linewidth = 1.4
        )
      )
    ) +
    theme_eahcp(
      base_size = 14
    ) +
    theme(
      plot.title = element_text(
        family = "Aptos Display",
        face = "bold",
        size = 16,
        color = EAHCP_COLORS[["aquifer"]],
        margin = margin(
          b = 5
        )
      ),
      
      strip.text = element_text(
        face = "bold",
        size = 12.5,
        color = EAHCP_COLORS[["aquifer"]]
      ),
      
      axis.text = element_text(
        size = 10.5
      ),
      axis.text.x = element_text(
        face = "bold"
      ),
      axis.title = element_text(
        size = 11.5
      ),
      
      legend.position = "bottom",
      legend.justification = "center",
      legend.direction = "horizontal",
      legend.text = element_text(
        size = 10.5,
        color = EAHCP_COLORS[["ink"]]
      ),
      legend.key.width = unit(
        0.90,
        "cm"
      ),
      legend.spacing.x = unit(
        0.20,
        "cm"
      ),
      
      panel.spacing = unit(
        0.55,
        "cm"
      ),
      
      plot.margin = margin(
        t = 5,
        r = 12,
        b = 4,
        l = 8
      )
    )
}

mandi_legend_labels <- c(
  "permitted_amt" = "Permitted Municipal and Industrial",
  "after_cpm"     = "Permitted Municipal and Industrial after CPM",
  "pumped"        = "Actual Pumped"
)

irrigation_legend_labels <- c(
  "permitted_amt" = "Permitted Irrigation",
  "after_cpm"     = "Permitted Irrigation after CPM",
  "pumped"        = "Actual Pumped"
)

mandi_plot <- make_pool_type_row(
  data = pool_type_data,
  selected_permit_type = "mandi",
  row_title = "Municipal and Industrial",
  legend_labels = mandi_legend_labels
)

irrigation_plot <- make_pool_type_row(
  data = pool_type_data,
  selected_permit_type = "irrigation",
  row_title = "Irrigation",
  legend_labels = irrigation_legend_labels
)

graph_2 <- (
  mandi_plot /
    irrigation_plot
) +
  plot_layout(
    heights = c(
      1,
      1
    )
  ) +
  plot_annotation(
    caption = "Y-axis scales vary by panel.",
    theme = theme(
      plot.background = element_rect(
        fill = EAHCP_COLORS[["paper"]],
        color = NA
      ),
      plot.caption = element_text(
        family = "Aptos",
        size = 9.5,
        color = EAHCP_COLORS[["muted"]],
        hjust = 0,
        margin = margin(
          t = 4,
          r = 0,
          b = 0,
          l = 10
        )
      )
    )
  )

graph_2_output <- file.path(
  output_dir,
  "permitted_after_cpm_actual_pumped_by_pool_type.png"
)

ggsave(
  filename = graph_2_output,
  plot = graph_2,
  device = ragg::agg_png,
  width = 13.5,
  height = 8.6,
  units = "in",
  dpi = 320,
  bg = EAHCP_COLORS[["paper"]]
)

# -----------------------------------------------------------------------------
# Print output locations
# -----------------------------------------------------------------------------

message(
  "Graph 1 written to: ",
  normalizePath(
    graph_1_output,
    winslash = "\\",
    mustWork = FALSE
  )
)

message(
  "Graph 2 written to: ",
  normalizePath(
    graph_2_output,
    winslash = "\\",
    mustWork = FALSE
  )
)