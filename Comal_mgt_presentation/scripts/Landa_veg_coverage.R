# ============================================================
# Landa Lake SAV stacked bar chart
#
# Stack bottom -> top:
# Vallisneria
# Sagittaria
# Ludwigia
# Other
# Potamogeton
# Cabomba
# Hygrophila
# Bryophyte
#
# Algae excluded
# ============================================================

library(tidyverse)
library(scales)
library(grid)

# ------------------------------------------------------------
# Paths
# ------------------------------------------------------------

input_file <- paste0(
  "C:/Users/cfurl/OneDrive - Edwards Aquifer Authority/",
  "SC_meetings/sep_2026/fall_meeting/",
  "EAHCP-Fall-2026-Sci-Comm/",
  "Comal_mgt_presentation/data/veg_community_comp_full_cf.txt"
)

output_file <- paste0(
  "C:/Users/cfurl/OneDrive - Edwards Aquifer Authority/",
  "SC_meetings/sep_2026/fall_meeting/",
  "EAHCP-Fall-2026-Sci-Comm/",
  "Comal_mgt_presentation/images/landa_lake_sav_stacked.png"
)

dir.create(
  dirname(output_file),
  recursive = TRUE,
  showWarnings = FALSE
)

# ------------------------------------------------------------
# Landa palette
# ------------------------------------------------------------

lake_deep      <- "#123240"
lake           <- "#1E6F73"
shallow        <- "#6CB7AF"
aquatic        <- "#4F7D4F"
aquatic_light  <- "#8BAE66"
cypress        <- "#3B5B45"
limestone      <- "#D8CFBA"
limestone_dark <- "#A99B80"
sand           <- "#F6F2E8"
paper          <- "#FBFAF4"
ink            <- "#243036"
muted          <- "#65737A"
gold           <- "#D6A13A"
coral          <- "#B86156"

# ------------------------------------------------------------
# Vegetation colors
# ------------------------------------------------------------

veg_colors <- c(
  "Bryophyte"   = "#0D19F2",
  "Cabomba"     = "#9C72E8",
  "Hygrophila"  = "#FF1A1A",
  "Ludwigia"    = "#F2A100",
  "Other"       = "#C96B00",
  "Potamogeton" = "#E3D680",
  "Sagittaria"  = "#F3EF00",
  "Vallisneria" = "#0C6B0C"
)

# ------------------------------------------------------------
# Desired physical stack order
#
# FIRST ITEM = BOTTOM OF BAR
# ------------------------------------------------------------

veg_stack_order <- c(
  "Vallisneria",
  "Sagittaria",
  "Ludwigia",
  "Other",
  "Potamogeton",
  "Cabomba",
  "Hygrophila",
  "Bryophyte"
)

# Keep legend organized independently of physical stack
veg_legend_order <- c(
  "Bryophyte",
  "Cabomba",
  "Hygrophila",
  "Ludwigia",
  "Other",
  "Potamogeton",
  "Sagittaria",
  "Vallisneria"
)

# ------------------------------------------------------------
# Read data
# ------------------------------------------------------------

sav_raw <- readr::read_tsv(
  input_file,
  show_col_types = FALSE,
  trim_ws = TRUE
)

# ------------------------------------------------------------
# Filter to Landa Lake
# Remove algae entirely
# ------------------------------------------------------------

sav_ll <- sav_raw |>
  mutate(
    River    = str_trim(River),
    Reach    = str_trim(Reach),
    Date     = str_trim(Date),
    Group    = str_trim(Group),
    Veg      = str_trim(Veg),
    Coverage = as.numeric(Coverage)
  ) |>
  filter(
    River == "CR",
    Reach == "LL",
    Veg != "Algae"
  )

# ------------------------------------------------------------
# Recent survey dates
# ------------------------------------------------------------

recent_dates <- c(
  "04-2022",
  "10-2022",
  "04-2023",
  "10-2023",
  "04-2024",
  "10-2024",
  "04-2025",
  "10-2025"
)

# ------------------------------------------------------------
# Recent observed bars
# ------------------------------------------------------------

recent_bars <- sav_ll |>
  filter(Date %in% recent_dates) |>
  select(
    bar_label = Date,
    Veg,
    Coverage
  )

# ------------------------------------------------------------
# 2013-2025 average
# ------------------------------------------------------------

avg_bar <- sav_ll |>
  filter(Date != "LTBG") |>
  group_by(Veg) |>
  summarise(
    Coverage = mean(Coverage, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    bar_label = "Avg 2013-2025"
  )

# ------------------------------------------------------------
# Long-term biological goal
# ------------------------------------------------------------

ltbg_bar <- sav_ll |>
  filter(Date == "LTBG") |>
  select(
    Veg,
    Coverage
  ) |>
  mutate(
    bar_label = "LTBG"
  )

# ------------------------------------------------------------
# X-axis order
# ------------------------------------------------------------

bar_order <- c(
  "04-2022",
  "10-2022",
  "04-2023",
  "10-2023",
  "04-2024",
  "10-2024",
  "04-2025",
  "10-2025",
  "Avg 2013-2025",
  "LTBG"
)

# ------------------------------------------------------------
# Combine
# ------------------------------------------------------------

plot_df <- bind_rows(
  recent_bars,
  avg_bar,
  ltbg_bar
) |>
  select(
    bar_label,
    Veg,
    Coverage
  ) |>
  mutate(
    bar_label = factor(
      bar_label,
      levels = bar_order
    ),
    Veg = factor(
      Veg,
      levels = veg_stack_order
    )
  ) |>
  complete(
    bar_label,
    Veg,
    fill = list(Coverage = 0)
  )

# ------------------------------------------------------------
# Y-axis limit
# ------------------------------------------------------------

bar_totals <- plot_df |>
  group_by(bar_label) |>
  summarise(
    total_cov = sum(Coverage, na.rm = TRUE),
    .groups = "drop"
  )

y_max <- max(bar_totals$total_cov, na.rm = TRUE)
y_lim <- y_max * 1.08

# ------------------------------------------------------------
# Plot
# ------------------------------------------------------------

p <- ggplot(
  plot_df,
  aes(
    x = bar_label,
    y = Coverage,
    fill = Veg
  )
) +
  
  geom_col(
    width = 0.78,
    
    # THIS IS THE IMPORTANT FIX:
    # force first factor level to bottom of stack
    position = position_stack(reverse = TRUE)
  ) +
  
  scale_fill_manual(
    values = veg_colors,
    breaks = veg_legend_order,
    drop = FALSE
  ) +
  
  scale_y_continuous(
    labels = label_comma(),
    expand = expansion(
      mult = c(0, 0.02)
    ),
    limits = c(0, y_lim)
  ) +
  
  labs(
    x = NULL,
    y = expression(
      paste("Areal Coverage (m"^2, ")")
    ),
    subtitle = "Landa Lake submerged aquatic vegetation"
  ) +
  
  guides(
    fill = guide_legend(
      title = NULL,
      nrow = 2,
      byrow = TRUE,
      keyheight = unit(0.9, "lines"),
      keywidth = unit(1.0, "lines")
    )
  ) +
  
  theme_minimal(
    base_family = "Aptos",
    base_size = 16
  ) +
  
  theme(
    
    plot.background = element_rect(
      fill = sand,
      color = NA
    ),
    
    panel.background = element_rect(
      fill = sand,
      color = NA
    ),
    
    panel.grid.minor = element_blank(),
    
    panel.grid.major.x = element_blank(),
    
    panel.grid.major.y = element_line(
      color = limestone,
      linewidth = 0.45
    ),
    
    axis.title.y = element_text(
      size = 17,
      face = "bold",
      color = ink,
      margin = margin(r = 10)
    ),
    
    axis.text.x = element_text(
      size = 12,
      color = ink,
      angle = 45,
      hjust = 1,
      vjust = 1
    ),
    
    axis.text.y = element_text(
      size = 13,
      color = ink
    ),
    
    plot.subtitle = element_text(
      size = 14,
      color = muted,
      margin = margin(b = 12)
    ),
    
    legend.position = "bottom",
    
    legend.text = element_text(
      size = 12,
      color = ink
    ),
    
    legend.margin = margin(t = 8),
    
    plot.margin = margin(
      t = 12,
      r = 18,
      b = 8,
      l = 8
    )
  )

# ------------------------------------------------------------
# Save
# ------------------------------------------------------------

ggsave(
  filename = output_file,
  plot = p,
  width = 8.64,
  height = 5.19,
  units = "in",
  dpi = 300,
  bg = sand
)

message("Saved plot to: ", output_file)