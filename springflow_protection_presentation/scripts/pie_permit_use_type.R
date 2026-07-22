library(tidyverse)
library(scales)
library(grid)

# -------------------------------------------------------------------------
# EAHCP Springflow Protection presentation theme
# -------------------------------------------------------------------------

source(
  "./springflow_protection_presentation/scripts/eahcp_plot_theme.R"
)

# -------------------------------------------------------------------------
# File paths
# -------------------------------------------------------------------------

input_file <- paste0(
  "./springflow_protection_presentation/",
  "data/permit_list_website.csv"
)

output_dir <- paste0(
  "./springflow_protection_presentation/",
  "output"
)

output_file <- file.path(
  output_dir,
  "edwards_aquifer_permits_by_use_type.png"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# -------------------------------------------------------------------------
# Read permit data
# -------------------------------------------------------------------------

permit_list <- read_csv(
  input_file,
  col_types = cols(
    Permit_No   = col_character(),
    Entity_Name = col_character(),
    Entity_No   = col_character(),
    County      = col_character(),
    Use         = col_character(),
    Base        = col_double(),
    Unrestrict  = col_double(),
    Auth_Use    = col_double()
  )
)

# -------------------------------------------------------------------------
# Summarize volumes by use type
# -------------------------------------------------------------------------

permit_clean <- permit_list %>%
  mutate(
    Use = str_to_lower(str_squish(Use))
  )

permit_pie_df <- bind_rows(
  tibble(
    Use_Type = "Municipal",
    Volume = permit_clean %>%
      filter(Use == "municipal") %>%
      summarise(value = sum(Unrestrict, na.rm = TRUE)) %>%
      pull(value)
  ),
  tibble(
    Use_Type = "Industrial",
    Volume = permit_clean %>%
      filter(Use == "industrial") %>%
      summarise(value = sum(Unrestrict, na.rm = TRUE)) %>%
      pull(value)
  ),
  tibble(
    Use_Type = "Irrigation - Base",
    Volume = permit_clean %>%
      filter(Use == "irrigation") %>%
      summarise(value = sum(Base, na.rm = TRUE)) %>%
      pull(value)
  ),
  tibble(
    Use_Type = "Irrigation - Unrestricted",
    Volume = permit_clean %>%
      filter(Use == "irrigation") %>%
      summarise(value = sum(Unrestrict, na.rm = TRUE)) %>%
      pull(value)
  )
) %>%
  mutate(
    Use_Type = factor(
      Use_Type,
      levels = c(
        "Municipal",
        "Industrial",
        "Irrigation - Base",
        "Irrigation - Unrestricted"
      )
    )
  ) %>%
  arrange(Use_Type) %>%
  mutate(
    Percent = Volume / sum(Volume),
    legend_label = paste0(
      Use_Type,
      " \u2014 ",
      comma(round(Volume, 0)),
      " ac-ft (",
      percent(Percent, accuracy = 0.1),
      ")"
    )
  )

# -------------------------------------------------------------------------
# Color palette
# -------------------------------------------------------------------------

pie_colors <- c(
  "Municipal" = EAHCP_COLORS[["aquifer"]],
  "Industrial" = EAHCP_COLORS[["spring"]],
  "Irrigation - Base" = EAHCP_COLORS[["cypress"]],
  "Irrigation - Unrestricted" = "#8EA892"
)

legend_labels <- permit_pie_df$legend_label
names(legend_labels) <- permit_pie_df$Use_Type

# -------------------------------------------------------------------------
# Build donut chart
# -------------------------------------------------------------------------

permit_pie_plot <- ggplot(
  permit_pie_df,
  aes(
    x = 2,
    y = Volume,
    fill = Use_Type
  )
) +
  geom_col(
    width = 0.95,
    color = EAHCP_COLORS[["paper"]],
    linewidth = 1.2
  ) +
  coord_polar(
    theta = "y",
    start = pi / 2,
    direction = -1
  ) +
  xlim(0.6, 2.9) +
  scale_fill_manual(
    values = pie_colors,
    breaks = levels(permit_pie_df$Use_Type),
    labels = legend_labels,
    name = NULL
  ) +
  guides(
    fill = guide_legend(
      byrow = TRUE,
      label.position = "right",
      keyheight = unit(0.62, "cm"),
      keywidth = unit(0.58, "cm")
    )
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_eahcp(base_size = 18) +
  theme(
    axis.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    panel.grid = element_blank(),
    
    legend.position = "right",
    legend.justification = "center",
    legend.title = element_blank(),
    legend.text = element_text(
      size = rel(0.98),
      color = EAHCP_COLORS[["ink"]],
      lineheight = 1.22
    ),
    legend.key.height = unit(0.62, "cm"),
    legend.key.width = unit(0.58, "cm"),
    legend.spacing.y = unit(0.22, "cm"),
    legend.margin = margin(6, 6, 6, 6),
    
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    plot.caption = element_blank(),
    
    plot.margin = margin(
      t = 12,
      r = 18,
      b = 12,
      l = 18
    )
  )

print(permit_pie_plot)

# -------------------------------------------------------------------------
# Save slide-ready PNG
# -------------------------------------------------------------------------

ggsave(
  filename = output_file,
  plot = permit_pie_plot,
  width = 11,
  height = 5.8,
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