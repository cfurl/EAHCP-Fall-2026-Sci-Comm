library(tidyverse)
library(scales)

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
  "permitted_groundwater_by_county_and_use.png"
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
# Plot configuration
# -------------------------------------------------------------------------

county_levels <- c(
  "Uvalde",
  "Medina",
  "Bexar",
  "Comal",
  "Hays"
)

use_levels <- c(
  "irrigation",
  "municipal",
  "industrial"
)

use_labels <- c(
  "irrigation" = "Irrigation",
  "municipal"  = "Municipal",
  "industrial" = "Industrial"
)

use_colors <- c(
  "Irrigation" = EAHCP_COLORS[["cypress"]],
  "Municipal"  = EAHCP_COLORS[["aquifer"]],
  "Industrial" = EAHCP_COLORS[["spring"]]
)

# -------------------------------------------------------------------------
# Summarize authorized use
#
# complete() ensures that every county has a row for each of the three
# use types. Missing county/use combinations are represented as zero.
# -------------------------------------------------------------------------

permit_plot_df <- permit_list %>%
  mutate(
    County = str_squish(County),
    Use = str_to_lower(str_squish(Use))
  ) %>%
  filter(
    County %in% county_levels,
    Use %in% use_levels
  ) %>%
  group_by(
    County,
    Use
  ) %>%
  summarise(
    Auth_Use = sum(Auth_Use, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  complete(
    County = county_levels,
    Use = use_levels,
    fill = list(Auth_Use = 0)
  ) %>%
  mutate(
    County = factor(
      County,
      levels = county_levels
    ),
    Use = factor(
      Use,
      levels = use_levels,
      labels = unname(use_labels[use_levels])
    )
  ) %>%
  arrange(
    County,
    Use
  )

# -------------------------------------------------------------------------
# Create grouped bar chart
# -------------------------------------------------------------------------

permit_plot <- ggplot(
  permit_plot_df,
  aes(
    x = County,
    y = Auth_Use,
    fill = Use
  )
) +
  geom_col(
    position = position_dodge(width = 0.80),
    width = 0.68,
    color = NA
  ) +
  geom_text(
    aes(
      label = comma(
        round(Auth_Use, 0),
        accuracy = 1
      )
    ),
    position = position_dodge(width = 0.80),
    vjust = -0.35,
    size = 4.1,
    family = "Aptos",
    color = EAHCP_COLORS[["ink"]]
  ) +
  scale_fill_manual(
    values = use_colors,
    breaks = c(
      "Irrigation",
      "Municipal",
      "Industrial"
    ),
    name = NULL,
    drop = FALSE
  ) +
  scale_y_continuous(
    labels = label_comma(
      accuracy = 1
    ),
    expand = expansion(
      mult = c(0, 0.12)
    )
  ) +
  labs(
    x = NULL,
    y = "Permitted Groundwater (acre-feet)"
  ) +
  theme_eahcp(
    base_size = 18
  ) +
  theme(
    legend.position = "top",
    legend.justification = "center",
    legend.box.just = "center",
    
    axis.text.x = element_text(
      face = "bold",
      color = EAHCP_COLORS[["ink"]]
    ),
    
    axis.title.y = element_text(
      color = EAHCP_COLORS[["ink"]]
    ),
    
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    plot.caption = element_blank(),
    
    plot.margin = margin(
      t = 8,
      r = 22,
      b = 12,
      l = 16
    )
  ) +
  coord_cartesian(
    clip = "off"
  )

# Display plot
print(permit_plot)

# -------------------------------------------------------------------------
# Save slide-ready PNG
# -------------------------------------------------------------------------

ggsave(
  filename = output_file,
  plot = permit_plot,
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