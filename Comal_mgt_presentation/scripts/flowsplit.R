# ============================================================
# Landa Lake Discharge
# Percent of Total Comal flow by measurement station
# ============================================================

library(tidyverse)
library(scales)

# ------------------------------------------------------------
# Paths
# ------------------------------------------------------------

input_file <- paste0(
  "C:/Users/cfurl/OneDrive - Edwards Aquifer Authority/",
  "SC_meetings/sep_2026/fall_meeting/",
  "EAHCP-Fall-2026-Sci-Comm/",
  "Comal_mgt_presentation/data/flow_split.csv"
)

output_file <- paste0(
  "C:/Users/cfurl/OneDrive - Edwards Aquifer Authority/",
  "SC_meetings/sep_2026/fall_meeting/",
  "EAHCP-Fall-2026-Sci-Comm/",
  "Comal_mgt_presentation/images/landa_lake_discharge.png"
)

# ------------------------------------------------------------
# Landa Lake palette
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
# Read and prepare data
# ------------------------------------------------------------

flow_raw <- read_csv(
  input_file,
  show_col_types = FALSE,
  trim_ws = TRUE
) |>
  mutate(
    date = as.Date(date),
    site = str_trim(site)
  )


# ------------------------------------------------------------
# Get Total Comal discharge for each date
# ------------------------------------------------------------

total_comal <- flow_raw |>
  filter(site == "Total Comal") |>
  select(
    date,
    total_comal_cfs = cfs
  )


# ------------------------------------------------------------
# Calculate percent of Total Comal flow
# ------------------------------------------------------------

flow_plot <- flow_raw |>
  filter(site != "Total Comal") |>
  left_join(total_comal, by = "date") |>
  mutate(
    percent_flow = cfs / total_comal_cfs,
    
    # Facet label includes date and total Comal discharge
    panel_label = paste0(
      format(date, "%B %d, %Y"),
      "\nTotal Comal = ",
      format(
        total_comal_cfs,
        trim = TRUE,
        scientific = FALSE
      ),
      " cfs"
    )
  )


# ------------------------------------------------------------
# Station order
#
# Ordered generally from upper spring runs through Landa Lake
# and downstream channel measurements.
# ------------------------------------------------------------

site_order <- c(
  "Upper Spring Run",
  "Spring Run 1",
  "Spring Run 2",
  "Spring Run 3",
  "Landa Lake Cable",
  "SI Upper Far",
  "SI Lower Near",
  "SI Lower Far"
)

flow_plot <- flow_plot |>
  mutate(
    site = factor(site, levels = site_order)
  )


# ------------------------------------------------------------
# Determine a clean common y-axis maximum
# ------------------------------------------------------------

max_percent <- max(flow_plot$percent_flow, na.rm = TRUE)

y_max <- ceiling(max_percent * 10) / 10

# Give the labels some breathing room
y_limit <- max(y_max + 0.05, 0.50)


# ------------------------------------------------------------
# Plot
# ------------------------------------------------------------

p <- ggplot(
  flow_plot,
  aes(
    x = site,
    y = percent_flow
  )
) +
  
  geom_col(
    width = 0.72,
    fill = lake
  ) +
  
  geom_text(
    aes(
      label = percent(
        percent_flow,
        accuracy = 1
      )
    ),
    vjust = -0.45,
    size = 5.0,
    family = "Aptos",
    fontface = "bold",
    color = ink
  ) +
  
  facet_wrap(
    ~ panel_label,
    ncol = 2
  ) +
  
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    breaks = pretty_breaks(n = 5),
    limits = c(0, y_limit),
    expand = expansion(mult = c(0, 0))
  ) +
  
  labs(
    x = NULL,
    y = "Percent of Total Comal discharge",
    subtitle = "Distribution of measured discharge among Landa Lake and spring-run stations"
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
    
    strip.background = element_rect(
      fill = limestone,
      color = NA
    ),
    
    strip.text = element_text(
      color = lake_deep,
      size = 15,
      face = "bold",
      margin = margin(
        t = 8,
        r = 8,
        b = 8,
        l = 8
      )
    ),
    
    plot.subtitle = element_text(
      color = muted,
      size = 15,
      margin = margin(
        b = 16
      )
    ),
    
    axis.title.y = element_text(
      size = 17,
      face = "bold",
      color = ink,
      margin = margin(
        r = 12
      )
    ),
    
    axis.text.y = element_text(
      size = 14,
      color = ink
    ),
    
    axis.text.x = element_text(
      size = 12.5,
      color = ink,
      angle = 45,
      hjust = 1,
      vjust = 1
    ),
    
    panel.grid.major.x = element_blank(),
    
    panel.grid.minor = element_blank(),
    
    panel.grid.major.y = element_line(
      color = limestone_dark,
      linewidth = 0.35
    ),
    
    panel.spacing = unit(
      0.6,
      "lines"
    ),
    
    plot.margin = margin(
      t = 18,
      r = 20,
      b = 15,
      l = 15
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

message("Saved: ", output_file)