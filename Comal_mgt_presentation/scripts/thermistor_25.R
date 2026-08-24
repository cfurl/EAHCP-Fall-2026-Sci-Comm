# ============================================================
# Comal thermistor water temperature - 2025
#
# Two figures, each arranged 2 rows x 3 columns
# Other Place excluded
#
# Figure 1: Upper system / Landa Lake
# Figure 2: Spring runs / channels
# ============================================================

library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(patchwork)

# ------------------------------------------------------------
# Paths
# ------------------------------------------------------------

input_file <- paste0(
  "C:/Users/cfurl/OneDrive - Edwards Aquifer Authority/",
  "annual report/2025/LTBGs/LTBG_production_cut/data/BW_drop/",
  "BIO-WEST Delivery of 2025 Data/p_Thermistors/",
  "Comal_Thermistors_2025.xlsx"
)

output_folder <- paste0(
  "C:/Users/cfurl/OneDrive - Edwards Aquifer Authority/",
  "SC_meetings/sep_2026/fall_meeting/",
  "EAHCP-Fall-2026-Sci-Comm/",
  "Comal_mgt_presentation/images"
)

dir.create(
  output_folder,
  recursive = TRUE,
  showWarnings = FALSE
)

output_upper <- file.path(
  output_folder,
  "comal_thermistors_2025_upper_lake.png"
)

output_channels <- file.path(
  output_folder,
  "comal_thermistors_2025_runs_channels.png"
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
# Read data
# ------------------------------------------------------------

temp_raw <- read_excel(
  input_file,
  sheet = "Data"
)

# ------------------------------------------------------------
# Clean data
# ------------------------------------------------------------

temp <- temp_raw |>
  transmute(
    Station = str_trim(Station),
    Date_Time = as.POSIXct(
      Date_Time,
      origin = "1899-12-30",
      tz = "America/Chicago"
    ),
    Water_Temp = as.numeric(Water_Temp)
  ) |>
  filter(
    Station != "Other Place",
    !is.na(Date_Time),
    !is.na(Water_Temp)
  ) |>
  arrange(
    Station,
    Date_Time
  )

# ------------------------------------------------------------
# Station groups
# ------------------------------------------------------------

stations_upper <- c(
  "Blieders",
  "Heidelberg",
  "Booneville Near",
  "Booneville Far",
  "Landa Lake Upper",
  "Landa Lake Lower"
)

stations_channels <- c(
  "Spring Run 1",
  "Spring Run 2",
  "Spring Run 3",
  "New Channel Upstream",
  "New Channel Downstream",
  "Old Channel"
)

# ------------------------------------------------------------
# Common plotting limits
# ------------------------------------------------------------

date_min <- as.POSIXct(
  "2025-01-01 00:00:00",
  tz = "America/Chicago"
)

date_max <- max(
  temp$Date_Time,
  na.rm = TRUE
)

# Actual overall range is approximately 6.3 to 33.1 C,
# so use consistent presentation-friendly limits.
temp_limits <- c(5, 35)

# ------------------------------------------------------------
# Function to make an individual panel
# ------------------------------------------------------------

make_temp_panel <- function(data, station_name) {
  
  station_dat <- data |>
    filter(Station == station_name)
  
  ggplot(
    station_dat,
    aes(
      x = Date_Time,
      y = Water_Temp
    )
  ) +
    
    geom_line(
      color = lake,
      linewidth = 0.55,
      lineend = "round"
    ) +
    
    scale_x_datetime(
      limits = c(date_min, date_max),
      date_breaks = "2 months",
      date_labels = "%b",
      expand = expansion(mult = c(0.005, 0.005))
    ) +
    
    scale_y_continuous(
      limits = temp_limits,
      breaks = seq(5, 35, 5),
      expand = expansion(mult = c(0, 0))
    ) +
    
    labs(
      title = station_name,
      x = NULL,
      y = NULL
    ) +
    
    theme_minimal(
      base_family = "Aptos",
      base_size = 13
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
      
      plot.title = element_text(
        size = 14,
        face = "bold",
        color = lake_deep,
        hjust = 0,
        margin = margin(b = 6)
      ),
      
      axis.text.x = element_text(
        size = 10.5,
        color = muted
      ),
      
      axis.text.y = element_text(
        size = 10.5,
        color = muted
      ),
      
      panel.grid.major.x = element_blank(),
      
      panel.grid.minor = element_blank(),
      
      panel.grid.major.y = element_line(
        color = limestone,
        linewidth = 0.4
      ),
      
      plot.margin = margin(
        t = 5,
        r = 7,
        b = 5,
        l = 7
      )
    )
}

# ------------------------------------------------------------
# Figure 1 - Upper system / Landa Lake
# ------------------------------------------------------------

upper_plots <- map(
  stations_upper,
  ~ make_temp_panel(temp, .x)
)

fig_upper <- wrap_plots(
  upper_plots,
  ncol = 3,
  nrow = 2
) +
  
  plot_annotation(
    subtitle = "2025 water temperature — upper Comal system and Landa Lake",
    theme = theme(
      plot.background = element_rect(
        fill = sand,
        color = NA
      ),
      
      plot.subtitle = element_text(
        family = "Aptos",
        size = 15,
        color = muted,
        margin = margin(
          b = 10
        )
      )
    )
  ) &
  
  theme(
    plot.background = element_rect(
      fill = sand,
      color = NA
    )
  )

# Shared y-axis label
fig_upper <- fig_upper +
  plot_annotation(
    subtitle = "2025 water temperature — upper Comal system and Landa Lake"
  )

# ------------------------------------------------------------
# Figure 2 - Spring runs / channels
# ------------------------------------------------------------

channel_plots <- map(
  stations_channels,
  ~ make_temp_panel(temp, .x)
)

fig_channels <- wrap_plots(
  channel_plots,
  ncol = 3,
  nrow = 2
) +
  
  plot_annotation(
    subtitle = "2025 water temperature — spring runs and river channels",
    theme = theme(
      plot.background = element_rect(
        fill = sand,
        color = NA
      ),
      
      plot.subtitle = element_text(
        family = "Aptos",
        size = 15,
        color = muted,
        margin = margin(
          b = 10
        )
      )
    )
  ) &
  
  theme(
    plot.background = element_rect(
      fill = sand,
      color = NA
    )
  )

# ------------------------------------------------------------
# Add common y-axis label using patchwork
# ------------------------------------------------------------

y_label <- ggplot() +
  annotate(
    "text",
    x = 1,
    y = 1,
    label = "Water Temperature (°C)",
    angle = 90,
    family = "Aptos",
    fontface = "bold",
    size = 5.2,
    color = ink
  ) +
  xlim(0, 2) +
  ylim(0, 2) +
  theme_void() +
  theme(
    plot.background = element_rect(
      fill = sand,
      color = NA
    )
  )

fig_upper_final <-
  y_label + fig_upper +
  plot_layout(
    widths = c(0.06, 1)
  )

fig_channels_final <-
  y_label + fig_channels +
  plot_layout(
    widths = c(0.06, 1)
  )

# ------------------------------------------------------------
# Save
#
# Slightly larger than our normal side-by-side figure because
# six individual panels need some room on a PowerPoint slide.
# ------------------------------------------------------------

ggsave(
  filename = output_upper,
  plot = fig_upper_final,
  width = 10.6,
  height = 6.0,
  units = "in",
  dpi = 300,
  bg = sand
)

ggsave(
  filename = output_channels,
  plot = fig_channels_final,
  width = 10.6,
  height = 6.0,
  units = "in",
  dpi = 300,
  bg = sand
)

message("Saved: ", output_upper)
message("Saved: ", output_channels)
message("Latest date in dataset: ", date_max)