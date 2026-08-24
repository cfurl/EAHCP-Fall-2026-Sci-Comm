# ============================================================
# Comal Springs daily mean springflow
# 2025-01-01 through most recent available date
# ============================================================

library(tidyverse)
library(scales)
library(lubridate)

# ------------------------------------------------------------
# Paths
# ------------------------------------------------------------

input_file <- paste0(
  "C:/Users/cfurl/OneDrive - Edwards Aquifer Authority/",
  "SC_meetings/sep_2026/fall_meeting/",
  "EAHCP-Fall-2026-Sci-Comm/",
  "springflow_protection_presentation/data/",
  "HistoricDailyMeanSpringflow.csv"
)

output_file <- paste0(
  "C:/Users/cfurl/OneDrive - Edwards Aquifer Authority/",
  "SC_meetings/sep_2026/fall_meeting/",
  "EAHCP-Fall-2026-Sci-Comm/",
  "Comal_mgt_presentation/images/",
  "comal_springs_daily_mean_2025_recent.png"
)

dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

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
# Read and prepare data
# ------------------------------------------------------------

flow_raw <- read_csv(
  input_file,
  show_col_types = FALSE
)

comal_flow <- flow_raw |>
  transmute(
    date = as.Date(Day),
    comal_cfs = `Comal Springs (cfs)`
  ) |>
  filter(
    date >= as.Date("2025-01-01"),
    !is.na(comal_cfs)
  ) |>
  arrange(date)

if (nrow(comal_flow) == 0) {
  stop("No Comal Springs records found on or after 2025-01-01.")
}

latest_date <- max(comal_flow$date, na.rm = TRUE)

# ------------------------------------------------------------
# Y-axis range with a little padding
# ------------------------------------------------------------

y_min_data <- min(comal_flow$comal_cfs, na.rm = TRUE)
y_max_data <- max(comal_flow$comal_cfs, na.rm = TRUE)
y_range    <- y_max_data - y_min_data

if (y_range == 0) {
  y_range <- max(1, y_max_data * 0.1)
}

y_pad <- y_range * 0.08

y_limits <- c(
  y_min_data - y_pad,
  y_max_data + y_pad
)

# ------------------------------------------------------------
# Plot
# ------------------------------------------------------------

p <- ggplot(
  comal_flow,
  aes(x = date, y = comal_cfs)
) +
  geom_line(
    linewidth = 1.2,
    color = lake
  ) +
  scale_x_date(
    date_breaks = "2 months",
    date_labels = "%b\n%Y",
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    labels = label_comma(),
    limits = y_limits,
    breaks = pretty_breaks(n = 6),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    x = NULL,
    y = "Springflow (cfs)",
    subtitle = paste0(
      "Comal Springs daily mean springflow, ",
      format(min(comal_flow$date), "%b %Y"),
      "–",
      format(latest_date, "%b %Y")
    )
  ) +
  theme_minimal(base_family = "Aptos", base_size = 16) +
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
      margin = margin(r = 12)
    ),
    axis.text.x = element_text(
      size = 14,
      color = muted
    ),
    axis.text.y = element_text(
      size = 14,
      color = muted
    ),
    plot.subtitle = element_text(
      size = 15,
      color = muted,
      margin = margin(b = 10)
    ),
    plot.margin = margin(
      t = 10,
      r = 15,
      b = 10,
      l = 10
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
message("Latest date plotted: ", latest_date)