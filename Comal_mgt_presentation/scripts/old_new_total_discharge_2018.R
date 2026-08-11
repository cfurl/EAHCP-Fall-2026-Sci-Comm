# Old channel gage = 08169000
# new channel gage = 08168932
# main gage rv = 08168913

# ============================================================
# COMAL RIVER DISCHARGE — 2018
#
# USGS daily mean discharge:
#   Old Channel   = 08169000
#   New Channel   = 08168932
#   Total Comal   = 08168913
#
# Parameter:
#   00060 = Discharge, cubic feet per second
#
# Statistic:
#   00003 = Daily mean
#
# IMPORTANT:
#   Each gage is downloaded separately because dataRetrieval
#   may return different discharge column names such as:
#
#       Flow
#       ..2.._Flow
#
#   This script detects the discharge column automatically
#   and standardizes it to "Flow".
#
# Output:
#   comal_river_discharge_2018.png
#
# Output size:
#   8.64 in wide x 5.19 in tall
# ============================================================

library(tidyverse)
library(lubridate)
library(scales)
library(grid)
library(dataRetrieval)


# ------------------------------------------------------------
# PATH
# ------------------------------------------------------------

output_dir <- paste0(
  "C:/Users/cfurl/OneDrive - Edwards Aquifer Authority/",
  "SC_meetings/sep_2026/fall_meeting/",
  "EAHCP-Fall-2026-Sci-Comm/",
  "Comal_mgt_presentation/images"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

output_file <- file.path(
  output_dir,
  "comal_river_discharge_2018.png"
)


# ------------------------------------------------------------
# LANDA COLORS
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


# ------------------------------------------------------------
# GAGES
# ------------------------------------------------------------

gage_lookup <- tibble(
  site_no = c(
    "08168913",
    "08168932",
    "08169000"
  ),
  
  gage = c(
    "Total Comal River",
    "New Channel",
    "Old Channel"
  )
)


# ------------------------------------------------------------
# DATE RANGE
# ------------------------------------------------------------

start_date <- "2018-01-01"
end_date   <- "2018-12-31"


# ------------------------------------------------------------
# FUNCTION TO DOWNLOAD ONE GAGE
#
# Downloads one site at a time and automatically identifies
# whatever column dataRetrieval has labeled as the Flow field.
# ------------------------------------------------------------

get_discharge <- function(
    site_number,
    gage_name
) {
  
  # Download daily mean discharge
  x <- readNWISdv(
    siteNumbers = site_number,
    parameterCd = "00060",
    startDate = start_date,
    endDate = end_date,
    statCd = "00003"
  )
  
  # Rename NWIS columns where possible
  x <- renameNWISColumns(x)
  
  # ----------------------------------------------------------
  # Identify the discharge value column
  #
  # Examples we may receive:
  #
  #   Flow
  #   ..2.._Flow
  #
  # But exclude qualifier fields such as:
  #
  #   Flow_cd
  #   ..2.._Flow_cd
  # ----------------------------------------------------------
  
  flow_columns <- names(x)[
    str_detect(
      names(x),
      "Flow$"
    )
  ]
  
  # Safety check
  if (length(flow_columns) == 0) {
    
    stop(
      paste0(
        "Could not identify a discharge column for site ",
        site_number,
        ". Columns returned were: ",
        paste(names(x), collapse = ", ")
      )
    )
  }
  
  if (length(flow_columns) > 1) {
    
    warning(
      paste0(
        "More than one Flow column found for site ",
        site_number,
        ". Using: ",
        flow_columns[1]
      )
    )
  }
  
  flow_column <- flow_columns[1]
  
  # Standardize output
  x |>
    transmute(
      site_no = site_no,
      gage = gage_name,
      Date = as.Date(Date),
      Flow = as.numeric(
        .data[[flow_column]]
      )
    ) |>
    filter(
      !is.na(Flow)
    )
}


# ------------------------------------------------------------
# DOWNLOAD EACH GAGE SEPARATELY
# ------------------------------------------------------------

total_comal <- get_discharge(
  site_number = "08168913",
  gage_name = "Total Comal River"
)

new_channel <- get_discharge(
  site_number = "08168932",
  gage_name = "New Channel"
)

old_channel <- get_discharge(
  site_number = "08169000",
  gage_name = "Old Channel"
)


# ------------------------------------------------------------
# COMBINE
# ------------------------------------------------------------

q_daily <- bind_rows(
  total_comal,
  new_channel,
  old_channel
) |>
  mutate(
    gage = factor(
      gage,
      levels = c(
        "Total Comal River",
        "New Channel",
        "Old Channel"
      )
    )
  ) |>
  arrange(
    gage,
    Date
  )


# ------------------------------------------------------------
# CHECK DATA
# ------------------------------------------------------------

gage_summary <- q_daily |>
  group_by(
    site_no,
    gage
  ) |>
  summarise(
    first_date = min(Date),
    last_date = max(Date),
    n_days = n(),
    
    min_cfs = min(
      Flow,
      na.rm = TRUE
    ),
    
    mean_cfs = mean(
      Flow,
      na.rm = TRUE
    ),
    
    max_cfs = max(
      Flow,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )


cat(
  "\n2018 USGS discharge summary:\n\n"
)

print(
  gage_summary,
  n = Inf
)


# ------------------------------------------------------------
# COLORS
# ------------------------------------------------------------

flow_colors <- c(
  
  "Total Comal River" =
    LANDA_COLORS[["lake_deep"]],
  
  "New Channel" =
    LANDA_COLORS[["lake"]],
  
  "Old Channel" =
    LANDA_COLORS[["coral"]]
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
        size = 14,
        color = LANDA_COLORS[["muted"]],
        margin = margin(
          b = 8
        )
      ),
      
      axis.title.x = element_blank(),
      
      axis.title.y = element_text(
        face = "bold",
        size = 15,
        color = LANDA_COLORS[["ink"]],
        margin = margin(
          r = 8
        )
      ),
      
      axis.text = element_text(
        size = 12,
        color = LANDA_COLORS[["muted"]]
      ),
      
      panel.grid.major.x =
        element_blank(),
      
      panel.grid.minor =
        element_blank(),
      
      panel.grid.major.y =
        element_line(
          color = "#DCD7C8",
          linewidth = 0.45
        ),
      
      axis.line.x =
        element_line(
          color = LANDA_COLORS[["muted"]],
          linewidth = 0.45
        ),
      
      axis.ticks =
        element_blank(),
      
      legend.position =
        "top",
      
      legend.justification =
        "left",
      
      legend.title =
        element_blank(),
      
      legend.text =
        element_text(
          size = 11.5,
          color = LANDA_COLORS[["ink"]]
        ),
      
      legend.key.width =
        unit(
          1.1,
          "cm"
        ),
      
      legend.spacing.x =
        unit(
          0.25,
          "cm"
        ),
      
      plot.margin =
        margin(
          8,
          14,
          10,
          10
        )
    )
}


# ------------------------------------------------------------
# PLOT
# ------------------------------------------------------------

flow_plot <- ggplot(
  q_daily,
  aes(
    x = Date,
    y = Flow,
    color = gage,
    group = gage
  )
) +
  
  geom_line(
    linewidth = 0.9,
    alpha = 0.95,
    lineend = "round"
  ) +
  
  scale_color_manual(
    values = flow_colors,
    
    breaks = c(
      "Total Comal River",
      "New Channel",
      "Old Channel"
    ),
    
    drop = FALSE
  ) +
  
  scale_x_date(
    limits = as.Date(
      c(
        "2018-01-01",
        "2018-12-31"
      )
    ),
    
    date_breaks =
      "2 months",
    
    date_labels =
      "%b",
    
    expand =
      expansion(
        mult = c(
          0,
          0
        )
      )
  ) +
  
  scale_y_continuous(
    labels =
      label_number(
        accuracy = 1,
        big.mark = ","
      ),
    
    expand =
      expansion(
        mult = c(
          0.02,
          0.05
        )
      )
  ) +
  
  labs(
    title = NULL,
    
    subtitle =
      "Daily Mean Comal River Discharge — 2018",
    
    y =
      "Discharge (cfs)"
  ) +
  
  theme_landa_flow()


# ------------------------------------------------------------
# PREVIEW
# ------------------------------------------------------------

print(
  flow_plot
)


# ------------------------------------------------------------
# SAVE
# ------------------------------------------------------------

ggsave(
  filename =
    output_file,
  
  plot =
    flow_plot,
  
  width =
    8.64,
  
  height =
    5.19,
  
  units =
    "in",
  
  dpi =
    300,
  
  bg =
    LANDA_COLORS[["sand"]]
)


# ------------------------------------------------------------
# FINISHED
# ------------------------------------------------------------

cat(
  "\nPlot written to:\n"
)

cat(
  output_file,
  "\n"
)