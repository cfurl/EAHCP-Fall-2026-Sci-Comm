# EAHCP Springflow Protection presentation plot theme
# Designed to match EAHCP_Springflow_Protection_Template.pptx

library(ggplot2)

EAHCP_COLORS <- c(
  aquifer        = "#0B3C5D",
  spring         = "#2D7F83",
  flow           = "#6DB6C4",
  cypress        = "#5F7D68",
  limestone      = "#D5CFC2",
  limestone_dark = "#A69E8F",
  paper          = "#F7F5F0",
  ink            = "#24343B",
  muted          = "#63747A",
  alert          = "#C65D4A",
  pale_blue      = "#DDECEF",
  pale_green     = "#E5ECE6",
  pale_alert     = "#F2E1DC"
)

# Default discrete series colors. Keep most plots restrained; use alert only
# for a threshold, exception, implementation trigger, or decision point.
EAHCP_DISCRETE <- unname(EAHCP_COLORS[c(
  "aquifer", "spring", "cypress", "flow", "limestone_dark", "alert"
)])

# A presentation-first ggplot2 theme.
theme_eahcp <- function(base_size = 18, base_family = "Aptos") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.background = element_rect(
        fill = EAHCP_COLORS[["paper"]],
        color = NA
      ),
      panel.background = element_rect(
        fill = EAHCP_COLORS[["paper"]],
        color = NA
      ),
      plot.title = element_text(
        family = "Aptos Display",
        face = "bold",
        size = rel(1.32),
        color = EAHCP_COLORS[["aquifer"]],
        margin = margin(b = 8)
      ),
      plot.subtitle = element_text(
        color = EAHCP_COLORS[["muted"]],
        size = rel(0.88),
        margin = margin(b = 18)
      ),
      plot.caption = element_text(
        color = EAHCP_COLORS[["muted"]],
        size = rel(0.62),
        hjust = 0,
        margin = margin(t = 12)
      ),
      axis.title = element_text(
        face = "bold",
        size = rel(0.76),
        color = EAHCP_COLORS[["ink"]]
      ),
      axis.text = element_text(
        size = rel(0.72),
        color = EAHCP_COLORS[["muted"]]
      ),
      axis.title.x = element_text(margin = margin(t = 12)),
      axis.title.y = element_text(margin = margin(r = 12)),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(
        color = "#D8DEDD",
        linewidth = 0.45
      ),
      axis.line.x = element_line(
        color = EAHCP_COLORS[["muted"]],
        linewidth = 0.45
      ),
      axis.ticks = element_blank(),
      legend.position = "top",
      legend.justification = "left",
      legend.title = element_text(
        face = "bold",
        size = rel(0.70),
        color = EAHCP_COLORS[["ink"]]
      ),
      legend.text = element_text(
        size = rel(0.70),
        color = EAHCP_COLORS[["ink"]]
      ),
      legend.key.height = grid::unit(0.34, "cm"),
      legend.key.width = grid::unit(0.55, "cm"),
      strip.background = element_rect(
        fill = EAHCP_COLORS[["pale_blue"]],
        color = NA
      ),
      strip.text = element_text(
        face = "bold",
        color = EAHCP_COLORS[["aquifer"]],
        margin = margin(6, 8, 6, 8)
      ),
      plot.margin = margin(18, 20, 14, 18)
    )
}

scale_color_eahcp <- function(..., values = EAHCP_DISCRETE) {
  scale_color_manual(values = values, ...)
}

scale_fill_eahcp <- function(..., values = EAHCP_DISCRETE) {
  scale_fill_manual(values = values, ...)
}

# Continuous scientific scales suitable for springflow, depth, volume, or
# monitoring intensity. Reverse the vector when high values should be darker.
scale_color_eahcp_continuous <- function(..., limits = NULL, breaks = waiver()) {
  scale_color_gradientn(
    colours = unname(EAHCP_COLORS[c("pale_blue", "flow", "spring", "aquifer")]),
    limits = limits,
    breaks = breaks,
    ...
  )
}

scale_fill_eahcp_continuous <- function(..., limits = NULL, breaks = waiver()) {
  scale_fill_gradientn(
    colours = unname(EAHCP_COLORS[c("pale_blue", "flow", "spring", "aquifer")]),
    limits = limits,
    breaks = breaks,
    ...
  )
}

# Use for threshold bands in hydrographs or trigger plots.
eahcp_threshold_band <- function(ymin, ymax, alpha = 0.48) {
  annotate(
    "rect",
    xmin = -Inf,
    xmax = Inf,
    ymin = ymin,
    ymax = ymax,
    fill = EAHCP_COLORS[["pale_alert"]],
    alpha = alpha
  )
}

# Example:
# ggplot(df, aes(date, springflow_cfs)) +
#   eahcp_threshold_band(45, 60) +
#   geom_line(linewidth = 1.25, color = EAHCP_COLORS[["spring"]]) +
#   geom_hline(yintercept = 45, linetype = "22",
#              color = EAHCP_COLORS[["alert"]], linewidth = 0.8) +
#   labs(
#     title = "Springflow remained above the implementation threshold",
#     subtitle = "Daily mean discharge; implementation year 2026",
#     x = NULL,
#     y = "Springflow (cfs)",
#     caption = "Source: Edwards Aquifer Authority"
#   ) +
#   theme_eahcp()
