# -----------------------------------------------------------------------------
# Script:   00_theme.R
# Purpose:  Shared chart style for the project: Payco palette (navy, teal,
#           white), ggplot2 theme, colour scales, number formats and a helper
#           to export charts in high resolution. Sourced by 04-07.
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(ggplot2)
  library(scales)
})

# Palette ---------------------------------------------------------------------
pay <- list(
  navy        = "#0B1F3A",   # titles, key text, second series
  navy_mid    = "#27466E",
  teal        = "#0E9F9A",   # primary accent
  teal_dark   = "#08736F",
  teal_light  = "#7FD1CC",
  teal_pale   = "#E3F4F3",
  alert       = "#E4572E",   # fraud / failure highlight, used sparingly
  slate       = "#5A6B7F",
  grey        = "#98A4B3",
  grey_light  = "#D9DFE6",
  grid        = "#EDF0F3",
  white       = "#FFFFFF",
  text_muted  = "#55606E"
)

# Categorical order: always assigned in this order, never recycled
pay_categorical <- c(pay$teal, pay$navy, pay$teal_light, pay$slate, pay$teal_dark, pay$grey)

# Sequential (magnitude): white -> teal -> navy
pay_sequential <- c(pay$white, pay$teal_pale, pay$teal_light, pay$teal, pay$navy)

# Two-colour comparison (e.g. model vs baseline, positive vs negative class)
pay_pair <- c(pay$teal, pay$navy)

# Theme -----------------------------------------------------------------------
theme_payco <- function(base_size = 12, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      plot.background    = element_rect(fill = pay$white, colour = NA),
      panel.background   = element_rect(fill = pay$white, colour = NA),
      panel.grid.major.y = element_line(colour = pay$grid, linewidth = 0.4),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      axis.line.x        = element_line(colour = pay$grey_light, linewidth = 0.5),
      axis.ticks         = element_blank(),
      axis.text          = element_text(colour = pay$text_muted, size = rel(0.85)),
      axis.title         = element_text(colour = pay$text_muted, size = rel(0.85)),
      axis.title.y       = element_text(colour = pay$text_muted, size = rel(0.85), angle = 90,
                                        margin = margin(r = 8)),
      axis.title.x       = element_text(colour = pay$text_muted, size = rel(0.85),
                                        margin = margin(t = 8)),
      plot.title         = element_text(colour = pay$navy, face = "bold", size = rel(1.35),
                                        hjust = 0, margin = margin(b = 4)),
      plot.subtitle      = element_text(colour = pay$text_muted, size = rel(0.95),
                                        hjust = 0, margin = margin(b = 14)),
      plot.caption       = element_text(colour = pay$grey, size = rel(0.75), hjust = 0,
                                        margin = margin(t = 12)),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.margin        = margin(18, 22, 14, 18),
      legend.position    = "top",
      legend.justification = "left",
      legend.title       = element_blank(),
      legend.text        = element_text(colour = pay$navy, size = rel(0.85)),
      legend.key.size    = unit(0.9, "lines"),
      legend.margin      = margin(0, 0, 4, 0),
      strip.text         = element_text(colour = pay$navy, face = "bold", size = rel(0.9),
                                        hjust = 0, margin = margin(b = 6, t = 6)),
      panel.spacing      = unit(1.2, "lines")
    )
}

theme_set(theme_payco())
update_geom_defaults("line",  list(colour = pay$teal, linewidth = 1))
update_geom_defaults("point", list(colour = pay$teal, size = 2.4))
update_geom_defaults("col",   list(fill = pay$teal))
update_geom_defaults("bar",   list(fill = pay$teal))
update_geom_defaults("text",  list(colour = pay$navy, size = 3.4))

# Scales ----------------------------------------------------------------------
scale_colour_payco <- function(...) scale_colour_manual(values = pay_categorical, ...)
scale_fill_payco   <- function(...) scale_fill_manual(values = pay_categorical, ...)
scale_fill_payco_seq <- function(...) scale_fill_gradientn(colours = pay_sequential, ...)

# Number formats --------------------------------------------------------------
# Short money labels: €950, €63K, €5.4M
fmt_eur <- function(x) {
  out <- ifelse(abs(x) >= 1e6, paste0("€", formatC(x / 1e6, format = "f", digits = 1), "M"),
         ifelse(abs(x) >= 1e3, paste0("€", formatC(x / 1e3, format = "f", digits = 0), "K"),
                paste0("€", formatC(x, format = "f", digits = 0))))
  out[is.na(x)] <- NA
  out
}
fmt_pct   <- label_percent(accuracy = 1)
fmt_pct1  <- label_percent(accuracy = 0.1)
fmt_pct2  <- label_percent(accuracy = 0.01)
fmt_num   <- label_comma(accuracy = 1)

chart_caption <- "Source: Payco synthetic payments dataset, 2021-2025"

# Export ----------------------------------------------------------------------
save_chart <- function(plot, name, width = 10, height = 6, dpi = 300, dir = "Output/charts") {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  file <- file.path(dir, paste0(name, ".png"))
  if (requireNamespace("ragg", quietly = TRUE)) {
    ggsave(file, plot, width = width, height = height, dpi = dpi, device = ragg::agg_png, bg = "white")
  } else {
    ggsave(file, plot, width = width, height = height, dpi = dpi, bg = "white")
  }
  invisible(file)
}
