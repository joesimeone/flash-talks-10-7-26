#' Shared pieces for the indoor heat index band plots (Goal 1 climate zone and
#' Goal 2 Philadelphia), ported from make_dist_plot() in
#' C:/git/resstock_descrip/quarto/r1_temp_distribution_stats.
#' Must be sourced before make_mixed_humid_hi_plot.R and make_philly_hi_plot.R.
#' Libraries (tidyverse / scales / glue) are loaded in the deck's setup chunk.
#' ============================================================================

## ===============================================
#' Units and seasonal color
## ===============================================

#' Data are stored in C; slides read in F with a secondary C axis
F_TO_C <- function(f) (f - 32) * 5 / 9
C_TO_F <- function(c) c * 9 / 5 + 32

#' Cyclic ramp: endpoints match so December meets January
SEASON_RAMP <- c("#2166ac", "#92c5de", "#d73027", "#f4a582", "#2166ac")

year_frac <- function(d) (as.integer(format(d, "%m")) - 0.5) / 12

#' Same cap as the r1 main results, so the slides match the report
PRIMARY_CAP_F <- 120
BAND_COLOR <- "#b2182b"

## ===============================================
#' Band plot
## ===============================================

#' df needs date plus mean / p05 / p25 / p75 / p99 already in F. One ribbon
#' polygon per month so the outer band's fill can follow the season; theming is
#' left to the deck (tsp_theme) so it matches the other slides.
make_hi_band_plot <- function(df, title, n_label) {
  df <- df |>
    mutate(month = as.integer(format(date, "%m")), yr_frac = year_frac(date))

  ggplot(df, aes(x = date)) +
    geom_ribbon(
      aes(ymin = p05, ymax = p99, fill = yr_frac, group = month),
      alpha = 0.35
    ) +
    geom_ribbon(aes(ymin = p25, ymax = p75), fill = BAND_COLOR, alpha = 0.35) +
    geom_line(aes(y = mean), color = BAND_COLOR, linewidth = 0.6) +
    scale_fill_gradientn(
      colors = SEASON_RAMP,
      limits = c(0, 1),
      breaks = year_frac(as.Date(c(
        "2018-01-15",
        "2018-04-15",
        "2018-07-15",
        "2018-10-15"
      ))),
      labels = c("Jan", "Apr", "Jul", "Oct"),
      name = "Time of year",
      guide = guide_colorbar(barwidth = 8, barheight = 0.4)
    ) +
    scale_x_date(date_breaks = "2 months", date_labels = "%b") +
    scale_y_continuous(
      sec.axis = sec_axis(~ F_TO_C(.), name = "Heat index (°C)")
    ) +
    labs(
      title = title,
      subtitle = glue(
        "*Line = daily mean; bands = IQR (p25–p75) and 5th–99th percentile · {n_label}*"
      ),
      x = NULL,
      y = "Heat index (°F)"
    )
}
