#' Daily max indoor heat index for Philadelphia County (Goal 2), with outdoor
#' daily max heat index from the AMY2018 simulation weather (dashed) and
#' 1991-2020 Philadelphia Intl normals (solid) overlaid.
#' Depends on helper_files/philly_hi/*.parquet from 00_prep_philly_hi_data.R,
#' and on 01_hi_plot_helpers.R being sourced first.
#' Libraries (tidyverse / arrow / scales / glue) are loaded in the deck's setup chunk.
#' ============================================================================

## ===============================================
#' Import and shape
## ===============================================

PHILLY_DIR <- file.path("helper_files", "philly_hi")

philly_hi <-
  read_parquet(file.path(PHILLY_DIR, "philly_indoor_hi_max.parquet")) |>
  left_join(
    read_parquet(file.path(PHILLY_DIR, "philly_amy2018_hi_max.parquet")),
    by = "date"
  ) |>
  left_join(
    read_parquet(file.path(PHILLY_DIR, "philly_normals_hi_max.parquet")),
    by = "date"
  ) |>
  mutate(
    date = as.Date(date),
    across(c(mean, p05, p25, p75, p99, amy_hi_max, norm_hi_max), C_TO_F)
  ) |>
  arrange(date)

#' Long form so linetype gets a legend instead of an unexplained pair of lines
philly_outdoor <-
  philly_hi |>
  select(date, amy_hi_max, norm_hi_max) |>
  pivot_longer(-date, names_to = "source", values_to = "hi_f") |>
  mutate(
    source = recode(
      source,
      amy_hi_max = "Outdoor, AMY2018 (simulation input)",
      norm_hi_max = "Outdoor, 1991–2020 normal (PHL Intl)"
    )
  )

## ===============================================
#' Plot
## ===============================================

philly_hi_plot <-
  make_hi_band_plot(
    philly_hi,
    title = "**Daily max heat index, Philadelphia County (2018)**",
    n_label = glue(
      "n = {comma(philly_hi$n_sim[1])} sims · {comma(round(philly_hi$n_homes[1]))} homes"
    )
  ) +
  geom_line(
    data = philly_outdoor,
    aes(y = hi_f, linetype = source),
    color = "black",
    linewidth = 0.4
  ) +
  scale_linetype_manual(
    values = c(
      "Outdoor, AMY2018 (simulation input)" = "dashed",
      "Outdoor, 1991–2020 normal (PHL Intl)" = "solid"
    ),
    name = NULL
  )
