#' Daily max indoor heat index for the 4A Mixed-Humid climate zone (Goal 1).
#' Depends on daily_hardcap_sweep_distribution_by_climate_region.parquet, written
#' by resstock_descrip/code/daily_metric_flows/02_derive_daily_clim_reg_metrics.R,
#' and on 01_hi_plot_helpers.R being sourced first.
#' Libraries (tidyverse / arrow / scales / glue) are loaded in the deck's setup chunk.
#' ============================================================================

## ===============================================
#' Import and shape
## ===============================================

mh_hi <-
  read_parquet(
    "C:/git/resstock_descrip/data/clim_reg_daily/daily_hardcap_sweep_distribution_by_climate_region.parquet",
    col_select = c(
      cap_f,
      group_value,
      date,
      n_sim,
      n_homes,
      starts_with("hi_max_")
    )
  ) |>
  filter(cap_f == PRIMARY_CAP_F, group_value == "4A") |>
  rename_with(~ str_remove(.x, "^hi_max_"), starts_with("hi_max_")) |>
  mutate(
    date = as.Date(date),
    across(c(mean, p05, p25, p75, p99), C_TO_F)
  ) |>
  arrange(date)

## ===============================================
#' Plot
## ===============================================

mh_hi_plot <- make_hi_band_plot(
  mh_hi,
  title = "**Daily max indoor heat index, 4A Mixed–Humid climate zone (2018)**",
  n_label = glue(
    "n = {comma(mh_hi$n_sim[1])} sims · {comma(round(mh_hi$n_homes[1]))} homes"
  )
)
