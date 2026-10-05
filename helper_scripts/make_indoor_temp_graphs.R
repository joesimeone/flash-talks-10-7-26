pa_demo <-
  tbl(nrel_new_con, 'nrel_annual_ver_0') |>
  filter(
    in.state == 'PA',
    in.vacancy_status == "Occupied",
    !in.geometry_building_type_acs %in%
      c('2 Unit', '3 or 4 unit', 'Mobile Home')
  ) |>
  select(
    bldg_id,
    weight,
    in.state,
    in.county_name,
    in.county,
    in.city,
    in.geometry_building_type_acs,
    in.vintage_acs,
    in.hvac_cooling_type
  ) |>
  mutate(
    x_year_broad = case_when(
      in.vintage_acs == '<1940' ~ ' Before 1940',
      in.vintage_acs %in% c('1940-59', '1960-79') ~ ' 1940 - 1980',
      TRUE ~ ' After 1980'
    ),
    x_hhtype = case_when(
      in.geometry_building_type_acs == 'Single-Family Detached' ~
        "single family detached",
      in.geometry_building_type_acs == 'Single-Family Attached' ~
        "single family attached",
      TRUE ~ "apartment 5+ Units"
    )
  ) |>
  left_join(tbl(nrel_new_con, 'pa_v0_sample_temps'), by = c('bldg_id')) |>
  mutate(
    date = make_date(year, month, day)
  )

phl_outdoor_temps <-
  tbl(nrel_new_con, 'pa_v32_amy_weath') |>
  filter(county_id == 'G4201010') |>
  select(date, county_id, daily_mean_outdoor_drybuld_temp) |>
  collect()


phl_indoor_v_outdoor <-
  pa_demo |>
  mutate(
    ac_binary = if_else(in.hvac_cooling_type == 'None', 'No AC', 'AC')
  ) |>
  filter(in.county == 'G4201010') |>
  summarise(
    mean_indoor_op_temp = mean(mean_daily_op_indoor_temp),
    .by = c(x_hhtype, x_year_broad, ac_binary, date)
  ) |>
  mutate(mean_indoor_op_temp_f = (mean_indoor_op_temp * 9 / 5) + 32) |>
  arrange(date, x_hhtype, x_year_broad, ac_binary) |>
  collect() |>
  left_join(phl_outdoor_temps, by = c('date')) |>
  mutate(
    mean_outdoor_db_temp_f = (daily_mean_outdoor_drybuld_temp * 9 / 5) + 32
  ) |>
  mutate(max_outdoor_db_temp_f = max(mean_outdoor_db_temp_f), .by = date)

year_counts <-
  tbl(nrel_new_con, 'nrel_annual_ver_0') |>
  mutate(
    x_year_broad = case_when(
      in.vintage_acs == '<1940' ~ ' Before 1940',
      in.vintage_acs %in% c('1940-59', '1960-79') ~ ' 1940 - 1980',
      TRUE ~ ' After 1980'
    )
  ) |>
  filter(in.county == 'G4201010') |>
  mutate(
    ac_binary = if_else(in.hvac_cooling_type == 'None', 'No AC', 'AC')
  ) |>
  summarise(
    yr_count = sum(weight),
    .by = c(x_year_broad, ac_binary)
  ) |>
  collect() |>
  mutate(
    yr_count = round(yr_count, 3),
    yr_count = format(yr_count, big.mark = ','),
yr_count = trimws(yr_count)
  )

phl_indoor_v_outdoor <-
  phl_indoor_v_outdoor |>
  left_join(year_counts, by = join_by(x_year_broad, ac_binary)) |>
  mutate(
    x_year_broad = as.factor(x_year_broad),
    x_year_broad = fct_relevel(
      x_year_broad,
      " Before 1940",
      " 1940 - 1980",
      " After 1980"
    ),
year_fac = str_glue('Built {x_year_broad} (N = **{yr_count}**)'),
year_fac = fct_reorder(year_fac, as.numeric(x_year_broad))    
  )


no_ac_phl_plot <-
  phl_indoor_v_outdoor |>
  filter(ac_binary == 'No AC') |>
  ggplot(aes(date, mean_indoor_op_temp_f, color = x_hhtype)) +
  geom_line(
    aes(y = mean_outdoor_db_temp_f,  linetype = 'Outdoor Temperature'),
    linetype = "dashed",
    color = 'darkgray'
  ) +
  geom_line() +
  facet_wrap(~year_fac, nrow = 3) +
  scale_color_brewer(palette = 'Set1') +
  labs(
    y = 'Operative Temperature (F°)',
    x = NULL,
     linetype = NULL,
    title = '**NREL ResStock Simulations in Non Air-Conditioned Homes**',
    subtitle = '*Daily Average Indoor Temperatures in Philadelphia County, 2018*',
    caption = 'Outdoor dry bulb temperature plotted as dashed line for reference.'
  )

ac_phl_plot <-
  phl_indoor_v_outdoor |>
  filter(ac_binary == 'AC') |>
  ggplot(aes(date, mean_indoor_op_temp_f, color = x_hhtype)) +
  geom_line(
    aes(y = mean_outdoor_db_temp_f,  linetype = 'Outdoor Temperature'),
    linetype = "dashed",
    color = 'darkgray'
  ) +
  geom_line() +
  facet_wrap(~year_fac, nrow = 3) +
  scale_color_brewer(palette = 'Set1') +
  labs(
    y = 'Operative Temperature (F°)',
    x = NULL,
     linetype = NULL,
    title = '**NREL ResStock Simulations in Air-Conditioned Homes**',
    subtitle = '*Daily Average Indoor Temperatures in Philadelphia County, 2018*',
    caption = 'Outdoor dry bulb temperature plotted as dashed line for reference.'
  )
