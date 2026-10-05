#' Builds the three daily series behind the Philadelphia County slide and saves
#' them to helper_files/: indoor daily max heat index (ResStock), outdoor daily
#' max heat index from the AMY2018 weather file used as a simulation input, and
#' outdoor daily max heat index from 1991-2020 Philadelphia Intl normals.
#' Run once by hand (not sourced by the deck). Needs the resstock_descrip repo
#' (output/daily_src, resstock_2025.1_inputs.parquet, heat index + S3 helpers)
#' and internet access for the S3 weather file and the normals EPW.
#' ============================================================================

library(tidyverse)
library(duckdb)
library(glue)
library(arrow)
library(data.table)

## ===============================================
#' Paths and constants
## ===============================================

DESCRIP_ROOT <- "C:/git/resstock_descrip"
DECK_ROOT <- "C:/git/flash_talks_10.7.26"
OUT_DIR <- file.path(DECK_ROOT, "helper_files", "philly_hi")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

#' Reuse the pipeline's Rothfusz SQL so indoor and outdoor heat index match
#' exactly, and the existing S3 reader for the AMY2018 weather file
source(file.path(
  DESCRIP_ROOT,
  "code",
  "helper_scripts",
  "get_daily_distribution.R"
))
source(file.path(
  DESCRIP_ROOT,
  "code",
  "helper_scripts",
  "get_amy2018_weather.R"
))

PHL_GISJOIN <- "G4201010"

#' Same exclusions as the climate-region results: 120 F building peak cap and
#' building-days with indoor dewpoint over 35 C dropped
CAP_C <- round((120 - 32) * 5 / 9, 4)
MAX_DP_C <- 35

#' Philadelphia Intl (WMO 724080), matching get_climate_normals_climate_regions.R
PHL_WMO <- "724080"

con <- dbConnect(duckdb())

## ===============================================
#' Indoor: daily max heat index across Philly buildings
## ===============================================

#' Peak is taken over all days (before the dewpoint filter) because that's how
#' bldg_peak_temps is built in 01_exclude_bad_bld_sims.R
indoor_sql <- glue(
  "
WITH meta AS (
  SELECT bldg_id, weight
  FROM read_parquet('{DESCRIP_ROOT}/resstock_2025.1_inputs.parquet')
  WHERE \"in.county\" = '{PHL_GISJOIN}'
    AND \"in.vacancy_status\" = 'Occupied'
    AND upgrade = 0
),
daily AS (
  SELECT d.bldg_id, d.date, d.hi_max_daily, d.dp_max_daily, d.op_max_daily, m.weight
  FROM read_parquet('{DESCRIP_ROOT}/output/daily_src/state=PA/*.parquet') d
  JOIN meta m USING (bldg_id)
),
keep AS (
  SELECT bldg_id, ANY_VALUE(weight) AS weight
  FROM daily
  GROUP BY bldg_id
  HAVING MAX(op_max_daily) <= {CAP_C}
),
counts AS (
  SELECT COUNT(*) AS n_sim, SUM(weight) AS n_homes FROM keep
)
SELECT
  d.date,
  COUNT(*) AS n_bldg_day,
  AVG(d.hi_max_daily) AS mean,
  quantile_cont(d.hi_max_daily, 0.05) AS p05,
  quantile_cont(d.hi_max_daily, 0.25) AS p25,
  quantile_cont(d.hi_max_daily, 0.50) AS median,
  quantile_cont(d.hi_max_daily, 0.75) AS p75,
  quantile_cont(d.hi_max_daily, 0.99) AS p99,
  ANY_VALUE(c.n_sim) AS n_sim,
  ANY_VALUE(c.n_homes) AS n_homes
FROM daily d
JOIN keep USING (bldg_id)
CROSS JOIN counts c
WHERE d.dp_max_daily <= {MAX_DP_C}
GROUP BY d.date
ORDER BY d.date
"
)

philly_indoor_hi <- dbGetQuery(con, indoor_sql) |> as_tibble()

## ===============================================
#' Outdoor: AMY2018 simulation weather (S3)
## ===============================================

phl_amy_hourly <- get_amy2018_weather(
  counties = PHL_GISJOIN,
  states = "PA"
)

duckdb_register(con, "phl_amy_hourly", phl_amy_hourly)

#' Timestamps are hour-ending (01:00 .. next day's 00:00), so shift back an
#' hour before taking the date or Dec 31's last hour lands on 2019-01-01
philly_amy_hi <- dbGetQuery(
  con,
  glue(
    "
  SELECT
    CAST(date_time - INTERVAL 1 HOUR AS DATE) AS date,
    MAX({rothfusz_hi_sql('outdoor_drybulb_temp_c', 'relative_humidity_pct')}) AS amy_hi_max
  FROM phl_amy_hourly
  GROUP BY 1
  ORDER BY 1
"
  )
) |>
  as_tibble()

## ===============================================
#' Outdoor: 1991-2020 Philadelphia Intl normals
## ===============================================

epw_dir <- file.path(OUT_DIR, "epw")
dir.create(epw_dir, showWarnings = FALSE)

eplusr::download_weather(
  sprintf("%s.*Normals.*1991.2020", PHL_WMO),
  dir = epw_dir,
  type = "all",
  ask = FALSE,
  max_match = 1
)

zip_file <- list.files(epw_dir, "\\.zip$", full.names = TRUE)[1]
epw_name <- grep("\\.epw$", unzip(zip_file, list = TRUE)$Name, value = TRUE)
unzip(zip_file, files = epw_name, exdir = epw_dir, junkpaths = TRUE)

#' eplusr::read_epw() trips on these files, so read raw like the existing
#' normals script and name only the columns we need
phl_norm_hourly <-
  fread(file.path(epw_dir, basename(epw_name)), skip = 8, header = FALSE) |>
  as_tibble() |>
  select(month = V2, day = V3, dry_bulb_c = V7, rh_pct = V9) |>
  #' Normals have no real year; pin to 2018 so they line up with AMY2018
  mutate(date = make_date(2018, month, day))

duckdb_register(con, "phl_norm_hourly", phl_norm_hourly)

philly_norm_hi <- dbGetQuery(
  con,
  glue(
    "
  SELECT
    date,
    MAX({rothfusz_hi_sql('dry_bulb_c', 'rh_pct')}) AS norm_hi_max
  FROM phl_norm_hourly
  GROUP BY 1
  ORDER BY 1
"
  )
) |>
  as_tibble()

dbDisconnect(con)

## ===============================================
#' Exports
## ===============================================

#' All in C; the plot script converts to F for display
write_parquet(
  philly_indoor_hi,
  file.path(OUT_DIR, "philly_indoor_hi_max.parquet")
)
write_parquet(
  philly_amy_hi,
  file.path(OUT_DIR, "philly_amy2018_hi_max.parquet")
)
write_parquet(
  philly_norm_hi,
  file.path(OUT_DIR, "philly_normals_hi_max.parquet")
)
