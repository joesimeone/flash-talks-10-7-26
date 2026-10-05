#' NLR ResStock 2025.1 Delaware & Philadelphia prototypes used in my test
#' temperature simulations. Ported from
#' C:/git/resstock_work/quarto/delco_phl_test/delco_phl_exp.qmd
phl_delco_stock <-
  read_csv(
    'C:/git/resstock_work/nlr_samps_2025/delco_phl_sample_tst2.csv',
    show_col_types = FALSE
  )

#' keep only characteristics that actually vary across prototypes
phl_delco_varying <-
  phl_delco_stock |>
  select(where(~ n_distinct(.x) > 1))

## --- Column classification (edit these vectors to re-bucket) ---
schedule_cols <- c(
  "Bathroom Spot Vent Hour", "Range Spot Vent Hour", "Holiday Lighting",
  "Lighting Interior Use", "Lighting Other Use", "Plug Loads", "Usage Level",
  "Clothes Dryer Usage Level", "Clothes Washer Usage Level",
  "Cooking Range Usage Level", "Dishwasher Usage Level", "Refrigerator Usage Level",
  "Hot Water Fixtures", "Plug Load Diversity", "Occupants", "Vacancy Status",
  "Cooling Setpoint", "Cooling Setpoint Has Offset", "Cooling Setpoint Offset Magnitude",
  "Cooling Setpoint Offset Period", "Heating Setpoint", "Heating Setpoint Has Offset",
  "Heating Setpoint Offset Magnitude", "Heating Setpoint Offset Period",
  "Cooling Unavailable Days", "Heating Unavailable Days",
  "Electric Vehicle Miles Traveled", "Electric Vehicle Charge At Home"
)

hybrid_cols <- c(
  "Ceiling Fan", "Clothes Washer Presence", "Clothes Washer", "Clothes Dryer",
  "Cooking Range", "Dishwasher", "Refrigerator", "Misc Extra Refrigerator",
  "Misc Freezer", "Misc Hot Tub Spa", "Misc Pool", "Misc Pool Heater",
  "Misc Pool Pump", "Misc Well Pump", "Misc Gas Fireplace", "Misc Gas Grill",
  "Misc Gas Lighting", "Electric Vehicle Battery", "Electric Vehicle Ownership",
  "Electric Vehicle Charger", "Electric Vehicle Outlet Access"
)

neutral_cols <- c(
  "Building", "ASHRAE IECC Climate Zone 2004", "County and PUMA", "AIANNH Area",
  "CEC Climate Zone", "City", "County", "AHS Region",
  "ASHRAE IECC Climate Zone 2004 - Sub-CZ Split", "Building America Climate Zone",
  "Energystar Climate Zone 2023", "Generation And Emissions Assessment Region",
  "ISO RTO Region", "Metropolitan and Micropolitan Statistical Area",
  "County Metro Status", "PUMA", "PUMA Metro Status", "REEDS Balancing Area",
  "State", "Census Division", "Census Division RECS", "Census Region",
  "Custom State", "Location Region", "Tenure", "Income", "Income RECS2015",
  "Income RECS2020", "Area Median Income", "Federal Poverty Level",
  "Household Has Tribal Persons", "State Metro Median Income"
)

cat_of <- function(x) case_when(
  x %in% schedule_cols ~ "schedule",
  x %in% hybrid_cols   ~ "hybrid",
  x %in% neutral_cols  ~ "neutral",
  TRUE                 ~ "physics"
)

## order columns: neutral -> physics -> hybrid -> schedule
present <- names(phl_delco_varying)
rank    <- c(neutral = 1, physics = 2, hybrid = 3, schedule = 4)
ord     <- present[order(rank[cat_of(present)], seq_along(present))]
phl_delco_varying <- select(phl_delco_varying, all_of(ord))

## light tints per category
fill_col <- c(neutral = "#F1EFE8", physics = "#E6F1FB", hybrid = "#EEEDFE", schedule = "#FAEEDA")
head_col <- c(neutral = "#D3D1C7", physics = "#B5D4F4", hybrid = "#CECBF6", schedule = "#FAC775")
grp_lab  <- c(neutral = "Geography & IDs", physics = "Physics / structural",
              hybrid = "Dual-role", schedule = "Stochastic-schedule inputs")

col_defs <- setNames(lapply(ord, function(cn) {
  k <- cat_of(cn)
  colDef(
    minWidth    = 110,
    headerStyle = list(fontSize = "12px", background = head_col[[k]]),
    style       = list(fontSize = "12px", whiteSpace = "nowrap", background = fill_col[[k]])
  )
}), ord)

col_groups <- Filter(Negate(is.null), lapply(c("neutral", "physics", "hybrid", "schedule"), function(k) {
  ck <- ord[cat_of(ord) == k]
  if (length(ck)) colGroup(name = grp_lab[[k]], columns = ck,
                           headerStyle = list(background = head_col[[k]], fontSize = "12px")) else NULL
}))

n_proto <- nrow(phl_delco_stock)
n_char  <- ncol(phl_delco_varying)

chip <- function(color, label) tags$span(
  style = "display:inline-flex;align-items:center;gap:5px;margin-right:14px;font-size:0.72rem;color:#555;",
  tags$span(style = glue("width:11px;height:11px;border-radius:2px;border:1px solid rgba(0,0,0,.15);background:{color};")),
  label
)

bldg_char_table <-
  browsable(tagList(
    tags$p(
      style = "font-size:0.8rem; color:#6c6c6c; margin:0 0 4px;",
      glue("↔ {scales::comma(n_proto)} sampled prototypes · {n_char} varying characteristics, grouped by role. Scroll right or use search to explore.")
    ),
    tags$div(
      style = "margin:0 0 8px;",
      chip(head_col[["physics"]],  "Physics / structural"),
      chip(head_col[["schedule"]], "Stochastic-schedule input"),
      chip(head_col[["hybrid"]],   "Both (dual-role)"),
      chip(head_col[["neutral"]],  "Geography & IDs")
    ),
    reactable(
      phl_delco_varying,
      columns             = col_defs,
      columnGroups        = col_groups,
      searchable          = TRUE,
      compact             = TRUE,
      resizable           = TRUE,
      highlight           = TRUE,
      bordered            = TRUE,
      defaultPageSize     = 8,
      showPageSizeOptions = TRUE,
      pageSizeOptions     = c(8, 15, 25),
      theme               = reactableTheme(cellPadding = "4px 6px")
    )
  ))
