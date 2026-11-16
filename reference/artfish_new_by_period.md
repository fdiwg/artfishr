# artfish_new_by_period

artfish_new_by_period

## Usage

``` r
artfish_new_by_period(
  year = NULL,
  month = NULL,
  active_vessels,
  effort,
  effort_source = c("fisher_interview", "boat_counting"),
  active_days = NULL,
  landings,
  minor_strata = NULL,
  validate = TRUE
)
```

## Arguments

- active_vessels:

  active vessels

- effort:

  effort

- effort_source:

  effort source whether it's derived from -B1- (fishers interviews) or
  -B2- (boat counting)

- active_days:

  active days

- landings:

  landings

- minor_strata:

  minor_strata (to investigate further later)

## Value

the result of Artfish for a given year/month
