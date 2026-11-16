# artfish_flouca_by_period

artfish_flouca_by_period

## Usage

``` r
artfish_flouca_by_period(
  year = NULL,
  month = NULL,
  active_vessels,
  effort,
  effort_source = c("survey", "registry"),
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

  effort source whether it's derived from survey -B1- (fishers
  interviews) or registry -B2- (boat counting)

- active_days:

  active days

- landings:

  landings

- minor_strata:

  minor_strata (to investigate further later)

## Value

the result of Artfish for a given year/month
