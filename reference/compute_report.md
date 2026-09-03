# compute_report

compute_report

## Usage

``` r
compute_report(
  effort,
  effort_source = c("fisher_interview", "boat_counting", "household_interview"),
  landings,
  active_days = NULL,
  active_vessels = NULL,
  active_vessels_strategy = NULL,
  census_typology = NULL,
  minor_strata = NULL,
  validate = FALSE,
  progress_fn = NULL
)
```

## Arguments

- effort:

  effort

- effort_source:

  effort_source

- landings:

  landings

- active_days:

  active_days

- active_vessels:

  active_vessels

- active_vessels_strategy:

  active_vessels_strategy

- census_typology:

  census_typology

- minor_strata:

  minor_strata. Default is `NULL`

- validate:

  validate

- progress_fn:

  a progress function with args (label, p). Default is `NULL`

## Value

the result of Artfish
