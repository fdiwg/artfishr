# Computes nominal effort estimate

Computes nominal effort estimate

## Usage

``` r
compute_effort_estimate(
  effort,
  effort_source = c("fisher_interview", "boat_counting", "household_interview"),
  landings,
  active_days = NULL,
  active_vessels = NULL,
  active_vessels_strategy = c("latest", "closest"),
  census_typology = NULL,
  minor_strata = NULL,
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

  minor_strata

- progress_fn:

  progress_fn
