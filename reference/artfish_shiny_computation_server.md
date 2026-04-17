# ARTFISH Computation - Shiny Server Module

Server-side shiny logic to run Artfish computation as business service

## Usage

``` r
artfish_shiny_computation_server(
  id,
  refresh,
  effort,
  effort_source,
  active_vessels,
  active_vessels_strategy,
  active_days,
  landings,
  minor_strata = NULL,
  progress_fn = NULL
)
```

## Arguments

- id:

  Character string. Module namespace identifier.

- refresh:

  refresher for computation

- effort:

  effort data

- effort_source:

  Character string indicating the type of effort source.

- active_vessels:

  active vessels data

- active_vessels_strategy:

  active vessels strategy

- active_days:

  active days

- landings:

  landings

- minor_strata:

  Character string targeting a column name considered as minor strata.

- progress_fn:

  A function to monitor progress
