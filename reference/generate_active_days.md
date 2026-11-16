# Generates a tibble of active days.

Function that generates a table of active days for all periods. The
unique list of fishing units are inherited from available tables
(active_vessels, effort, landings). In the same way, the list of minor
strata values will be inherited based on the minor_strata columns
available in data.

## Usage

``` r
generate_active_days(
  active_vessels,
  effort,
  effort_source,
  landings,
  minor_strata = NULL
)
```

## Arguments

- active_vessels:

  active vessels table

- effort:

  effort table

- effort_source:

  effort source

- landings:

  landings table

- minor_strata:

  minor strata. Default is `NULL`

## Value

an object of class tibble give active days
