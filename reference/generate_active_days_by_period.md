# Generates a tibble of active days.

Function that generates a table of active days by year/month. The unique
list of fishing units are inherited from available tables (effort,
landings + eventually active_vessels). In the same way, the list of
minor strata values will be inherited based on the minor_strata columns
available in data.

## Usage

``` r
generate_active_days_by_period(
  year,
  month,
  effort,
  effort_source = c("fisher_interview", "boat_counting"),
  landings,
  active_vessels = NULL,
  minor_strata = NULL
)
```

## Arguments

- year:

  year

- month:

  month

- effort:

  effort table

- effort_source:

  effort source

- landings:

  landings table

- active_vessels:

  active vessels table

- minor_strata:

  minor strata. Default is `NULL`

## Value

an object of class tibble give active days
