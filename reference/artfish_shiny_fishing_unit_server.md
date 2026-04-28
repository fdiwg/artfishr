# ARTFISH Fishing Unit - Shiny Server Module

Server-side logic for the ARTFISH Fishing Unit dashboard.

The \*"Catch and effort - Detailed by fishing unit"\* module provides a
dedicated workspace for exploring key output metrics derived from
`artfish_compute_report`, with a focus on one or multiple fishing units.
It enables users to analyse the evolution of fishing activity over time,
with filtering options for fishing units and time range.

The module is composed of:

- A selector panel to adjust the time range and fishing units to display

- A set of KPIs presenting aggregated metrics for the selected period

- Three boxed plots powered by `fdishinyr::generic_chart` allowing users
  to analyse ranked top items for:

  - \(1\) Species composition (by quantity)

  - \(2\) Fishing unit composition (by quantity)

  - \(3\) Fishing unit composition (by value)

- Six boxed plots powered by `fdishinyr::generic_chart` allowing users
  to analyse key Artfish metrics:

  - \(1\) Total catch estimation

  - \(2\) Catch per unit effort (CPUE)

  - \(3\) Total nominal effort

  - \(4\) Activity coefficient

  - \(5\) Number of active vessels

  - \(6\) Total value estimation

## Usage

``` r
artfish_shiny_fishing_unit_server(
  id,
  lang = NULL,
  estimate,
  effort_source,
  minor_strata = NULL,
  opts = list()
)
```

## Arguments

- id:

  Character string. Module namespace identifier.

- lang:

  Optional language parameter. Can be either:

  - a character string (static language), or

  - a reactive returning a character string (dynamic language)

  If `NULL`, the current global language is used. Default is `NULL`

- estimate:

  A reactive data frame aggregating the output of `artfish_compute`,
  enriched with human-readable labels.

- effort_source:

  Reactive haracter string indicating the type of effort source. Must be
  either `"fisher_interview"` or `"boat_counting"`.

- minor_strata:

  Reactive haracter string targeting a column name considered as minor
  strata. Not activated

- opts:

  a named list of options. For now limited to: - `refresh_ui` that gives
  the capacity to inject a refresh UI button (for dynamic computation) -
  `values_ui` that allows to hide the UI related to value measurement
