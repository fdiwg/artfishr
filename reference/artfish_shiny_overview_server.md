# ARTFISH Overview - Shiny Server Module

Server-side logic for the ARTFISH Overview dashboard.

The \*"Total catch and effort - Statistics at country level"\* module
provides a high-level visualization of key output metrics derived from
`artfish_compute_report`. It acts as a dashboard to track the overall
evolution of fishing activity over time, with the ability to filter by
fishing units and time range.

The module is composed of:

- A selector panel to adjust the time range and fishing units to display

- A set of KPIs presenting aggregated country-level metrics for the
  selected period

- Seven boxed plots powered by `fdishinyr::generic_chart` allowing users
  to analyse:

  - \(1\) Total catch estimation

  - \(2\) Species composition (by quantity)

  - \(3\) Total nominal effort

  - \(4\) Number of active vessels

  - \(5\) Total value estimation

  - \(6\) Species composition (by value)

  - \(7\) Catch per unit effort (CPUE)

## Usage

``` r
artfish_shiny_overview_server(
  id,
  lang = NULL,
  estimate,
  effort_source = NULL,
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

  Reactive Character string indicating the type of effort source. Must
  be either `"fisher_interview"` or `"boat_counting"`. Not activated

- minor_strata:

  Reactive Character string targeting a column name considered as minor
  strata. Not activated

- opts:

  a named list of options. For now only supports the `refresh_ui` that
  gives the capacity to inject a refresh UI button (for dynamic
  computation)
