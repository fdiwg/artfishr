# ARTFISH Report - Shiny Server Module

Server-side logic for the ARTFISH Report dashboard.

The \*"Detailed results of Artfish computation – by stratum"\* module
provides an ergonomic view of key intermediate results and calculation
steps underlying the Artfish algorithm for a given temporal unit.

The initial view displays a selector inviting the user to choose a year.
If no year is available, the user must first compute at least one
indicator using `artfish_compute_report`. Successive dropdown lists then
allow the user to select a month and a fishing unit before submitting
the selection. A complete report is displayed on the right-hand side.

The module is composed of:

- A selector panel to choose the report to display by selecting year,
  month, and fishing unit

- A button to validate the selection

- A set of KPIs presenting the report status and the type of effort
  source

- A gauge chart presenting the overall accuracy quality of the indicator

- An ergonomic report view including intermediate steps and final
  indicators for the estimation of:

  - \(1\) Effort

  - \(2\) Landings

  - \(3\) Estimated catch by species

## Usage

``` r
artfish_shiny_report_server(
  id,
  lang = NULL,
  estimate,
  effort_source,
  minor_strata
)
```

## Arguments

- id:

  Character string. Module namespace identifier.

- lang:

  Optional language parameter. Can be either:

  - A character string (static language), or

  - A reactive expression returning a character string (dynamic
    language)

  If `NULL`, the current global language is used. Default is `NULL`.

- estimate:

  A data frame aggregating the output of `artfish_compute`, enriched
  with human-readable labels.

- effort_source:

  Character string indicating the type of effort source. Must be either
  `"fisher_interview"` or `"boat_counting"`.

- minor_strata:

  Character string targeting a column name considered as minor strata.
