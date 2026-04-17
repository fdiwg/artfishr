# ARTFISH Species - Shiny Server Module

Server-side logic for the ARTFISH Species dashboard.

The \*"Catch and effort - Detailed by species"\* module provides a
species-oriented workspace for exploring key output metrics derived from
`artfish_compute_report`, with a focus on individual species caught.

The initial view displays a selector inviting the user to choose a
species. The dropdown lists all species for which entries are available
in the database. Users can then analyse the evolution of fishing
activity over time, with filtering options for species, fishing units,
and time range.

The module is composed of:

- A selector panel to choose the species to display

- A selector panel to adjust the time range and fishing units to display

- Two boxed plots powered by `fdishinyr::generic_chart` allowing users
  to analyse, at species level:

  - \(1\) Fishing unit composition (by quantity)

  - \(2\) Target species ranking (by quantity)

- A set of KPIs presenting aggregated metrics for the selected period

- Five boxed plots powered by `fdishinyr::generic_chart` allowing users
  to analyse key Artfish metrics:

  - \(1\) Total catch estimation

  - \(2\) Catch per unit effort (CPUE)

  - \(3\) Total value estimation

  - \(4\) Average price

  - \(5\) Total nominal effort

## Usage

``` r
artfish_shiny_species_server(
  id,
  lang = NULL,
  estimate,
  effort_source = NULL,
  minor_strata = NULL
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

  Reactive character string indicating the type of effort source. Must
  be either `"fisher_interview"` or `"boat_counting"`. Not activated

- minor_strata:

  Reactive character string targeting a column name considered as minor
  strata. Not activated
