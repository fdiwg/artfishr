# Computes catch estimates by species

Computes catch estimates by species

## Usage

``` r
compute_catch_estimates_by_species(
  landings,
  catch_estimate,
  minor_strata = NULL
)
```

## Arguments

- landings:

  landings

- catch_estimate:

  result of catch estimate computed with
  [compute_catch_estimate](https://fdiwg.github.io/artfishr/reference/compute_catch_estimate.md)

- minor_strata:

  minor_strata. Default is `NULL`

## Value

a tibble
