# Computes catch estimate

Computes catch estimate

## Usage

``` r
compute_catch_estimate(effort_estimate, landings, minor_strata = NULL)
```

## Arguments

- effort_estimate:

  effort estimate computed with
  [compute_effort_estimate](https://fdiwg.github.io/artfishr/reference/compute_effort_estimate.md)

- landings:

  landings

- minor_strata:

  minor_strata. Default is `NULL`

## Value

a tibble giving the estimated catch by strata
