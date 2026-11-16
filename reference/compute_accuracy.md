# Computes Accuracy

Compute spatial and temporal accuracy for activity coefficient and cpue.
Overall accuracy return the minimal value of the 4 accuracy indicators.

## Usage

``` r
compute_accuracy(
  activity_coefficient,
  effort_estimate,
  cpue,
  sui,
  minor_strata = NULL
)
```

## Arguments

- activity_coefficient:

  activity_coefficient

- effort_estimate:

  effort_estimate

- cpue:

  cpue

- minor_strata:

  minor_strata. Default is `NULL`

## Value

a [tibble](https://tibble.tidyverse.org/reference/tibble.html) object
giving the different accuracy by strata
