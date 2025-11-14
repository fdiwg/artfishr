# Computes effort activity coefficient

The activity coefficient is computed from the effort. Depending on the
source of effort data, the source information used is different: - In
the case of 'fisher_interview' the coefficient of activity is computed
as the ratio between the `effort_fishing_duration` and the
`effort_fishing_reference_period`. - In the case of 'boat_counting' the
coefficient of activity is computed as the ratio between the
`fleet_engagement_number` and the `fleet_engagement_max`

The computation is performed grouped by a strata compound at minimum by
the `year`, `month` and `fishing_unit`. This strata can be extended by
adding one or more columns with the `minor_strata` argument.

Note: Additional check are performed to remove data with NAs, and ensure
data consistency

## Usage

``` r
compute_effort_activity_coefficient(
  effort,
  effort_source = c("fisher_interview", "boat_counting"),
  minor_strata = NULL
)
```

## Arguments

- effort:

  effort data

- effort_source:

  effort

- minor_strata:

  minor_strata. Default is `NULL`

## Value

a [tibble](https://tibble.tidyverse.org/reference/tibble.html) object
giving activity coefficient by strata
