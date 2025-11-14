# Computes CPUE

The CPUE (Catch Per Unit of Effort) is computed from the landings, as
the ratio between nominal landed catches (`catch_nominal_landed`) and
the effort fishing duration (`effort_fishing_duration`).

The computation is performed grouped by a strata compound by `year`,
`month` and `fishing_unit`. This strata can be extended with additional
columns with the `minor_strata` argument.

Since landings give details data on landed species, nominal landed
catches are sum grouped by the strata and by fishing trip to get the
total nominal landed catches by fishing trip. Both
`catch_nominal_landed` and `effort_fishing_duration` are then sum by
strata and the CPUE is computed as the ratio of these sum.

Note: Additional checks are performed to remove data with NAs.

## Usage

``` r
compute_cpue(landings, minor_strata = NULL)
```

## Arguments

- landings:

  landings

- minor_strata:

  minor_strata. Default is `NULL`

## Value

a [tibble](https://tibble.tidyverse.org/reference/tibble.html) object
giving the CPUE by strata
