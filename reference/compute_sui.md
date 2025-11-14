# Computes sui

Compute the sufficient uniformity index for effort and catch
corresponding to the uniformity of sampling over the sampled days.

## Usage

``` r
compute_sui(effort, landings, minor_strata = NULL)
```

## Arguments

- effort:

  effort

- landings:

  landings

- minor_strata:

  minor_strata. Default is `NULL`

## Value

a [tibble](https://tibble.tidyverse.org/reference/tibble.html) object
giving the different accuracy by strata
