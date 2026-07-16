# Introduction to artfishr package

This vignette provides a general introduction to the
[artfishr](https://github.com/fdiwg/artfishr) package and guides users
through the available documentation.

## Installation

The R [artfishr](https://github.com/fdiwg/artfishr) can be installed
from **Github**.

As prerequisites, packages `remotes` and `vrule` should be installed.

- Package `remotes` can be installed from CRAN using:

``` r

install.packages("remotes", repos = "https://cloud.r-project.org")
```

- Package [vrule](https://github.com/fdiwg/vrule) should be installed
  from GitHub using `remotes`:

``` r

remotes::install_github("fdiwg/vrule")
```

Once the `remotes` and `vrule` packages have been installed, the
`artfishr` package can be installed using:

``` r

remotes::install_github("fdiwg/artfishr")
```

Once installed, `artfishr` can be loaded using
[`library(artfishr)`](https://github.com/fdiwg/artfishr) or
[`require(artfishr)`](https://github.com/fdiwg/artfishr)

## Data Requirements and Validation

For detailed information on the required input datasets and validation
procedures, see:
[`vignette("02_data-requirements-validation")`](https://fdiwg.github.io/artfishr/articles/02_data-requirements-validation.md)

## Using `artfishr` for Small-Scale Fisheries Estimation

Full tutorial:
[`vignette("03_artfishr-workflow")`](https://fdiwg.github.io/artfishr/articles/03_artfishr-workflow.md)

## Statistical framework of the ARTFISH methodology

See:
[`vignette("04_artfish-methodology")`](https://fdiwg.github.io/artfishr/articles/04_artfish-methodology.md)

## Artfish visualisations

See:
[`vignette("05_artfish-visualisations")`](https://fdiwg.github.io/artfishr/articles/05_artfish-visualisations.md)

## Exemple of use

``` r

# Load sample datasets
active_vessels <- read.csv(system.file("extdata/samples/active_vessels.csv", package = "artfishr"))
effort <- read.csv(system.file("extdata/samples/effort.csv", package = "artfishr"))
landings <- read.csv(system.file("extdata/samples/landings.csv", package = "artfishr"))
active_days <- read.csv(system.file("extdata/samples/active_days.csv", package = "artfishr"))

# Validate data 
validate_input_datasets(
  active_vessels = active_vessels,
  effort = effort,
  effort_source = "fisher_interview",
  landings = landings,
  active_days = active_days
)

#Run artfish
report <- artfishr::compute_report(
  active_vessels = active_vessels,
  effort = effort,
  effort_source = "fisher_interview",
  active_days = active_days,
  active_vessels_strategy = "closest",
  landings = landings,
  minor_strata = "minor_stratum"
)

#See result
str(report, max.level = 1)
```
