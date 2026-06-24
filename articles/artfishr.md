# Introduction to artfishr package

This vignette shows how to use
[artfishr](https://github.com/fdiwg/artfishr) R package.

## Installation

The R [artfishr](https://github.com/fdiwg/artfishr) can be installed
from Github.

As prequirements, packages `remotes` and `vrule` should be installed.

- Package `remotes` can be installed from CRAN using:

``` r

install.packages("remotes", repos = "https://cloud.r-project.org")
```

- Package [vrule](https://github.com/fdiwg/vrule) should be installed
  from GitHub using `remotes`:

``` r

remotes::install_github("fdiwg/vrule")
```

Once packages `remotes` and `vrule`, R package `artfishr` can be
installed using:

``` r

remotes::install_github("fdiwg/artfishr")
```

Once installed, `artfishr` can be loaded using
[`library(artfishr)`](https://github.com/fdiwg/artfishr) or
\`require(artfishr)\`\`

## Data requirements

See the full guide:
[`vignette("02_data-requirements-validation")`](https://fdiwg.github.io/artfishr/articles/02_data-requirements-validation.md)

## How to run Artfish with `artfishr`

Full tutorial:
[`vignette("03_artfishr-workflow")`](https://fdiwg.github.io/artfishr/articles/03_artfishr-workflow.md)

## The Artfish methodology - summary

See:
[`vignette("04_artfish-methodology")`](https://fdiwg.github.io/artfishr/articles/04_artfish-methodology.md)

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
head(report)
```
