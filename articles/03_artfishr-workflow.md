# Using ArtFishR for Small-Scale Fisheries Estimation

## Introduction

This vignette demonstrates how to apply the **ARTFISH methodology** for
estimating fisheries catch and effort, and related indicators using the
`artfishr` R package. This methodology aims to extrapolate sample-based
data to produce total estimates for each stratum of the sample.

The vignette walks through each computation step and shows how to use
the unified workflow function `artfish_compute_report()`, which
automates the full estimation process.

## Data preparation

The package includes example datasets stored under
`inst/extdata/samples/`.

Let’s load them using
[`system.file()`](https://rdrr.io/r/base/system.file.html):

``` r

active_vessels <- readr::read_csv(
  system.file("extdata/samples", "active_vessels.csv", package = "artfishr")
)
#> Rows: 12 Columns: 6
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (1): fishing_unit
#> dbl (5): year, month, minor_stratum, landing_site, fleet_engagement_number
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

effort <- readr::read_csv(
  system.file("extdata/samples", "effort.csv", package = "artfishr")
)
#> Rows: 164 Columns: 9
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (2): fishing_unit, effort_fishing_duration_unit
#> dbl (7): year, month, day, minor_stratum, landing_site, effort_fishing_durat...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

active_days <- readr::read_csv(
  system.file("extdata/samples", "active_days.csv", package = "artfishr")
)
#> Rows: 12 Columns: 6
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (1): fishing_unit
#> dbl (5): year, month, minor_stratum, landing_site, effort_fishable_duration
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

landings <- readr::read_csv(
  system.file("extdata/samples", "landings.csv", package = "artfishr")
)
#> Rows: 570 Columns: 15
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (6): fishing_trip, fishing_unit, effort_fishing_duration_unit, species, ...
#> dbl (9): year, month, day, minor_stratum, landing_site, effort_fishing_durat...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

Following data are required to produce the estimates: - Active vessels:
number of active vessels per stratum (or per landing site) - Active
days: number of days in the moth recording a fishing actity - Effort
survey data: fisher interviews or boat counting - Landings data: catch
per species for observed fishing trip For more information on the data
structure, refer to the example and to the data validation requirements.

## Identify data collection strategy

(to complete with different cases, identify different sources of
information and setting linked + point on strata)

Different cases are listed below, depending on the different data
sources. The effort survey type (fisher interview or boat counting) must
be indicated in the function to use the correct computation. In case of
boat counting, the dataset active_days is mandatory.

## Step-by-step ARTFISH workflow

Below is the detailed workflow showing each function used by `artfishr`
to compute the various ARTFISH components.

Indicators are computed for each stratum of the sampling plan. Major and
minor strata must be indicated in the function to define the level of
aggregation.

### 1. Effort Activity Coefficient

Activity coefficient represents the probability that a certain boat is
out on a certain day. It is obtained with the effort survey that can be
either fisher interview or boat counting.

``` r

activity_coefficient <- artfishr::compute_effort_activity_coefficient(
  effort = effort,
  effort_source = "fisher_interview",
  minor_strata = "minor_stratum"
)
```

### 2. Effort Estimate

Effort estimate calculation is based on the 3 components active vessel,
active days and activity coefficient. Effort = Active vessels x Active
days X Activity coefficient

``` r

effort_estimate <- artfishr::compute_effort_estimate(
  active_vessels = active_vessels,
  active_vessels_strategy = "latest",
  landings = landings,
  effort = effort,
  effort_source = "fisher_interview",
  active_days = active_days,
  minor_strata = "minor_stratum"
)
#> [1] "2025-01-01"
```

### 3. Catch per Unit of Effort (CPUE)

CPUE are calculated with the landing survey data. For each stratum, the
overall average CPUE is calculated.

``` r

cpue <- artfishr::compute_cpue(
  landings,
  minor_strata = "minor_stratum"
)
```

### 4. Catch Estimate

Catch estimate calculation is based on the formula: Catch = CPUE x
Effort.

``` r

catch_estimate <- artfishr::compute_catch_estimate(
  effort_estimate,
  landings = landings,
  minor_strata = "minor_stratum"
)
```

### 5. Catch Estimate by Species

For the calculation the catch estimate by species, the catch composition
is used to distribute the proportion of species in each stratum.

``` r

catch_estimate_by_species <- artfishr::compute_catch_estimates_by_species(
  landings,
  catch_estimate,
  minor_strata = "minor_stratum"
)
```

## Using the unified workflow

The individual steps above are integrated into a single convenience
function:  
`artfish_compute_report()`.  
This function executes the complete workflow, returning a structured
report that includes all intermediate and final results.

``` r

report <- artfishr::compute_report(
  active_vessels = active_vessels,
  effort = effort,
  effort_source = "fisher_interview",
  active_days = active_days,
  active_vessels_strategy = "closest",
  landings = landings,
  minor_strata = "minor_stratum"
)
#> [1] "2025-01-01"

# Inspect report structure
str(report, max.level = 1)
#> tibble [22 × 36] (S3: tbl_df/tbl/data.frame)
```

## Interpreting results

Each component of the output can be inspected individually:

``` r

head(report$effort_estimate)
#> Warning: Unknown or uninitialised column: `effort_estimate`.
#> NULL
head(report$catch_estimate_by_species)
#> Warning: Unknown or uninitialised column: `catch_estimate_by_species`.
#> NULL
```

## Summary

This vignette illustrated:

- The **data requirements** for ARTFISH (active vessels, effort, active
  days, landings)  
- The **sequential workflow** used to compute all indicators manually  
- The **integrated workflow** provided by `artfish_compute_report()` for
  convenience

For production use, users should adapt the workflow to their own
datasets, ensuring that data formats comply with the specifications in
`inst/extdata/format_specs/`.
