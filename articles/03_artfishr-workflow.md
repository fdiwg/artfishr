# Using artfishr for Small-Scale Fisheries Estimation

## Introduction

This vignette demonstrates how to apply the **ARTFISH methodology** for
estimating fisheries catch and effort, and related indicators using the
`artfishr` R package. This methodology aims to extrapolate sample-based
observations to produce total estimates for each stratum of the sample.

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

The Following datasets are required to produce the estimates: - **Active
vessels:** number of active vessels per stratum (or per landing site) -
**Active days:** number of days in the month recording a fishing
activity - **Effort survey data:** fisher interviews or boat counting -
**Landings data:** catch by species for each observed fishing trip For
more information on the data structure, refer to the example datasets
and to the data validation requirements.

## Identify data collection strategy

(to complete with different cases + schema)

Different cases are listed above, depending on the different data
sources. The effort survey type (fisher interview or boat counting) must
be indicated in the function to use the correct computation. In case of
boat counting, the dataset active_days is mandatory.

## Step-by-step ARTFISH workflow

Below is the detailed workflow showing each function used by `artfishr`
to compute the various ARTFISH components.

Indicators are computed for each stratum of the sampling plan. Major and
minor strata must be specified in the function to define the level of
aggregation.

### 1. Effort Activity Coefficient

The activity coefficient represents the probability that a certain boat
is out on a certain day. It is obtained with the effort survey using
either fisher interviews or boat counts.

``` r

activity_coefficient <- artfishr::compute_effort_activity_coefficient(
  effort = effort,
  effort_source = "fisher_interview",
  minor_strata = "minor_stratum"
)
```

### 2. Effort Estimate

Effort estimate calculation is based on the three components active
vessels, active days and activity coefficient.

``` math
Effort =
ActiveVessels \times
ActiveDays \times
ActivityCoefficient
```

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

CPUE is calculated with the landing survey data. For each stratum, the
overall average CPUE is calculated.

``` r

cpue <- artfishr::compute_cpue(
  landings,
  minor_strata = "minor_stratum"
)
```

### 4. Catch Estimate

Catch estimate calculation is based on the formula:

``` math
Catch = CPUE \times Effort
```

``` r

catch_estimate <- artfishr::compute_catch_estimate(
  effort_estimate,
  landings = landings,
  minor_strata = "minor_stratum"
)
```

### 5. Catch Estimate by Species

Catch estimates by species are obtained by distributing the estimated
total catch according to the observed species composition within each
stratum.

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

head(effort_estimate)
#> # A tibble: 6 × 11
#>    year month fishing_unit            minor_stratum effort_sample_size
#>   <dbl> <dbl> <chr>                           <dbl>              <int>
#> 1  2025     1 Artisanal 0-6m trap                 1                 20
#> 2  2025     1 Artisanal 0-6m trap                 2                 19
#> 3  2025     1 Artisanal 6-12m gillnet             1                 12
#> 4  2025     1 Artisanal 6-12m gillnet             2                 49
#> 5  2025     1 Artisanal 6-12m trawl               1                 25
#> 6  2025     1 Artisanal 6-12m trawl               2                 39
#> # ℹ 6 more variables: effort_coefficient_variation <dbl>,
#> #   effort_total_fishing_duration <dbl>, effort_activity_coefficient <dbl>,
#> #   fleet_engagement_number <dbl>, effort_fishable_duration <int>,
#> #   effort_nominal <dbl>
head(catch_estimate_by_species)
#> # A tibble: 6 × 18
#>    year month fishing_unit minor_stratum species catch_number effort_sample_size
#>   <dbl> <dbl> <chr>                <dbl> <chr>          <dbl>              <int>
#> 1  2025     1 Artisanal 0…             1 Crab             248                 20
#> 2  2025     1 Artisanal 0…             1 Grouper          485                 20
#> 3  2025     1 Artisanal 0…             1 Lobster          630                 20
#> 4  2025     1 Artisanal 0…             1 Parrot…          902                 20
#> 5  2025     1 Artisanal 0…             1 Snapper          534                 20
#> 6  2025     1 Artisanal 0…             2 Crab             268                 19
#> # ℹ 11 more variables: effort_coefficient_variation <dbl>,
#> #   effort_total_fishing_duration <dbl>, catch_sample_size <int>,
#> #   catch_coefficient_variation <dbl>, catch_cpue <dbl>,
#> #   catch_number_species <int>, catch_species_ratio <dbl>,
#> #   catch_nominal_landed <dbl>, trade_price <dbl>, trade_value <dbl>,
#> #   catch_fish_average_weight <dbl>
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
