# Introduction to artfishr

This vignette shows how to use
[artfishr](https://github.com/fdiwg/artfishr).

## Installation

The R [artfishr](https://github.com/fdiwg/artfishr) can be installed
from Github.

As prequirements, packages `remotes` and `vrule` should be installed.

- Package `remotes` can be installed from CRAN using:

``` r
install.packages("remotes", repos = "https://cloud.r-project.org")
```

    ## Installing package into '/home/runner/work/_temp/Library'
    ## (as 'lib' is unspecified)

- Package [vrule](https://github.com/fdiwg/vrule) should be installed
  from GitHub using `remotes`:

``` r
remotes::install_github("fdiwg/vrule")
```

    ## Using github PAT from envvar GITHUB_PAT. Use `gitcreds::gitcreds_set()` and unset GITHUB_PAT in .Renviron (or elsewhere) if you want to use the more secure git credential store instead.

    ## Skipping install of 'vrule' from a github remote, the SHA1 (3f4b5500) has not changed since last install.
    ##   Use `force = TRUE` to force installation

Once packages `remotes` and `vrule`, R package `artfishr` can be
installed using:

``` r
remotes::install_github("fdiwg/artfishr")
```

    ## Using github PAT from envvar GITHUB_PAT. Use `gitcreds::gitcreds_set()` and unset GITHUB_PAT in .Renviron (or elsewhere) if you want to use the more secure git credential store instead.

    ## Downloading GitHub repo fdiwg/artfishr@HEAD

    ## cpp11 (0.5.2 -> 0.5.3) [CRAN]
    ## vroom (1.6.7 -> 1.7.0) [CRAN]

    ## Installing 2 packages: cpp11, vroom

    ## Installing packages into '/home/runner/work/_temp/Library'
    ## (as 'lib' is unspecified)

    ## ── R CMD build ─────────────────────────────────────────────────────────────────
    ## * checking for file ‘/tmp/RtmpcyS3vR/remotes1ddc2c1ae1a/fdiwg-artfishr-a9b531e/DESCRIPTION’ ... OK
    ## * preparing ‘artfishr’:
    ## * checking DESCRIPTION meta-information ... OK
    ## * checking for LF line-endings in source and make files and shell scripts
    ## * checking for empty or unneeded directories
    ## * building ‘artfishr_0.1.20260128.tar.gz’

    ## Installing package into '/home/runner/work/_temp/Library'
    ## (as 'lib' is unspecified)

Once installed, `artfishr` can be loaded using
[`library(artfishr)`](https://github.com/fdiwg/artfishr) or
\`require(artfishr)\`\`

## Data requirements

TODO

## The Artfish methodology - summary

TODO

## How to run Artfish with `artfishr`

TODO
