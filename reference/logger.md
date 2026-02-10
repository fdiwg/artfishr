# util generic logger

Generic logger

## Usage

``` r
logger(type = c("INFO", "WARN", "ERROR"), txt, ...)
```

## Arguments

- type:

  type either INFO, WARN or DEBUG

- txt:

  a character vector of format strings, each of up to 8192 bytes.

- ...:

  any values to be passed into `txt`. See
  [sprintf](https://rdrr.io/r/base/sprintf.html)
