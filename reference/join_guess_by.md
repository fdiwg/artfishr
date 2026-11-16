# join_guess_by

Gives the common column names between two tables. Util to be used in
dplyr `*_join` calls `by` argument to avoid explicit messages triggered
by dplyr when guessing the columns (in case `by` is not specified) param
x a [data.frame](https://rdrr.io/r/base/data.frame.html) or
[tibble](https://tibble.tidyverse.org/reference/tibble.html) param y a
[data.frame](https://rdrr.io/r/base/data.frame.html) or
[tibble](https://tibble.tidyverse.org/reference/tibble.html)

## Usage

``` r
join_guess_by(x, y)
```
