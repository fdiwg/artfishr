# Wrapper functions for creating ArtFishR templates

Family of functions to create empty templates for artfishr dataset
types.

## Usage

``` r
create_active_vessels_template(include_meta = FALSE, save_as = NULL)

create_active_days_template(include_meta = FALSE, save_as = NULL)

create_landings_template(include_meta = FALSE, save_as = NULL)

create_effort_template(
  effort_source = c("boat_counting", "fisher_interview"),
  include_meta = FALSE,
  save_as = NULL
)
```

## Details

These functions are convenience wrappers around
\[create_artfish_template()\].

## See also

\[create_artfish_template()\], \[decode_artfish_schema()\]
