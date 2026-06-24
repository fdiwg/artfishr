# Create an empty artfishr data template

Generates an empty data frame following one of the artfishr format
specifications.

## Usage

``` r
create_artfish_template(
  format = c("artfish_A_active_vessels", "artfish_B1_effort", "artfish_B2_effort",
    "artfish_C_active_days", "artfish_D_landings"),
  include_meta = FALSE,
  save_as = NULL
)
```

## Arguments

- format:

  Character string indicating which template to create.

- include_meta:

  Logical; if TRUE, include schema metadata.

- save_as:

  Optional file path to save the template as CSV.

## Value

A zero-row data frame matching the schema definition.

## See also

\[decode_artfish_schema()\]
