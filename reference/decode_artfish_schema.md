# Decode an artfishr JSON schema

Reads a JSON format specification (as used in
`inst/extdata/format_specs`) and returns an empty data frame with the
appropriate column names.

## Usage

``` r
decode_artfish_schema(json_path, include_meta = FALSE)
```

## Arguments

- json_path:

  Path to the JSON specification file.

- include_meta:

  Logical; if TRUE, attaches metadata (e.g. name, title, urn) as
  attributes to each column. Default is FALSE.

## Value

A `data.frame` with zero rows and the columns defined in the schema.
