#' @name decode_artfish_schema
#' @title Decode an artfishr JSON schema
#' @description Reads a JSON format specification (as used in \code{inst/extdata/format_specs}) 
#' and returns an empty data frame with the appropriate column names.
#' @param json_path Path to the JSON specification file.
#' @param include_meta Logical; if TRUE, attaches metadata (e.g. name, title, urn) as attributes to each column. Default is FALSE.
#' @return A \code{data.frame} with zero rows and the columns defined in the schema.
#' @export
decode_artfish_schema <- function(json_path, include_meta = FALSE) {
  if (!file.exists(json_path)) stop("File not found: ", json_path, call. = FALSE)
  
  spec <- jsonlite::fromJSON(json_path)
  if (is.null(spec$column_specs) || !is.data.frame(spec$column_specs))
    stop("Invalid schema: missing 'column_specs'.", call. = FALSE)
  
  col_names <- spec$column_specs$name
  df <- as.data.frame(
    setNames(replicate(length(col_names), character(0), simplify = FALSE), col_names),
    stringsAsFactors = FALSE
  )
  
  if (isTRUE(include_meta)) {
    for (i in seq_along(col_names)) {
      col <- col_names[i]
      meta <- as.list(spec$column_specs[i, ])
      for (attr_name in names(meta)) attr(df[[col]], attr_name) <- meta[[attr_name]]
    }
    attr(df, "schema_name")  <- spec$name
    attr(df, "schema_title") <- spec$title
    attr(df, "schema_urn")   <- spec$urn
  }
  
  return(df)
}


#' @name create_artfish_template
#' @title Create an empty artfishr data template
#' @description Generates an empty data frame following one of the artfishr format specifications.
#' @param format Character string indicating which template to create.
#' @param include_meta Logical; if TRUE, include schema metadata.
#' @param save_as Optional file path to save the template as CSV.
#' @return A zero-row data frame matching the schema definition.
#' @seealso [decode_artfish_schema()]
#' @export
create_artfish_template <- function(
  format = c("artfish_A_active_vessels", "artfish_B1_effort", "artfish_B2_effort",
             "artfish_C_active_days", "artfish_D_landings"),
  include_meta = FALSE,
  save_as = NULL) {
  
  # format <- match.arg(format)
  path <- system.file("extdata/format_specs", paste0(format, ".json"), package = "artfishr")
  if (path == "") stop("Specification not found for format: '", format, "'.", call. = FALSE)
  
  df <- decode_artfish_schema(path, include_meta = include_meta)
  
  if (!is.null(save_as)) {
    dir.create(dirname(save_as), showWarnings = FALSE, recursive = TRUE)
    utils::write.csv(df, save_as, row.names = FALSE)
    message("Template saved to: ", normalizePath(save_as))
  }
  
  return(df)
}


#' @name create_artfish_templates
#' @title Wrapper functions for creating ArtFishR templates
#' @description Family of functions to create empty templates for artfishr dataset types.
#' @details These functions are convenience wrappers around [create_artfish_template()].
#' @seealso [create_artfish_template()], [decode_artfish_schema()]
NULL


#' @rdname create_artfish_templates
#' @export
create_active_vessels_template <- function(include_meta = FALSE, save_as = NULL) {
  create_artfish_template("artfish_A_active_vessels", include_meta = include_meta, save_as = save_as)
}

#' @rdname create_artfish_templates
#' @export
create_active_days_template <- function(include_meta = FALSE, save_as = NULL) {
  create_artfish_template("artfish_C_active_days", include_meta = include_meta, save_as = save_as)
}

#' @rdname create_artfish_templates
#' @export
create_landings_template <- function(include_meta = FALSE, save_as = NULL) {
  create_artfish_template("artfish_D_landings", include_meta = include_meta, save_as = save_as)
}

#' @rdname create_artfish_templates
#' @export
create_effort_template <- function(effort_source = c("boat_counting", "fisher_interview"),
                                   include_meta = FALSE, save_as = NULL) {
  effort_source <- match.arg(effort_source)
  format <- switch(effort_source,
                   "boat_counting"   = "artfish_B1_effort",
                   "fisher_interview" = "artfish_B2_effort")
  create_artfish_template(format, include_meta = include_meta, save_as = save_as)
}