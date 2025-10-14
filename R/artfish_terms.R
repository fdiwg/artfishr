#'@name get_fdi_terms
#'@title Select get_fdi_terms vessels
#'@description Get FDI terms for Artfish inputs/outputs
#'@export
get_fdi_terms = function(){
  readr::read_csv(system.file("extdata", "fdi_terms.csv", package = "artfishr"))
}