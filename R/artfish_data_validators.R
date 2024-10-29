#'@name set_vrule_validators
#'@title Set data validators powered by \pkg{vrule}
#'@export
set_vrule_validators = function(){
  format_spec_files = list.files(file.path(system.file(package = "artfishr"), "extdata/format_specs"), full.names = T)
  .artfish$format_specs = lapply(format_spec_files, function(x){
    vrule::format_spec$new(json = jsonlite::read_json(x))
  })
  names(.artfish$format_specs) = sapply(.artfish$format_specs, function(x){x$name})
}

#'@name get_vrule_validators
#'@title Get data validators powered by \pkg{vrule}
#'@return the list of internal data validators as objects of class \link{format_spec}
#'@export
get_vrule_validators = function(){
  return(.artfish$format_specs)
}