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

#'@name validate_input_datasets
#'@title Validate input datasets
#'@param active_vessels active vessels
#'@param effort effort
#'@param effort_source effort source whether it's derived from survey -B1- (fishers interviews) 
#'or registry -B2- (boat counting)
#'@param active_days active days
#'@param landings landings
#'@export
validate_input_datasets <- function(
    active_vessels,
    effort,
    effort_source = c("survey", "registry"),
    active_days = NULL,
    landings){
  errors <- NULL
  validators = get_vrule_validators()
  #structure (B1/B2) will depend on the effort source
  #active_vessels
  active_vessels_report = validators$cwp_rh_artfish_active_vessels$validate(active_vessels)
  if(any(active_vessels_report$type == "ERROR")){
    stop("Data 'active_vessels' validation errors")
  }
  #effort
  effort_validator = switch(effort_source,
                            "survey" = validators$cwp_rh_artfish_effort_survey,
                            "registry" = validators$cwp_rh_artfish_effort_registry
  )
  effort_report = effort_validator$validate(effort)
  if(any(effort_report$type == "ERROR")){
    stop("Data 'effort' validation errors")
  }
  #landings
  landings_report = validators$cwp_rh_artfish_landings$validate(landings)
  if(any(landings_report$type == "ERROR")){
    stop("Data 'landings' validation errors")
  }
  #active_days
  if(!is.null(active_days)){
    active_days_report = validators$cwp_rh_artfish_active_days$validate(active_days)
    if(any(active_days_report$type == "ERROR")){
      stop("Data 'active_days' validation errors")
    }
  }
}