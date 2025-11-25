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
#'@param effort_source effort source whether it's derived from -B1- (fishers interviews) 
#'or -B2- (boat counting)
#'@param active_days active days
#'@param landings landings
#'@export
validate_input_datasets <- function(
    active_vessels,
    effort,
    effort_source = c("fisher_interview", "boat_counting"),
    active_days = NULL,
    landings){
  errors <- NULL
  validators = get_vrule_validators()
  #structure (B1/B2) will depend on the effort source
  #active_vessels
  active_vessels_report = validators$cwp_rh_artfish_active_vessels$validate(active_vessels)
  #effort
  effort_validator = switch(effort_source,
                            "fisher_interview" = validators$cwp_rh_artfish_effort_fisher_interview,
                            "boat_counting" = validators$cwp_rh_artfish_effort_boat_counting
  )
  effort_report = effort_validator$validate(effort)
  #landings
  landings_report = validators$cwp_rh_artfish_landings$validate(landings)
  
  #overall report
  qa_report = rbind(active_vessels_report, effort_report, landings_report)
  
  #active_days
  if(!is.null(active_days)){
    active_days_report = validators$cwp_rh_artfish_active_days$validate(active_days)
    qa_report = rbind(qa_report, active_days_report)
  }
  return(qa_report)
}

#' @name validate_artfish_datasets
#' @title Wrapper functions for validating artfishr input data
#' @description Family of functions to validate empty input dataset.
#' @details These functions are convenience wrappers around [validate_input_datasets()].
#' @seealso [validate_input_datasets()]
NULL


#' @rdname validate_active_vessels
#' @export
validate_active_vessels <- function(data) {
  validators = get_vrule_validators()
  validation_report = validators$cwp_rh_artfish_active_vessels$validate(data)
  return(validation_report)
}

#' @rdname validate_active_days
#' @export
validate_active_days <- function(data) {
  validators = get_vrule_validators()
  validation_report = validators$cwp_rh_artfish_active_days$validate(data)
  return(validation_report)
}

#' @rdname validate_landings
#' @export
validate_landings <- function(data) {
  validators = get_vrule_validators()
  validation_report = validators$cwp_rh_artfish_landings$validate(data)
  return(validation_report)
}

#' @rdname validate_effort
#' @export
validate_effort <- function(data,effort_source = c("boat_counting", "fisher_interview")) {
  validators = get_vrule_validators()
  validation_report = switch(effort_source,
                             "fisher_interview" = validators$cwp_rh_artfish_effort_fisher_interview,
                             "boat_counting" = validators$cwp_rh_artfish_effort_boat_counting
  )
  return(validation_report)
}