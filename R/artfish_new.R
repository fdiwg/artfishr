#'@name artfish_new
#'@title artfish_new
#'
#'@param active_vessels active vessels
#'@param effort effort
#'@param effort_source effort source whether it's derived from survey -B1- (fishers interviews) 
#'or registry -B2- (boat counting)
#'@param active_days active days
#'@param landings landings
#'@param minor_strata minor_strata (to investigate further later)
#'
#'@return the result of Artfish
#'@export
#'
artfish_new <- function(
  active_vessels = NULL,
  effort = NULL,
  effort_source = c("survey", "registry"),
  active_days = NULL,
  landings = NULL, 
  minor_strata = NULL){
  
  #TODO by sur artfish_new_by_period
  

}

#'@name artfish_new_by_period
#'@title artfish_new_by_period
#'
#'@param active_vessels active vessels
#'@param effort effort
#'@param effort_source effort source whether it's derived from survey -B1- (fishers interviews) 
#'or registry -B2- (boat counting)
#'@param active_days active days
#'@param landings landings
#'@param minor_strata minor_strata (to investigate further later)
#'
#'@return the result of Artfish for a given year/month
#'@export
#'
artfish_new_by_period <- function(
  year = NULL, month = NULL,  
  active_vessels,
  effort,
  effort_source = c("survey", "registry"),
  active_days = NULL,
  landings,
  minor_strata = NULL){
  
  errors <- NULL
  validators = get_vrule_validators()
  
  #mandatory columns: year, month, (minor_stratum), fishing_unit
  
  #verify data availability
  #active_vessels NOT NULL (through arg)
  #effort NOT NULL (through arg)
  #active_days IF NULL then will need autogenerate it
  #landings NOT NULL (through arg)
  
  #validate A/B/C/D components (delegated to vrule)
  #structure (B1/B2) will depend on the effort source
  #active_vessels
  active_vessels_report = validators$cwp_rh_artfish_active_vessels$validate(active_vessels)
  if(any(active_vessels_report$type == "ERROR")){
    stop("Data 'active_vessels' validation errors")
  }
  #effort
  effort_validator = switch(effort_source,
    "survey" = validators$cwp_rh_artfish_effort_survey,
    "registry" = NULL #todo
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
  }else{
    #autogenerate active_days table
    fishing_units = unique(c(active_vessels$fishing_unit, effort$fishing_unit))
    active_days = generate_active_days(year, month, fishing_units)
  }
  
  #identify strata (that may include minor stratum)
  #-> columns that identify dimensions for grouping
  #examples
  #- year/month/fishing_unit (minimum requirement)
  #- year/month/(additional minor stratum)/fishing_unit
  
  #verify that year/month is ok on all tables (except eventually active_days IF NULL)
  #filter on these year/month (if there is more, raise a warning to alert user)
  #if active_days was NULL, then generate the equivalent table for the reference period year/month
  
  #verify that year/month/(minor_stratum)/fishing_unit are the same across all tables
  
  out <- NULL
  
  #CURRENT CODE?
  # focus on result (without any step attempts)
  
  #OBJECTIVE
  #step 1 - boat activity 
  # bac_out = artfishr::compute_bac(...)
  # #step 2 - effort
  # effort_out = artfishr::compute_effort(...)
  # #step 3 - cpu (catch)
  # cpu_out = artfishr::compute_cpu(...)
  # #=> result should be the same
  # out <- list(
  #   bac = data.frame(),
  #   effort = data.frame(),
  #   cpu = data.frame()
  # )
  # if(nrow(errors)>0){
  #   attr(out, "errors") <- errors
  # }
  return(out)
}

