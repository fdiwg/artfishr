#'@name compute_effort_estimate
#'@title Computes nominal effort estimate
#'@export
compute_effort_estimate = function(
    effort, effort_source = c("fisher_interview", "boat_counting", "household_interview"), 
    landings, 
    active_days = NULL,
    active_vessels = NULL, active_vessels_strategy = c("latest", "closest"),
    census_typology = NULL,
    minor_strata = NULL,
    progress_fn = NULL
){
  
  effort_source = match.arg(effort_source)
  #arg validation
  switch(effort_source,
    "fisher_interview" = {
      if(is.null(active_vessels)) stop("Effort source 'fisher_interview' requires an 'active_vessels' dataset")
    },
    "boat_counting" = {
      if(is.null(active_vessels)) stop("Effort source 'boat_counting' requires an 'active_vessels' dataset")
    },
    "household_interview" = {
      if(is.null(census_typology)) stop("Effort source 'household_interview' requires a 'census_typology' dataset")
    }
  )
  
  effort_estimate = if(effort_source %in% c("fisher_interview", "boat_counting")){
    compute_effort_estimate_with_FAO_Artfish(
      effort = effort, 
      effort_source = effort_source, 
      landings = landings,
      active_days = active_days,
      active_vessels = active_vessels, 
      active_vessels_strategy = active_vessels_strategy,
      minor_strata = minor_strata,
      progress_fn = progress_fn
    )
  }else if(effort_source == "household_interview"){
    compute_effort_estimate_with_IRD_Pechart(
      effort = effort,
      effort_source = effort_source,
      landings = landings,
      active_days = active_days,
      census_typology = census_typology,
      minor_strata = minor_strata
    )
  }
  return(effort_estimate)
}