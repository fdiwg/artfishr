#'@name compute_effort_estimate
#'@title Computes effort estimate
#'@param active_vessels active vessels
#'@param effort effort data
#'@param effort_source effort_source (register_interview / boat_counting)
#'@param active_days active_days
#'@param minor_strata minor_strata
#'@export
compute_effort_estimate = function(
    active_vessels, effort, effort_source, active_days, minor_strata = NULL
){
  
  strata = c("year", "month", "fishing_unit")
  if(!is.null(minor_strata)) strata = c(strata, minor_strata)
  
  #complete active days by eventually filling missing or zero values for 'effort_fishable_duration'
  active_days = complete_active_days(active_days)
  
  #compute effort activity coefficient
  AC = compute_effort_activity_coefficient(effort = effort, effort_source = effort_source, minor_strata = minor_strata)
  
  if(!"month" %in% colnames(active_vessels)) strata = strata[-which(strata == "month")]
  if(!"year" %in% colnames(active_vessels)) strata = strata[-which(strata == "year")]
  active_vessels_by_strata = active_vessels %>%
    dplyr::group_by_at(strata) %>%
    dplyr::summarize(fleet_engagement_number = sum(fleet_engagement_number))
  
  dt = AC %>% 
    dplyr::left_join(y = active_vessels_by_strata) %>% 
    dplyr::left_join(y = active_days)
  dt$effort_estimate = dt$fleet_engagement_number * dt$effort_fishable_duration * dt$effort_activity_coefficient
  
  return(dt)
}