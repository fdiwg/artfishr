#'@name compute_effort_estimate
#'@title Computes effort estimate
#'@param active_vessels active vessels
#'@param effort effort data
#'@param active_days active_days
#'@export
compute_effort_estimate = function(
    active_vessels, effort, active_days
){
  
  #complete active days by eventually filling missing or zero values for 'effort_fishable_duration'
  active_days = complete_active_days(active_days)
  
  #compute effort activity coefficient
  AC = compute_effort_activity_coefficient(effort = effort)
  
  dt = AC %>% 
    dplyr::left_join(y = active_vessels) %>% 
    dplyr::left_join(y = active_days)
  dt$effort_estimate = dt$fleet_engagement_number * dt$effort_fishable_duration * dt$effort_activity_coefficient
  
  return(dt)
}