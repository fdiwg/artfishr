#'@name compute_report
#'@title compute_report
#'
#'@param effort effort
#'@param effort_source effort_source
#'@param active_vessels active_vessels
#'@param active_vessels_strategy active_vessels_strategy
#'@param active_days active_days
#'@param landings landings
#'@param minor_strata minor_strata
#'
#'@return the result of Artfish
#'@export
#'
compute_report <- function(
  effort,
  effort_source,
  active_vessels,
  active_vessels_strategy,
  active_days,
  landings,
  minor_strata){
  

  
  #activity coefficient
  activity_coefficient = artfishr::compute_effort_activity_coefficient(
    effort = effort,
    effort_source = effort_source,
    minor_strata = minor_strata
  )
  
  #effort estimate (includes calculation of activity coefficient)
  effort_estimate = artfishr::compute_effort_estimate(
    active_vessels = active_vessels, 
    active_vessels_strategy = active_vessels_strategy, 
    effort = effort, 
    effort_source = effort_source, 
    active_days = active_days,
    minor_strata = minor_strata
  )
  
  #cpue
  cpue = artfishr::compute_cpue(landings, minor_strata =minor_strata)
  
  #catch estimate
  catch_estimate = artfishr::compute_catch_estimate(effort_estimate, landings,minor_strata = minor_strata)
  
  sui = artfishr::compute_sui(effort, landings, minor_strata = minor_strata)
  
  accuracy = artfishr::compute_accuracy(
    activity_coefficient,
    effort_estimate,
    cpue,
    sui,
    minor_strata = minor_strata
  )
  
  #catch estimate by species
  catch_estimate_by_species = artfishr::compute_catch_estimates_by_species(landings, catch_estimate,minor_strata = minor_strata)
  
  #global report
  out<-catch_estimate_by_species%>%
    full_join(activity_coefficient)%>%
    full_join(effort_estimate)%>%
    full_join(cpue%>%select(-catch_cpue,-catch_nominal_landed,-effort_fishing_duration))%>%
    full_join(catch_estimate%>%select(-catch_cpue))%>%
    full_join(sui)%>%
    full_join(accuracy)%>%
    ungroup()
  

  
  return(out)
}