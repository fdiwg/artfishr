#'@name compute_catch_estimate
#'@title Computes catch estimate
#'@param effort_estimate effort estimate computed with \link{compute_effort_estimate}
#'@param cpue cpue
#'@return a \link{tibble} giving the estimated catch by strata
#'@export
compute_catch_estimate = function(effort_estimate, cpue){
  cpue$catch_nominal_landed = NULL
  cpue$effort_fishing_duration = NULL
  out = effort_estimate %>%
    dplyr::left_join(cpue)
  out$catch_nominal = out$effort_nominal * out$catch_cpue
  out$effort_activity_coefficient = NULL
  out$effort_fishable_duration = NULL
  out$fleet_engagement_number = NULL
  return(out)
}