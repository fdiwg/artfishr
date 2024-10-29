#'@name compute_catch_estimate
#'@title Computes catch estimate
#'@param effort_estimate effort estimate computed with \link{compute_effort_estimate}
#'@param cpue cpue
#'@return a \link{tibble} giving the estimated catch by strata
#'@export
compute_catch_estimate = function(effort_estimate, cpue){
  out = effort_estimate %>%
    dplyr::left_join(cpue)
  out$catch_estimate = out$effort_estimate * out$cpue
  return(out)
}