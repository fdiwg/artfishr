#'@name compute_cpue
#'@title Computes CPUE
#'@param landings landings
#'@return a \link{tibble} giving CPUE per strata
#'@export
compute_cpue = function(landings){
  
  if(any(is.na(landings$catch_nominal_landed))){
    #TODO warnings here to be reported (to investigate how)
    landings = subset(landings, !is.na(catch_nominal_landed))
  }
  if(any(is.na(landings$effort_fishing_duration))){
    #TODO warnings here to be reported (to investigate how)
    landings = subset(landings, !is.na(effort_fishing_duration))
  }
  
  out = landings %>%
    dplyr::group_by(year, month, fishing_unit, fishing_trip, effort_fishing_duration) %>%
    dplyr::summarize(catch_nominal_landed = sum(catch_nominal_landed, na.rm = T)) %>%
    dplyr::ungroup(fishing_trip) %>%
    dplyr::summarize(sum_effort_fishing_duration = sum(effort_fishing_duration, na.rm = T), sum_catch_nominal_landed = sum(catch_nominal_landed, na.rm = T))
  out$cpue = out$sum_catch_nominal_landed / out$sum_effort_fishing_duration
  
  return(out)
}