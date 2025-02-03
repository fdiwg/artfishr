#'@name compute_cpue
#'@title Computes CPUE
#'@param landings landings
#'@param minor_strata minor_strata
#'@return a \link{tibble} giving CPUE per strata
#'@export
compute_cpue = function(landings, minor_strata = NULL){
  
  if(any(is.na(landings$catch_nominal_landed))){
    #TODO warnings here to be reported (to investigate how)
    landings = subset(landings, !is.na(catch_nominal_landed))
  }
  if(any(is.na(landings$effort_fishing_duration))){
    #TODO warnings here to be reported (to investigate how)
    landings = subset(landings, !is.na(effort_fishing_duration))
  }
  
  strata = c("year", "month", "fishing_unit")
  if(!is.null(minor_strata)) strata = c(strata, minor_strata)
  
  out = landings %>%
    dplyr::group_by_at(c(strata, "fishing_trip", "effort_fishing_duration")) %>%
    dplyr::summarize(catch_nominal_landed = sum(catch_nominal_landed, na.rm = T)) %>%
    dplyr::ungroup(fishing_trip) %>%
    dplyr::summarize(effort_fishing_duration = sum(effort_fishing_duration, na.rm = T), catch_nominal_landed = sum(catch_nominal_landed, na.rm = T))
  out$catch_cpue = out$catch_nominal_landed / out$effort_fishing_duration
  
  return(out)
}