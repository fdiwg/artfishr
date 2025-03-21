#'@name compute_catch_estimate
#'@title Computes catch estimate
#'@param effort_estimate effort estimate computed with \link{compute_effort_estimate}
#'@param landings landings
#'@param minor_strata minor_strata
#'@return a \link{tibble} giving the estimated catch by strata
#'@export
compute_catch_estimate = function(effort_estimate, landings, minor_strata){
  
  strata = c("year", "month", "fishing_unit")
  if(!is.null(minor_strata)) strata = c(strata, minor_strata)
  
  cpue = artfishr::compute_cpue(landings, minor_strata=minor_strata)
  cpue$catch_nominal_landed_sampled = NULL
  cpue$effort_fishing_duration = NULL
  out = effort_estimate %>%
    dplyr::left_join(cpue)
  out$catch_total_nominal_landed = out$effort_nominal * out$catch_cpue
  out$effort_activity_coefficient = NULL
  out$effort_fishable_duration = NULL
  out$fleet_engagement_number = NULL
  
  out<-out%>%
  dplyr::left_join(
    landings%>%
      dplyr::group_by_at(strata) %>%
      dplyr::summarize(
        catch_number_species=length(unique(species))
      )%>%
      dplyr::ungroup()%>%
      dplyr::select(
        all_of(strata),
        all_of(minor_strata),
        catch_number_species
      )
  )%>%
    dplyr::ungroup()
  
  return(out)
}