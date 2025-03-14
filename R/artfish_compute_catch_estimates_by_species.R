#'@name compute_catch_estimates_by_species
#'@title Computes catch estimates by species
#'@param landings landings
#'@param catch_estimate result of catch estimate computed with \link{compute_catch_estimate}
#'@param minor_strata minor_strata
#'@return a \link{tibble}
#'@export
compute_catch_estimates_by_species = function(landings, catch_estimate, minor_strata = NULL){
  
  catch_estimate$effort_activity_coefficient_spatial_accuracy = NULL
  catch_estimate$effort_activity_coefficient_temporal_accuracy = NULL
  catch_estimate$cpue = NULL

  strata = c("year", "month", "fishing_unit")
  if(!is.null(minor_strata)) strata = c(strata, minor_strata)
  
  species_compo <- landings %>%
    dplyr::group_by_at(c(strata, "species")) %>%
    dplyr::summarize(
      species_tot = sum(catch_nominal_landed, na.rm = T),
      species_value = sum(trade_value, na.rm = T),
      catch_number = sum(catch_number, na.rm = T)
    ) %>%
    dplyr::ungroup()
  species_compo <- species_compo %>%
    dplyr::left_join(catch_estimate)%>%
    dplyr::ungroup()
  
  species_compo_tot <- species_compo %>%
    dplyr::group_by_at(strata) %>%
    dplyr::summarise(sum_species_tot = sum(species_tot, na.rm = T))
  species_compo = species_compo %>%
    dplyr::left_join(species_compo_tot)
  
  species_compo$catch_species_ratio<-species_compo$species_tot / species_compo$sum_species_tot
  species_compo$catch_nominal_landed <- species_compo$catch_species_ratio * species_compo$catch_nominal #catch_nominal_landed
  species_compo$catch_cpue <- species_compo$catch_nominal_landed / species_compo$effort_nominal #catch_cpue
  species_compo$trade_price <- species_compo$species_value /species_compo$species_tot #trade_price
  species_compo$trade_value <- species_compo$trade_price * species_compo$catch_nominal_landed #trade_value
  species_compo$catch_fish_average_weight <- species_compo$catch_nominal_landed/species_compo$catch_number #catch_fish_average_weight
  species_compo$species_tot = NULL
  species_compo$species_value = NULL
  species_compo$sum_species_tot = NULL
  species_compo$effort_nominal = NULL
  species_compo$catch_nominal = NULL
  species_compo$sample_size = NULL
  
  
  return(species_compo)
  
}
