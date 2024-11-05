#'@name compute_catch_estimates_by_species
#'@title Computes catch estimates by species
#'@param landings landings
#'@param catch_estimate result of catch estimate computed with \link{compute_catch_estimate}
#'@return a \link{tibble}
#'@export
compute_catch_estimates_by_species = function(landings, catch_estimate){
  
  species_compo <- landings %>%
    dplyr::group_by(year, month, fishing_unit, species) %>%
    dplyr::summarize(
      species_tot = sum(catch_nominal_landed, na.rm = T),
      species_value = sum(trade_value, na.rm = T)
    )
  species_compo <- species_compo %>%
    dplyr::left_join(catch_estimate)
  
  species_compo_tot <- species_compo %>%
    dplyr::summarise(sum_species_tot = sum(species_tot, na.rm = T))
  species_compo = species_compo %>%
    dplyr::left_join(species_compo_tot)
  
  species_compo$ratio<-species_compo$species_tot / species_compo$sum_species_tot
  species_compo$species_catch <- species_compo$ratio * species_compo$catch_estimate
  species_compo$species_cpue <-species_compo$species_catch / species_compo$effort_estimate
  species_compo$species_price <- species_compo$species_value /species_compo$species_tot
  species_compo$species_tot_value <- species_compo$species_price * species_compo$species_catch
  return(species_compo)
}