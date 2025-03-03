#'@name compute_sui
#'@title Computes sui
#'@description
#'TODO
#'
#'@param effort effort
#'@param landings landings
#'@param minor_strata minor_strata
#'@return a \link[tibble]{tibble} object giving the different accuracy by strata
#'@export
compute_sui = function(effort,landings, minor_strata = NULL){
  
  strata = c("year", "month", "fishing_unit")
  if(!is.null(minor_strata)) strata = c(strata, minor_strata)
  
  out = effort %>%
    dplyr::select(c(strata),day)%>%
    dplyr::group_by_at(strata) %>%
    dplyr::summarise(
      effort_number_days=length(unique(day)),
      effort_sui=unif_index(day))%>%
    dplyr::ungroup()%>%
    dplyr::left_join(
      landings %>%
      dplyr::select(c(strata),day,fishing_trip)%>%
      dplyr::distinct()%>%
      dplyr::group_by_at(strata) %>%
      dplyr::summarise(
        catch_number_days=length(unique(day)),
        catch_sui=unif_index(day)
        )%>%
    dplyr::ungroup()
    )%>%
    dplyr::ungroup()
  
  return(out)
}