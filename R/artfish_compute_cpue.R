#'@name compute_cpue
#'@title Computes CPUE
#'@description
#'The CPUE (Catch Per Unit of Effort) is computed from the landings, as the ratio
#'between nominal landed catches (\code{catch_nominal_landed}) and the effort fishing
#'duration (\code{effort_fishing_duration}).
#'
#'The computation is performed grouped by a strata compound by \code{year}, \code{month}
#'and \code{fishing_unit}. This strata can be extended with additional columns with the
#'\code{minor_strata} argument.
#'
#'Since landings give details data on landed species, nominal landed catches are sum grouped 
#'by the strata and by fishing trip to get the total nominal landed catches by fishing trip.
#'Both \code{catch_nominal_landed} and \code{effort_fishing_duration} are then sum by strata
#'and the CPUE is computed as the ratio of these sum.
#'
#'Note: Additional checks are performed to remove data with NAs.
#'
#'@param landings landings
#'@param minor_strata minor_strata. Default is \code{NULL}
#'@return a \link[tibble]{tibble} object giving the CPUE by strata
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
    dplyr::summarize(
      catch_sample_size=n(),
      catch_coefficient_variation=(sd(catch_nominal_landed,na.rm=T)/sqrt(sum(effort_fishing_duration, na.rm = T)))/(sum(catch_nominal_landed)/sum(effort_fishing_duration, na.rm = T)),
      effort_fishing_duration = sum(effort_fishing_duration, na.rm = T), 
      catch_nominal_landed = sum(catch_nominal_landed, na.rm = T)
      )%>%
    dplyr::ungroup()%>%
    dplyr::mutate(catch_cpue = catch_nominal_landed / effort_fishing_duration)%>%
    dplyr::ungroup()
  
  return(out)
}