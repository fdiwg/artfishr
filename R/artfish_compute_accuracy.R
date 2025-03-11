#'@name compute_accuracy
#'@title Computes Accuracy
#'@description
#'TODO
#'
#'@param activity_coefficient activity_coefficient
#'@param effort_estimate effort_estimate
#'@param cpue cpue
#'@param minor_strata minor_strata
#'@return a \link[tibble]{tibble} object giving the different accuracy by strata
#'@export
#'
compute_accuracy = function(activity_coefficient,effort_estimate,cpue,sui, minor_strata = NULL){
  
  strata = c("year", "month", "fishing_unit")
  if(!is.null(minor_strata)) strata = c(strata, minor_strata)
  
  out = effort_estimate %>%
    dplyr::left_join(
      activity_coefficient%>%
      dplyr::select(strata, effort_fishing_reference_period),
      by = strata
      )%>%
    dplyr::left_join(cpue)%>%
    dplyr::left_join(sui)%>%
    dplyr::rowwise()%>%
    dplyr::mutate(
      effort_activity_coefficient_spatial_accuracy=artfish_accuracy(n = effort_sample_size,N = fleet_engagement_number * 30/effort_fishing_reference_period, method="higher"),
      effort_activity_coefficient_temporal_accuracy = 1L,
      catch_cpue_spatial_accuracy=artfish_accuracy(n = catch_sample_size,N = effort_population, method="higher"),
      catch_cpue_temporal_accuracy=artfish_accuracy(n = catch_number_sampled_days,N = effort_fishable_duration, method="higher"),
      overall_accuracy=min(effort_activity_coefficient_spatial_accuracy,effort_activity_coefficient_temporal_accuracy,catch_cpue_spatial_accuracy,catch_cpue_temporal_accuracy,na.rm=T)
      )%>%
    dplyr::ungroup()%>%
    dplyr::select(
      all_of(strata),
      all_of(minor_strata),
      effort_activity_coefficient_spatial_accuracy,
      effort_activity_coefficient_temporal_accuracy,
      catch_cpue_spatial_accuracy,
      catch_cpue_temporal_accuracy,
      overall_accuracy
    )
  
  return(out)
}
