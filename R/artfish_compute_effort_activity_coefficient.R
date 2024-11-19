#'@name compute_effort_activity_coefficient
#'@title Computes effort activity coefficient
#'@param effort effort data
#'@param effort_source effort source whether it's derived from -B1- (fishers interviews) 
#'or -B2- (boat counting)
#'@param minor_strata minor_strata
#'@return the activity coefficient by strata
#'@export
compute_effort_activity_coefficient = function(effort, effort_source, minor_strata = NULL){
  
  if(effort_source == "fisher_interview") if(any(is.na(effort$effort_fishing_duration))){
    #TODO warnings here to be reported (to investigate how)
    effort<-subset(effort,!is.na(effort_fishing_duration))
  }
  
  strata = c("year", "month", "fishing_unit")
  if(!is.null(minor_strata)) strata = c(strata, minor_strata)
  
  out <- switch(effort_source,
    "fisher_interview" = {
      out_fisher = effort %>%
        dplyr::group_by_at(strata) %>%
        dplyr::summarize(effort_fishing_duration = sum(effort_fishing_duration),effort_fishing_reference_period = sum(effort_fishing_reference_period)) %>%
        dplyr::ungroup()
      out_fisher$effort_activity_coefficient = out_fisher$effort_fishing_duration / out_fisher$effort_fishing_reference_period
      out_fisher$effort_fishing_duration = NULL
      out_fisher$effort_fishing_reference_period = NULL
      out_fisher
    },
    "boat_counting" = {
      out_boat = effort %>%
        dplyr::group_by_at(strata) %>%
        dplyr::summarize(fleet_engagement_number = sum(fleet_engagement_number), fleet_engagement_max = sum(fleet_engagement_max)) %>%
        dplyr::ungroup()
      out_boat$effort_activity_coefficient = out_boat$fleet_engagement_number / out_boat$fleet_engagement_max
      out_boat$fleet_engagement_number = NULL
      out_boat$fleet_engagement_max = NULL
      out_boat
    }
  )
      
  return(out)
}