#'@name compute_effort_activity_coefficient
#'@title Computes effort activity coefficient
#'@description
#'The activity coefficient is computed from the effort. Depending on the source of
#'effort data, the source information used is different:
#'- In the case of 'fisher_interview' the coefficient of activity is computed as the ratio 
#'between the \code{effort_fishing_duration} and the \code{effort_fishing_reference_period}.
#'- In the case of 'boat_counting' the coefficient of activity is computed as the ratio 
#'between the \code{fleet_engagement_number} and the \code{fleet_engagement_max}
#'
#'The computation is performed grouped by a strata compound at minimum by the \code{year},
#'\code{month} and \code{fishing_unit}. This strata can be extended by adding one or more
#'columns with the \code{minor_strata} argument.
#'
#'Note: Additional check are performed to remove data with NAs, and ensure data consistency
#'
#'@param effort effort data
#'@param effort_source effort 
#'@param minor_strata minor_strata. Default is \code{NULL}
#'@return a \link[tibble]{tibble} object giving activity coefficient by strata
#'@export
compute_effort_activity_coefficient = function(effort, effort_source = c("fisher_interview", "boat_counting"), 
                                               minor_strata = NULL){
  effort_source = match.arg(effort_source)
  
  if(effort_source == "fisher_interview") if(any(is.na(effort$effort_fishing_duration))){
    #TODO warnings here to be reported (to investigate how)
    effort<-subset(effort,!is.na(effort_fishing_duration))
  }
  if(effort_source == "boat_counting"){
    if(any(is.na(effort$fleet_engagement_max))){
      #TODO warnings here to be reported (to investigate how)
      WARN("Effort data include missing value(s). Removing NAs...")
      effort<-subset(effort,!is.na(fleet_engagement_max))
    }
    if(any(effort$fleet_engagement_max < effort$fleet_engagement_number)){
      #TODO warnings here. What do do if fleet_engagement_max < fleet_engagement_number
      WARN("Some values for 'fleet_engagement_number' are greater than 'fleet_engagement_max'. Normalizing data...")
      effort[effort$fleet_engagement_max < effort$fleet_engagement_number,]$fleet_engagement_number = fleet_engagement_max
    }
  }
  
  strata = c("year", "month", "fishing_unit")
  if(!is.null(minor_strata)) strata = c(strata, minor_strata)
  
  out <- switch(effort_source,
    "fisher_interview" = {
      out_fisher = effort %>%
        dplyr::group_by_at(strata) %>%
        dplyr::summarize(
          effort_sample_size = n(),
          effort_coefficient_variation = (sd(effort_fishing_duration,na.rm=T)/sqrt(effort_sample_size))/mean(effort_fishing_duration,na.rm=T),
          effort_total_fishing_duration = sum(effort_fishing_duration),
          effort_total_fishing_reference_period = sum(effort_fishing_reference_period),
          effort_fishing_reference_period = unique(effort_fishing_reference_period)
        ) %>%
        dplyr::ungroup()
      out_fisher$effort_activity_coefficient = out_fisher$effort_total_fishing_duration / out_fisher$effort_total_fishing_reference_period
      out_fisher
    },
    "boat_counting" = {
      out_boat = effort %>%
        dplyr::group_by_at(strata) %>%
        dplyr::summarize(
          effort_sample_size = n(),
          effort_coefficient_variation = (sd(fleet_engagement_number,na.rm=T)/sqrt(effort_sample_size))/mean(fleet_engagement_number,na.rm=T),
          fleet_engagement_number = sum(fleet_engagement_number), 
          fleet_engagement_max = sum(fleet_engagement_max),
          effort_fishing_reference_period = 30
        ) %>%
        dplyr::ungroup()
      out_boat$effort_activity_coefficient = out_boat$fleet_engagement_number / out_boat$fleet_engagement_max
      out_boat
    }
  )
      
  return(out)
}