#' @name compute_effort_activity_coefficient_per_observation_statistics
#' @title Compute activity coefficient descriptive statistics
#'
#' @description
#' Computes descriptive and precision statistics on individual activity coefficient values.
#' This function relies on the generic descriptive statistics engine \code{compute_summary_statistics()}.
#' 
#' Unlike \code{compute_effort_activity_coefficient()}, which returns aggregated activity coefficient estimates, this function summarises the distribution of coefficient values at observation level.
#'
#' For fisher interview data, one observation corresponds to one fisher interview and the activity coefficient is computed as the ratio between fishing duration and the fishing reference period.
#'
#' For boat counting data, one observation corresponds to one boat counting event and the activity coefficient is computed as the ratio between fleet engagement number and fleet engagement maximum.
#' 
#' @param effort A standardized effort dataset.
#' @param effort_source Source of effort observations.
#' @param minor_strata Optional column name defining the minor strata. Default is \code{NULL}.
#' @param confidence Confidence level used to compute confidence intervals.Default is \code{0.90}.
#' @param keep_values Logical. Should the original values be returned in the output? Default is \code{TRUE}.
#'
#' @return A tibble containing descriptive statistics.
#' @export
#' 
compute_effort_activity_coefficient_per_observation_statistics <- function(
  effort,
  effort_source = c("fisher_interview", "boat_counting"),
  minor_strata = NULL,
  confidence = 0.90,
  keep_values = TRUE
  ){
  
  effort_source <- match.arg(effort_source)
  
  if(effort_source == "fisher_interview"){
    effort <- effort |>
      dplyr::filter(!is.na(effort_fishing_duration))
  }
  
  if(effort_source == "boat_counting"){
    
    if(any(is.na(effort$fleet_engagement_max))){
      WARN("Effort data include missing value(s). Removing NAs...")
      effort <- subset(effort, !is.na(fleet_engagement_max))
    }
    
    if(any(effort$fleet_engagement_max < effort$fleet_engagement_number)){
      WARN("Some values for 'fleet_engagement_number' are greater than 'fleet_engagement_max'. Normalizing data...")
      effort[effort$fleet_engagement_max < effort$fleet_engagement_number,"fleet_engagement_number"] <- effort[effort$fleet_engagement_max < effort$fleet_engagement_number,"fleet_engagement_max"]
    }
    
  }
  
  strata <- c("year", "month", "fishing_unit")
  
  if(!is.null(minor_strata)) strata <- c(strata, minor_strata)
  
  activity <- switch(
    
    effort_source,
    "fisher_interview" = {
      effort |>
        dplyr::mutate(
          effort_activity_coefficient_per_observation =
            effort_fishing_duration /
            effort_fishing_reference_period
        )
    },
    "boat_counting" = {
      effort |>
        dplyr::mutate(
          effort_activity_coefficient_per_observation =
            fleet_engagement_number /
            fleet_engagement_max
        )
    }
  )
  
  out <- activity |>
    dplyr::group_by_at(strata) |>
    dplyr::group_modify(
      ~{
        stats <- compute_summary_statistics(
                   .x$effort_activity_coefficient_per_observation,
                   confidence = confidence,
                   keep_values = keep_values
                 )
        
        dplyr::rename_with(
          stats,
          ~paste0("effort_activity_coefficient_per_observation_", .x)
        )
      }
    ) |>
    dplyr::ungroup()
  
  return(out)
  
}