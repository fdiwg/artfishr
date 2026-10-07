#' @name compute_cpue_per_trip_statistics
#' @title Compute CPUE per trip descriptive statistics
#'
#' @description
#' Computes descriptive and precision statistics for fishing-trip CPUE values, aggregated by year, month, fishing unit and optionally by minor strata.
#' This function relies on the generic descriptive statistics engine \code{compute_summary_statistics()}.
#'
#' Unlike \code{compute_cpue()}, which returns aggregated CPUE estimates, this function summarises the distribution of individual fishing-trip CPUE observations.
#'
#' @param landings A standardized landings dataset.
#' @param minor_strata Optional column name defining the minor strata. Default is \code{NULL}.
#' @param confidence Confidence level used to compute confidence intervals.Default is \code{0.90}.
#' @param keep_values Logical. Should the original values be returned in the output? Default is \code{TRUE}.
#'
#' @return A tibble containing descriptive statistics.
#' @export
compute_cpue_per_trip_statistics <- function(
                                    landings,
                                    minor_strata = NULL,
                                    confidence = 0.90,
                                    keep_values = TRUE
                                    ){
  
  strata <- c("year","month","fishing_unit")
  
  if(!is.null(minor_strata)) strata <- c(strata, minor_strata)
  
  cpue_trip <- landings |>
    dplyr::group_by_at(
      c(
        strata,
        "fishing_trip",
        "effort_fishing_duration"
      )
    ) |>
    dplyr::summarise(
      catch_nominal_landed =
        sum(catch_nominal_landed,
            na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      catch_cpue_per_trip =
        catch_nominal_landed /
        effort_fishing_duration
    )
  
  out <-cpue_trip |>
    dplyr::group_by_at(strata) |>
    dplyr::group_modify(
      ~{
        stats <- compute_summary_statistics(
                  .x$catch_cpue_per_trip,
                  confidence = confidence,
                  keep_values = keep_values
                )
        
        dplyr::rename_with(
          stats,
          ~paste0("catch_cpue_per_trip_", .x)
        )
      }
    ) |>
    dplyr::ungroup()
  
  return(out)
  
}