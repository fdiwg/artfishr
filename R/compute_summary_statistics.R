#' @name compute_summary_statistics
#' @title Compute descriptive summary statistics
#'
#' @description
#' Computes a set of descriptive summary statistics from a numeric vector.
#' This function provides the generic engine used internally to compute descriptive statistics for specific indicators of interest
#' (e.g. \code{compute_cpue_per_trip_statistics()}, \code{compute_effort_activity_coefficient_per_observation_statistics()}).
#'
#' @param x A numeric vector.
#' @param confidence Confidence level used to compute confidence intervals.Default is \code{0.90}.
#' @param keep_values Logical. Should the original values be returned in the output? Default is \code{TRUE}.
#'
#' @return A tibble containing descriptive statistics.
#'
#' @export
#' 

compute_summary_statistics <- function(x, confidence = 0.90,keep_values = TRUE){
  
  x <- x[!is.na(x)]
  
  n <- length(x)
  
  mean_x <- mean(x)
  
  sd_x   <- stats::sd(x)
  
  sem <- sd_x / sqrt(n)
  
  z  <- stats::qnorm((1 + confidence) / 2)
  
  hw <- z * sem
  
  confidence_name <- paste0("ci", round(confidence * 100))
  
  out <- dplyr::tibble(
    sample_size = n,
    mean = mean_x,
    standard_deviation = sd_x,
    median = stats::median(x),
    quantile25 = stats::quantile(x, 0.25, names = FALSE),
    quantile75 = stats::quantile(x, 0.75, names = FALSE),
    min = min(x),
    max = max(x),
    standard_error = sem,
    half_width = hw,
    lower = mean_x - hw,
    upper = mean_x + hw,
    relative_standard_error = 100 * sem / mean_x
  )
  
  names(out)[names(out) == "half_width"] <- paste0(confidence_name, "_half_width")
  names(out)[names(out) == "lower"]               <- paste0(confidence_name, "_lower")
  names(out)[names(out) == "upper"]               <- paste0(confidence_name, "_upper")
  
  if (keep_values) {
    out$values <- list(x)
  }
  
  return(out)
}