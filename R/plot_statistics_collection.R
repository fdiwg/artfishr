#' @name plot_statistics_collection
#' @title Generate a collection of statistics plots
#'
#' @description
#' Recursively applies \code{plot_statistics()} over one or several grouping variables and returns the resulting plots as a nested named list.
#'
#' @param data Output from a \code{compute_*_statistics()} function.
#' @param prefix Variable prefix used in the statistics table (e.g. \code{"catch_cpue_per_trip"}).
#' @param minor_strata Optional name of the minor strata column.
#' @param by Character vector defining the grouping hierarchy
#'
#' @return
#' A nested named list of patchwork objects.
#'
#' @export
plot_statistics_collection <- function(data,
                                       prefix,
                                       minor_strata = NULL,
                                       by = NULL){
  
  
  # Default grouping
  
  if(is.null(by)){
    
    by <- "fishing_unit"
    
    if(!is.null(minor_strata))
      by <- c(minor_strata, by)
    
  }
  
  # Checks
  
  missing_cols <- setdiff(by, names(data))
  
  if(length(missing_cols) > 0){
    stop("Unknown grouping column(s): ", paste(missing_cols, collapse = ", "))
  }
  
  # If no grouping level
  
  if(length(by) == 0){
    return(
      plot_statistics(
        data = data,
        prefix = prefix,
        minor_strata = minor_strata
      )
    )
  }
  
  # Split according to first grouping variable
  
  current <- by[1]
  
  groups <- split(
              data,
              data[[current]],
              drop = TRUE
            )
  
  # Recursive call
  
  out <- lapply( groups,
                 
          function(x){
            plot_statistics_collection(
              data = x,
              prefix = prefix,
              minor_strata = minor_strata,
              by = by[-1]
            )
          }
    )
  
  return(out)
  
}