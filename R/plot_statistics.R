#' @name plot_statistics
#' @title Plot descriptive statistics over time
#'
#' @description
#' Produces a standardized diagnostic visualization from the output of \code{compute_*_statistics()} functions.
#'
#' The function displays the temporal evolution of descriptive statistics, confidence intervals, dispersion and estimation precision for a single fishing unit (and optionally a single minor stratum).
#'
#' When raw values are available, additional distribution diagnostics are automatically displayed.
#'
#' @param data Output from a \code{compute_*_statistics()} function.
#' @param prefix Variable prefix used in the statistics table (e.g. \code{"catch_cpue_per_trip"}).
#' @param minor_strata Optional name of the minor strata column.
#'
#' @return
#' A patchwork object composed of several ggplot panels.
#'
#' @import patchwork
#' @export

plot_statistics <- function(data,
                            prefix,
                            minor_strata = NULL){
  
  df <- data
  
  # Controls
  
  if (!is.null(minor_strata)) {
    if (!minor_strata %in% names(df)) {
      stop(sprintf("Column '%s' not found in 'data'.",minor_strata),call. = FALSE)
    }
    
    if (dplyr::n_distinct(df[[minor_strata]]) > 1) {
      stop(sprintf("'data' contains several values of '%s'. Please filter the data before calling plot_statistics().",minor_strata),call. = FALSE)
    }
  }
  
  if ("fishing_unit" %in% names(df)) {
    if (dplyr::n_distinct(df$fishing_unit) > 1) {
      stop("'data' contains several fishing units. Please filter the data before calling plot_statistics().",call. = FALSE)
    }
  }
  
  df$date <- as.Date(sprintf("%04d-%02d-01",df$year,df$month))
  
  df <- df |>
    dplyr::arrange(year, month) |>
    dplyr::mutate(
      period = factor(
        format(date, "%m-%Y"),
        levels = format(date, "%m-%Y")
      )
    )
  
  # Column names
  
  mean_col   <- paste0(prefix,"_mean")
  med_col    <- paste0(prefix,"_median")
  q25_col    <- paste0(prefix,"_quantile25")
  q75_col    <- paste0(prefix,"_quantile75")
  min_col    <- paste0(prefix,"_min")
  max_col    <- paste0(prefix,"_max")
  rse_col    <- paste0(prefix,"_relative_standard_error")
  values_col <- paste0(prefix,"_values")
  ci_lower   <- grep(paste0("^",prefix,"_ci[0-9]+_lower$"),names(df),value = TRUE)
  ci_upper   <- grep(paste0("^",prefix,"_ci[0-9]+_upper$"),names(df),value = TRUE)
  
  # Base theme
  
  base_theme <- theme_bw() +
                theme(
                  axis.text.x = element_text(
                    angle = 45,
                    hjust = 1,
                    vjust = 1
                  )
                )
  
  # Panel 1 : Mean + CI + Median
  
  p1 <- ggplot(df, aes(x = period)) +
    
    geom_ribbon(
      aes(
        ymin = .data[[ci_lower]],
        ymax = .data[[ci_upper]],
        group = 1
      ),
      fill = "gold",
      alpha = .18
    ) +
    
    geom_line(
      aes(y = .data[[ci_lower]],group = 1),
      colour = "goldenrod3",
      linetype = "dashed"
    ) +
    
    geom_line(
      aes(y = .data[[ci_upper]],group = 1),
      colour = "goldenrod3",
      linetype = "dashed"
    ) +
    
    geom_line(
      aes(y = .data[[mean_col]],
          colour = "Mean",group = 1),
      linewidth = .8
    ) +
    
    geom_point(
      aes(y = .data[[mean_col]],
          colour = "Mean"),
      size = 2
    ) +
    
    geom_line(
      aes(y = .data[[med_col]],
          colour = "Median",group = 1),
      linetype = "22"
    ) +
    
    geom_point(
      aes(y = .data[[med_col]],
          colour = "Median"),
      shape = 17,
      size = 2.3
    ) +
    
    scale_colour_manual(
      values = c(
        Mean = "black",
        Median = "grey35"
      ),
      guide = guide_legend(
        override.aes = list(
          linetype = c("solid","22"),
          shape = c(16,17),
          linewidth = c(.8,.8)
        )
      ),
      name = NULL
    ) +
    
    labs(
      y = "Mean ± CI90",
      x = NULL
    ) +
    
    base_theme +
    theme(
      legend.position = c(1, 0),
      legend.justification = c(1,0),
      legend.direction = "horizontal" ,
      legend.background =element_rect(fill = "transparent",colour = NA)
    )
  
  # Panel 2 : Boxplot
  
  if(values_col %in% names(df)){
    
    box_data <- df |>
      
      dplyr::select(
        period,
        values = .data[[values_col]]
      ) |>
      
      tidyr::unnest_longer(values)
    
    p2 <- ggplot(box_data, aes(period, values)) +
      
      geom_boxplot(
        fill = "grey85"
      ) +
      
      stat_summary(
        fun = mean,
        geom = "point",
        shape = 23,
        size = 2,
        fill = "black"
      ) +
      
      labs(
        y = "Distribution",
        x = NULL
      ) +
      base_theme
    
  }else{
    
    p2 <-ggplot(df) +
      
      geom_boxplot(
        aes(
          x = period,
          ymin = .data[[min_col]],
          lower = .data[[q25_col]],
          middle = .data[[med_col]],
          upper = .data[[q75_col]],
          ymax = .data[[max_col]]
        ),
        stat = "identity",
        fill = "grey85"
      ) +
      
      geom_point(
        aes(
          x = period,
          y = .data[[mean_col]]
        ),
        shape = 23,
        fill = "red",
        size = 3
      ) +
      
      labs(
        y = "Distribution",
        x = NULL
      ) +
      base_theme
    
  }
  
  # Panel 3 : RSE
  
  ymax <- max(df[[rse_col]], na.rm = TRUE)
  
  ymax <-
    if(ymax <= 40)
      40
  else
    ceiling(ymax / 10) * 10
  
  p3 <- ggplot(df, aes(period,.data[[rse_col]],group = 1)) +
    
    geom_line() +
    
    geom_point(size = 2) +
    
    coord_cartesian(
      ylim = c(0, ymax)
    ) +
    
    labs(
      y = "RSE (%)",
      x = NULL
    ) +
    base_theme
  
  # Panel 4 : Raw observations
  
  if(values_col %in% names(df)){
    
    p4 <- ggplot(box_data, aes(period,values)) +
      
      geom_violin(
        trim = FALSE,
        fill = "grey90",
        colour = "grey70"
      ) +
      
      geom_jitter(
        width = .15,
        alpha = .5,
        colour = "#3A86D4"
      ) +
      
      labs(
        y = "Observations",
        x = NULL
      ) +
      
      base_theme
    
    out <-
      
      p1 /
      p2 /
      p4 /
      p3 +
      
      patchwork::plot_layout(
        heights = c(3,2,2,1)
      )
    
  }else{
    
    out <-
      
      p1 /
      p2 /
      p3 +
      
      patchwork::plot_layout(
        heights = c(3,2,1)
      )
    
  }
  
  out
  
}