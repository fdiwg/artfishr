#' Tools for implementing ArtFish methodology
#'
#' artfishr provides a set of utility tools to implement ArtFish methodology
#' 
#' @import methods
#' @importFrom stats qt sd setNames
#' @importFrom rlang .data .env
#' @import dplyr
#' @import tidyr
#' @importFrom jsonlite read_json 
#' @import tibble
#' @importFrom lubridate wday second isoweek yday hour year month week isoyear minute mday quarter
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_segment geom_text labs
#' @rawNamespace import(shiny, except = tabsetPanel)
#' @import shiny.i18n
#' @importFrom shinyWidgets pickerInput numericInputIcon
#' @importFrom shinycssloaders withSpinner
#' @importFrom bs4Dash dashboardPage dashboardHeader dashboardBrand dashboardSidebar dashboardBody tabsetPanel box
#' @import plotly
#' @importFrom DT datatable renderDT
#'
#' @import vrule
#' @import fdishinyr

#' @name artfishr
#' @author Emmanuel Blondel \email{emmanuel.blondel1@@gmail.com}
#' 
"_PACKAGE"