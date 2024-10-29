#'@name generate_active_days
#'@title Generates a \link{tibble} of active days
#'@param year year
#'@param month month
#'@param fishing_units one or more fishing unit(s)
#'@return an object of class \link{tibble} give active days
generate_active_days = function(year, month, fishing_units){
  tibble::tibble(
    year = year, 
    month = month,
    fishing_unit = fishing_units,
    effort_fishable_duration = lubridate::days_in_month(ISOdate(year, month, 1))
  )
}