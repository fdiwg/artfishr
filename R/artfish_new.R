#'@name artfish_new
#'@title artfish_new
#'
#'@param active_vessels active vessels
#'@param effort effort
#'@param effort_source effort source whether it's derived from survey -B1- (fishers interviews) 
#'or registry -B2- (boat counting)
#'@param active_days active days
#'@param landings landings
#'@param minor_strata minor_strata (to investigate further later)
#'
#'@return the result of Artfish
#'@export
#'
artfish_new <- function(
  active_vessels = NULL,
  effort = NULL,
  effort_source = c("survey", "registry"),
  active_days = NULL,
  landings = NULL, 
  minor_strata = NULL){
  
  #TODO by sur artfish_new_by_period
}

#'@name artfish_new_by_period
#'@title artfish_new_by_period
#'
#'@param active_vessels active vessels
#'@param effort effort
#'@param effort_source effort source whether it's derived from survey -B1- (fishers interviews) 
#'or registry -B2- (boat counting)
#'@param active_days active days
#'@param landings landings
#'@param minor_strata minor_strata (to investigate further later)
#'
#'@return the result of Artfish for a given year/month
#'@export
#'
artfish_new_by_period <- function(
  year = NULL, month = NULL,  
  active_vessels,
  effort,
  effort_source = c("survey", "registry"),
  active_days = NULL,
  landings,
  minor_strata = NULL,
  validate = TRUE){
  
  #validate A/B/C/D components (delegated to vrule)
  if(validate) validate_input_datasets(
    active_vessels = active_vessels,
    effort = effort,
    effort_source = effort_source,
    active_days = active_days,
    landings = landings
  )
  
  #active_days generation?
  if(is.null(active_days)){
    #autogenerate active_days table
    fishing_units = unique(c(active_vessels$fishing_unit, effort$fishing_unit))
    active_days = generate_active_days(year, month, fishing_units)
  }
  
  #filter control period match args
  #TODO manage case atemporal (no year/no month) -> case of TTO
  #TODO manage case where active vessels is year-based select the latest for the period
  active_vessels = subset(active_vessels, year == year & month == month)
  effort = subset(effort, year == year & month == month)
  active_days = subset(active_days, year == year & month == month)
  landings = subset(landings, year == year & month == month)
  
  #identify strata (that may include minor stratum)
  strata <- c("year", "month", "fishing_unit")
  #-> columns that identify dimensions for grouping
  #examples
  #- year/month/fishing_unit (minimum requirement) - validated by vrule
  #- year/month/(additional minor stratum)/fishing_unit
  
  #verify that year/month is ok on all tables (except eventually active_days IF NULL)
  #filter on these year/month (if there is more, raise a warning to alert user)
  #if active_days was NULL, then generate the equivalent table for the reference period year/month
  
  #verify that year/month/(minor_stratum)/fishing_unit are the same across all tables
  
  #effort estimate
  effort_estimate = compute_effort_estimate(
    active_vessels = active_vessels, 
    effort = effort, 
    active_days = active_days
  )
  
  #cpue
  cpue = compute_cpue(landings)
  
  #catch estimate
  catch_estimate = compute_catch_estimate(
    effort_estimate = effort_estimate,
    cpue = cpue
  )
  
  #catch estimate by species
  catch_estimates_by_species = compute_catch_estimates_by_species(
    landings = landings,
    catch_estimate = catch_estimate
  )
  
  out <- list(
    effort = effort_estimate,
    cpue = cpue,
    catch = catch_estimate,
    catch_by_species = catch_estimates_by_species
  )

  # if(nrow(errors)>0){
  #   attr(out, "errors") <- errors
  # }
  return(out)
}







