#'@name compute_report
#'@title compute_report
#'
#'@param effort effort
#'@param effort_source effort_source
#'@param active_vessels active_vessels
#'@param active_vessels_strategy active_vessels_strategy
#'@param active_days active_days
#'@param landings landings
#'@param minor_strata minor_strata. Default is \code{NULL}
#'@param validate validate
#'
#'@return the result of Artfish
#'@export
#'
compute_report <- function(
  effort,
  effort_source,
  active_vessels,
  active_vessels_strategy,
  active_days,
  landings,
  minor_strata = NULL,
  validate = FALSE){
  
  result = NULL
  qa_report = NULL
  if(validate){
    INFO("Validating data inputs...")
    qa_report = validate_input_datasets(
      active_vessels = active_vessels,
      effort = effort,
      effort_source = effort_source,
      active_days = active_days,
      landings = landings
    )
    if(any(qa_report$type == "ERROR")){
      ERROR("There is at least one input validation error, please check the report")
      out = list(result = result, report = qa_report)
      return(out)
    }
  }
  
  #activity coefficient
  activity_coefficient = compute_effort_activity_coefficient(
    effort = effort,
    effort_source = effort_source,
    minor_strata = minor_strata
  )
  
  #effort estimate (includes calculation of activity coefficient)
  effort_estimate = compute_effort_estimate(
    active_vessels = active_vessels, 
    active_vessels_strategy = active_vessels_strategy, 
    effort = effort, 
    effort_source = effort_source,
    active_days = active_days,
    landings=landings,
    minor_strata = minor_strata
  )
  
  #cpue
  cpue = compute_cpue(landings, minor_strata = minor_strata)
  
  #catch estimate
  catch_estimate = compute_catch_estimate(effort_estimate, landings,minor_strata = minor_strata)
  
  sui = compute_sui(effort, landings, minor_strata = minor_strata)
  
  accuracy = compute_accuracy(
    activity_coefficient,
    effort_estimate,
    cpue,
    sui,
    minor_strata = minor_strata
  )
  
  #catch estimate by species
  catch_estimate_by_species = compute_catch_estimates_by_species(landings, catch_estimate,minor_strata = minor_strata)
  
  #global report
  strata = c("year", "month", "fishing_unit", minor_strata)
  result <- catch_estimate_by_species %>%
    left_join(activity_coefficient[,c(strata, setdiff(names(activity_coefficient), names(catch_estimate_by_species)))], by = strata)
  
  result <- result %>%  
    left_join(effort_estimate[,c(strata, setdiff(names(effort_estimate), names(result)))], strata)
  
  cpue_formatted = cpue %>% select(-effort_fishing_duration) %>% rename(catch_total_cpue=catch_cpue)
  result <- result %>%
    left_join(cpue_formatted[,c(strata, setdiff(names(cpue_formatted), names(result)))], by = strata)
  
  catch_estimate_formatted = catch_estimate %>% select(-catch_cpue)
  result <- result %>%
    left_join(catch_estimate_formatted[,c(strata, setdiff(names(catch_estimate_formatted), names(result)))], by = strata)
  
  result <- result %>%
    left_join(sui[,c(strata, setdiff(names(sui), names(result)))], by = strata)
  
  result <- result %>%
    left_join(accuracy[,c(strata, setdiff(names(accuracy), names(result)))], by = strata) %>%
    ungroup()
  
  return(result)
}