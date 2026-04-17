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
#'@param progress_fn a progress function with args (label, p). Default is \code{NULL}
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
  validate = FALSE,
  progress_fn = NULL){
  
  if(!is.null(progress_fn)) progress_fn("Compute report...")
  
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
  if(!is.null(progress_fn)) progress_fn("Computing effort activity coefficient")
  activity_coefficient = compute_effort_activity_coefficient(
    effort = effort,
    effort_source = effort_source,
    minor_strata = minor_strata
  )
  
  #effort estimate (includes calculation of activity coefficient)
  if(!is.null(progress_fn)) progress_fn("Computing effort estimate")
  effort_estimate = compute_effort_estimate(
    active_vessels = active_vessels, 
    active_vessels_strategy = active_vessels_strategy, 
    effort = effort, 
    effort_source = effort_source,
    active_days = active_days,
    landings=landings,
    minor_strata = minor_strata,
    progress_fn = progress_fn
  )
  
  #cpue
  if(!is.null(progress_fn)) progress_fn("Computing CPUE")
  cpue = compute_cpue(landings, minor_strata = minor_strata)
  
  #catch estimate
  if(!is.null(progress_fn)) progress_fn("Computing catch estimate")
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
  if(!is.null(progress_fn)) progress_fn("Computing catch estimate by species")
  catch_estimate_by_species = compute_catch_estimates_by_species(landings, catch_estimate,minor_strata = minor_strata)
  
  #global report
  if(!is.null(progress_fn)) progress_fn("Computing final report")
  strata = c("year", "month", "fishing_unit", minor_strata)
  result <- catch_estimate_by_species |>
    left_join(activity_coefficient[,c(strata, setdiff(names(activity_coefficient), names(catch_estimate_by_species)))], by = strata)
  
  result <- result |>  
    left_join(effort_estimate[,c(strata, setdiff(names(effort_estimate), names(result)))], by = strata)
  
  cpue_formatted = cpue |> select(-effort_fishing_duration) |> rename(catch_total_cpue=catch_cpue)
  result <- result |>
    left_join(cpue_formatted[,c(strata, setdiff(names(cpue_formatted), names(result)))], by = strata)
  
  catch_estimate_formatted = catch_estimate |> select(-catch_cpue)
  result <- result |>
    left_join(catch_estimate_formatted[,c(strata, setdiff(names(catch_estimate_formatted), names(result)))], by = strata)
  
  result <- result |>
    left_join(sui[,c(strata, setdiff(names(sui), names(result)))], by = strata)
  
  result <- result |>
    left_join(accuracy[,c(strata, setdiff(names(accuracy), names(result)))], by = strata) |>
    ungroup()
  
  if(!is.null(progress_fn)) progress_fn("Success")
  
  return(result)
}
