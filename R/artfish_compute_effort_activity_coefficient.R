#'@name compute_effort_activity_coefficient
#'@title Computes effort activity coefficient
#'@param effort effort data
#'@return the activity coefficient by strata
#'@export
compute_effort_activity_coefficient = function(effort){
  
  if(any(is.na(effort$effort_fishing_duration))){
    #TODO warnings here to be reported (to investigate how)
    effort<-subset(effort,!is.na(effort_fishing_duration))
  }
  
  out <- effort %>%
    group_by(year, month, fishing_unit) %>%
    summarize(effort_fishing_duration = sum(effort_fishing_duration),effort_reference_period_duration = sum(effort_reference_period_duration))
  
  out$effort_activity_coefficient = out$effort_fishing_duration / out$effort_reference_period_duration
  return(out)
}