#'@name compute_effort_estimate
#'@title Computes effort estimate
#'@param active_vessels active vessels
#'@param active_vessels_strategy The strategy to associate the active vessels to
#' the effort based on time. Active vessels period does not match necessarily the 
#' periods of data (effort, landings), and can be reported either by year or by year/month. 
#' This parameter let decide which methodology should be used to select the active vessels 
#' based on time. 
#' It can be either "latest" (taking the latest period), "closest" ie the closest active vessels
#' in time, after or before the data period. In case 2 periods before/after are equally closer,
#' the latest in time before the data period will be taken.
#'@param effort effort data
#'@param effort_source effort_source (register_interview / boat_counting)
#'@param active_days active_days
#'@param minor_strata minor_strata
#'@export
compute_effort_estimate = function(
    active_vessels, active_vessels_strategy = c("latest", "closest"),
    effort, effort_source, active_days, minor_strata = NULL
){
  
  active_vessels_strategy = match.arg(active_vessels_strategy)
  
  strata = c("fishing_unit")
  if(!is.null(minor_strata)) strata = c(strata, minor_strata)
  
  #complete active days by eventually filling missing or zero values for 'effort_fishable_duration'
  active_days = complete_active_days(active_days)
  
  #compute effort activity coefficient
  AC = compute_effort_activity_coefficient(effort = effort, effort_source = effort_source, minor_strata = minor_strata)
  
  #derivate join AC with active_vessels
  #for this, the active_vessels needs to be selected based on its temporal extent
  #2 methods possible (latest/closest)
  dt = NULL
  if(all(c("year","month") %in% colnames(active_vessels))) {
    #=>by year
    if(all(is.na(active_vessels$month))){ #we assume that month column is mandatory, but not its content..
      effort_years =  unique(effort$year)
      dt = do.call("rbind", lapply(effort_years, function(year){
        print(year)
        ac_year = AC[AC$year == year,]
        
        #filter active vessels on latest/closest year
        av_selection = switch(active_vessels_strategy,
          "latest" = {
            #if there is no year before or equal year in active_vessels, we stop
            if(!any(active_vessels$year <= year)){
              stop(sprintf("Active vessels strategy 'latest': no latest active_vessels available for year %s", year))
            }
            #"latest" method we filter on active vessels (previous active vessels or current year)
            av_out = active_vessels[active_vessels$year <= year,]
            av_out = av_out[(year - av_out$year) == min(year - av_out$year),]
            av_out
          },
          "closest" = {
            #"closest" method we don't filter on active vessels, consider those before and after year
            av_out = active_vessels
            av_out = av_out[abs(year - av_out$year) == min(abs(year - av_out$year)),]
            #we check if we have 2 av periods (possible latest + closest after year)
            #in which case we take the latest
            if(length(unique(av_out$year))>1){
              av_out = av_out[av_out$year == min(av_out$year),]
            }
            av_out
          }
        )
        #group by strata
        av_year_by_strata = av_selection %>%
          dplyr::group_by_at(strata) %>%
          dplyr::summarize(fleet_engagement_number = sum(fleet_engagement_number)) %>%
          dplylr::ungroup()
        
        dt_year = ac_year %>% dplyr::left_join(y = av_year_by_strata)
        return(dt_year)
      }))
      
    }else{
      #=> by month
      av_datetime = active_vessels
      av_datetime$period_date = as.Date(do.call(paste0, list(active_vessels$year, "-", sprintf("%02d", active_vessels$month), "-01")))
      AC_datetime = AC
      AC_datetime$period_date = as.Date(do.call(paste0, list(AC$year, "-", sprintf("%02d", AC$month), "-01")))
      effort_datetime = effort
      effort_datetime$period_date = as.Date(do.call(paste0, list(effort$year, "-", sprintf("%02d", effort$month), "-01")))
      effort_period_dates = unique(effort_datetime$period_date)
      dt = do.call("rbind", lapply(effort_period_dates, function(period_date){
        print(period_date)
        ac_period = AC_datetime[AC_datetime$period_date == period_date,]
        
        #filter active vessels on latest/closest year
        av_selection = switch(active_vessels_strategy,
          "latest" = {
            #if there is no year before or equal year in active_vessels, we stop
            if(!any(av_datetime$period_date <= period_date)){
              stop(sprintf("Active vessels strategy 'latest': no latest active_vessels available for period %s", substr(period_date, 1,7)))
            }
            #"latest" method we filter on active vessels (previous active vessels or current year)
            av_out = av_datetime[av_datetime$period_date <= period_date,]
            av_out = av_out[(period_date - av_out$period_date) == min(period_date - av_out$period_date),]
            av_out$period_date = NULL
            av_out
          },
          "closest" = {
            #"closest" method we don't filter on active vessels, consider those before and after year/month period
            av_out = av_datetime
            av_out = av_out[abs(period_date - av_out$period_date) == min(abs(period_date - av_out$period_date)),]
            #we check if we have 2 av periods (possible latest + closest after period)
            #in which case we take the latest
            if(length(unique(av_out$period_date))>1){
              av_out = av_out[av_out$period_date == min(av_out$period_date),]
            }
            av_out$period_date = NULL
            av_out
          }
        )
        #group by strata
        av_period_by_strata = av_selection %>%
          dplyr::group_by_at(strata) %>%
          dplyr::summarize(fleet_engagement_number = sum(fleet_engagement_number)) %>%
          dplyr::ungroup()
        
        ac_period$period_date = NULL
        dt_period = ac_period %>% dplyr::left_join(y = av_period_by_strata)
        return(dt_period)
        
      }))
    }
  }else{
    #temporary patch in case active_vessels has no year/month
    active_vessels_by_strata = active_vessels %>%
      dplyr::group_by_at(strata) %>%
      dplyr::summarize(fleet_engagement_number = sum(fleet_engagement_number)) %>%
      dplyr::ungroup()
    
    dt = AC %>% dplyr::left_join(y = active_vessels_by_strata)
  }
  
  #we join with active_days
  dt = dt %>% dplyr::left_join(y = active_days)
  #and compute the effort estimate
  dt$effort_estimate = dt$fleet_engagement_number * dt$effort_fishable_duration * dt$effort_activity_coefficient
  
  return(dt)
}