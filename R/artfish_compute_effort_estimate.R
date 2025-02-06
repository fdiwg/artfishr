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
    effort, effort_source = c("fisher_interview", "boat_counting"), active_days = NULL, minor_strata = NULL
){
  
  active_vessels_strategy = match.arg(active_vessels_strategy)
  effort_source = match.arg(effort_source)
  
  strata = c("fishing_unit")
  if(!is.null(minor_strata)) strata = c(strata, minor_strata)
  
  #complete active days by eventually filling missing or zero values for 'effort_fishable_duration'
  if(effort_source == "fisher_interview"){
    INFO("Effort source set to 'fisher_interview' - force use of generated active days (number of days in the month)")
    active_days = generate_active_days(
      active_vessels = active_vessels, 
      effort = effort,
      effort_source = "fisher_interview",
      landings = landings,
      minor_strata = "minor_stratum"
    )
  }else{
    if(is.null(active_days)){
        errMsg = "Active days are required as input data"
        ERROR(errMsg)
        stop(errMsg)
    }
  }
  active_days = complete_active_days(active_days)
  
  #compute effort activity coefficient
  AC = compute_effort_activity_coefficient(effort = effort, effort_source = effort_source, minor_strata = minor_strata)
  AC$effort_fishing_duration = NULL
  AC$effort_fishing_reference_period = NULL
  AC$fleet_engagement_number = NULL
  AC$fleet_engagement_max = NULL
  
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
        av_year_by_strata = av_selection[!is.na(av_selection$fleet_engagement_number),] %>%
          dplyr::group_by_at(strata) %>%
          dplyr::summarize(fleet_engagement_number = sum(fleet_engagement_number)) %>%
          dplylr::ungroup()
        
        #in case of boat_counting and the presence of landing site in active_days table
        #we first compute the weighted mean of active_days (weighted by number of vessels)
        ad_selection = active_days[active_days$year == year,] #default
        if(effort_source == "boat_counting" & "landing_site" %in% colnames(active_days)){
          main_strata = c("year","month","fishing_unit")
          if(!is.null(minor_strata)) main_strata = c(main_strata, minor_strata)
          #weighted mean
          ad_selection = ad_selection %>% dplyr::left_join(av_selection)
          ad_selection = ad_selection[!is.na(ad_selection$fleet_engagement_number),]
          ad_selection = ad_selection %>%
            dplyr::group_by_at(main_strata) %>%
            dplyr::summarise(
              effort_fishable_duration = sum(effort_fishable_duration * fleet_engagement_number) / sum(fleet_engagement_number)
            )
        }
        
        #we join with selected active vessels
        dt_year = ac_year %>% dplyr::left_join(y = av_year_by_strata)
        #we join with active_days
        dt_year = dt_year %>% dplyr::left_join(y = ad_selection)
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
        av_period_by_strata = av_selection[!is.na(av_selection$fleet_engagement_number),] %>%
          dplyr::group_by_at(strata) %>%
          dplyr::summarize(fleet_engagement_number = sum(fleet_engagement_number)) %>%
          dplyr::ungroup()
        
        #in case of boat_counting and the presence of landing site in active_days table
        #we first compute the weighted mean of active_days (weighted by number of vessels)
        ad_selection = active_days[active_days$year == as.integer(format(period_date, "%Y")) &
                                   active_days$month == as.integer(format(period_date, "%m")),] #default
        if(effort_source == "boat_counting" & "landing_site" %in% colnames(active_days)){
          main_strata = c("year","month","fishing_unit")
          if(!is.null(minor_strata)) main_strata = c(main_strata, minor_strata)
          #weighted mean
          ad_selection = ad_selection %>% dplyr::left_join(av_selection)
          ad_selection = ad_selection[!is.na(ad_selection$fleet_engagement_number),]
          ad_selection = ad_selection %>%
            dplyr::group_by_at(main_strata) %>%
            dplyr::summarise(
              effort_fishable_duration = sum(effort_fishable_duration * fleet_engagement_number) / sum(fleet_engagement_number)
            )
        }
        
        ac_period$period_date = NULL
        #we join with selected active vessels
        dt_period = ac_period %>% dplyr::left_join(y = av_period_by_strata)
        #we join with active_days
        dt_period = dt_period %>% dplyr::left_join(y = ad_selection)
        return(dt_period)
        
      }))
      
    }
  }else{
    #temporary patch in case active_vessels has no year/month
    active_vessels_by_strata = active_vessels[!is.na(active_vessels$fleet_engagement_number),] %>%
      dplyr::group_by_at(strata) %>%
      dplyr::summarize(fleet_engagement_number = sum(fleet_engagement_number)) %>%
      dplyr::ungroup()
    
    #in case of boat_counting and the presence of landing site in active_days table
    #we first compute the weighted mean of active_days (weighted by number of vessels)
    if(effort_source == "boat_counting" & "landing_site" %in% colnames(active_days)){
      main_strata = c("year","month","fishing_unit")
      if(!is.null(minor_strata)) main_strata = c(main_strata, minor_strata)
      #weighted mean
      active_days = active_days %>% dplyr::left_join(active_vessels)
      active_days = active_days[!is.na(active_days$fleet_engagement_number),]
      active_days = active_days %>%
        dplyr::group_by_at(main_strata) %>%
        dplyr::summarise(
           effort_fishable_duration = sum(effort_fishable_duration * fleet_engagement_number) / sum(fleet_engagement_number)
        )
    }
    
    #we join with active_vessels
    dt = AC %>% dplyr::left_join(y = active_vessels_by_strata)
    #we join with active_days
    dt = dt %>% dplyr::left_join(y = active_days)
  }
  
  #and compute the effort nominal
  dt$effort_nominal = dt$fleet_engagement_number * dt$effort_fishable_duration * dt$effort_activity_coefficient
  
  #add accuracy
  dt$effort_spatial_accuracy = sapply(1:nrow(dt), function(x){
    artfish_accuracy(n = dt[x,]$sample_size,N = dt[x,]$fleet_engagement_number * 4, method="higher")
  })
  dt$effort_temporal_accuracy = 1L
  
  return(dt)
}
