#'@name compute_effort_estimate
#'@title Computes nominal effort estimate
#'@description
#'The effort estimate is computed as the product of the \code{fleet_engagement_number}
#'(derived from the \code{active_vessels}), the \code{effort_fishable_duration} (derivated from
#'the \code{active_days}, and the \code{effort_activity_coefficient} computed with the 
#'\link{compute_effort_activity_coefficient} function.
#'
#'The computation of the nominal effort estimate is done grouped by strata compound of \code{year}, \code{month}
#'and \code{fishing_unit}. This strata can be extended with additional columns with the \code{minor_strata} argument.
#'
#'\strong{Active vessels}:
#'
#'The \code{active_vessels} can be either time-dependent,ie given by \code{year}/\code{month}, \code{year} only
#'or be atemporal, mainly depending on how and at which frequency the vessels census/survey is
#'operated in the country.
#'
#'An additional argument called \code{active_vessels_strategy} controls how to select  the \code{active_vessels} data in time, 
#'when this data is time-dependent. The \code{latest} strategy will select the latest data acquired in time, while the \code{closest} 
#'will select the closest data acquired in time, which could be data acquired after the effort data considered.
#'
#'As example, a vessel census is performed each five years, and data is available for 2007 and 2012. Effort data for 2011
#'needs to be computed. Which \code{active_vessels} data should be used? In case of a \code{latest} strategy, in 2011, 
#'the latest data available is 2007, so this one will be used. In the case of a \code{closest} strategy, in 2011, we used
#'the closest vessel data (ie 2012), assuming it betters characterizes the fleet engagement for the year considered.
#'
#'\strong{Active days}:
#'
#'In case of 'fisher_interview' (as source of the effort data), the \code{active_days} data should be ignored, as the days
#'of the month will be used (and generated automatically by the function)
#'
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
#'@param active_days active_days. Default is \code{NULL} (auto-generated)
#'@param landings landings
#'@param minor_strata minor_strata. Default is \code{NULL}
#'@export
compute_effort_estimate = function(
    active_vessels, active_vessels_strategy = c("latest", "closest"),
    effort, effort_source = c("fisher_interview", "boat_counting"), active_days = NULL,landings, minor_strata = NULL
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
      minor_strata = minor_strata
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
  AC$effort_fishing_reference_period = NULL
  AC$effort_total_fishing_duration = NULL
  AC$effort_total_fishing_reference_period = NULL
  AC$fleet_engagement_number = NULL
  AC$fleet_engagement_max = NULL
  
  #derivate join AC with active_vessels
  #for this, the active_vessels needs to be selected based on its temporal extent
  #2 methods possible (latest/closest)
  
  dt = NULL
  if(all(c("year","month") %in% colnames(active_vessels))) {
    
    #reference periods (either years or year/month pairs)
    #used both for selection of active vessels based on time, and derivate join with AC
    ref_periods = if(all(is.na(active_vessels$month))){
      unique(effort$year)
    }else{
      unique(as.Date(do.call(paste0, list(effort$year, "-", sprintf("%02d", effort$month), "-01"))))
    }
    
    AV = select_active_vessels(
      periods = ref_periods,
      active_vessels = active_vessels,
      active_vessels_strategy = active_vessels_strategy
    )
    
    #=>by year
    if(all(is.na(active_vessels$month))){ #we assume that month column is mandatory, but not its content..
      dt = do.call("rbind", lapply(ref_periods, function(year){
        print(year)
        ac_year = AC[AC$year == year,]
        
        #filter active vessels on latest/closest year/month
        av_selection = AV[AV$ref_period == year,]
        av_selection$ref_period = NULL
        
        #group by strata
        av_year_by_strata = av_selection[!is.na(av_selection$fleet_engagement_number),] %>%
          dplyr::group_by_at(strata) %>%
          dplyr::summarize(fleet_engagement_number = sum(fleet_engagement_number)) %>%
          dplylr::ungroup()
        
        #in case of boat_counting and the presence of landing site in active_days table
        #we first compute the weighted mean of active_days (weighted by number of vessels)
        ad_selection = active_days[active_days$year == year,] #default
        if(effort_source == "boat_counting" & "landing_site" %in% colnames(active_days)){
          main_strata = c("fishing_unit")
          if(!is.null(minor_strata)) main_strata = c(main_strata, minor_strata)
          #weighted mean
          ad_selection = ad_selection %>% dplyr::left_join(av_selection, by = c(main_strata, "landing_site"))
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
      
      dt = do.call("rbind", lapply(ref_periods, function(period_date){
        print(period_date)
        ac_period = AC_datetime[AC_datetime$period_date == period_date,]
        
        #filter active vessels on latest/closest year/month
        av_selection = AV[AV$ref_period == period_date,]
        av_selection$ref_period = NULL
        
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
          main_strata = c("fishing_unit")
          if(!is.null(minor_strata)) main_strata = c(main_strata, minor_strata)
          #weighted mean
          ad_selection = ad_selection %>% dplyr::left_join(av_selection, by = c(main_strata, "landing_site"))
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
      main_strata = c("fishing_unit")
      if(!is.null(minor_strata)) main_strata = c(main_strata, minor_strata)
      #weighted mean
      active_days = active_days %>% dplyr::left_join(active_vessels, by = c(main_strata,"landing_site"))
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
  
  return(dt)
}
