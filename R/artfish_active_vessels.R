#'@name select_active_vessels
#'@title Select active vessels
#'@description Select active vessels for a selection of years
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
#'@param periods periods, list of unique years or year/month periods
#'@param active_vessels active vessels object of class \link{tibble}
#'@param active_vessels_strategy The strategy to associate the active vessels to
#' the effort based on time. Active vessels period does not match necessarily the 
#' periods of data (effort, landings), and can be reported either by year or by year/month. 
#' This parameter let decide which methodology should be used to select the active vessels 
#' based on time. 
#' It can be either "latest" (taking the latest period), "closest" ie the closest active vessels
#' in time, after or before the data period. In case 2 periods before/after are equally closer,
#' the latest in time before the data period will be taken.
#'@return a series of active vessels selection for the years considered (added as \code{ref_period})
#'@export
select_active_vessels = function(periods, active_vessels, active_vessels_strategy = c("latest", "closest")){
  AV = NULL
  #=>by year
  if(all(is.na(active_vessels$month))){ #we assume that month column is mandatory, but not its content..
    AV = do.call("rbind", lapply(periods, function(year){
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
      av_selection$ref_period = year
      return(av_selection)
    }))
    
  }else{
    #=> by month
    av_datetime = active_vessels
    av_datetime$period_date = as.Date(do.call(paste0, list(active_vessels$year, "-", sprintf("%02d", active_vessels$month), "-01")))
    AV = do.call("rbind", lapply(periods, function(period_date){
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
      av_selection$ref_period = period_date
      return(av_selection)
    }))
  }
  return(AV)
}