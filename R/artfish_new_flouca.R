
#'@name artfish_flouca_by_period
#'@title artfish_flouca_by_period
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
artfish_flouca_by_period <- function(
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
  
  target_active_vessels<-active_vessels%>%filter(year==.env$year & month==.env$month)
  target_effort<-effort%>%filter(year==.env$year & month==.env$month)
  target_active_days<-active_days%>%filter(year==.env$year & month==.env$month)
  target_landings<-landings%>%filter(year==.env$year & month==.env$month)
  
  effort<-target_effort%>%
    full_join(target_active_vessels)%>%
    full_join(target_active_days)%>%
    dplyr::rename(EST_YEAR=year,
                  EST_MONTH=month,
                  EST_BGC=fishing_unit)%>%
    mutate(EST_BGC=as.character(EST_BGC))%>%
    group_by(EST_YEAR,EST_MONTH,EST_BGC)%>%
    dplyr::summarise(
      EST_EFF_NSMP=length(effort_fishing_duration),
      EST_EFF_NBDAYS=length(unique(day)),
      EST_EFF_SRVTYPE=as.integer(unique(effort_type)),
      EST_EFF_ACTDAYS=sum(effort_fishing_duration,na.rm = T),
      EST_EFF_EXDAYS=EST_EFF_NSMP*EST_EFF_SRVTYPE,
      EST_EFF_PBA=EST_EFF_ACTDAYS/EST_EFF_EXDAYS,
      EST_EFF_NACT=unique(effort_fishable_duration),
      EST_EFF_NBOATS=unique(fleet_engagement_number),
      EST_EFF_POP=EST_EFF_NBOATS*EST_EFF_NACT,
      EST_EFF_EFFORT=EST_EFF_PBA*EST_EFF_NBOATS*EST_EFF_NACT,
      mean=mean(effort_fishing_duration,na.rm=T),
      sd=sd(effort_fishing_duration,na.rm=T),
      se=sd/sqrt(EST_EFF_NSMP),
      EST_EFF_CV=se/mean,
      EST_EFF_SPAACCUR=artfish_accuracy(n=EST_EFF_NSMP,N=EST_EFF_NBOATS*4,method="higher"),
      EST_EFF_TMPACCUR=1,
      EST_EFF_SUI=unif_index(day)
    )%>%
    ungroup()%>%
    select(EST_YEAR,
           EST_MONTH,
           EST_BGC,
           EST_EFF_NBOATS,
           EST_EFF_NACT,
           EST_EFF_NBDAYS,
           EST_EFF_ACTDAYS,
           EST_EFF_EXDAYS,
           EST_EFF_PBA,
           EST_EFF_NSMP,
           EST_EFF_CV,
           EST_EFF_SUI,
           EST_EFF_SRVTYPE,
           EST_EFF_SPAACCUR,
           EST_EFF_TMPACCUR,
           EST_EFF_POP,
           EST_EFF_EFFORT
    )
  
  landing<-target_landings%>%
    dplyr::rename(EST_YEAR=year,
                  EST_MONTH=month,
                  EST_BGC=fishing_unit)%>%
    mutate(EST_BGC=as.character(EST_BGC))%>%
    group_by(EST_YEAR,EST_MONTH,EST_BGC,day,fishing_trip)%>%
    dplyr::summarise(quantity=sum(catch_nominal_landed,na.rm = T),value=sum(trade_value,na.rm=T))%>%
    group_by(EST_YEAR,EST_MONTH,EST_BGC)%>%
    dplyr::mutate(quantity = replace(quantity,is.na(quantity), 0))%>%
    dplyr::summarise(
      EST_LND_NDAYS=length(unique(day)),
      EST_LND_SMPCATCH=sum(quantity),
      EST_LND_NSMP=length(quantity),
      EST_LND_CPUE_G=EST_LND_SMPCATCH/EST_LND_NSMP,
      sd=sd(quantity,na.rm=T),
      se=sd/sqrt(EST_LND_NSMP),
      EST_LND_CV=se/EST_LND_CPUE_G,
      EST_LND_SUI=unif_index(day)
    )%>%
    ungroup()%>%
    select(
      EST_YEAR,
      EST_MONTH,
      EST_BGC,
      EST_YEAR,
      EST_LND_NDAYS,
      EST_LND_SMPCATCH,
      EST_LND_NSMP,
      EST_LND_CPUE_G,
      EST_LND_CV,
      EST_LND_SUI
    )
  
  estimate<-effort%>%
    left_join(landing)%>%
    mutate(EST_BGC=as.character(EST_BGC))%>%
    group_by(EST_YEAR,EST_MONTH,EST_BGC)%>%
    dplyr::mutate(
      EST_LND_CATCH_G=EST_EFF_EFFORT*EST_LND_CPUE_G,
      EST_LND_SPAACCUR=artfish_accuracy(n=EST_LND_NSMP,N=EST_EFF_POP,method="higher"),
      EST_LND_TMPACCUR=artfish_accuracy(n=EST_LND_NDAYS,N=EST_EFF_NACT,method="higher"),
      EST_ACCUR=min(EST_EFF_SPAACCUR,EST_EFF_TMPACCUR,EST_LND_SPAACCUR,EST_LND_TMPACCUR,na.rm=T)
    )
  
  estimate<-target_landings%>%
    dplyr::rename(EST_YEAR=year,
                  EST_MONTH=month,
                  EST_BGC=fishing_unit,
                  EST_SPC=species)%>%
    mutate(EST_SPC=as.character(EST_SPC))%>%
    mutate(EST_BGC=as.character(EST_BGC))%>%
    group_by(EST_YEAR,EST_MONTH,EST_BGC)%>%
    filter(!is.na(EST_SPC))%>%
    group_by(EST_YEAR,EST_MONTH,EST_BGC,EST_SPC)%>%
    dplyr::summarise(n=sum(catch_nominal_landed),EST_LND_NOFISH=sum(trade_value),EST_LND_PRICE=mean(trade_value/catch_nominal_landed))%>%
    group_by(EST_YEAR,EST_MONTH,EST_BGC)%>%
    dplyr::mutate(sum=sum(n),ratio=n/sum,EST_NOSPE=length(unique(EST_SPC)))%>%
    select(-n,-sum)%>%
    left_join(estimate)%>%
    ungroup()%>%
    dplyr::mutate(EST_LND_CPUE=EST_LND_CPUE_G*ratio,
                  EST_LND_CATCH=EST_EFF_EFFORT*EST_LND_CPUE,
                  EST_LND_VALUE=EST_LND_CATCH*EST_LND_PRICE,
                  EST_LND_AVW=EST_LND_CATCH/EST_LND_NOFISH)%>%
    select(-ratio)%>%
    group_by(EST_YEAR,EST_MONTH,EST_BGC)%>%
    dplyr::mutate(EST_LND_VALUE_G=sum(EST_LND_VALUE,na.rm = T))%>%
    ungroup()%>%
    dplyr::mutate(EST_LND_PRICE_G=EST_LND_VALUE_G/EST_LND_CATCH_G)
  
  estimate<-estimate%>%
    select(
      EST_YEAR,
      EST_MONTH,
      EST_BGC,
      EST_EFF_EFFORT,
      EST_EFF_NBOATS,
      EST_EFF_NACT,
      EST_EFF_PBA,
      EST_EFF_ACTDAYS,
      EST_EFF_EXDAYS,
      EST_EFF_NSMP,
      EST_EFF_NBDAYS,
      EST_EFF_POP,
      EST_EFF_SRVTYPE,
      EST_EFF_CV,
      EST_EFF_SPAACCUR,
      EST_EFF_TMPACCUR,
      EST_EFF_SUI,
      EST_LND_CATCH_G,
      EST_LND_CPUE_G,
      EST_LND_SMPCATCH,
      EST_LND_NSMP,
      EST_LND_VALUE_G,
      EST_LND_PRICE_G,
      EST_LND_NDAYS,
      EST_LND_CV,
      EST_LND_SPAACCUR,
      EST_LND_TMPACCUR,
      EST_LND_SUI,
      EST_ACCUR,
      EST_NOSPE,
      EST_SPC,
      EST_LND_NOFISH,
      EST_LND_CATCH,
      EST_LND_CPUE,
      EST_LND_VALUE,
      EST_LND_PRICE,
      EST_LND_AVW
    )
  
  
  #OBJECTIVE
  #step 1 - boat activity 
  # bac_out = artfishr::compute_bac(...)
  # #step 2 - effort
  # effort_out = artfishr::compute_effort(...)
  # #step 3 - cpu (catch)
  # cpu_out = artfishr::compute_cpu(...)
  # #=> result should be the same
  # out <- list(
  #   bac = data.frame(),
  #   effort = data.frame(),
  #   cpu = data.frame()
  # )

 return(estimate)
}







