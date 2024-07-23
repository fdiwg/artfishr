#'@name artfish_estimates
#'@title artfish_estimates
#'
#'@param data_effort effort data
#'@param data_landing landing data
#'@param ref_fishingunits fishing units reference dataset
#'@param ref_species species reference dataset
#'
#'@return the result of Artfish
#'@export
#'
artfish_estimates <- function(
    data_effort=NULL, data_landing=NULL,
    ref_fishingunits = NULL, ref_species,
    year = NULL, month = NULL){
  
  #TODO for future, check availability of args inputs
  #and choose the right protocol to derivate Artfish estimates
  
  #TODO for future, QA = use vrule to check structure/content of inputs
  
  #run
  effort<-data_effort%>%
    dplyr::rename(EST_YEAR=year,
                  EST_MONTH=month,
                  EST_BGC=fishing_unit)%>%
    mutate(EST_BGC=as.character(EST_BGC))%>%
    group_by(EST_YEAR,EST_MONTH,EST_BGC)%>%
    dplyr::summarise(
      EST_EFF_NSMP=length(days_sampled),
      EST_EFF_NBDAYS=length(unique(days)),
      EST_EFF_SRVTYPE=as.integer(unique(effort_type)),
      EST_EFF_APPROACH=ifelse(EST_EFF_SRVTYPE==7,"WEEKLY","To DO"),
      EST_EFF_ACTDAYS=sum(days_sampled,na.rm = T),
      EST_EFF_EXDAYS=EST_EFF_NSMP*EST_EFF_SRVTYPE,
      EST_EFF_PBA=EST_EFF_ACTDAYS/EST_EFF_EXDAYS,
      EST_EFF_NACT=unique(NB_ACTIVE_DAYS),
      EST_EFF_NBOATS=unique(BG),
      EST_EFF_POP=EST_EFF_NBOATS*EST_EFF_NACT,
      EST_EFF_EFFORT=EST_EFF_PBA*EST_EFF_NBOATS*EST_EFF_NACT,
      mean=mean(days_sampled,na.rm=T),
      sd=sd(days_sampled,na.rm=T),
      se=sd/sqrt(EST_EFF_NSMP),
      EST_EFF_CV=se/mean,
      EST_EFF_SPAACCUR=artfish_accuracy(n=EST_EFF_NSMP,N=EST_EFF_NBOATS*4,method="higher"),
      EST_EFF_TMPACCUR=1,
      EST_EFF_SUI=unif_index(days)
    )%>%
    ungroup()%>%
    left_join(fishing_units, by="EST_BGC")%>%
    select(EST_YEAR,
           EST_MONTH,
           EST_BGC,
           EST_BGC_NAME,
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
           EST_EFF_APPROACH,
           EST_EFF_SPAACCUR,
           EST_EFF_TMPACCUR,
           EST_EFF_POP,
           EST_EFF_EFFORT
    )
  
  ####Landing
  
  landing<-data_landing%>%
    dplyr::rename(EST_YEAR=year,
                  EST_MONTH=month,
                  EST_BGC=fishing_unit)%>%
    mutate(EST_BGC=as.character(EST_BGC))%>%
    group_by(EST_YEAR,EST_MONTH,EST_BGC,days,id)%>%
    dplyr::summarise(quantity=sum(quantity,na.rm = T),value=sum(value,na.rm=T),price=mean(price,na.rm=T))%>%
    group_by(EST_YEAR,EST_MONTH,EST_BGC)%>%
    dplyr::mutate(quantity = replace(quantity,is.na(quantity), 0))%>%
    dplyr::summarise(
      EST_LND_NDAYS=length(unique(days)),
      EST_LND_SMPCATCH=sum(quantity),
      EST_LND_NSMP=length(quantity),
      EST_LND_CPUE_G=EST_LND_SMPCATCH/EST_LND_NSMP,
      sd=sd(quantity,na.rm=T),
      se=sd/sqrt(EST_LND_NSMP),
      EST_LND_CV=se/EST_LND_CPUE_G,
      EST_LND_SUI=unif_index(days)
    )%>%
    ungroup()%>%
    left_join(fishing_units, by="EST_BGC")%>%
    select(
      EST_YEAR,
      EST_MONTH,
      EST_BGC,
      EST_BGC_NAME,
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
  
  estimate<-data_landing%>%
    dplyr::rename(EST_YEAR=year,
                  EST_MONTH=month,
                  EST_BGC=fishing_unit,
                  EST_SPC=species)%>%
    mutate(EST_SPC=as.character(EST_SPC))%>%
    mutate(EST_BGC=as.character(EST_BGC))%>%
    left_join(ref_species, by="EST_SPC")%>%
    group_by(EST_YEAR,EST_MONTH,EST_BGC)%>%
    filter(!is.na(EST_SPC))%>%
    group_by(EST_YEAR,EST_MONTH,EST_BGC,EST_SPC,EST_SPC_NAME)%>%
    dplyr::summarise(n=sum(quantity),EST_LND_NOFISH=sum(number),EST_LND_PRICE=mean(price))%>%
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
      EST_BGC_NAME,
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
      EST_EFF_APPROACH,
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
      EST_SPC_NAME,
      EST_LND_NOFISH,
      EST_LND_CATCH,
      EST_LND_CPUE,
      EST_LND_VALUE,
      EST_LND_PRICE,
      EST_LND_AVW
    )
  
  return(estimate)
}