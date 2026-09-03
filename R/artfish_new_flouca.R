
#'@name artfish_flouca_by_period
#'@title artfish_flouca_by_period
#'
#'@param year year
#'@param month month
#'@param active_vessels active vessels
#'@param effort effort
#'@param effort_source effort source whether it's derived from survey -B1- (fishers interviews) 
#'or registry -B2- (boat counting)
#'@param active_days active days
#'@param landings landings
#'@param minor_strata minor_strata (to investigate further later)
#'@param validate validate
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
  
  target_active_vessels <- active_vessels |> dplyr::filter(year==.env$year & month==.env$month)
  target_effort<-effort |> dplyr::filter(year==.env$year & month==.env$month)
  target_active_days <- active_days |> dplyr::filter(year==.env$year & month==.env$month)
  target_landings <- landings |> dplyr::filter(year==.env$year & month==.env$month)
  
  group_cols = c("EST_YEAR","EST_MONTH","EST_BGC")
  
  effort <- target_effort|>
    dplyr::full_join(target_active_vessels) |>
    dplyr::full_join(target_active_days) |>
    dplyr::rename(
      EST_YEAR = "year",
      EST_MONTH = "month",
      EST_BGC = "fishing_unit"
    ) |>
    dplyr::mutate(EST_BGC = as.character(.data$EST_BGC)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(
      EST_EFF_NSMP = length(.data$effort_fishing_duration),
      EST_EFF_NBDAYS = length(unique(.data$day)),
      EST_EFF_SRVTYPE = as.integer(unique(.data$effort_type)),
      EST_EFF_ACTDAYS = sum(.data$effort_fishing_duration,na.rm = T),
      EST_EFF_EXDAYS = .data$EST_EFF_NSMP * .data$EST_EFF_SRVTYPE,
      EST_EFF_PBA = .data$EST_EFF_ACTDAYS / .data$EST_EFF_EXDAYS,
      EST_EFF_NACT = unique(.data$effort_fishable_duration),
      EST_EFF_NBOATS = unique(.data$fleet_engagement_number),
      EST_EFF_POP = .data$EST_EFF_NBOATS * .data$EST_EFF_NACT,
      EST_EFF_EFFORT = .data$EST_EFF_PBA * .data$EST_EFF_NBOATS * .data$EST_EFF_NACT,
      mean = base::mean(.data$effort_fishing_duration,na.rm=T),
      sd = stats::sd(.data$effort_fishing_duration,na.rm=T),
      se = .data$sd / sqrt(.data$EST_EFF_NSMP),
      EST_EFF_CV = .data$se / .data$mean,
      EST_EFF_SPAACCUR = artfish_accuracy(n=.data$EST_EFF_NSMP,N=.data$EST_EFF_NBOATS*4,method="higher"),
      EST_EFF_TMPACCUR = 1,
      EST_EFF_SUI = unif_index(.data$day)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
      "EST_YEAR",
      "EST_MONTH",
      "EST_BGC",
      "EST_EFF_NBOATS",
      "EST_EFF_NACT",
      "EST_EFF_NBDAYS",
      "EST_EFF_ACTDAYS",
      "EST_EFF_EXDAYS",
      "EST_EFF_PBA",
      "EST_EFF_NSMP",
      "EST_EFF_CV",
      "EST_EFF_SUI",
      "EST_EFF_SRVTYPE",
      "EST_EFF_SPAACCUR",
      "EST_EFF_TMPACCUR",
      "EST_EFF_POP",
      "EST_EFF_EFFORT"
    )
  
  landing <- target_landings|>
    dplyr::rename(
      EST_YEAR = "year",
      EST_MONTH = "month",
      EST_BGC = "fishing_unit"
    ) |>
    dplyr::mutate(EST_BGC = as.character(.data$EST_BGC)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_cols,"day","fishing_trip")))) |>
    dplyr::summarise(quantity = sum(.data$catch_nominal_landed,na.rm = T), value=sum(.data$trade_value,na.rm=T)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::mutate(quantity = replace(.data$quantity,is.na(.data$quantity), 0)) |>
    dplyr::summarise(
      EST_LND_NDAYS = length(unique(.data$day)),
      EST_LND_SMPCATCH = sum(.data$quantity),
      EST_LND_NSMP = length(.data$quantity),
      EST_LND_CPUE_G = .data$EST_LND_SMPCATCH / .data$EST_LND_NSMP,
      sd = stats::sd(.data$quantity,na.rm=T),
      se = .data$sd / sqrt(.data$EST_LND_NSMP),
      EST_LND_CV = .data$se / .data$EST_LND_CPUE_G,
      EST_LND_SUI = unif_index(.data$day)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
      "EST_YEAR",
      "EST_MONTH",
      "EST_BGC",
      "EST_YEAR",
      "EST_LND_NDAYS",
      "EST_LND_SMPCATCH",
      "EST_LND_NSMP",
      "EST_LND_CPUE_G",
      "EST_LND_CV",
      "EST_LND_SUI"
    )
  
  estimate <- effort|>
    dplyr::left_join(landing) |>
    dplyr::mutate(EST_BGC = as.character(.data$EST_BGC)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::mutate(
      EST_LND_CATCH_G = .data$EST_EFF_EFFORT * .data$EST_LND_CPUE_G,
      EST_LND_SPAACCUR = artfish_accuracy(n = .data$EST_LND_NSMP, N = .data$EST_EFF_POP, method="higher"),
      EST_LND_TMPACCUR = artfish_accuracy(n = .data$EST_LND_NDAYS,N = .data$EST_EFF_NACT, method="higher"),
      EST_ACCUR = min(.data$EST_EFF_SPAACCUR, .data$EST_EFF_TMPACCUR, .data$EST_LND_SPAACCUR, .data$EST_LND_TMPACCUR,na.rm=T)
    )
  
  estimate <- target_landings |>
    dplyr::rename(
      EST_YEAR = "year",
      EST_MONTH = "month",
      EST_BGC = "fishing_unit",
      EST_SPC = "species"
    ) |>
    dplyr::mutate(EST_SPC = as.character(.data$EST_SPC)) |>
    dplyr::mutate(EST_BGC = as.character(.data$EST_BGC)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::filter(!is.na(.data$EST_SPC)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_cols,"EST_SPC")))) |>
    dplyr::summarise(
      n = sum(.data$catch_nominal_landed),
      EST_LND_NOFISH = sum(.data$trade_value),
      EST_LND_PRICE = mean(.data$trade_value / .data$catch_nominal_landed)
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::mutate(sum=sum(.data$n),ratio = .data$n/.data$sum,EST_NOSPE = length(unique(.data$EST_SPC))) |>
    dplyr::select(-c("n","sum")) |>
    dplyr::left_join(estimate) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      EST_LND_CPUE = .data$EST_LND_CPUE_G * .data$ratio,
      EST_LND_CATCH = .data$EST_EFF_EFFORT * .data$EST_LND_CPUE,
      EST_LND_VALUE = .data$EST_LND_CATCH * .data$EST_LND_PRICE,
      EST_LND_AVW = .data$EST_LND_CATCH / .data$EST_LND_NOFISH
    ) |>
    dplyr::select(-"ratio") |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::mutate(EST_LND_VALUE_G = sum(.data$EST_LND_VALUE,na.rm = T)) |>
    dplyr::ungroup() |>
    dplyr::mutate(EST_LND_PRICE_G = .data$EST_LND_VALUE_G / .data$EST_LND_CATCH_G)
  
  estimate <- estimate |>
    dplyr::select(
      "EST_YEAR",
      "EST_MONTH",
      "EST_BGC",
      "EST_EFF_EFFORT",
      "EST_EFF_NBOATS",
      "EST_EFF_NACT",
      "EST_EFF_PBA",
      "EST_EFF_ACTDAYS",
      "EST_EFF_EXDAYS",
      "EST_EFF_NSMP",
      "EST_EFF_NBDAYS",
      "EST_EFF_POP",
      "EST_EFF_SRVTYPE",
      "EST_EFF_CV",
      "EST_EFF_SPAACCUR",
      "EST_EFF_TMPACCUR",
      "EST_EFF_SUI",
      "EST_LND_CATCH_G",
      "EST_LND_CPUE_G",
      "EST_LND_SMPCATCH",
      "EST_LND_NSMP",
      "EST_LND_VALUE_G",
      "EST_LND_PRICE_G",
      "EST_LND_NDAYS",
      "EST_LND_CV",
      "EST_LND_SPAACCUR",
      "EST_LND_TMPACCUR",
      "EST_LND_SUI",
      "EST_ACCUR",
      "EST_NOSPE",
      "EST_SPC",
      "EST_LND_NOFISH",
      "EST_LND_CATCH",
      "EST_LND_CPUE",
      "EST_LND_VALUE",
      "EST_LND_PRICE",
      "EST_LND_AVW"
    )

 return(estimate)
}







