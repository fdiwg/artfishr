#'@name artfish_estimates
#'@title artfish_estimates
#'
#'@param data_effort effort data
#'@param data_landing landing data
#'@param ref_fishingunits fishing units reference dataset
#'@param ref_species species reference dataset
#'@param year year
#'@param month month
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
  
  group_cols = c("EST_YEAR", "EST_MONTH", "EST_BGC")
  
  #run
  effort<-data_effort|>
    dplyr::rename(EST_YEAR = "year",
                  EST_MONTH = "month",
                  EST_BGC = "fishing_unit") |>
    dplyr::mutate(EST_BGC = as.character(.data$EST_BGC)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols)))|>
    dplyr::summarise(
      EST_EFF_NSMP = length(.data$days_sampled),
      EST_EFF_NBDAYS = length(unique(.data$days)),
      EST_EFF_SRVTYPE = as.integer(unique(.data$effort_type)),
      EST_EFF_APPROACH = ifelse(.data$EST_EFF_SRVTYPE==7,"WEEKLY","To DO"),
      EST_EFF_ACTDAYS = sum(.data$days_sampled,na.rm = T),
      EST_EFF_EXDAYS = .data$EST_EFF_NSMP * .data$EST_EFF_SRVTYPE,
      EST_EFF_PBA = .data$EST_EFF_ACTDAYS / .data$EST_EFF_EXDAYS,
      EST_EFF_NACT = unique(.data$NB_ACTIVE_DAYS),
      EST_EFF_NBOATS = unique(.data$BG),
      EST_EFF_POP = .data$EST_EFF_NBOATS * .data$EST_EFF_NACT,
      EST_EFF_EFFORT=.data$EST_EFF_PBA * .data$EST_EFF_NBOATS * .data$EST_EFF_NACT,
      mean=mean(.data$days_sampled,na.rm=T),
      sd = stats::sd(.data$days_sampled,na.rm=T),
      se = .data$sd / sqrt(.data$EST_EFF_NSMP),
      EST_EFF_CV= .data$se / .data$mean,
      EST_EFF_SPAACCUR = artfish_accuracy(n = .data$EST_EFF_NSMP, N = .data$EST_EFF_NBOATS*4, method = "higher"),
      EST_EFF_TMPACCUR = 1,
      EST_EFF_SUI = unif_index(.data$days)
    ) |>
    dplyr::ungroup() |>
    dplyr::left_join(ref_fishingunits, by = "EST_BGC") |>
    dplyr::select(
      "EST_YEAR",
      "EST_MONTH",
      "EST_BGC",
      "EST_BGC_NAME",
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
      "EST_EFF_APPROACH",
      "EST_EFF_SPAACCUR",
      "EST_EFF_TMPACCUR",
      "EST_EFF_POP",
      "EST_EFF_EFFORT"
    )
  
  ####Landing
  landing <- data_landing |>
    dplyr::rename(EST_YEAR = "year",
                  EST_MONTH = "month",
                  EST_BGC = "fishing_unit") |>
    dplyr::mutate(EST_BGC = as.character(.data$EST_BGC)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_cols,"days","id"))))|>
    dplyr::summarise(quantity=sum(.data$quantity,na.rm = T),value=sum(.data$value,na.rm=T),price=mean(.data$price,na.rm=T)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols)))|>
    dplyr::mutate(quantity = replace(.data$quantity,is.na(.data$quantity), 0)) |>
    dplyr::summarise(
      EST_LND_NDAYS = length(unique(.data$days)),
      EST_LND_SMPCATCH = sum(.data$quantity),
      EST_LND_NSMP = length(.data$quantity),
      EST_LND_CPUE_G = .data$EST_LND_SMPCATCH / .data$EST_LND_NSMP,
      sd = stats::sd(.data$quantity,na.rm=T),
      se = .data$sd / sqrt(.data$EST_LND_NSMP),
      EST_LND_CV = .data$se / .data$EST_LND_CPUE_G,
      EST_LND_SUI = unif_index(.data$days)
    ) |>
    dplyr::ungroup() |>
    dplyr::left_join(ref_fishingunits, by = "EST_BGC") |>
    select(
      "EST_YEAR",
      "EST_MONTH",
      "EST_BGC",
      "EST_BGC_NAME",
      "EST_YEAR",
      "EST_LND_NDAYS",
      "EST_LND_SMPCATCH",
      "EST_LND_NSMP",
      "EST_LND_CPUE_G",
      "EST_LND_CV",
      "EST_LND_SUI"
    )
  
  estimate <- effort |>
    dplyr::left_join(landing) |>
    dplyr::mutate(EST_BGC = as.character(.data$EST_BGC)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols)))|>
    dplyr::mutate(
      EST_LND_CATCH_G = .data$EST_EFF_EFFORT * .data$EST_LND_CPUE_G,
      EST_LND_SPAACCUR = artfish_accuracy(n = .data$EST_LND_NSMP, N = .data$EST_EFF_POP, method = "higher"),
      EST_LND_TMPACCUR = artfish_accuracy(n = .data$EST_LND_NDAYS, N = .data$EST_EFF_NACT, method="higher"),
      EST_ACCUR = min(.data$EST_EFF_SPAACCUR, .data$EST_EFF_TMPACCUR, .data$EST_LND_SPAACCUR, .data$EST_LND_TMPACCUR, na.rm = T)
    )
  
  estimate <- data_landing |>
    dplyr::rename(EST_YEAR = "year",
                  EST_MONTH = "month",
                  EST_BGC = "fishing_unit",
                  EST_SPC = "species") |>
    dplyr::mutate(EST_SPC = as.character(.data$EST_SPC)) |>
    dplyr::mutate(EST_BGC = as.character(.data$EST_BGC)) |>
    dplyr::left_join(ref_species, by = "EST_SPC") |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols)))|>
    dplyr::filter(!is.na(.data$EST_SPC)) |>
    dplyr::group_by(.data$EST_YEAR, .data$EST_MONTH, .data$EST_BGC, .data$EST_SPC, .data$EST_SPC_NAME) |>
    dplyr::summarise(n = sum(.data$quantity), EST_LND_NOFISH = sum(.data$number) , EST_LND_PRICE = mean(.data$price)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols)))|>
    dplyr::mutate(sum=sum(.data$n),ratio=.data$n/.data$sum,EST_NOSPE = length(unique(.data$EST_SPC))) |>
    dplyr::select(-c("n","sum")) |>
    dplyr::left_join(.data$estimate) |>
    dplyr::ungroup() |>
    dplyr::mutate(EST_LND_CPUE = .data$EST_LND_CPUE_G * .data$ratio,
                  EST_LND_CATCH = .data$EST_EFF_EFFORT * .data$EST_LND_CPUE,
                  EST_LND_VALUE = .data$EST_LND_CATCH * .data$EST_LND_PRICE,
                  EST_LND_AVW = .data$EST_LND_CATCH / .data$EST_LND_NOFISH) |>
    dplyr::select(-"ratio") |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols)))|>
    dplyr::mutate(EST_LND_VALUE_G=sum(.data$EST_LND_VALUE,na.rm = T)) |>
    dplyr::ungroup() |>
    dplyr::mutate(EST_LND_PRICE_G = .data$EST_LND_VALUE_G / .data$EST_LND_CATCH_G)
  
  estimate <- estimate |>
    dplyr::select(
      "EST_YEAR",
      "EST_MONTH",
      "EST_BGC",
      "EST_BGC_NAME",
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
      "EST_EFF_APPROACH",
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
      "EST_SPC_NAME",
      "EST_LND_NOFISH",
      "EST_LND_CATCH",
      "EST_LND_CPUE",
      "EST_LND_VALUE",
      "EST_LND_PRICE",
      "EST_LND_AVW"
    )
  
  return(estimate)
}
