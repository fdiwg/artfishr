#'@name compute_effort_estimate_with_IRD_Pechart
#'@title Computes nominal effort estimate
#'@description TBD
#'@author Johanna Herfaut
#'@export
compute_effort_estimate_with_IRD_Pechart <- function(
    effort,
    effort_source = c("household_interview"),
    landings,
    active_days,
    census_typology,
    minor_strata = NULL,
    progress_fn = NULL){
  
  #___________________________________________________________________________________________________________________________________________________________
  # 2. Effort: per province per type and per fishing unit
  #___________________________________________________________________________________________________________________________________________________________
  #___________________________________________________________________________________________________________________________________________________________
  # 2.1 Number of households in the census per province per type
  #___________________________________________________________________________________________________________________________________________________________
  strata_0 = c("year", "month")
  strata_1 = c("year", "month", "household_type")
  strata_2 = c("year", "month", "household_id")
  strata_3 = c("year", "month", "fishing_unit", "household_type")
  if(!is.null(minor_strata)){
    strata_0 = c("year", "month", minor_strata)
    strata_1 = c("year", "month", minor_strata, "household_type")
    strata_2 = c("year", "month", minor_strata, "household_id")
    strata_3 = c("year", "month", minor_strata, "fishing_unit", "household_type")
  }
  strata_4 = strata_3[-length(strata_3)]
  
  # Number of households in the census per province
  hh_typo_minor_stratum <- census_typology |>
    dplyr::group_by_at(strata_1) |> #by year/month/(minor_strata)/household_type
    dplyr::summarise(household_number = sum(household_number)) |>
    dplyr::ungroup()
  
  #___________________________________________________________________________________________________________________________________________________________
  # 2.2 Number of households in the sample per province per type (for the selected sites)
  #___________________________________________________________________________________________________________________________________________________________
  
  # Select hh_effort where fishing_day = 1
  hh_effort <- subset(effort, effort$fishing_day==1)
  
  # Number of households selected in the sample per category per province
  hh_typo_sample <- hh_effort |>
    dplyr::group_by_at(strata_1) |> #by year/month/(minor_strata)/household_type
    dplyr::summarise(household_sample_size = dplyr::n_distinct(household_id)) |>
    dplyr::ungroup()
  
  #___________________________________________________________________________________________________________________________________________________________
  # 2.3. Number of sampled days in the month per year per month per province
  #___________________________________________________________________________________________________________________________________________________________
  
  hh_effort$interview_date <- lubridate::make_date(
    year = hh_effort$year,
    month = hh_effort$month,
    day = hh_effort$day
  )
  
  sampled_days_number<-hh_effort |>
    dplyr::group_by_at(strata_2) |> #by year/month/(minor_strata)/household_id
    dplyr::summarise(sampled_days_number = dplyr::n_distinct(interview_date)) |>
    dplyr::ungroup()
  
  sampled_days_number <- sampled_days_number |>
    dplyr::group_by_at(strata_0) |> #by year/month/(minor_strata)
    dplyr::summarise(sampled_days_number = max(sampled_days_number)) |>
    dplyr::ungroup()
  
  #___________________________________________________________________________________________________________________________________________________________
  # 2.4 Number of days in the month per year per month
  #___________________________________________________________________________________________________________________________________________________________
  
  month_days <- hh_effort |>
    dplyr::distinct(year, month) |>   # keep only the moth that are present
    dplyr::mutate(
      days_month_number = lubridate::days_in_month(lubridate::make_date(year, month, 1))
    ) |>
    dplyr::arrange(year, month)
  
  # or recycle function generate_active_days but arguments are different
  # generate_active_days(active_vessels, effort, effort_source, landings, minor_stratum)
  # year, month de interview_date à créer au début de la fonction dans le cas de effort_source="household_interview"
  
  #___________________________________________________________________________________________________________________________________________________________
  # 2.5. Number of days at sea per province per fishing unit per type
  #___________________________________________________________________________________________________________________________________________________________
  
  # aggregated data per fihsing unit
  day_at_sea <- hh_effort |>
    dplyr::group_by_at(strata_3) |> #by year/month/fishing_unit/(minor_strata)/household_type
    dplyr::summarise(effort_sample_size = dplyr::n_distinct(interview_date, household_id),
              effort_coefficient_variation= sd(effort_fishing_duration, na.rm=TRUE)/ mean(effort_fishing_duration, na.rm=TRUE)*100,
              effort_fishing_duration = sum(effort_fishing_duration))|>
    dplyr::ungroup()
  
  #___________________________________________________________________________________________________________________________________________________________
  # 2.6. Calculation of effort per fishing unit per province (without gears)
  #___________________________________________________________________________________________________________________________________________________________
  # combine the 5 elements of the formula to calculate the total effort estimate
  
  day_at_sea$household_number <- hh_typo_minor_stratum$household_number[match(paste0(day_at_sea$minor_stratum,"-",day_at_sea$household_type),paste0(hh_typo_minor_stratum$minor_stratum,"-",hh_typo_minor_stratum$household_type))]
  
  day_at_sea$household_sample_size <- hh_typo_sample$household_sample_size[match(paste0(day_at_sea$minor_stratum,"-",day_at_sea$household_type),paste0(hh_typo_sample$minor_stratum,"-",hh_typo_sample$household_type))]
  
  day_at_sea$sampled_days_number <- sampled_days_number$sampled_days_number[match(paste0(day_at_sea$year,"-",day_at_sea$month),paste0(sampled_days_number$year,"-",sampled_days_number$month))]
  
  # à changer quand generate_active_day mis à jour
  day_at_sea$effort_fishable_duration <- month_days$days_month_number[match(paste0(day_at_sea$year,"-",day_at_sea$month),paste0(month_days$year,"-",month_days$month))]
  
  day_at_sea$effort <- day_at_sea$household_number * day_at_sea$effort_fishing_duration * day_at_sea$effort_fishable_duration / day_at_sea$sampled_days_number / day_at_sea$household_sample_size
  
  
  effort_estimate <- day_at_sea |>
    dplyr::group_by_at(c(strata_4, "effort_sample_size", "effort_coefficient_variation", "effort_fishable_duration")) |>
    dplyr::summarise(effort_nominal = sum(effort, na.rm = TRUE)) |>
    dplyr::ungroup()
  effort_estimate$effort_activity_coefficient <- NA
  effort_estimatefleet_engagement_number <- NA
  
  # effort_estimate: year, month, minor_stratum, fishing_unit, effort_sample_size, effort_coefficient_variation, effort_activity_coefficient, fleet_engagement_number, effort_fishable_duration, effort_nominal
  return(effort_estimate)
}