testthat::test_that("artfishr",{
  
  active_vessels = readr::read_csv(system.file("extdata/samples", "active_vessels.csv", package = "artfishr"))
  effort = readr::read_csv(system.file("extdata/samples", "effort.csv", package = "artfishr"))
  active_days = readr::read_csv(system.file("extdata/samples", "active_days.csv", package = "artfishr"))
  landings = readr::read_csv(system.file("extdata/samples", "landings.csv", package = "artfishr"))
  
  
  #activity coefficient
  activity_coefficient = artfishr::compute_effort_activity_coefficient(
    effort = effort,
    effort_source = "fisher_interview",
    minor_strata = "minor_stratum"
  )
  
  #effort estimate (includes calculation of activity coefficient)
  effort_estimate = artfishr::compute_effort_estimate(
    active_vessels = active_vessels, 
    active_vessels_strategy = "latest", 
    effort = effort, 
    effort_source = "fisher_interview", 
    active_days = active_days,
    minor_strata = "minor_stratum"
  )
  
  #cpue
  cpue = artfishr::compute_cpue(landings, minor_strata = "minor_stratum")
  
  #catch estimate
  catch_estimate = artfishr::compute_catch_estimate(effort_estimate, cpue)
  
  #catch estimate by species
  catch_estimate_by_species = artfishr::compute_catch_estimates_by_species(landings, catch_estimate)
  
})