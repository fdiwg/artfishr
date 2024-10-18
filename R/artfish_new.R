#'@name artfish_new
#'@title artfish_new
#'
#'@param active_vessels active vessels
#'@param effort effort
#'@param effort_source effort source whether it's derived from survey -B1- (fishers interviews) 
#'or registry -B2- (boat counting)
#'@param active_days active days
#'@param landings landings
#'@param minor_strata minor_strata (to investigate further later)
#'
#'@return the result of Artfish
#'@export
#'
artfish_new <- function(
  active_vessels = NULL,
  effort = NULL,
  effort_source = c("survey", "registry"),
  active_days = NULL,
  landings = NULL, 
  minor_strata = NULL){
  
  #TODO by sur artfish_new_by_period
  

}

#'@name artfish_new_by_period
#'@title artfish_new_by_period
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
artfish_new_by_period <- function(
  year = NULL, month = NULL,  
  active_vessels,
  effort,
  effort_source = c("survey", "registry"),
  active_days = NULL,
  landings,
  minor_strata = NULL){
  
  #mandatory columns: year, month, (minor_stratum), fishing_unit
  
  #verify data availability
  #active_vessels NOT NULL (through arg)
  #effort NOT NULL (through arg)
  #active_days IF NULL then will need autogenerate it
  #landings NOT NULL (through arg)
  
  #verify structure for A/B/C/D components (delegated to vrule)
  #structure (B1/B2) will depend on the effort source
  
  #identify strata (that may include minor stratum)
  #-> columns that identify dimensions for grouping
  #examples
  #- year/month/fishing_unit (minimum requirement)
  #- year/month/(additional minor stratum)/fishing_unit
  
  #verify that year/month is ok on all tables (except eventually active_days IF NULL)
  #filter on these year/month (if there is more, raise a warning to alert user)
  #if active_days was NULL, then generate the equivalent table for the reference period year/month
  
  #verify that year/month/(minor_stratum)/fishing_unit are the same across all tables
  
  out <- NULL
  
  #CURRENT CODE?
  # focus on result (without any step attempts)
  
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
  # if(nrow(errors)>0){
  #   attr(out, "errors") <- errors
  # }
  return(out)
}