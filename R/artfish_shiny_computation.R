#' @name artfish_shiny_computation_server
#' @title ARTFISH Computation - Shiny Server Module
#' @description
#' Server-side shiny logic to run Artfish computation as business service
#'
#' @param id Character string. Module namespace identifier.
#' @param refresh refresher for computation
#' @param effort effort data
#' @param effort_source Character string indicating the type of effort source.
#' @param active_vessels active vessels data
#' @param active_vessels_strategy active vessels strategy
#' @param active_days active days
#' @param landings landings
#' @param minor_strata Character string targeting a column name considered as minor strata.
#' @param progress_fn A function to monitor progress
#'
#' @export
artfish_shiny_computation_server <- function(
    id,
    refresh,
    effort, effort_source, 
    active_vessels, active_vessels_strategy,
    active_days,
    landings,
    minor_strata = NULL,
    progress_fn = NULL
) {
  moduleServer(id, function(input, output, session) {
    
    ready <- reactiveVal(FALSE)
    
    estimates <- reactive({
      refresh()

      # mark computation as running
      ready(FALSE)
      
      on.exit({
        # mark computation as finished (DB queries done)
        ready(TRUE)
      }, add = TRUE)
      
      
      isolate({
        artfishr::compute_report(
          effort = effort(),
          effort_source = effort_source(),
          active_vessels = active_vessels(),
          active_vessels_strategy = active_vessels_strategy(),
          active_days = active_days(),
          landings = landings(),
          minor_strata = if (is.null(minor_strata)) NULL else minor_strata(),
          progress_fn = progress_fn
        )
      })
    }) |>
      bindCache(list(
        effort(),
        effort_source(),
        active_vessels(),
        active_vessels_strategy(),
        active_days(),
        landings(),
        if (!is.null(minor_strata)) minor_strata()
      ))
    
    list(
      ready = ready,
      estimates = estimates,
      effort_source = effort_source,
      active_vessels_strategy = active_vessels_strategy,
      minor_strata = minor_strata
    )
  })
}
