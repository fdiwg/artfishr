#'@name artfishr_shiny_server
#'@title Artfishr shiny server function
artfish_shiny_server <- function(input, output, session) {
  
}

#'@name artfishr_shiny_ui
#'@title Artfishr shiny ui function
artfish_shiny_ui <- function(id){
  shiny::fluidPage(
    h3("Artfish R shiny application",tags$small(" powered by 'artfishr' R package")),
    hr(),
    "COMING SOON"
  )
}


#'@name run_artfishr_shiny
#'@title Runs a Artfishr Shiny application
#'@export
run_artfish_shiny <- function(...){
  shiny::shinyApp(
    server = artfish_shiny_server,
    ui = artfish_shiny_ui,
    ...)
}