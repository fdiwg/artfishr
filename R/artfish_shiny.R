#'@name artfishr_shiny_server
#'@title Artfishr shiny server function
artfish_shiny_server <- function(input, output, session) {
  
  output$app_language <- renderUI({
    tags$div(
      selectInput(
        "selected_language", label = NULL,
        choices = setNames(
          artfishr::translator()$get_languages()[-1],
          c("العربية", "English", "Español", "Français","Русский","中文")
        ),
        selected = fdishinyr::translator()$get_translation_language(),
        width = "110px",
        
      ),
      style = "float:right;margin-left:5px;padding-top:2px;"
    )
  })
  
  #we render the welcome module based on dynamic language (provided by the selector)
  observeEvent(input$selected_language, {
    shiny.i18n::update_lang(input$selected_language)
    artfishr::artfish_shiny_welcome_server("welcome_from_artfishr", lang = reactive({input$selected_language}))
    artfishr::artfish_shiny_accuracy_server("accuracy_toolbox", lang = reactive({input$selected_language}))
  })
  
}

#'@name artfishr_shiny_ui
#'@title Artfishr shiny ui function
artfish_shiny_ui <- function(id){
  
  bs4Dash::dashboardPage(
    header = bs4Dash::dashboardHeader(
      title = bs4Dash::dashboardBrand(
        title = "Artfishr UI",
        color = "primary",
        href = "https://github.com/fdiwg/artfishr"
      ),
      rightUi = tags$li(
        class = "dropdown",
        uiOutput("app_language", inline = T)
      )
    ),
    sidebar = bs4Dash::dashboardSidebar(),
    body = bs4Dash::dashboardBody(
      h3("Artfish R shiny application",tags$small(" powered by 'artfishr' R package")),
      hr(),
      artfishr::artfish_shiny_welcome_ui("welcome_from_artfishr"),hr(),
      artfishr::artfish_shiny_accuracy_ui("accuracy_toolbox")
    ),
    controlbar = dashboardControlbar(),
    title = "DashboardPage"
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