
library(shiny)
library(bs4Dash)
library(artfishr)

testthat::test_that("Shiny app with static language",{
  ## Only run this example in interactive R sessions
  if (interactive()) {
    options(device.ask.default = FALSE)
    
    artfishr::set_translation_language("fr")
    
    # Apps can be run without a server.r and ui.r file
    runApp(list(
      ui = bs4Dash::dashboardPage(
        header = bs4Dash::dashboardHeader(
          title = bs4Dash::dashboardBrand(
            title = "My dashboard",
            color = "primary",
            href = "https://adminlte.io/themes/v3",
            image = "https://adminlte.io/themes/v3/dist/img/AdminLTELogo.png"
          ),
          rightUi = tags$li(
            class = "dropdown",
            uiOutput("app_language", inline = T)
          )
        ),
        sidebar = bs4Dash::dashboardSidebar(),
        body = bs4Dash::dashboardBody(
          #my welcome module UI invoked here
          artfishr::artfish_shiny_welcome_ui("welcome_you")
        ),
        controlbar = dashboardControlbar(),
        title = "DashboardPage"
      ),
      server = function(input, output) {
        artfishr::artfish_shiny_welcome_server("welcome_you")
      }
    ))
  } 
  
})

testthat::test_that("Shiny app with dynamic language",{
  
  ## Only run this example in interactive R sessions
  if (interactive()) {
    options(device.ask.default = FALSE)
    
    # Apps can be run without a server.r and ui.r file
    runApp(list(
      ui = bs4Dash::dashboardPage(
        header = bs4Dash::dashboardHeader(
          title = bs4Dash::dashboardBrand(
            title = "My dashboard",
            color = "primary",
            href = "https://adminlte.io/themes/v3",
            image = "https://adminlte.io/themes/v3/dist/img/AdminLTELogo.png"
          ),
          rightUi = tags$li(
            class = "dropdown",
            uiOutput("app_language", inline = T)
          )
        ),
        sidebar = bs4Dash::dashboardSidebar(),
        body = bs4Dash::dashboardBody(
          #my welcome module UI invoked here
          artfishr::artfish_shiny_welcome_ui("welcome_you")
        ),
        controlbar = dashboardControlbar(),
        title = "DashboardPage"
      ),
      server = function(input, output) {
        
        output$app_language <- renderUI({
          tags$div(
            selectInput(
              "selected_language", label = NULL,
              choices = setNames(
                fdishinyr::translator()$get_languages()[-1],
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
          artfishr::artfish_shiny_welcome_server("welcome_you", lang = reactive({input$selected_language}))
        })
        
      }
    ))
  } 
})