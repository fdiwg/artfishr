#'@name artfish_shiny_welcome_server
#'@title POC shiny module server
#'@description POC shiny module server
#'@param id id
#'@param lang lang a reactive version of the language. Default is \code{NULL} (optional)
#'@export
artfish_shiny_welcome_server <- function(id, lang = NULL){
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    #i18n translation mechanism
    #functional for both static language set-up (lang = NULL) or dynamic language set-up
    #(case where the language is passed as reactive)
    i18n_translator <- reactive({
      if(is.reactive(lang)){
        set_translation_language(lang())
      }else{
        if(!is.null(lang)) set_translation_language(lang)
      }
      artfishr::INFO("Welcome to artfishr ecosystem in lang '%s'", translator()$get_translation_language())
      translator()
    })
    
    #i18n util function
    #The function wraps the translator translation (t) function
    i18n <- function(key){
      i18n_translator()$t(key)
    }
    
    output$main <- renderUI({
      tagList(
        bs4Dash::box(
          title = i18n("WELCOME"),
          width = 12,
          status = "info",
          solidHeader = TRUE,
          tags$p(i18n("WELCOME_PARAGRAPH"))
        ),
        hr(),
        #here we call a module from fdishinyr (fdishinyr/welcome)
        #since we call it from within a module (artfishr/artfish_shiny_welcome)
        #we MUST use the namespace
        fdishinyr::welcome_ui(ns("welcome_from_fdishinyr")) 
      )
    })
    
    #here the fdishinyr/welcome module is loaded based on lang reactive
    #the lang argument is optional, so in case of a module for which the lang is not reacting (fixed for the app)
    #the module can be called directly in the server: fdishinyr::welcome_server("welcome_from_fdishinyr")
    #Below code show both cases. For test apps see tests/testthat/test_shiny.R
    if(!is.null(lang)){
      if(is.reactive(lang)){
        observeEvent(lang(),{
          fdishinyr::welcome_server("welcome_from_fdishinyr", lang = reactive({ lang() }))
        })
      }else{
        fdishinyr::welcome_server("welcome_from_fdishinyr", lang = lang)
      }
    }else{
      fdishinyr::welcome_server("welcome_from_fdishinyr")
    }
  })
  
}

#'@name artfish_shiny_welcome_ui
#'@title POC shiny module UI
#'@description POC shiny module UI
#'@param id id
#'@export
artfish_shiny_welcome_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("main"))
}