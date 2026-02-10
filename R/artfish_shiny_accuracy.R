#'@name artfish_shiny_accuracy_server
#'@title POC shiny module server
#'@description Accuracy toolbox shiny module server
#'@param id id
#'@param lang lang a reactive version of the language. Default is \code{NULL} (optional)
#'@export
artfish_shiny_accuracy_server <- function(id, lang = NULL){
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    #i18n translation mechanism
    #functional for both static language set-up (lang = NULL) or dynamic language set-up
    #(case where the language is passed as reactive)
    i18n_translator <- reactive({
      if(is.reactive(lang)) set_translation_language(lang())
      translator()
    })
    
    #i18n util function
    #The function wraps the translator translation (t) function
    i18n <- function(key){
      i18n_translator()$t(key)
    }
    
    output$button<-renderUI({
      if(!is.na(input$days)&!is.na(input$boats)&!is.na(input$effort_smp)&!is.na(input$effort_days_smp)&!is.na(input$landing_smp)&!is.na(input$landing_days_smp)){
        actionButton(ns("run"),i18n("ACTIONBUTTON_COMPUTE_LABEL"))}else{NULL}
    })
    
    iconChoice<-function(x){ifelse(x<0.9,"exclamation-triangle","check-circle")}
    colorChoice<-function(x){ifelse(x<0.9,"warning","success")}
    
    observeEvent(input$run,{
      pop<-input$boats*input$days
      SAE<-artfish_accuracy(n=input$effort_smp,N=pop,method="higher")
      TAE<-1
      SAC<-artfish_accuracy(n=input$landing_smp,N=pop,method="higher")
      TAC<-artfish_accuracy(n=input$landing_days_smp,N=input$days,method="higher")
      OAC<-min(SAE,TAE,SAC,TAC)
      output$result<-renderUI({
        tagList(
          fluidRow(
            valueBox(paste(format(round(SAE*100,1), nsmall = 1),"%"),i18n("VALUEBOX_TITLE_SPATIAL_ACCURACY_EFFORT"),width = 3,icon=icon(iconChoice(SAE)),color=colorChoice(SAE)),
            valueBox(paste(format(round(TAE*100,1), nsmall = 1),"%"),i18n("VALUEBOX_TITLE_TEMPORAL_ACCURACY_EFFORT"),width= 3,icon=icon(iconChoice(TAE)),color=colorChoice(TAE)),
            valueBox(paste(format(round(SAC*100,1), nsmall = 1),"%"),i18n("VALUEBOX_TITLE_SPATIAL_ACCURACY_CATCH"),width=3,icon=icon(iconChoice(SAC)),color=colorChoice(SAC)),
            valueBox(paste(format(round(TAC*100,1), nsmall = 1),"%"),i18n("VALUEBOX_TITLE_TEMPORAL_ACCURACY_CATCH"),width=3,icon=icon(iconChoice(TAC)),color=colorChoice(TAC))
          ),
          fluidRow(
            column(8,offset=4,valueBox(paste(format(round(OAC*100,1), nsmall = 1),"%"),i18n("VALUEBOX_TITLE_OVERALL_ACCURACY"),width=4,icon=icon(iconChoice(OAC)),color=colorChoice(OAC)))
          )
        )
      })
    })
    
    reactiveData<- reactiveVal()
    Day1 <- paste0(i18n("DAY_LABEL"),'1')
    table<-data.table(Day1=1)
    row.names(table)<- i18n("SAMPLES_LABEL")
    reactiveData(table)
    
    observeEvent(input$table_cell_edit, {
      info = input$table_cell_edit
      newData <- reactiveData()
      newData[info$row, info$col] <- info$value
      reactiveData(newData)
    })
    
    observeEvent(input$addColumn,{
      newData <- reactiveData()
      newData[[paste0(i18n("DAY_LABEL"),ncol(newData)+1)]] <- 1
      reactiveData(newData)
    })
    
    output$table<-renderDT(server = FALSE,{
      DT::datatable(
        reactiveData(),
        escape = FALSE,
        rownames = TRUE,
        selection = 'none',
        editable = 'cell',
        options = list(
          pageLength = 1, dom = 't', 
          autoWidth = TRUE,
          language = list(url = i18n("TABLE_LANGUAGE"))
        )
      )
    })
    
    observeEvent(input$compute,{
      newData <- reactiveData()
      
      samples<-as.numeric(newData[1,])
      nbdays<-length(samples)
      ratio<-ifelse(samples/mean(samples)>1,1,samples/mean(samples))
      index<-round(sum(ratio),0)/nbdays
      
      output$index<-renderUI({
        tagList(
          valueBox(index,i18n("VALUEBOX_TITLE_UNIFORMITY_INDEX"),icon=icon(ifelse(index<0.6,"exclamation-triangle","check-circle")),color=ifelse(index<0.6,"warning","success"),width = 3)
        )
      })
    })
    
    #main UI
    output$main <- renderUI({
      tagList(
        fluidRow(
          div(
            width = 12, style = "margin:12px;",
            
            tags$h2(i18n("ARTFISH_ACCURACY_TITLE")),
          )
        ),
        bs4Dash::tabsetPanel(
          tabPanel(i18n("TABPANEL_ACCURACY"),
                   fluidRow(
                     div(
                       class = "col-md-4",
                       bs4Dash::box(id=ns("global_box"),title=i18n("GLOBAL_BOX_TITLE"),width = 12,
                                    numericInputIcon(ns("boats"),i18n("NUMERIC_INPUT_NUMBER_OF_BOATS_TITLE"),value=NULL,icon=icon("ship")),
                                    numericInputIcon(ns("days"),i18n("NUMERIC_INPUT_NUMBER_OF_FISHING_DAYS_TITLE"),value=NULL,icon=icon("calendar")),
                                    collapsible = FALSE
                       )
                     ),
                     div(
                       class = "col-md-4",
                       bs4Dash::box(id=ns("effort_box"),title=i18n("EFFORT_BOX_TITLE"),width = 12,
                                    numericInputIcon(ns("effort_smp"),i18n("NUMERIC_INPUT_NUMBER_OF_EFFORT_SAMPLES_TITLE"),value=NULL,icon=icon("ship")),
                                    numericInputIcon(ns("effort_days_smp"),i18n("NUMERIC_INPUT_NUMBER_OF_DAYS_SAMPLED_EFFORTS_TITLE"),value=NULL,icon=icon("calendar")),
                                    collapsible = FALSE
                       )
                     ),
                     div(
                       class = "col-md-4",
                       bs4Dash::box(id=ns("landing_box"),title=i18n("LANDING_BOX_TITLE"),width = 12,
                                    numericInputIcon(ns("landing_smp"),i18n("NUMERIC_INPUT_NUMBER_OF_LANDING_SAMPLES_TITLE"),value=NULL,icon=icon("ship")),
                                    numericInputIcon(ns("landing_days_smp"),i18n("NUMERIC_INPUT_NUMBER_OF_DAYS_SAMPLED_LANDING_TITLE"),value=NULL,icon=icon("calendar")),
                                    collapsible = FALSE
                       )
                     )
                   ),
                   fluidRow(uiOutput(ns("button"))),
                   br(),
                   uiOutput(ns("result"))
          ),
          tabPanel(i18n("TABPANEL_UNIFORMITY"),
                   fluidRow(
                     p(i18n("UNIFORMITY_HINT")),
                     actionButton(ns("addColumn"), i18n("ACTIONBUTTON_ACTIVATE_NEWCOLUMN_LABEL")),
                     DTOutput(ns("table")),br(),
                     actionButton(ns("compute"), i18n("ACTIONBUTTON_COMPUTE_LABEL"))
                   ),br(),
                   fluidRow(
                     column(width = 6,
                            uiOutput(ns("index"))
                     )
                   )
          )
        )
      )
    })
    

  })
  
}

#'@name artfish_shiny_accuracy_ui
#'@title POC shiny module UI
#'@description POC shiny module UI
#'@param id id
#'@export
artfish_shiny_accuracy_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("main"))
}