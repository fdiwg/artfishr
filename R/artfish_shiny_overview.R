#' @name artfish_shiny_overview_server
#' @title ARTFISH Overview - Shiny Server Module
#' @description
#' Server-side logic for the ARTFISH Overview dashboard.
#'
#' The *"Total catch and effort - Statistics at country level"* module provides a high-level visualization of key output metrics derived from \code{artfish_compute_report}. 
#' It acts as a dashboard to track the overall evolution of fishing activity over time, with the ability to filter by fishing units and time range.
#'
#' The module is composed of:
#' \itemize{
#'   \item A selector panel to adjust the time range and fishing units to display
#'   \item A set of KPIs presenting aggregated country-level metrics for the selected period
#'   \item Seven boxed plots powered by \code{fdishinyr::generic_chart} allowing users to analyse:
#'   \itemize{
#'     \item (1) Total catch estimation
#'     \item (2) Species composition (by quantity)
#'     \item (3) Total nominal effort
#'     \item (4) Number of active vessels
#'     \item (5) Total value estimation
#'     \item (6) Species composition (by value)
#'     \item (7) Catch per unit effort (CPUE)
#'   }
#' }
#'
#' @param id Character string. Module namespace identifier.
#' 
#' @param lang Optional language parameter.
#' Can be either:
#' \itemize{
#'   \item a character string (static language), or
#'   \item a reactive returning a character string (dynamic language)
#' }
#' If \code{NULL}, the current global language is used.
#' Default is \code{NULL}
#'
#' @param estimate A reactive data frame aggregating the output of \code{artfish_compute}, enriched with human-readable labels.
#'
#' @param effort_source Reactive Character string indicating the type of effort source.
#' Must be either \code{"fisher_interview"} or \code{"boat_counting"}.
#' Not activated
#'
#' @param minor_strata Reactive Character string targeting a column name considered as minor strata.
#' Not activated
#' 
#' @param opts a named list of options. 
#' For now limited to:
#' - \code{refresh_ui} that gives the capacity to inject a refresh UI button (for dynamic computation)
#' - \code{values_ui} that allows to hide the UI related to value measurement
#'
#' @export

artfish_shiny_overview_server <- function(id, lang = NULL, estimate, effort_source = NULL, minor_strata = NULL, opts = list()){
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    INFO("Start - Artfish siny Overview server Module")
    # -------------------------------------------------------------------------
    # i18n handling
    # - Supports static language (lang = "en")
    # - Supports reactive language (lang = reactive)
    # -------------------------------------------------------------------------
    
    i18n_translator <- reactive({
      if(is.reactive(lang)){
        set_translation_language(lang())
      }else{
        if(!is.null(lang)) set_translation_language(lang)
      }
      translator()
    })
    
    # Simple wrapper around translator$t()
    i18n <- function(key){
      i18n_translator()$t(key)
    }
    
    # -------------------------------------------------------------------------
    # Reactives
    # -------------------------------------------------------------------------
    data_bg<-reactiveVal(NULL)
    
    # -------------------------------------------------------------------------
    # Options
    # -------------------------------------------------------------------------
    values_ui <- if(!is.null(opts$values_ui)) opts$values_ui else TRUE
    
    # -------------------------------------------------------------------------
    # Case if no data to display
    # -------------------------------------------------------------------------
    #UI to indicate if there is no release
    output$no_release<-renderUI({
      div(
        if(nrow(estimate())>0){
          NULL
        }else{
          p(i18n("OVERVIEW_NO_RELEASE"))
        }
      )
    })
    
    observe({
      req(nrow(estimate()) > 0)
      data_bg(estimate())
    })
    
    # -------------------------------------------------------------------------
    # UI Selectors
    # -------------------------------------------------------------------------
    #time selector UI
    output$time_selector <- renderUI({
      sliderInput(
        ns("time"),
        label = i18n("OVERVIEW_TIME_SLIDER_LABEL"),
        min = min(estimate()$date, na.rm = TRUE),
        max = max(estimate()$date, na.rm = TRUE),
        value = c(
          min(estimate()$date, na.rm = TRUE),
          max(estimate()$date, na.rm = TRUE)
        ),
        timeFormat = "%b %Y"
      )
    })
    
    #fishing unit UI
    output$fishing_unit_selector <- renderUI({
      
      ref_bg_sp <- estimate()|>select(fishing_unit,fishing_unit_label)|>distinct()
      
      choices <- setNames(ref_bg_sp$fishing_unit, ref_bg_sp$fishing_unit_label)
      
      shinyWidgets::pickerInput(
        inputId = ns("fishing_unit"),
        label   = i18n("OVERVIEW_FISHING_UNIT_SELECTOR_LABEL"),
        choices = choices,
        selected = ref_bg_sp$fishing_unit,
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          `select-all-text` = i18n("OVERVIEW_FISHING_UNIT_SELECTOR_SELECT_ALL"),
          `deselect-all-text` = i18n("OVERVIEW_FISHING_UNIT_SELECTOR_DESELECT_ALL"),
          `selected-text-format` = "count > 3",
          `count-selected-text` = paste0("{0} ",i18n("OVERVIEW_FISHING_UNIT_SELECTOR_SELECTED")),
          `none-selected-text` = i18n("OVERVIEW_FISHING_UNIT_SELECTOR_NO_SELECTION"),
          `live-search` = TRUE
        )
      )
    })
    
    #UI for selectors (time, fishing_unit)
    output$filter_selectors <- renderUI({
      fluidRow(
        column(4,uiOutput(ns("time_selector")),offset = 1),
        column(4,uiOutput(ns("fishing_unit_selector")),offset = 1)
      )
    })
    
    # -------------------------------------------------------------------------
    # Data filtering
    # -------------------------------------------------------------------------
    
    #Process data based on fishing_unit/time selection
    observeEvent(c(input$fishing_unit,input$time), {
      
      req(!is.null(estimate()))
      
      data<-estimate()|>
        filter(
          date >= input$time[1],
          date <= input$time[2]
        )
      
      if (length(input$fishing_unit) == 0) {
        selection <- data[0, ]
      } else {
        selection <- subset(
          data,
          fishing_unit %in% input$fishing_unit
        )
      }
      
      selection <- selection[, c(
        "year",
        "month",
        "date",
        "fishing_unit",
        "fishing_unit_label",
        "species_label",
        "effort_nominal",
        "fleet_engagement_number",
        "catch_nominal_landed",
        "trade_value",
        "catch_cpue"
      )]
      
      data_bg(selection)
    })
    
    # -------------------------------------------------------------------------
    # UI Outputs and Rendering
    # -------------------------------------------------------------------------
    
    #Indicators and timeline
    observeEvent(data_bg(),{
      req(!is.null(data_bg()))
      data <- data_bg()|>
        ungroup()
      
      data_effort<-data|>
        select(date,fishing_unit,fishing_unit_label,effort_nominal,fleet_engagement_number) |>
        distinct() |>
        ungroup()
      
      total_effort<-data_effort|>
        summarise(effort_nominal=sum(effort_nominal,na.rm=T))
      
      total_catch<-data|>
        summarise(catch_nominal_landed=sum(catch_nominal_landed,na.rm=T),
                  trade_value=sum(trade_value,na.rm=T))
      
      # ===== UI Indicators (KPI) =====
      output$indicators <- renderUI({
        fluidRow(
          bs4InfoBox(
            title = i18n("OVERVIEW_INFOBOX_CATCH_TITLE"),
            value = sprintf("%s (%s)",formatC(total_catch$catch_nominal_landed, format = "f", digits = 0, big.mark = "\u202F"),i18n("OVERVIEW_INFOBOX_CATCH_UNIT")),
            icon = icon("fish"),
            color = "primary",
            width = if(values_ui) 4 else 6
          ),
          bs4InfoBox(
            title = i18n("OVERVIEW_INFOBOX_EFFORT_TITLE"),
            value = sprintf("%s (%s)",formatC(total_effort$effort_nominal, format = "f", digits = 0, big.mark = "\u202F"),i18n("OVERVIEW_INFOBOX_EFFORT_UNIT")),
            icon = icon("clock"),
            color = "primary",
            width = if(values_ui) 4 else 6
          ),
          if(values_ui){
            bs4InfoBox(
              title = i18n("OVERVIEW_INFOBOX_VALUE_TITLE"),
              value = sprintf("%s (%s)",formatC(total_catch$trade_value, format = "f", digits = 0, big.mark = "\u202F"),i18n("OVERVIEW_INFOBOX_VALUE_UNIT")),
              icon = icon("clock"),
              color = "primary",
              width = 4
            )
          }
        )
      })
      
      # ===== UI Chart box =====
      
      #Fleet engagement Plot
      fdishinyr::generic_chart_server(
        id = "boats",
        lang = appConfig$language,
        df = data_effort,
        col_date = "date",
        col_group = "fishing_unit_label",
        col_value = "fleet_engagement_number",
        time_label = "",
        value_label = i18n("OVERVIEW_PLOT_BOATS_VALUE_LABEL"),
        group_label = i18n("OVERVIEW_PLOT_BOATS_GROUP_LABEL"),
        stat = "sum",
        plot_type_default = 'bar_stack'
      )

      #Effort plot
      fdishinyr::generic_chart_server(
        id = "effort",
        lang = appConfig$language,
        df = data_effort,
        col_date = "date",
        col_group = "fishing_unit_label",
        col_value = "effort_nominal",
        time_label = "",
        value_label = i18n("OVERVIEW_PLOT_EFFORT_VALUE_LABEL"),
        group_label = i18n("OVERVIEW_PLOT_EFFORT_GROUP_LABEL"),
        stat = "sum",
        plot_type_default = 'bar_stack'
      )
      
      #Catch plot
      fdishinyr::generic_chart_server(
        id = "catch",
        lang = appConfig$language,
        df = data,
        col_date = "date",
        col_group = "fishing_unit_label",
        col_value = "catch_nominal_landed",
        stat = "sum",
        plot_type_default = 'bar_stack',
        time_label = "",
        value_label = i18n("OVERVIEW_PLOT_CATCH_VALUE_LABEL"),
        group_label = i18n("OVERVIEW_PLOT_CATCH_GROUP_LABEL")
      )
      
      #Catch species composition plot
      fdishinyr::generic_chart_server(
        id = "catch_sp",
        lang = appConfig$language,
        df = data,
        col_date = "date",
        col_group = "species_label",
        col_value = "catch_nominal_landed",
        stat = "sum",
        time_label = "",
        value_label = i18n("OVERVIEW_PLOT_CATCH_SP_VALUE_LABEL"),
        group_label = i18n("OVERVIEW_PLOT_CATCH_SP_GROUP_LABEL"),
        plot_types = c("rank_sum")
      )
      
      #Value species composition plot
      if(values_ui) fdishinyr::generic_chart_server(
        id = "value_sp",
        lang = appConfig$language,
        df = data,
        col_date = "date",
        col_group = "species_label",
        col_value = "trade_value",
        stat = "sum",
        time_label = "",
        value_label = i18n("OVERVIEW_PLOT_VALUE_SP_VALUE_LABEL"),
        group_label = i18n("OVERVIEW_PLOT_VALUE_SP_GROUP_LABEL"),
        plot_types = c("rank_sum")
      )
      
      #Value plot
      if(values_ui) fdishinyr::generic_chart_server(
        id = "value",
        lang = appConfig$language,
        df = data,
        col_date = "date",
        col_group = "fishing_unit_label",
        col_value = "trade_value",
        time_label = "",
        value_label = i18n("OVERVIEW_PLOT_VALUE_VALUE_LABEL"),
        group_label = i18n("OVERVIEW_PLOT_VALUE_GROUP_LABEL"),
        stat = "sum",
        plot_type_default = 'bar_stack'
      )
      
      #CPUE plot
      fdishinyr::generic_chart_server(
        id = "cpue",
        lang = appConfig$language,
        df = data,
        col_date = "date",
        col_group = "fishing_unit_label",
        col_value = "catch_cpue",
        time_label = "",
        value_label = i18n("OVERVIEW_PLOT_CPUE_VALUE_LABEL"),
        group_label = i18n("OVERVIEW_PLOT_CPUE_GROUP_LABEL"),
        stat = "mean",
        time_choices = "month"
      )
      
      #UI to render the generic charts
      output$results<-renderUI({

        tagList(
          fluidRow(
            column(6,
                   fdishinyr::generic_chart_ui(ns("catch"),title=i18n("OVERVIEW_PLOT_CATCH_TITLE"),sliderWidth =25)
            ),
            column(6,
                   fdishinyr::generic_chart_ui(ns("catch_sp"),title=i18n("OVERVIEW_PLOT_CATCH_SP_TITLE"),sliderWidth =25)
            )
          ),
          fluidRow(
            column(6,
                   fdishinyr::generic_chart_ui(ns("effort"),title=i18n("OVERVIEW_PLOT_EFFORT_TITLE"),sliderWidth =25)
            ),
            column(6,
                   fdishinyr::generic_chart_ui(ns("boats"),title=i18n("OVERVIEW_PLOT_BOATS_TITLE"),sliderWidth =25)
            )
          ),
          if(values_ui) fluidRow(
            column(6,
                   fdishinyr::generic_chart_ui(ns("value"),title=i18n("OVERVIEW_PLOT_VALUE_TITLE"),sliderWidth =25)
            ),
            column(6,
                   fdishinyr::generic_chart_ui(ns("value_sp"),title=i18n("OVERVIEW_PLOT_VALUE_SP_TITLE"),sliderWidth =25)
            )
          ),
          fluidRow(
            column(12,        
                   fdishinyr::generic_chart_ui(ns("cpue"),title=i18n("OVERVIEW_PLOT_CPUE_TITLE"),sliderWidth =25)
            )
          )
        )
      })
    })
    
    #main UI
    output$main <- renderUI({
      tagList(
        fluidRow(
          column(
            width = 12, style = "margin:12px;",
            if(!is.null(opts$refresh_ui)){ tags$div(opts$refresh_ui, style = "float:right;") },
            tags$h2(i18n("OVERVIEW_TITLE")),
            tags$h3(class = "text-muted", i18n("OVERVIEW_SUBTITLE"))
          )
        ),
        uiOutput(ns("no_release")),
        uiOutput(ns("filter_selectors")),
        uiOutput(ns("indicators")),
        uiOutput(ns("results"))
      )
      })
    
    INFO("End - Artfish siny Overview server Module")
  })
  
}

#' @name artfish_shiny_overview_ui
#' @title ARTFISH Overview - Shiny UI Module
#' @description User interface for the ARTFISH Overview Dashboard
#' @param id Character string. Module namespace identifier.
#' @export

artfish_shiny_overview_ui <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("main"))
  
}
