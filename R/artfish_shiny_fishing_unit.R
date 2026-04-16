#' @name artfish_shiny_fishing_unit_server
#' @title ARTFISH Fishing Unit - Shiny Server Module
#' @description
#' Server-side logic for the ARTFISH Fishing Unit dashboard.
#'
#' The *"Catch and effort - Detailed by fishing unit"* module provides a dedicated workspace for exploring key output metrics derived from \code{artfish_compute_report}, with a focus on one or multiple fishing units.
#' It enables users to analyse the evolution of fishing activity over time, with filtering options for fishing units and time range.
#'
#' The module is composed of:
#' \itemize{
#'   \item A selector panel to adjust the time range and fishing units to display
#'   \item A set of KPIs presenting aggregated metrics for the selected period
#'   \item Three boxed plots powered by \code{fdishinyr::generic_chart} allowing users to analyse ranked top items for:
#'   \itemize{
#'     \item (1) Species composition (by quantity)
#'     \item (2) Fishing unit composition (by quantity)
#'     \item (3) Fishing unit composition (by value)
#'   }
#'   \item Six boxed plots powered by \code{fdishinyr::generic_chart} allowing users to analyse key Artfish metrics:
#'   \itemize{
#'     \item (1) Total catch estimation
#'     \item (2) Catch per unit effort (CPUE)
#'     \item (3) Total nominal effort
#'     \item (4) Activity coefficient
#'     \item (5) Number of active vessels
#'     \item (6) Total value estimation
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
#' @param effort_source Reactive haracter string indicating the type of effort source.
#' Must be either \code{"fisher_interview"} or \code{"boat_counting"}.
#'
#' @param minor_strata Reactive haracter string targeting a column name considered as minor strata.
#' Not activated
#'
#' @export

artfish_shiny_fishing_unit_server <- function(id, lang = NULL, estimate, effort_source, minor_strata = NULL){
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
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
    # Case if no data to display
    # -------------------------------------------------------------------------
    #UI to indicate if there is no release
    output$no_release<-renderUI({
      div(
        if(nrow(estimate())>0){
          NULL
        }else{
          p(i18n("FISHING_UNIT_NO_RELEASE"))
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
        label = i18n("FISHING_UNIT_TIME_SLIDER_LABEL"),
        min = min(estimate()$date, na.rm = TRUE),
        max = max(estimate()$date, na.rm = TRUE),
        value = c(
          min(estimate()$date, na.rm = TRUE),
          max(estimate()$date, na.rm = TRUE)
        ),
        timeFormat = "%b %Y"
      )
      
    })
    
    #fishing_unit selector UI
    output$fishing_unit_selector <- renderUI({
      
      ref_bg_sp <- estimate()|>select(fishing_unit,fishing_unit_label)|>distinct()
      choices <- setNames(ref_bg_sp$fishing_unit, ref_bg_sp$fishing_unit_label)
      
      shinyWidgets::pickerInput(
        inputId = ns("fishing_unit"),
        label   = i18n("FISHING_UNIT_FU_SELECTOR_LABEL"),
        choices = choices,
        selected = ref_bg_sp$fishing_unit,
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          `select-all-text` = i18n("FISHING_UNIT_FU_SELECTOR_SELECT_ALL"),
          `deselect-all-text` = i18n("FISHING_UNIT_FU_SELECTOR_DESELECT_ALL"),
          `selected-text-format` = "count > 3",
          `count-selected-text` = paste0("{0} ",i18n("FISHING_UNIT_FU_SELECTOR_SELECTED")),
          `none-selected-text` = i18n("FISHING_UNIT_FU_SELECTOR_NO_SELECTION"),
          `live-search` = TRUE
        )
      )
    })
    
    #UI for filter selectors (time, fishing_unit)
    output$filter_selectors <- renderUI({
      fluidRow(
        column(4,uiOutput(ns("time_selector")),offset = 1),
        column(4,uiOutput(ns("fishing_unit_selector")),offset = 1)
      )
    })
    
    # -------------------------------------------------------------------------
    # Data filtering
    # -------------------------------------------------------------------------
    
    #Process data and based on fishing_unit/time selection
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
      
      selection_cols = c(
        "year",
        "month",
        "date",
        "fishing_unit",
        "fishing_unit_label",
        "species_label",
        "effort_nominal",
        "effort_activity_coefficient",
        "effort_total_fishing_duration",
        "fleet_engagement_number",
        "catch_nominal_landed",
        "catch_nominal_landed_sampled",
        "trade_value",
        "catch_cpue"
      )
      if(effort_source() == "boat_counting"){
        #no effort_total_fishing_duration
        selection_cols = selection_cols[selection_cols != "effort_total_fishing_duration"]
      }
      
      selection <- selection[, selection_cols]
      
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
      
      data_effort_cols = c("date","fishing_unit","fishing_unit_label","effort_nominal","fleet_engagement_number","effort_activity_coefficient","effort_total_fishing_duration")
      data_effort<-data|>
        select(any_of(data_effort_cols)) |>
        distinct() |>
        ungroup()
      
      total_effort<-data_effort|>
        summarise(effort_nominal=sum(effort_nominal,na.rm=T),
                  fleet_engagement_number=sum(fleet_engagement_number,na.rm=T)
        )
      
      total_catch<-data|>
        summarise(catch_nominal_landed=sum(catch_nominal_landed,na.rm=T),
                  trade_value=sum(trade_value,na.rm=T)
        )
      
      # ===== UI Indicators (KPI) =====
      output$indicators <- renderUI({
        fluidRow(
          bs4InfoBox(
            title = i18n("FISHING_UNIT_INFOBOX_CATCH_TITLE"),
            value = sprintf("%s (%s)",formatC(total_catch$catch_nominal_landed, format = "f", digits = 0, big.mark = "\u202F"),i18n("FISHING_UNIT_INFOBOX_CATCH_UNIT")),
            icon = icon("fish"),
            color = "primary",
            width = 3
          ),
          bs4InfoBox(
            title = i18n("FISHING_UNIT_INFOBOX_VALUE_TITLE"),
            value = sprintf("%s (%s)",formatC(total_catch$trade_value, format = "f", digits = 0, big.mark = "\u202F"),i18n("FISHING_UNIT_INFOBOX_VALUE_UNIT")),
            icon = icon("fish"),
            color = "primary",
            width = 3
          ),
          bs4InfoBox(
            title = i18n("FISHING_UNIT_INFOBOX_EFFORT_TITLE"),
            value = sprintf("%s (%s)",formatC(total_effort$effort_nominal, format = "f", digits = 0, big.mark = "\u202F"),i18n("FISHING_UNIT_INFOBOX_EFFORT_UNIT")),
            icon = icon("clock"),
            color = "primary",
            width = 3
          ),
          bs4InfoBox(
            title = i18n("FISHING_UNIT_INFOBOX_BOAT_TITLE"),
            value = sprintf("%s (%s)",formatC(total_effort$fleet_engagement_number, format = "f", digits = 0, big.mark = "\u202F"),i18n("FISHING_UNIT_INFOBOX_BOAT_UNIT")),
            icon = icon("clock"),
            color = "primary",
            width = 3
          )
        )
      })
      
      # ===== UI Chart box =====
      
      #Catch per fishing unit plot
      fdishinyr::generic_chart_server(
        id = "catch_fu_tot",
        lang = appConfig$language,
        df = data,
        col_date = "date",
        col_group = "fishing_unit_label",
        col_value = "catch_nominal_landed",
        stat = "sum",
        time_label = "",
        value_label = i18n("FISHING_UNIT_PLOT_CATCH_FU_TOT_VALUE_LABEL"),
        group_label = i18n("FISHING_UNIT_PLOT_CATCH_FU_TOT_GROUP_LABEL"),
        plot_types = c("rank_sum","donut")
      )
      
      #Value per fishing unit plot
      fdishinyr::generic_chart_server(
        id = "value_fu_tot",
        lang = appConfig$language,
        df = data,
        col_date = "date",
        col_group = "fishing_unit_label",
        col_value = "trade_value",
        stat = "sum",
        time_label = "",
        value_label = i18n("FISHING_UNIT_PLOT_VALUE_FU_TOT_VALUE_LABEL"),
        group_label = i18n("FISHING_UNIT_PLOT_VALUE_FU_TOT_GROUP_LABEL"),
        plot_types = c("rank_sum","donut")
      )
      
      #Catch species composition per fishing unit plot
      fdishinyr::generic_chart_server(
        id = "catch_sp_tot",
        lang = appConfig$language,
        df = data,
        col_date = "date",
        col_group = "species_label",
        col_value = "catch_nominal_landed",
        stat = "sum",
        time_label = "",
        value_label = i18n("FISHING_UNIT_PLOT_CATCH_SP_TOT_VALUE_LABEL"),
        group_label = i18n("FISHING_UNIT_PLOT_CATCH_SP_TOT_GROUP_LABEL"),
        plot_types = c("rank_sum","donut")
      )
      
      #
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
        value_label = i18n("FISHING_UNIT_PLOT_CATCH_VALUE_LABEL"),
        group_label = i18n("FISHING_UNIT_PLOT_CATCH_GROUP_LABEL")
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
        value_label = i18n("FISHING_UNIT_PLOT_CPUE_VALUE_LABEL"),
        group_label = i18n("FISHING_UNIT_PLOT_CPUE_GROUP_LABEL"),
        stat = "mean",
        time_choices = "month"
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
        value_label = i18n("FISHING_UNIT_PLOT_EFFORT_VALUE_LABEL"),
        group_label = i18n("FISHING_UNIT_PLOT_EFFORT_GROUP_LABEL"),
        stat = "sum",
        plot_type_default = 'bar_stack'
      )
      
      #Activity Coefficient plot
      fdishinyr::generic_chart_server(
        id = "activity",
        lang = appConfig$language,
        df = data_effort,
        col_date = "date",
        col_group = "fishing_unit_label",
        col_value = "effort_activity_coefficient",
        time_label = "",
        value_label = i18n("FISHING_UNIT_PLOT_ACTIVITY_VALUE_LABEL"),
        group_label = i18n("FISHING_UNIT_PLOT_ACTIVITY_GROUP_LABEL"),
        stat = "sum",
        plot_type_default = 'bar_stack'
      )
      
      #Fleet engagement number
      fdishinyr::generic_chart_server(
        id = "boats",
        lang = appConfig$language,
        df = data_effort,
        col_date = "date",
        col_group = "fishing_unit_label",
        col_value = "fleet_engagement_number",
        time_label = "",
        value_label = i18n("FISHING_UNIT_PLOT_BOATS_VALUE_LABEL"),
        group_label = i18n("FISHING_UNIT_PLOT_BOATS_GROUP_LABEL"),
        stat = "sum",
        plot_type_default = 'bar_stack'
      )
      
      #Value
      fdishinyr::generic_chart_server(
        id = "value",
        lang = appConfig$language,
        df = data,
        col_date = "date",
        col_group = "fishing_unit_label",
        col_value = "trade_value",
        time_label = "",
        value_label = i18n("FISHING_UNIT_PLOT_VALUE_VALUE_LABEL"),
        group_label = i18n("FISHING_UNIT_PLOT_VALUE_GROUP_LABEL"),
        stat = "sum",
        plot_type_default = 'bar_stack'
      )
      
      
      #UI to render the generic charts
      output$results<-renderUI({
        
        tagList(
          fluidRow(
            column(4,fdishinyr::generic_chart_ui(ns("catch_sp_tot"),title=i18n("FISHING_UNIT_PLOT_CATCH_SP_TOT_TITLE"),sliderWidth =25)),
            column(4,fdishinyr::generic_chart_ui(ns("catch_fu_tot"),title=i18n("FISHING_UNIT_PLOT_CATCH_FU_TOT_TITLE"),sliderWidth =25)),
            column(4,fdishinyr::generic_chart_ui(ns("value_fu_tot"),title=i18n("FISHING_UNIT_PLOT_VALUE_FU_TOT_TITLE"),sliderWidth =25))
          ),
          fluidRow(fdishinyr::generic_chart_ui(ns("catch"),title=i18n("FISHING_UNIT_PLOT_CATCH_TITLE"),sliderWidth =25)),
          fluidRow(fdishinyr::generic_chart_ui(ns("cpue"),title=i18n("FISHING_UNIT_PLOT_CPUE_TITLE"),sliderWidth =25)),
          fluidRow(fdishinyr::generic_chart_ui(ns("effort"),title=i18n("FISHING_UNIT_PLOT_EFFORT_TITLE"),sliderWidth =25)),
          fluidRow(fdishinyr::generic_chart_ui(ns("activity"),title=i18n("FISHING_UNIT_PLOT_ACTIVITY_TITLE"),sliderWidth =25)),
          fluidRow(fdishinyr::generic_chart_ui(ns("boats"),title=i18n("FISHING_UNIT_PLOT_BOATS_TITLE"),sliderWidth =25)),
          fluidRow(fdishinyr::generic_chart_ui(ns("value"),title=i18n("FISHING_UNIT_PLOT_VALUE_TITLE"),sliderWidth =25))
        )
      })
    })
    
    #main UI
    output$main <- renderUI({
      tagList(
        fluidRow(
          div(
            width = 12, style = "margin:12px;",
            tags$h2(i18n("FISHING_UNIT_TITLE")),tags$h3(class = "text-muted", i18n("FISHING_UNIT_SUBTITLE"))
          )
        ),
        uiOutput(ns("no_release")),
        uiOutput(ns("filter_selectors")),
        uiOutput(ns("indicators")),
        uiOutput(ns("results"))
      )
    })
    
  })
  
}

#' @name artfish_shiny_fishing_unit_ui
#' @title ARTFISH Fishing unit - Shiny UI Module
#' @description User interface for the ARTFISH Fishing Unit Dashboard
#' @param id Character string. Module namespace identifier.
#' @export

artfish_shiny_fishing_unit_ui <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("main"))
  
}
