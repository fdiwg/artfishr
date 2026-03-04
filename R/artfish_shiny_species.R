#' @name artfish_shiny_species_server
#' @title ARTFISH Species - Shiny Server Module
#' @description
#' Server-side logic for the ARTFISH Species dashboard.
#'
#' The *"Catch and effort - Detailed by species"* module provides a species-oriented workspace for exploring key output metrics derived from \code{artfish_compute_report}, with a focus on individual species caught.
#'
#' The initial view displays a selector inviting the user to choose a species. 
#' The dropdown lists all species for which entries are available in the database. 
#' Users can then analyse the evolution of fishing activity over time, with filtering options for species, fishing units, and time range.
#'
#' The module is composed of:
#' \itemize{
#'   \item A selector panel to choose the species to display
#'   \item A selector panel to adjust the time range and fishing units to display
#'   \item Two boxed plots powered by \code{fdishinyr::generic_chart} allowing users to analyse, at species level:
#'   \itemize{
#'     \item (1) Fishing unit composition (by quantity)
#'     \item (2) Target species ranking (by quantity)
#'   }
#'   \item A set of KPIs presenting aggregated metrics for the selected period
#'   \item Five boxed plots powered by \code{fdishinyr::generic_chart} allowing users to analyse key Artfish metrics:
#'   \itemize{
#'     \item (1) Total catch estimation
#'     \item (2) Catch per unit effort (CPUE)
#'     \item (3) Total value estimation
#'     \item (4) Average price
#'     \item (5) Total nominal effort
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
#' @param estimate A data frame aggregating the output of \code{artfish_compute}, enriched with human-readable labels.
#'
#' @param effort_source Character string indicating the type of effort source.
#' Must be either \code{"fisher_interview"} or \code{"boat_counting"}.
#' Not activated
#'
#' @param minor_strata Character string targeting a column name considered as minor strata.
#' Not activated
#'
#' @export

artfish_shiny_species_server <- function(id, lang = NULL, estimate, effort_source = NULL, minor_strata = NULL){
  
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
    data_sp<-reactiveVal(NULL)
    data_sp_bg<-reactiveVal(NULL)
    
    # -------------------------------------------------------------------------
    # Case if no data to display
    # -------------------------------------------------------------------------
    #UI to indicate if there is no release
    output$no_release<-renderUI({
      div(
        if(nrow(estimate)>0){
          NULL
        }else{
          p(i18n("SPECIES_NO_RELEASE"))
        }
      )
    })
    
    req(nrow(estimate)>0)
    
    # -------------------------------------------------------------------------
    # UI Selectors
    # -------------------------------------------------------------------------
    
    #species selector UI
    output$species_selector <- renderUI({
      
      ref_sp <- estimate%>%select(species,species_label,species_scientific)%>%distinct()%>%rowwise()%>%mutate(label_name_scientific=sprintf("%s [%s]",species_label,species_scientific))%>%ungroup()
      choices <- setNames(ref_sp$species, ref_sp$label_name_scientific)
      choices <- choices[order(names(choices))]
      
      selectizeInput(ns("species"),paste0(i18n("SPECIES_SPECIES_SELECTOR_LABEL")," :"),choices = choices,multiple = F,selected = NULL,
                     options = list(
                       placeholder = i18n("SPECIES_SPECIES_SELECTOR_PLACEHOLDER"),
                       onInitialize = I('function() { this.setValue(""); }')
                     )
      )
    })
    
    #display main selectors UI (time, fishing_unit) on species reactive set
    observeEvent(data_sp(),{
      if(!is.null(data_sp())){ 
        
        #time selector (slider)
        output$time_selector <- renderUI({
          selection <- data_sp()
          sliderInput(
            ns("time"),
            label = i18n("SPECIES_TIME_SLIDER_LABEL"),
            min = min(selection$date, na.rm = TRUE),
            max = max(selection$date, na.rm = TRUE),
            value = c(
              min(selection$date, na.rm = TRUE),
              max(selection$date, na.rm = TRUE)
            ),
            timeFormat = "%b %Y"
          )
        })
        
        #fishing unit selector (multiple item selection)
        output$fishing_unit_selector <- renderUI({
          selection <- data_sp()
          
          ref_bg_sp <- selection%>%select(fishing_unit,fishing_unit_label)%>%distinct()
          choices <- setNames(ref_bg_sp$fishing_unit, ref_bg_sp$fishing_unit_label)
          
          shinyWidgets::pickerInput(
            inputId = ns("fishing_unit"),
            label   = i18n("SPECIES_FISHING_UNIT_SELECTOR_LABEL"),
            choices = choices,
            selected = ref_bg_sp$fishing_unit,
            multiple = TRUE,
            options = list(
              `actions-box` = TRUE,
              `select-all-text` = i18n("SPECIES_FISHING_UNIT_SELECTOR_SELECT_ALL"),
              `deselect-all-text` = i18n("SPECIES_FISHING_UNIT_SELECTOR_DESELECT_ALL"),
              `selected-text-format` = "count > 3",
              `count-selected-text` = paste0("{0} ",i18n("SPECIES_FISHING_UNIT_SELECTOR_SELECTED")),
              `none-selected-text` = i18n("SPECIES_FISHING_UNIT_SELECTOR_NO_SELECTION"),
              `live-search` = TRUE
            )
          )
        })
      }
    })
    
    # -------------------------------------------------------------------------
    # Data filtering
    # -------------------------------------------------------------------------
    
    #Subset data with species
    observeEvent(input$species,{
      if(input$species!=""){
        INFO("Select species '%s'", input$species)
        selection <- subset(estimate, species == input$species)
        data_sp(selection)
      }
    })
    
    #Process data and based on fishing_unit/time selection
    observeEvent(c(input$fishing_unit,input$time), {
      
      req(!is.null(input$fishing_unit))
      req(!is.null(data_sp()))
      selection<-data_sp()
      
      data<-selection%>%
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
      
      data_sp_bg(selection)
      
    })
    
    
    # -------------------------------------------------------------------------
    # UI Outputs and Rendering
    # -------------------------------------------------------------------------
    
    #Indicators and timeline
    observeEvent(data_sp_bg(),{
      
      req(!is.null(data_sp_bg()))
      data<-data_sp_bg()
      data_effort<-data%>%
        select(date,fishing_unit,fishing_unit_label,species,species_label,effort_nominal,catch_species_ratio) %>%
        distinct() %>%
        ungroup()
      
      total_effort<-data_effort%>%
        summarise(effort_nominal=sum(effort_nominal,na.rm=T),
                  catch_species_ratio=mean(catch_species_ratio,na.rm=T)
        )
      
      total_catch<-data%>%
        summarise(catch_nominal_landed=sum(catch_nominal_landed,na.rm=T),
                  trade_value=sum(trade_value,na.rm=T),
                  trade_price=mean(trade_price,na.rm=T)
        )
      
      # ===== UI Indicators (KPI) =====
      output$indicators <- renderUI({
        fluidRow(
          bs4InfoBox(
            title = i18n("SPECIES_INFOBOX_CATCH_TITLE"),
            value = sprintf("%s (%s)",formatC(total_catch$catch_nominal_landed, format = "f", digits = 0, big.mark = "\u202F"),i18n("SPECIES_INFOBOX_CATCH_UNIT")),
            icon = icon("fish"),
            color = "primary",
            width = 2
          ),
          bs4InfoBox(
            title = i18n("SPECIES_INFOBOX_VALUE_TITLE"),
            value = sprintf("%s (%s)",formatC(total_catch$trade_value, format = "f", digits = 0, big.mark = "\u202F"),i18n("SPECIES_INFOBOX_VALUE_UNIT")),
            icon = icon("dollar-sign"),
            color = "primary",
            width = 2
          ),
          bs4InfoBox(
            title = i18n("SPECIES_INFOBOX_PRICE_TITLE"),
            value = sprintf("%s (%s)",formatC(total_catch$trade_price, format = "f", digits = 2, big.mark = "\u202F"),i18n("SPECIES_INFOBOX_PRICE_UNIT")),
            icon = icon("dollar-sign"),
            color = "primary",
            width = 2
          ),
          bs4InfoBox(
            title = i18n("SPECIES_INFOBOX_EFFORT_TITLE"),
            value = sprintf("%s (%s)",formatC(total_effort$effort_nominal, format = "f", digits = 0, big.mark = "\u202F"),i18n("SPECIES_INFOBOX_EFFORT_UNIT")),
            icon = icon("ship"),
            color = "primary",
            width = 2
          ),
          bs4InfoBox(
            title = i18n("SPECIES_INFOBOX_SP_RATE_TITLE"),
            value = formatC(total_effort$catch_species_ratio, format = "f", digits = 2, big.mark = "\u202F"),
            icon = icon("fish"),
            color = "primary",
            width = 2
          )
        )
      })
      
      # ===== UI Chart box =====
      
      #Catches plot
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
        value_label = i18n("SPECIES_PLOT_CATCH_VALUE_LABEL"),
        group_label = i18n("SPECIES_PLOT_CATCH_GROUP_LABEL")
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
        value_label = i18n("SPECIES_PLOT_CPUE_VALUE_LABEL"),
        group_label = i18n("SPECIES_PLOT_CPUE_GROUP_LABEL"),
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
        value_label = i18n("SPECIES_PLOT_EFFORT_VALUE_LABEL"),
        group_label = i18n("SPECIES_PLOT_EFFORT_GROUP_LABEL"),
        stat = "sum",
        plot_type_default = 'bar_stack'
      )
      
      #Value plot
      fdishinyr::generic_chart_server(
        id = "value",
        lang = appConfig$language,
        df = data,
        col_date = "date",
        col_group = "fishing_unit_label",
        col_value = "trade_value",
        time_label = "",
        value_label = i18n("SPECIES_PLOT_VALUE_VALUE_LABEL"),
        group_label = i18n("SPECIES_PLOT_VALUE_GROUP_LABEL"),
        stat = "sum",
        plot_type_default = 'bar_stack'
      )
      
      #Prices plot
      fdishinyr::generic_chart_server(
        id = "price",
        lang = appConfig$language,
        df = data,
        col_date = "date",
        col_group = "fishing_unit_label",
        col_value = "trade_price",
        time_label = "",
        value_label = i18n("SPECIES_PLOT_PRICE_VALUE_LABEL"),
        group_label = i18n("SPECIES_PLOT_PRICE_GROUP_LABEL"),
        stat = "mean",
        time_choices = "month"
      )
      
      #UI for generic charts
      output$results<-renderUI({
        tagList(
          fluidRow(fdishinyr::generic_chart_ui(ns("catch"),title=i18n("SPECIES_PLOT_CATCH_TITLE"),sliderWidth =25)),
          fluidRow(fdishinyr::generic_chart_ui(ns("cpue"),title=i18n("SPECIES_PLOT_CPUE_TITLE"),sliderWidth =25)),
          fluidRow(fdishinyr::generic_chart_ui(ns("value"),title=i18n("SPECIES_PLOT_VALUE_TITLE"),sliderWidth =25)),
          fluidRow(fdishinyr::generic_chart_ui(ns("price"),title=i18n("SPECIES_PLOT_PRICE_TITLE"),sliderWidth =25)),
          fluidRow(fdishinyr::generic_chart_ui(ns("effort"),title=i18n("SPECIES_PLOT_EFFORT_TITLE"),sliderWidth =25))
        )
      })
      
    })
    
    #donuts and rank chart (no fishing unit selection) 
    observeEvent(data_sp(),{
      
      req(!is.null(data_sp()))
        
        selection<-data_sp()

        #donut chart with all fishing units
        fdishinyr::generic_chart_server(
          id = "donut",
          lang = appConfig$language,
          df = selection,
          col_date = "date",
          col_group = "fishing_unit_label",
          col_value = "catch_nominal_landed",
          stat = "sum",
          time_label = "",
          value_label = "",
          group_label = "",
          plot_types = c("donut","pie"),
          plot_type_default = "donut"
        )
        
        #ranked species chart with selected species highlighted
        fdishinyr::generic_chart_server(
          id = "rank",
          lang = appConfig$language,
          df = estimate,
          col_date = "date",
          col_group = "species_label",
          col_value = "catch_nominal_landed",
          stat = "sum",
          time_label = "",
          value_label = "",
          group_label = "",
          plot_types = c("rank_sum"),
          rank_target_id = unique(selection$species_label)
        )
      
    })
    
    #Causes the module to break when changing species
    # observeEvent(data_sp(), {
    #   req(data_sp())
    #   
    #   sel <- data_sp()
    #   
    #   if(is.null(input$fishing_unit) || as.integer(input$fishing_unit) == 0){
    #     data_sp_bg(sel)
    #   } else {
    #     data_sp_bg(subset(sel, fishing_unit == input$fishing_unit))
    #   }
    # })
    
    
    #donut chart wrapper
    output$donut_wrapper<-renderUI({
      if(!is.null(data_sp())){
        fluidRow(fdishinyr::generic_chart_ui(ns("donut"),title=i18n("SPECIES_PLOT_DONUT_TITLE"),sliderWidth =25))
      }else{
        NULL
      }
    })
    
    #ranked chart wrapper
    output$rank_wrapper<-renderUI({
      if(!is.null(data_sp())){
        fluidRow(fdishinyr::generic_chart_ui(ns("rank"),title=i18n("SPECIES_PLOT_RANK_TITLE"),sliderWidth =25))
      }else{
        NULL
      }
    })
    
    #main UI
    output$main <- renderUI({
      
      tagList(
        fluidRow(
          div(
            width = 12, style = "margin:12px;",
            tags$h2(i18n("SPECIES_TITLE")),tags$h3(class = "text-muted", i18n("SPECIES_SUBTITLE"))
          )
        ),            
        div(class="row",
            column(3,
                   uiOutput(ns("no_release")),
                   uiOutput(ns("species_selector")),
                   uiOutput(ns("time_selector")),
                   uiOutput(ns("fishing_unit_selector"))
            ),
            column(5,
                   uiOutput(ns("donut_wrapper"))
                   
            ),
            column(4,
                   uiOutput(ns("rank_wrapper"))        
                   
            )
            ,height=200),
        uiOutput(ns("indicators")),
        uiOutput(ns("results"))
      )
    })
    
  })
  
}

#' @name artfish_shiny_species_ui
#' @title ARTFISH Species - Shiny UI Module
#' @description User interface for the ARTFISH Species Dashboard
#' @param id Character string. Module namespace identifier.
#' @export

artfish_shiny_species_ui <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("main"))

  
}