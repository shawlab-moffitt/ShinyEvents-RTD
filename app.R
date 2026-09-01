version_id <- paste0("v1.0.20260825")

# lite swap able

# User File Input ---------------------------------------------------------

source("R/config.R")

# Text file
#Event_Param_File <- PARAM

# Text file or API URL
Event_Data_File <- API
#Event_Data_File <- "RTD_PHI_Data_20260901_102700.csv"

API_Auth <- API_AUTH_KEY



#####
# Highlight event not working
# Turned off hover choice
# timeline double loading something different at first
# x-axis breaks and unit changing
# add refresh button for API retrieval



# DO NOT EDIT BELOW THIS LINE... OR ELSE #######################################


fetch_csv_from_api <- function(url,auth) {
  resp <- POST(url,add_headers(Authorization=auth))
  stop_for_status(resp)
  txt <- content(resp, "text", encoding = "UTF-8")
  t = fread(text = txt, na.strings = c("","NA","N/A","n/a"), data.table = F)
}


GlobalAppTimeUnit <- "Months" # NOT WORKING
Event_Cluster_Window <- 1
app_lite <- TRUE
set.seed(42)
heat_hover_avail <- FALSE
Event_Cluster_Window <- 1



if (file.exists(Event_Data_File)) {
  if (tolower(tools::file_ext(Event_Data_File)) == "csv") {
    event_data_raw_in <- fread(Event_Data_File, fill = T, sep = ',', header = T, na.strings = c("","NA","N/A","n/a"), data.table = F)
  } else {
    event_data_raw_in <- fread(Event_Data_File, fill = T, sep = '\t', header = T, na.strings = c("","NA","N/A","n/a"), data.table = F)
  }
  colnames(event_data_raw_in)[1] <- "Name"
} else {
  API_URL <- Event_Data_File
  api_url <- Sys.getenv("API_URL", API_URL)
  event_data_raw_in <- fetch_csv_from_api(api_url,API_Auth)
  colnames(event_data_raw_in)[1] <- "Name"
}

#if (file.exists(Event_Param_File)) {
#  param_data_in <- fread(Event_Param_File,na.strings = c("","NA"), data.table = F)
#}

# Process event data ------------
if ("RTD Case Number" %in% colnames(event_data_raw_in)) {
  event_data_raw_in <- event_data_raw_in %>%
    arrange(`RTD Case Number`,`Event Start`,`Event End`) %>%
    as.data.frame()
}
## Remove space from column names
colnames(event_data_raw_in) <- gsub(" ","",colnames(event_data_raw_in))

## ensure correct order
event_data_raw_in <- event_data_raw_in %>%
  relocate(Name, EventName, EventType, EventStart, EventEnd) %>%
  as.data.frame()

## Add event ID
event_data_raw_in$EventID <- paste0(gsub(" ","",(event_data_raw_in$Name)),"_",rownames(event_data_raw_in))
## Adjust NA event times
event_data_raw_in[,"EventEnd"] <- ifelse(is.na(event_data_raw_in[,"EventEnd"]),
                                          event_data_raw_in[,"EventStart"],
                                          event_data_raw_in[,"EventEnd"])
event_data_raw_in[,"EventStart"] <- ifelse(is.na(event_data_raw_in[,"EventStart"]),
                                            event_data_raw_in[,"EventEnd"],
                                            event_data_raw_in[,"EventStart"])
event_data_raw_in <- event_data_raw_in[which(!is.na(event_data_raw_in$EventName)),]
## Generate patients selection table
### Find columns where every group has exactly 1 unique value
valid_cols <- sapply(event_data_raw_in, function(col) {
  all(tapply(col, event_data_raw_in$Name, function(x) length(unique(x))) == 1)
})
### Keep only the valid columns (and make sure to keep your grouping column)
pat_anno <- unique(event_data_raw_in[, valid_cols])
pat_events_avail <- event_data_raw_in %>%
  select(Name,EventName,EventType) %>%
  mutate(EventAvail = paste0(EventName[EventType == "Diagnosis"], collapse = " // "), .by = Name) %>%
  mutate(EventAvail = ifelse(EventType != "Diagnosis","Available",EventAvail)) %>%
  select(-EventName) %>%
  distinct() %>%
  pivot_wider(id_cols = Name,
              names_from = EventType,
              values_from = EventAvail) %>%
  relocate(any_of(c("Name","Diagnosis",grep("lesion|genom|autopsy",colnames(.),ignore.case = T,value = T))))

pat_anno <- merge(pat_anno,pat_events_avail,all.X = T, sort = F)
pat_anno <- pat_anno %>% relocate(any_of(c("Name","Diagnosis"))) %>% as.data.frame()
#pat_anno <- event_count_df(event_data_raw_in)
## Format event name label
event_data_raw_in$EventNameLabel <- ifelse(event_data_raw_in$EventName != event_data_raw_in$EventType,
                                           paste0(event_data_raw_in$EventType,": ",event_data_raw_in$EventName),
                                           event_data_raw_in$EventName)

eventTypeOptions <- unique(event_data_raw_in$EventType)
eventTypeOptions_selected <- grep("diagnosis|death|autopsy|medication|drug|trial|surgery|genom|lesion|radio",
                                  eventTypeOptions,value = T,ignore.case = T)

# Create event data index ----------------------
event_data_index_in <- sapply(pat_anno[,1],function(patient_id) {
  pat_event_data <- event_data_raw_in[which(event_data_raw_in[,1] == patient_id),]
  pat_lesion_data <- pat_event_data[which(pat_event_data$EventType == "Lesion Scan"),]
  pat_event_data <- pat_event_data[which(pat_event_data$EventType != "Lesion Scan"),]
  pat_event_data_uniq <- unique(pat_event_data[,c(2,3)])
  pat_event_opts <- split(pat_event_data_uniq[,1], pat_event_data_uniq[,2])
  return(list(patient = patient_id,
              pat_event_data = pat_event_data,
              pat_lesion_data = pat_lesion_data,
              pat_event_opts = pat_event_opts))
}, USE.NAMES = T, simplify = F)


#increase file upload size
options(shiny.maxRequestSize=5000*1024^2)


# Patient Tab ------------------------------------------------------------------

PatientLevel_tab_contents <- sidebarLayout(
  sidebarPanel(
    width = 3,
    tabsetPanel(
      id = "PatientTimeline",
      tabPanel("Data Input",
               p(),
               virtualSelectInput(inputId = "EventTypesToShow",label = "Cohort Event Types:",
                                  choices = eventTypeOptions,selected = eventTypeOptions_selected,
                                  showValueAsTags = TRUE,multiple = TRUE),
               virtualSelectInput(inputId = "SwimmerYlinesSelect",label = "Patient Event Filter:",
                                  choices = NULL,showValueAsTags = TRUE,search = TRUE,multiple = TRUE),
               #div(virtualSelectInput(inputId = "HighlightEventSelect",label = "Highlight Event:",
               #                       choices = NULL,showValueAsTags = TRUE,search = TRUE,multiple = TRUE), style = margin_adjust(-15,NA,app_lite)),
               h4("Patient Selection"),
               div(DT::dataTableOutput("PatientSelectionTab"), style = "font-size:10px"),
      ),
      tabPanel("Figure Settings",
               p(),
                                #uiOutput("rendTimeLineTitle"),
                                ColorPalSelect_UI("PatTimelineColorPal"),
                                selectInput("PatTimelineXunit","X-Axis Unit:", choices = c("Years","Months","Days"), selected = "Months"),
                                h4("Font Sizes"),
                                fluidRow(
                                  column(4,
                                         numericInput("TimeLineTitleSize","Title:",
                                                      value = 18, step = 1)
                                  ),
                                  column(4,
                                         numericInput("TimeLineXAxisSize","X-Axis:",
                                                      value = 14, step = 1)
                                  ),
                                  column(4,
                                         numericInput("TimeLineYAxisSize","Y-Axis:",
                                                      value = 12, step = 1)
                                  )
                                ),
                                h4("Figure Download Parameters"),
                                fluidRow(
                                  column(6,
                                         numericInput("TimeLineHeight","Height (px)",value = 800)
                                  ),
                                  column(6,
                                         numericInput("TimeLineWidth","Width (px)",value = 1000)
                                  )
                                )
      )
    )
  ),
  mainPanel(
    p(),
    shinycssloaders::withSpinner(
      shinyjqui::jqui_resizable(
        plotlyOutput("PatientTimelineLineSummPlot_full",height = "750px", width = "100%")), type = 6),
    fluidRow(
      column(3,
             downloadButton("dnldPatientEventTab_full","Patient Event Table")
      )
    ),
    p(),
    tabsetPanel(
      tabPanel("Patient Data",
               p(),
               div(DT::dataTableOutput("patient_event_table"), style = "font-size:14px"),
               downloadButton("patient_event_table_Dnld", "Download Table")),
      tabPanel("Lesion Data",
               p(),
               div(DT::dataTableOutput("patient_lesion_table"), style = "font-size:14px"),
               downloadButton("patient_lesion_table_Dnld"), "Download Table"))
    )
)
PatientLevel_tab <- tabPanel("Patient Visual Analytics",
                             value = "patient_visual_analytics",
                             fluidPage(
                               tags$head(
                                 tags$style(HTML("
                                                        .nav-tabs {
                                                        overflow-x: auto;
                                                        overflow-y: hidden;
                                                        white-space: nowrap;
                                                        flex-wrap: nowrap !important;
                                                        display: flex;
                                                        }
                                                        "))
                               ),
                               tags$head(
                                 tags$style(HTML("
                                                        .selectize-input {
                                                        max-height: 82px;
                                                        overflow-y: auto;
                                                        }
                                                        #SwimmerYlinesSelect .vscomp-value {
                                                        max-height: 82px;
                                                        overflow-y: auto;
                                                        }
                                                        .vscomp-options-container {
                                                        max-height: 200px !important;
                                                        overflow-y: auto !important;
                                                        }
                                                        .selectize-dropdown {
                                                        width: 500px !important;
                                                        }
                                                        .html-embed img {
                                                        max-width: 100%;
                                                        height: auto;
                                                        display: block;
                                                        margin: 0 auto;
                                                        }
                                                        "))
                               ),
                               PatientLevel_tab_contents,
                               tagList(
                                 tags$head(
                                   tags$style(
                                     HTML("
                                     .info_box {
                                     width: auto;
                                     height: auto;
                                     color: #000000;
                                     background-color: #f5f5f5;
                                     padding: 3px 8px;
                                     font-size: 12px;
                                     z-index : 9999;
                                     }",
                                          glue::glue("#{'AppVersion'} {{
                                                position: {'fixed'};
                                                top: 0;
                                                right: 0;
                                                }}")
                                     )
                                   )
                                 ),
                                 div(id = "AppVersion", class = "info_box", version_id)
                               )
                             )
)

# Patient Data Tab --------------------
PatientData_tab_contents <- sidebarLayout(
  sidebarPanel(
    width = 3,
    p(),
    p("1. Filter table to identify patients of interest."),
    p("2. Select row or type in patient ID from first column then press button to open and view selected patients timeline."),
    fluidRow(
      column(5,
             textInput("PatientIDinput","Patient ID","")
             ),
      column(7, style = "margin-top:20px",
             actionButton("PatientTransfer", HTML(paste0(c("Press button to open","patient timeline"), collapse="</br>")),
                          width = "100%")
             )
    )
  ),
  mainPanel(
    p(),
    div(DT::dataTableOutput("RTD_patientTable"), style = "font-size:14px")
  )
)
PatientData_tab <- tabPanel("RTD Patient Data",
                             value = "PatientData_tab",
                             fluidPage(
                               PatientData_tab_contents,
                               tagList(
                                 div(id = "AppVersion", class = "info_box", version_id)
                               )
                             )
)



ui <- navbarPage(
  title = paste("{ ShinyEvents - Rapid Tissue Dontation }",sep = ""),
  id = "shinyevents_tabs",
  theme = shinytheme("flatly"),
  PatientLevel_tab,
  PatientData_tab,
  selected = "patient_visual_analytics")



# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  if(!interactive()) pdf(NULL)
  
  # Data Input -------------------------------------------------------------
  
  event_data_raw <- reactiveVal(event_data_raw_in)
  pat_anno_raw <- reactiveVal(pat_anno)
  event_data_index <- reactiveVal(event_data_index_in)
  
      GlobalAppTimeUnit_react <- reactiveVal(GlobalAppTimeUnit)
      
      # Color Pal Reacts
      colPalSelected <- ColorPalSelect_server("PatTimelineColorPal")
      PatTimelineSummColorPal_sel <- ColorPalSelect_server("PatTimelineColorPal")

      observeEvent(input$PatTimelineXunit, {
        GlobalAppTimeUnit_react(input$PatTimelineXunit)
      })
      
      # Patient Data -----------------------------------------------------------
      
      output$RTD_patientTable <- DT::renderDataTable({
        event_data <- event_data_raw()
        DT::datatable(event_data,
                      escape = F,
                      class = "display nowrap",
                      extensions = 'ColReorder',
                      filter = list(position = 'top', clear = FALSE),
                      options = list(lengthMenu = c(5, 10, 20, 100, 1000),
                                     pageLength = 20,
                                     scrollX = T,
                                     target = "cell",
                                     colReorder = TRUE),
                      selection = 'single',
                      rownames = F
        )
      })
      PatientSelectionTab_proxy <- dataTableProxy("PatientSelectionTab")
      
      observeEvent(input$RTD_patientTable_rows_selected, {
        event_data <- event_data_raw()
        Patient_Row_Selec <- input$RTD_patientTable_rows_selected
        Patient <- event_data[Patient_Row_Selec,1]
        updateTextInput(session,"PatientIDinput",value = Patient)
      })
      
      observeEvent(input$PatientTransfer, {
        req(input$PatientIDinput)
        pat_anno_raw <- pat_anno_raw()
        patient <- input$PatientIDinput
        if (!patient %in% pat_anno_raw[,1]) {
          
        } else {
          patient_row <- which(pat_anno_raw[,1] == patient)
          PatientSelectionTab_proxy %>% selectRows(as.numeric(patient_row))
          updateNavbarPage(session,"shinyevents_tabs", selected = "patient_visual_analytics")
        }
      })
      
      row_index <- reactiveValues(index = 1)
      observe({
        row_index$index <- which(input$PatientSelectionTab_rows_selected == input$PatientSelectionTab_rows_all)
      })
      
      # Patient Analysis -------------------------------------------------------
      patient_id_react <- reactive({
        req(input$PatientSelectionTab_rows_selected)
        req(pat_anno_raw())
        Patient_Row_Selec <- input$PatientSelectionTab_rows_selected
        Patient_Table_df <- pat_anno_raw()
        Patient <- as.character(Patient_Table_df[Patient_Row_Selec, 1, drop = TRUE])
        Patient
      })
      
      patient_edi <- reactive({
        req(patient_id_react())
        req(event_data_index())
        edi <- event_data_index()
        Patient <- patient_id_react()
        pat_event_obj <- edi[[Patient]]
        req(pat_event_obj)
        pat_event_obj
      })
      
      #highlight_opts_index <- reactive({
      #  req(patient_event_data_single())
      #  pat_event_data <- patient_event_data_single()
      #  pat_event_data_uniq <- unique(pat_event_data[,c("EventStart","EventType")])
      #  highlight_opts <- split(pat_event_data_uniq[,"EventStart"], pat_event_data_uniq[,"EventType"])
      #  highlight_opts
      #})
      
      #output$rendTimeLineTitle <- renderUI({
      #  Patient_Row_Selec <- input$PatientSelectionTab_rows_selected
      #  Patient_Table_df <- pat_anno_raw()
      #  Patient <- Patient_Table_df[Patient_Row_Selec,1]
      #  SwimmerTitle <- paste0("Clinical Course of Patient: ", as.character(Patient))
      #  textInput("TimeLineTitle","Title:",value = SwimmerTitle)
      #})
      
      output$PatientSelectionTab <- DT::renderDataTable({
        Patient_Table_df <- pat_anno_raw()
          DT::datatable(Patient_Table_df,
                        extensions = 'Scroller',
                        options = list(lengthMenu = c(5,10, 20, 100, 1000),
                                       pageLength = 10,
                                       scrollX = T
                        ),
                        selection = list(mode = 'single', selected = 1),
                        rownames = F
          )
      })
      
      #observe({
      #  Patient <- patient_id_react()
      #  edi <- event_data_index()
      #  Patient_Table_df <- pat_anno_raw()
      #  Patient_Row_Selec <- input$PatientSelectionTab_rows_selected
      #  colorPal <- PatTimelineSummColorPal_sel()
      #  TitleFont <- input$TimeLineTitleSize
      #  xFont <- input$TimeLineXAxisSize
      #  yFont <- input$TimeLineYAxisSize
      #  eventsY <- input$SwimmerYlinesSelect
      #  TimeLineHeight <- input$TimeLineHeight
      #  TimeLineWidth <- input$TimeLineWidth
      #  AppTimeUnit <- input$PatTimelineXunit
      #  RemoveUnkNA_opt <- TRUE
      #  TimeLineTitle <- input$TimeLineTitle
      #  SwimmerTheme <- input$TimeLineTheme
      #  AppTimeUnits <- tolower(GlobalAppTimeUnit_react())
      #  
      #  yAxis_lp <- "LesionSize"
      #  yaxlab_lp <- "Lesion Size (cm)"
      #  xAxis_lp <- "EventStart"
      #  xAxisUnits_lp <- AppTimeUnits
      #  userCutP_lp <- input$linePlotCutP
      #  Xaxis_font_lp <- input$LinePlotXAxisSize
      #  Yaxis_font_lp <- input$LinePlotYAxisSize
      #  title_font_lp <- input$LinePlotTitleSize
      #  unitCol_lp <- input$LinePunitCol
      #  unitSel_lp <- input$LinePunitSelect
      #  LinePlotTheme_lp <- input$LinePlotTheme
      #  save(list = ls(), file = "RTD_user_inputs.RData", envir = environment())
      #})
      
      # TRUE when the next change to EventTypesToShow was made by the server
      suppress_event_type_observer <- reactiveVal(FALSE)
      
      # TRUE when the next change to SwimmerYlinesSelect was made by the server
      suppress_swimmer_observer <- reactiveVal(FALSE)
      
      observeEvent(patient_edi(), {
        pat_event_obj <- patient_edi()
        pat_event_opts <- pat_event_obj$pat_event_opts
        EventTypesToShow <- isolate(input$EventTypesToShow)
        if (is.null(EventTypesToShow)) {
          EventTypesToShow <- character(0)
        }
        pat_event_opts_select <- unname(unlist(pat_event_opts[EventTypesToShow]))
        suppress_swimmer_observer(TRUE)
        updateVirtualSelect(
          session = session,
          inputId = "SwimmerYlinesSelect",
          choices = pat_event_opts,
          selected = pat_event_opts_select
        )
      }, ignoreInit = FALSE)
      
      observeEvent(input$EventTypesToShow, {
        if (isTRUE(suppress_event_type_observer())) {
          suppress_event_type_observer(FALSE)
          return()
        }
        req(patient_edi())
        pat_event_obj <- patient_edi()
        pat_event_opts <- pat_event_obj$pat_event_opts
        EventTypesToShow <- input$EventTypesToShow
        SwimmerYlinesSelect <- isolate(input$SwimmerYlinesSelect)
        if (is.null(SwimmerYlinesSelect)) {
          SwimmerYlinesSelect <- character(0)
        }
        pat_event_opts_select <- unname(unlist(pat_event_opts[EventTypesToShow]))
        if (setequal(pat_event_opts_select,SwimmerYlinesSelect)) {
          return()
        }
        suppress_swimmer_observer(TRUE)
        updateVirtualSelect(
          session = session,
          inputId = "SwimmerYlinesSelect",
          selected = pat_event_opts_select
        )
      }, ignoreInit = TRUE)
      
      observeEvent(input$SwimmerYlinesSelect, {
        if (isTRUE(suppress_swimmer_observer())) {
          suppress_swimmer_observer(FALSE)
          return()
        }
        pat_event_obj <- patient_edi()
        pat_event_opts <- pat_event_obj$pat_event_opts
        pat_event_data <- pat_event_obj$pat_event_data
        SwimmerYlinesSelect <- input$SwimmerYlinesSelect
        if (is.null(SwimmerYlinesSelect)) {
          SwimmerYlinesSelect <- character(0)
        }
        EventTypesToShow <- isolate(input$EventTypesToShow)
        if (is.null(EventTypesToShow)) {
          EventTypesToShow <- character(0)
        }
        if (length(SwimmerYlinesSelect) == 0) {
          return()
        }
        selected_event_types <- unique(pat_event_data$EventType[pat_event_data$EventName %in% SwimmerYlinesSelect])
        updated_event_types <- unique(c(EventTypesToShow,selected_event_types))
        if (setequal(updated_event_types,selected_event_types)) {
          return()
        }
        suppress_event_type_observer(TRUE)
        updateVirtualSelect(
          session = session,
          inputId = "EventTypesToShow",
          selected = updated_event_types
        )
      }, ignoreInit = TRUE)
      
      
      
      
      PatientTimelineSummPlotDF_react_full <- reactive({
        req(patient_id_react())
        req(input$SwimmerYlinesSelect)
        colorPal <- PatTimelineSummColorPal_sel()
        if (colorPal == "Standard colors") {
          colorPal <- NULL 
        }
        Patient <- patient_id_react()
        edi <- event_data_index()
        Patient_Event_Data_sub <- edi[[Patient]]$pat_event_data
          RemoveUnkNA_opt <- TRUE
          #SwimmerTitle_in <- paste0("Clinical Course: ", as.character(Patient))
          SwimmerTitle_in <- as.character(Patient)
          #SwimmerTitle_in <- ifelse(!isTruthy(input$TimeLineTitle),paste0("Clinical Course of Patient: ", as.character(Patient)),input$TimeLineTitle)
          #SwimmerTheme <- input$TimeLineTheme
          TitleFont <- input$TimeLineTitleSize
          xFont <- input$TimeLineXAxisSize
          yFont <- input$TimeLineYAxisSize
          eventsY <- input$SwimmerYlinesSelect
          TimeLineHeight <- input$TimeLineHeight
          TimeLineWidth <- input$TimeLineWidth
          AppTimeUnit <- input$PatTimelineXunit
          Patient_Event_Data_sub <- Patient_Event_Data_sub[which(Patient_Event_Data_sub[,2] %in% eventsY),]
          Patient_Event_Data_sub <- Patient_Event_Data_sub[order(match(Patient_Event_Data_sub[,2], eventsY)),]
          Patient_Event_Data_sub$EventStart <- round(Patient_Event_Data_sub$EventStart,2)
          Patient_Event_Data_sub$EventEnd <- round(Patient_Event_Data_sub$EventEnd,2)
          Patient_Event_Data_sub
        #}
        
      })
      
      PatientTimelineSummPlot_react_full <- reactive({
        req(PatientTimelineSummPlotDF_react_full())
        req(patient_id_react())
        Patient <- patient_id_react()
        Patient_Event_Data_sub <- PatientTimelineSummPlotDF_react_full()
          colorPal <- PatTimelineSummColorPal_sel()
          if (colorPal == "Standard colors") {
            colorPal <- NULL 
          }
            RemoveUnkNA_opt <- TRUE
            #SwimmerTitle_in <- paste0("Clinical Course: ", as.character(Patient))
            SwimmerTitle_in <- as.character(Patient)
            #SwimmerTitle_in <- ifelse(!isTruthy(input$TimeLineTitle),paste0("Clinical Course of Patient: ", as.character(Patient)),input$TimeLineTitle)
            SwimmerTheme <- input$TimeLineTheme
            TitleFont <- input$TimeLineTitleSize
            xFont <- input$TimeLineXAxisSize
            yFont <- input$TimeLineYAxisSize
            eventsY <- input$SwimmerYlinesSelect
            TimeLineHeight <- input$TimeLineHeight
            TimeLineWidth <- input$TimeLineWidth
            AppTimeUnit <- input$PatTimelineXunit
            #plot2 <- timelinePlot(data = Patient_Event_Data_sub[,-1],event_col = "Event", event_type_col = "EventType",
            #                      start_col = "EventStart", stop_col = "EventEnd", unit = AppTimeUnit, plotly = TRUE,
            #                      title_font = TitleFont, x_font = xFont, y_font = yFont,na.rm = RemoveUnkNA_opt,
            #                      title = SwimmerTitle_in,svg_name = paste0("Moffitt_RTD_Patient_",Patient,"_Timeline"),
            #                      svg_height = TimeLineHeight, svg_width = TimeLineWidth, highlight_col = "highlight", col_pal = colorPal)
            plot2 <- timelinePlot(data = Patient_Event_Data_sub[,-1],event_col = "EventNameLabel", event_type_col = "EventType",
                                  start_col = "EventStart", stop_col = "EventEnd", unit = AppTimeUnit, plotly = TRUE,
                                  title_font = TitleFont, x_font = xFont, y_font = yFont,na.rm = RemoveUnkNA_opt,
                                  title = SwimmerTitle_in,svg_name = paste0("Moffitt_RTD_Patient_",Patient,"_Timeline"),
                                  svg_height = TimeLineHeight, svg_width = TimeLineWidth, col_pal = colorPal)
            plot2
        #  }
        #}
      })
      
      
      
      #output$PatientTimelineLineSummPlot_full <- renderPlotly({
      #  req(patient_id_react())
      #  Patient <- patient_id_react()
      #  plot_title <- paste0("Clinical Course of Patient: ", as.character(Patient))
      #  p1 <- PatientTimelineSummPlot_react_full()
      #  p_out <- p1
      #  line_p <- PatientLinePlot_react_full()
      #  AppTimeUnits <- tolower(GlobalAppTimeUnit_react())
      #  xAxis <- "EventStart"
      #  xAxisUnits <- AppTimeUnits
      #  req(p1)
      #  if (isTruthy(line_p)) {
      #    p1_xlim <- p1$x$layout$xaxis$range
      #    if (nrow(line_p$data[xAxis]) > 0) {
      #      p2_xlim <- range(line_p$data[xAxis], na.rm = T)
      #      line_p <- line_p +
      #        ggplot2::scale_x_continuous(limits=c(min(c(p1_xlim,p2_xlim), na.rm = T), max(c(p1_xlim,p2_xlim), na.rm = T)))
      #      p2 <- plotly::ggplotly(line_p)
      #      p_out <- subplot(
      #        p1, p2,
      #        nrows = 2,
      #        shareY = FALSE,
      #        shareX = TRUE,
      #        titleY = TRUE,
      #        titleX = TRUE,
      #        which_layout = 1
      #      ) %>%
      #        layout(
      #          showlegend = TRUE,
      #          margin = list(t = 50, r = 140)
      #        )
      #      #p_out <- subplot(p1, p2, nrows = 2, shareY = TRUE, shareX = TRUE,
      #      #                 titleY = TRUE, titleX = TRUE, which_layout = 1)
      #      #p_out <- p_out %>%
      #      #  layout(margin = list(t = 50))
      #    }
      #    p_out
      #  } else {
      #    p_out
      #  }
      #  
      #})
      
      output$PatientTimelineLineSummPlot_full <- renderPlotly({
        req(patient_id_react())
        
        p1 <- PatientTimelineSummPlot_react_full()
        line_p <- PatientLinePlot_react_full()
        
        req(p1)
        
        # Return only the swimmer plot when there is no lesion plot.
        if (!isTruthy(line_p) || nrow(line_p$data) == 0) {
          return(p1)
        }
        
        # Find a common x-axis range.
        p1_xlim <- p1$x$layout$xaxis$range
        p2_xlim <- range(line_p$data$EventStart, na.rm = TRUE)
        
        combined_xlim <- range(
          c(p1_xlim, p2_xlim),
          na.rm = TRUE
        )
        
        line_p <- line_p +
          ggplot2::scale_x_continuous(limits = combined_xlim)
        
        p2 <- plotly::ggplotly(line_p)
        
        # Hide legend entries belonging to the swimmer plot at the trace level.
        p1 <- plotly::style(p1, showlegend = FALSE)
        
        p_out <- plotly::subplot(
          p1,
          p2,
          nrows = 2,
          heights = c(0.68, 0.32),
          shareX = TRUE,
          shareY = FALSE,
          titleX = TRUE,
          titleY = TRUE,
          which_layout = 1,
          margin = 0.04
        ) %>%
          plotly::layout(
            showlegend = TRUE,
            legend = list(
              title = list(
                text = "Lesion Site",
                side = "top left"
              ),
              orientation = "h",
              x = 0,
              xanchor = "left",
              y = -0.14,
              yanchor = "top",
              entrywidth = 0.30,
              entrywidthmode = "fraction",
              font = list(size = 12)
            ),
            margin = list(
              t = 50,
              r = 30,
              b = 180
            )
          )
        
        p_out
      })
      
      
      PatientLinePlot_df_full <- reactive({
        req(patient_id_react())
        Patient <- patient_id_react()
        edi <- event_data_index()
        lesion_data <- edi[[Patient]]$pat_lesion_data
        #lineP_df <- lesion_data[which(lesion_data[,1] == Patient),]
        plot_df_2 <- lesion_data
        plot_df_2[,"Lesion Site"] <- gsub("^Lesion Scan: |^LesionScan: ","",plot_df_2$EventName)
        plot_df_2
      })
      
      PatientLinePlot_react_full <- reactive({
        req(PatientLinePlot_df_full())
        plot_df <- PatientLinePlot_df_full()
        AppTimeUnits <- tolower(GlobalAppTimeUnit_react())
        yAxis <- "LesionSize"
        yaxlab <- "Lesion Size (cm)"
        xAxis <- "EventStart"
        xAxisUnits <- AppTimeUnits
        userCutP <- input$linePlotCutP
        Xaxis_font <- input$TimeLineXAxisSize
        Yaxis_font <- input$TimeLineYAxisSize
        title_font <- input$TimeLineTitleSize
        unitCol <- input$LinePunitCol
        unitSel <- input$LinePunitSelect
        LinePlotTheme <- input$LinePlotTheme
        plotTitle <- paste0("Lesion History of patient ",unique(plot_df[,1])," (cm)")
        plot_df[,yAxis] <- as.numeric(plot_df[,yAxis])
        plot_df[,xAxis] <- as.numeric(plot_df[,xAxis])
        
        plot_df <- plot_df[which(!is.na(plot_df[,xAxis]) & !is.na(plot_df[,yAxis])),]
        if (isTruthy(plot_df) & nrow(plot_df) > 0) {
          p2 <- ggplot(data=plot_df, aes(x=EventStart, y=`LesionSize`, colour=`Lesion Site`)) +
            geom_line() +
            geom_point(size = 2) +
            theme_minimal() +
            theme(axis.title.x = element_text(size = Xaxis_font, margin = margin(30,0,0,0)),
                  axis.text.x = element_text(size = Xaxis_font),
                  axis.text.y = element_text(size = Yaxis_font),
                  axis.title.y = element_text(size = Yaxis_font),
                  plot.title = element_text(size = title_font, margin = margin(-10,0,30,0))) +
            ylab(paste0("Lesion Size (cm)"))
          p2
        }
        
        
        
      })
      
      output$patient_event_table <- DT::renderDataTable({
        req(patient_id_react())
        Patient <- patient_id_react()
        edi <- event_data_index()
        df <- edi[[Patient]]$pat_event_data
        #df <- patient_event_data_single()
        DT::datatable(df,
                      escape = F,
                      class = "display nowrap",
                      extensions = 'ColReorder',
                      options = list(lengthMenu = c(5, 10, 20, 100, 1000),
                                     pageLength = 100,
                                     scrollX = T,
                                     target = "cell",
                                     colReorder = TRUE),
                      rownames = F
        )
      })
      
      output$patient_lesion_table <- DT::renderDataTable({
        req(patient_id_react())
        Patient <- patient_id_react()
        edi <- event_data_index()
        df <- edi[[Patient]]$pat_lesion_data
        df_sub <- df %>%
          select(any_of(c("Name","EventName","EventStart","LesionSite","Lesion Site","LesionSize","LesionTarget/Non-Target"))) %>%
          as.data.frame()
        DT::datatable(df_sub,
                      escape = F,
                      class = "display nowrap",
                      extensions = 'ColReorder',
                      options = list(lengthMenu = c(5, 10, 20, 100, 1000),
                                     pageLength = 100,
                                     scrollX = T,
                                     target = "cell",
                                     colReorder = TRUE),
                      rownames = F
        )
      })
      
      
      
      ## Downloads
      output$dnldPatientTimelinePlot <- downloadHandler(
        filename = function() {
          Patient_Row_Selec <- input$PatientSelectionTab_rows_selected
          Patient_Table_df <- pat_anno_raw()
          Patient <- Patient_Table_df[Patient_Row_Selec,1]
          paste0("Moffitt_RTD_Patient_",Patient,"_Timeline.svg")
        },
        content = function(file) {
          p <- PatientTimelinePlot_react()
          ggsave(file,p,width = input$TimeLineWidth, height = input$TimeLineHeight, units = input$TimeLineUnits)
        }
      )
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
}

# Run the application
shinyApp(ui = ui, server = server)



