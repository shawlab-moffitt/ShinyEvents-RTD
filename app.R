version_id <- paste0("v1.0.20260618")

# lite swap able

# User File Input ---------------------------------------------------------

source("R/config.R")

# Text file
Event_Param_File <- PARAM

# Text file or API URL
Event_Data_File <- API

API_Auth <- API_AUTH_KEY


#####
# Highlight event not working
# Turned off hover choice
# timeline double loading something different at first




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
#wkbk_pre <- NULL
#AllFilesReady <- TRUE
#start_trigger <- 1
heat_hover_avail <- FALSE
#Workbook_file <- ""
Event_Cluster_Window <- 1
#Patient_Annotation_File <- ""



if (file.exists(Event_Data_File)) {
  if (tolower(tools::file_ext(Event_Data_File)) == "csv") {
    event_data_raw <- fread(Event_Data_File, fill = T, sep = ',', header = T, na.strings = c("","NA","N/A","n/a"), data.table = F)
  } else {
    event_data_raw <- fread(Event_Data_File, fill = T, sep = '\t', header = T, na.strings = c("","NA","N/A","n/a"), data.table = F)
  }
  colnames(event_data_raw)[1] <- "Name"
} else {
  API_URL <- Event_Data_File
  api_url <- Sys.getenv("API_URL", API_URL)
  event_data_raw <- fetch_csv_from_api(api_url,API_Auth)
  colnames(event_data_raw)[1] <- "Name"
}

if (file.exists(Event_Param_File)) {
  param_data <- fread(Event_Param_File,na.strings = c("","NA"), data.table = F)
  paramEvent_data <- param_data[which(!is.na(param_data[,3])),]
  rownames(paramEvent_data) <- NULL
}


























#increase file upload size
options(shiny.maxRequestSize=5000*1024^2)

# Password Table
# user database for logins
#if (Password_Protected) {
#  user_base <- tibble::tibble(
#    user = "user",
#    password = PasswordSet,
#    permissions = "admin",
#    name = "User"
#  )
#}




# Patient Tab ------------------------------------------------------------------

PatientLevel_tab_contents <- sidebarLayout(
  sidebarPanel(
    width = 3,
    tabsetPanel(
      id = "PatientTimeline",
      tabPanel("Data Input",
               #conditionalPanel(condition = "input.PatientMainPanel == '1'",
                                p(),
                                #fluidRow(
                                #  column(9,
                                         virtualSelectInput(inputId = "SwimmerYlinesSelect",label = "Timeline Row Filter:",
                                                            choices = NULL,showValueAsTags = TRUE,search = TRUE,multiple = TRUE),
                                #  ),
                                #  column(3, style = "margin-top:25px",
                                         checkboxInput("displaySummaryRows","Display Summary Rows", value = FALSE),
                                #  )
                                #),
               
                                #div(virtualSelectInput(inputId = "SwimmerHoverSelect",label = "Hover-text Information:",
                                #                       choices = NULL,showValueAsTags = TRUE,search = TRUE,multiple = TRUE), style = margin_adjust(-15,NA,app_lite)),
                                div(virtualSelectInput(inputId = "HighlightEventSelect",label = "Highlight Event:",
                                                       choices = NULL,showValueAsTags = TRUE,search = TRUE,multiple = TRUE), style = margin_adjust(-15,NA,app_lite)),
               #),
               conditionalPanel(condition = "input.PatientMainPanel == '2'",
                                #conditionalPanel(condition = "output.SuppDataAdded",
                                #                 div(selectInput("LinePlotTable","Data Table:",choices = NULL,
                                #                                 selected = 1), style = margin_adjust(-20,NA,app_lite))
                                #),
                                div(selectizeInput("LinePlotSub","Subset Table:",choices = NULL, selected = 1), style = margin_adjust(-20,NA,app_lite)),
                                conditionalPanel(condition = "input.LinePlotSub != 'Select all data' && input.LinePlotSub != ''",
                                                 div(selectizeInput("LinePlotSubCrit","Subset criteria:",choices = NULL, selected = 1), style = margin_adjust(-25,NA,app_lite))
                                ),
                                conditionalPanel(condition = "output.SuppDataAdded",
                                                 fluidRow(
                                                   column(6, style = margin_adjust(-25,NA,app_lite),
                                                          selectizeInput("LinePlotX","X-Axis", choice = NULL, selected = 1)
                                                   ),
                                                   column(6, style = margin_adjust(-25,NA,app_lite),
                                                          selectInput("LinePlotXunits","X-Axis Units", choices = c("Hours","Days","Months","Years"), selected = "Years")
                                                   )
                                                 )
                                ),
                                fluidRow(
                                  column(6, style = margin_adjust(-30,NA,app_lite),
                                         selectizeInput("LinePlotY","Y-Axis", choice = NULL, selected = 1)
                                  ),
                                  column(6, style = margin_adjust(-30,NA,app_lite),
                                         selectizeInput("LinePunitCol","Y-Axis Units Column", choice = NULL, selected = 1,
                                                        options = list(
                                                          placeholder = 'Optional',
                                                          onInitialize = I('function() { this.setValue(""); }')
                                                        )),
                                         conditionalPanel(condition = "input.LinePunitCol != ''",
                                                          div(selectizeInput("LinePunitSelect","Y-Axis Units", choice = NULL, selected = 1), style = margin_adjust(-15,NA,app_lite,top_new = -10))
                                         )
                                  )
                                ),
                                fluidRow(
                                  column(6, style = margin_adjust(-25,NA,app_lite),
                                         numericInput("linePlotCutP","User defined cut-point:",
                                                      value = NULL)
                                  ),
                                  column(6, style = margin_adjust(-25,NA,app_lite),
                                         textInput("linePlotCutPAnno","Cut-Point Annotation:", placeholder = "i.e. Adverse Event Name")
                                  )
                                ),
                                div(h4("Save Annotation:"), style = "margin-top:-20px"),
                                fluidRow(
                                  column(6, style = margin_adjust(-15,NA,app_lite),
                                         actionButton("saveLinePlotAbvCutP","Above Cut-Point", width = "100%")
                                  ),
                                  column(6, style = margin_adjust(-15,NA,app_lite),
                                         actionButton("saveLinePlotBelCutP","Below Cut-Point", width = "100%")
                                  )
                                )
               ),
               h4("Patient Selection"),
               div(DT::dataTableOutput("PatientSelectionTab"), style = "font-size:10px"),
               p(),
               downloadButton("dnldCohortEventTab","Cohort Event Table")
      ),
      tabPanel("Figure Settings",
               p(),
               conditionalPanel(condition = "input.PatientMainPanel == '1'",
                                uiOutput("rendTimeLineTitle"),
                                ColorPalSelect_UI("PatTimelineColorPal"),
                                selectInput("PatTimelineXunit","X-Axis Unit:", choices = c("Years","Months","Days","Hours"), selected = "Months"),
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
               ),
               conditionalPanel(condition = "input.PatientMainPanel == '2'",
                                ColorPalSelect_UI("PatTimelineSummColorPal"),
                                #conditionalPanel(condition = "output.BiomarkerData",
                                                 selectInput("LinePlotTheme","Select Theme:",
                                                             choices = c("Void" = "theme_void","BW" = "theme_bw","Minimal" = "theme_minimal",
                                                                         "Grey" = "theme_grey","Linedraw" = "theme_linedraw","Light" = "theme_light",
                                                                         "Dark" = "theme_dark","Classic" = "theme_classic","Test" = "theme_test")),
                                                 h4("Font Sizes"),
                                                 fluidRow(
                                                   column(4,
                                                          numericInput("LinePlotTitleSize","Title:",
                                                                       value = 20, step = 1)
                                                   ),
                                                   column(4,
                                                          numericInput("LinePlotXAxisSize","X-Axis:",
                                                                       value = 14, step = 1)
                                                   ),
                                                   column(4,
                                                          numericInput("LinePlotYAxisSize","Y-Axis:",
                                                                       value = 14, step = 1)
                                                   )
                                                 ),
                                #),
                                h4("Figure Download Parameters"),
                                fluidRow(
                                  column(6,
                                         numericInput("LinePlotHeight","Height (in)",value = 8)
                                  ),
                                  column(6,
                                         numericInput("LinePlotWidth","Width (in)",value = 10)
                                  )
                                )
               )
      )
    )
  ),
  mainPanel(
    p(),
    shinycssloaders::withSpinner(
      shinyjqui::jqui_resizable(
        plotlyOutput("PatientTimelineLineSummPlot_full",height = "600px", width = "100%")), type = 6),
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
    #tabsetPanel(
    #  id = "PatientMainPanel",
      #tabPanel("Patient Timeline",
      #         p(),
      #         shinycssloaders::withSpinner(
      #           shinyjqui::jqui_resizable(
      #             plotlyOutput("PatientTimelinePlot",height = "400px", width = "100%")), type = 6),
      #         fluidRow(
      #           column(3,
      #                  downloadButton("dnldPatientEventTab","Patient Event Table")
      #           )
      #         ),
      #         p(),
      #         uiOutput("rendTimelineTableTabs"),
      #         value = 1
      #),
      #tabPanel("Change-Point Analysis",
      #         p(),
      #         shinycssloaders::withSpinner(
      #           shinyjqui::jqui_resizable(
      #             plotlyOutput("PatientTimelineLineSummPlot",height = "600px", width = "100%")), type = 6),
      #         div(DT::dataTableOutput("LinePlotdf"), style = "font-size:14px"),
      #         value = 2
      #),
      #tabPanel("Patient Timeline Full",
      #         p(),
      #         shinycssloaders::withSpinner(
      #           shinyjqui::jqui_resizable(
      #             plotlyOutput("PatientTimelineLineSummPlot_full",height = "600px", width = "100%")), type = 6),
      #         fluidRow(
      #           column(3,
      #                  downloadButton("dnldPatientEventTab_full","Patient Event Table")
      #           )
      #         ),
      #         p(),
      #         tabsetPanel(
      #           tabPanel("Patient Data",
      #                    p(),
      #                    div(DT::dataTableOutput("patient_event_table"), style = "font-size:14px"),
      #                    downloadButton("patient_event_table_Dnld", "Download Table")),
      #           tabPanel("Lesion Data",
      #                    p(),
      #                    div(DT::dataTableOutput("patient_lesion_table"), style = "font-size:14px"),
      #                    downloadButton("patient_lesion_table_Dnld"), "Download Table")),
      #         value = 3
      #)
    )
    #value = 1
  #)
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
                                                        #EventDataTreatmentEvents .vscomp-value {
                                                        max-height: 122px !important;
                                                        overflow-y: auto !important;
                                                        }
                                                        #EventDataResponseEvents .vscomp-value {
                                                        max-height: 122px !important;
                                                        overflow-y: auto !important;
                                                        }
                                                        #TTEstartEvent .vscomp-value {
                                                        max-height: 82px !important;
                                                        overflow-y: auto !important;
                                                        }
                                                        #TTEstopEvent .vscomp-value {
                                                        max-height: 82px !important;
                                                        overflow-y: auto !important;
                                                        }
                                                        .vscomp-value {
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
                                                        #ttepanel1 .scrolling-well {
                                                        max-height: 102px;
                                                        overflow: auto !important;
                                                        border: 1px solid #ddd;
                                                        padding: 10px;
                                                        }
                                                        #ttepanel2 .scrolling-well {
                                                        max-height: 102px;
                                                        overflow: auto !important;
                                                        border: 1px solid #ddd;
                                                        padding: 10px;
                                                        }
                                                        #desc_table table.dataTable td, 
                                                        #desc_table table.dataTable th,
                                                        #example_table table.dataTable td, 
                                                        #example_table table.dataTable th {
                                                          font-size: 12px !important;
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



ui <- navbarPage(
  title = paste("{ ShinyEvents - Rapid Tissue Dontation }",sep = ""),
  id = "shinyevents_tabs",
  theme = shinytheme("flatly"),
  PatientLevel_tab,
  selected = "patient_visual_analytics")



# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  if(!interactive()) pdf(NULL)
  
      # Data Input -------------------------------------------------------------
      
      
      #ProjectName_react <- reactiveVal('ShinyEvents - Rapid Tissue Dontation')
      #Param_File_react <- reactiveVal(Event_Param_File)
      #Workbook_file_predf_react <- reactiveVal()
      #Workbook_file_react <- reactiveVal(Workbook_file)
      #PatientAnno_file_react <- reactiveVal(Patient_Annotation_File)
      #PatientEvent_File_react <- reactiveVal(Event_Data_File)
      #PatientTumor_File_react <- reactiveVal(patient_tumor_data_file)
      
      
      #wkbk_raw_react <- reactiveVal()
      #wkbk_react <- reactiveVal()
      #wkbk_react_anno <- reactiveVal()
      #wkbk_react_anno_sub <- reactiveVal()
      #wkbk_react_sub <- reactiveVal()
      Clin_Supp_Cols_List_react <- reactiveVal()
      pat_react <- reactiveVal()
      #event_data_raw <- reactiveVal(event_data_raw)
      #event_data_summ <- reactiveVal()
      #event_data_tr_clusters_clean <- reactiveVal()
      event_data <- reactiveVal()
      #param_data <- reactiveVal(params)
      #paramEvent_data <- reactiveVal(param_events)
      #GlobalAppTimeUnit_react <- reactiveVal(input$GlobalAppTimeUnit)
      GlobalAppTimeUnit_react <- reactiveVal(GlobalAppTimeUnit)
      #pat_tumors <- reactiveVal()
      #eventDataInput_raw <- reactiveVal(event_data_raw)
      #AllFilesReady_react <- reactiveVal(FALSE)
      #process_input_react <- reactiveVal(NULL)
      
      # URL input
      #observe({
      #  query <- parseQueryString(session$clientData$url_search)
      #  print(query)
      #  if (isTruthy(query[['data']])) {
      #    Workbook_file_react(query[['data']])
      #  }
      #  if (isTruthy(query[['proj']])) {
      #    ProjectName_react(query[['proj']])
      #  }
      #  if (isTruthy(query[['patient']])) {
      #    PatientAnno_file_react(query[['patient']])
      #  }
      #})
      
      # Color Pal Reacts
      colPalSelected <- ColorPalSelect_server("PatTimelineColorPal")
      PatTimelineSummColorPal_sel <- ColorPalSelect_server("PatTimelineSummColorPal")
      
      observeEvent(input$PatTimelineXunit, {
        GlobalAppTimeUnit_react(input$PatTimelineXunit)
      })
      
      # Workbook Adj ------------------------------------------------------------
      # Adjust workbook if needed
      #observe({
      #  print("507")
      #  req(param_data)
      #  req(wkbk_raw_react())
      #  req(GlobalAppTimeUnit_react())
      #  param <- param_data
      #  wkbk <- wkbk_raw_react()
      #  AppTimeUnit <- input$PatTimelineXunit
      #  #AppTimeUnit <- GlobalAppTimeUnit_react()
      #  AppTimeUnit_low <- tolower(AppTimeUnit)
      #  save(list = ls(), file = "Workbook_adj.RData", envir = environment())
      #  if (length(which(is.na(param[,1]) == T)) == 0) {
      #    print("517")
      #    Clin_Supp_Cols_List <- list()
      #    wkbk_tabs <- unique(param[,1])
      #    #st <- Sys.time()
      #    withProgress(message = "Processing Supplementary Data", value = 0, {
      #      data_to_proc <- length(names(wkbk))
      #      print("523")
      #      wkbk_adj <- lapply(names(wkbk), function(df_name) {
      #        incProgress((1/data_to_proc), detail = paste0("Processing: ",df_name))
      #        df <- wkbk[[df_name]]
      #        if (df_name %in% param[,1]) {
      #          param_tab <- unique(param[which(param[,1] == df_name),c(6,7,10,11)])
      #          rownames(param_tab) <- NULL
      #          start_col_names <- unique(param_tab[,"Event Start Column"])
      #          if (any(start_col_names %in% colnames(df))) {
      #            if (all(!is.na(start_col_names))) {
      #              start_col_names <- start_col_names[which(!is.na(start_col_names))]
      #              df <- df %>%
      #                arrange(!!!syms(c(colnames(df)[1],start_col_names))) %>%
      #                as.data.frame()
      #              for (row in 1:nrow(param_tab)) {
      #                row <- unlist(param_tab[row,])
      #                start_col_name <- row["Event Start Column"]
      #                stop_col_name <- row["Event End Column"]
      #                stop_col_name <- ifelse(is.na(stop_col_name),start_col_name,stop_col_name)
      #                start_unit <- row["Event Start Time Units"]
      #                stop_unit <- row["Event End Time Units"]
      #                stop_unit <- ifelse(is.na(stop_unit),start_unit,stop_unit)
      #                
      #                if (!is.na(start_unit)) {
      #                  df[,start_col_name] <- convert_time_units(suppressWarnings(as.numeric(df[,start_col_name])),start_unit,AppTimeUnit_low)
      #                }
      #                if (start_col_name != stop_col_name) {
      #                  if (!is.na(stop_unit)) {
      #                    df[,stop_col_name] <- convert_time_units(suppressWarnings(as.numeric(df[,stop_col_name])),stop_unit,AppTimeUnit_low)
      #                  }
      #                }
      #              }
      #            }
      #          }
      #        }
      #        return(df)
      #      })
      #      
      #    })
      #    #et <- Sys.time()
      #    #print("Adjust workbook")
      #    #print(et-st)
      #    names(wkbk_adj) <- names(wkbk)
      #    Clin_Supp_Cols_List <- lapply(wkbk_adj,function(x){
      #      return(colnames(x)[-1])
      #    })
      #    wkbk_react(wkbk_adj)
      #    wkbk_react_anno(wkbk_adj)
      #    #wkbk_react_anno_sub(wkbk_adj)
      #    wkbk_react_sub(wkbk_adj)
      #    Clin_Supp_Cols_List_react(Clin_Supp_Cols_List)
      #  }
      #  
      #})
      
      ## Event Data ------------------------------------------------------------
      
      observeEvent(input$SummaryLeinInput,{
        updateNumericInput(session,"MainClusterWindowSet",value = input$SummaryLeinInput)
      })
      observeEvent(input$MainClusterWindowSet,{
        updateNumericInput(session,"SummaryLeinInput",value = input$MainClusterWindowSet)
      })
      
      
      
      observe({
        #eventDataInput_raw <- event_data_raw
        cluster_window <- 1
        event_data_processed <- data.frame(Name = event_data_raw[,1],
                                           Event = event_data_raw[,"Event Name"],
                                           EventType = event_data_raw[,"Event Type"],
                                           EventTab = "RTD Event Data",
                                           EventStart = event_data_raw[,"Event Start"],
                                           EventEnd = event_data_raw[,"Event End"],
                                           EventColumn = event_data_raw[,"Event Type"]
        )
        other_cols <- setdiff(colnames(event_data_raw),
                              c("Event Name","Event Type","Event Start","Event End","Event Type",
                                colnames(event_data_processed)))
        if (length(other_cols) > 0) {
          event_data_processed <- cbind(event_data_processed,
                                        event_data_raw[,other_cols])
        }
        event_data_processed[,"EventEnd"] <- ifelse(is.na(event_data_processed[,"EventEnd"]),
                                                    event_data_processed[,"EventStart"],
                                                    event_data_processed[,"EventEnd"])
        event_data_processed[,"EventStart"] <- ifelse(is.na(event_data_processed[,"EventStart"]),
                                                      event_data_processed[,"EventEnd"],
                                                      event_data_processed[,"EventStart"])
        event_data_processed <- event_data_processed[which(!is.na(event_data_processed$Event)),]
        pat_anno <- event_count_df(event_data_processed)
        event_new <- apply(event_data_processed,1,function(x) {
          event <- x[["Event"]]
          eventtype <- x[["EventType"]]
          if (event == eventtype) {
            return(event)
          } else {
            if (grepl(paste0("^",eventtype,": "),event, ignore.case = T)) {
              return(event)
            } else {
              return(paste0(eventtype,": ",event))
            }
          }
        })
        event_data_processed$Event <- event_new
        
        Patient_Event_Data <- event_data_processed
        treatment_events <- unique(param_data[which(param_data$Treatment == TRUE),])
        treatment_events <- unique(ifelse(treatment_events$`Column Defined Event` == FALSE,treatment_events$`Event Name`,
                                          paste0(treatment_events$`Event Category`,": ")))
        response_events <- unique(param_data[which(param_data$Response == TRUE),])
        response_events <- unique(ifelse(response_events$`Column Defined Event` == FALSE,response_events$`Event Name`,
                                         paste0(response_events$`Event Category`,": ")))
        if (length(c(treatment_events,response_events)) > 0) {
          withProgress(message = "Summaizing Event Data", value = 0, {
            if (length(treatment_events) > 0) {
              incProgress(0.25, detail = "Summarizing treatment events")
              event_data_tr <- Patient_Event_Data[grepl(paste(treatment_events,collapse = "|"),Patient_Event_Data$Event),]
              event_data_tr_cls <- eventDataSummary(event_data_tr, event_summary = "Treatment", verbose = F, cluster_window = cluster_window)
            } else {event_data_tr_cls <- NULL}
            if (length(response_events) > 0) {
              incProgress(0.25, detail = "Summarizing response events")
              event_data_re <- Patient_Event_Data[grepl(paste(response_events,collapse = "|"),Patient_Event_Data$Event),]
              event_data_re_cls <- eventDataSummary(event_data_re, event_summary = "Response", verbose = F, cluster_window = cluster_window)
            } else {event_data_re_cls <- NULL}
            incProgress(0.25, detail = "Merging all event data")
            event_data_cls <- rbind(event_data_tr_cls,event_data_re_cls)
            if (!is.null(event_data_cls)) {
              event_data_cls <- event_data_cls %>%
                group_by(Name) %>%
                arrange(!EventType %in% c("Full Treatment Summary","Full Response Summary"), .by_group = TRUE) %>%
                as.data.frame()
              Patient_Event_Data_cls_all <- as.data.frame(data.table::rbindlist(list(event_data_cls,Patient_Event_Data), fill = T))
              Patient_Event_Data_cls_all <- Patient_Event_Data_cls_all[order(Patient_Event_Data_cls_all[,1]),]
              Patient_Event_Data <- Patient_Event_Data_cls_all
            }
            incProgress(0.25, detail = "Colmplete!")
          })
        }
        
        data_cols <- colnames(Patient_Event_Data)[-1]
        Clin_Supp_Cols_List_react(data_cols)
        pat_react(pat_anno)
        event_data(Patient_Event_Data)
        
      })
      
      
      
      #observe({
      #  if (tabs_trigger() == 1) {
      #    req(event_data_raw())
      #    eventDataInput_raw <- event_data_raw()
      #    #input_file <- Event_Data_File
      #    cluster_window <- 1
      #    #eventtype_in <- "Event Type"
      #    # Ensure proper event data formatting
      #    event_data_processed <- data.frame(Name = eventDataInput_raw[,1],
      #                                       Event = eventDataInput_raw[,"Event Name"],
      #                                       EventType = eventDataInput_raw[,"Event Type"],
      #                                       EventTab = "InputData",
      #                                       EventStart = eventDataInput_raw[,"Event Start"],
      #                                       EventEnd = eventDataInput_raw[,"Event End"],
      #                                       EventColumn = eventDataInput_raw[,"Event Type"]
      #    )
      #    if (all(c("EventStart","EventEnd") %in% colnames(event_data_processed))) {
      #      event_data_processed[,"EventEnd"] <- ifelse(is.na(event_data_processed[,"EventEnd"]),
      #                                                  event_data_processed[,"EventStart"],
      #                                                  event_data_processed[,"EventEnd"])
      #      event_data_processed[,"EventStart"] <- ifelse(is.na(event_data_processed[,"EventStart"]),
      #                                                    event_data_processed[,"EventEnd"],
      #                                                    event_data_processed[,"EventStart"])
      #    }
      #    event_data_processed <- event_data_processed[which(!is.na(event_data_processed$Event)),]
      #    # Format mock param file
      #    
      #    #if (isTruthy(param_data)) {
      #      event_params <- param_data
      #    #} else {
      #    #  param_cols <- c("Data Table Name","Data File","Event Name","Column Defined Event","Event Category","Event Start Column",
      #    #                  "Event End Column","Treatment","Response","Event Start Time Units","Event End Time Units")
      #    #  event_params_base <- unique(event_data_processed[,c("EventTab","EventType")])
      #    #  event_params_base[which(is.na(event_params_base$EventTab)),"EventTab"] <- "InputData"
      #    #  event_params <- data.frame(data_table_name = event_params_base$EventTab,
      #    #                             data_file = basename(input_file),
      #    #                             event_name = event_params_base$EventType,
      #    #                             column_defined_event = TRUE,
      #    #                             event_category = event_params_base$EventType,
      #    #                             event_start_col = EventDataEventStartcol,
      #    #                             event_end_col = EventDataEventEndcol,
      #    #                             treatment = FALSE,
      #    #                             response = FALSE,
      #    #                             start_time_units = EventDataEventStartUnits,
      #    #                             end_time_units = EventDataEventEndUnits)
      #    #  colnames(event_params) <- param_cols
      #    #}
      #    
      #    # Create mock wkbk
      #    wkbk <- list(InputData = eventDataInput_raw)
      #    #if (isTruthy(pat_tumors())) {
      #    #  wkbk <- c(wkbk, list(TumorData = pat_tumors()))
      #    #}
      #    # Generate patient selection table
      #    pat_anno <- event_count_df(event_data_processed)
      #    # Format full event names
      #    #if (!is.na(eventtype_in)) {
      #      event_new <- apply(event_data_processed,1,function(x) {
      #        event <- x[["Event"]]
      #        eventtype <- x[["EventType"]]
      #        if (event == eventtype) {
      #          return(event)
      #        } else {
      #          if (grepl(paste0("^",eventtype,": "),event, ignore.case = T)) {
      #            return(event)
      #          } else {
      #            return(paste0(eventtype,": ",event))
      #          }
      #        }
      #      })
      #      event_data_processed$Event <- event_new
      #    #}
      #    # Update reactive vals
      #    #Param_File_react(NULL)
      #    #Workbook_file_predf_react(NULL)
      #    #Workbook_file_react(NULL)
      #    #PatientAnno_file_react(NULL)
      #    #PatientEvent_File_react(NULL)
      #    
      #    wkbk_raw_react(wkbk)
      #    wkbk_react(wkbk)
      #    wkbk_react_anno(wkbk)
      #    #wkbk_react_anno_sub(wkbk)
      #    wkbk_react_sub(wkbk)
      #    Clin_Supp_Cols_List <- lapply(wkbk,function(x){
      #      return(colnames(x)[-1])
      #    })
      #    Clin_Supp_Cols_List_react(Clin_Supp_Cols_List)
      #    pat_react(pat_anno)
      #    event_data_raw(event_data_processed)
      #    event_data_summ(event_data_processed)
      #    event_data(event_data_processed)
      #    param_data(event_params)
      #    paramEvent <- event_params[which(!is.na(event_params[,3])),]
      #    paramEvent_data(paramEvent)
      #    
      #    tabs_trigger(tabs_trigger()+1)
      #    
      #    #process_input_react(NULL)
      #  }
      #  
      #})
      
      # Upload event data file
      #observe({
      #  req(paramEvent_data())
      #  req(event_data_raw())
      #  param <- paramEvent_data()
      #  Patient_Event_Data <- event_data_raw()
      #  cluster_window <- Event_Cluster_Window
      #  # Event data already has summary columns
      #  #st <- Sys.time()
      #  #save(list = ls(), file = "event_data_summ.RData", envir = environment())
      #  #if (!any(grepl("summary$", colnames(Patient_Event_Data), ignore.case = T))) {
      #    treatment_events <- unique(param[which(param$Treatment == TRUE),])
      #    treatment_events <- unique(ifelse(treatment_events$`Column Defined Event` == FALSE,treatment_events$`Event Name`,
      #                                      paste0(treatment_events$`Event Category`,": ")))
      #    response_events <- unique(param[which(param$Response == TRUE),])
      #    response_events <- unique(ifelse(response_events$`Column Defined Event` == FALSE,response_events$`Event Name`,
      #                                     paste0(response_events$`Event Category`,": ")))
      #    if (length(c(treatment_events,response_events)) > 0) {
      #      withProgress(message = "Summaizing Event Data", value = 0, {
      #        if (length(treatment_events) > 0) {
      #          incProgress(0.25, detail = "Summarizing treatment events")
      #          event_data_tr <- Patient_Event_Data[grepl(paste(treatment_events,collapse = "|"),Patient_Event_Data$Event),]
      #          event_data_tr_cls <- eventDataSummary(event_data_tr, event_summary = "Treatment", verbose = F, cluster_window = cluster_window)
      #        } else {event_data_tr_cls <- NULL}
      #        if (length(response_events) > 0) {
      #          incProgress(0.25, detail = "Summarizing response events")
      #          event_data_re <- Patient_Event_Data[grepl(paste(response_events,collapse = "|"),Patient_Event_Data$Event),]
      #          event_data_re_cls <- eventDataSummary(event_data_re, event_summary = "Response", verbose = F, cluster_window = cluster_window)
      #        } else {event_data_re_cls <- NULL}
      #        incProgress(0.25, detail = "Merging all event data")
      #        event_data_cls <- rbind(event_data_tr_cls,event_data_re_cls)
      #        if (!is.null(event_data_cls)) {
      #          event_data_cls$Event <- gsub("Cluster \\d+$","Cluster",event_data_cls$Event)
      #          event_data_cls <- event_data_cls %>%
      #            group_by(Name) %>%
      #            arrange(!EventType %in% c("Full Treatment Summary","Full Response Summary"), .by_group = TRUE)
      #          Patient_Event_Data_cls_all <- data.table::rbindlist(list(event_data_cls,Patient_Event_Data), fill = T)
      #          Patient_Event_Data_cls_all <- Patient_Event_Data_cls_all[order(Patient_Event_Data_cls_all[,1]),]
      #          Patient_Event_Data_cls_all <- as.data.frame(Patient_Event_Data_cls_all)
      #          Patient_Event_Data <- Patient_Event_Data_cls_all
      #        }
      #        incProgress(0.25, detail = "Colmplete!")
      #      })
      #    }
      #  #}
      #  #et <- Sys.time()
      #  #print("update event data")
      #  #print(et-st)
      #  event_data_summ(Patient_Event_Data)
      #  
      #})
      
      #sankey_added_events <- reactiveVal()
      #observe({
      #  req(event_data_summ())
      #  req(paramEvent_data())
      #  event_data <- event_data_summ()
      #  param <- paramEvent_data()
      #  treatment_events <- param[which(param$Treatment == TRUE),"Event Category"]
      #  #save(list = ls(), file = "event_data_tr_clusters_clean.RData", envir = environment())
      #  if (length(unique(treatment_events)) > 0) {
      #    treat_summ_events <- paste0(unique(param[which(param$Treatment == TRUE),"Event Category"])," Summary")
      #    event_data_treat_summ <- event_data[which(event_data$EventType %in% c(treat_summ_events,"Full Treatment Summary")),]
      #    event_data_treat_summ$EventType <- ifelse(event_data_treat_summ$EventType == "Full Treatment Summary","Full Treatment Summary",
      #                                              gsub(" Summary$","",event_data_treat_summ$EventType))
      #    # Assign numeric cluster line treatments
      #    event_data_clusters <- event_data_treat_summ %>%
      #      mutate(Event = EventType) %>%
      #      group_by(Name,Event) %>%
      #      arrange(EventStart,EventEnd, .by_group = TRUE) %>%
      #      mutate(Treatment_Line_Cluster = paste0(unique(Event)," Cluster Line ",seq(n())),
      #             Event = Treatment_Line_Cluster) %>%
      #      #mutate(Event = Treatment_Line_Cluster) %>%
      #      select(-c(EventTab,EventColumn)) %>%
      #      as.data.frame()
      #    event_data_clusters$EventType <- ifelse(event_data_clusters$EventType == "Full Treatment Summary","Full Treatment Summary",
      #                                            paste0(event_data_clusters$EventType," Cluster"))
      #    # Clean event summary
      #    sankey_event_opts <- unique(event_data_clusters$EventType)
      #    event_data_clusters_clean <- lapply(sankey_event_opts, function(x) {
      #      event_data_clusters_sub <- event_data_clusters[which(event_data_clusters$EventType == x),]
      #      x <- ifelse(x == "Full Treatment Summary","Full Treatment Summary",gsub(" Cluster$","",x))
      #      if (all(grepl(gsub(" Summary$|Summary$","",x),event_data_clusters_sub$EventSummary))) {
      #        # Remove EventSummary header
      #        event_data_clusters_sub$EventSummary <- gsub(gsub(" Summary$|Summary$","",x),"",event_data_clusters_sub$EventSummary)
      #        # trim leading and trailing white space
      #        event_data_clusters_sub$EventSummary <- trimws(event_data_clusters_sub$EventSummary)
      #        # Remove delimiting white space
      #        event_data_clusters_sub$EventSummary <- gsub(", ",",",event_data_clusters_sub$EventSummary)
      #        # Remove hidden end of line characters
      #        event_data_clusters_sub$EventSummary <- gsub("\\\r\n  |\\\n  ",", ",event_data_clusters_sub$EventSummary)
      #      } else {
      #        new_col <- unname(sapply(event_data_clusters_sub$EventSummary, function(y) {
      #          # breakdown summary column into parts
      #          check_splt <- strsplit(y,"\\\r\n|\\\n")[[1]]
      #          if (length(check_splt) > 1) {
      #            # Look for summary event header but checking for portion without leading space
      #            headers <- grep("^\\s+",check_splt,invert = T)
      #            # if there is more than one event type in the summary
      #            if (length(headers) > 1) {
      #              new_col <- paste0(sapply(seq_along(headers), function(i) {
      #                header <- check_splt[headers[i]]
      #                start <- headers[i] + 1
      #                end <- if (i < length(headers)) headers[i+1] - 1 else length(check_splt)
      #                items <- trimws(check_splt[start:end])
      #                items_joined <- paste0(items, collapse = ", ")
      #                result_parts <- paste0(header, ": ", items_joined)
      #                return(result_parts)
      #              }), collapse = " - ")
      #            } else {
      #              # event summary only contains one event type
      #              # remove comma's with space from original test, to differentiate from edited version
      #              check_splt_tocollapse <- gsub(", ",",",check_splt[-headers])
      #              new_col <- trimws(paste0(check_splt_tocollapse, collapse = ", "))
      #              new_col <- paste0(check_splt[headers],": ",new_col)
      #            }
      #          } else {
      #            check_splt_tocollapse <- gsub(", ",",",check_splt)
      #            new_col <- trimws(paste0(check_splt_tocollapse, collapse = ", "))
      #          }
      #        }))
      #        event_data_clusters_sub$EventSummary <- new_col
      #      }
      #      return(event_data_clusters_sub)
      #    })
      #    event_data_clusters_clean <- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, all = TRUE),
      #                                        event_data_clusters_clean)
      #    event_data_tr_clusters_clean(event_data_clusters_clean)
      #    
      #    sankey_added <- unique(paste0(event_data_clusters_clean$Event,": ",event_data_clusters_clean$EventSummary))
      #    sankey_added_events(sankey_added)
      #  }
      #})
      
      #observe({
      #  req(event_data_summ())
      #  req(event_data_tr_clusters_clean())
      #  event_data <- event_data_summ()
      #  event_data_tr_clusters_clean <- event_data_tr_clusters_clean()
      #  event_data_clusters_revent <- event_data_tr_clusters_clean %>%
      #    mutate(EventType = Event) %>%
      #    mutate(Event = paste0(Event,": ",EventSummary)) %>%
      #    select(Name,Event,EventType,EventStart,EventEnd)
      #  event_data2 <- as.data.frame(rbindlist(list(event_data,event_data_clusters_revent),fill = T))
      #  event_data3 <- event_data2 %>%
      #    group_by(!!sym(colnames(event_data2)[1])) %>%
      #    arrange(Event %in% unique(event_data2$Event), .by_group = TRUE) %>%
      #    as.data.frame()
      #  event_data(event_data3)
      #})
      
      #event_data_key <- reactive({
      #  req(paramEvent_data())
      #  req(event_data())
      #  paramEvent_data <- paramEvent_data()
      #  event_data <- event_data()
      #  #added_events <- added_events()
      #  sankey_added_events <- sankey_added_events()
      #  events_exp <- unique(paramEvent_data[which(paramEvent_data[,"Column Defined Event"] == TRUE),"Event Name"])
      #  event_data_key <- event_data %>%
      #    select(Event,EventType,EventTab,EventColumn) %>%
      #    unique() %>%
      #    mutate(EventLabel = case_when(
      #      Event %in% sankey_added_events ~ EventType,
      #      #Event %in% added_events ~ paste0(EventTab,": Cut-Points"),
      #      !is.na(EventTab) & !is.na(EventColumn) ~ paste0(EventTab,": ",EventColumn),
      #      is.na(EventTab) ~ EventType
      #    )) %>%
      #    mutate(EventExpanded = ifelse(EventColumn %in% events_exp | Event %in% sankey_added_events,TRUE,FALSE)) %>%
      #    #mutate(EventExpanded = ifelse(EventColumn %in% events_exp | Event %in% sankey_added_events | Event %in% added_events,TRUE,FALSE)) %>%
      #    as.data.frame()
      #  event_data_key$EventSpecified <- str_split_fixed(event_data_key$Event,": ",2)[,-1]
      #  event_data_key$EventSpecified[event_data_key$EventSpecified == ""] <- NA
      #  event_data_key$EventSpecified <- ifelse(is.na(event_data_key$EventSpecified) & event_data_key$EventExpanded == TRUE,event_data_key$Event,event_data_key$EventSpecified)
      #  #event_data_key$EventSpecified <- ifelse(event_data_key$Event %in% added_events,event_data_key$Event,event_data_key$EventSpecified)
      #  event_data_key$EventSpecified <- ifelse(event_data_key$Event %in% sankey_added_events,event_data_key$Event,event_data_key$EventSpecified)
      #  rownames(event_data_key) <- NULL
      #  event_data_key
      #})
      
      ## Render UI ----------------------------------------------------
      
      
      
      #inserted_tabs <- reactiveVal(character())
      #tabs_trigger <- reactiveVal(start_trigger)
      
      #observe({
      #  if (tabs_trigger() > 0) {
      #    req(wkbk_react_sub())
      #    wkbk <- wkbk_react_sub()
      #    new_tab_names <- names(wkbk)
      #    lapply(inserted_tabs(), function(tab_name) {
      #      removeTab(inputId = "PreProcessingTabs", target = tab_name)
      #    })
      #    if (start_trigger > 0) {
      #      appendTab(inputId = "PreProcessingTabs",
      #                tab = tabPanel("Event Parameters",
      #                               p(),
      #                               div(DT::dataTableOutput(paste0("Event Parameters", "_WkbkTable")), style = "font-size:12px"),
      #                               downloadButton(paste0("Event Parameters", "_WkbkTableDnld"), "Download Table")
      #                ),
      #                select = TRUE,
      #                session = session
      #      )
      #    }  else {
      #      insertTab(inputId = "PreProcessingTabs",
      #                tab = tabPanel("Event Parameters",
      #                               p(),
      #                               div(DT::dataTableOutput(paste0("Event Parameters", "_WkbkTable")), style = "font-size:12px"),
      #                               downloadButton(paste0("Event Parameters", "_WkbkTableDnld"), "Download Table")
      #                ),
      #                target = "Input Data Formatting",
      #                position = "after",
      #                select = FALSE
      #      )
      #    }
      #    lapply(new_tab_names, function(tab_name) {
      #      insertTab(inputId = "PreProcessingTabs",
      #                tab = tabPanel(tab_name,
      #                               p(),
      #                               div(DT::dataTableOutput(paste0(tab_name, "_WkbkTable")), style = "font-size:12px"),
      #                               downloadButton(paste0(tab_name, "_WkbkTableDnld"), "Download Table")
      #                ),
      #                target = "Event Parameters",
      #                position = "after",
      #                select = FALSE
      #      )
      #    })
      #    # Optionally insert Event Data tab
      #    if (isTruthy(event_data())) {
      #      insertTab(inputId = "PreProcessingTabs",
      #                tab = tabPanel("Event Data",
      #                               p(),
      #                               div(DT::dataTableOutput("EventDataTable"), style = "font-size:12px"),
      #                               downloadButton("EventDataTable_dnld", "Download Table")
      #                ),
      #                target = "Event Parameters",
      #                position = "after",
      #                select = FALSE
      #      )
      #      new_tab_names <- c("Event Parameters",new_tab_names,"Event Data")
      #    }
      #    inserted_tabs(new_tab_names)
      #  }
      #  
      #})
      
      #output$EventDataTable <- DT::renderDataTable({
      #  event_data <- event_data()
      #  DT::datatable(event_data,
      #                escape = F,
      #                class = "display nowrap",
      #                extensions = 'ColReorder',
      #                options = list(lengthMenu = c(5, 10, 20, 100, 1000),
      #                               pageLength = 20,
      #                               scrollX = T,
      #                               target = "cell",
      #                               colReorder = TRUE),
      #                rownames = F
      #  )
      #})
      
      ## Generate Patient Clinical data tables
      #shiny::observe({
      #  req(wkbk_react_sub())
      #  wkbk <- wkbk_react_sub()
      #  
      #  lapply(names(wkbk), function(i) {
      #    output[[paste0(i,"_WkbkTable")]] <- DT::renderDataTable({
      #      df <- wkbk[[i]]
      #      DT::datatable(df,
      #                    escape = F,
      #                    class = "display nowrap",
      #                    extensions = 'ColReorder',
      #                    options = list(lengthMenu = c(5, 10, 20, 100, 1000),
      #                                   pageLength = 20,
      #                                   scrollX = T,
      #                                   target = "cell",
      #                                   colReorder = TRUE),
      #                    rownames = F
      #      )
      #    })
      #  })
      #})
      
      #output[["Event Parameters_WkbkTable"]] <- DT::renderDataTable({
      #  param_data <- param_data
      #  DT::datatable(param_data,
      #                escape = F,
      #                class = "display nowrap",
      #                extensions = 'ColReorder',
      #                options = list(lengthMenu = c(5, 10, 20, 100, 1000),
      #                               pageLength = 20,
      #                               scrollX = T,
      #                               target = "cell",
      #                               colReorder = TRUE),
      #                rownames = F
      #  )
      #})
      
      ## Patient Anno ----------------------------------------------------------
      
      #observe({
      #  #if (isTruthy(PatientAnno_file_react())) {
      #  #  patAnno <- as.data.frame(fread(PatientAnno_file_react()))
      #  #  pat_react(patAnno)
      #  #} else {
      #    req(event_data_raw())
      #    event_data <- event_data_raw()
      #    patAnno <- event_count_df(event_data)
      #    pat_react(patAnno)
      #  #}
      #})
      
      # Patient Analysis -------------------------------------------------------
      ## Swim --------------------------------------------------
      ## Render UI
      
      patient_id_react <- reactive({
        req(input$PatientSelectionTab_rows_selected)
        req(Patient_Table_React())
        Patient_Row_Selec <- input$PatientSelectionTab_rows_selected
        Patient_Table_df <- Patient_Table_React()
        Patient <- Patient_Table_df[Patient_Row_Selec,1]
        Patient
      })
      event_data_index <- reactive({
        req(event_data())
        event_data <- event_data()
        event_data_index <- sapply(unique(event_data[,1]),function(patient_id) {
          pat_event_data <- event_data[which(event_data[,1] == patient_id),]
          pat_event_data_uniq <- unique(pat_event_data[,c(2,3)])
          pat_event_opts <- split(pat_event_data_uniq[,1], pat_event_data_uniq[,2])
          return(pat_event_opts)
        }, USE.NAMES = T, simplify = F)
        event_data_index
      })
      
      highlight_opts_index <- reactive({
        req(patient_event_data_single())
        pat_event_data <- patient_event_data_single()
        #print(colnames(pat_event_data))
        pat_event_data_uniq <- unique(pat_event_data[,c("Event_Age","EventType")])
        highlight_opts <- split(pat_event_data_uniq[,"Event_Age"], pat_event_data_uniq[,"EventType"])
        highlight_opts
      })
      
      patient_event_data_single <- reactive({
        req(event_data())
        req(patient_id_react())
        event_data <- event_data()
        Patient <- patient_id_react()
        pat_event_data <- event_data[which(event_data$Name == Patient),]
        
        pat_event_data <- pat_event_data %>%
          mutate(Age_Comb = case_when(
            is.na(EventStart) | is.na(EventEnd) ~ as.character(formatC(sum(EventStart,EventEnd,na.rm = T))),
            EventStart == EventEnd ~ as.character(formatC(EventStart)),
            EventStart != EventEnd ~ paste0(as.character(formatC(EventStart))," - ",as.character(formatC(EventEnd)))
          )) %>%
          mutate(Event_Age = paste0(Event,": ",Age_Comb)) %>%
          as.data.frame()
        pat_event_data
        
      })
      
      patient_selected_data <- reactive({
        req(patient_id_react())
        req(patient_event_data_single())
        req(event_data_index())
        req(highlight_opts_index())
        edi <- event_data_index()
        patient <- patient_id_react() 
        pat_event_opts <- edi[[patient]]
        pat_event_data <- patient_event_data_single()
        pat_event_hili <- highlight_opts_index()
        return(list(patient = patient,
                    pat_event_opts = pat_event_opts,
                    pat_event_data = pat_event_data,
                    pat_event_hili = pat_event_hili))
        
      })
      #EventsToHighlight <- input$HighlightEventSelect
      #  save(list = ls(), file = "PatientTimelinePlot_df.RData", envir = environment())
      #  if (length(EventsToHighlight) > 0) {
      #    Patient_Event_Data_sub$highlight <- ifelse(Patient_Event_Data_sub$Event_Age %in% EventsToHighlight,TRUE,FALSE)
      #  }
      #  Patient_Event_Data_sub <- Patient_Event_Data_sub %>%
      #    select(-any_of(c("Age_Comb","Event_Age"))) %>%
      #    #select(any_of(c("Name","Event","EventType","EventStart","EventEnd",hoverCols))) %>%
      #    as.data.frame()
      
      #patient_event_data_helper <- reactiveVal()
      #patient_event_data_single <- reactiveVal()
      #single_patient_swimmer_ylines <- reactiveVal()
      #single_patient_highlight_opt <- reactiveVal()
      
      #observe({
      #  req(input$PatientSelectionTab_rows_selected)
      #  req(Patient_Table_React())
      #  req(event_data())
      #  event_data <- event_data()
      #  Patient_Row_Selec <- input$PatientSelectionTab_rows_selected
      #  Patient_Table_df <- Patient_Table_React()
      #  Patient <- Patient_Table_df[Patient_Row_Selec,1]
      #  
      #  save(list = ls(), file = "patient_event_data_helper.RData", envir = environment())
      #  pat_event_data <- event_data[which(event_data$Name == Patient),]
      #  #patient_event_data_single(pat_event_data)
      #  
      #  #pat_event_data_uniq <- unique(pat_event_data[,c(2,3)])
      #  #pat_event_opts <- split(pat_event_data_uniq[,1], pat_event_data_uniq[,2])
      #  #pat_event_opts <- pat_event_opts[unique(pat_event_data_uniq[,2])]
      #  #single_patient_swimmer_ylines(pat_event_opts)
      #  
      #  pat_event_data <- pat_event_data %>%
      #    mutate(Age_Comb = case_when(
      #      is.na(EventStart) | is.na(EventEnd) ~ as.character(formatC(sum(EventStart,EventEnd,na.rm = T))),
      #      EventStart == EventEnd ~ as.character(formatC(EventStart)),
      #      EventStart != EventEnd ~ paste0(as.character(formatC(EventStart))," - ",as.character(formatC(EventEnd)))
      #    )) %>%
      #    mutate(Event_Age = paste0(Event,": ",Age_Comb)) %>%
      #    as.data.frame()
      #  #patient_event_data_helper(pat_event_data)
      #  highlight_opts <- split(pat_event_data[,"Event_Age"], pat_event_data[,"EventType"])
      #  single_patient_highlight_opt(highlight_opts)
      #  
      #})
      
      #swimmer_hover_opts <- reactiveVal()
      #shiny::observe({
      #  req(Clin_Supp_Cols_List_react())
      #  Clin_Supp_Cols_List_2 <- Filter(Negate(anyNA), Clin_Supp_Cols_List_react())
      #  Clin_Supp_Cols_List_3 <- lapply(names(Clin_Supp_Cols_List_2),function(x) {
      #    paste0(x,": ",Clin_Supp_Cols_List_2[[x]])
      #  })
      #  names(Clin_Supp_Cols_List_3) <- names(Clin_Supp_Cols_List_2)
      #  swimmer_hover_opts(Clin_Supp_Cols_List_3)
      #})
      #shiny::observe({
      #  #req(swimmer_hover_opts())
      #  swimmer_hover_opts <- colnames(event_data)[-1]
      #  #preSelectedGet_new <- c("Diagnosis: Histology","Diagnosis: ClinTStage","Diagnosis: ClinNStage","Diagnosis: ClinMStage",
      #  #                        "Diagnosis: PathTStage","Diagnosis: PathNStage","Diagnosis: PathMStage",
      #  #                        "Imaging: EvidenceOfLesion","Imaging: ImageLesionGrowth","Imaging: ImageLesionSiteDesc",
      #  #                        "Radiation: RadModality","Radiation: ExtBeamTechnique","Radiation: RadDose","Radiation: RadFractions",
      #  #                        "Surgery Biopsy: SurgeryBiopsyLocation","Surgery Biopsy: SiteDiagnostic","Surgery Biopsy: SiteTherapeutic","Surgery Biopsy: SitePalliative",
      #  #                        "Tumor Marker: TMarkerTest","Tumor Marker: TMarkerResultValue",
      #  #                        "Physical Assessment: BodyHeight","Physical Assessment: BodyWeight","Physical Assessment: BMI")
      #  #
      #  shinyWidgets::updateVirtualSelect(session = session,inputId = "SwimmerHoverSelect",choices = swimmer_hover_opts, selected = NULL)
      #  #shinyWidgets::updateVirtualSelect(session = session,inputId = "HighlightEventSelect",choices = swimmer_hover_opts, selected = preSelectedGet_new)
      #})
      observe({
        req(patient_selected_data())
        highlight_opts <- patient_selected_data()$pat_event_hili
        swimmer_y_lines <- patient_selected_data()$pat_event_opts
        #req(highlight_opts_index())
        #req(patient_event_data_single())
        #req(single_patient_highlight_opt())
        #req(input$SwimmerYlinesSelect)
        #swimmer_y_lines <- input$SwimmerYlinesSelect
        
        #highlight_opts <- highlight_opts_index()
        #highlight_opts <- single_patient_highlight_opt()
        highlight_opts <- lapply(highlight_opts,function(x) {
          grep(paste0(swimmer_y_lines,collapse = "|"),x,value = T)
        })
        highlight_opts <- Filter(length,highlight_opts)
        highlight_opts_select <- ifelse(any(grepl("Metasta",names(highlight_opts), ignore.case = T)),
                                        highlight_opts[[grep("Metasta",names(highlight_opts),value = T, ignore.case = T)[1]]],
                                        "")
        
        shinyWidgets::updateVirtualSelect(session = session,inputId = "HighlightEventSelect",choices = highlight_opts, selected = highlight_opts_select)
      })
      
      ## Render tabset panel below timeline
      #output$rendTimelineTableTabs <- renderUI({
      #  
      #  tabs <- c("Patient Events",names(wkbk_react_sub()))
      #  
      #  myTabs <- lapply(1:length(tabs), function(x){
      #    tabPanel(paste(tabs[x]),
      #             p(),
      #             div(DT::dataTableOutput(paste0(tabs[x],"_Table")), style = "font-size:14px"),
      #             downloadButton(paste0(tabs[x],"_TableDnld"), "Download Table")
      #    )
      #  })
      #  do.call(tabsetPanel, myTabs)
      #})
      
      ## timeline plot customization
      output$rendTimeLineTitle <- renderUI({
        Patient_Row_Selec <- input$PatientSelectionTab_rows_selected
        Patient_Table_df <- Patient_Table_React()
        Patient <- Patient_Table_df[Patient_Row_Selec,1]
        SwimmerTitle <- paste0("Clinical Course of Patient: ", as.character(Patient))
        textInput("TimeLineTitle","Title:",value = SwimmerTitle)
      })
      
      ## Reactive
      ## Generate Patient Clinical data tables
      #pat_wkbk_react_sub <- reactive({
      #  req(wkbk_react_sub())
      #  req(input$PatientSelectionTab_rows_selected)
      #  req(Patient_Table_React())
      #  wkbk <- wkbk_react_sub()
      #  Patient_Row_Selec <- input$PatientSelectionTab_rows_selected
      #  Patient_Table_df <- Patient_Table_React()
      #  Patient <- Patient_Table_df[Patient_Row_Selec,1]
      #  #save(list = ls(), file = "pat_wkbk_react_sub.RData", envir = environment())
      #  pat_wkbk <- lapply(wkbk,function(x) {
      #    return(x[which(x[,1] == Patient),])
      #  })
      #  pat_wkbk
      #})
      #shiny::observe({
      #  req(pat_wkbk_react_sub())
      #  Clin_Supp_List <- pat_wkbk_react_sub()
      #  req(PatientTimelinePlot_df())
      #  pat_table <- PatientTimelinePlot_df()
      #  pat_table <- pat_table %>%
      #    relocate(any_of(c("highlight","EventTab")), .after = EventEnd)
      #  Clin_Supp_List <- c(list(`Patient Events` = pat_table),
      #                      Clin_Supp_List)
      #  
      #  lapply(names(Clin_Supp_List), function(i) {
      #    output[[paste0(i,"_Table")]] <- DT::renderDataTable({
      #      df <- Clin_Supp_List[[i]]
      #      DT::datatable(df,
      #                    escape = F,
      #                    class = "display nowrap",
      #                    extensions = 'ColReorder',
      #                    options = list(lengthMenu = c(5, 10, 20, 100, 1000),
      #                                   pageLength = 100,
      #                                   scrollX = T,
      #                                   target = "cell",
      #                                   colReorder = TRUE),
      #                    rownames = F
      #      )
      #    })
      #  })
      #})
      
      ## Generate updated patient annotation table for UI
      Patient_Table_React <- reactive({
        Patient_Annotation <- pat_react()
        Patient_Annotation
      })
      ## Render Patient Selection Table
      output$PatientSelectionTab <- DT::renderDataTable({
        Patient_Table_df <- Patient_Table_React()
        if (nrow(Patient_Table_df) > 10) {
          DT::datatable(Patient_Table_df,
                        extensions = 'Scroller',
                        options = list(lengthMenu = c(5,10, 20, 100, 1000),
                                       pageLength = 20,
                                       scrollX = T,
                                       scrollY = 400),
                        selection = list(mode = 'single', selected = 1),
                        rownames = F
          )
        } else {
          DT::datatable(Patient_Table_df,
                        extensions = 'Scroller',
                        options = list(lengthMenu = c(5,10, 20, 100, 1000),
                                       scrollX = T
                        ),
                        selection = list(mode = 'single', selected = 1),
                        rownames = F
          )
        }
        
      })
      
      ## Plot
      #observe({
      #  wkbk <- pat_wkbk_react_sub()
      #  pat_event_data <- patient_event_data_single()
      #  pat_event_data_help <- patient_event_data_helper()
      #  hoverCols <- input$SwimmerHoverSelect
      #  swimmer_hover_opts <- swimmer_hover_opts()
      #  param <- param_data
      #  event_data_key <- event_data_key()
      #  #save(list = ls(), file = "event_data_key.RData", envir = environment())
      #})
      
      #patient_event_data_single_hover <- reactive({
      #  req(pat_wkbk_react_sub())
      #  req(patient_event_data_helper())
      #  #req(swimmer_hover_opts())
      #  req(event_data_key())
      #  #req(param_data)
      #  
      #  wkbk <- pat_wkbk_react_sub()
      #  pat_event_data <- patient_event_data_helper()
      #  hoverCols <- input$SwimmerHoverSelect
      #  #swimmer_hover_opts <- swimmer_hover_opts()
      #  #event_data_key <- event_data_key()
      #  
      #  
      #  pat_event_data_out <- pat_event_data %>%
      #    select(any_of(c("Name","Event","EventType","EventStart","EventEnd")))
      #  
      #  
      #  param <- param_data
      #  
      #  #Patient_Event_Data_sub <- pat_event_data
      #  event_order <- pat_event_data
      #  save(list = ls(), file = "patient_event_data_single_hover.RData", envir = environment())
      #  
      #  if (nrow(pat_event_data) > 0) {
      #    if (isTruthy(hoverCols)) {
      #      for (col in hoverCols) {
      #        
      #        #colAnno_dfName <- strsplit(col,": ")[[1]][1]
      #        #colAnno <- paste0(strsplit(col,": ")[[1]][-1],collapse = ": ")
      #        
      #        #anno_df <- wkbk[[colAnno_dfName]]
      #        anno_df <- pat_event_data
      #        #startCol <- param[which(param[,1] == colAnno_dfName),6]
      #        #startCol <- startCol[!is.na(startCol)]
      #        #stopCol <- param[which(param[,1] == colAnno_dfName),7]
      #        #stopCol[which(is.na(stopCol))] <- startCol[which(is.na(stopCol))]
      #        #EventTab <- unique(param[which(param[,1] == colAnno_dfName),1])
      #        #
      #        #startCol <- unique(startCol)
      #        #stopCol <- unique(stopCol)
      #        
      #        if (nrow(anno_df) > 0) {
      #        #if (!all(is.na(startCol)) & nrow(anno_df) > 0) {
      #          #for (timeCol in seq(startCol)) {
      #          timeCol <- startCol
      #            startCol_name <- startCol[timeCol]
      #            # If event is expanded
      #            if (unique(param[which(param[,1] == colAnno_dfName & param[,6] == startCol_name),4])) {
      #              temp_df <- anno_df %>%
      #                select(any_of(c(colnames(anno_df)[1],startCol[timeCol],stopCol[timeCol],colAnno))) %>%
      #                unique() %>%
      #                rename(any_of(c(EventStart = startCol[timeCol], EventEnd = stopCol[timeCol], Name = colnames(anno_df)[1]))) %>%
      #                as.data.frame()
      #              if ("EventStart" %in% colnames(temp_df)) {
      #                if (!"EventEnd" %in% colnames(temp_df)) {
      #                  temp_df[,"EventEnd"] <- temp_df[,"EventStart"]
      #                }
      #                temp_df[which(is.na(temp_df[,"EventEnd"])),"EventEnd"] <- temp_df[which(is.na(temp_df[,"EventEnd"])),"EventStart"]
      #              }
      #              Patient_Event_Data_sub <- merge(Patient_Event_Data_sub,temp_df, all.x = T, sort = F)
      #            } else {
      #              # If event is simple - not expanded
      #              temp_df <- anno_df %>%
      #                select(any_of(c(colnames(anno_df)[1],startCol[timeCol],stopCol[timeCol],colAnno))) %>%
      #                unique() %>%
      #                rename(any_of(c(EventStart = startCol[timeCol], EventEnd = stopCol[timeCol], Name = colnames(anno_df)[1]))) %>%
      #                as.data.frame()
      #              # This catches that the annotation is not event specific
      #              if (nrow(temp_df) > 1) {
      #                if ("EventStart" %in% colnames(temp_df)) {
      #                  if (!"EventEnd" %in% colnames(temp_df)) {
      #                    temp_df[,"EventEnd"] <- temp_df[,"EventStart"]
      #                  }
      #                  temp_df[which(is.na(temp_df[,"EventEnd"])),"EventEnd"] <- temp_df[which(is.na(temp_df[,"EventEnd"])),"EventStart"]
      #                }
      #                Patient_Event_Data_sub <- merge(Patient_Event_Data_sub,temp_df, all.x = T, sort = F)
      #              } else {
      #                temp_df <- temp_df %>%
      #                  select(-any_of(c("EventStart","EventEnd"))) %>%
      #                  as.data.frame()
      #                Patient_Event_Data_sub <- merge(Patient_Event_Data_sub,temp_df, all.x = T, sort = F)
      #              }
      #            }
      #            
      #          #}
      #        } else {
      #          annos <- paste(unique(anno_df[,colAnno]), collapse = " - ")
      #          Patient_Event_Data_sub[,colAnno] <- annos
      #        }
      #      }
      #    }
      #    Patient_Event_Data_sub <- merge(event_order,Patient_Event_Data_sub,all.x = T, sort = F)
      #    Patient_Event_Data_sub
      #  }
      #})
      
      ## Generate Patient timeline plot
      #PatientTimelinePlot_df <- reactive({
      #  req(patient_event_data_helper())
      #  Patient_Event_Data_sub <- patient_event_data_helper()
      #  #req(patient_event_data_single_hover())
      #  #patient_event_data_single_hover <- patient_event_data_single_hover()
      #  #Patient_Event_Data_sub <- patient_event_data_single_hover
      #  #hoverCols <- input$SwimmerHoverSelect
      #  EventsToHighlight <- input$HighlightEventSelect
      #  save(list = ls(), file = "PatientTimelinePlot_df.RData", envir = environment())
      #  if (length(EventsToHighlight) > 0) {
      #    Patient_Event_Data_sub$highlight <- ifelse(Patient_Event_Data_sub$Event_Age %in% EventsToHighlight,TRUE,FALSE)
      #  }
      #  Patient_Event_Data_sub <- Patient_Event_Data_sub %>%
      #    select(-any_of(c("Age_Comb","Event_Age"))) %>%
      #    #select(any_of(c("Name","Event","EventType","EventStart","EventEnd",hoverCols))) %>%
      #    as.data.frame()
      #  
      #  
      #  Patient_Event_Data_sub
      #})
      
      observe({
        req(patient_selected_data())
        pat_event_opts <- patient_selected_data()$pat_event_opts
        displaySumm <- input$displaySummaryRows
        #req(patient_id_react())
        #req(event_data_index())
        ##req(single_patient_swimmer_ylines())
        #displaySumm <- input$displaySummaryRows
        ##pat_event_opts <- single_patient_swimmer_ylines()
        ##sankey_events <- sankey_added_events()
        ##print(displaySumm)
        ##print(pat_event_opts)
        #edi <- event_data_index()
        #patient <- patient_id_react() 
        #pat_event_opts <- edi[[patient]]
        #save(list = ls(), file = "SwimmerYlinesSelect.RData", envir = environment())
        if (!displaySumm) { # Remove summary swimmer lines
          pat_event_opts_select <- unname(unlist(pat_event_opts[grep("Summary$",names(pat_event_opts),invert = T)]))
        } else {
          pat_event_opts_select <- unname(unlist(pat_event_opts))
        }
        #if (length(sankey_events) > 0) {
        #  pat_event_opts_select <- pat_event_opts_select[which(!pat_event_opts_select %in% sankey_events)]
        #}
        #print(pat_event_opts_select)
        shinyWidgets::updateVirtualSelect(session = session,inputId = "SwimmerYlinesSelect",choices = pat_event_opts, selected = pat_event_opts_select)
      })
      
      #PatientTimelinePlot_react <- reactive({
      #  if (!is.null(input$PatientSelectionTab_rows_selected)) {
      #    Patient_Row_Selec <- input$PatientSelectionTab_rows_selected
      #    Patient_Table_df <- Patient_Table_React()
      #    Patient <- Patient_Table_df[Patient_Row_Selec,1]
      #    colorPal <- colPalSelected()
      #    if (colorPal == "Standard colors") {
      #      colorPal <- NULL 
      #    }
      #    svgW <- input$TimeLineWidth
      #    svgH <- input$TimeLineHeight
      #    #Project_Name <- ProjectName_react()
      #    if (Patient %in% Patient_Table_df[,1]) {
      #      RemoveUnkNA_opt <- TRUE
      #      SwimmerTitle_in <- ifelse(!isTruthy(input$TimeLineTitle),paste0("Clinical Course of Patient: ", as.character(Patient)),input$TimeLineTitle)
      #      SwimmerTheme <- input$TimeLineTheme
      #      TitleFont <- input$TimeLineTitleSize
      #      xFont <- input$TimeLineXAxisSize
      #      yFont <- input$TimeLineYAxisSize
      #      eventsY <- input$SwimmerYlinesSelect
      #      AppTimeUnit <- input$PatTimelineXunit
      #      Patient_Event_Data_sub <- PatientTimelinePlot_df()
      #      eventsY_summ <- grep("Summary",eventsY,value = T)
      #      eventsY_reg <- grep("Summary",eventsY,value = T, invert = T)
      #      eventsY <- c(eventsY_summ,eventsY_reg)
      #      if (isTruthy(eventsY)) {
      #        Patient_Event_Data_sub <- Patient_Event_Data_sub[which(Patient_Event_Data_sub[,2] %in% eventsY),]
      #        Patient_Event_Data_sub <- Patient_Event_Data_sub[order(match(Patient_Event_Data_sub[,2], eventsY)),]
      #      }
      #      Patient_Event_Data_sub$EventStart <- round(Patient_Event_Data_sub$EventStart,2)
      #      Patient_Event_Data_sub$EventEnd <- round(Patient_Event_Data_sub$EventEnd,2)
      #      if (length(eventsY) == length(unique(Patient_Event_Data_sub$Event))) {
      #        #save(list = ls(), file = "PatientTimelinePlot_react.RData", envir = environment())
      #        plot2 <- timelinePlot(data = Patient_Event_Data_sub[,-1],event_col = "Event", event_type_col = "EventType",
      #                              start_col = "EventStart", stop_col = "EventEnd",unit = AppTimeUnit, plotly = TRUE,
      #                              title_font = TitleFont, x_font = xFont, y_font = yFont,na.rm = RemoveUnkNA_opt,
      #                              title = SwimmerTitle_in,svg_name = paste0("Moffitt_RTD_Patient_",Patient,"_Timeline"),
      #                              svg_height = svgH, svg_width = svgW, highlight_col = "highlight", col_pal = colorPal)
      #        plot2
      #      }
      #      
      #    }
      #  }
      #})
      ### Render Patient timeline plot
      #output$PatientTimelinePlot <- renderPlotly({
      #  p <- PatientTimelinePlot_react()
      #  p
      #})
      
      ## Downloads
      output$dnldCohortEventTab <- downloadHandler(
        filename = function() {
          paste0("Moffitt_RTD_Timeline_Event_Data.txt")
        },
        content = function(file) {
          req(cohort_EventSumm_df())
          Patient_Event_Data <- cohort_EventSumm_df()
          Patient_Event_Data <- Patient_Event_Data[,-c(9:11)]
          write.table(Patient_Event_Data,file, sep = '\t', row.names = F)
        }
      )
      output$dnldPatientTimelinePlot <- downloadHandler(
        filename = function() {
          Patient_Row_Selec <- input$PatientSelectionTab_rows_selected
          Patient_Table_df <- Patient_Table_React()
          Patient <- Patient_Table_df[Patient_Row_Selec,1]
          paste0("Moffitt_RTD_Patient_",Patient,"_Timeline.svg")
        },
        content = function(file) {
          p <- PatientTimelinePlot_react()
          ggsave(file,p,width = input$TimeLineWidth, height = input$TimeLineHeight, units = input$TimeLineUnits)
        }
      )
      #output$dnldPatientEventTab <- downloadHandler(
      #  filename = function() {
      #    Patient_Row_Selec <- input$PatientSelectionTab_rows_selected
      #    Patient_Table_df <- Patient_Table_React()
      #    Patient <- Patient_Table_df[Patient_Row_Selec,1]
      #    paste0("Moffitt_RTD_",Patient,"_Timeline_Event_Data.txt")
      #  },
      #  content = function(file) {
      #    df <- PatientTimelinePlot_df()
      #    df <- apply(df,2,function(x) gsub("^,","",gsub("\n",",",x)))
      #    write.table(df,file, sep = '\t', row.names = F)
      #  }
      #)
      
      # New Tab -------------------------------
      
      #PatientTimelineSummPlot_df_full <- reactive({
      #  req(patient_event_data_single())
      #  if (!is.null(input$PatientSelectionTab_rows_selected)) {
      #    pat_event_data <- patient_event_data_single()
      #    #sankey_added_events <- sankey_added_events()
      #    #pat_event_data <- pat_event_data[which(!pat_event_data$Event %in% sankey_added_events),]
      #    pat_event_data
      #  }
      #})
      
      PatientTimelineSummPlotDF_react_full <- reactive({
        req(patient_id_react())
        req(patient_selected_data())
        req(patient_event_data_single())
        req(input$SwimmerYlinesSelect)
        colorPal <- PatTimelineSummColorPal_sel()
        if (colorPal == "Standard colors") {
          colorPal <- NULL 
        }
        Patient <- patient_id_react()
        Patient_Event_Data_sub <- patient_event_data_single()
        if (patient_selected_data()$patient == Patient) {
          RemoveUnkNA_opt <- TRUE
          SwimmerTitle_in <- ifelse(!isTruthy(input$TimeLineTitle),paste0("Clinical Course of Patient: ", as.character(Patient)),input$TimeLineTitle)
          SwimmerTheme <- input$TimeLineTheme
          TitleFont <- input$TimeLineTitleSize
          xFont <- input$TimeLineXAxisSize
          yFont <- input$TimeLineYAxisSize
          eventsY <- input$SwimmerYlinesSelect
          #print(paste("eventsY:",eventsY))
          TimeLineHeight <- input$TimeLineHeight
          TimeLineWidth <- input$TimeLineWidth
          AppTimeUnit <- input$PatTimelineXunit
          
          #if (isTruthy(eventsY)) {
            #print("1551")
            Patient_Event_Data_sub <- Patient_Event_Data_sub[which(Patient_Event_Data_sub[,2] %in% eventsY),]
            Patient_Event_Data_sub <- Patient_Event_Data_sub[order(match(Patient_Event_Data_sub[,2], eventsY)),]
          #}
          Patient_Event_Data_sub$EventStart <- round(Patient_Event_Data_sub$EventStart,2)
          Patient_Event_Data_sub$EventEnd <- round(Patient_Event_Data_sub$EventEnd,2)
          Patient_Event_Data_sub
        }
        
      })
      
      PatientTimelineSummPlot_react_full <- reactive({
        req(PatientTimelineSummPlotDF_react_full())
        req(patient_id_react())
        Patient <- patient_id_react()
        Patient_Event_Data_sub <- PatientTimelineSummPlotDF_react_full()
        #if (!is.null(input$PatientSelectionTab_rows_selected)) {
        #  Patient_Row_Selec <- input$PatientSelectionTab_rows_selected
        #  Patient_Table_df <- Patient_Table_React()
        #  Patient <- Patient_Table_df[Patient_Row_Selec,1]
          colorPal <- PatTimelineSummColorPal_sel()
          if (colorPal == "Standard colors") {
            colorPal <- NULL 
          }
        #  if (Patient %in% Patient_Table_df[,1]) {
            RemoveUnkNA_opt <- TRUE
            SwimmerTitle_in <- ifelse(!isTruthy(input$TimeLineTitle),paste0("Clinical Course of Patient: ", as.character(Patient)),input$TimeLineTitle)
            SwimmerTheme <- input$TimeLineTheme
            TitleFont <- input$TimeLineTitleSize
            xFont <- input$TimeLineXAxisSize
            yFont <- input$TimeLineYAxisSize
            eventsY <- input$SwimmerYlinesSelect
            #print(paste("eventsY:",eventsY))
            TimeLineHeight <- input$TimeLineHeight
            TimeLineWidth <- input$TimeLineWidth
            AppTimeUnit <- input$PatTimelineXunit
            #Patient_Event_Data_sub <- PatientTimelineSummPlot_df_full()
            #Patient_Event_Data_sub <- patient_event_data_single()
            #save(list = ls(), file = "PatientTimelineSummPlot_react1.RData", envir = environment())
            #eventsY_summ <- grep("Summary",eventsY,value = T)
            #eventsY_reg <- grep("Summary",eventsY,value = T, invert = T)
            #eventsY <- c(eventsY_summ,eventsY_reg)
            ##print(head(Patient_Event_Data_sub[,1:5]))
            #print(paste("eventsY:",eventsY))
            #if (isTruthy(eventsY)) {
            #  #print("1551")
            #  Patient_Event_Data_sub <- Patient_Event_Data_sub[which(Patient_Event_Data_sub[,2] %in% eventsY),]
            #  Patient_Event_Data_sub <- Patient_Event_Data_sub[order(match(Patient_Event_Data_sub[,2], eventsY)),]
            #}
            #Patient_Event_Data_sub$EventStart <- round(Patient_Event_Data_sub$EventStart,2)
            #Patient_Event_Data_sub$EventEnd <- round(Patient_Event_Data_sub$EventEnd,2)
            #save(list = ls(), file = "PatientTimelineSummPlot_react2.RData", envir = environment())
            #print(head(Patient_Event_Data_sub[,1:5]))
            plot2 <- timelinePlot(data = Patient_Event_Data_sub[,-1],event_col = "Event", event_type_col = "EventType",
                                  start_col = "EventStart", stop_col = "EventEnd", unit = AppTimeUnit, plotly = TRUE,
                                  title_font = TitleFont, x_font = xFont, y_font = yFont,na.rm = RemoveUnkNA_opt,
                                  title = SwimmerTitle_in,svg_name = paste0("Moffitt_RTD_Patient_",Patient,"_Timeline"),
                                  svg_height = TimeLineHeight, svg_width = TimeLineWidth, highlight_col = "highlight", col_pal = colorPal)
            plot2
        #  }
        #}
      })
      
      
      
      output$PatientTimelineLineSummPlot_full <- renderPlotly({
        req(patient_id_react())
        Patient <- patient_id_react()
        plot_title <- paste0("Clinical Course of Patient: ", as.character(Patient))
        #if (!is.null(input$PatientSelectionTab_rows_selected)) {
        #  Patient_Row_Selec <- input$PatientSelectionTab_rows_selected
        #  Patient_Table_df <- Patient_Table_React()
        #  Patient <- Patient_Table_df[Patient_Row_Selec,1]
        #  plot_title <- paste0("Clinical Course of Patient: ", as.character(Patient))
        #}
        p1 <- PatientTimelineSummPlot_react_full()
        p_out <- p1
        line_p <- PatientLinePlot_react_full()
        AppTimeUnits <- tolower(GlobalAppTimeUnit_react())
        xAxis <- "EventStart"
        xAxisUnits <- AppTimeUnits
        #save(list = ls(), file = "PatientTimelineLineSummPlot_full.RData", envir = environment())
        req(p1)
        #req(line_p)
        if (isTruthy(line_p)) {
          p1_xlim <- p1$x$layout$xaxis$range
          if (nrow(line_p$data[xAxis]) > 0) {
            p2_xlim <- range(line_p$data[xAxis], na.rm = T)
            line_p <- line_p +
              ggplot2::scale_x_continuous(limits=c(min(c(p1_xlim,p2_xlim), na.rm = T), max(c(p1_xlim,p2_xlim), na.rm = T)))
            p2 <- plotly::ggplotly(line_p)
            p_out <- subplot(p1, p2, nrows = 2, shareY = TRUE, shareX = TRUE,
                             titleY = TRUE, titleX = TRUE, which_layout = 1)
            p_out <- p_out %>%
              layout(margin = list(t = 50))
          }
          p_out
        } else {
          p_out
        }
        
      })
      
      
      PatientLinePlot_df_full <- reactive({
        req(input$PatientSelectionTab_rows_selected)
        lineP_df <- event_data()
        Patient_Row_Selec <- input$PatientSelectionTab_rows_selected
        Patient_Table_df <- Patient_Table_React()
        Patient <- Patient_Table_df[Patient_Row_Selec,1]
        #save(list = ls(), file = "PatientLinePlot_df.RData", envir = environment())
        plot_df_2 <- lineP_df[which(lineP_df$Name == Patient & lineP_df$EventType == "Imaging"),]
        plot_df_2[,"Lesion Site"] <- gsub("^Imaging: ","",plot_df_2$Event)
        plot_df_2
      })
      
      
      PatientLinePlot_react_full <- reactive({
        req(PatientLinePlot_df_full())
        plot_df <- PatientLinePlot_df_full()
        AppTimeUnits <- tolower(GlobalAppTimeUnit_react())
        yAxis <- "Lesion Size"
        yaxlab <- "Lesion Size (cm)"
        xAxis <- "EventStart"
        xAxisUnits <- AppTimeUnits
        userCutP <- input$linePlotCutP
        Xaxis_font <- input$LinePlotXAxisSize
        Yaxis_font <- input$LinePlotYAxisSize
        title_font <- input$LinePlotTitleSize
        unitCol <- input$LinePunitCol
        unitSel <- input$LinePunitSelect
        LinePlotTheme <- input$LinePlotTheme
        #save(list = ls(), file = "PatientLinePlot_react.RData", envir = environment())
        plotTitle <- paste0("Lesion History of patient ",unique(plot_df[,1])," (cm)")
        plot_df[,yAxis] <- as.numeric(plot_df[,yAxis])
        plot_df[,xAxis] <- as.numeric(plot_df[,xAxis])
        
        #plot_df <- plot_df[complete.cases(plot_df),]
        plot_df <- plot_df[which(!is.na(plot_df[,xAxis]) & !is.na(plot_df[,yAxis])),]
        if (isTruthy(plot_df) & nrow(plot_df) > 0) {
          #plot_df <- PatientLinePlot_df()
          p2 <- ggplot(data=plot_df, aes(x=EventStart, y=`Lesion Size`, colour=`Lesion Site`)) +
            geom_line() +
            geom_point(size = 3) +
            theme_minimal() +
            theme(axis.title.x = element_text(size = 14, margin = margin(30,0,0,0)),
                  axis.text.x = element_text(size = 14, angle = 45, vjust = 0.5, hjust=1),
                  axis.text.y = element_text(size = 14),
                  axis.title.y = element_text(size = 14),
                  plot.title = element_text(size = 20, margin = margin(0,0,30,0))) +
            ylab(paste0("Lesion Size"))
          p2
        }
        
        
        
      })
      
      output$patient_event_table <- DT::renderDataTable({
        df <- patient_event_data_single()
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
        df <- PatientLinePlot_df_full()
        #df_sub <- df[,c("Name","Event Name","Event Start","Lesion Site","Lesion Size","Lesion Size Unit","Lesion Target/Non-Target")]
        df_sub <- df %>%
          select(any_of(c("Name","Event Name","Event Start","Lesion Site","Lesion Size","Lesion Size Unit","Lesion Target/Non-Target"))) %>%
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
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
}

# Run the application
shinyApp(ui = ui, server = server)



