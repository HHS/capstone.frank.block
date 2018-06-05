#===============================================================================
# Current version: app2.6.3_4vs4_new_word_historical_trend_sarasota 
# 
# Previous version: app2.6.2_4vs4_new_word_historical_trend
#
# Note: This app contains 4 individual xbgoost models and can make predictions on 4 device types.
# 
# New features:
#   
# Added a new tab Find Error Codes to identify error codes appeared in the uploaded doc.
# Added functionality to visualize historical trend using multiple keywords
# Added functionality to visualize historical trend with different time frequency
# Added functionality to find top longest logs
# Added AND OR NOT opereations in historical keyword search
#
# 2017-01-30
# Shufang Ci
#===============================================================================

## Load required packages
packages_to_use = c("shinyBS", "shinydashboard", "RColorBrewer","wordcloud",
                    "shiny","magrittr","stringi","stringr","tm","SnowballC",
                    "ggplot2","wordcloud","data.table","plotly","DT",
                    "lubridate","dendextend", "shinythemes","plotly", 
                    "reshape2", "tidyr", "tidytext", "plyr", "dplyr", 
                    "ggrepel", "networkD3", "data.tree", "quanteda", 
                    "topicmodels", "fpc", "cluster", "Rtsne", "text2vec", 
                    "stats", "stringi", "LDAvis", "scales", "grid", "zoo", 
                    "rpart", "rpart.plot", "textstem", "RPostgreSQL", "xgboost")

install_load = function(packages){
    to_install <- packages[!(packages %in% installed.packages()[, "Package"])]
    if (length(to_install)){
        install.packages(to_install, repos='http://cran.us.r-project.org', dependencies = TRUE)
    }
    lapply(packages, library, character.only = TRUE)
}
install_load(packages_to_use)
###------------------------------------------------------------------------------

ui <- dashboardPage(skin = "blue", 
                    dashboardHeader(title = "Computerized Maintenance Management System: Signal Detection Prototype", titleWidth = 1100),
                    dashboardSidebar(disable = TRUE),
                    body <- dashboardBody(
                        navbarPage("Menu",
                                   tabPanel("About", 
                                            box("This tool helps to detect safety signals from a Hospital’s
                                                Clinical Engineering Department’s Computerized Maintenance
                                                Management System (CMMS),which tracks all work performed on medical equipment.",
                                                tags$div(
                                                    tags$ul(
                                                        tags$li("Categorizes problems according to selected keyword and device type", 
                                                                style="font-size:100%"),
                                                        tags$li("Generates useful visualizations and textual output for analysis", 
                                                                style="font-size:100%"),
                                                        tags$li("Shows data trending over time", style="font-size:100%")
                                                    )
                                                ), width = 16)
                                   ),
                                   
                                   tabPanel("Upload Data",
                                            box("Upload CMMS data. Then select the appropriate filters 
                                                for Hospitals, device types, manufacturers and models 
                                                that you want to see analyzed in the other tabs. 
                                                The filters selected here will be applied to all the 
                                                graphs on the dashboard.", 
                                                br(), 
                                                br(),
                                                fileInput('file1', em('Upload CMMS data in .csv format', 
                                                                      style="text-align:center;color:blue;font-size:100%"),
                                                          multiple = TRUE, 
                                                          accept = c('.csv')
                                                ),
                                                fluidRow(
                                                    width = 3,
                                                    valueBoxOutput("entry_total", width = 3),
                                                    valueBoxOutput("entry_invalid", width = 3),
                                                    valueBoxOutput("hospital", width = 3),
                                                    valueBoxOutput("device", width = 3)
                                                ),
                                                fluidRow(
                                                    width = 3,
                                                    box("Select Hospitals", width=3,
                                                        uiOutput("hospital_checklist")),
                                                    box("Select Device Types", width=3,
                                                        uiOutput("device_checklist")),
                                                    box("Select Manufacturers", width=3,
                                                        uiOutput("manufacturer_checklist")),
                                                    box("Select Model Types", width=3,
                                                        uiOutput("model_checklist"))
                                                ),
                                                box(
                                                    "Sample Data",
                                                    dataTableOutput("contents"),
                                                    width = 16
                                                ),
                                                width = 16)
                                   ),
                                   
                                   navbarMenu("Exploratory Analysis",
                                              tabPanel("Overview", 
                                                       box("The graphs below provide a high level view of the data uploaded. 
                                                           The first graph is a classification of all the hospitals, 
                                                           device types, manufacturers and models used in the dataset. 
                                                           The second graph provides a breakdown of all the device 
                                                           types according to the filter selected by the user.", width = 16),
                                                       box("Classification Tree",
                                                           fluidRow(
                                                               diagonalNetworkOutput("data_tree", height = "2500px")
                                                               
                                                           ),
                                                           width = 16
                                                       ),
                                                       box("Classification by Device Type",
                                                           fluidRow(
                                                               column(
                                                                   plotlyOutput("stacked"),
                                                                   width = 12
                                                               )
                                                           ),
                                                           fluidRow(
                                                               column(
                                                                   selectInput("stacked_choice", "Select Filter",
                                                                               choices = c("Hospital" = "cli_name",
                                                                                           "Manufacturer" = "manufacturer",
                                                                                           "Model Type" = "model",
                                                                                           "Type Code" = "wo_type_code")
                                                                   ),
                                                                   width = 3
                                                               )
                                                           ),
                                                           width = 16
                                                       )
                                                       ),
                                              
                                              tabPanel("Trend Analysis", 
                                                       box("This tab contains graphs detailing the trend analysis 
                                                           of uploaded data over time. The first graph provides 
                                                           the number of reports generated over time categorized by
                                                           a user selected filter. The second graph further drills 
                                                           down into the trend by number of reports generated with 
                                                           a particular keyword", width = 16),
                                                       fluidRow(width=6,
                                                                box(
                                                                    selectInput("period_choice", "Select Reporting Frequency",
                                                                                choices = c("Daily" = "wo_opened_date",
                                                                                            "Monthly" = "wo_opened_mon",
                                                                                            "Quarterly" = "wo_opened_qtr")
                                                                    )
                                                                ),
                                                                box(
                                                                    selectInput("trend_choice", "Select Filter",
                                                                                choices = c("Hospital" = "cli_name",
                                                                                            "Device Type" = "device_type",
                                                                                            "Manufacturer" = "manufacturer",
                                                                                            "Model Type" = "model",
                                                                                            "Type Code" = "wo_type_code")
                                                                    )
                                                                )
                                                       ),
                                                       fluidRow(
                                                           box("Trend Analysis",
                                                               width = 12,
                                                               column(
                                                                   width = 12,
                                                                   fluidRow(
                                                                       plotlyOutput("trend_device")
                                                                   )
                                                               )
                                                           )
                                                       ),
                                                       fluidRow(
                                                           box(
                                                               width = 12,
                                                               column(
                                                                   width = 4,
                                                                   uiOutput("keyword_choice")
                                                               )
                                                           )
                                                       ),
                                                       fluidRow(
                                                           box("Trend Analysis by selected keyword",
                                                               width = 12,
                                                               plotlyOutput("trend_device_keyword") 
                                                           )
                                                       )
                                                       ),
                                              
                                              tabPanel("Frequent Terms", 
                                                       box("Provide frequent terms overview. List of text cleaning methods done:",
                                                           tags$div(
                                                               tags$ul(
                                                                   tags$li("Removes Punctuations like period, comma etc", 
                                                                           style="font-size:100%"),
                                                                   tags$li("Stems words: reduces a word down to its root, 
                                                                           which helps standardize different versions of the same word
                                                                           like removes to remove, removing to remove etc", 
                                                                           style="font-size:100%"),
                                                                   tags$li("Removes common stopwords in english like and, they, etc. 
                                                                           The complete list can be found below", 
                                                                           style="font-size:100%"), 
                                                                   helpText(
                                                                       a(
                                                                           "Full list of stopwords used", 
                                                                           href = "http://jmlr.csail.mit.edu/papers/volume5/lewis04a/a11-smart-stop-list/english.stop", 
                                                                           target = "_blank"
                                                                       )
                                                                   )
                                                               )
                                                                   ), width = 16),
                                                       box("Frequent Terms", 
                                                           width = 16,
                                                           fluidRow(  ## SLIDER ADJUST
                                                               column( #FEB MADE THESE CHANGES TO MIN AND MAX FROM 200 and 100000 respectiv!!!! this changes the SLIDER
                                                                   sliderInput("select_frequency", "Select minimum frequency", 
                                                                               min = 3, max = 100, value = 10000, step = 1, ticks = TRUE),
                                                                   width = 4
                                                               ),
                                                               column(
                                                                   width = 4,
                                                                   selectInput("term_choice", "Select Filter",
                                                                               choices = c("Hospital" = "cli_name",
                                                                                           "Device Type" = "device_type",
                                                                                           "Manufacturer" = "manufacturer",
                                                                                           "Model Type" = "model")
                                                                   )
                                                               ),
                                                               column(
                                                                   width = 4,
                                                                   textInput("add_stopword", "Enter additional stopwords; 
                                                                             each separated by a comma. Example: add, remove, stop", 
                                                                             value = ""),
                                                                   actionButton("update", "Update Stopwords")
                                                               )
                                                               ),
                                                           fluidRow(
                                                               column(width = 12, plotOutput("frequent_term"))
                                                           )
                                                       ),
                                                       box("Terms closest in usage",
                                                           width = 16,
                                                           fluidRow(
                                                               column(
                                                                   width = 4,
                                                                   uiOutput("keyword_ref")
                                                               )
                                                           ),
                                                           fluidRow(
                                                               column(width = 10, plotOutput("glove_term"))
                                                           ))
                                                       ),
                                              tabPanel("Find New Words",
                                                       box("This tab displays new words appeared in the uploaded file.", 
                                                           width = 16),
                                                       box("New Word Frequency Plot",
                                                           width=16,
                                                           fluidRow(
                                                               column(
                                                                   width = 6,
                                                                   selectInput("freq_type_choice", "Select Frequency type",
                                                                               choices = c("Unique" = "frequency_unique",
                                                                                           "Total" = "frequency_total")
                                                                   ),
                                                                   plotOutput("new_word_frequency")
                                                               )
                                                           ),
                                                           br()
                                                       ),
                                                       box("New Word Summary",
                                                           width=16,
                                                           fluidRow(
                                                               column(width=6, dataTableOutput("new_word_summary"))
                                                           )
                                                       ),
                                                       box("New Word Associated Logs",
                                                           width = 16,
                                                           fluidRow(
                                                               column(width=12, dataTableOutput("new_word_associated_log"))
                                                           )
                                                           
                                                       )
                                              )
                                   ),
                                   navbarMenu("Dangerous Signal Detection",
                                              tabPanel("Find Dangerous Signals",
                                                       box("The potential dangerous CMMS logs are detected by a 
                                                           decision tree algorithm.Based on the words that appeared 
                                                           in the work order problem/repair descriptions,
                                                           the algorithm automatically splits the logs into 
                                                           branch-like segments.Some important words are selected 
                                                           as rules to characterize the relationships between the 
                                                           input logs and the outcomes. Once the decision rules are 
                                                           determined, they are used to predict the outcomes of 
                                                           unseen observations. ", width = 16),
                                                       fluidRow(
                                                           box("Dangerous Signal Summary",
                                                               width = 12,
                                                               dataTableOutput("summary_dt"),
                                                               downloadButton("downloadData_all", "Download All Signals")
                                                           )
                                                       ),
                                                       fluidRow(
                                                           box("Abnormal Infusion Pump Signals",
                                                               width = 12,
                                                               dataTableOutput("dt_table_pump"),
                                                               downloadButton("downloadData_pump", "Download")
                                                           )
                                                       ),
                                                       fluidRow(
                                                           box("Abnormal Anesthesia System Signals",
                                                               width = 12,
                                                               dataTableOutput("dt_table_anes"),
                                                               downloadButton("downloadData_anes", "Download")
                                                           )
                                                       ),
                                                       fluidRow(
                                                           box("Abnormal Defibrillator Signals",
                                                               width = 12,
                                                               dataTableOutput("dt_table_defib"),
                                                               downloadButton("downloadData_defib", "Download")
                                                           )
                                                       ),
                                                       fluidRow(
                                                           box("Abnormal Ventilators Signals",
                                                               width = 12,
                                                               dataTableOutput("dt_table_vent"),
                                                               downloadButton("downloadData_vent", "Download")
                                                           )
                                                       ),
                                                       fluidRow(
                                                           box("Other Signals",
                                                               width = 12,
                                                               dataTableOutput("dt_table_other"),
                                                               downloadButton("downloadData_other", "Download")
                                                           )
                                                       )
                                                       ),
                                              tabPanel("Find Error Codes",
                                                       box("This tab displays logs with error codes appeared in the uploaded file.", 
                                                           width = 16),
                                                       fluidRow(
                                                           box("Error Code Associated Logs",
                                                               width = 12,
                                                               dataTableOutput("error_code_associated_log_table"),
                                                               downloadButton("downloadData_error_code", "Download")
                                                           )
                                                       )
                                              )
                                   ),
                                   tabPanel("Historical Trend",
                                            box("Visualize historical trends of keywords. Type in a keyword, and
                                                select a specific Date period. ", width = 16),
                                            box(width=16,
                                                fluidRow(
                                                    column(width=2,
                                                           textInput("historical_keyword_contain", 
                                                                     "Words contained (sep by commas)", value="super,o2")),
                                                    column(width=2,
                                                           selectInput("logic_operation", "Operation among contained keywords",
                                                                       choices = c("Or" = " OR ",
                                                                                   "And" = " AND "))),
                                                    column(width=2,
                                                           textInput("historical_keyword_not_contain", 
                                                                     "Words not contained (sep by commas)", value="pm")),
                                                    column(width=2,
                                                           selectInput("freq_choice", "Frequency",
                                                                       choices = c("Daily" = "wo_opened_date",
                                                                                   "Monthly" = "wo_opened_mon",
                                                                                   "Quarterly" = "wo_opened_qtr")
                                                           )
                                                    ),
                                                    column(width=2,
                                                           dateRangeInput("date_range", "Date Range:",
                                                                          start="2013-01-01",
                                                                          end  ="2017-12-31", 
                                                                          min  ="2000-01-01",
                                                                          max  ="2100-01-01",
                                                                          format="yyyy-mm-dd",
                                                                          separator=" - "),
                                                           tags$style(HTML(".datepicker {z-index:99999 !important;}"))),
                                                    column(width=2, br(), actionButton("dbbutton", "Retrieve Data")),
                                                    column("Frequency Visualization", width=10, br(),
                                                           plotlyOutput("dbdf_plot")),
                                                    column(width=12, br(), dataTableOutput("dbdf_table")),
                                                    column(width=12, plotlyOutput("historical_trend"))
                                                ))
                                            )
                                   )
                        )  
                    )



server = function(input, output, session) {

    #===========================================================================
    # Upload Data
    #===========================================================================
    
    # specify the size of the file that we can upload
    # If unset, the maximum request size defaults to 5MB.
    options(shiny.maxRequestSize=800*1024^2)   
    
    # uploaded file name and path
    file_data <- reactive({
        if (is.null(input$file1))
            return(NULL)
        (fread(input$file1$datapath, stringsAsFactors = F, header = T, 
               colClasses = c("character", "character", "character", "character", 
                              "character", "character", "character", "character", 
                              "character", "character")))
    })
    
    #modified data
    string0 <- reactive({
        if (is.null(input$file1))
            return(NULL)
        data <- as.data.table(file_data())
        names(data) <- c("cli_name", "device_type", "manufacturer", "model", 
                         "wo_opened_date", "wo_type_code", "wo_problem_code", 
                         "wo_problem_description", "wo_repair_description", "other1")
        wo_date <- as.Date(data$wo_opened_date, "%m/%d/%Y")
        #all the text fields into one data table
        string0 <- data[, .(tolower(cli_name), tolower(device_type), 
                            tolower(manufacturer), tolower(model), 
                            wo_date, as.Date(as.yearmon(wo_date)), as.Date(as.yearqtr(wo_date)), 
                            tolower(wo_type_code), tolower(wo_problem_code), 
                            tolower(wo_problem_description), tolower(wo_repair_description), 
                            tolower(other1) )]
        names(string0) <- c("cli_name", "device_type", "manufacturer", "model", 
                            "wo_opened_date", "wo_opened_mon", "wo_opened_qtr", 
                            "wo_type_code", "wo_problem_code", "wo_problem_description", 
                            "wo_repair_description", "other1")
        string0
    })
    
    # Annotate for sarasota 12/07/2017
    #     string1 <- reactive({
    #       if (is.null(string0()))
    #         return(NULL)
    #       string1 <- string0()[!is.na(string0()$wo_opened_date), ]
    #       string1
    #     })
    
    # Instead using this for sarasota 12/07/2017
    string1 <- reactive({
        if (is.null(string0()))
            return(NULL)
        string0()
    })
    
    
    # The currently selected tab from the first box
    output$tabset1Selected <- renderText({
        input$tabset1
    })
    
    
    #ValueBoxes
    output$entry_invalid <- renderValueBox({
        valueBox(
            value = dim(string0())[1] - dim(string1())[1],
            subtitle = "Number of Invalid Reports",
            width = 3,
            color = "aqua"
        )
    })  
    
    output$entry_total <- renderValueBox({
        valueBox(
            value = dim(string0())[1],
            subtitle = "Number of Total Reports",
            width = 3,
            color = "yellow"
        )
    })  
    
    output$hospital <- renderValueBox({
        valueBox(
            value = dim(string1()[ , .(unique(cli_name))])[1],
            subtitle = "Number of Hospitals",
            width = 3,
            color = "aqua"
        )
    })
    
    output$device <- renderValueBox({
        valueBox(
            value = dim(string1()[ , .(unique(device_type))])[1],
            subtitle = "Number of Device Types",
            width = 3,
            color = "aqua"
        )
    })
    
    
    #Select Hospitals Checklist
    output$hospital_checklist <- renderUI({
        if (is.null(string1())) {
            return()
        } else tagList(
            checkboxGroupInput(inputId = "selected_hospital",
                               label = "",
                               choices = string1()[, unique(cli_name)],
                               width = "100%",
                               selected = string1()[, unique(cli_name)]
            )
        )
    })
    
    #Filtered Hospital Dataset
    string_hos_filter <- reactive({
        if (is.null(string1())) 
            return(NULL)
        string_hos <- string1()[cli_name %in% input$selected_hospital, ]
        string_hos
    })
    
    #Select Device type Checklist
    output$device_checklist <- renderUI({
        if (is.null(string1())) {
            return()
        } else tagList(
            checkboxGroupInput(inputId = "selected_device",
                               label = "",
                               choices = string_hos_filter()[ , unique(device_type)],
                               width = "100%",
                               selected = string_hos_filter()[ , unique(device_type)]
            )
        )
    })
    
    #Filtered Hospital and Device Dataset
    string_hos_dev_filter <- reactive({
        if (is.null(string1())) 
            return(NULL)
        string_hos_dev <- string_hos_filter()[device_type %in% input$selected_device, ]
        string_hos_dev
    })
    
    #Select manufacturer Checklist: based on Hosp and Device
    output$manufacturer_checklist <- renderUI({
        if (is.null(string1())) {
            return()
        } else tagList(
            checkboxGroupInput(inputId = "selected_manufacturer",
                               label = "",
                               choices = string_hos_dev_filter()[ , unique(manufacturer)],
                               width = "100%",
                               selected = string_hos_dev_filter()[ , unique(manufacturer)]
            )
        )
    })
    
    #Filtered Hospital, Device, Manufacturer Dataset
    string_hos_dev_man_filter <- reactive({
        if (is.null(string1())) 
            return(NULL)
        string_hos_dev_man <- string_hos_dev_filter()[manufacturer %in% input$selected_manufacturer, ]
        string_hos_dev_man
    })
    
    #Select model types Checklist: based on Hosp, Device and Manufacturer
    output$model_checklist <- renderUI({
        if (is.null(string1())) {
            return()
        } else tagList(
            checkboxGroupInput(inputId = "selected_model",
                               label = "",
                               choices = string_hos_dev_man_filter()[, unique(model)],
                               width = "100%",
                               selected = string_hos_dev_man_filter()[, unique(model)]
            )
        )
    })
    
    #Filtered Hospital, Device, Manufacturer and Model Dataset
    string_filter <- reactive({
        if (is.null(string1())) 
            return(NULL)
        string_hos_dev_man_model <- string_hos_dev_man_filter()[model %in% input$selected_model, ]
        string_hos_dev_man_model
    })
    
    #output table in Upload Data tab
    output$contents <- renderDataTable({
        as.data.table(string_filter())
    },
    options = list(scrollX = TRUE))

    #===========================================================================
    # Exploratory Analysis: Overview
    #===========================================================================
    
    #data_tree plot
    output$data_tree <- renderDiagonalNetwork({
        withProgress(message = 'Please Wait:', value = 0, {
            incProgress(detail ='Preparing Overview ...')
            if (is.null(input$file1)) {
                return()
            } else{
                string2 <- as.data.table(string_filter())
                cols <- c("cli_name", "device_type", "manufacturer", "model")
                string2_group <- string2[, cols, by = cols]
                string2_group <- string2_group[, cols := NULL]
                string2_tree <- string2_group[, pathString := paste("Medical Devices", cli_name, 
                                                                    device_type, manufacturer, model, sep="|")]
                useRtree <- as.Node(string2_tree, pathDelimiter = "|")
                useRtreeList <- ToListExplicit(useRtree, unname=TRUE)
                height1 <- paste(10*nrow(string2_group)/3, "px", sep = "")
                diagonalNetwork(useRtreeList, fontSize = 10, opacity = 0.9, height = height1)
            }
        })
    })
    
    #stacked bar graphs
    output$stacked <- renderPlotly({
        if (is.null(string1())) 
            return()
        
        col_names <- c("device_type", input$stacked_choice)
        string_group <- string_filter()[, .N, by = col_names]
        g1 <- ggplotly(ggplot(data = string_group, 
                              aes_string(x = "device_type", y = "N", fill = input$stacked_choice)) + 
                           geom_bar(stat = "identity") + 
                           guides(fill = guide_legend(title = "Legend")) + 
                           theme(axis.text.x = element_text(angle=45, hjust=1, size=6), 
                                 axis.title = element_text(size = 8), 
                                 legend.text = element_text(size = 6)) + 
                           xlab("Device Type") + 
                           ylab("Number of Reports") + 
                           scale_y_continuous(labels = comma))
        ggplotly(tooltip = c("text"))
    })

    #===========================================================================
    # Exploratory Analysis: Trend Analysis
    #===========================================================================
    
    #user added stopwords
    user_stopword <- reactive({
        if (nchar(input$add_stopword) > 0) {
            a <- unlist(strsplit(input$add_stopword,","))
            a_trim <- sapply(a, function(x) {gsub("^\\s+|\\s+$", "", x)})
            a_trim
        } else {
            return(NULL)
        }
    })
    
    #Filtered stopwords data
    string_stopwords_filter_0 <- reactive({
        if (is.null(string1())) 
            return()
        
        string2 <- string_filter()[, wo_problem_description := paste(wo_problem_description, 
                                                                     wo_repair_description, other1, " ")]
        string2 <- string2[, ':=' (wo_repair_description = NULL, other1 = NULL)]
        string2
    })
    
    #stopwords filter data
    string_stopwords_filter05 <- reactive({
        if (is.null(string1())) 
            return()
        
        string3 <- string_stopwords_filter_0()[, wo_problem_description]
        input$update
        stopword1 <- isolate(input$add_stopword)
        user_stopwords <- if (nchar(isolate(stopword1)) > 0) {
            a <- unlist(strsplit(stopword1,","))
            a_trim <- sapply(a, function(x) {gsub("^\\s+|\\s+$", "", x)})
            a_trim
        } #FEB i am disabling the sparse string function here, and changing stem to false
        sparse_string <- dfm(string3, 
                             remove = c(stopwords("english"), user_stopwords), 
                             stem = FALSE, 
                             removePunct = TRUE, 
                             removeNumbers = TRUE, 
                             removeTwitter = TRUE)
        sparse_string
    })
    
    #intermediate data set
    string_stopwords_filter <- reactive({
        
        if (is.null(string1())) 
            return()
        
        sparse_table <- as.data.table(as.matrix(string_stopwords_filter05()))
        cols <- names(sparse_table)
        names_sums <- sparse_table[, lapply(.SD, sum), .SDcols = cols]   ##HERE IS WHERE THE FREQUENCY BIN IS SET
        ind <- (names_sums > 3) #FEB MADE THIS CHANGE FROM 200 TO MAKE THIS FIND THINGS WITH AS FEW AS 10 frequent terms!!!
        sparse_table_new <- sparse_table[, ind, with = FALSE]
        sparse_table_new
    })
    
    #stopwords filter data
    string_stopwords_filter1 <- reactive({
        if (is.null(string1())) 
            return()
        
        dfm_data <- cbind(string_stopwords_filter_0(), string_stopwords_filter())
        string4 <- melt(dfm_data, 
                        id.vars = c("cli_name", "device_type", "manufacturer", 
                                    "model", "wo_type_code", "wo_problem_code", 
                                    "wo_opened_date", "wo_opened_mon","wo_opened_qtr",
                                    "wo_problem_description"))
        string4 <- string4[value > 0, ]
        string4
    })
    
    #Select keywords for trend analysis
    output$keyword_choice <- renderUI({
        if (is.null(string1())) {
            return()
        } else tagList(
            selectInput(inputId = "selected_keyword",
                        label = "Select Keyword",
                        choices = unique(names(string_stopwords_filter())),
                        selected = "return"
            )
        )
    })
    
    #Trend analysis by device type
    output$trend_device <- renderPlotly({
        withProgress(message = 'Please Wait:', value = 0, {
            incProgress(detail ='Calculating trends ...')
            if (is.null(string1())) 
                return()
            
            col_names1 <- c(input$period_choice, input$trend_choice)
            #string_choice <- string2[, col_names1, with = FALSE]
            string4_group <- string_stopwords_filter1()[, .N, by = col_names1]
            g2 <- ggplot(string4_group, aes_string(x = input$period_choice, y = "N", color = input$trend_choice)) +
                geom_point() +
                scale_x_date(date_labels = "%b %y", date_breaks = "3 months") +
                theme(axis.text.x = element_text(angle = 45, hjust=1, size=6, lineheight = 1.5)) +
                labs(x = "", y = "Number of Reports") +
                guides(color = guide_legend(title = "Legend", 
                                            title.hjust = 0.5, 
                                            title.vjust = 0.1, 
                                            label.hjust = 0.8)) +
                theme(plot.margin = unit(c(0.2,1,1,0.5), "cm"))
            
            ggplotly(g2, tooltip = "text")
        })
    })
    
    #Trend analysis by device type and keyword
    output$trend_device_keyword <- renderPlotly({
        if (is.null(string1())) 
            return()
        
        string5 <- string_stopwords_filter1()[variable == input$selected_keyword, ]
        col_names5 <- c(input$period_choice, input$trend_choice)
        string5_group <- string5[, .N, by = col_names5]
        column_names <- names(string5_group)[2]
        
        g3 <- ggplot(string5_group, aes_string(x = input$period_choice, y = "N", color = column_names)) + 
            geom_point() + 
            scale_x_date(date_labels = "%b %y", date_breaks = "3 months") + 
            theme(axis.text.x = element_text(angle = 45, hjust=1, size=6, lineheight = 1.5)) +
            labs(x = "", y = "Number of Reports") + 
            guides(color = guide_legend(title = "Legend", 
                                        title.hjust = 0.5, 
                                        title.vjust = 0.1, 
                                        label.hjust = 0.8)) +
            theme(plot.margin = unit(c(0.2,1,1,0.5), "cm"))
        
        ggplotly(g3, tooltip = "text")
    })

    #===========================================================================
    # Exploratory Analysis: Frenquent Terms
    #===========================================================================
    
    #string term frequency filters data
    string_term_group_filter <- reactive({
        if (is.null(string1()))
            return()
        
        freq_cols <- c(input$term_choice, "variable")
        terms_group <- string_stopwords_filter1()[, sum(value), by = freq_cols]
        names(terms_group)[3] <- "value"
        names(terms_group)[1] <- "filter_term"
        terms_group
    })
    
    #frequent terms output
    output$frequent_term <- renderPlot({
        withProgress(message = 'Please Wait:', value = 0, {
            incProgress(detail ='Computing term frequencies ...')
            if (is.null(string1()))
                return()
            
            g <- ggplot(string_term_group_filter(), 
                        aes(x = filter_term, y = value, group = variable, color=value)) + 
                geom_point() + 
                geom_label_repel(data = subset(string_term_group_filter(), value > input$select_frequency), 
                                 aes(label=variable, fill=value), 
                                 fontface="bold", 
                                 color="white", 
                                 angle=45, 
                                 size=3, 
                                 box.padding = unit(0.35, "lines"), 
                                 point.padding = unit(0.3, "lines")) + 
                theme(axis.text.x = element_text(angle=45, hjust=1, size=8)) + 
                labs(y = "Frequency of Terms", title = "Top terms used", x = "") + guides(fill=FALSE)
            
            plot(g)
        })
    })
    
    #terms reference for trend analysis
    output$keyword_ref <- renderUI({
        if (is.null(string1()))
            return()
        else
            selectInput(inputId = "selected_ref",
                        label = "Keywords from Uploaded data",
                        choices = unique(names(string_stopwords_filter())),
                        selected = "return"
            )
    })

    #output for finding associations - named glove_term for now
    output$glove_term <- renderPlot({
        withProgress(message = 'Please Wait:', value = 0, {
            incProgress(detail ='Computing terms closest in usage...')
            if (is.null(string1())) {
                return()
            }
            
            assoc <- similarity(string_stopwords_filter05(), input$selected_ref, margin = "features")
            labels <- names(assoc[[1]])[1:20]
            y <- assoc[[1]][1:20]
            x <- c(1:20)
            
            attach(assoc)
            plot(x, y, xlab = "Terms", ylab = "Score", 
                 pch = 18, col = "red", main = "Top Terms closest in usage")
            text(x, y, labels, cex = 1, pos = 4, col = "blue") 
        })
    })
    
    #===========================================================================
    # Exploratory Analysis: Find New Words
    #===========================================================================

    new_word_table <- reactive({
        # Generate a new word table with 3 columns: new word, frequency in unique logs, frequency in all logs
        # This table is under Find New Words tab
        withProgress(message='Please wait:', value=0, {
            if (is.null(string1())) 
                return() 
            else {
                incProgress(detail='Computing term frequencies ...')
                word_bag <- readRDS("word_bag_4devices.rds")
                find_new <- function(new_string, old_word_bag){
                    new_string <- unlist(lapply(new_string, strsplit, " ")) 
                    new_string[!new_string %in% old_word_bag]
                }
                
                new_word_unique <- testing_raw_unique()
                new_word_unique <- data_proc_merge(new_word_unique)
                new_word_unique_table <- data.frame(table(unlist(lapply(new_word_unique, find_new, word_bag))))
                names(new_word_unique_table) <- c("new_words", "frequency_unique")
                
                new_word_with_duplicate <- testing_raw_with_duplicate()
                new_word_with_duplicate <- data_proc_merge(new_word_with_duplicate)
                new_word_with_duplicate_table <- data.frame(table(unlist(lapply(new_word_with_duplicate, find_new, word_bag))))
                names(new_word_with_duplicate_table) <- c("new_words", "frequency_total")
                
                new_word_table <- merge(x=new_word_unique_table, y=new_word_with_duplicate_table, by.x="new_words")
                new_word_table
            }
        })
        
    })
    
    output$new_word_summary <- renderDataTable({
        if (is.null(new_word_table())) 
            return() 
        new_word_table()
    },
    options = list(scrollX = TRUE))
    
    output$new_word_associated_log <- renderDataTable({
        if (is.null(string1())) 
            return()
        raw_log <- string1()
        raw_log$wo_opened_mon <- NULL
        raw_log$wo_opened_qtr <- NULL
        new_word_list <- as.character(new_word_table()$new_words)
        s = input$new_word_summary_rows_selected
        
        if (length(s) > 0){
            new_word_associated_log <- raw_log[grepl(paste(new_word_list[s], collapse="|"), data_proc_merge(raw_log)),]
            return(new_word_associated_log)
        }
        return()
    },
    options = list(scrollX = TRUE))
    
    output$new_word_frequency <- renderPlot({
        withProgress(message = 'Please Wait:', value = 0, {
            incProgress(detail ='Plotting ...')
            if (is.null(new_word_table()))
                return()
            g <- ggplot(new_word_table(), 
                        aes_string(x = reorder("new_words", input$freq_type_choice), y = input$freq_type_choice)) + 
                geom_point() + 
                geom_label_repel(data = subset(new_word_table()), 
                                 aes_string(label="new_words", fill=input$freq_type_choice), 
                                 fontface="bold", 
                                 color="white", 
                                 angle=45, 
                                 size=3, 
                                 box.padding = unit(0.35, "lines"), 
                                 point.padding = unit(0.3, "lines")) + 
                theme(axis.text.x = element_text(angle=45, hjust=1, size=8)) + 
                labs(x = "New Words", y = "Frequency of Terms", title = "Top terms used", x = "") + guides(fill=FALSE)
            
            plot(g)
        })
    })
    
    #===========================================================================
    # Dangerous Signal Detection: Find Dangerous Signals
    #===========================================================================
    
    ## define a procedure for text processing
    add_device_category <- function(dataframe){
        # Add a device category colomn to the data
        #
        # Args: 
        #   dataframe: A dataframe containing infusion pump, anesthesia systems, defibrillator, and ventilator
        # 
        # Return:
        #   A dataframe with colomn names added
        
        dataframe$device_category <- 0
        dataframe$device_type <- tolower(dataframe$device_type)
        dataframe$device_category[grepl("pump", dataframe$device_type)] = 1
        dataframe$device_category[grepl("esthe", dataframe$device_type)] = 2
        dataframe$device_category[grepl("defib", dataframe$device_type)] = 3
        dataframe$device_category[grepl("vent", dataframe$device_type)] = 4
        
        return(dataframe)
    }
    
    data_proc_merge <- function(dataframe=pump, to_lower=T, lemmatize=T, remove_punct=T, remove_digit=T){
        # Merge work order information and perform text preprocessing
        #
        # Args: 
        #   dataframe: A dataframe containing work order information
        # 
        # Return:
        #   A list of clean and merged text 
        
        df <- dataframe[, c("wo_type_code", "wo_problem_code", "wo_problem_description", "wo_repair_description")]
        df$text <- with(df, paste(wo_type_code, wo_problem_code, wo_problem_description, wo_repair_description))
        df <- data.frame(text = df$text)
        
        # remove words in parenthesis, lemmatize, remove punctuations and numbers from the text
        df$text <- gsub("\\s*\\([^\\)]+\\)","", df$text)
        if(to_lower) df$text <- tolower(df$text)
        if(lemmatize) df$text <- lemmatize_strings(df$text)
        if(remove_punct) df$text <- gsub("[[:punct:]]","", df$text)
        if(remove_digit) df$text <- gsub("[[:digit:]]","", df$text)
        #df$text <- gsub("[^[:alpha:]///' ]", "", df$text)
        
        # Replace synonyms
        ref <- read.csv("syn_reference.csv", header=T, stringsAsFactors = F)
        find_word <- function(x, ref, text) gsub(paste('\\<', ref[x,1], '\\>', sep=''), ref[x,2], text)
        for (i in 1:nrow(ref)) {df$text = find_word(i, ref, df$text)}
        
        return(df$text)
    }
    
    wordcount <- function(str) {
        # Count number of words from a list of logs
        #
        # Args:
        #   str: a list of text. e.g. c("abc def", "gh, i jkl")
        #
        # Return:
        #   A list of numbers. e.g. 2, 3
        
        sapply(gregexpr("\\b\\W+\\b", str, perl=TRUE), function(x) sum(x>0) ) + 1 
    }
    
    find_top_percent_elements <- function(lst, n=0.05){
        # Find the largest percent of numbers from a list of numbers
        #
        # Args:
        #   lst: a list of numbers
        #   n: percent. Default is 0.05 (5%)
        #
        # Return:
        #   A list of True, False (True means the number is among the top largest numbers)
        
        level <- quantile(lst, probs=1.0-n)
        top_elements <- lst > level[[1]]
        return(top_elements)
    }
    
    contain_kw <- function(text=data_proc_merge(), kw_file="keywords.csv"){
        # Find the index of logs containing user defined keywords
        #
        # Args: 
        #   text: A list of text. Default is the output of data_proc_merge()
        #   kw_file: keyword file directory
        # 
        # Return:
        #   A list of 0,1 (1 means at least 1 keyword can be found from the log)
        
        find_word <- function(x){grepl(paste('\\<', x, '\\>', sep=''), wordStem(unlist(text)))}
        kw <- read.csv(kw_file, header=F, stringsAsFactors = F)[,1]
        kw_num <- apply(sapply(kw, find_word), 1, sum)
        
        return(kw_num!=0)
    }
    
    data_proc_dfm <- function(text=data_proc_merge(), pred.col="", remove.rows=contain_kw()){
        # Process the text and generate document-feature matrix
        #
        # Args: 
        #   text: A list of text. Default is the output of data_proc_merge()
        #   pred.col: the column name of the outcome if there is. Default is no outcome column
        #   remove.rows: index. Text entries needs to be removed. 
        # 
        # Return:
        #   A dataframe (columns: features/words, rows: index of input text)
        
        text_corpus <- corpus(text[!remove.rows])
        text_dfm <- dfm(text_corpus, remove = stopwords("english"), stem = FALSE)
        text_dfm <- dfm_trim(text_dfm, min_count=1, min_docfreq=1)
        text_dfm_tfidf <- dfm_weight(text_dfm, type='tfidf')
        text_df <- convert(text_dfm_tfidf, to = "data.frame")
        if (pred.col == "") text_df$outcome <- NA else text_df$outcome <- dataframe[, c(pred.col)]
        
        return(text_df)
    }
    
    testing_raw_unique <- reactive({
        if (is.null(string1())){
            return()
        } else{
            testing_raw <- unique(string1())
            testing_raw$wo_opened_mon <- NULL
            testing_raw$wo_opened_qtr <- NULL
            testing_raw <- add_device_category(testing_raw)
            testing_raw
        }
    })
    
    testing_raw_with_duplicate <- reactive({
        if (is.null(string1())){
            return()
        } else{
            testing_raw <- string1()
            testing_raw$wo_opened_mon <- NULL
            testing_raw$wo_opened_qtr <- NULL
            testing_raw <- add_device_category(testing_raw)
            testing_raw
        }
    })
    
    danger_predict <- function(device_num=1, 
                               training_names_filedir="pump/training_names.csv", 
                               model_filedir="pump/xgboost_pump.rds",
                               filter_top_percent=0.05){
        # Detect the dangerous signals from text
        #
        # Args: 
        #   device_num: the index of device. Infusion pump: 1, Anesthesia sys: 2, Defib: 3, Ventilator: 4
        #   training_names_filedir: file directory that containing columns of the dfm matrix
        #   model_filedir: file directory of device specific models
        # 
        # Return:
        #   A dataframe: logs containing user defined keywords or positive by model prediction
        
        training_names <- read.csv(training_names_filedir)$x
        
        #reading testing dataset from file
        testing_device <- testing_raw_unique()[testing_raw_unique()$device_category==device_num,]
        
        if (nrow(testing_device)==0) return()
        testing_device$device_category <- NULL
        
        merged_testing_device = data_proc_merge(dataframe=testing_device)
        
        # find logs with predifined keywords
        kw_num = contain_kw(text=merged_testing_device)

        # find longest logs
        word_count = wordcount(merged_testing_device)
        data_long_idx = find_top_percent_elements(lst=word_count, n=filter_top_percent)
        
        long_or_kw_idx = kw_num | data_long_idx
        long_data_or_contain_kw = testing_device[long_or_kw_idx,]
        short_data_not_contain_kw = testing_device[!long_or_kw_idx,]
        
        if (nrow(short_data_not_contain_kw)==0) return(long_data_or_contain_kw)
        
        #testing data processing
        incProgress(detail = 'Processing uploaded data...')
        testing <- data_proc_dfm(data_proc_merge(dataframe=short_data_not_contain_kw), pred.col = "", remove.rows=long_or_kw_idx)
        testing_values <- data.frame(testing[,intersect(colnames(testing), training_names)])
        testing_names <- read.table(textConnection(""), col.names = training_names, colClasses = "integer")
        testing <- rbind.fill(testing_names, testing_values)
        testing[is.na(testing)] <- 0
        names(testing) <- gsub("[.]", "", names(testing)) 
        
        # prediction on the test data
        fit <- readRDS(model_filedir)
        test_pred <- predict(fit, as.matrix(testing), type = 'class')
        test_pred <- as.numeric(test_pred>0.4)
        
        # combine logs with keywords and dt predictions
        data.frame(rbind(long_data_or_contain_kw, short_data_not_contain_kw[test_pred!=0,]))
    }
    
    #--------------------------------------
    # PUMP: Show abnormal table and download
    danger_log_pump <- reactive({
        withProgress(message = 'Please wait:', value = 0, {
            if (is.null(string1())){
                return()
            } else{
                incProgress(detail = 'Loading model...')
                danger_predict(device_num=1, 
                               training_names_filedir="pump/training_names.csv", 
                               model_filedir="pump/xgboost_pump.rds")
            }
        })
    })
    
    output$dt_table_pump <- renderDataTable({
        if (is.null(danger_log_pump())) return() else danger_log_pump()
    },
    options = list(scrollX = TRUE))
    
    output$downloadData_pump <- downloadHandler(
        filename = function(){paste(gsub('.csv', '', input$file1$name), "_abnormal_pump_", Sys.Date(), ".csv", sep='')},
        content = function(file){
            write.csv(danger_log_pump(), file, row.names = F)
        }
    )
    
    #--------------------------------------
    # ANES: Show abnormal table and download
    danger_log_anes <- reactive({
        withProgress(message = 'Please wait:', value = 0, {
            if (is.null(string1())){
                return()
            } else{
                incProgress(detail = 'Loading model...')
                danger_predict(device_num=2, 
                               training_names_filedir="anes/training_names.csv", 
                               model_filedir="anes/xgboost_anes.rds")
            }
        })
    })
    
    output$dt_table_anes <- renderDataTable({
        if (is.null(danger_log_anes())) return() else danger_log_anes()
    },
    options = list(scrollX = TRUE))
    
    output$downloadData_anes <- downloadHandler(
        filename = function(){paste(gsub('.csv', '', input$file1$name), "_abnormal_anes_", Sys.Date(), ".csv", sep='')},
        content = function(file){
            write.csv(danger_log_anes(), file, row.names = F)
        }
    )
    #--------------------------------------
    
    # DEFIB: Show abnormal table and download
    danger_log_defib <- reactive({
        withProgress(message = 'Please wait:', value = 0, {
            if (is.null(string1())){
                return()
            } else{
                incProgress(detail = 'Loading model...')
                danger_predict(device_num=3, 
                               training_names_filedir="defib/training_names.csv", 
                               model_filedir="defib/xgboost_defib.rds")
            }
        })
    })
    
    output$dt_table_defib <- renderDataTable({
        if (is.null(danger_log_defib())) return() else danger_log_defib()
    },
    options = list(scrollX = TRUE))
    
    output$downloadData_defib <- downloadHandler(
        filename = function(){paste(gsub('.csv', '', input$file1$name), "_abnormal_defib_", Sys.Date(), ".csv", sep='')},
        content = function(file){
            write.csv(danger_log_defib(), file, row.names = F)
        }
    )
    #--------------------------------------
    
    # VENT: Show abnormal table and download
    danger_log_vent <- reactive({
        withProgress(message = 'Please wait:', value = 0, {
            if (is.null(string1())){
                return()
            } else{
                incProgress(detail = 'Loading model...')
                danger_predict(device_num=4, 
                               training_names_filedir="vent/training_names.csv", 
                               model_filedir="vent/xgboost_vent.rds")
            }
        })
    })
    
    output$dt_table_vent <- renderDataTable({
        if (is.null(danger_log_vent())) return() else danger_log_vent()
    },
    options = list(scrollX = TRUE))
    
    output$downloadData_vent <- downloadHandler(
        filename = function(){paste(gsub('.csv', '', input$file1$name), "_abnormal_vent_", Sys.Date(), ".csv", sep='')},
        content = function(file){
            write.csv(danger_log_vent(), file, row.names = F)
        }
    )
    #--------------------------------------
    
    # Other signals: Show abnormal table and download
    danger_log_other <- reactive({
        if (is.null(string1()))
            return()
        testing_device <- testing_raw_unique()[testing_raw_unique()$device_category==0,]
        if (nrow(testing_device)==0) return()
        testing_device$device_category <- NULL
    })
    
    output$dt_table_other <- renderDataTable({
        if (is.null(danger_log_other())) return() else danger_log_other()
    },
    options = list(scrollX = TRUE))
    
    output$downloadData_other <- downloadHandler(
        filename = function(){paste(gsub('.csv', '', input$file1$name), "_abnormal_other_", Sys.Date(), ".csv", sep='')},
        content = function(file){
            write.csv(danger_log_other(), file, row.names = F)
        }
    )
    #---------------------------------------------------------------------------
    
    # Summary table
    abnormal <- reactive({
        # Combine all signals from 4 devices. 
        # Generate data for Download All under Dangerous Signal Detection tab
        if (is.null(string1())) 
            return() 
        else 
            rbind(danger_log_pump(), danger_log_anes(), danger_log_defib(), danger_log_vent(), danger_log_other())
    })
    
    # Download all
    output$downloadData_all <- downloadHandler(
        filename = function(){paste(gsub('.csv', '', input$file1$name), "_abnormal_all_", Sys.Date(), ".csv", sep='')},
        content = function(file){
            write.csv(abnormal(), file, row.names = T)
        }
    )
    
    summary_table <- reactive({
        # A summary of dangerous signals under Dangerous Signal Detection tab
        # This table contains 4 columns: device, manufacturer, model, signal_keywords
        if (is.null(abnormal())) {
            return ()
        } else {
            ab <- abnormal()
            ab <- ab[,c("device_type", "manufacturer", "model", "wo_type_code", 
                        "wo_problem_code", "wo_problem_description", "wo_repair_description")]
            ab$device <- NA
            devices <- tolower(ab$device_type)
            ab$device[grepl("pump", devices)] = "Infusion Pump"
            ab$device[grepl("esthe", devices)] = "Anesthesia System"
            ab$device[grepl("defib", devices)] = "Defibrillator"
            ab$device[grepl("vent", devices)] = "Ventilator"
            ab$device_type <- NULL
            
            ab$manufacturer <- gsub("([A-Za-z]+).*", "\\1", ab$manufacturer)
            ab$text <- with(ab, paste(wo_type_code, wo_problem_code, wo_problem_description, wo_repair_description))
            ab$wo_type_code <- NULL
            ab$wo_problem_code <- NULL
            ab$wo_problem_description <- NULL
            ab$wo_repair_description <- NULL
            
            get_kw <- function(user_kw_file="keywords.csv", model_kw_file="pump/imp_feature.csv"){
                user_kw <- read.csv(user_kw_file, header=F, stringsAsFactors = F)[,1]
                model_kw <- read.csv(model_kw_file, header=F, stringsAsFactors = F)[,1]
                unique(c(user_kw, model_kw))
            }
            
            ab1 = aggregate(ab$text, 
                            list(device=ab$device, manufacturer=ab$manufacturer, model=ab$model), 
                            function(x) paste(x, collapse = " "))
            
            find_word <- function(text, keyword=kw){
                text <- gsub("\\s*\\([^\\)]+\\)","", text)
                text <- tolower(text)
                text <- lemmatize_strings(text)
                text <- gsub("[[:punct:]]","", text)
                text <- gsub("[[:digit:]]","", text)
                text <- gsub("[^[:alpha:]///' ]", "", text)
                text <- unique(strsplit(text, " "))[[1]]
                paste(intersect(keyword, text), collapse="; ")
            }
            
            # pump
            kw = get_kw(user_kw_file="keywords.csv", model_kw_file="pump/imp_feature.csv")
            ab1$x[ab1$device=="Infusion Pump"] = lapply(ab1$x[ab1$device=="Infusion Pump"], find_word)
            
            # anes
            kw = get_kw(user_kw_file="keywords.csv", model_kw_file="anes/imp_feature.csv")
            ab1$x[ab1$device=="Anesthesia System"] = lapply(ab1$x[ab1$device=="Anesthesia System"], find_word)
            
            # defib
            kw = get_kw(user_kw_file="keywords.csv", model_kw_file="defib/imp_feature.csv")
            ab1$x[ab1$device=="Defibrillator"] = lapply(ab1$x[ab1$device=="Defibrillator"], find_word)
            
            # vent
            kw = get_kw(user_kw_file="keywords.csv", model_kw_file="vent/imp_feature.csv")
            ab1$x[ab1$device=="Ventilator"] = lapply(ab1$x[ab1$device=="Ventilator"], find_word)
            
            names(ab1)[names(ab1) == 'x'] <- 'signal_keywords'
            data.frame(ab1[with(ab1, order(device)),])
        }
    })
    
    output$summary_dt <- renderDataTable({
        if (is.null(summary_table())) 
            return() 
        else 
            datatable(summary_table(), rownames=F)
    },
    options = list(scrollX = TRUE))
    
    #===========================================================================
    # Dangerous Signal Detection: Find Error Codes
    #===========================================================================

    error_code_associated_log <- reactive({
        withProgress(message='Please wait:', value=0, {
            if (is.null(string1())) 
                return() 
            else {
                incProgress(detail='Searching for error code ...')
                raw_log <- string1()
                raw_log$wo_opened_mon <- NULL
                raw_log$wo_opened_qtr <- NULL
                
                # search for logs with error pattern 1
                raw_log_merged = data_proc_merge(raw_log, to_lower=T, lemmatize=T, remove_punct=F, remove_digit=F)
                patterns_1 = c( 
                    '\\D+(\\d{2})[[:punct:]](\\d{2})[[:punct:]](\\d{3})\\D+', # <##.##.###>
                    '\\D+(\\d{3})[[:blank:]]*[[:punct:]][[:blank:]]*(\\d{4})\\D+', # <###.####>
                    '\\D+(\\d{4})[[:blank:]]*[[:punct:]][[:blank:]]*(\\d{4})\\D+' # <####.####>
                )
                matches_1 <- grep(paste(patterns_1,collapse="|"), raw_log_merged, value=F)
                
                # search for logs with error pattern 2
                raw_log_merged = data_proc_merge(raw_log, to_lower=T, lemmatize=T, remove_punct=T, remove_digit=F)
                patterns_2 = c( 
                    "error message",
                    "error code",
                    paste0("error", "[[:blank:]]*[[:digit:]]")
                )
                matches_2 <- grep(paste(patterns_2,collapse="|"), raw_log_merged, value=F)
                
                matches = sort(unique(c(matches_1, matches_2)))
                if (length(matches) > 0){
                    error_code_associated_log <- raw_log[matches,]
                    return(error_code_associated_log)
                }
                return()
            }
        })
    })
    
    # Generate error code table
    output$error_code_associated_log_table <- renderDataTable({
        if(is.null(error_code_associated_log()))
            return()
        error_code_associated_log()
    },
    options = list(scrollX = TRUE))
    
    # Download error code associated logs
    output$downloadData_error_code <- downloadHandler(
        filename = function(){paste(gsub('.csv', '', input$file1$name), "_error_code_", Sys.Date(), ".csv", sep='')},
        content = function(file){
            write.csv(error_code_associated_log(), file, row.names = T)
        }
    )

    #===========================================================================
    # Historical Trend
    #===========================================================================
    
    # Historical Trend tab
    retrievedf <- function(kw_contain, kw_not_contain, start_date, end_date){
        # Retrieve historical logs containing a specific keyword from local database (master file) 
        # 
        # Args: 
        #   kw: user defined keyword
        #   start_date: from when the log need to be retrieved
        #   end_date: until when the log need to be retrieved
        # 
        # Return:
        #   A dataframe: historical logs containing user defined keywords
        
        # Database connection. Can be changed according to new dbs later.
        con <- dbConnect(dbDriver("PostgreSQL"), dbname = "postgres", 
                         host = "localhost", port = 5432, user = "postgres", password = "1990")
        
        col_names <- paste("CONCAT_WS(' ', ",
                           "cli_name,", "device_type,", "manufacturer,", "model,", 
                           "wo_type_code,", "wo_problem_code,", "wo_problem_description,", 
                           "wo_repair_description,", "other1", ")")
        if (kw_not_contain==""){
            dbdf <- dbGetQuery(con, paste0("SELECT * FROM cmms_master WHERE (",
                                           col_names, " ILIKE '%",  
                                           gsub(",", paste0("%'", input$logic_operation , col_names, " ILIKE '%"), kw_contain), "%')",
                                           " AND wo_opened_date >= '", start_date, 
                                           "' AND wo_opened_date <= '", end_date, "'",
                                           " ORDER BY wo_opened_date DESC",
                                           sep=""))
            
        }
        else{
            dbdf <- dbGetQuery(con, paste0("SELECT * FROM cmms_master WHERE ((",
                                           col_names, " ILIKE '%",  
                                           gsub(",", paste0("%'", input$logic_operation , col_names, " ILIKE '%"), kw_contain), "%')",
                                           " AND ", col_names, " NOT ILIKE '%", 
                                           gsub(",", paste0("%'", " AND " , col_names, " NOT ILIKE '%"), kw_not_contain), "%')",
                                           " AND wo_opened_date >= '", start_date, 
                                           "' AND wo_opened_date <= '", end_date, "'",
                                           " ORDER BY wo_opened_date DESC",
                                           sep=""))
            
        }
        dbDisconnect(con)
        return(dbdf)
    }
    
    dbdf <- eventReactive(input$dbbutton, {
        dbdf <- retrievedf(kw_contain=input$historical_keyword_contain, 
                           kw_not_contain=input$historical_keyword_not_contain,
                           start_date=input$date_range[1], 
                           end_date=input$date_range[2])
        dbdf
    })
    
    # Tab: Historical Trend, Table: historical logs with keyword
    output$dbdf_table <- renderDataTable({
        if (is.null(dbdf()) || nrow(dbdf())==0)
            return()
        as.data.table(dbdf())
    },
    options = list(scrollX = TRUE))
    
    # Tab: Historical Trend, Plot: Frequency of historical logs with keyword
    output$dbdf_plot <- renderPlotly({
        if (is.null(dbdf()) || nrow(dbdf())==0)
            return()
        
        dates <- dbdf()$wo_opened_date
        dates <- as.data.frame(table(dates))
        names(dates) <- c("wo_opened_date", "Freq")
        dates$wo_opened_date <- as.Date(dates$wo_opened_date)
        dates$wo_opened_mon <- as.Date(as.yearmon(dates$wo_opened_date))
        dates$wo_opened_qtr <- as.Date(as.yearqtr(dates$wo_opened_date))
        
#         ggplotly(ggplot(data=dates, aes(x=Date, y=Freq))+
#                      geom_point()+
#                      scale_x_date(breaks=seq(min(dates$Date)-60, max(dates$Date)+60, 61),
#                                   #labels=date_format("%Y-%b"),
#                                   limits=c(as.Date(input$date_range[1]), as.Date(input$date_range[2])))+
#                      #scale_y_continuous(breaks=seq(0, max(dates$Freq) + 1, 1))+
#                      ylab("Frequency")+
#                      theme(axis.text.x=element_text(size=7, angle=45, hjust=0.5))+
#                      theme(plot.margin = unit(c(0,1,1,0.5), "cm")))
        
#         col_names1 <- input$freq_choice
#         print(col_names1)
#         dates_group <- dates[, .N, by = col_names1]
#         print(head(dates_group))
        graphics.off()
        g2 <- ggplot(dates, aes_string(x = input$freq_choice, y = "Freq")) +
            stat_summary(fun.y = sum, geom = "point")+
            scale_x_date(date_labels = "%b %y", date_breaks = "3 months") +
            theme(axis.text.x = element_text(angle = 45, hjust=1, size=6, lineheight = 1.5)) +
            labs(x = "", y = "Number of Reports") +
            guides(color = guide_legend(title = "Legend", 
                                        title.hjust = 0.5, 
                                        title.vjust = 0.1, 
                                        label.hjust = 0.8)) +
            theme(plot.margin = unit(c(0.2,1,1,0.5), "cm"))
        
        ggplotly(g2, tooltip = "text")
        
        
    })
}

shinyApp(ui, server)