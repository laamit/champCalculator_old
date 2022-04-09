#' use_table UI Function
#'
#' @description A shiny Module for the calculation of champ risk for multiple patients
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_use_table_ui <- function(id){
  ns <- NS(id)
  tagList(
      titlePanel(h1("CHAMP risk calculator", align = "center")),
      hr(),
      fluidRow(
        column(5,
               h3("Upload definitions and data"),
               fileInput(ns('definitions'), 'Variable definitions table',
                         multiple = FALSE,
                         accept = c(".xlsx")),
               
               fileInput(ns('data'), 'Data in excel or csv format',
                         multiple = FALSE,
                         accept = c('text/plain',
                                    '.txt',
                                    '.csv',
                                    ".xlsx",
                                    ".xls")),
        ),
        column(4, 
               h3("Calculate and download"),
               HTML(strrep(br(), 1)),
               actionButton(ns("calculate_risk"),
                            "Calculate the risk",
                            class = "btn btn-primary"),
               HTML(strrep(br(), 4)),
               downloadButton(ns("downloadData"), "Download data with risk"),
               HTML(strrep(br(), 4)),
               verbatimTextOutput(ns("warning_output")),
        ),
        column(3,
               h3("Options"),
               checkboxInput(ns("limit_values"), "Winsorize values to match original data", TRUE)
        ))
  )
}
    
#' use_table Server Functions
#'
#' @noRd 
mod_use_table_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    values <- reactiveValues(data_orig = NULL)
    
    data_input <- reactive({
      ## check extension and load accordingly
      if (is.null(input$data)) return(NULL)
      
      file_path <- input$data$datapath
      filename <- input$data$name
      
      if ( grepl("\\.xlsx$", filename) | grepl("\\.xls$", filename) ) {
        readxl::read_excel(file_path, guess_max = 100000)
      } else if (grepl("\\.csvx$", filename) | grepl("\\.csv$", filename)) {
        readr::read_csv(file_path, guess_max = 100000)
      } else {
        NULL
      }
    })
    
    data_definitions <- reactive({
      ## check extension and load accordingly
      if (is.null(input$definitions)) return(NULL)
      file_path <- input$definitions$datapath
      filename <- input$definitions$name
      
      if ( grepl("\\.xlsx$", filename) | grepl("\\.xls$", filename) ) {
        readxl::read_excel(file_path, guess_max = 100000)
      } else {
        NULL
      }
    })
    
    observeEvent(input$calculate_risk, {
      
      if (!is.null(data_input()) & !is.null(data_definitions())) {
        data_wrangled <- lapply(champCalculator::var_names$data, 
                                wrangle_variable,
                                df_in = data_input(), 
                                df_definitions = data_definitions()) %>%
          purrr::set_names(champCalculator::var_names$func) %>%
          dplyr::bind_cols()
        ## create warnings for potentially bad variables
        txt <- "Warning:"
        
        if (!all(dplyr::between(data_wrangled$rr, 0, 300), na.rm = TRUE)) {
          txt <- paste(txt, "\nBlood pressure not between 0-300") }
        if (!all(dplyr::between(data_wrangled$pulse, 0, 300), na.rm = TRUE)) {
          txt <- paste(txt, "\nHeart rate not between 0-300")} 
        if (!all(dplyr::between(data_wrangled$spo2, 0, 100), na.rm = TRUE)) {
          txt <- paste(txt, "\nOxygen saturation not between 0-100")}
        if (!all(dplyr::between(data_wrangled$time_to_hems, 0, 180), na.rm = TRUE)) {
          txt <- paste(txt, "\nTime to HEMS not between 0-180")}
        if (!all(dplyr::between(data_wrangled$gcs, 3, 15), na.rm = TRUE)) {
          txt <- paste(txt, "\nGCS not between 3-15")}
        if (!all(dplyr::between(data_wrangled$age, 16, 120), na.rm = TRUE)) {
          txt <- paste(txt, "\nAge not between 16-120")}
        
        if (!(all(dplyr::between(data_wrangled$cardiac_rhythm, 0, 1), na.rm = TRUE) | 
            rlang::is_logical(data_wrangled$cardiac_rhythm))) {
          txt <- paste(txt, "\nCardiac rhythm not in the correct format")}
        
        if (!(all(dplyr::between(data_wrangled$medical_facility, 0, 1), na.rm = TRUE) | 
            rlang::is_logical(data_wrangled$medical_facility))) {
          txt <- paste(txt, "\nMedical facility not in the correct format")}
        if (!(all(dplyr::between(data_wrangled$vehicle_ground_unit, 0, 1), na.rm = TRUE) | 
            rlang::is_logical(data_wrangled$vehicle_ground_unit))) {
          txt <- paste(txt, "\nVehicle not in the correct format")}
        if (!(all(dplyr::between(data_wrangled$sex_male, 0, 1), na.rm = TRUE) | 
            rlang::is_logical(data_wrangled$sex_male))) {
          txt <- paste(txt, "\nSex not in the correct format")}
        
        if (txt != "Warning:") {
          output$warning_output <- renderText({ txt })
        } else {
          output$warning_output <- renderText({ NULL })
        }
        
      } else {
        return(NULL)
      }
      
      
      risk <- calculate_champ(
        rr                  = data_wrangled$rr,
        pulse               = data_wrangled$pulse,
        spo2                = data_wrangled$spo2,
        gcs                 = data_wrangled$gcs,
        time_to_hems        = data_wrangled$time_to_hems,
        age                 = data_wrangled$age,
        cardiac_rhythm      = data_wrangled$cardiac_rhythm,
        medical_facility    = data_wrangled$medical_facility,
        vehicle_ground_unit = data_wrangled$vehicle_ground_unit,
        sex_male            = data_wrangled$sex_male,
        code                = data_wrangled$code,
        limit_values        = TRUE,
        errors_as_warnings  = TRUE) 
      
      df_orig <- data_input()
      df_orig$risk <- risk
      values$df_orig <- df_orig
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        "data_with_risk.xlsx"
      },
      content = function(file) {
        openxlsx::write.xlsx(values$df_orig, file)
      }
    )
    
  })
}
    
## To be copied in the UI
# mod_use_table_ui("use_table_ui_1")
    
## To be copied in the server
# mod_use_table_server("use_table_ui_1")
