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
               fileInput('definitions', 'Variable definitions table',
                         multiple = FALSE,
                         accept = c(".xlsx")),
               
               fileInput('data', 'Data in excel or csv format',
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
               actionButton("calculate_risk",
                            "Calculate the risk",
                            class = "btn btn-primary"),
               HTML(strrep(br(), 4)),
               downloadButton("downloadData", "Download data with risk"),
        ),
        column(3,
               h3("Options"),
               checkboxInput("limit_values", "Winsorize values to match original data", TRUE)
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
      } else {
        return(NULL)
      }
      
      risk <- calculate_champ(data_wrangled, champCalculator::coeffs)
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
