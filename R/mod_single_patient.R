#' single_patient UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_single_patient_ui <- function(id){
  ns <- NS(id)
  tagList(
    titlePanel(h1("CHAMP risk calculator", align = "center")),
    hr(),
    fluidRow(
      column(
        5,
        
        selectInput(ns("dispatch_code"), 
                    h3("Patient group"),
                    choices = list(
                      "Cardiac arrest"              = "Cardiac arrest"              , 
                      "Trauma"                      = "Trauma"                      ,
                      "Respiratory failure"         = "Respiratory failure"         ,
                      "Chest pain"                  = "Chest pain"                  ,
                      "Stroke"                      = "Stroke"                      ,
                      "Neurological"                = "Neurological"                ,
                      "Gynaecology and obsterics"   = "Gynaecology and obsterics"   ,
                      "Infection"                   = "Infection"                   ,
                      "Psychiatric or intoxication" = "Psychiatric or intoxication" ,
                      "Other"                       = "Other"                       ),
                    selected = NA),
        
        radioButtons(ns("vehicle"), 
                     h3("HEMS vehicle"),
                     choices = list("Ground unit" = 1, "Helicopter"  = 0), 
                     selected = NA),
        
        radioButtons(ns("med_facility"), 
                     h3("Medical facility or nursing home"),
                     choices = list("No"  = 0, "Yes" = 1), 
                     selected = NA),
        
        radioButtons(ns("sex_male"), 
                     h3("Patient sex"),
                     choices = list("Female"  = 0, "Male" = 1), 
                     selected = NA),
        
             numericInput(ns("age"), h3("Age (years)"), 
                          value = NA, min = 16, max = 100),
             
        
      ),
      column(4, 
        conditionalPanel(condition = "!input.rhythm_not_available", ns = ns,             
                         radioButtons(ns("cardiac_rhythm"), 
                                      h3("Cardiac rhythm"),
                                      choices = list("Other category"        = 0, 
                                                     "VF, VT, Asystole, PEA" = 1,
                                                     "<Missing>"             = -1), 
                                      selected = -1)),
             conditionalPanel(condition = "!input.pulse_not_available", ns = ns,
                              numericInput(ns("pulse"), h3("Heart rate (bpm)"), 
                                           value = NA, min = 20, max = 220)),
             
             conditionalPanel(condition = "!input.rr_not_available", ns = ns,               
                              numericInput(ns("rr"), h3("Systolic blood pressure (mmHg)"), 
                                           value = NA, min = 40, max = 250)),
             
             conditionalPanel(condition = "!input.spo2_not_available", ns = ns,               
                              numericInput(ns("spo2"), h3("Oxygen saturation (%)"), 
                                           value = NA, min = 40, max = 100)),   
             
             conditionalPanel(condition = "!input.gcs_not_available",  ns = ns,              
                              numericInput(ns("gcs"), h3("Glasgow Coma Scale"),  
                                           min = 0, max = 15, value = NA)),
             
             conditionalPanel(condition = "!input.time_to_hems_not_available",  ns = ns,              
                              numericInput(ns("time_to_hems"), h3("Time to HEMS arrival (minutes)"), 
                                           value = NA, min = 0, max = 160))
      ),
      column(3,
             h3("30d mortality risk:"),
             h2(textOutput(ns("patient_risk"))),
             tags$br(),
             tags$br(),
             h3("Options:"),
             checkboxInput(ns("winsorize_values"), "Winsorize values to match original data", TRUE),
      )
    )
  )
}
    
#' single_patient Server Functions
#'
#' @noRd 
mod_single_patient_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    patient_risk <- reactive({
      
      cardiac_rhythm <- input$cardiac_rhythm %>% as.numeric()
      if (cardiac_rhythm == -1) cardiac_rhythm <- as.numeric(NA)

      risk <- calculate_champ(
        pulse               = input$pulse          %>% as.numeric(),
        rr                  = input$rr             %>% as.numeric(),
        spo2                = input$spo2           %>% as.numeric(),
        time_to_hems        = input$time_to_hems   %>% as.numeric(),
        gcs                 = input$gcs            %>% as.numeric(),
        cardiac_rhythm      = cardiac_rhythm,
        age                 = input$age            %>% as.numeric(),
        sex_male            = input$sex_male       %>% as.numeric(),
        medical_facility    = input$med_facility   %>% as.numeric(),
        vehicle_ground_unit = input$vehicle        %>% as.numeric(),
        code                = input$dispatch_code,
        limit_values        = input$winsorize_values)
      
      risk <- paste0(round(risk, digits = 3) * 100, "%")
      
      risk
    })
    
    output$patient_risk <- renderText({ patient_risk() })
  })
}



## To be copied in the UI
# mod_single_patient_ui("single_patient_ui_1")

## To be copied in the server
# mod_single_patient_server("single_patient_ui_1")
