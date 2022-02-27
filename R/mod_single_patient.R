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
        
        selectInput("dispatch_code", 
                    h3("Dispatch code"),
                    choices = list(
                      "Cardiac arrest"              = "Cardiac arrest"              , 
                      "Trauma"                      = "Trauma"                      ,
                      "Respiratory failure"         = "Respiratory failure"         ,
                      "Chest pain"                  = "Chest pain"                  ,
                      "Stroke"                      = "Stroke"                      ,
                      "Neurological"                = "Neurological"                ,
                      "Gynecological and obsterics" = "Gynecological and obsterics" ,
                      "Infection"                   = "Infection"                   ,
                      "Psychiatric or intoxication" = "Psychiatric or intoxication" ,
                      "Other"                       = "Other"                       ),
                    selected = "Cardiac arrest"),
        
        radioButtons("vehicle", 
                     h3("Vehicle type"),
                     choices = list(
                       "Ground unit" = "Ground unit",
                       "Helicopter or BG helicopter" = "Helicopter or BG helicopter"), 
                     selected = "Ground unit"),
        
        radioButtons("med_facility", 
                     h3("Located in medical facility"),
                     choices = list("No"  = "Not medical facility", 
                                    "Yes" = "Medical facility"), 
                     selected = "Not medical facility"),
        
        radioButtons("sex", 
                     h3("Sex"),
                     choices = list("Woman"  = "Woman", "Man" = "Man"), 
                     selected = "Woman"),
        
        conditionalPanel(condition = "!input.rhythm_available",             
                         radioButtons("cardiac_rhythm", 
                                      h3("Cardiac rhythm (VF, VT, ASY, PEA)"),
                                      choices = list("No"  = "Other category", 
                                                     "Yes" = "VF, VT, ASY, PEA"), 
                                      selected = "Other category")),
        
      ),
      column(4, 
             numericInput("age", h3("Age (years)"), 
                          value = 18, min = 18, max = 100),
             
             conditionalPanel(condition = "!input.pulse_available",
                              numericInput("pulse", h3("Heart rate (bpm)"), 
                                           value = 100, min = 20, max = 220)),
             
             conditionalPanel(condition = "!input.rr_available",               
                              numericInput("rr", h3("Sys blood pressure (mmHg)"), 
                                           value = 120, min = 40, max = 250)),
             
             conditionalPanel(condition = "!input.spo2_available",               
                              numericInput("spo2", h3("Oxygen saturation"), 
                                           value = 100, min = 40, max = 100)),   
             
             conditionalPanel(condition = "!input.time_to_hems_available",               
                              numericInput("time_to_hems", h3("Time to HEMS arrival (minutes)"), 
                                           value = 30, min = 0, max = 160)),   
             
             conditionalPanel(condition = "!input.gcs_available",               
                              numericInput("gcs", h3("Glasgow Coma Scale"),  min = 3, max = 15, value = 15))
             
             
             
      ),
      column(3,
             h3("Missing variables"),
             checkboxInput("pulse_available",        "Heart rate",              FALSE),
             checkboxInput("rr_available",           "Systolic blood pressure", FALSE),
             checkboxInput("rhythm_available",       "Cardiac rhythm",          FALSE),
             checkboxInput("spo2_available",         "Oxygen saturation",       FALSE),
             checkboxInput("gcs_available",          "Glasgow Coma Scale",      FALSE),
             checkboxInput("time_to_hems_available", "Time to HEMS",            FALSE),
             
             
             h3("Results"),
             textOutput("patient_risk")
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
      ## calculate patients risk reactively
      df_patient <- tibble(
        age            = input$age,
        pulse          = input$pulse,
        rr             = input$rr,
        spo2           = input$spo2,
        time_to_hems   = input$time_to_hems,
        gcs            = input$gcs,
        sex            = input$sex,
        cardiac_rhythm = input$cardiac_rhythm,
        med_facility   = input$med_facility,
        vehicle        = input$vehicle,
        code           = input$dispatch_code
      ) 
      

      calculate_champ(df_patient, champCalculator::coeffs)
      
    })
    
    output$patient_risk <- renderText({ paste0("Risk: ", patient_risk(), "%") })
    
  })
}
    
## To be copied in the UI
# mod_single_patient_ui("single_patient_ui_1")
    
## To be copied in the server
# mod_single_patient_server("single_patient_ui_1")
