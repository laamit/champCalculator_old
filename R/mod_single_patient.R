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
                    selected = "Cardiac arrest"),
        
        radioButtons(ns("vehicle"), 
                     h3("HEMS vehicle"),
                     choices = list(
                       "Ground unit" = "Ground unit",
                       "Helicopter"  = "Helicopter"), 
                     selected = "Ground unit"),
        
        radioButtons(ns("med_facility"), 
                     h3("Medical facility or nursing home"),
                     choices = list("No"  = "No", 
                                    "Yes" = "Yes"), 
                     selected = "No"),
        
        radioButtons(ns("sex"), 
                     h3("Patient sex"),
                     choices = list("Female"  = "Female", "Male" = "Male"), 
                     selected = "Female"),
        
        conditionalPanel(condition = "!input.rhythm_not_available",             
                         radioButtons(ns("cardiac_rhythm"), 
                                      h3("Cardiac rhythm (VF, VT, ASY, PEA)"),
                                      choices = list("No"  = "Other category", 
                                                     "Yes" = "VF, VT, ASY, PEA"), 
                                      selected = "Other category")),
        
      ),
      column(4, 
             numericInput(ns("age"), h3("Age (years)"), 
                          value = 18, min = 18, max = 100),
             
             conditionalPanel(condition = "!input.pulse_not_available",
                              numericInput(ns("pulse"), h3("Heart rate (bpm)"), 
                                           value = 100, min = 20, max = 220)),
             
             conditionalPanel(condition = "!input.rr_not_available",               
                              numericInput(ns("rr"), h3("Systolic blood pressure (mmHg)"), 
                                           value = 120, min = 40, max = 250)),
             
             conditionalPanel(condition = "!input.spo2_not_available",               
                              numericInput(ns("spo2"), h3("Oxygen saturation /%)"), 
                                           value = 100, min = 40, max = 100)),   
             
             conditionalPanel(condition = "!input.time_to_hems_not_available",               
                              numericInput(ns("time_to_hems"), h3("Time to HEMS arrival (minutes)"), 
                                           value = 30, min = 0, max = 160)),   
             
             conditionalPanel(condition = "!input.gcs_not_available",               
                              numericInput(ns("gcs"), h3("Glasgow Coma Scale"),  min = 3, max = 15, value = 15))
             
             
             
      ),
      column(3,
             h3("Missing variables"),
             checkboxInput(ns("pulse_not_available"),        "Heart rate",              FALSE),
             checkboxInput(ns("rr_not_available"),           "Systolic blood pressure", FALSE),
             checkboxInput(ns("rhythm_not_available"),       "Cardiac rhythm",          FALSE),
             checkboxInput(ns("spo2_not_available"),         "Oxygen saturation",       FALSE),
             checkboxInput(ns("gcs_not_available"),          "Glasgow Coma Scale",      FALSE),
             checkboxInput(ns("time_to_hems_not_available"), "Time to HEMS arrival",    FALSE),
             
             tags$br(),
             tags$br(),
             h3("Estimated risk"),
             h4(textOutput(ns("patient_risk")))
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
      df_patient <- tibble(
        age            = input$age %>% as.double(),
        pulse          = input$pulse %>% as.double(),
        rr             = input$rr %>% as.double(),
        spo2           = input$spo2 %>% as.double(),
        time_to_hems   = input$time_to_hems %>% as.double(),
        gcs            = input$gcs %>% as.double(),
        sex            = input$sex,
        cardiac_rhythm = input$cardiac_rhythm,
        med_facility   = input$med_facility,
        vehicle        = input$vehicle,
        code           = input$dispatch_code
      ) %>% 
        mutate(
          cardiac_rhythm = ifelse(cardiac_rhythm == "Yes", "VF, VT, ASY, PEA", "Other category"),
          # med_facility = ifelse(med_facility == "Yes", "Medical facility", "Not medical facility"),
          
          
          pulse          = if_else(input$pulse_not_available,  NA_real_, .data$pulse),
          rr             = if_else(input$rr_not_available,     NA_real_, .data$rr),
          cardiac_rhythm = if_else(input$rhythm_not_available, NA_character_, .data$cardiac_rhythm),
          spo2           = if_else(input$spo2_not_available,   NA_real_, .data$spo2),
          gcs            = if_else(input$gcs_not_available,    NA_real_, .data$gcs),
          time_to_hems   = if_else(input$time_to_hems_not_available, NA_real_, .data$time_to_hems),
        )
      
      risk <- calculate_champ(df_patient, champCalculator::coeffs)
      risk_formatted <- paste0(round(risk, digits = 3) * 100, "%")
      risk_formatted
      
    })
    
    output$patient_risk <- renderText({ patient_risk() })
    
  })
}
    
## To be copied in the UI
# mod_single_patient_ui("single_patient_ui_1")
    
## To be copied in the server
# mod_single_patient_server("single_patient_ui_1")
