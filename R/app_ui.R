#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny shinythemes
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    navbarPage(
      "CHAMP risk calculator",
      theme = shinytheme("cerulean"),
      inverse = TRUE,
      # theme = shinytheme("sandstone"),
      tabPanel(
        "Multiple patients",
        fluidPage( mod_use_table_ui("use_table_ui_1") )
      ),
      tabPanel(
        "Single patient",
        fluidPage( mod_single_patient_ui("single_patient_ui_1") )
      )
  ))
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'champCalculator'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

