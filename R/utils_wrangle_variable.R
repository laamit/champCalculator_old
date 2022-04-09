#' wrangle_variable:  Wrangle a single input variable for the champ calculator 
#'
#' @description A utils function
#'
#'
#' @noRd
#' 
#' @param var_name Name of the variable to be wrangled
#' @param df_in Input data frame
#' @param df_definitions Variable definitions gotten from the 
#'    variable_definitions.xlsx
#' @return Vector containing the wrangled variable
#'
#' @importFrom dplyr filter %>% pull
#' @importFrom tibble tribble
#' 
#' @export
#'
#' @examples
#' 
wrangle_variable <- function(var_name, df_in, df_definitions) {
  # setup ------------------------------------------------
  df_var_info <- df_definitions %>% dplyr::filter(.data$variable == var_name)
  na_vals <- df_var_info %>% 
    dplyr::filter(.data$name_of_category == "<missing>") %>% 
    dplyr::pull(.data$value)
  
  x <- df_in[[df_var_info$name_in_data[1]]]
  
  if (!is.na(na_vals)) {
    ## potential multiple values need to be converted to a vector
    na_vals <- na_vals %>% strsplit(",") %>% unlist() 
    x <- ifelse(x %in% na_vals, NA, x)
  }
  
  # forcefully convert numeric variables to numeric--------------------------
  if (var_name %in% c(
    "Age (years)",
    "Heart rate (bpm)",
    "Systolic blood pressure (mmHg)",
    "Oxygen saturation (%)",
    "Time to HEMS arrival (minutes)",
    "Glasgow Coma Scale"
  )) x <- as.numeric(x)
  

  # wrangle categorical data ------------------------------------------------
  
  if (var_name %in% c(
    "Patient sex",
    "Cardiac rhythm",
    "Medical facility or nursing home",
    "HEMS vehicle",
    "Patient group"
  )) {
    ## force variable to be character
    x <- as.character(x)
    var_levels <- df_var_info %>% dplyr::filter(.data$name_of_category != "<missing>")
    
    for (lvl_i in 1:nrow(var_levels)) {
      ## multiple values per level need to be pre wrangled i.e. separated to a vector
      lvl_i_values <- var_levels$value[lvl_i] %>% strsplit(",") %>% unlist() %>% trimws()
      x[x %in% lvl_i_values] <- var_levels$name_of_category[lvl_i]
    }
    ## change the "<any other value>" values to NA, i.e. values that were not defined above
    x[!(x %in% var_levels$name_of_category)] <- NA
    
    ## change binary variables to 0/1
    if (var_name == "Patient sex") {
      x <- dplyr::case_when(x == "Male" ~ 1, x == "Female" ~ 0)
    }
    if (var_name == "Cardiac rhythm") {
      x <- dplyr::case_when(x == "VF, VT, Asystole, PEA" ~ 1, 
                            x != "VF, VT, Asystole, PEA" ~ 0)
      
    }
    if (var_name == "Medical facility or nursing home") {
      x  <- dplyr::case_when(x == "Yes" ~ 1,
                             x != "Yes" ~ 0)
    }
    if (var_name == "HEMS vehicle") {
      x  <- dplyr::case_when(x == "Ground unit" ~ 1,
                             x != "Ground unit" ~ 0)
      
    }
  }
  
  x
}
