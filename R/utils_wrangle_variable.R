#' wrangle_variable:  Wrangle input variables for the champ calculator 
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
#' @param limit_values Should values be limited close to what the original data 
#'    had? Default TRUE. Results might be unreliable without this.
#'
#' @return Vector containing the wrangled variable
#'
#' @importFrom dplyr filter %>% pull
#' @importFrom tibble tribble
#'
#' @examples
#' 
wrangle_variable <- function(var_name, df_in, df_definitions, limit_values = TRUE) {
  # setup ------------------------------------------------
  df_var_info <- df_definitions %>% dplyr::filter(.data$variable == var_name)
  na_vals <- df_var_info %>% 
    dplyr::filter(.data$name_of_category == "<missing>") %>% 
    dplyr::pull(.data$value)
  ## potential multiple values need to be converted to a vector
  na_vals <- na_vals %>% strsplit(",") %>% unlist() 
  
  x <- df_in[[df_var_info$name_in_data[1]]]
  x <- ifelse(x %in% na_vals, NA, x)
  
  # forcefully convert numeric variables to numeric--------------------------
  if (var_name %in% c(
    "age",
    "heart rate",
    "systolic blood pressure",
    "respiratory rate",
    "oxygen saturation",
    "delay from emergency call to HEMS unit arrival,",
    "Glasgow Coma Scale"
  )) x <- as.numeric(x)
  
  # wrangle numeric data ---------------------------------------
  if (limit_values) {
    ## Limits taken from the original data as 0.5 and 99.5 percentile values
    ## for variables can have long tails based on the original data
    df_limits <- tibble::tribble(
      ~variable,                                            ~low,      ~high,
      # "age",                                                16.4,       94.1,
      "heart rate",                                         25,       200, 
      "systolic blood pressure",                            51,       235, 
      "respiratory rate",                                   0,        48,
      "oxygen saturation",                                  50,       100,
      "delay from emergency call to HEMS unit arrival,",     6,        126,
    )
    
    var_limits <- df_limits %>% dplyr::filter(.data$variable == var_name)
    
    if (nrow(var_limits) != 0) {
      x <- ifelse(x < var_limits$low, var_limits$low, x)
      x <- ifelse(x > var_limits$high, var_limits$high, x)
    }
  } 
  
  # wrangle categorical data ------------------------------------------------
  
  if (var_name %in% c(
    "sex",
    "cardiac rhythm",
    "mission located in a medical facility",
    "vehicle type",
    "dispatch code"
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
  }
  
  x
}