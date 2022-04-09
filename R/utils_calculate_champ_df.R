#' calculate_champ_df: Calculate the risk for 30 mortality using CHAMP with 
#' data frame as the input
#'
#' @description A fct function
#'
#' @return Vector of risks for each observation.
#'
#' 
#' @param df_in Input data frame, needs to contain the following variables
#' "rr", "pulse", "code", "cardiac_rhythm", "spo2", 
#' "gcs", "time_to_hems", "age", 
#' "medical_facility", "vehicle_ground_unit", "sex_male"
#' and be in the correct format i.e. continuous variables as numeric, 
#' binary ones  as 0/1 and patient group code as string matching the defined 
#' categories.
#'
#' @import assertthat dplyr rlang
#'
#' @export
#'
#' @examples
#' 
#' @importFrom assertthat not_empty has_name
#' 
#' 
calculate_champ_df <- function(df_in) {
  
  
  # sanity checks ---------------------------------------------------------------
  assertthat::not_empty(df_in)
  required_vars <- c("rr", "pulse", "code", "cardiac_rhythm", "spo2", 
                     "gcs", "time_to_hems", "age", 
                     "medical_facility", "vehicle_ground_unit", "sex_male")
  
  assertthat::has_name(df_in, required_vars)
  
  

  # use the calculate_champ function  ---------------------------------------
  
  with(df_in, champCalculator::calculate_champ(rr = rr,
                  pulse = pulse,
                  spo2 = spo2,
                  gcs = gcs,
                  time_to_hems = time_to_hems,
                  cardiac_rhythm = cardiac_rhythm,
                  age = age,
                  medical_facility = medical_facility,
                  vehicle_ground_unit = vehicle_ground_unit,
                  sex_male = sex_male,
                  code = code))  
  
  
  
  
}
