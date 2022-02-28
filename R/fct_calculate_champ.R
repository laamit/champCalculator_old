#' calculate_champ: Calculate the risk for 30 mortality using CHAMP
#'
#' @description A fct function
#'
#' @return Vector of risks for each observation.
#'
#' 
#' @param df_in Input data frame
#' @param df_ind_coefs Data frame containing the coefficients for the models
#' @param return_risk Should risk be returned? Default TRUE.
#'
#' @import assertthat dplyr rlang
#'
#' @export
#'
#' @examples
#' 
#' 
calculate_champ <- function(df_in, 
                            df_coefs,
                            return_risk = TRUE) {
  
  
  # sanity checks ---------------------------------------------------------------
  assertthat::not_empty(df_in)
  required_vars <- c("rr", "pulse", "code", "cardiac_rhythm", "spo2", 
                     "gcs", "time_to_hems", "age", 
                     "med_facility", "vehicle", "sex")
  
  assertthat::has_name(df_in, required_vars)
  
  assertthat::not_empty(df_coefs)
  assertthat::has_name(df_coefs, c("ind_rr", "ind_pulse", "ind_rhythm", 
                                   "ind_spo2", "ind_gcs", "ind_time_to_hems"))
  
  
  # wrangle data to match risk calculation ----------------------------------
  
  df_in <- df_in %>% 
    dplyr::mutate(
      sex_man        = dplyr::case_when(.data$sex == "Man" ~ 1, .data$sex == "Woman" ~ 0),
      
      cardiac_rhythm = dplyr::case_when(
        .data$cardiac_rhythm == "VF, VT, ASY, PEA" ~ 1, 
        .data$cardiac_rhythm != "VF, VT, ASY, PEA" ~ 0),
      
      med_facility   = dplyr::case_when(
        .data$med_facility == "Medical facility" ~ 1,
        .data$med_facility != "Medical facility" ~ 0),
      
      vehicle_ground_unit = dplyr::case_when(
        .data$vehicle == "Ground unit" ~ 1,
        .data$vehicle != "Ground unit" ~ 0)
    )
  
  
  # add the coefficients to the df ---------------------------
  ## use the best available model -> indicators for missingness from data
  df_in <- df_in %>% 
    dplyr::mutate(ind_rr           = is.na(.data$rr),
                  ind_pulse        = is.na(.data$pulse),
                  ind_rhythm       = is.na(.data$cardiac_rhythm),
                  ind_spo2         = is.na(.data$spo2),
                  ind_gcs          = is.na(.data$gcs),
                  ind_time_to_hems = is.na(.data$time_to_hems))
  
  ## add the beta coefficient for each patient separately
  df_tmp <- dplyr::left_join(df_in, df_coefs, 
                             by = c("ind_rr", "ind_pulse", "ind_rhythm", 
                                    "ind_spo2", "ind_gcs", "ind_time_to_hems")) 
  
  
  # make not used variables zero for easier score calculation --------------
  df_tmp <- df_tmp %>%
    dplyr::mutate(
      ## make unused NA variables zero
      rr              = dplyr::if_else(.data$ind_rr,    0, .data$rr),
      pulse           = dplyr::if_else(.data$ind_pulse, 0, .data$pulse),
      spo2            = dplyr::if_else(.data$ind_spo2,  0, .data$spo2),
      gcs             = dplyr::if_else(.data$ind_gcs,   0, .data$gcs),
      cardiac_rhythm  = dplyr::if_else(.data$ind_rhythm,       0, .data$cardiac_rhythm),
      time_to_hems    = dplyr::if_else(.data$ind_time_to_hems, 0, .data$time_to_hems),
      
    ) %>% 
    ## add the transformations
    dplyr::mutate(
      rr_p2              = .data$rr^2,
      spo2_p2            = .data$spo2^2,
      pulse_p2           = .data$pulse^2,
      time_to_hems_p2    = .data$time_to_hems^2,
      age_p2             = .data$age^2,
      gcs_p2             = .data$gcs^2,
      
      rr_p3              = .data$rr^3,
      spo2_p3            = .data$spo2^3,
      pulse_p3           = .data$pulse^3,
      time_to_hems_p3    = .data$time_to_hems^3,
      age_p3             = .data$age^3,
      gcs_p3             = .data$gcs^3,
      
      code_cardiac_arr  = dplyr::case_when(.data$code == "cardiac arrest"     ~  1, !is.na(.data$code) ~ 0),          
      code_trauma       = dplyr::case_when(.data$code == "trauma"             ~  1, !is.na(.data$code) ~ 0),   
      code_respitory    = dplyr::case_when(.data$code == "respitory failure"  ~  1, !is.na(.data$code) ~ 0),        
      code_chest_pain   = dplyr::case_when(.data$code == "chest pain"         ~  1, !is.na(.data$code) ~ 0),         
      code_stroke       = dplyr::case_when(.data$code == "stroke"             ~  1, !is.na(.data$code) ~ 0),   
      code_neurological = dplyr::case_when(.data$code == "neurological"       ~  1, !is.na(.data$code) ~ 0),     
      code_psyc_intox   = dplyr::case_when(.data$code == "psychiatric or intoxication" ~  1, !is.na(.data$code) ~ 0),     
      code_other        = dplyr::case_when(.data$code == "other"                     ~  1, !is.na(.data$code) ~ 0),   
      
      `intercept` = 1, # makes calculation easier
      
    )
  
  
  # calculate score ---------------------------------------------------------
  vars_included <- df_coefs %>% 
    dplyr::select(dplyr::starts_with("beta_")) %>% 
    colnames()
  vars_included <- sub("beta_", "", vars_included)
  
  score <- vapply(vars_included, function(x_var) {
    df_tmp[[ x_var ]] * df_tmp[[ paste0("beta_", x_var) ]]}, numeric(nrow(df_tmp))) 

  if ( nrow(df_tmp) == 1 ) {
    score <- sum(score) 
  } else {
    score <- rowSums(score)
  }
  
  if (!return_risk) return(score)
  
  risk <- exp(score) / (1 + exp(score)) 
  risk
  
  
}
