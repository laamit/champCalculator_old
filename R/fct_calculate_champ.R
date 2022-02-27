#' calculate_champ: Calculate the risk for 30 mortality using CHAMP
#'
#' @description A fct function
#'
#' @return Vector of risks for each observation.
#'
#' @noRd
#' 
#' @param df_in Input data frame
#' @param df_ind_coefs Data frame containing the coefficients for the models
#' @param return_risk Should risk be returned? Default TRUE.
#' @param calculate_available Should the best available model be used? 
#' If FALSE missing data will lead to NA results. Default TRUE.
#'
#'
#' @examples
#' 
#' 
calculate_champ <- function(df_in, 
                            df_coefs,
                            return_risk = TRUE,
                            calculate_available = TRUE) {
  
  
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
      sex_man        = dplyr::case_when(sex == "man" ~ 1, sex == "woman" ~ 0),
      cardiac_rhythm = dplyr::case_when(cardiac_rhythm == "VF, VT, ASY, PEA" ~ 1, 
                                        cardiac_rhythm != "VF, VT, ASY, PEA" ~ 0),
      med_facility   = dplyr::case_when(med_facility == "Medical facility" ~ 1,
                                        med_facility == "Not medical facility" ~ 0),
      vehicle_ground_unit = dplyr::case_when(vehicle == "Ground unit" ~ 1,
                                             vehicle == "Helicopter or BG helicopter" ~ 0)
    )
  
  
  # add the coefficients to the df ---------------------------
  ## use the best available model -> indicators for missingness from data
  df_in <- df_in %>% 
    dplyr::mutate(ind_rr           = is.na(rr),
                  ind_pulse        = is.na(pulse),
                  ind_rhythm       = is.na(cardiac_rhythm),
                  ind_spo2         = is.na(spo2),
                  ind_gcs          = is.na(gcs),
                  ind_time_to_hems = is.na(time_to_hems))
  
  ## add the beta coefficient for each patient separately
  df_tmp <- df_in %>% 
    dplyr::left_join(df_coefs, by = c("ind_rr", "ind_pulse", "ind_rhythm", 
                                      "ind_spo2", "ind_gcs", "ind_time_to_hems")) 
  
  
  # make not used variables zero for easier score calculation --------------
  df_tmp <- df_tmp %>%
    dplyr::mutate(
      ## make unused NA variables zero
      rr              = dplyr::if_else(ind_rr,    0, rr),
      pulse           = dplyr::if_else(ind_pulse, 0, pulse),
      spo2            = dplyr::if_else(ind_spo2,  0, spo2),
      gcs             = dplyr::if_else(ind_gcs,   0, gcs),
      cardiac_rhythm  = dplyr::if_else(ind_rhythm,       0, cardiac_rhythm),
      time_to_hems    = dplyr::if_else(ind_time_to_hems, 0, time_to_hems),
      
    ) %>% 
    ## add the transformations
    dplyr::mutate(
      rr_p2              = rr^2,
      spo2_p2            = spo2^2,
      pulse_p2           = pulse^2,
      time_to_hems_p2    = time_to_hems^2,
      age_p2             = age^2,
      gcs_p2             = gcs^2,
      
      rr_p3              = rr^3,
      spo2_p3            = spo2^3,
      pulse_p3           = pulse^3,
      time_to_hems_p3    = time_to_hems^3,
      age_p3             = age^3,
      gcs_p3             = gcs^3,
      
      code_cardiac_arr  = dplyr::case_when(code == "cardiac arrest"     ~  1, !is.na(code) ~ 0),          
      code_trauma       = dplyr::case_when(code == "trauma"             ~  1, !is.na(code) ~ 0),   
      code_respitory    = dplyr::case_when(code == "respitory failure"  ~  1, !is.na(code) ~ 0),        
      code_chest_pain   = dplyr::case_when(code == "chest pain"         ~  1, !is.na(code) ~ 0),         
      code_stroke       = dplyr::case_when(code == "stroke"             ~  1, !is.na(code) ~ 0),   
      code_neurological = dplyr::case_when(code == "neurological"       ~  1, !is.na(code) ~ 0),     
      code_psyc_intox   = dplyr::case_when(code == "psychiatric or intoxication" ~  1, !is.na(code) ~ 0),     
      code_other        = dplyr::case_when(code == "other"                     ~  1, !is.na(code) ~ 0),   
      
      `intercept` = 1, # makes calculation easier
      
    )
  
  
  # calculate score ---------------------------------------------------------
  
  
  vars_included <- df_coefs %>% select(starts_with("beta_")) %>% colnames() %>% sub("beta\\_", "", .)
  
  score <- vapply(vars_included, function(x_var) {
    df_tmp[[ x_var ]] * df_tmp[[ paste0("beta_", x_var) ]]}, numeric(nrow(df_tmp))) 
  score <- rowSums(score)
  
  if (!return_risk) return(score)
  
  risk <- exp(score) / (1 + exp(score)) 
  risk
  
  
}
