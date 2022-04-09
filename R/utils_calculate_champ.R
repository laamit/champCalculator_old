#' calculate_champ: Calculate the risk for 30 mortality using CHAMP
#'
#' @param gcs Glasgow Coma Scale. Missing values allowed.
#' @param pulse Heart rate (bpm). Missing values allowed.
#' @param rr Systolic blood pressure (mmHg). Missing values allowed.
#' @param spo2 Oxygen saturation (%) as digits e.g. 1% = 1 rather than 0.01.
#'  Missing values allowed.
#' @param time_to_hems Time to HEMS arrival (minutes). Missing values allowed.
#' @param cardiac_rhythm Cardiac rhythm (VF, VT, Asystole, PEA). Missing values allowed.
#' @param age Age in years.
#' @param medical_facility Medical facility or nursing home as TRUE/FALSE or 1/0
#' @param vehicle_ground_unit HEMS vehicle ground unit (vs helicopter) as TRUE/FALSE or 1/0.
#' @param sex_male Patient sex male, TRUE/FALSE or 1/0.
#' @param code Patient group i.e. "cardiac arrest", "trauma", "respitory failure",
#' "chest pain", "stroke", "neurological", "psychiatric or intoxication", or "other". 
#' All other non-missing values are assumed to not be in any of those groups 
#' and instead belong to "Gynaecology and obstetrics" and "Infection" groups.
#' @param limit_values Should values be winsorized to match the original data set better? 
#' Default FALSE. Limits the values below / larger than 0.5th and 99.5th percentiles
#' to the percentile values. Note: Some variables such as *time_to_hems* can have
#' really long tales in outlier cases leading to questionable risk estimates. 
#' 
#' 
#' @param errors_as_warnings Should error's be returned as warnings? Default FALSE.
#'
#' @description Calculates the CHAMP 30 day mortality risk for the given values.
#' All the variables need to be of the same length. NA values are allowed for 
#' *gcs*, *pulse*, *rr*, *cardiac_rhythm*, *spo2*, and *time_to_hems* as the 
#' calculator will automatically select the model fitted for those variables.
#'
#' @return Vector of risks for each observation.
#'
#' 
#' @import assertthat dplyr rlang
#'
#' @export
#'
#' @examples
#' 
#' calculate_champ(rr = 100,
#'                pulse = 100,
#'                spo2 = 100,
#'                gcs = 15,
#'                time_to_hems = 100,
#'                cardiac_rhythm = 1,
#'                age = 20,
#'                medical_facility = 0,
#'                vehicle_ground_unit = 1,
#'                sex_male = 1,
#'                code = "trauma")
#' calculate_champ(rr = 100,
#'                pulse = 100,
#'                spo2 = 100,
#'                gcs = 15,
#'                time_to_hems = 100,
#'                cardiac_rhythm = NA,
#'                age = 20,
#'                medical_facility = 0,
#'                vehicle_ground_unit = 1,
#'                sex_male = 1,
#'                code = "trauma")
#' calculate_champ(rr = rep(100, 3),
#'                pulse = rep(100, 3),
#'                spo2 = rep(100, 3),
#'                gcs = rep(15, 3),
#'                time_to_hems = rep(100, 3),
#'                cardiac_rhythm = rep(NA, 3),
#'                age = rep(20, 3),
#'                medical_facility = rep(0, 3),
#'                vehicle_ground_unit = rep(1, 3),
#'                sex_male = rep(1, 3),
#'                code = rep("trauma", 3))
#' 
#' calculate_champ(rr = c(100, 200, 300),
#'                pulse = rep(100, 3),
#'                spo2 = rep(100, 3),
#'                gcs = rep(15, 3),
#'                time_to_hems = rep(100, 3),
#'                cardiac_rhythm = rep(NA, 3),
#'                age = rep(20, 3),
#'                medical_facility = rep(0, 3),
#'                vehicle_ground_unit = rep(1, 3),
#'                sex_male = rep(1, 3),
#'                code = rep("trauma", 3),
#'                limit_values = TRUE)
#' 
#' 
calculate_champ <- function(rr,
                            pulse,
                            spo2,
                            gcs,
                            time_to_hems,
                            age,
                            cardiac_rhythm,
                            medical_facility,
                            vehicle_ground_unit,
                            sex_male,
                            code,
                            limit_values = TRUE,
                            errors_as_warnings = FALSE) {
  
  df_coefs <- champCalculator::coeffs
  code <- tolower(code)

  # sanity checks ---------------------------------------------------------------
  
  if (errors_as_warnings) {
    txt <- ""
    if (!all(dplyr::between(rr, 0, 300), na.rm = TRUE)) {
      txt <- paste(txt, "\nBlood pressure not between 0-300") }
    if (!all(dplyr::between(pulse, 0, 300), na.rm = TRUE)) {
      txt <- paste(txt, "\nHeart rate not between 0-300")} 
    if (!all(dplyr::between(spo2, 0, 100), na.rm = TRUE)) {
      txt <- paste(txt, "\nOxygen saturation not between 0-100")}
    if (!all(dplyr::between(time_to_hems, 0, 180), na.rm = TRUE)) {
      txt <- paste(txt, "\nTime to HEMS not between 0-180")}
    if (!all(dplyr::between(gcs, 3, 15), na.rm = TRUE)) {
      txt <- paste(txt, "\nGCS not between 3-15")}
    if (!all(dplyr::between(age, 16, 120), na.rm = TRUE)) {
      txt <- paste(txt, "\nAge not between 16-120")}
    
    if (!(all(dplyr::between(cardiac_rhythm, 0, 1), na.rm = TRUE) | 
          rlang::is_logical(cardiac_rhythm))) {
      txt <- paste(txt, "\nCardiac rhythm not in the correct format")}
    
    if (!(all(dplyr::between(medical_facility, 0, 1), na.rm = TRUE) | 
          rlang::is_logical(medical_facility))) {
      txt <- paste(txt, "\nMedical facility not in the correct format")}
    if (!(all(dplyr::between(vehicle_ground_unit, 0, 1), na.rm = TRUE) | 
          rlang::is_logical(vehicle_ground_unit))) {
      txt <- paste(txt, "\nVehicle not in the correct format")}
    if (!(all(dplyr::between(sex_male, 0, 1), na.rm = TRUE) | 
          rlang::is_logical(sex_male))) {
      txt <- paste(txt, "\nSex not in the correct format")}
    
    if (txt != "") warning(txt)
 
  } else {
    
  assertthat::assert_that(all(dplyr::between(rr, 0, 300), na.rm = TRUE), 
                          msg = "Blood pressure not between 0-300")
  assertthat::assert_that(all(dplyr::between(pulse, 0, 300), na.rm = TRUE), 
                          msg = "Heart rate not between 0-300")
  assertthat::assert_that(all(dplyr::between(spo2, 0, 100), na.rm = TRUE), 
                          msg = "Oxygen saturation not between 0-100")
  assertthat::assert_that(all(dplyr::between(time_to_hems, 0, 180), na.rm = TRUE), 
                          msg = "Time to HEMS not between 0-180")
  assertthat::assert_that(all(dplyr::between(gcs, 3, 15), na.rm = TRUE), 
                          msg = "GCS not between 3-15")
  assertthat::assert_that(all(dplyr::between(cardiac_rhythm, 0, 1), na.rm = TRUE) | 
                            rlang::is_logical(cardiac_rhythm))
  
  assertthat::assert_that(all(dplyr::between(age, 16, 120), na.rm = TRUE), 
                          msg = "Age not between 16-120")
  
  assertthat::assert_that(all(dplyr::between(medical_facility, 0, 1), na.rm = TRUE) | 
                            rlang::is_logical(medical_facility))
  assertthat::assert_that(all(dplyr::between(vehicle_ground_unit, 0, 1), na.rm = TRUE) | 
                            rlang::is_logical(vehicle_ground_unit))
  assertthat::assert_that(all(dplyr::between(sex_male, 0, 1), na.rm = TRUE) | 
                            rlang::is_logical(sex_male))
  
  }

  
  
  # wrangle data ----------------------------------
  # if all values are NA need to declare type of NA
  
  if ( all(is.na(cardiac_rhythm)) ) {cardiac_rhythm <- rep(NA_real_, length(cardiac_rhythm))}
  if ( all(is.na(medical_facility)) ) {medical_facility <- rep(NA_real_, length(medical_facility))}
  if ( all(is.na(vehicle_ground_unit)) ) {vehicle_ground_unit <- rep(NA_real_, length(vehicle_ground_unit))}
  if ( all(is.na(sex_male)) ) {sex_male <- rep(NA_real_, length(sex_male))}

  
  df_in <- dplyr::tibble(rr, pulse, cardiac_rhythm, spo2, gcs, time_to_hems, 
                         age, medical_facility, vehicle_ground_unit, 
                         sex_male, code) %>% 
    ## convert boolean to 0/1
    # dplyr::mutate_at(c("cardiac_rhythm", "medical_facility", "vehicle_ground_unit", "sex_male"), 
                     # ~dplyr::if_else(all(rlang::is_logical(.)), as.numeric(.), .)) %>% 
    dplyr::mutate(
      cardiac_rhythm      = cardiac_rhythm %>% as.numeric(),
      medical_facility    = medical_facility %>% as.numeric(),
      vehicle_ground_unit = vehicle_ground_unit %>% as.numeric(),
      sex_male            = sex_male %>% as.numeric(),
      
      code_cardiac_arr  = dplyr::case_when(.data$code == "cardiac arrest"     ~  1, !is.na(.data$code) ~ 0),          
      code_trauma       = dplyr::case_when(.data$code == "trauma"             ~  1, !is.na(.data$code) ~ 0),   
      code_respitory    = dplyr::case_when(.data$code == "respitory failure"  ~  1, !is.na(.data$code) ~ 0),        
      code_chest_pain   = dplyr::case_when(.data$code == "chest pain"         ~  1, !is.na(.data$code) ~ 0),         
      code_stroke       = dplyr::case_when(.data$code == "stroke"             ~  1, !is.na(.data$code) ~ 0),   
      code_neurological = dplyr::case_when(.data$code == "neurological"       ~  1, !is.na(.data$code) ~ 0),     
      code_psyc_intox   = dplyr::case_when(.data$code == "psychiatric or intoxication" ~  1, !is.na(.data$code) ~ 0),     
      code_other        = dplyr::case_when(.data$code == "other"                     ~  1, !is.na(.data$code) ~ 0),   
      
      intercept = 1, # makes calculation easier
    )
  
  
  # winsorize numeric data  ---------------------------------------
  if (limit_values) {
    ## Limits taken from the original data as 0.5 and 99.5 percentile values
    ## for variables can have long tails based on the original data
    df_limits <- tibble::tribble(
      ~x,               ~low,      ~up,
      "pulse",            25,       200, 
      "rr",               51,       235, 
      "spo2",             50,       100,
      "time_to_hems",     6,        126,
    )

    for (i in 1:nrow(df_limits)) {
      df_limits_i <- df_limits %>% dplyr::slice(i)
      df_in[[ df_limits_i$x ]] <- ifelse(df_in[[ df_limits_i$x ]] < df_limits_i$low, 
                                     df_limits_i$low, df_in[[ df_limits_i$x ]])
      df_in[[ df_limits_i$x ]] <- ifelse(df_in[[ df_limits_i$x ]] > df_limits_i$up, 
                                     df_limits_i$up, df_in[[ df_limits_i$x ]])
          
   
    }
    
  } 
  
  
  # add the coefficients to the data ---------------------------
  ## use the best available model -> indicators for missingness from data
  df_in <- df_in %>% 
    dplyr::mutate(
      ind_rr           = is.na(.data$rr),
      ind_pulse        = is.na(.data$pulse),
      ind_rhythm       = is.na(.data$cardiac_rhythm),
      ind_spo2         = is.na(.data$spo2),
      ind_gcs          = is.na(.data$gcs),
      ind_time_to_hems = is.na(.data$time_to_hems),
    )
  
  ## add the beta coefficients for each patient by choosing the model using 
  ## the indicators for missingness
  df_in_coefs <- dplyr::left_join(df_in, df_coefs, 
                             by = c("ind_rr", "ind_pulse", "ind_rhythm", 
                                    "ind_spo2", "ind_gcs", "ind_time_to_hems")) 
  
  
  # make not used variables zero for easier score calculation --------------
  df_in_coefs <- df_in_coefs %>%
    dplyr::mutate(
      ## make unused NA variables zero
      rr              = dplyr::if_else(.data$ind_rr,           0, .data$rr %>% as.numeric()),
      pulse           = dplyr::if_else(.data$ind_pulse,        0, .data$pulse %>% as.numeric()),
      spo2            = dplyr::if_else(.data$ind_spo2,         0, .data$spo2 %>% as.numeric()),
      gcs             = dplyr::if_else(.data$ind_gcs,          0, .data$gcs %>% as.numeric()),
      cardiac_rhythm  = dplyr::if_else(.data$ind_rhythm,       0, .data$cardiac_rhythm %>% as.numeric()),
      time_to_hems    = dplyr::if_else(.data$ind_time_to_hems, 0, .data$time_to_hems %>% as.numeric()),
      
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
    )
  
  
  # calculate score ---------------------------------------------------------
  model_variables <- grep("beta_", colnames(df_coefs), value = TRUE) 
  model_variables <- sub("beta_", "", model_variables)
  
  score <- vapply(model_variables, function(x_var) {
    df_in_coefs[[ x_var ]] * df_in_coefs[[ paste0("beta_", x_var) ]]}, 
    numeric(nrow(df_in_coefs))) 

  if ( nrow(df_in_coefs) == 1 ) {
    score <- sum(score) 
  } else {
    score <- rowSums(score)
  }
  
  risk <- exp(score) / (1 + exp(score)) 
  risk
  
  
}
