
# setup -------------------------------------------------------------------


df_coeffs <- champCalculator::coeffs


manual_champ_calc <- function(df_coeffs_i,
                              rr,
                              pulse,
                              spo2,
                              gcs,
                              time_to_hems,
                              cardiac_rhythm,
                              age,
                              medical_facility,
                              vehicle_ground_unit,
                              sex_male,
                              code) {
  
  code_cardiac_arr  <- dplyr::case_when(code == "cardiac arrest"     ~  1, !is.na(code) ~ 0)
  code_trauma       <- dplyr::case_when(code == "trauma"             ~  1, !is.na(code) ~ 0)
  code_respitory    <- dplyr::case_when(code == "respitory failure"  ~  1, !is.na(code) ~ 0)
  code_chest_pain   <- dplyr::case_when(code == "chest pain"         ~  1, !is.na(code) ~ 0)
  code_stroke       <- dplyr::case_when(code == "stroke"             ~  1, !is.na(code) ~ 0)
  code_neurological <- dplyr::case_when(code == "neurological"       ~  1, !is.na(code) ~ 0)
  code_psyc_intox   <- dplyr::case_when(code == "psychiatric or intoxication" ~  1, !is.na(code) ~ 0)
  code_other        <- dplyr::case_when(code == "other"                     ~  1, !is.na(code) ~ 0)
  
  ## make zero if NA, coefficients are already chosen for the model
  if (is.na(rr) ) rr <- 0
  if (is.na(pulse) ) pulse <- 0
  if (is.na(spo2) ) spo2 <- 0
  if (is.na(gcs) ) gcs <- 0
  if (is.na(time_to_hems) ) time_to_hems <- 0
  if (is.na(cardiac_rhythm) ) cardiac_rhythm <- 0
  
  
  score <- age * df_coeffs_i$beta_age + 
    age^2 * df_coeffs_i$beta_age_p2 + age^3 * df_coeffs_i$beta_age_p3 +
    
  rr * df_coeffs_i$beta_rr + 
    # rr^2 * df_coeffs_i$beta_rr_p2 + # no model uses this
    rr^3 * df_coeffs_i$beta_rr_p3 +
    
  pulse * df_coeffs_i$beta_pulse + 
    pulse^2 * df_coeffs_i$beta_pulse_p2 + pulse^3 * df_coeffs_i$beta_pulse_p3 +
    
  spo2 * df_coeffs_i$beta_spo2 + 
    spo2^2 * df_coeffs_i$beta_spo2_p2 + spo2^3 * df_coeffs_i$beta_spo2_p3 +
    
  gcs * df_coeffs_i$beta_gcs + 
    gcs^2 * df_coeffs_i$beta_gcs_p2 + gcs^3 * df_coeffs_i$beta_gcs_p3 +
    
  time_to_hems * df_coeffs_i$beta_time_to_hems + 
    time_to_hems^2 * df_coeffs_i$beta_time_to_hems_p2 + time_to_hems^3 * df_coeffs_i$beta_time_to_hems_p3 +
    
    
    
    cardiac_rhythm * df_coeffs_i$beta_cardiac_rhythm + 
    medical_facility * df_coeffs_i$beta_medical_facility + 
    vehicle_ground_unit * df_coeffs_i$beta_vehicle_ground_unit + 
    sex_male * df_coeffs_i$beta_sex_male + 
    
    df_coeffs_i$beta_intercept + 
    
    code_cardiac_arr * df_coeffs_i$beta_code_cardiac_arr + 
    code_trauma * df_coeffs_i$beta_code_trauma + 
    code_respitory * df_coeffs_i$beta_code_respitory + 
    code_chest_pain * df_coeffs_i$beta_code_chest_pain + 
    code_stroke * df_coeffs_i$beta_code_stroke + 
    code_neurological * df_coeffs_i$beta_code_neurological + 
    code_psyc_intox * df_coeffs_i$beta_code_psyc_intox + 
    code_other * df_coeffs_i$beta_code_other 
    
  
  risk <- exp(score) / (1 + exp(score)) 
  risk
  
}


# tests for all model types - single patient ----------------------------------------------------


for (i in 1:nrow(df_coeffs)) {
  
  test_that(paste("single champ calculation works for model", i, "/ 64"), {
    gcs_i <- 15 
    pulse_i <- 100 
    cardiac_rhythm_i <- 1 
    rr_i <- 100
    spo2_i <- 100 
    time_to_hems_i <- 100 
    age_i <- 20
    medical_facility_i <- 0 
    vehicle_ground_unit_i <- 1 
    sex_male_i <- 1 
    code_i <- "trauma"
    
    df_coeffs_i <- df_coeffs %>% dplyr::slice(i)
    
    
    if (df_coeffs_i$ind_gcs)           gcs_i <- NA
    if (df_coeffs_i$ind_pulse)         pulse_i <- NA
    if (df_coeffs_i$ind_rhythm)        cardiac_rhythm_i <- NA
    if (df_coeffs_i$ind_rr)            rr_i <- NA
    if (df_coeffs_i$ind_spo2)          spo2_i <- NA
    if (df_coeffs_i$ind_time_to_hems)  time_to_hems_i <- NA
    
    
    risk_function <- calculate_champ(
      rr = rr_i, pulse = pulse_i, spo2 = spo2_i, gcs = gcs_i, 
      time_to_hems = time_to_hems_i, cardiac_rhythm = cardiac_rhythm_i, age = age_i, 
      medical_facility = medical_facility_i, vehicle_ground_unit = vehicle_ground_unit_i, 
      sex_male = sex_male_i, code = code_i)
    
    risk_manual <- manual_champ_calc(
      df_coeffs_i = df_coeffs_i,
      rr = rr_i, pulse = pulse_i, spo2 = spo2_i, gcs = gcs_i, 
      time_to_hems = time_to_hems_i, cardiac_rhythm = cardiac_rhythm_i, age = age_i, 
      medical_facility = medical_facility_i, vehicle_ground_unit = vehicle_ground_unit_i, 
      sex_male = sex_male_i, code = code_i)
    
    expect_equal(risk_function, risk_manual)
  })
}
# tests for all model types - multiple patients ----------------------------------------------------


for (i in 1:nrow(df_coeffs)) {
  
  test_that(paste("multiple champ calculation works for model", i, "/ 64"), {
    gcs_i <- 6:15 
    pulse_i <- 91:100 
    cardiac_rhythm_i <- rep(c(0, 1), each = 5) 
    rr_i <- 91:100 
    spo2_i <- 91:100  
    time_to_hems_i <- 91:100  
    age_i <- 51:60
    medical_facility_i <- rep(c(0, 1), each = 5) 
    vehicle_ground_unit_i <- rep(c(0, 1), each = 5) 
    sex_male_i <- rep(c(0, 1), each = 5) 
    code_i <- rep(c("trauma", "other"), each = 5)
    
    df_coeffs_i <- df_coeffs %>% dplyr::slice(i)
    
    
    if (df_coeffs_i$ind_gcs)           gcs_i <- rep(NA, 10)
    if (df_coeffs_i$ind_pulse)         pulse_i <- rep(NA, 10)
    if (df_coeffs_i$ind_rhythm)        cardiac_rhythm_i <- rep(NA, 10)
    if (df_coeffs_i$ind_rr)            rr_i <- rep(NA, 10)
    if (df_coeffs_i$ind_spo2)          spo2_i <- rep(NA, 10)
    if (df_coeffs_i$ind_time_to_hems)  time_to_hems_i <- rep(NA, 10)
    
    
    risk_function <- calculate_champ(
      rr = rr_i, pulse = pulse_i, spo2 = spo2_i, gcs = gcs_i, 
      time_to_hems = time_to_hems_i, cardiac_rhythm = cardiac_rhythm_i, age = age_i, 
      medical_facility = medical_facility_i, vehicle_ground_unit = vehicle_ground_unit_i, 
      sex_male = sex_male_i, code = code_i)
    
    risk_manual <- vapply(1:10, function(j) manual_champ_calc(
      df_coeffs_i = df_coeffs_i,
      rr = rr_i[j], pulse = pulse_i[j], spo2 = spo2_i[j], gcs = gcs_i[j], 
      time_to_hems = time_to_hems_i[j], cardiac_rhythm = cardiac_rhythm_i[j], age = age_i[j], 
      medical_facility = medical_facility_i[j], vehicle_ground_unit = vehicle_ground_unit_i[j], 
      sex_male = sex_male_i[j], code = code_i[j]), numeric(1))
    
    expect_equal(risk_function, risk_manual)
  })
}


# tests for bad input -----------------------------------------------------

test_that("bad inputs stop calculation", {
  expect_error(calculate_champ(
    rr = 100, pulse = 100, spo2 = 100, gcs = 15, 
    time_to_hems = 100, cardiac_rhythm = 10, age = 40, 
    medical_facility = 1, vehicle_ground_unit = 0, 
    sex_male = 0, code = "trauma"))
  
  expect_error(calculate_champ(
    rr = 100, pulse = 100, spo2 = 100, gcs = 15, 
    time_to_hems = 100, cardiac_rhythm = 1, age = 40, 
    medical_facility = 1, vehicle_ground_unit = 10, 
    sex_male = 0, code = "trauma"))
  
  expect_error(calculate_champ(
    rr = 100, pulse = 100, spo2 = 100, gcs = 15, 
    time_to_hems = 100, cardiac_rhythm = 1, age = 40, 
    medical_facility = 10, vehicle_ground_unit = 1, 
    sex_male = 0, code = "trauma"))
  
  expect_error(calculate_champ(
    rr = 100, pulse = 100, spo2 = 100, gcs = 15, 
    time_to_hems = 100, cardiac_rhythm = 1, age = 40, 
    medical_facility = 1, vehicle_ground_unit = 1, 
    sex_male = 10, code = "trauma"))
  
  expect_error(calculate_champ(
    rr = 400, pulse = 100, spo2 = 100, gcs = 15, 
    time_to_hems = 100, cardiac_rhythm = 1, age = 40, 
    medical_facility = 1, vehicle_ground_unit = 1, 
    sex_male = 1, code = "trauma"))

  expect_error(calculate_champ(
    rr = 100, pulse = 400, spo2 = 100, gcs = 15, 
    time_to_hems = 100, cardiac_rhythm = 1, age = 40, 
    medical_facility = 1, vehicle_ground_unit = 1, 
    sex_male = 1, code = "trauma"))


  expect_error(calculate_champ(
    rr = 100, pulse = 100, spo2 = 400, gcs = 15, 
    time_to_hems = 100, cardiac_rhythm = 1, age = 40, 
    medical_facility = 1, vehicle_ground_unit = 1, 
    sex_male = 1, code = "trauma"))

  expect_error(calculate_champ(
    rr = 100, pulse = 100, spo2 = 100, gcs = 40, 
    time_to_hems = 100, cardiac_rhythm = 1, age = 40, 
    medical_facility = 1, vehicle_ground_unit = 1, 
    sex_male = 1, code = "trauma"))

  expect_error(calculate_champ(
    rr = 100, pulse = 100, spo2 = 100, gcs = 15, 
    time_to_hems = 400, cardiac_rhythm = 1, age = 40, 
    medical_facility = 1, vehicle_ground_unit = 1, 
    sex_male = 1, code = "trauma"))

  expect_error(calculate_champ(
    rr = 100, pulse = 100, spo2 = 100, gcs = 15, 
    time_to_hems = 100, cardiac_rhythm = 1, age = 160, 
    medical_facility = 1, vehicle_ground_unit = 1, 
    sex_male = 1, code = "trauma"))

  expect_error(calculate_champ(
    rr = 100, pulse = 100, spo2 = 400, gcs = 15, 
    time_to_hems = 100, cardiac_rhythm = 1, age = 40, 
    medical_facility = 1, vehicle_ground_unit = 1, 
    sex_male = 1, code = "trauma"))

  expect_error(calculate_champ(
    rr = 100, pulse = 100, spo2 = 100, gcs = 40, 
    time_to_hems = 100, cardiac_rhythm = 1, age = 40, 
    medical_facility = 1, vehicle_ground_unit = 1, 
    sex_male = 1, code = "trauma"))

  expect_error(calculate_champ(
    rr = 100, pulse = 100, spo2 = 100, gcs = 15, 
    time_to_hems = 400, cardiac_rhythm = 1, age = 40, 
    medical_facility = 1, vehicle_ground_unit = 1, 
    sex_male = 1, code = "trauma"))

  expect_error(calculate_champ(
    rr = 100, pulse = 100, spo2 = 100, gcs = 15, 
    time_to_hems = 100, cardiac_rhythm = 1, age = 160, 
    medical_facility = 1, vehicle_ground_unit = 1, 
    sex_male = 1, code = "trauma"))

  expect_error(calculate_champ(
    rr = -1, pulse = 100, spo2 = 100, gcs = 15, 
    time_to_hems = 100, cardiac_rhythm = 1, age = 18, 
    medical_facility = 1, vehicle_ground_unit = 1, 
    sex_male = 1, code = "trauma"))

  expect_error(calculate_champ(
    rr = 100, pulse = -1, spo2 = 100, gcs = 15, 
    time_to_hems = 100, cardiac_rhythm = 1, age = 18, 
    medical_facility = 1, vehicle_ground_unit = 1, 
    sex_male = 1, code = "trauma"))
  
  expect_error(calculate_champ(
    rr = 100, pulse = 100, spo2 = -1, gcs = 15, 
    time_to_hems = 100, cardiac_rhythm = 1, age = 18, 
    medical_facility = 1, vehicle_ground_unit = 1, 
    sex_male = 1, code = "trauma"))
  
  expect_error(calculate_champ(
    rr = 100, pulse = 100, spo2 = 100, gcs = 2, 
    time_to_hems = 100, cardiac_rhythm = 1, age = 18, 
    medical_facility = 1, vehicle_ground_unit = 1, 
    sex_male = 1, code = "trauma"))
  
  expect_error(calculate_champ(
    rr = 100, pulse = 100, spo2 = 100, gcs = 15, 
    time_to_hems = -1, cardiac_rhythm = 1, age = 18, 
    medical_facility = 1, vehicle_ground_unit = 1, 
    sex_male = 1, code = "trauma"))
  
  expect_error(calculate_champ(
    rr = 100, pulse = 100, spo2 = 100, gcs = 15, 
    time_to_hems = 10, cardiac_rhythm = 1, age = 4, 
    medical_facility = 1, vehicle_ground_unit = 1, 
    sex_male = 1, code = "trauma"))

})
  

test_that("returns NA when expected", {
  
  expect_true(calculate_champ(
    rr = 100, pulse = 100, spo2 = 100, gcs = 15, 
    time_to_hems = 100, cardiac_rhythm = 1, age = NA, 
    medical_facility = 1, vehicle_ground_unit = 1, 
    sex_male = 1, code = "trauma") %>% is.na())
  
  expect_true(calculate_champ(
    rr = 100, pulse = 100, spo2 = 100, gcs = 15, 
    time_to_hems = 100, cardiac_rhythm = 1, age = 40, 
    medical_facility = NA, vehicle_ground_unit = 1, 
    sex_male = 1, code = "trauma") %>% is.na())

    expect_true(calculate_champ(
    rr = 100, pulse = 100, spo2 = 100, gcs = 15, 
    time_to_hems = 100, cardiac_rhythm = 1, age = 40, 
    medical_facility = 0, vehicle_ground_unit = NA, 
    sex_male = 0, code = "trauma") %>% is.na())
  
  expect_true(calculate_champ(
    rr = 100, pulse = 100, spo2 = 100, gcs = 15, 
    time_to_hems = 100, cardiac_rhythm = 1, age = 40, 
    medical_facility = 0, vehicle_ground_unit = 1, 
    sex_male = NA, code = "trauma") %>% is.na())

  expect_true(calculate_champ(
    rr = 100, pulse = 100, spo2 = 100, gcs = 15, 
    time_to_hems = 100, cardiac_rhythm = 1, age = 40, 
    medical_facility = 1, vehicle_ground_unit = 1, 
    sex_male = 1, code = NA) %>% is.na())
  
})

# test winsorization ------------------------------------------------------

  
df_for_win <- tibble::tribble(
  ~x,               ~val,         ~val_winsorized,
  "pulse",          10,       25,                
  "rr",             10,       51,                
  "spo2",           10,       50,                
  "time_to_hems",   0,        6,                 
  
  "pulse",          300,       200,
  "rr",             300,       235,
  "time_to_hems",   180,       126,
  )
  
df_coeffs_i <- df_coeffs %>% dplyr::slice(1)

for (i in 1:nrow(df_for_win) ) {
  
  test_that(paste("check winsorization toggle works", i, "/", nrow(df_for_win)), {
    
    df_for_win_i <- df_for_win %>% dplyr::slice(i)
    
    gcs_i <- 15 
    pulse_i <- 100 
    cardiac_rhythm_i <- 1 
    rr_i <- 100
    spo2_i <- 100 
    time_to_hems_i <- 100 
    age_i <- 20
    medical_facility_i <- 0 
    vehicle_ground_unit_i <- 1 
    sex_male_i <- 1 
    code_i <- "trauma"

    
    # set the values beyond the limits
    if (df_for_win_i$x == "pulse")        pulse_i <- df_for_win_i$val
    if (df_for_win_i$x == "rr")           rr_i <- df_for_win_i$val
    if (df_for_win_i$x == "spo2")         spo2_i <- df_for_win_i$val
    if (df_for_win_i$x == "time_to_hems") time_to_hems_i <- df_for_win_i$val
    
    
    risk_function <- calculate_champ(
      rr = rr_i, pulse = pulse_i, spo2 = spo2_i, gcs = gcs_i, 
      time_to_hems = time_to_hems_i, cardiac_rhythm = cardiac_rhythm_i, age = age_i, 
      medical_facility = medical_facility_i, vehicle_ground_unit = vehicle_ground_unit_i, 
      sex_male = sex_male_i, code = code_i, limit_values = FALSE)
    
    risk_function_win <- calculate_champ(
      rr = rr_i, pulse = pulse_i, spo2 = spo2_i, gcs = gcs_i, 
      time_to_hems = time_to_hems_i, cardiac_rhythm = cardiac_rhythm_i, age = age_i, 
      medical_facility = medical_facility_i, vehicle_ground_unit = vehicle_ground_unit_i, 
      sex_male = sex_male_i, code = code_i, limit_values = TRUE)
    
    
    risk_manual <- manual_champ_calc(
      df_coeffs_i = df_coeffs_i,
      rr = rr_i, pulse = pulse_i, spo2 = spo2_i, gcs = gcs_i, 
      time_to_hems = time_to_hems_i, cardiac_rhythm = cardiac_rhythm_i, age = age_i, 
      medical_facility = medical_facility_i, vehicle_ground_unit = vehicle_ground_unit_i, 
      sex_male = sex_male_i, code = code_i)
    
    # limit the values for the manual function
    if (df_for_win_i$x == "pulse")        pulse_i <- df_for_win_i$val_winsorized
    if (df_for_win_i$x == "rr")           rr_i <- df_for_win_i$val_winsorized
    if (df_for_win_i$x == "spo2")         spo2_i <- df_for_win_i$val_winsorized
    if (df_for_win_i$x == "time_to_hems") time_to_hems_i <- df_for_win_i$val_winsorized
    
    risk_manual_win <- manual_champ_calc(
      df_coeffs_i = df_coeffs_i,
      rr = rr_i, pulse = pulse_i, spo2 = spo2_i, gcs = gcs_i, 
      time_to_hems = time_to_hems_i, cardiac_rhythm = cardiac_rhythm_i, age = age_i, 
      medical_facility = medical_facility_i, vehicle_ground_unit = vehicle_ground_unit_i, 
      sex_male = sex_male_i, code = code_i)
    
    expect_equal(risk_function_win, risk_manual_win)
    expect_true(risk_function_win != risk_function)
    expect_true(risk_function_win != risk_manual)
  
})

}
