## code to prepare `var_names` dataset goes here
var_names <- tibble::tribble(
  ~data,                                        ~func,
  "Age (years)",                               "age",
  "Heart rate (bpm)",                          "pulse",
  "Systolic blood pressure (mmHg)",            "rr",
  "Oxygen saturation (%)",                     "spo2",
  "Time to HEMS arrival (minutes)",            "time_to_hems",
  "Glasgow Coma Scale",                        "gcs",
  "Patient sex",                               "sex_male",
  "Cardiac rhythm",                            "cardiac_rhythm",
  "Medical facility or nursing home",          "medical_facility",
  "HEMS vehicle",                              "vehicle_ground_unit",
  "Patient group",                             "code",
)

usethis::use_data(var_names, overwrite = TRUE)
