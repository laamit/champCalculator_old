## code to prepare `var_names` dataset goes here
var_names <- tibble::tribble(
  ~data,                                        ~func,
  "Age (years)",                               "age",
  "Heart rate (bpm)",                          "pulse",
  "Systolic blood pressure (mmHg)",            "rr",
  "Oxygen saturation (%)",                     "spo2",
  "Time to HEMS arrival (minutes)",            "time_to_hems",
  "Glasgow Coma Scale",                        "gcs",
  "Patient sex",                               "sex",
  "Cardiac rhythm",                            "cardiac_rhythm",
  "Medical facility or nursing home",          "med_facility",
  "HEMS vehicle",                              "vehicle",
  "Patient group",                             "code",
)

usethis::use_data(var_names, overwrite = TRUE)
