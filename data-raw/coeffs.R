## code to prepare `coeffs` dataset goes here
coeffs <- readr::read_csv("../coefficients_wide_formatted.csv")
usethis::use_data(coeffs, overwrite = TRUE)
