## code to prepare `coeffs` dataset goes here
coeffs <- readr::read_csv("/media/laamit/asteroid/Documents/LSC/2021/finnhemms/score_ei_finnhemms/shiny/coefficients_wide_formatted.csv")
usethis::use_data(coeffs, overwrite = TRUE)
