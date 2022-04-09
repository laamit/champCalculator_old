## code to prepare `coeffs` dataset goes here
coeffs <- readr::read_csv("data-raw/coefficients_wide_formatted.csv")
## reorder columns
coeffs <- coeffs %>% 
  dplyr::select(str_sort(colnames(coeffs)[1:6]), str_sort(colnames(coeffs)[7:36]))

usethis::use_data(coeffs, overwrite = TRUE)
