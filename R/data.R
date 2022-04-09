#' Coefficients for all the models
#'
#' Coefficients for the models used  in the CHAMP algorithm. 
#'
#' @format A tibble with 64 rows and 1370 variables:
#' \describe{
#'   \item{ind_X}{Indicator if variable X is used}
#'   \item{beta_X}{Regression coefficient for X variable}
#'   ...
#' }
"coeffs"

#' Names of the variables
#'
#' Names of the variables used in the CHAMP algorithm. Matches the variable 
#' definition excel to the functions.
#'
#' @format A tibble with 11 rows and 2 variables:
#' \describe{
#'   \item{data}{Name in the data}
#'   \item{func}{Name in the function}
#' }
"var_names"

