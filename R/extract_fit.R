#' Extract fit statistics
#'
#' @param ...
#' @param details
#'
#' @return
#' @export
#'
#' @examples
extract_fit <- function(..., details = FALSE) {

  lavaan_objects <- list(...)

  fit_data <- purrr::map_df(.x = lavaan_objects, .f = broom::glance, .id = "model")

  fit_data <- fit_data %>%
    rename_all(tolower)

  # Figure out a way to name these things
  # fit_return$model <- paste0("m", 1:length(lavaan_objects))

  if (details == FALSE) {
    fit_return <- dplyr::select(fit_data,
                                model, chisq, npar, aic, bic, cfi, rmsea, srmr, tli, converged)
  }

  if (details == TRUE) {
    fit_return <- dplyr::select(fit_data,
                                model, chisq, npar, dplyr::everything())
  }

  return(fit_return)

}


