#' Title
#'
#' @param data
#' @param n_timepoints
#' @param n_items
#' @param measure_name
#' @param details
#' @param return
#'
#' @return
#' @export
#'
#' @examples
long_minvariance <- function(data, n_timepoints, n_items, add = NULL, measure_name = "x", details = FALSE, return = c("fit_statistics", "model_tests", "lavaan_objects", "lavaan_syntax")) {

  return <- match.arg(return)

  configural_syntax <- suppressMessages(long_minvariance_syntax(measure_name = measure_name, n_timepoints = n_timepoints, n_items = n_items, add = add, model = "configural"))
  weak_syntax <- suppressMessages(long_minvariance_syntax(measure_name = measure_name, n_timepoints = n_timepoints, n_items = n_items, add = add, model = "weak"))
  strong_syntax <- suppressMessages(long_minvariance_syntax(measure_name = measure_name, n_timepoints = n_timepoints, n_items = n_items, add = add, model = "strong"))
  strict_syntax <- suppressMessages(long_minvariance_syntax(measure_name = measure_name, n_timepoints = n_timepoints, n_items = n_items, add = add, model = "strict"))

  configural <- cfa(model = configural_syntax, data = data, mimic = "mplus")
  weak <- cfa(model = weak_syntax, data = data, mimic = "mplus")
  strong <- cfa(model = strong_syntax, data = data, mimic = "mplus")
  strict <- cfa(model = strict_syntax, data = data, mimic = "mplus")

  # Get all output ready

  # lavaan
  lavaan_syntax <- list()

  lavaan_syntax$configural <- configural_syntax
  lavaan_syntax$weak <- weak_syntax
  lavaan_syntax$strong <- strong_syntax
  lavaan_syntax$strict <- strict_syntax


  lavaan_objects <- list()

  lavaan_objects$configural <- configural
  lavaan_objects$weak <- weak
  lavaan_objects$strong <- strong
  lavaan_objects$strict <- strict


  # fits
  fit_statistics <- extract_fit(configural, weak, strong, strict, details = details) %>%
    mutate(model = case_when(model == "1" ~ "configural",
                             model == "2" ~ "weak",
                             model == "3" ~ "strong",
                             model == "4" ~ "strict"))

  # model tests
  model_tests <- lavaan::anova(configural, weak, strong, strict)


  if (return == "fit_statistics") {

    return(fit_statistics)

  }


  if (return == "model_tests") {

    return(model_tests)

  }

  if (return == "lavaan_objects") {

    return(lavaan_objects)

  }

  if (return == "lavaan_syntax") {

    return(lavaan_syntax)

  }

}
