
#' Test longitudinal measurement invariance
#'
#' @param data Data in wide format (one row for each participant, one column for each item at each time point)
#' @param n_timepoints Number of time points
#' @param n_items Number of items
#' @param add lavaan syntax to add to the model
#' @param remove List, remove specific parts of the automatically generated model
#' @param measure_name String, name of the measure
#' @param time_str String, prefix of the point specifier
#' @param item_str String, prefix of the item specifier
#' @param details Return details of selected output selected in return argument
#' @param return What should be returned ("fit_statistics", "model_tests", "lavaan_objects", "lavaan_syntax")
#'
#' @return
#' @export
#'
long_minvariance <- function(data, n_timepoints, n_items, add = NULL, remove = NULL, measure_name = "x", time_str = "_t", item_str = "_i", details = FALSE, return = c("fit_statistics", "model_tests", "lavaan_objects", "lavaan_syntax")) {

  return <- match.arg(return)

  configural_syntax <- suppressMessages(long_minvariance_syntax(measure_name = measure_name, time_str = time_str, item_str = item_str, n_timepoints = n_timepoints, n_items = n_items, add = add, remove = remove, model = "configural"))
  weak_syntax <- suppressMessages(long_minvariance_syntax(measure_name = measure_name, time_str = time_str, item_str = item_str, n_timepoints = n_timepoints, n_items = n_items, add = add, remove = remove, model = "weak"))
  strong_syntax <- suppressMessages(long_minvariance_syntax(measure_name = measure_name, time_str = time_str, item_str = item_str, n_timepoints = n_timepoints, n_items = n_items, add = add, remove = remove, model = "strong"))
  strict_syntax <- suppressMessages(long_minvariance_syntax(measure_name = measure_name, time_str = time_str, item_str = item_str, n_timepoints = n_timepoints, n_items = n_items, add = add, remove = remove, model = "strict"))

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
