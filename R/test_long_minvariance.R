
#' Test longitudinal measurement invariance
#'
#' @details This function automatically runs 4 automatically generated measurement invariance models.
#' This may not be appropriate and it might be more useful to generate lavaan syntax using long_minvariance_syntax() function for each model separately and run confirmatory factor analyses using lavaan::cfa()
#'
#' @param data Data in wide format (one row for each participant, one column for each item at each time point)
#' @param var_list List specifying the variable names for each time point. The number of variable names need to be identical at each time point (same measure).
#' @param add String, lavaan syntax to be added to the model
#' @param remove List, remove specific parts of the automatically generated model
#' @param measure_name String, name of the measure
#' @param time_str String, prefix of the point specifier
#' @param item_str String, prefix of the item specifier
#' @param details Return details of selected output selected in return argument
#' @param return What should be returned ("fit_statistics", "model_tests", "lavaan_objects", "lavaan_syntax", "all")
#'
#' @return
#' @export
#'
minvariance <- function(data, var_list, add = NULL, remove = NULL, details = FALSE, return = c("fit_statistics", "model_tests", "lavaan_objects", "lavaan_syntax", "all")) {

  return <- match.arg(return)


  configural_syntax <- suppressMessages(long_minvariance_syntax(var_list = var_list, add = add, remove = remove, model = "configural"))
  weak_syntax <- suppressMessages(long_minvariance_syntax(var_list = var_list, add = add, remove = remove, model = "weak"))
  strong_syntax <- suppressMessages(long_minvariance_syntax(var_list = var_list, add = add, remove = remove, model = "strong"))
  strict_syntax <- suppressMessages(long_minvariance_syntax(var_list = var_list, add = add, remove = remove, model = "strict"))

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


  output <- list()
  output$fit_statistics <- fit_statistics
  output$model_tests <- model_tests
  output$lavaan_objects <- lavaan_objects
  output$lavaan_syntax <- lavaan_syntax


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

  if (return == "all") {

    return(output)

  }

}
