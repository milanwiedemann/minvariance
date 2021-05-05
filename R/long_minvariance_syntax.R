

#' Specify longitudinal measurement invariance of one factor models.
#'
#' @param var_list List specifying the variable names for each time point. The number of variable names need to be identical at each time point (same measure).
#' @param add String, lavaan syntax to be added to the model
#' @param remove List, remove specific parts of the automatically generated model
#' @param model String, specify measurement invariance model: "configural", "weak", "strong", or "strict".
#' @param measure_name String, name of the measure
#'
#' @return
#' @export
long_minvariance_syntax <- function(var_list, add = NULL, remove = NULL, model = c("configural", "weak", "strong", "strict")) {

  model <- match.arg(model)

  # Specify temporary var names to be replaced in other functions by var_list
  measure_name <- "x"
  time_str <-  "_t"
  item_str <-  "_i"

  # Test if equal, if not error
  if( length((unique(lengths(var_list))) == 1L) == FALSE) {
    stop("Same number of items need to be specified at each time point", call. = FALSE)
  }

  # Set time points and number of measures
  n_timepoints <- length(var_list)

  n_items <- unique(lengths(var_list))

  if (is.null(remove$unique_covar) == TRUE) {
    remove$unique_covar <- FALSE
  }

  message_start <- paste0(stringr::str_to_sentence(model), " Invariance Model")

  if (model == "configural") {
    message_labels <- "(Pattern Invariance)"
    # message_refs <- "References: Thurstone, (1947) and Horn, McArdle, & Mason, (1983)"
  }

  if (model == "weak") {
    message_labels <-"(Metric Invariance, Loading Invariance)"
    # message_refs <- "Reference: Meredith (1993)"
  }

  if (model == "strong") {
    message_labels <- "(Scalar Invariance, Intercept Invariance)"
    # message_refs <- "References: Super super reference (YEAR)"
  }

  if (model == "strict") {
    message_labels <- "(Error Variance Invariance, Residual Invariance)"
    # message_refs <- "References: Super super reference (YEAR)"
  }

  message(paste0(message_start, " ", message_labels
                 #, message_refs
  ))

  lavaan_str_return <- ""

  lavaan_str_heading <- paste0("#### ", stringr::str_to_upper(model), " INVARIANCE MODEL ####", "\n")

  lavaan_str_latent_factors <-
    tidy_lavaan_str(syntax_heading = "# Specify Latent Factors ----\n",
                    lavaan_str = specify_latent_factors(measure_name = measure_name, n_timepoints = n_timepoints, n_items = n_items, model = model, time_str = time_str, item_str = item_str))


  lavaan_str_latent_variable_means <-
    tidy_lavaan_str(syntax_heading = "# Specify Latent Variable Means ----\n",
                    lavaan_str = specify_latent_variable_means(measure_name = measure_name, n_timepoints = n_timepoints, n_items = n_items, model = model, time_str = time_str, item_str = item_str))

  specify_latent_variable_variances <-
    tidy_lavaan_str(syntax_heading = "# Specify Latent Variable Variances ----\n",
                    lavaan_str = specify_latent_variable_variances(measure_name = measure_name, n_timepoints = n_timepoints, n_items = n_items, model = model, time_str = time_str, item_str = item_str))


  specify_latent_variable_covariances <-
    tidy_lavaan_str(syntax_heading = "# Specify Latent Variable Covariances ----\n",
                    lavaan_str = specify_latent_variable_covariances(measure_name = measure_name, n_timepoints = n_timepoints, n_items = n_items, model = model, time_str = time_str, item_str = item_str))


  lavaan_str_observed_intercepts <-
    tidy_lavaan_str(syntax_heading = "# Specify Observed Variable Intercepts ----\n",
                    lavaan_str = specify_observed_intercepts(measure_name = measure_name, n_timepoints = n_timepoints, n_items = n_items, model = model, time_str = time_str, item_str = item_str))

  lavaan_str_unique_variances <-
    tidy_lavaan_str(syntax_heading = "# Specify Unique Variances ----\n",
                    lavaan_str = specify_unique_variances(measure_name = measure_name, n_timepoints = n_timepoints, n_items = n_items, model = model, time_str = time_str, item_str = item_str))


  if (remove$unique_covar == FALSE) {
    lavaan_str_unique_covariances <-
      tidy_lavaan_str(syntax_heading = "# Specify Unique Covariances ----\n",
                      lavaan_str = specify_unique_covariances(measure_name = measure_name, n_timepoints = n_timepoints, n_items = n_items, model = model, time_str = time_str, item_str = item_str))

  }

  if (remove$unique_covar == TRUE) {
    lavaan_str_unique_covariances <- "# Specify Unique Covariances ----\n# REMOVED\n"
  }

  if (is.null(add) == TRUE) {
    add_lavaan_syntax <- ""
  }

  if (is.null(add) == FALSE) {
    add_lavaan_syntax <-
      tidy_lavaan_str(syntax_heading = "# Additaional Model Specifications ----\n",
                      lavaan_str = add)

  }

  lavaan_str_temp <- stringr::str_c(lavaan_str_heading,
                                      lavaan_str_latent_factors,
                                      lavaan_str_latent_variable_means,
                                      specify_latent_variable_variances,
                                      specify_latent_variable_covariances,
                                      lavaan_str_observed_intercepts,
                                      lavaan_str_unique_variances,
                                      lavaan_str_unique_covariances,
                                      add_lavaan_syntax,
                                      collapse = "")

  # NOW RENAME
  str_replace_pattern <- var_list %>%
    unlist()

  names(str_replace_pattern) <- paste0(measure_name, time_str,  1:n_timepoints) %>%
    as.list() %>%
    purrr::map(~ paste0(.x, item_str, 1:n_items)) %>%
    unlist()

  lavaan_str_return <- str_replace_all(string = lavaan_str_temp,
                  pattern = str_replace_pattern)

  return(lavaan_str_return)

}
