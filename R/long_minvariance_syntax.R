

#' Specify longitudinal measurement invariance models
#'
#' @param n_items
#' @param n_timepoints
#' @param add
#' @param remove
#' @param model
#' @param measure_name
#' @param time_str
#' @param item_str
#'
#' @return
#' @export
long_minvariance_syntax <- function(var_list, add = NULL, remove = NULL, model = c("configural", "weak", "strong", "strict"), measure_name = "x", time_str = "_t", item_str = "_i") {

  model <- match.arg(model)

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

  lavaan_str_return <- stringr::str_c(lavaan_str_heading,
                                      lavaan_str_latent_factors,
                                      lavaan_str_latent_variable_means,
                                      specify_latent_variable_variances,
                                      specify_latent_variable_covariances,
                                      lavaan_str_observed_intercepts,
                                      lavaan_str_unique_variances,
                                      lavaan_str_unique_covariances,
                                      add_lavaan_syntax,
                                      collapse = "")
  return(lavaan_str_return)
}




