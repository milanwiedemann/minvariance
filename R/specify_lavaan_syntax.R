#' Specify lavaan syntax for latent factors
#'
#' @param n_items
#' @param measure_name
#' @param n_timepoints
#' @param model
#'
#' @return
#' @export
#'
#' @examples
specify_latent_factors <- function(n_items, n_timepoints, model = c("configural", "weak", "strong", "strict"), measure_name = "x", time_str = "s", item_str = "i"){

  model <- match.arg(model)
  latent_factors <- ""
  lavaan_str_time <- list()

  latent_factors <- paste0("eta", 1:n_timepoints)

  if (model == "configural") {

    for (time in 1:n_timepoints) {

      paste_str <- paste0(measure_name, "_t", time, "_i", 1:n_items)
      paste_str_from2 <- paste_str[2:n_items]
      paste_str_from2_c <- stringr::str_c(paste_str_from2, collapse = " + ")

      # Pre-multiply it with NA to force this factor loading to be free, see https://lavaan.ugent.be/tutorial/syntax2.html
      lavaan_str_time[[time]] <- paste0(latent_factors[time], " =~ ", "NA * ", paste_str[1], " + ", "lambda1 * ", paste_str[1], " + ", paste_str_from2_c, "\n")

    }

    return(unlist(stringr::str_c(lavaan_str_time, collapse = "")))

  }


  if (model %in% c("weak", "strong", "strict")) {

    for (time in 1:n_timepoints) {

      paste_str <- paste0(measure_name, "_t", time, "_i", 1:n_items)
      paste_str_lambda <- paste0("lambda", 1:n_items, " * ", paste_str)
      paste_str_lambda_c <- stringr::str_c(paste_str_lambda, collapse = " + ")

      lavaan_str_time[[time]] <- paste0(latent_factors[time], " =~ ", "NA * ", paste_str[1], " + ", paste_str_lambda_c, "\n")

    }
    # lavaan_str_time
    return(unlist(stringr::str_c(lavaan_str_time, collapse = "")))

  }


}


#' Title
#'
#' @param n_items
#' @param measure_name
#' @param n_timepoints
#' @param model
#'
#' @return
#' @export
#'
#' @examples
#'
specify_latent_variable_variances <- function(n_items, n_timepoints, model = c("configural", "weak", "strong", "strict"), measure_name = "x", time_str = "s", item_str = "i"){

  model <- match.arg(model)

  latent_factors <- ""
  lavaan_str_time <- list()

  latent_factors <- paste0("eta", 1:n_timepoints)

  # Create empty string
  other_latent_covariances <- ""

  # Count latent variables
  n_latent_variables <- length(latent_factors)

  # Specify latent variances first
  latent_variances_first <- paste0(latent_factors[1], " ~~ ", "1 * ", latent_factors[1], "\n", collapse = "")

  latent_variances_from2 <- paste0(latent_factors[2:n_latent_variables], " ~~ ", latent_factors[2:n_latent_variables], "\n", collapse = "")

  paste0(latent_variances_first, latent_variances_from2, collapse = "")

}


#' Title
#'
#' @param n_items
#' @param measure_name
#' @param n_timepoints
#' @param model
#'
#' @return
#' @export
#'
#' @examples
specify_latent_variable_means <- function(n_items, n_timepoints, model = c("configural", "weak", "strong", "strict"), measure_name = "x", time_str = "s", item_str = "i"){

  model <- match.arg(model)
  lavaan_str <- ""

  if (model %in% c("configural", "weak", "strong", "strict")) {

    # Create empty str object for lavaan syntax
    lavaan_str <- base::paste("eta", 1, " ~ 0 * 1", " \n", sep = "")

    for (i in 2:(n_timepoints)) {
      lavaan_str <- base::paste(lavaan_str, "eta", i, " ~ 1", " \n", sep = "")
    }

    return(lavaan_str)

  }

}

#' Title
#'
#' @param n_items
#' @param n_timepoints
#' @param model
#' @param measure_name
#' @param time_str
#' @param item_str
#'
#' @return
#' @export
#'
#' @examples
specify_latent_variable_covariances <- function(n_items, n_timepoints, model = c("configural", "weak", "strong", "strict"), measure_name = "x", time_str = "s", item_str = "i"){

  model <- match.arg(model)

  latent_factors <- ""
  lavaan_str_time <- list()

  latent_factors <- paste0("eta", 1:n_timepoints)

  # Create empty string
  other_latent_covariances <- ""

  # Count latent variables
  n_latent_variables <- length(latent_factors)

  # Now specify a first set of latent covariances
  first_latent_covariances <- paste0(latent_factors[1], " ~~ ", latent_factors[-1], "\n", collapse = "")

  # If there are more than 2 latent variables loop for the other latent covariances

  if (n_latent_variables == 2) {
    other_latent_covariances <- ""
  }

  if (n_latent_variables > 2) {

    for (index_lvar in 2:(n_latent_variables - 1)) {

      other_latent_covariances <- paste0(other_latent_covariances,
                                         latent_factors[index_lvar], " ~~ ", latent_factors[(index_lvar + 1):n_latent_variables],
                                         "\n", collapse = "")
    }

  }

  paste0(first_latent_covariances, other_latent_covariances, collapse = "")

}



#' Title
#'
#' @param n_items
#' @param measure_name
#' @param n_timepoints
#' @param model
#'
#' @return
#' @export
#'
#' @examples
specify_intercepts <- function(n_items, n_timepoints, model = c("configural", "weak", "strong", "strict"), measure_name = "x", time_str = "s", item_str = "i"){

  model <- match.arg(model)
  lavaan_str <- ""

  if (model %in% c("configural", "weak")) {

    # Create empty str object for lavaan syntax
    lavaan_str <- base::paste(measure_name, "_t", 1:n_timepoints, "_i", 1, " ~ i1 * 1", " \n", sep = "")

    for (i in 2:(n_items)) {
      lavaan_str <- base::paste(lavaan_str, measure_name, "_t", 1:n_timepoints, "_i", i, " ~ 1", " \n", sep = "")
    }

    return(lavaan_str)

  }

  if (model %in% c("strong", "strict")) {

    for (i in 1:(n_items)) {
      lavaan_str <- base::paste(lavaan_str, measure_name, "_t", 1:n_timepoints, "_i", i, " ~ ", "i", i, " * 1", " \n", sep = "")
    }

    return(lavaan_str)

  }
}



#' Title
#'
#' @param n_items
#' @param measure_name
#' @param n_timepoints
#' @param model
#'
#' @return
#' @export
#'
#' @examples
specify_unique_variances <- function(n_items, n_timepoints, model = c("configural", "weak", "strong", "strict"), measure_name = "x", time_str = "s", item_str = "i"){

  model <- match.arg(model)
  lavaan_str <- ""

  if (model %in% c("configural", "weak", "strong")) {

    for (i in 1:(n_items)) {
      lavaan_str <- base::paste(lavaan_str, measure_name, "_t", 1:n_timepoints, "_i", i, " ~~ ", measure_name, "_t", 1:n_timepoints, "_i", i, " \n", sep = "")
    }

    return(paste0(lavaan_str, collapse = ""))

  }

  if (model %in% c("strict")) {

    for (i in 1:(n_items)) {
      lavaan_str <- base::paste(lavaan_str, measure_name, "_t", 1:n_timepoints, "_i", i, " ~~ ", "theta", i, " * ", measure_name, "_t", 1:n_timepoints, "_i", i, " \n", sep = "")
    }

    return(paste0(lavaan_str, collapse = ""))

  }

}



#' Title
#'
#' @param n_items
#' @param measure_name
#' @param n_timepoints
#' @param model
#'
#' @return
#' @export
#'
#' @examples
specify_unique_covariances <- function(n_items, n_timepoints, model = c("configural", "weak", "strong", "strict"), measure_name = "x", time_str = "_t", item_str = "_i"){

  model <- match.arg(model)

  var_session_item <- ""
  lavaan_str_time <- list()

  # Create variable names
  var_session <-  paste0(paste0("x", "_t", rep(c(1:n_timepoints), times = n_items)))
  var_item <- paste0("_i", rep(c(1:n_items), each = n_timepoints))
  var_session_item <- paste0(var_session, var_item)
  var_session_item_list <- split(var_session_item, ceiling(seq_along(var_session_item) / n_timepoints))

  # Create empty string
  first_unique_covariances <- ""
  other_unique_covariances <- ""


  for (item in 1:n_items) {
    # Now specify a first set of unique covariances
    first_unique_covariances[item] <- paste0(var_session_item_list[[item]][1], " ~~ ", var_session_item_list[[item]][-1], "\n", collapse = "")

  }


  # If there are more than 2 unique variables loop for the other unique covariances

  if (n_items == 2) {
    other_unique_covariances <- ""
  }

  if (n_items > 2) {

    for (index_uitem in 2:(n_items - 1)) {

      other_unique_covariances <- paste0(other_unique_covariances,
                                         var_session_item_list[[arsch]][index_uitem], " ~~ ", var_session_item_list[[arsch]][(index_uitem + 1):n_items],
                                         "\n", collapse = "")
    }

  }

  paste0(first_unique_covariances, other_unique_covariances, collapse = "")
}






#' Title
#'
#' @param syntax_heading
#' @param lavaan_str
#'
#' @return
#' @export
#'
#' @examples
tidy_lavaan_str <- function(syntax_heading, lavaan_str) {

  lavaan_str <- paste0(syntax_heading, stringr::str_c(lavaan_str, collapse = ""))

  return(lavaan_str)

}


# Specify longitudinal measurement invariance models ----
#' Title
#'
#' @param n_items
#' @param measure_name
#' @param n_timepoints
#' @param add
#' @param model
#'
#' @return
#' @export
#'
#' @examples
long_minvariance_syntax <- function(n_items, n_timepoints, add = NULL, model = c("configural", "weak", "strong", "strict"), measure_name = "x", time_str = "s", item_str = "i") {

  model <- match.arg(model)

  message_start <- paste0(stringr::str_to_sentence(model), " Invariance Model")

  if(model == "configural") {
    message_labels <- "(Pattern Invariance)\n"
    message_refs <- "References: Thurstone, (1947) and Horn, McArdle, & Mason, (1983)"
  }

  if (model == "weak") {
    message_labels <-"(Metric Invariance, Loading Invariance)\n"
    message_refs <- "Reference: Meredith (1993)"
  }

  if (model == "strong") {
    message_labels <- "(Scalar Invariance, Intercept Invariance)\n"
    message_refs <- "References: Super super reference (YEAR)"
  }

  if (model == "strict") {
    message_labels <- "(Error Variance Invariance, Residual Invariance)\n"
    message_refs <- "References: Super super reference (YEAR)"
  }

  message(paste0(message_start, " ", message_labels, message_refs))

  lavaan_str_return <- ""

  lavaan_str_heading <- paste0("#### ", stringr::str_to_upper(model), " INVARIANCE MODEL ####", "\n")

  lavaan_str_latent_factors <-
    tidy_lavaan_str(syntax_heading = "# Specify Latent Factors ----\n",
                    lavaan_str = specify_latent_factors(measure_name = measure_name, n_timepoints = n_timepoints, n_items = n_items, model = model))

  specify_latent_variable_variances <-
    tidy_lavaan_str(syntax_heading = "# Specify Latent Variable Variances ----\n",
                    lavaan_str = specify_latent_variable_co_variances(measure_name = measure_name, n_timepoints = n_timepoints, n_items = n_items, model = model))


  specify_latent_variable_covariances <-
    tidy_lavaan_str(syntax_heading = "# Specify Latent Covariable Variances ----\n",
                    lavaan_str = specify_latent_variable_co_variances(measure_name = measure_name, n_timepoints = n_timepoints, n_items = n_items, model = model))

  lavaan_str_intercepts <-
    tidy_lavaan_str(syntax_heading = "# Specify Observed Variable Intercepts ----\n",
                    lavaan_str = specify_intercepts(measure_name = measure_name, n_timepoints = n_timepoints, n_items = n_items, model = model))


  lavaan_str_unique_variances <-
    tidy_lavaan_str(syntax_heading = "# Specify Unique Variances ----\n",
                    lavaan_str = specify_unique_variances(measure_name = measure_name, n_timepoints = n_timepoints, n_items = n_items, model = model))


  lavaan_str_latent_variable_means <-
    tidy_lavaan_str(syntax_heading = "# Specify Latent Variable Means ----\n",
                    lavaan_str = specify_latent_variable_means(measure_name = measure_name, n_timepoints = n_timepoints, n_items = n_items, model = model))




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
                                      specify_latent_variable_variances,
                                      specify_latent_variable_covariances,
                                      lavaan_str_intercepts,
                                      lavaan_str_unique_variances,
                                      lavaan_str_latent_variable_means,
                                      add_lavaan_syntax,
                                      collapse = "")
  return(lavaan_str_return)
}
