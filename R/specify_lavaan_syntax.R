
specify_latent_factors <- function(n_items, n_timepoints, model = c("configural", "weak", "strong", "strict"), measure_name = "x", time_str = "_t", item_str = "_i"){

  model <- match.arg(model)
  latent_factors <- ""
  lavaan_str_time <- list()

  latent_factors <- paste0("eta", 1:n_timepoints)

  if (model == "configural") {

    for (time in 1:n_timepoints) {

      paste_str <- paste0(measure_name, time_str, time, item_str, 1:n_items)
      paste_str_from2 <- paste_str[2:n_items]
      paste_str_from2_c <- stringr::str_c(paste_str_from2, collapse = " + ")

      # Pre-multiply it with NA to force this factor loading to be free, see https://lavaan.ugent.be/tutorial/syntax2.html
      lavaan_str_time[[time]] <- paste0(latent_factors[time], " =~ ", "NA * ", paste_str[1], " + ", "lambda1 * ", paste_str[1], " + ", paste_str_from2_c, "\n")

    }

    return(unlist(stringr::str_c(lavaan_str_time, collapse = "")))

  }


  if (model %in% c("weak", "strong", "strict")) {

    for (time in 1:n_timepoints) {

      paste_str <- paste0(measure_name, time_str, time, item_str, 1:n_items)
      paste_str_lambda <- paste0("lambda", 1:n_items, " * ", paste_str)
      paste_str_lambda_c <- stringr::str_c(paste_str_lambda, collapse = " + ")

      lavaan_str_time[[time]] <- paste0(latent_factors[time], " =~ ", "NA * ", paste_str[1], " + ", paste_str_lambda_c, "\n")

    }
    # lavaan_str_time
    return(unlist(stringr::str_c(lavaan_str_time, collapse = "")))

  }


}



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


specify_latent_variable_means <- function(n_items, n_timepoints, model = c("configural", "weak", "strong", "strict"), measure_name = "x", time_str = "_t", item_str = "_i"){

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



specify_latent_variable_covariances <- function(n_items, n_timepoints, model = c("configural", "weak", "strong", "strict"), measure_name = "x", time_str = "s", item_str = "i"){

  model <- match.arg(model)

  if (model %in% c("configural", "weak", "strong", "strict")) {

    # Create variable names
    latent_factors <- list()
    latent_factors <- paste0("eta", 1:n_timepoints)

    # Create empty list for first looop
    covars_list <- list()

    #  get pairs
    covars_unlist <- combn(latent_factors, m = 2, FUN = NULL, simplify = FALSE)

    # Create empty list for second looop
    covars_list_lavaan <- list()

    for (icovar in seq_along(covars_unlist)) {

      covars_list_lavaan[[icovar]] <- paste0(covars_unlist[[icovar]][1], " ~~ ", covars_unlist[[icovar]][2], "\n")

    }
  }

  return(paste0(covars_list_lavaan, collapse = ""))

}




specify_observed_intercepts <- function(n_items, n_timepoints, model = c("configural", "weak", "strong", "strict"), measure_name = "x", time_str = "_t", item_str = "_i"){

  model <- match.arg(model)
  lavaan_str <- ""

  if (model %in% c("configural", "weak")) {

    # Create empty str object for lavaan syntax
    lavaan_str <- base::paste(measure_name, time_str, 1:n_timepoints, item_str, 1, " ~ tau1 * 1", " \n", sep = "")

    for (i in 2:(n_items)) {
      lavaan_str <- base::paste(lavaan_str, measure_name, time_str, 1:n_timepoints, item_str, i, " ~ 1", " \n", sep = "")
    }

    return(paste0(lavaan_str, collapse = ""))

  }

  if (model %in% c("strong", "strict")) {

    for (i in 1:(n_items)) {
      lavaan_str <- base::paste(lavaan_str, measure_name, time_str, 1:n_timepoints, item_str, i, " ~ ", "tau", i, " * 1", " \n", sep = "")
    }

    return(paste0(lavaan_str, collapse = ""))

  }
}


specify_unique_variances <- function(n_items, n_timepoints, model = c("configural", "weak", "strong", "strict"), measure_name = "x", time_str = "_t", item_str = "_i"){

  model <- match.arg(model)
  lavaan_str <- ""

  if (model %in% c("configural", "weak", "strong")) {

    for (i in 1:(n_items)) {
      lavaan_str <- base::paste(lavaan_str, measure_name, time_str, 1:n_timepoints, item_str, i, " ~~ ", measure_name, time_str, 1:n_timepoints, item_str, i, " \n", sep = "")
    }

    return(paste0(lavaan_str, collapse = ""))

  }

  if (model %in% c("strict")) {

    for (i in 1:(n_items)) {
      lavaan_str <- base::paste(lavaan_str, measure_name, time_str, 1:n_timepoints, item_str, i, " ~~ ", "theta", i, " * ", measure_name, time_str, 1:n_timepoints, item_str, i, " \n", sep = "")
    }

    return(paste0(lavaan_str, collapse = ""))

  }

}



specify_unique_covariances <- function(n_items, n_timepoints, model = c("configural", "weak", "strong", "strict"), measure_name = "x", time_str = "_t", item_str = "_i"){

  model <- match.arg(model)

  if (model %in% c("configural", "weak", "strong", "strict")) {

  # Create variable names
  var_session <-  paste0(paste0(measure_name, time_str, rep(c(1:n_timepoints), times = n_items)))
  var_item <- paste0(item_str, rep(c(1:n_items), each = n_timepoints))
  var_session_item <- paste0(var_session, var_item)
  var_list_item <- split(var_session_item, ceiling(seq_along(var_session_item) / n_timepoints))

  # Create empty list for first looop
  covars_list <- list()

  # First loop to get pairs
  for (item in 1:n_items) {
    covars_list[[item]] <- combn(var_list_item[[item]], m = 2, FUN = NULL, simplify = FALSE)
  }

  # Unlist to make next step easier
  covars_unlist <-  unlist(covars_list, recursive = FALSE)

  # Create empty list for second looop
  covars_list_lavaan <- list()

  for (icovar in seq_along(covars_unlist)) {

    covars_list_lavaan[[icovar]] <- paste0(covars_unlist[[icovar]][1], " ~~ ", covars_unlist[[icovar]][2], "\n")

  }
  }

  return(paste0(covars_list_lavaan, collapse = ""))

}


tidy_lavaan_str <- function(syntax_heading, lavaan_str) {

  lavaan_str <- paste0(syntax_heading, stringr::str_c(lavaan_str, collapse = ""))

  return(lavaan_str)

}

