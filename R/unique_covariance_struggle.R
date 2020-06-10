# # Unique covariance struggle
# # Works fine if n time and n items are the same
# # if not the same there are nas ...
# n_timepoints <- 5
# n_items <- 3
#
# var_session <-  paste0(paste0("x", "_s", rep(c(1:n_timepoints), times = n_items)))
# var_session
#
# var_item <- paste0("_i", rep(c(1:n_items), each = n_timepoints))
# var_item
#
# var_session_item <- paste0(var_session, var_item)
# var_session_item
#
# var_list_item <- split(var_session_item, ceiling(seq_along(var_session_item) / n_timepoints))
# var_list_item
# first_unique_covariances <- list()
# other_unique_covariances_item <- list()
# other_unique_covariances_item2 <- list()
# count_loop <- 0
#
# for (item in 1:n_items) {
#   # Now specify a first set of unique covariances
#   first_unique_covariances[item] <- paste0(var_list_item[[item]][1], " ~~ ", var_list_item[[item]][-1], "\n", collapse = "")
#
#   if (n_items > 2 & n_timepoints > 2) {
#
#     for (time in 2:(n_timepoints - 1)) {
#
#       count_loop <- count_loop + 1
#
#       # need this storage for each item and each time,
#       other_unique_covariances_item[item] <- paste0(var_list_item[[item]][time], " ~~ ", var_list_item[[item]][(time + 1):n_items],
#                                                     "\n", collapse = "")
#       # save from each loop
#       other_unique_covariances_item2[count_loop] <- other_unique_covariances_item[item]
#     }
#   }
# }
#
# first_unique_covariances %>% unlist() %>% paste0(collapse = "") %>% cat()
# other_unique_covariances_item %>% unlist() %>% paste0(collapse = "") %>% cat()
# other_unique_covariances_item2 %>% unlist() %>% paste0(collapse = "") %>% cat()
#
# # mapply(c, first_unique_covariances, other_unique_covariances, SIMPLIFY = FALSE) %>% unlist() %>% paste0(collapse = "") %>% cat()
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# specify_unique_covariances <- function(n_items, n_timepoints, model = c("configural", "weak", "strong", "strict"), measure_name = "x", time_str = "s", item_str = "i"){
#
#   model <- match.arg(model)
#
#   latent_factors <- ""
#   lavaan_str_time <- list()
#
#   latent_factors <- paste0("eta", 1:n_timepoints)
#
#   # Create empty string
#   other_latent_covariances <- ""
#
#   # Count latent variables
#   n_latent_variables <- length(latent_factors)
#
#   # Now specify a first set of latent covariances
#   first_latent_covariances <- paste0(latent_factors[1], " ~~ ", latent_factors[-1], "\n", collapse = "")
#
#   # If there are more than 2 latent variables loop for the other latent covariances
#
#   if (n_latent_variables == 2) {
#     other_latent_covariances <- ""
#   }
#
#   if (n_latent_variables > 2) {
#
#     for (index_lvar in 2:(n_latent_variables - 1)) {
#
#       other_latent_covariances <- paste0(other_latent_covariances,
#                                          latent_factors[index_lvar], " ~~ ", latent_factors[(index_lvar + 1):n_latent_variables],
#                                          "\n", collapse = "")
#     }
#
#   }
#
#   paste0(first_latent_covariances, other_latent_covariances, collapse = "")
#
# }
#
# specify_unique_covariances(n_items = 3, n_timepoints = 5, model = "weak") %>% cat()
