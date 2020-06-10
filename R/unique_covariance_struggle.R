# Unique covariance struggle
# Works fine if n time and n items are the same
# if not the same there are nas ...
n_timepoints <- 4
n_items <- 4

var_session <-  paste0(paste0("x", "_s", rep(c(1:n_timepoints), times = n_items)))
var_session

var_item <- paste0("_i", rep(c(1:n_items), each = n_timepoints))
var_item

var_session_item <- paste0(var_session, var_item)
var_session_item

var_list_item <- split(var_session_item, ceiling(seq_along(var_session_item) / n_timepoints))
var_list_item
first_unique_covariances <- list()
other_unique_covariances_item <- list()
other_unique_covariances_item2 <- list()
count_loop <- 0

for (item in 1:n_items) {
  # Now specify a first set of unique covariances
  first_unique_covariances[item] <- paste0(var_list_item[[item]][1], " ~~ ", var_list_item[[item]][-1], "\n", collapse = "")

  if (n_items > 2 & n_timepoints > 2) {

    for (time in 2:(n_timepoints - 1)) {

      count_loop <- count_loop + 1

      # need this storage for each item and each time,
      other_unique_covariances_item[item] <- paste0(var_list_item[[item]][time], " ~~ ", var_list_item[[item]][(time + 1):n_items],
                                                    "\n", collapse = "")
      # save from each loop
      other_unique_covariances_item2[count_loop] <- other_unique_covariances_item[item]
    }
  }
}

first_unique_covariances %>% unlist() %>% paste0(collapse = "") %>% cat()
other_unique_covariances_item %>% unlist() %>% paste0(collapse = "") %>% cat()
other_unique_covariances_item2 %>% unlist() %>% paste0(collapse = "") %>% cat()

mapply(c, first_unique_covariances, other_unique_covariances, SIMPLIFY = FALSE) %>% unlist() %>% paste0(collapse = "") %>% cat()
