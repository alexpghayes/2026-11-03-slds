#' Take network averages as if na.rm = TRUE, kinda
#'
#' NAs propagate partially -- if x is NA, Ax will be NA for that same entry. however, that NA value of x will
#' just be ignored by X's peers, such that x's NA entry does not propagate to all its neighbors
#'
#' @param A sparse adjacency matrix (symmetric)
#' @param x vector (with NA values that should be ignored, as in na.rm = TRUE, except network average edition)
safe_network_average <- function(A, x) {
  na_indices <- which(is.na(x))

  A_safe <- as.matrix(A)
  A_safe[, na_indices] <- 0
  A_safe[na_indices, ] <- 0
  A_safe <- drop0(A_safe)

  G_safe <- rowScale(A_safe, 1 / rowSums(A_safe))

  out <- drop(G_safe %*% x)
  out[na_indices] <- NA
  out
}

safe_network_average_matrix <- function(A, X) {
  apply(X, 2, safe_network_average, A = as.matrix(A))
}
