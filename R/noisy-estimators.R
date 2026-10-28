# Low-rank estimators of the population matrix P, one per kind of noisy data.

low_rank_from_svd <- function(s, rank) {
  s$u[, seq_len(rank), drop = FALSE] %*%
    (s$d[seq_len(rank)] * t(s$v[, seq_len(rank), drop = FALSE]))
}

# mean-zero noise: truncated SVD of the observed matrix
estimate_P_gaussian <- function(A_noisy, rank) {
  low_rank_from_svd(irlba(A_noisy, nv = rank, nu = rank), rank)
}

# missing edges: adaptive impute on the observed entries only. Implicit zeroes
# of the sparse matrix denote missing values, explicit zeroes denote non-edges
estimate_P_missing <- function(A_missing, rank) {
  obs <- which(!is.na(A_missing), arr.ind = TRUE)

  A_sparse <- sparseMatrix(
    i = obs[, 1],
    j = obs[, 2],
    x = A_missing[obs],
    dims = dim(A_missing)
  )

  mf <- adaptive_impute(A_sparse, rank = rank, max_iter = 75, check_interval = 10)
  tcrossprod(mf$u, rowScale(mf$v, mf$d))
}

#' Estimate P from an ego-sampled partial network (Chan & Li 2023, Algorithm 1
#' plus full matrix recovery), "Fitting Low-rank Models on Egocentrically
#' Sampled Partial Networks".
#'
#' @param A_ego matrix with the non-ego x non-ego block unobserved (NA)
#' @param egos indices of the ego nodes
estimate_P_ego <- function(A_ego, egos, rank) {
  N <- nrow(A_ego)
  non_egos <- setdiff(seq_len(N), egos)
  n <- length(egos)

  A11 <- A_ego[egos, egos, drop = FALSE]
  A12 <- A_ego[egos, non_egos, drop = FALSE]

  s11 <- irlba(A11, nv = rank, nu = rank)
  P_tilde_11_pinv <- s11$v %*% diag(1 / s11$d, nrow = rank) %*% t(s11$u)

  P_hat_22 <- t(A12) %*% P_tilde_11_pinv %*% A12

  A_obs <- cbind(A11, A12)
  P_tilde_obs <- low_rank_from_svd(irlba(A_obs, nv = rank, nu = rank), rank)

  P_hat_11 <- P_tilde_obs[, seq_len(n), drop = FALSE]
  P_hat_11 <- (P_hat_11 + t(P_hat_11)) / 2
  P_hat_12 <- P_tilde_obs[, (n + 1):ncol(P_tilde_obs), drop = FALSE]

  P_hat_blocked <- rbind(
    cbind(P_hat_11, P_hat_12),
    cbind(t(P_hat_12), P_hat_22)
  )

  reorder <- order(c(egos, non_egos))
  as.matrix(P_hat_blocked[reorder, reorder])
}

# aggregated relational data: regress Y on the observed traits W (in the
# column space of Y) to recover P
estimate_P_aggregated <- function(Y, W) {
  U <- svd(Y)$u

  UtY <- crossprod(U, Y)
  UtW <- crossprod(U, W)

  Sigmatilde <- tcrossprod(UtY, UtW) %*% solve(tcrossprod(UtW))

  U %*% tcrossprod(Sigmatilde, U)
}
