# Ways of corrupting a symmetric adjacency matrix A. Each returns the data an
# analyst would actually see; the matching estimators are in R/noisy-estimators.R.

# symmetric mean-zero Gaussian noise added to every dyad
add_gaussian_noise <- function(A, sigma = 0.1) {
  n <- nrow(A)
  noise <- matrix(rnorm(n * n, mean = 0, sd = sigma), nrow = n, ncol = n)
  noise <- (noise + t(noise)) / 2
  diag(noise) <- 0
  as.matrix(A) + noise
}

# dense matrix with NA for dyads missing completely at random (symmetrically)
remove_edges <- function(A, missing_rate = 0.1) {
  A <- as.matrix(A)
  n <- nrow(A)

  missing_upper <- matrix(runif(n * n) < missing_rate, n, n)
  missing_upper[lower.tri(missing_upper, diag = TRUE)] <- FALSE
  missing <- missing_upper | t(missing_upper)

  A[missing] <- NA
  A
}

#' Ego-centric network sampling
#'
#' Randomly sample a fraction of nodes as "egos" and keep only edges incident
#' to at least one ego. The non-ego x non-ego block is unobserved and returned
#' as NA.
#'
#' @return list with `A_ego` (dense, NA in the unobserved block) and `egos`.
ego_sample <- function(A, fraction = 0.5) {
  A <- as.matrix(A)
  n <- nrow(A)
  egos <- sort(sample.int(n, size = floor(n * fraction)))
  non_egos <- setdiff(seq_len(n), egos)

  A[non_egos, non_egos] <- NA

  list(A_ego = A, egos = egos)
}

#' Generate a vector with specified correlation to y
complement <- function(y, corr = 0.5) {
  x <- rnorm(length(y))
  y.perp <- residuals(lm(x ~ y))
  corr * sd(y.perp) * y + y.perp * sd(y) * sqrt(1 - corr^2)
}

generate_traits <- function(U, corr, num_traits) {
  k <- ncol(U)

  if (length(corr) == 1) {
    corr <- rep(corr, num_traits)
  } else {
    stopifnot(length(corr) == num_traits)
  }

  traits <- matrix(0, nrow = nrow(U), ncol = num_traits)

  for (trait in seq_len(num_traits)) {
    # repeat latent dimensions if num_traits > k
    trait_index <- trait %% k
    if (trait_index == 0) {
      trait_index <- k
    }
    traits[, trait] <- complement(U[, trait_index], corr = corr[trait])
  }

  traits
}

# aggregated relational data: Y[i, t] = number of i's alters with trait t
aggregated_relational_data <- function(A, P, rank, corr, num_traits = 5) {
  s_pop <- svds(P, k = rank)
  X <- s_pop$u %*% diag(sqrt(s_pop$d), nrow = rank)

  traits <- generate_traits(X, corr = corr, num_traits = num_traits)

  list(
    Y = as.matrix(A %*% traits),
    W = traits
  )
}
