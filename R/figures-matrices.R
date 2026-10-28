dense_to_long_tidy <- function(dense) {
  dense |>
    as.matrix() |>
    as_tibble(rownames = "row") |>
    gather(col, value, -row) |>
    mutate_all(as.numeric)
}

make_concentration_figure <- function(palette) {
  k <- 3

  B <- matrix(0.075, k, k)
  diag(B) <- 0.8

  theta <- runif(50, min = 1, max = 5)

  model <- dcsbm(
    theta = theta,
    B = B,
    expected_density = 0.25,
    sort_nodes = TRUE,
    poisson_edges = FALSE,
    allow_self_loops = FALSE
  )
  P <- expectation(model)
  A <- sample_sparse(model)

  rownames(A) <- rownames(P)
  colnames(A) <- colnames(P)

  s <- irlba(A, k = k)
  Phat <- tcrossprod(s$u, rowScale(s$v, s$d))
  rownames(Phat) <- rownames(P)
  colnames(Phat) <- colnames(P)

  noise <- matrix(
    rnorm(prod(dim(A)), sd = 0.2),
    nrow = nrow(A),
    ncol = ncol(A)
  )

  A_dense <- as.matrix(A)
  A_noise <- A_dense + noise
  rownames(A_noise) <- rownames(P)
  colnames(A_noise) <- colnames(P)

  missing_indicator <- rsparsematrix(
    nrow = nrow(A),
    ncol = ncol(A),
    density = 0.1,
    rand.x = \(x) NA
  )
  A_missing <- A_dense + as.matrix(missing_indicator)

  s_noise <- irlba(A_noise, k = k)
  Phat_noise <- tcrossprod(s_noise$u, rowScale(s_noise$v, s_noise$d))
  rownames(Phat_noise) <- rownames(P)
  colnames(Phat_noise) <- colnames(P)

  idx <- which(!is.na(A_missing), arr.ind = TRUE)
  vals <- A_missing[!is.na(A_missing)]

  # explicit zeroes retained, implicit zeroes denote missing values
  # inefficient but that's fine
  A_missing_sparse <- sparseMatrix(
    i = idx[, 1],
    j = idx[, 2],
    x = vals,
    dims = dim(A_missing),
    giveCsparse = TRUE
  )

  s_missing <- adaptive_impute(A_missing_sparse, rank = k)

  Phat_missing <- tcrossprod(s_missing$u, rowScale(s_missing$v, s_missing$d))
  rownames(Phat_missing) <- rownames(P)
  colnames(Phat_missing) <- colnames(P)

  facet_data_helper <- function(
    X,
    dgp = c("true", "noisy", "missing"),
    matrix = c("observed", "population", "estimate")
  ) {
    dgp <- arg_match(dgp)
    matrix <- arg_match(matrix)

    X <- as.matrix(X)
    tidied <- dense_to_long_tidy(X)
    tidied$dgp <- dgp
    tidied$matrix <- matrix
    tidied
  }

  all_matrices_long <- bind_rows(
    facet_data_helper(A, "true", "observed"),
    facet_data_helper(A_noise, "noisy", "observed"),
    facet_data_helper(A_missing, "missing", "observed"),
    facet_data_helper(P, "true", "population"),
    facet_data_helper(P, "noisy", "population"),
    facet_data_helper(P, "missing", "population"),
    facet_data_helper(Phat, "true", "estimate"),
    facet_data_helper(Phat_noise, "noisy", "estimate"),
    facet_data_helper(Phat_missing, "missing", "estimate"),
  ) |>
    mutate(
      dgp = fct_relevel(as.factor(dgp), "true", "noisy", "missing"),
      matrix = fct_relevel(
        as.factor(matrix),
        "observed",
        "population",
        "estimate"
      )
    )

  all_matrices_plot <- all_matrices_long |>
    ggplot(aes(x = col, y = row, fill = value)) +
    geom_raster() +
    scale_y_reverse() +
    scale_fill_gradient2(
      high = palette[["dark-teal"]],
      low = palette[["red"]],
      mid = palette[["brighter"]] |> lighten_hex(),
      oob = oob_squish,
      na.value = palette[["bright"]] # orange = emphasis: the missing entries
    ) +
    theme_deck(palette, base_size = 22, void = TRUE) +
    theme(
      legend.position = "none",
      strip.text.y.left = element_text(angle = 90, size = 30)
    ) +
    facet_grid(
      rows = vars(matrix),
      cols = vars(dgp),
      switch = "y",
      labeller = labeller(
        dgp = c(
          true = "Exact network",
          noisy = "Noisy network",
          missing = "Partial network"
        ),
        matrix = as_labeller(
          c(
            observed = "Observed~A",
            population = "True~P",
            estimate = "Estimated~hat(P)"
          ),
          default = label_parsed
        )
      )
    )

  save_deck_plot(all_matrices_plot, "figures/all_matrices.png", width = 8, height = 8, dpi = 500)
}

# a near-white version of a hex color, used as the zero point of diverging fills
lighten_hex <- function(hex) {
  colorRampPalette(c(hex, "white"))(10)[9]
}
