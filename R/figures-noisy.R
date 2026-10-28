# Pairs of figures, one pair per kind of noisy data: the data matrix an analyst
# observes, and the low-rank structure estimated from it. Styled like
# make_concentration_figure() in R/figures-matrices.R.

noisy_types <- c("gaussian", "missing", "ard", "ego")

# a dcsbm with 3 blocks, the same population as the concentration figure
simulate_noisy_population <- function(n = 50, k = 3) {
  B <- matrix(0.075, k, k)
  diag(B) <- 0.8

  model <- dcsbm(
    theta = runif(n, min = 1, max = 5),
    B = B,
    expected_density = 0.25,
    sort_nodes = TRUE,
    poisson_edges = FALSE,
    allow_self_loops = FALSE
  )

  list(
    A = as.matrix(sample_sparse(model)),
    P = as.matrix(expectation(model)),
    k = k
  )
}

# returns the observed data matrix (NA = unobserved) and the estimated P
simulate_noisy_data <- function(population, noise_type) {
  A <- population$A
  P <- population$P
  k <- population$k

  switch(
    noise_type,
    gaussian = {
      data <- add_gaussian_noise(A, sigma = 0.2)
      list(data = data, estimate = estimate_P_gaussian(data, k))
    },
    missing = {
      data <- remove_edges(A, missing_rate = 0.1)
      list(data = data, estimate = estimate_P_missing(data, k))
    },
    ego = {
      ego <- ego_sample(A, fraction = 0.5)
      # the data is drawn with the observed (ego) nodes first, then the
      # unobserved ones; the estimate keeps the original node order, like
      # the estimates for the other noise processes
      order <- c(ego$egos, setdiff(seq_len(nrow(A)), ego$egos))
      list(
        data = ego$A_ego[order, order],
        estimate = estimate_P_ego(ego$A_ego, ego$egos, k)
      )
    },
    ard = {
      ard <- aggregated_relational_data(A, P, rank = k, corr = 0.95, num_traits = k)
      list(
        data = list(
          "ARD counts\n(outcomes)" = ard$Y,
          "Trait\ncensus" = ard$W
        ),
        estimate = estimate_P_aggregated(ard$Y, ard$W)
      )
    }
  )
}

plot_deck_matrix <- function(M, palette, limit, title = NULL) {
  long <- tibble(
    row = rep(seq_len(nrow(M)), times = ncol(M)),
    col = rep(seq_len(ncol(M)), each = nrow(M)),
    value = as.vector(M)
  )

  ggplot(long, aes(x = col, y = row, fill = value)) +
    geom_raster() +
    scale_y_reverse() +
    scale_fill_gradient2(
      high = palette[["dark-teal"]],
      low = palette[["red"]],
      mid = lighten_hex(palette[["brighter"]]),
      limits = c(-limit, limit),
      oob = oob_squish,
      na.value = palette[["bright"]] # orange = emphasis: the unobserved entries
    ) +
    coord_fixed(expand = FALSE) +
    theme_deck(palette, base_size = 22, void = TRUE) +
    labs(title = title) +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, size = 10),
      plot.margin = margin(5, 15, 5, 15)
    )
}

# the observed data is a matrix, or a named list of matrices drawn side by side
# (ARD: counts and trait census), each on its own color scale
plot_deck_data <- function(data, palette) {
  if (!is.list(data)) {
    return(plot_deck_matrix(data, palette, max(abs(data), na.rm = TRUE)))
  }

  panels <- imap(data, \(M, title) {
    plot_deck_matrix(M, palette, max(abs(M), na.rm = TRUE), title = title)
  })
  wrap_plots(panels, nrow = 1)
}

# writes figures/noisy/<type>-data.png and <type>-estimate.png; the estimate
# shares its color scale with the data matrix, except for ARD, whose data (counts
# and traits) is not on the scale of P; returns both paths
make_noisy_figures <- function(simulated, noise_type, palette) {
  data <- simulated$data
  first <- if (is.list(data)) data[[1]] else data
  limit <- if (is.list(data)) {
    max(abs(simulated$estimate))
  } else {
    max(abs(data), na.rm = TRUE)
  }

  plots <- list(
    data = plot_deck_data(data, palette),
    estimate = plot_deck_matrix(simulated$estimate, palette, limit)
  )
  widths <- list(
    data = if (is.list(data)) 1.6 * length(data) else 4,
    estimate = 4
  )

  paths <- imap_chr(plots, \(plot, name) {
    path <- glue("figures/noisy/{noise_type}-{name}.png")
    save_deck_plot(plot, path, width = max(widths[[name]], 1), height = 4, dpi = 500)
  })

  unname(paths)
}
