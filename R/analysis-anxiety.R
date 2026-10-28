construct_anxiety_graph <- function(node_path, edge_path) {
  node_data <- read_csv(node_path)
  edgelist <- read_csv(edge_path)

  nodes <- node_data |>
    mutate(
      name = as.character(par_id)
    )

  edges <- edgelist |>
    filter(value == 1) |>
    mutate(
      from = as.character(from),
      to = as.character(to)
    ) |>
    select(-value)

  graph <- tbl_graph(nodes = nodes, edges = edges, directed = FALSE)
  graph
}

get_anxiety_estimates <- function(graph) {
  nodes <- graph |>
    activate(nodes) |>
    as_tibble()

  tbl4 <- graph |>
    activate(edges) |>
    filter(time == 4)

  y4 <- tbl4 |>
    activate(nodes) |>
    as_tibble() |>
    pull(pss_t4_round)

  A4 <- as_adjacency_matrix(tbl4)

  nodes <- nodes |>
    mutate(
      # peer_pss_t1 = safe_network_average(A1, y1), # no NAs
      # peer_pss_t2 = safe_network_average(A2, y2), # lots of NAs
      # peer_pss_t3 = safe_network_average(A3, y3), # even more NAs
      peer_pss_t4 = safe_network_average(A4, y4) # etc
    )

  ranks <- 1:40

  s4 <- irlba(A4, max(ranks))
  Xhat_max4 <- s4$u %*% diag(sqrt(s4$d))
  colnames(Xhat_max4) <- paste0("Xhat", 1:max(ranks))

  covariates <- c(
    "outside_support_count_t4",
    "conscientiousness",
    "neuroticism",
    "locus_of_control",
    "extroversion",
    "agreeableness",
    "openness",
    "white",
    "sex",
    "us"
  )

  ols_peer_at_rank_t4 <- function(rank = rank, homophily = TRUE) {
    Xhat <- Xhat_max4[, 1:rank, drop = FALSE]
    Xhat_tbl <- as.data.frame(Xhat) |>
      as_tibble()

    features <- covariates

    if (homophily) {
      features <- c(features, colnames(Xhat_tbl))
    }

    data <- nodes |>
      bind_cols(Xhat_tbl) |>
      select(one_of(features), pss_t4_round, peer_pss_t4)

    fit <- lm(pss_t4_round ~ ., data = data)
    tidy(fit, conf.int = TRUE, conf.level = 0.95) |>
      mutate(
        rank = rank,
        estimator = "ols",
        network = "peer",
        homophily = homophily
      )
  }

  # appears to have some colinearity and numerical things happening at lower ranks
  # not sure why this is or if it's important or not
  ols_latent_at_rank_t4 <- function(rank = rank, homophily = TRUE) {
    Xhat <- Xhat_max4[, 1:rank, drop = FALSE]
    Xhat_tbl <- as.data.frame(Xhat) |>
      as_tibble()

    D <- diag(s4$d[1:rank])

    if (rank == 1) {
      # diag() behaves differently for length 1 and length 1+ inputs, smh
      D <- s4$d[1]
    }
    Phat <- s4$u[, 1:rank, drop = FALSE] %*%
      D %*%
      t(s4$v[, 1:rank, drop = FALSE])

    diag(Phat) <- 0

    features <- covariates

    if (homophily) {
      features <- c(features, colnames(Xhat_tbl))
    }

    data <- nodes |>
      bind_cols(Xhat_tbl) |>
      select(one_of(features), pss_t4_round) |>
      # construct the latent contagion effect, but abuse notation call it the same
      # thing we called the peer contagion effect so we don't have to re-write code
      # that already works for the peer contagione effect
      mutate(
        peer_pss_t4 = safe_network_average(Phat, y4)
      )

    fit <- lm(pss_t4_round ~ ., data = data)
    tidy(fit, conf.int = TRUE, conf.level = 0.95) |>
      mutate(
        rank = rank,
        estimator = "ols",
        network = "latent",
        homophily = homophily
      )
  }

  tsls_peer_at_rank_t4 <- function(rank = rank, homophily = TRUE) {
    Xhat <- Xhat_max4[, 1:rank, drop = FALSE]
    Xhat_tbl <- as.data.frame(Xhat) |>
      as_tibble()

    features <- covariates

    if (homophily) {
      features <- c(features, colnames(Xhat_tbl))
    }

    # t4 has lots of missing data, beware
    data <- nodes |>
      bind_cols(Xhat_tbl) |>
      select(one_of(features), pss_t4_round, peer_pss_t4)

    W4 <- model.matrix.lm(
      pss_t4_round ~ . - peer_pss_t4,
      data = data,
      na.action = "na.pass" # keep, let downstream modeling functions drop NAs
    )

    # don't use intercept as an instrument (sanity check this choice)
    W4 <- W4[, -1]

    G4W4 <- safe_network_average_matrix(A4, W4)
    G4G4W4 <- safe_network_average_matrix(A4, G4W4)

    Z4 <- cbind(W4, G4W4, G4G4W4)
    Z4 <- as.matrix(Z4)

    tsls_t4 <- ivreg(
      pss_t4_round ~ . | Z4,
      data = data
    )

    tidy(tsls_t4, conf.int = TRUE) |>
      mutate(
        rank = rank,
        estimator = "tsls",
        network = "peer",
        homophily = homophily
      )
  }

  # has numerical issues just like the OLS latent function, need to
  # investigate what is happening there
  tsls_latent_at_rank_t4 <- function(rank = rank, homophily = TRUE) {
    Xhat <- Xhat_max4[, 1:rank, drop = FALSE]
    Xhat_tbl <- as.data.frame(Xhat) |>
      as_tibble()

    D <- diag(s4$d[1:rank])

    if (rank == 1) {
      # diag() behaves differently for length 1 and length 1+ inputs, smh
      D <- s4$d[1]
    }

    Phat <- s4$u[, 1:rank, drop = FALSE] %*%
      D %*%
      t(s4$v[, 1:rank, drop = FALSE])

    diag(Phat) <- 0

    features <- covariates

    if (homophily) {
      features <- c(features, colnames(Xhat_tbl))
    }

    # t4 has lots of missing data, beware
    data <- nodes |>
      bind_cols(Xhat_tbl) |>
      select(one_of(features), pss_t4_round) |>
      # construct the latent contagion effect, but abuse notation call it the same
      # thing we called the peer contagion effect so we don't have to re-write code
      # that already works for the peer contagione effect
      mutate(
        peer_pss_t4 = safe_network_average(Phat, y4)
      )

    W4 <- model.matrix.lm(
      pss_t4_round ~ . - peer_pss_t4,
      data = data,
      na.action = "na.pass" # keep, let downstream modeling functions drop NAs
    )

    # don't use intercept as an instrument (sanity check this choice)
    W4 <- W4[, -1]

    G4W4 <- safe_network_average_matrix(Phat, W4)
    G4G4W4 <- safe_network_average_matrix(Phat, G4W4)

    Z4 <- cbind(W4, G4W4, G4G4W4)
    Z4 <- as.matrix(Z4)

    colnames(Z4) <- paste0("Z", 1:ncol(Z4))
    covariates <- setdiff(colnames(data), "pss_t4_round")
    instruments <- colnames(Z4)

    merged <- bind_cols(
      data,
      as.data.frame(Z4)
    )

    formula <- glue(
      "pss_t4_round ~ {paste0(covariates, collapse = ' + ')} | {paste0(instruments, collapse = ' + ')}"
    )

    tsls_t4 <- ivreg(
      as.formula(formula),
      data = merged
    )

    # can fail due to numerical issues at low ranks -- Ghat and Xhat are colinear at rank 1 definitionally,
    # and can have issues at other low ranks depending on the precise structure of Xhat
    tidied <- try(
      tidy(tsls_t4, conf.int = TRUE),
      silent = TRUE
    )

    if (inherits(tidied, "try-error")) {
      tidied <- enframe(coef(tsls_t4), name = "term", value = "estimate")
    }

    mutate(
      tidied,
      rank = rank,
      estimator = "tsls",
      network = "latent",
      homophily = homophily
    )
  }

  estimates_homophily <- bind_rows(
    map_dfr(ranks, ols_peer_at_rank_t4),
    map_dfr(ranks, ols_latent_at_rank_t4),
    map_dfr(ranks, tsls_peer_at_rank_t4),
    map_dfr(ranks, tsls_latent_at_rank_t4)
  )

  estimates_no_homophily <- bind_rows(
    map_dfr(ranks, ols_peer_at_rank_t4, homophily = FALSE),
    map_dfr(ranks, ols_latent_at_rank_t4, homophily = FALSE),
    map_dfr(ranks, tsls_peer_at_rank_t4, homophily = FALSE),
    map_dfr(ranks, tsls_latent_at_rank_t4, homophily = FALSE)
  )

  bind_rows(
    estimates_homophily,
    estimates_no_homophily
  )
}

plot_anxiety_estimates <- function(estimates, palette) {
  plot <- estimates |>
    filter(
      term == "peer_pss_t4",
      rank > 2
    ) |>
    ggplot() +
    aes(
      x = rank,
      ymin = conf.low,
      y = estimate,
      ymax = conf.high,
      fill = network,
      color = network
    ) +
    geom_hline(yintercept = c(-1, 0, 1), color = palette[["olive"]], linetype = "dashed") +
    geom_ribbon(alpha = 0.4) +
    # orange = standard (peer) model, red = our latent model
    scale_fill_manual(
      values = c(latent = palette[["red"]], peer = palette[["bright"]]),
      labels = c(peer = "Peer", latent = "Latent")
    ) +
    scale_color_manual(
      values = c(latent = palette[["red"]], peer = palette[["bright"]]),
      labels = c(peer = "Peer", latent = "Latent")
    ) +
    theme_deck(palette, 18) +
    facet_grid(
      cols = vars(estimator),
      rows = vars(homophily),
      labeller = labeller(
        homophily = c("TRUE" = "Adjusting for X", "FALSE" = "Not adjusting for X"),
        estimator = c(ols = "OLS", tsls = "TSLS")
      )
    ) +
    labs(
      y = "Confidence interval (95%)\nfor contagion coefficient",
      x = "Embedding dimension",
      fill = "Estimator",
      color = "Estimator"
    )

  save_deck_plot(plot, "figures/anxiety/estimates.png", width = 10, height = 9 * 9 / 16, dpi = 300)

  # single panel for the main talk: TSLS only, not adjusting for X
  plot_tsls <- plot %+%
    filter(plot$data, estimator == "tsls", !homophily) +
    facet_null()

  save_deck_plot(plot_tsls, "figures/anxiety/estimates-tsls.png", width = 8, height = 8 * 9 / 16, dpi = 300)
}
