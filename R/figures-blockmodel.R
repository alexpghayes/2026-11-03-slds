make_blockmodel_figure <- function(palette) {
  k <- 4
  colors <- deck_block_colors(palette)

  B <- matrix(0.075, k, k)
  diag(B) <- 0.8

  theta <- runif(100, min = 1, max = 2)

  model <- dcsbm(
    theta = theta,
    B = B,
    expected_density = 0.15,
    sort_nodes = TRUE,
    poisson_edges = FALSE,
    allow_self_loops = FALSE
  )

  graph <- sample_tidygraph(model) |>
    mutate(
      block_int = as.integer(model$z),
      fill = colors[block_int],
      popularity = centrality_degree()
    ) |>
    filter(popularity > 2)

  plot <- ggraph(graph, layout = "fr") +
    geom_edge_link2(aes(colour = node.fill), alpha = 0.4) +
    geom_node_point(aes(size = popularity, color = fill)) +
    geom_node_point(
      aes(size = popularity),
      stroke = 1,
      shape = 21,
      color = "white"
    ) +
    scale_edge_color_identity(guide = "none") +
    scale_color_identity(guide = "none") +
    theme_deck(palette, void = TRUE) +
    theme(legend.position = "none")

  save_deck_plot(plot, "figures/blockmodel-poster.png", width = 5, height = 5, dpi = 500)
}
