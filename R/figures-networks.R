# Stochastic blockmodel network drawings used on the "noisy edges" and
# "co-evolution" slides.

make_missing_edges_figure <- function(palette) {
  n_blocks <- 4
  n <- 80

  B <- matrix(0.01, n_blocks, n_blocks)
  diag(B) <- 0.3

  model <- sbm(
    n = n,
    B = B,
    pi = rep(1 / n_blocks, n_blocks),
    poisson_edges = FALSE,
    allow_self_loops = FALSE
  )

  graph <- sample_tidygraph(model) |>
    mutate(block = model$z) |>
    activate(edges) |>
    mutate(edge_id = row_number())

  n_edges <- graph |> activate(edges) |> as_tibble() |> nrow()
  missing_edges <- sample(n_edges, 25)

  # orange = emphasis: the edges we failed to observe
  plot <- graph |>
    activate(edges) |>
    mutate(
      edge_color = if_else(edge_id %in% missing_edges, palette[["bright"]], palette[["muted"]]),
      edge_linetype = if_else(edge_id %in% missing_edges, "dashed", "solid")
    ) |>
    ggraph(layout = "fr") +
    geom_edge_fan(aes(color = edge_color, linetype = edge_linetype), alpha = 0.6) +
    geom_node_point(aes(fill = block), size = 3, stroke = 1.2, shape = 21, color = "white") +
    scale_edge_color_identity() +
    scale_edge_linetype_identity() +
    scale_fill_manual(values = deck_block_colors(palette), guide = "none") +
    theme_deck(palette, void = TRUE)

  save_deck_plot(plot, "figures/missing-edges.pdf", width = 5, height = 3)
}

make_time_varying_figure <- function(palette) {
  n_blocks <- 4
  n <- 80
  colors <- deck_block_colors(palette)
  node_to_shift <- 77

  B <- matrix(0.005, n_blocks, n_blocks)
  diag(B) <- 0.22

  model <- sbm(
    n = n,
    B = B,
    pi = rep(1 / n_blocks, n_blocks),
    poisson_edges = FALSE,
    allow_self_loops = FALSE
  )

  graph <- sample_tidygraph(model) |>
    mutate(
      block_int = as.integer(model$z),
      fill = colors[block_int]
    ) |>
    activate(edges) |>
    mutate(
      edge_color = palette[["muted"]],
      edge_linetype = "solid"
    )

  layout <- create_layout(graph, layout = "fr")
  node_coords <- tibble(x = layout$x, y = layout$y)

  draw_graph <- function(g, title) {
    ggraph(g, layout = "manual", x = node_coords$x, y = node_coords$y) +
      geom_edge_fan(aes(color = edge_color, linetype = edge_linetype), alpha = 0.6) +
      geom_node_point(aes(fill = fill), size = 1.5, stroke = 0.3, shape = 21, color = "white") +
      scale_edge_color_identity() +
      scale_edge_linetype_identity() +
      scale_fill_identity(guide = "none") +
      labs(title = title) +
      theme_deck(palette, base_size = 11, void = TRUE) +
      theme(plot.title = element_text(hjust = 0.5))
  }

  p1 <- draw_graph(graph, "T = 1")

  block3_nodes <- graph |>
    activate(nodes) |>
    mutate(row_number = row_number()) |>
    filter(block_int == 3) |>
    sample_n(10) |>
    pull(row_number)

  new_edges <- tibble(
    from = node_to_shift,
    to = block3_nodes,
    new = TRUE
  )

  # orange = emphasis: the node that rewired between T = 1 and T = 2
  graph2 <- graph |>
    activate(edges) |>
    filter(from != node_to_shift, to != node_to_shift) |>
    mutate(new = FALSE) |>
    bind_edges(new_edges) |>
    mutate(
      edge_color = if_else(new, palette[["bright"]], palette[["muted"]]),
      edge_linetype = if_else(new, "dashed", "solid")
    ) |>
    activate(nodes) |>
    mutate(
      block_int = if_else(row_number() == node_to_shift, 3L, block_int),
      fill = colors[block_int]
    )

  p2 <- draw_graph(graph2, "T = 2")

  save_deck_plot(p1 + p2, "figures/time-varying.png", width = 5, height = 3, dpi = 300)
}
