library(targets)
library(crew)
library(tarchetypes)
library(tibble)

# Figures for slides.typ. Run with targets::tar_make(); outputs land in figures/.
# Colors are read from theme.typ and text is set in the deck font (see R/theme.R).

tar_option_set(
  controller = crew_controller_local(workers = 2),
  seed = 26,
  packages = c(
    "broom",
    "dplyr",
    "fastadi",
    "forcats",
    "ggplot2",
    "ggraph",
    "glue",
    "igraph",
    "irlba",
    "ivreg",
    "Matrix",
    "patchwork",
    "purrr",
    "ragg",
    "readr",
    "RSpectra",
    "rlang",
    "scales",
    "stringr",
    "tibble",
    "tidygraph",
    "tidyr",
    # attached last on purpose: its sbm() must mask igraph::sbm()
    "fastRG"
  )
)

tar_source()

list(
  # --- shared look --------------------------------------------------------
  tar_target(theme_file, "theme.typ", format = "file"),
  tar_target(palette, read_deck_palette(theme_file)),

  # --- stochastic blockmodel / latent space figures -----------------------
  tar_target(
    blockmodel_figure,
    make_blockmodel_figure(palette),
    format = "file"
  ),
  tar_target(
    matrices_figure,
    make_concentration_figure(palette),
    format = "file"
  ),
  tar_target(
    missing_edges_figure,
    make_missing_edges_figure(palette),
    format = "file"
  ),
  tar_target(
    time_varying_figure,
    make_time_varying_figure(palette),
    format = "file"
  ),

  # --- noisy data and the low-rank structure estimated from it ------------
  tar_target(noisy_population, simulate_noisy_population()),
  tar_map(
    values = tibble(noise_type = noisy_types),
    names = noise_type,
    tar_target(noisy_sim, simulate_noisy_data(noisy_population, noise_type)),
    tar_target(
      noisy_figures,
      make_noisy_figures(noisy_sim, noise_type, palette),
      format = "file"
    )
  ),

  # --- anxiety application (Li et al. 2023 data) --------------------------
  tar_target(
    anxiety_edge_path,
    "data/li2023/edgelist.csv",
    format = "file"
  ),
  tar_target(
    anxiety_node_path,
    "data/li2023/node_data.csv",
    format = "file"
  ),
  tar_target(
    anxiety_graph,
    construct_anxiety_graph(anxiety_node_path, anxiety_edge_path)
  ),
  tar_target(
    anxiety_estimates,
    get_anxiety_estimates(anxiety_graph)
  ),
  tar_target(
    anxiety_figure,
    plot_anxiety_estimates(anxiety_estimates, palette),
    format = "file"
  )
)
