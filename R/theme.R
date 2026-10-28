# Shared look for every figure: colors come straight from theme.typ so the
# plots can never drift from the slide theme; text is set in the deck's font.

deck_font <- "Charter"
deck_background <- "#FAFAFA" # white.darken(2%) in theme.typ

# Parse `#let name = rgb("#rrggbb")` lines out of theme.typ
read_deck_palette <- function(theme_path) {
  lines <- readLines(theme_path)
  matched <- str_match(lines, '^#let ([a-z-]+) = rgb\\("(#[0-9A-Fa-f]{6})"\\)')
  matched <- matched[!is.na(matched[, 1]), , drop = FALSE]
  set_names(toupper(matched[, 3]), matched[, 2])
}

# Color semantics (see the convention comment at the top of slides.typ):
#   dark-teal : text and structure
#   bright    : emphasis, and the standard (observed-network) model
#   red       : our proposal, the latent model
#   olive     : reference lines
#   muted     : background / de-emphasized
#   blocks    : categorical node colors for community structure
deck_block_colors <- function(palette) {
  unname(palette[c("olive", "dark-teal", "yellow", "red")])
}

theme_deck <- function(palette, base_size = 18, void = FALSE) {
  text_color <- palette[["dark-teal"]]
  transparent <- list(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  )

  if (void) {
    # no axes or grid: keep theme_void()'s emptiness, just set the font and color
    theme_void(base_size) +
      theme(text = element_text(family = deck_font, color = text_color), !!!transparent)
  } else {
    theme_minimal(base_size) +
      theme(
        text = element_text(family = deck_font, color = text_color),
        axis.text = element_text(color = text_color),
        strip.text = element_text(color = text_color),
        panel.grid = element_line(color = palette[["brighter"]], linewidth = 0.3),
        !!!transparent
      )
  }
}

# Save a plot with a transparent background (PNG via ragg, PDF via cairo so fonts are embedded); returns the path
save_deck_plot <- function(plot, path, width, height, dpi = 300) {
  # `&` also reaches patchwork composites, whose outer background is otherwise opaque
  plot <- plot &
    theme(
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA)
    )

  if (str_detect(path, "\\.pdf$")) {
    ggsave(path, plot, width = width, height = height, device = cairo_pdf, bg = "transparent", create.dir = TRUE)
  } else {
    ggsave(path, plot, width = width, height = height, dpi = dpi, device = agg_png, bg = "transparent", create.dir = TRUE)
  }
  path
}
