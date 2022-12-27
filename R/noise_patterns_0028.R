# Set custom parameters --------------------------------------------------------

iteration_id <- "noise_patterns_0028"
palette <- c("#ED6A5A", "#9BC1BC", "#F0A202", "#612940", "#3E2F5B")
initial_seed <- 587028

# Source custom functions ------------------------------------------------------

source(here::here("R/functions/create_noise_grid.R"))
source(here::here("R/functions/create_overlay.R"))

# Derived parameters -----------------------------------------------------------

set.seed(initial_seed)
gradient_colours <- sample(palette, length(palette), replace = FALSE)

set.seed(initial_seed)
seed_vec <- sample(1:1000000, 2, replace = FALSE)

set.seed(initial_seed)
octaves_vec <- sample(2:10, 2, replace = FALSE)

set.seed(initial_seed)
rotate_vec <- sample(2:15, 4, replace = FALSE)

set.seed(initial_seed)
shear_vec <- sample(2:10, 2, replace = FALSE)

# Make data frames -------------------------------------------------------------

# Noise grids
grid <- create_noise_grid(
  seed_num = seed_vec[1], octaves_num = octaves_vec[1],
  fracture_noise = "simplex")

grid2 <- create_noise_grid(
  seed_num = seed_vec[2], octaves_num = octaves_vec[2],
  fracture_noise = "simplex")

# Overlays
grid_chess <- create_overlay(
  rotate_value1 = rotate_vec[1], rotate_value2 = rotate_vec[2],
  shear_value = shear_vec[1])

grid_chess2 <- create_overlay(
  rotate_value1 = rotate_vec[3], rotate_value2 = rotate_vec[4],
  shear_value = shear_vec[2])

# Build plot -------------------------------------------------------------------

ggplot2::ggplot() +
  ggfx::as_reference(
    ggplot2::geom_raster(
      data = grid,
      ggplot2::aes(x = x, y = y, fill = full)),
    id = "base") +
  ggfx::with_blend(
    ggplot2::geom_raster(
      data = grid2,
      ggplot2::aes(x = x, y = y, fill = full)),
    bg_layer = "base",
    blend_type = "darken_intensity",
    id = "blend1") +
  ggfx::with_blend(
    ggplot2::geom_raster(
      data = grid_chess,
      ggplot2::aes(x = x, y = y, fill = chess)),
    bg_layer = "blend1",
    blend_type = "soft_light",
    id = "chess1") +
  ggfx::with_blend(
    ggplot2::geom_raster(
      data = grid_chess2,
      ggplot2::aes(x = x, y = y, fill = chess)),
    bg_layer = "chess1",
    blend_type = "lighten") +
  ggplot2::scale_fill_gradientn(
    colours = gradient_colours) +
  ggplot2::coord_cartesian(expand = FALSE) +
  ggplot2::theme_void() +
  ggplot2::theme(
    legend.position = "none")

# Save plot to file ------------------------------------------------------------

ggplot2::ggsave(
  here::here(glue::glue("img/{`iteration_id`}.png")),
  ggplot2::last_plot(),
  width = 10, height = 8, units = "in", dpi = 600)
