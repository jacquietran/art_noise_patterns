# Set custom parameters --------------------------------------------------------

iteration_id <- "noise_patterns_0050"
palette <- c("#DD7373", "#3B3561", "#EAD94C", "#D1D1D1", "#51A3A3")
initial_seed <- 587050
length_out_value <- 1000

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
  fracture_noise = "cubic", length_out = length_out_value)

grid2 <- create_noise_grid(
  seed_num = seed_vec[2], octaves_num = octaves_vec[2],
  fracture_noise = "value", length_out = length_out_value)

grid3 <- create_noise_grid(
  seed_num = seed_vec[2], octaves_num = octaves_vec[2],
  fracture_noise = "perlin", length_out = length_out_value)

grid4 <- create_noise_grid(
  seed_num = seed_vec[2], octaves_num = octaves_vec[2],
  fracture_noise = "simplex", length_out = length_out_value)

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
      data = grid3,
      ggplot2::aes(x = x, y = y, fill = full)),
    bg_layer = "blend1",
    blend_type = "soft_light",
    id = "blend2") +
  ggfx::with_blend(
    ggplot2::geom_raster(
      data = grid4,
      ggplot2::aes(x = x, y = y, fill = full)),
    bg_layer = "blend2",
    blend_type = "soft_light") +
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
