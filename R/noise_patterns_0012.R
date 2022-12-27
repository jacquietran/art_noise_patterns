# Set custom parameters --------------------------------------------------------

iteration_id <- "noise_patterns_0012"
palette <- c("#C6C5B9", "#62929E", "#1E352F", "#CC3F0C", "#9A6D38")
initial_seed <- 587012

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

# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ambient)

# Make data frames -------------------------------------------------------------

# Noise grids
grid <- long_grid(x = seq(0, 10, length.out = 1000), 
                  y = seq(0, 10, length.out = 1000)) %>% 
  mutate(
    x1 = x + gen_simplex(x, y) / 2, 
    y1 = y + gen_simplex(x, y) / 2,
    worley = gen_worley(x, y, value = 'distance', seed = seed_vec[1]),
    worley_frac = fracture(gen_simplex, ridged, octaves = octaves_vec[1], x = x, y = y, 
                           value = 'distance', seed = seed_vec[1]),
    full = blend(normalise(worley), normalise(worley_frac), gen_spheres(x1, y1)))

grid2 <- long_grid(x = seq(0, 10, length.out = 1000), 
                   y = seq(0, 10, length.out = 1000)) %>% 
  mutate(
    x1 = x + gen_simplex(x, y) / 2, 
    y1 = y + gen_simplex(x, y) / 2,
    worley = gen_worley(x, y, value = 'distance', seed = seed_vec[2]),
    worley_frac = fracture(gen_simplex, ridged, octaves = octaves_vec[2], x = x, y = y, 
                           value = 'distance', seed = seed_vec[2]),
    full = blend(normalise(worley), normalise(worley_frac), gen_spheres(x1, y1)))

# Overlays
grid_chess <- long_grid(
  x = seq(0, 10, length.out = 1000),
  y = seq(0, 10, length.out = 1000)) %>%
  mutate(
    trans = trans_affine(
      x, y,
      rotate(pi/rotate_vec[1]), shear(-shear_vec[1]), rotate(-pi/rotate_vec[2])),
    chess = gen_checkerboard(trans$x, trans$y))

grid_chess2 <- long_grid(
  x = seq(0, 10, length.out = 1000),
  y = seq(0, 10, length.out = 1000)) %>%
  mutate(
    trans = trans_affine(
      x, y,
      rotate(pi/rotate_vec[3]), shear(-shear_vec[2]), rotate(-pi/rotate_vec[4])),
    chess = gen_checkerboard(trans$x, trans$y))

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
    blend_type = "lighten_intensity",
    id = "blend1") +
  ggfx::with_blend(
    ggplot2::geom_raster(
      data = grid_chess,
      ggplot2::aes(x = x, y = y, fill = chess)),
    bg_layer = "blend1",
    blend_type = "overlay",
    id = "chess1") +
  ggfx::with_blend(
    ggplot2::geom_raster(
      data = grid_chess2,
      ggplot2::aes(x = x, y = y, fill = chess)),
    bg_layer = "chess1",
    blend_type = "pegtop_light") +
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
