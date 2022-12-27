# Set custom parameters --------------------------------------------------------

iteration_id <- "noise_patterns_0001"


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
    worley = gen_worley(x, y, value = 'distance', seed = 5),
    worley_frac = fracture(gen_worley, ridged, octaves = 8, x = x, y = y, 
                           value = 'distance', seed = 5),
    full = blend(normalise(worley), normalise(worley_frac), gen_spheres(x1, y1)))

grid2 <- long_grid(x = seq(0, 10, length.out = 1000), 
                   y = seq(0, 10, length.out = 1000)) %>% 
  mutate(
    x1 = x + gen_simplex(x, y) / 2, 
    y1 = y + gen_simplex(x, y) / 2,
    worley = gen_worley(x, y, value = 'distance', seed = 4),
    worley_frac = fracture(gen_worley, ridged, octaves = 7, x = x, y = y, 
                           value = 'distance', seed = 4),
    full = blend(normalise(worley), normalise(worley_frac), gen_spheres(x1, y1)))

# Checkerboards
grid_chess <- long_grid(
  x = seq(0, 10, length.out = 1000),
  y = seq(0, 10, length.out = 1000)) %>%
  mutate(
    trans = trans_affine(x, y, rotate(pi/20), shear(-2), rotate(-pi/4)),
    chess = gen_checkerboard(trans$x, trans$y))

grid_chess2 <- long_grid(
  x = seq(0, 10, length.out = 1000),
  y = seq(0, 10, length.out = 1000)) %>%
  mutate(
    trans = trans_affine(x, y, rotate(pi/4), shear(-4), rotate(-pi/8)),
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
    blend_type = "darken_intensity",
    id = "blend1") +
  ggfx::with_blend(
    ggplot2::geom_raster(
      data = grid_chess,
      ggplot2::aes(x = x, y = y, fill = chess)),
    bg_layer = "blend1",
    blend_type = "color_burn",
    id = "chess1") +
  ggfx::with_blend(
    ggplot2::geom_raster(
      data = grid_chess2,
      ggplot2::aes(x = x, y = y, fill = chess)),
    bg_layer = "chess1",
    blend_type = "soft_light") +
  ggplot2::scale_fill_gradient(
    low = "#7FD8BE", high = "#FCAB64") +
  ggplot2::coord_cartesian(expand = FALSE) +
  ggplot2::theme_void() +
  ggplot2::theme(
    legend.position = "none")

# Save plot to file ------------------------------------------------------------

ggplot2::ggsave(
  here::here(glue::glue("img/{`iteration_id`}.png")),
  ggplot2::last_plot(),
  width = 10, height = 8, units = "in", dpi = 600)
