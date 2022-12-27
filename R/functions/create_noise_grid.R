create_noise_grid <- function(
    seed_num, octaves_num, fracture_noise = c(
      "worley", "simplex", "cubic", "value", "perlin", "spheres", "waves")){
  
  # Requires {ambient}, {dplyr}
  
  # Worley noise
  if(fracture_noise == "worley"){
    
    grid <- ambient::long_grid(
      x = seq(0, 10, length.out = 1000), 
      y = seq(0, 10, length.out = 1000)) |>
      dplyr::mutate(
        x1 = x + ambient::gen_simplex(x, y) / 2, 
        y1 = y + ambient::gen_simplex(x, y) / 2,
        noise = ambient::gen_worley(x, y, value = 'distance', seed = seed_num),
        noise_frac = ambient::fracture(
          ambient::gen_worley, ambient::ridged,
          octaves = octaves_num, x = x, y = y, value = 'distance',
          seed = seed_num),
        full = ambient::blend(
          ambient::normalise(noise),
          ambient::normalise(noise_frac),
          ambient::gen_spheres(x1, y1)))
    
  }
  
  # Simplex noise
  if(fracture_noise == "simplex"){
    
    grid <- ambient::long_grid(
      x = seq(0, 10, length.out = 1000), 
      y = seq(0, 10, length.out = 1000)) |>
      dplyr::mutate(
        x1 = x + ambient::gen_simplex(x, y) / 2, 
        y1 = y + ambient::gen_simplex(x, y) / 2,
        noise = ambient::gen_worley(x, y, value = 'distance', seed = seed_num),
        noise_frac = ambient::fracture(
          ambient::gen_simplex, ambient::ridged,
          octaves = octaves_num, x = x, y = y, value = 'distance',
          seed = seed_num),
        full = ambient::blend(
          ambient::normalise(noise),
          ambient::normalise(noise_frac),
          ambient::gen_spheres(x1, y1)))
    
  }
  
  # Cubic noise
  if(fracture_noise == "cubic"){
    
    grid <- ambient::long_grid(
      x = seq(0, 10, length.out = 1000), 
      y = seq(0, 10, length.out = 1000)) |>
      dplyr::mutate(
        x1 = x + ambient::gen_simplex(x, y) / 2, 
        y1 = y + ambient::gen_simplex(x, y) / 2,
        noise = ambient::gen_worley(x, y, value = 'distance', seed = seed_num),
        noise_frac = ambient::fracture(
          ambient::gen_cubic, ambient::ridged,
          octaves = octaves_num, x = x, y = y, value = 'distance',
          seed = seed_num),
        full = ambient::blend(
          ambient::normalise(noise),
          ambient::normalise(noise_frac),
          ambient::gen_spheres(x1, y1)))
    
  }
  
  # Value noise
  if(fracture_noise == "value"){
    
    grid <- ambient::long_grid(
      x = seq(0, 10, length.out = 1000), 
      y = seq(0, 10, length.out = 1000)) |>
      dplyr::mutate(
        x1 = x + ambient::gen_simplex(x, y) / 2, 
        y1 = y + ambient::gen_simplex(x, y) / 2,
        noise = ambient::gen_worley(x, y, value = 'distance', seed = seed_num),
        noise_frac = ambient::fracture(
          ambient::gen_value, ambient::ridged,
          octaves = octaves_num, x = x, y = y, value = 'distance',
          seed = seed_num),
        full = ambient::blend(
          ambient::normalise(noise),
          ambient::normalise(noise_frac),
          ambient::gen_spheres(x1, y1)))
    
  }
  
  # Perlin noise
  if(fracture_noise == "perlin"){
    
    grid <- ambient::long_grid(
      x = seq(0, 10, length.out = 1000), 
      y = seq(0, 10, length.out = 1000)) |>
      dplyr::mutate(
        x1 = x + ambient::gen_simplex(x, y) / 2, 
        y1 = y + ambient::gen_simplex(x, y) / 2,
        noise = ambient::gen_worley(x, y, value = 'distance', seed = seed_num),
        noise_frac = ambient::fracture(
          ambient::gen_perlin, ambient::ridged,
          octaves = octaves_num, x = x, y = y, value = 'distance',
          seed = seed_num),
        full = ambient::blend(
          ambient::normalise(noise),
          ambient::normalise(noise_frac),
          ambient::gen_spheres(x1, y1)))
    
  }
  
  # Spheres noise
  if(fracture_noise == "spheres"){
    
    grid <- ambient::long_grid(
      x = seq(0, 10, length.out = 1000), 
      y = seq(0, 10, length.out = 1000)) |>
      dplyr::mutate(
        x1 = x + ambient::gen_simplex(x, y) / 2, 
        y1 = y + ambient::gen_simplex(x, y) / 2,
        noise = ambient::gen_worley(x, y, value = 'distance', seed = seed_num),
        noise_frac = ambient::fracture(
          ambient::gen_spheres, ambient::ridged,
          octaves = octaves_num, x = x, y = y, value = 'distance',
          seed = seed_num),
        full = ambient::blend(
          ambient::normalise(noise),
          ambient::normalise(noise_frac),
          ambient::gen_spheres(x1, y1)))
    
  }
  
  # Waves noise
  if(fracture_noise == "waves"){
    
    grid <- ambient::long_grid(
      x = seq(0, 10, length.out = 1000), 
      y = seq(0, 10, length.out = 1000)) |>
      dplyr::mutate(
        x1 = x + ambient::gen_simplex(x, y) / 2, 
        y1 = y + ambient::gen_simplex(x, y) / 2,
        noise = ambient::gen_worley(x, y, value = 'distance', seed = seed_num),
        noise_frac = ambient::fracture(
          ambient::gen_waves, ambient::ridged,
          octaves = octaves_num, x = x, y = y, value = 'distance',
          seed = seed_num),
        full = ambient::blend(
          ambient::normalise(noise),
          ambient::normalise(noise_frac),
          ambient::gen_spheres(x1, y1)))
    
  }
  
  return(grid)
  
}