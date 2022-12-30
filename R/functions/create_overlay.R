create_overlay <- function(
    rotate_value1, rotate_value2, shear_value, length_out){
  
  # Requires {ambient}, {dplyr}
  
  if(missing(length_out)){
    length_out <- 1000
  }
  
  grid <- ambient::long_grid(
    x = seq(0, 10, length.out = length_out),
    y = seq(0, 10, length.out = length_out)) |>
    dplyr::mutate(
      trans = ambient::trans_affine(
        x, y,
        ambient::rotate(pi/rotate_value1),
        ambient::shear(-shear_value),
        ambient::rotate(-pi/rotate_value2)),
      chess = ambient::gen_checkerboard(trans$x, trans$y))
  
  return(grid)
  
}