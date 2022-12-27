create_overlay <- function(rotate_value1, rotate_value2, shear_value){
  
  # Requires {ambient}, {dplyr}
  
  grid <- ambient::long_grid(
    x = seq(0, 10, length.out = 1000),
    y = seq(0, 10, length.out = 1000)) |>
    dplyr::mutate(
      trans = ambient::trans_affine(
        x, y,
        ambient::rotate(pi/rotate_value1),
        ambient::shear(-shear_value),
        ambient::rotate(-pi/rotate_value2)),
      chess = ambient::gen_checkerboard(trans$x, trans$y))
  
  return(grid)
  
}