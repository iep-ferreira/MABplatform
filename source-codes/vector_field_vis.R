vector_field_vis <- function(weights_matrix, theta_matrix, norm_matrix, reduction_factor, n_grid ){
  
  weights_raster <- raster(weights_matrix, 0, 1800/reduction_factor, 0, 1800/reduction_factor)
  ### Verificar se o ângulo está trocado
  raster_theta <- raster(theta_matrix , 0, 1800/reduction_factor, 0, 1800/reduction_factor)
  #
  nrows <- nrow(raster_theta)
  ncols <- ncol(raster_theta)
  #
  grid_spacing <- max(1, floor(min(nrows, ncols) / n_grid))
  x_coords <- seq(1, ncols, by = grid_spacing)
  y_coords <- seq(1, nrows, by = grid_spacing)
  #
  vector_field <- expand.grid(x = x_coords, y = y_coords)
  vector_field$theta <- extract(raster_theta, cbind(vector_field$x, vector_field$y))
  vector_field$dx <- cos(vector_field$theta)
  vector_field$dy <- sin(vector_field$theta)
  vector_field$scale_factor <- extract(raster(norm_matrix, 0, 1800/reduction_factor, 0, 1800/reduction_factor), cbind(vector_field$x, vector_field$y))
  #
  # vector_field$scale_factor[ vector_field$scale_factor < 300 ] <- 0
  arrow_scale <- 1/50
  #
  # Aplicar a matriz de escala aos componentes dx e dy
  vector_field$dx <- vector_field$dx * vector_field$scale_factor * arrow_scale
  vector_field$dy <- vector_field$dy * vector_field$scale_factor * arrow_scale
  
  # Converter o raster para data.frame
  raster_df <- as.data.frame(weights_raster, xy = TRUE)
  colnames(raster_df) <- c("x", "y", "weights")
  raster_df$weights_mod <- raster_df$weights*(raster_df$weights<-10)
  
  # plot
  ggplot(vector_field, aes(x = x, y = y))  +
    geom_raster(data = raster_df, aes(x = x, y = y, fill = weights_mod))  +
    scale_fill_gradient(low = "red", high = "white", na.value = "white", name = "Weights") +
    geom_segment(aes(xend = x + dx, yend = y + dy), 
                 arrow = arrow(length = unit(0.1, "cm")), color = "black", alpha = 0.8, linewidth = 0.5) +
    theme_minimal() +
    coord_equal() +
    labs(title = "Raster  - Vector Field", x = "x", y = "y")
  
}