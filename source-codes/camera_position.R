camera_position <- function(x_center, y_center, field_depth, cam_direction, sector_angle, arc_points = 6){
  
  depth_px <- field_depth / 28.8
  
  # Gerar pontos para representar o setor de detecção
  thetas <- seq(-sector_angle / 2, sector_angle / 2, length.out = arc_points) 
  x_sector <- x_center + depth_px * cos(cam_direction + thetas)
  y_sector <- y_center + depth_px * sin(cam_direction + thetas)
  
  # Criar data frame para o setor de detecção
  sector_df <- data.frame(x = c(x_center, x_sector, x_center),
                          y = c(y_center, y_sector, y_center))
  
  # Converter para objeto sf
  sector_sf <- st_as_sf(st_sfc(st_polygon(list(as.matrix(sector_df)))))
  return(list(poly_sf = sector_sf, 
              poly_df = sector_df, 
              x_center = x_center, 
              y_center = y_center, 
              field_depth = field_depth, 
              cam_direction, 
              sector_angle))
} # 