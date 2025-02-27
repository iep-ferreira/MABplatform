camera_counts <- function(cam_obj, trajectory, temp_buffer = 60, ft = 28.8/1000, chart = FALSE,  
                          sample = "undentified" ){
  
  xc <- cam_obj$x_center
  yc <- cam_obj$y_center
  
  traj_within <- point.in.polygon(trajectory$x, trajectory$y, cam_obj$poly_df$x,  cam_obj$poly_df$y)
  traj_filtered <- trajectory %>% filter(traj_within == 1)
  
  # aplicando buffer temporal (em minutos)
  traj_filtered <- traj_filtered %>%
    group_by(id_numeric) %>%
    arrange(step, .by_group = TRUE) %>% 
    mutate(
      keep = {
        # Vetor lógico para marcar quais linhas serão mantidas
        keep_vec <- logical(n())
        # Variável que guarda o "step" da última detecção
        last_detection_step <- -Inf
        
        for (i in seq_len(n())) {
          # Se o step atual for MAIOR que (última detecção + buffer),
          # então esta linha passa a ser uma nova detecção
          if (step[i] > last_detection_step + temp_buffer) {
            keep_vec[i] <- TRUE
            last_detection_step <- step[i]  # atualiza o último step de detecção
          } else {
            keep_vec[i] <- FALSE
          }
        }
        keep_vec
      }
    ) %>%
    filter(keep) %>%
    ungroup()
  
  traj_filtered$distance <- sqrt( (traj_filtered$x - xc )^2 +  (traj_filtered$y - yc)^2 )*ft*1000
  
  life_area_data <- trajectory %>% filter( xc - 150 < x & x < xc + 150 & yc - 150 < y &  yc + 150 > y)
  
  result <- calc_centroid_radius(life_area_data, weeks = 2:4, ft = ft)
  
  point_sf <- st_sfc(st_point(c(xc, yc)), crs = st_crs(result$mcp_polygons))
  mcp_sf <- st_as_sf(result$mcp_polygons)
  counts_polygons <- sum(st_contains(mcp_sf, point_sf, sparse = FALSE))
  counts_animals <- dim( traj_filtered )[1]
  
  if(chart == TRUE &  dim(traj_filtered)[1] > 0){ 
    # Criar o mapa com cores personalizadas
    mapa <- mapview(cam_obj$poly_sf, color = "red", col.regions = "red", alpha.regions = 0.3) +  
      mapview(result$mcp_polygons, color = "blue", col.regions = "blue", alpha.regions = 0.3) +               mapview(traj_filtered, xcol = "x", ycol = "y", color = "green", col.regions = "green", alpha.regions = 0.7)
  } else{mapa <- NULL}
  
  
  return(list(detections_table = traj_filtered, map = mapa, sample_data = sample, counts_polygons = counts_polygons, counts_animals = counts_animals))
  
} # end-function