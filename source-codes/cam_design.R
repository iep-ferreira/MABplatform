# Aplicar a fórmula de Rowcliffe (2008)
est_dens <- function(co, detect_probability = 1., duration = 7*12*3*60 , v = 0.02082, r = 0.030, theta = pi/2, correction = FALSE){
# duration  = minuto, v = veloc (km/min), r sector radius (km) 
  res <- NULL
  effective_duration <- NULL
  for(j in 1:length(co)){
    if(correction == TRUE){
    effective_duration[j] <- duration - co[j]*60 # mod - descontar contagens que caem no buffer temporal
    } else{
    effective_duration[j] <- duration  
    }
    
    res <- cbind(res, (co[j] * pi) / (detect_probability*effective_duration[j]* v * r * (2 + theta))) # Rowcliffe modificada
  }
  
  ###PRECISO PENSAR!!!!
  res_2 <- (sum(co) * pi) / ( detect_probability*sum(effective_duration) * v * r * (2 + theta) )
  
  return(list(per_cam = res, est_total = res_2))
}

cam_design <- function(sample_list, list_cam , detections_summary , camera_records){
  
  df <- get_counts(sample_list, camera = list_cam[1], detections_summary, camera_records)$camera_counts
  
  true_density <- df$points_local_density[1]
  
  true_population_local <- df$total_true_population[1]
  
  sum_counts_week <- sum(df$total_counts)
  
  if(length(list_cam)>1){
    for(i in 2:length(list_cam)){
      df_aux <- get_counts(sample_list, camera = list_cam[i], detections_summary, camera_records)$camera_counts
      true_population_local <- c(true_population_local, df_aux$total_true_population[1])
      true_density <- c(true_density, df_aux$points_local_density[1])
      sum_counts_week <- c(sum_counts_week, sum(df_aux$total_counts) )
      df <- rbind(df, df_aux)
    } # end-for
  } # end-if  
  
  #print(sum_counts_week)
  # só vai precisar para o N-mixture
  # df_wide <- df %>% select(camera_id, week, total_counts) %>% pivot_wider(names_from = week, values_from = total_counts, values_fill = 0)
  
  # vetor de densidades estimadas
  density_vector <- est_dens(sum_counts_week)$per_cam
  
  # local densities
  density_rem <- data.frame(list_cam, sum_counts_week, as.vector(density_vector)) 
  colnames(density_rem) <- c("cam_id", "counts", "REM density")
  
  # densidade real da paisagem
  total_area <- 2661.32
  n_trails <- 2000*length(sample_list)
  mean_density <- n_trails/total_area
  
  return(
    list(counts_long = df, 
        #counts_wide = df_wide, 
        local_density_REM = density_rem, 
        mean_density_rem = est_dens(sum_counts_week)$est_total,
        true_cam_density = true_density,
        true_density_average = mean(true_density),
        landscape_true_density = mean_density, 
        counts_cam = sum_counts_week
        ) 
  )  
}