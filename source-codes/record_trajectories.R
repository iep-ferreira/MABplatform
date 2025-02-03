record_trajectories <- function(mov_obj, hash_value){
  ngroup <- length(mov$trajectory)
  for(i in 1:ngroup){
    id_aux <- UUIDgenerate()
    linhas <- length(mov$time.step)
    trajectory_df <- data.frame(
      unique_id <- rep(id_aux, linhas),
      step = 1:mov$time.step,
      week = as.integer((1:mov$time.step/30240))+1,
      x = round(mov$trajectory[[i]][, 2], 2),
      y = round(mov$trajectory[[i]][, 3], 2), 
      hash_landscape = rep(hash_value, linhas)
    )
    file_name <- paste0("./data_sim/", id_aux,".csv")  
    write.csv(trajectory_df, file_name, row.names = FALSE)  
  }
}