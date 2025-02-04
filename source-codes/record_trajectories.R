record_trajectories <- function(mov_obj, hash_value){
  
ngroup <- length(mov_obj$trajectory)
  if(ngroup != 1){stop("Simular apenas um individuo por vez")}
  #for(i in 1:ngroup){
    id_aux <- UUIDgenerate()
    linhas <- length(mov_obj$time.step)
    trajectory_df <- data.frame(
      id = rep(id_aux, linhas),
      step = 1:mov_obj$time.step,
      week = as.integer((1:mov_obj$time.step/5040))+1,
      x = round(mov_obj$trajectory[[1]][, 2], 4),
      y = round(mov_obj$trajectory[[1]][, 3], 4)
    )
    
    file_name <- paste0("./data_sim/", id_aux, ".csv")
    write.csv(NULL, file_name, row.names = FALSE)
  
    con <- file(file_name, "w")
    
    writeLines(c(
      paste0("unique_id,", id_aux),
      paste0("hash_landscape,", hash_value),
      "id,step,week,x,y"  
    ), con)
    
    close(con)
    
    # Escrever os dados da trajetoria corretamente no formato CSV
    write.table(trajectory_df, file_name, sep = ",", col.names = FALSE, 
                row.names = FALSE, append = TRUE, quote = FALSE)
      
}