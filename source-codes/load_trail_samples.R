library(data.table)

load_and_join_trails <- function(list_files) {
  
  # Listar os arquivos CSV na pasta
  arquivos <- list_files
  
  # Aqui usamos 'lapply()' e depois rbindlist() para combinar tudo
  metadados_list <- lapply(arquivos, function(file) {
    # Lê apenas as 2 primeiras linhas
    lines <- readLines(file, n = 2)
    unique_id <- sub("unique_id,", "", lines[1]) 
    hash_landscape <- sub("hash_landscape,", "", lines[2])
    data.table(
      unique_id = unique_id,
      hash_landscape = hash_landscape
    )
  })
  metadados_final <- rbindlist(metadados_list, use.names = TRUE, fill = TRUE)
  
  # Ler cada CSV pulando as 2 primeiras linhas (que são metadados)
  # e combinar tudo com rbindlist()
  data_frames_list <- lapply(arquivos, function(file) {
    # fread é bem mais rápido que read.csv
    fread(file, skip = 2)
  })
  data_final <- rbindlist(data_frames_list, use.names = TRUE, fill = TRUE)
  
  return(list(
    traj_table = data_final, 
    selected_files = arquivos, 
    selected_files_metadados = metadados_final
  ))
}

load_sample_of_trails <- function(list_files){
  sim_res <- load_and_join_trails(list_files)
  
  traj <- sim_res$traj_table
  # Ajuste dos nomes das colunas
  setnames(traj, c("id", "step", "week", "x", "y"))
  
  # Cria coluna numérica a partir do 'id'
  traj[, id_numeric := as.numeric(factor(id))]
  
  # Retorna lista com as infos
  return(list(
    trajectories = traj[, !"id"], 
    files = sim_res$selected_files, 
    metadata = sim_res$selected_files_metadados
  ))
}