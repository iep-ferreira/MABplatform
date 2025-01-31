# Função para selecionar aleatoriamente n arquivos e uni-los
csv_traj_join <- function(pasta, n) {
  # Listar os arquivos CSV na pasta
  arquivos <- list.files(path = pasta, pattern = "\\.csv$", full.names = TRUE)
  
  # Verificar se existem arquivos suficientes
  if (length(arquivos) < n) {
    stop("A pasta não contém arquivos CSV suficientes.")
  }
  
  # Selecionar aleatoriamente n arquivos
  arquivos_selecionados <- sample(arquivos, n)
  
  # Ler e unir os arquivos em um único data frame
  data_frames <- lapply(arquivos_selecionados, read.csv)
  data_final <- do.call(rbind, data_frames)
  
  return(list(traj_table = data_final, selected_files = arquivos_selecionados))
}

# função para selecionar arquivos e reunir dados de trajetórias
select_trajectories <- function(dir_path, n_sample){
  sim_res <- csv_traj_join(dir_path, n_sample)
  sim_res$selected_files
  traj <- sim_res$traj_table
  colnames(traj) <- c("id", "step", "week", "x", "y")
  traj$id_numeric <- as.numeric(factor(traj$id))
  return(traj)
}
