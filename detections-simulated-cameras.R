# loop for - para cada lote de processamento
for(i in 9:20){
  
library(raster); library(landscapeR); library(sf);
library(mapview); library(ggplot2); library(unmarked); 
library(dplyr)
library(ggspatial)  # Para lidar com rasters e mapas
library(sf)# Para geometria vetorial (círculo)
library(plotly)
library(uuid)
library(readxl)
library(writexl)
library(digest) # para criar hash
library(sp)
library(jsonlite)

# carrega as funções de dependência
carrega <- function(){
  source("./source-codes/animal_mov.R")
  source("./source-codes/tracks_viewer.R")
  source("./source-codes/xytoij_transform.R")
  source("./source-codes/csv_traj_join_fast.R")
  source("./source-codes/load_trail_samples.R")
  source("./source-codes/record_trajectories.R")  
  source("./source-codes/calc_centroid_radius.R")  
  source("./source-codes/camera_position.R")    
  source("./source-codes/camera_attributes.R")  
  source("./source-codes/camera_counts.R")
}
carrega()


# ler a tabela de cameras 
cam_attributes <- read_xlsx("./pre-processamento/cam_attributes.xlsx")

# define o caminho do hd externo
dir_path = "D:/dados-abundancia-animal-temp/todas-as-trilhas"

# define a lista de arquivos
json_file <- "./pre-processamento/trails_samples.json"
trails_samples_reloaded <- fromJSON(json_file)


  # abre o excel das contagens
  file_path <- paste0("./pre-processamento/cam_records_sample",i,".xlsx")
  
  # abre o excel das contagens
  file_path_2 <- paste0("./pre-processamento/cam_detections_table_sample",i,".xlsx")  
  
# carregas as trilhas 
arquivos <- paste0(dir_path, "/", trails_samples_reloaded[[i]])
system.time({
traj <- load_sample_of_trails(arquivos)$trajectories
})

# 
for(j in 1:dim(cam_attributes)[1]){

  if(j%%20 == 0){ print(paste("Amostra:", i)); print(paste("Camera:", j))  }
    
  current_camera <- cam_records_row <- cam_attributes[j,] 
  camera <- camera_position(
    x = current_camera$x, y = current_camera$y, 
    field_depth = current_camera$field_depth, 
    cam_direction = current_camera$orientation_angle, 
    sector_angle = current_camera$viewing_angle, 
    arc_points = 15)  
  
  temp_buffer <- 60
  counts_result <- camera_counts(cam_obj = camera, trajectory = traj, 
  temp_buffer = temp_buffer, ft = 28.8/1000, chart = FALSE, 
  sample = paste0("sample",i) )

  ## tabela de resumo das contagens
  new_row <- data.frame(
    cam_records_row,  
    sample = counts_result$sample_data,
    counts = counts_result$counts_animals,
    true_population = counts_result$counts_polygons, 
    temp_buffer = temp_buffer
  )
  
  if (file.exists(file_path)) {
    existing_data <- read_xlsx(file_path)
    updated_data <- rbind(existing_data, new_row)
  } else {
    updated_data <- new_row
  }
  
  write_xlsx(updated_data, file_path)

  # Tabela de detecções 
  if(dim(counts_result$detections_table)[1] > 0){
  new_data <- counts_result$detections_table %>%
    mutate(
      sample = paste0("sample",i),  # Valor fixo para a coluna 'sample'
      camera = paste0("camera",j)    # Valor fixo para a coluna 'camera'
    )
  
  if (file.exists(file_path_2)) {
    existing_data <- read_xlsx(file_path_2)
    updated_data <- bind_rows(existing_data, new_data)
  } else {
    updated_data <- new_data
  }
  write_xlsx(updated_data, file_path_2)
  } # end-if tabela de deteccoes
  
} # end-for j (cameras)

rm(list = setdiff(ls(), c("i", "j")))
gc()

} # end-for i (amostras)


