get_counts <- function(sample_list, cam_id, detections_summary, camera_records   ){
  
  cam_records$points_local_density <- cam_records$freq_animal_local_km*length(sample_list)*2000
  
  counts_table <- detections_summary %>%
    filter(sample %in% sample_list) %>%
    group_by(week, camera) %>%  
    summarise(
      total_counts = sum(counts),  
      total_distinct_animals = sum(distinct_animals),  
      .groups = "drop"
    )
  
  counts_table <- counts_table %>%
    mutate(camera_id = parse_number(camera)) %>%
    arrange(camera_id) %>% relocate(last_col(), .before = everything())
  
  # counts per week  
  camera_counts <- counts_table %>% filter(camera_id == cam_id)
  counts_table_df <- data.frame(camera_counts)
  source("./source-codes/fill_missing_weeks.R")
  counts_results <- fill_missing_weeks(counts_table_df, cam_id)
  
  # popul. total representada pelos poligonos de area de vida
  total_polygons <-  cam_records %>% filter(camera_id == cam_id) %>% select(camera_id, x, y, orientation_angle, viewing_angle, field_depth, habitat_weights, mov_norm, mov_vector, sample, true_population, temp_buffer) %>% 
    filter(sample %in% sample_list) %>% summarise(total_true_population = sum(true_population) )
  
  counts_results <- cbind(counts_results, cam_records %>% filter(camera_id == cam_id) %>% filter(sample == "sample1") %>% select(x, y, orientation_angle, viewing_angle, field_depth, habitat_weights, mov_norm, mov_vector, sample, true_population, temp_buffer, freq_animal_local_km, points_local_density)) 
  
  counts_results <- cbind(counts_results, total_polygons)
  
  return(list(camera_counts = counts_results))
}