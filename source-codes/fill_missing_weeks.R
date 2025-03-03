fill_missing_weeks <- function(camera_count, camera_id) {
  expected_cols <- c("cam_id", "week", "total_counts", "total_distinct_animals")
  all_weeks <- data.frame(
    cam_id = rep(camera_id, 3),
    week = 2:4,
    total_counts = rep(0, 3),
    total_distinct_animals = rep(0, 3)
  )
  if (is.null(camera_count) || nrow(camera_count) == 0) {
    return(all_weeks)
  }
  if (is.null(names(camera_count))) {
    names(camera_count) <- expected_cols[seq_along(camera_count)]
  }
  missing_weeks <- all_weeks[!all_weeks$week %in% camera_count$week, ]
  complete_data <- rbind(camera_count, missing_weeks) %>% arrange(week)
  return(complete_data)
}  