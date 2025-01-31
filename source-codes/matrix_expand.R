matrix_expand <- function(matrix, expand_factor){
  
  side <- dim(matrix$angle_matrix)[1]
  
  matrix_theta_expanded <- matrix(0, nrow = side*expand_factor, ncol = side*expand_factor)
  matrix_norm_expanded <- matrix(0, nrow = side*expand_factor, ncol = side*expand_factor)
    
  for (i in seq_len(side)) {
    for (j in seq_len(side)) {
      
      ang <- matrix$angle_matrix[i, j]
      nor <- matrix$norm_matrix[i, j]
      
      row_start <- 1 + expand_factor * (i - 1)
      row_end   <- min(expand_factor * i, expand_factor * side)
      col_start <- 1 + expand_factor * (j - 1)
      col_end   <- min(expand_factor * j, expand_factor * side)
      
      matrix_theta_expanded[row_start:row_end, col_start:col_end] <- ang
      matrix_norm_expanded[row_start:row_end, col_start:col_end] <- nor
    }
  }
  
  return(list(theta = matrix_theta_expanded, norm = matrix_norm_expanded))
}
