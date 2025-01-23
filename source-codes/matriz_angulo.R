get_neighbors <- function(i, j, radius, nrows, ncols) {
  # Limites
  row_min <- max(1, i - radius)
  row_max <- min(nrows, i + radius)
  col_min <- max(1, j - radius)
  col_max <- min(ncols, j + radius)
  
  # Gera todas as combinações (linhas e colunas) dentro desse retângulo
  grid <- expand.grid(rr = row_min:row_max, cc = col_min:col_max)
  
  grid <- grid[!(grid$rr == i & grid$cc == j), ]
  
  return(grid)
}

get_angle_map <- function(obj, radius=4) {
  
  nr <- nrow(obj)
  nc <- ncol(obj)

  # Obter valores do raster como matriz para acesso rápido
  v_mat <- as.matrix(obj)
  
  angle_matrix <- matrix(NA, nrow = nr, ncol = nc)
  norm_matrix <- matrix(NA, nrow = nr, ncol = nc)
  
  for(i in seq_len(nr)) {
    print(i/(nr)*100)
    
    for(j in seq_len(nc)) {
        
      # Lista de vizinhos dentro do raio de Moore
      neighbors <- get_neighbors(i, j, radius, nr, nc)
      
      # Se não houver vizinhos (ou se for a borda muito pequena), continue
      if (nrow(neighbors) == 0) {
        angle_matrix[i, j] <- NA
        next
      }
      
      # Coordenadas da célula central em "linhas/colunas"
      
      sum_x <- 0
      sum_y <- 0
      
      # Varre os vizinhos
      for(k in seq_len(nrow(neighbors))) {
        rr <- neighbors$rr[k] # linha do vizinho k
        cc <- neighbors$cc[k] # coluna do vizinho k 
        
        # Peso do vizinho
        w <- v_mat[rr, cc]
        
        dx <- cc - j  
        dy <- i - rr
        
        # Soma vetorial ponderada pelo peso
        sum_x <- sum_x + w * dx
        sum_y <- sum_y + w * dy
      }
      
      # atan2(y, x) retorna o ângulo em radianos no intervalo [-pi, +pi]
      angle_matrix[i, j] <- atan2(sum_y, sum_x)
      if(i>3&i<7&j>3&j<7){
      print(c(sum_x, sum_y,  atan2(sum_y, sum_x)*360/(2*pi)))
      }
      norm_matrix[i, j] <- sqrt(sum_y^2 + sum_x^2) 
    }
    }
  
  return(list(angle_matrix = angle_matrix, norm_matrix = norm_matrix))
}
