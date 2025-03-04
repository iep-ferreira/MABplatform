reduce_matrix <- function(M, reduction_factor) {
  
  # Dimens�es originais da matriz
  original_H <- dim(M)[1]  # N�mero de linhas
  original_W <- dim(M)[2]  # N�mero de colunas
  
  # Dimens�es reduzidas
  new_H <- floor(original_H / reduction_factor)
  new_W <- floor(original_W / reduction_factor)
  
  # Matriz reduzida inicializada com zeros
  M_reduc <- matrix(0, nrow = new_H, ncol = new_W)
  
  for (i in seq_len(new_H)) {
    for (j in seq_len(new_W)) {
      
      # Intervalo de linhas e colunas correspondente ao bloco de redu��o
      row_start <- 1 + reduction_factor * (i - 1)
      row_end   <- min(reduction_factor * i, original_H)
      col_start <- 1 + reduction_factor * (j - 1)
      col_end   <- min(reduction_factor * j, original_W)
      
      # Seleciona o bloco da matriz original
      bloco <- M[row_start:row_end, col_start:col_end]
      
      # Aplica a opera��o desejada, aqui m�dia (average pooling)
      M_reduc[i, j] <- mean(bloco, na.rm = TRUE)
    }
  }
  
  return(M_reduc)
}

