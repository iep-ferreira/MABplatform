# espeficicada para rasters com resolução 1
xy_to_ij <- function(x, y, xminimo, ymaximo, res){
 
  # Calcule a coluna (j) e a linha (i)
  j <- floor((x - xminimo) / res[1]) + 1
  i <- floor((ymaximo - y) / res[2]) + 1
  
  return(list(ij = c(i, j)))
}