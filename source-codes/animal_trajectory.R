animal_trajectory <- function(initial_coords = NULL){
  
  # considerando time-frames de 10 s
  x <- initial_coords[1]
  y <- initial_coords[2]
  if(landscape_on == FALSE){
    trajectory <- NULL
    for(i in 1:time.step){
      repeat {
        theta <- runif(1, 0, 2*pi)  # Sorteia um novo valor de theta
        new_x <- x + delta * cos(theta)
        new_y <- y + delta * sin(theta)
        
        # Verifica as condições de contorno
        if(new_x > 0 && new_x < l && new_y > 0 && new_y < l) {
          x <- new_x
          y <- new_y
          break  # Sai do loop se as condições forem satisfeitas
        } # end-if
      } # end-repeat
      trajectory <- rbind(trajectory, c(i, x, y))
    }
    
  } else{ stop("Falta implementar!")}
  
return(trajectory)  
}