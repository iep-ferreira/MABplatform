animal_mov <- function(n = 100, ra = NULL, time = 1, time_adapt = 1, where = "random", landscape_on = TRUE, velocity = 2){
  
  # Time in weeks 
  if(time<1){print("Error. At least one week is necessary!"); break}
  
  # considerando 12h de atividade por dia
  
  # Total de tempo gasto nas simulações (a cada min)
  time.step<-round(time_adapt+time)*7*12*60
  
  # Tempo total de adaptação (a cada min)
  time.step.adapt<-time_adapt*7*12*60
  
  # tamanho do passo (com base na velocidade em m/s - px / a cada 10s)
  # por exemplo: 7,2km/2 - 2m/s - 120m / min  - 4px/min
  delta <- velocity*60/30
  
  # Número de animais (n)
  # r = valores do raster (formato de matriz)
  if(is.null(ra)){print("Erro. Inserir os valores do raster no segundo argumento"); break}
  
  # Extraindo valores do raster, na forma de matriz 
  r <- values(ra, format="matrix")
  # Neste caso, r vira uma matriz auxiliar
  
  # dimensão da paisagem
  l <- dim(r)[1]
  
  # Aleatoriamente ou só onde tem recursos?
  if(where=="random"||where=="resource"){
    
    if(where=="random"){ # distribuição espacial sem restrições
      # posição inicial dos indíviduos (x_0, y_0)
      
      sample_coords <- sample(1:l,2*n,replace=TRUE)
      #message("passou!/n")
      
      M <- matrix(sample_coords, ncol=2)
      
    } else{ # entra no else se for "resource"
      
      # distribuição espacial apenas aonde tem recurso
      coords<-NULL
      for(i in 2:(l-1)){
        for(j in 2:(l-1)){
          # vamos trabalhar com outras formas de alocação (fazer revisão)
          if(r[i,j]==3|r[i,j]==9){coords<-rbind(coords,c(i,j))}  
          # coords são os pontos onde tem recurso
        } # end for 1 
      } # end for 2
      
      size <- dim(coords)[1] # tamanho do objeto coords  
      # sorteio das posições iniciais dentro de coords
      sample_lines <- sample.int(size, n, replace=TRUE)  
      
      M <- coords[sample_lines,]  # matriz de posição dos animais
    } # fim else if random 
    
  } else{
    print("Error. where must be random or resource"); break  
  }  
  
  # função de movimentação individual
  # loop sobre os n animais 
  result <- list()
  for(j in 1:n){
  #  result[[j]] <- animal_trajectory(M[j,])
    
    # considerando time-frames de 10 s
    x <- M[j, 1]
    y <- M[j, 2]
    if(landscape_on == FALSE){
      trajectory <- NULL
      #time.step
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
    
  result[[j]] <- trajectory  
    
  } # end-for j
  
  return(list("time.step" = time.step,"time.step.adapt" = time.step.adapt, "trajectory" = result))
} # fim for animal movements




