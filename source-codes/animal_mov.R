animal_mov <- function(n = 100, raster_theta = NULL, raster_norm = NULL, time = 1, time_adapt = 1, 
                       where = "random", landscape_on = TRUE, velocity = 0.347){
  
  # considerando uma caminhada média de 15km, os porcos andam em um ritmo médio de 0,174m/s 
  # tamanho do passo (com base na velocidade em m/s - px / min)
  # por exemplo: 1.24km/h - 0.347m/s - 20.83m/min  - 0.77px/min
  pixel_side <- 28.66 # (metros, após correção)
  # delta é quanto andou a cada minuto, medido em comprimento do pixel
  delta <- velocity*60/pixel_side
  
  # passos com distribuicao de pareto, media delta e alpha igual 5
  alpha <- 5
  
  # angle raster properties
  resolucao <- res(raster_theta)
  xminimo <- xmin(raster_theta); ymaximo <- ymax(raster_theta)
  
  # normalização da norma
  raster_norm <- raster_norm/  max(getValues(raster_norm))
  
  # Time in weeks 
  if(time<1){print("Error. At least one week is necessary!"); break}
  
  # considerando 12h de atividade por dia
  # Total de tempo gasto nas simulações (em min)
  time.step<-round(time_adapt+time)*7*12*60 - 1
  
  # Tempo total de adaptação (em min)
  time.step.adapt<-time_adapt*7*12*60 - 1
  
  # Número de animais (n)
  # r = valores do raster (formato de matriz)
  if(is.null(raster_theta)){print("Erro. Inserir os valores do raster no segundo argumento"); break}
  
  # Extraindo valores do raster, na forma de matriz 
  r <- values(raster_theta, format="matrix")
  # Neste caso, r vira uma matriz auxiliar
  
  # dimensão da paisagem
  l <- dim(r)[1]
  
  ## definindo as posições iniciais dos animais
  
  # Aleatoriamente ou só onde tem recursos?
  if(where=="random"||where=="resource"){
    
    if(where=="random"){ # distribuição espacial sem restrições
      # posição inicial dos indíviduos (x_0, y_0)
      
      sample_coords <- sample(3:l-3,2*n,replace=TRUE)
      #message("passou!/n")
      
      # matriz de posições iniciais
      M <- matrix(sample_coords, ncol=2)
      
     } else{ # entra no else se for "resource"
      
      ## Agora estamos usando matroz de ângulos, e não a de uso e ocupação. 
      
    #   
    #   # distribuição espacial apenas aonde tem recurso
    #   coords<-NULL
    #   for(i in 2:(l-1)){
    #     for(j in 2:(l-1)){
    #       # vamos trabalhar com outras formas de alocação (fazer revisão)
    #       if(r[i,j]==3|r[i,j]==9){coords<-rbind(coords,c(i,j))}  
          # coords são os pontos onde tem recurso
    #    } # end for 1 
      
    #    } # end for 2
      
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
      theta <- runif(1, 0, 2*pi)  # Sorteia um novo valor de theta
      for(i in 1:time.step){
        if(i%%1000==0){print(100*i/time.step)}
        repeat {
          step_size <- rpareto_media(1, delta, alpha)
          new_x <- x + step_size * cos(theta)
          new_y <- y + step_size * sin(theta)
          
          # Verifica as condições de contorno
          if(new_x > 0 && new_x < l && new_y > 1 && new_y <= l) {
            x <- new_x
            y <- new_y
            break  # Sai do loop se as condições forem satisfeitas
          } # end-if
        } # end-repeat
        trajectory <- rbind(trajectory, c(i, x, y))
      }
      
    } else{ 
      
      trajectory <- NULL
      #time.step
      theta <- mu <- runif(1, 0, 2*pi)  # Sorteia um novo valor de theta
      for(i in 1:time.step){
        if(i%%1000==0){
          print(100*i/time.step)
          }
        repeat {
          
          coords_ij <- xy_to_ij(floor(x), floor(y), xminimo, ymaximo, resolucao)$ij
          mu <- raster_theta[coords_ij[1], coords_ij[2]] 
          norm <- raster_norm[coords_ij[1], coords_ij[2]]
          theta <- mu + rnorm(1, 0, (1-norm+0.01)*pi)  # Sorteia um novo valor de theta
          step_size <- rpareto_media(1, delta, alpha)
          new_x <- x + step_size * cos(theta)
          new_y <- y + step_size * sin(theta)
          # Verifica as condições de contorno
          if(new_x > 0 && new_x < l && new_y > 1 && new_y <= l) {
            x <- new_x
            y <- new_y
            break  # Sai do loop se as condições forem satisfeitas
          } # end-if
        } # end-repeat
        trajectory <- rbind(trajectory, c(i, x, y))
      }
      
      
      
      }
    
  result[[j]] <- trajectory  
    
  } # end-for j
  
  return(list("time.step" = time.step,"time.step.adapt" = time.step.adapt, "trajectory" = result))
} # fim for animal movements

rpareto_media <- function(n, media, alpha) {
  if (alpha <= 1) stop("O parâmetro alpha deve ser maior que 1 para a média existir.")
  
  # Calcular o parâmetro x_m a partir da média desejada
  x_m <- (media * (alpha - 1)) / alpha
  
  # Gerar números da distribuição de Pareto
  U <- runif(n)  # Números uniformes entre 0 e 1
  step_size <- x_m * (1 - U)^(-1 / alpha)
  
  return(step_size)
}


