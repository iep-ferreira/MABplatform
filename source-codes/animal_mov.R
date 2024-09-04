animal_mov <- function( n = 100, ra = NULL, time = 5, where = "random", landscape_on = TRUE, n_camera = 4){
  
  # sempre 20 semanas de adaptação
  
  # Time in weeks (20 adaptation + 1 experiment)
  if(time<=1){print("Error. At least two weeks are necessary!"); break}
  
  # Total de tempo gasto nas simulações (em h)
  time.step<-round(20+time)*7*24
  # Tempo total de adaptação (em h)
  time.step.adapt<-20*7*24
  
  # Número de animais (n)
  # r = valores do raster (formato de matriz)
  if(is.null(ra)){print("Erro. Inserir os valores do raster no segundo argumento"); break}
  
  # Extraindo valores do raster, na forma de matriz 
  r <- values(ra, format="matrix")
  # Neste caso, r vira uma matriz auxiliar
  
  # dimensão da paisagem
  l <- dim(r)[1]
  
  # Gera coordenadas inicias x e y para cada 
  # um dos n indivíduos simulados
  
  # Onde tem camera? 
  
  cam_coords <- sample(3:l-3,2*n_camera,replace=TRUE)
  M_CAM <- matrix(cam_coords, ncol=2)
  
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
  
  # Atualizando as posições
  result <- update_mov(M, time.step, time.step.adapt, l, r, landscape_on, M_CAM)
  
  # return from function R, traces
  
  # Exibindo o resultado
  #counts_wide <- NULL
  
  #mov$registros
  counts_summarised <- data.frame(result$registros)
  ####counts_summarised <- data.frame(mov$registros)
  counts_summarised$week <- round(counts_summarised$j/(24*7))
  counts_summarised <- counts_summarised %>% dplyr::group_by(k, week) %>% dplyr::summarise(counts = sum(contagem)) %>%
    dplyr::ungroup()
  
  counts_wide <- counts_summarised %>% tidyr::pivot_wider(names_from = week, values_from = counts, values_fill = 0)
  
  
  return(list("Evol"=result$R,"Traces"=result$traces,"time.step"=time.step,"time.step.adapt"=time.step.adapt, 
              "registros" = result$registros, "count_table" = counts_wide, "cam_position" = M_CAM))
} # fim for animal movements



# traces.viewer<-function(obj=NULL, r=NULL){
#   if(is.null(obj)){print("Erro. Inserir os resultados da simulação"); break}
#   # r = valores do raster (formato de matriz)
#   if(is.null(r)){print("Erro. Inserir os valores do raster no segundo argumento"); break}
#   #flip(t(rtest),direction='y')
#   plot(flip(t(r),direction='y'))+points(obj$Traces,cex=0.3,pch=19,col=1)
# }
# 
# tracks.set<-function(obj = NULL, ra = NULL, l = 30, sampling = "random", loc = NULL, con = 0.01){
#   # ntracks = number of transects
#   # l = length of transects 
#   # w = width of transects  
#   # r = valores do raster (formato de matriz)
#   if(is.null(obj)){print("Erro. Inserir o objeto tipo animal.mov"); break}
#   
#   
#   if(is.null(ra)){print("Erro. Inserir os valores do raster no segundo argumento"); break}
#   
#   ll<-dim(ra)[1]
#   
#   dir <- runif(1, 0, 360)
#   #if(is.null(loc)){loc<-round(runif(2,ll/9,ll*8/9))}  
#   
#   if(sampling=="random"||sampling=="convenience"){
#     if(sampling=="random"){
#       # spt = NULL -> then random locations
#       m <- matrix(0, ll, ll) # the empty landscape
#       r <- raster(m, xmn=0, xmx=ll, ymn=0, ymx=ll)
#       rtest <- makeLine(r, size = l, direction = dir, convol = con, spt = loc, edge = FALSE, bgr=0, rast = TRUE, val = 10)
#     } else{
#       rtest <- makeLine(ra, size = l, direction = dir, convol = con, spt = loc, edge = FALSE, bgr=7, rast = TRUE, val = 10)  
#     } # end first else
#   } else{
#     print("Error. Sampling design must be at random or by convenience")
#   } # end second else
#   
#   
#   area.t<-sum(values(rtest,format="matrix")==10)
#   coverage<-area.t/(ll^2)
#   coord<-NULL
#   for(i in 1:ll){
#     for(j in 1:ll){
#       if(rtest[i,j]==10){coord<-rbind(coord,c(i,j))}
#     }
#   }
#   
#   traces<-obj$Traces
#   counts<-0
#   for(k in 1:dim(traces)[1]){
#     if(rtest[traces[k,1],traces[k,2]]==10){counts<-counts+1}  
#   }
#   
#   plot(flip(t(ra),direction='y'))+points(coord[,1],coord[,2],col="blue",pch=15,type="p",xlim=c(0,ll),ylim=c(0,ll),cex=0.5)+points(traces,pch=15,cex=0.2)
#   return(list("transect"=rtest,"coverage"=coverage,"area"=area.t,
#               "counts"=counts))
# }





