tracks.viewer<-function(obj=NULL, r=NULL, adapt=FALSE, biomastats_std = TRUE){
  
  if(is.null(obj)){print("Erro. Inserir os resultados da simulação"); break}
  # r = valores do raster (formato de matriz)
  if(is.null(r)){print("Erro. Inserir os valores do raster no segundo argumento"); break}
  
  # number of animals / tracks
  n<-dim(obj$Evol[[1]])[1]
  time.step<-obj$time.step
  time.step.adapt<-obj$time.step.adapt
  tracks<-NULL
  for(i in 1:n){
    coords<-NULL
    for(j in 1:(time.step-1)){
      coords<-rbind(coords,obj$Evol[[j]][i,])
    }
    tracks[[i]]<-coords
  }
  
  if(biomastats_std == FALSE){  
    
    if(adapt==FALSE){
      plot(flip(t(r),direction='y'))+
        for(i in 1:n) lines(tracks[[i]][(time.step.adapt):(time.step-1),],type='l',cex=0.05,col=1)+
        points(tracks[[i]][(time.step-1),1],tracks[[i]][(time.step-1),2],cex=0.8, col=2, pch=19)} 
    else{
      plot(flip(t(r),direction='y'))+for(i in 1:n) lines(tracks[[i]][1:time.step.adapt,],type='l',cex=0.05,col=4) +
        lines(tracks[[i]][(time.step.adapt):(time.step-1),],type='l',cex=0.05,col=1) 
      points(tracks[[i]][(time.step-1),1],tracks[[i]][(time.step-1),2],cex=0.8,col=2,pch=19)                                                                                                                                 }
    
  } else{
    
    tracks_df <- data.frame()
    
    for(i in 1:n){
      coords <- data.frame(
        x = numeric(),
        y = numeric(),
        animal = integer(),
        step = integer()
      )
      for(j in (time.step.adapt):(time.step-1)){
        coords <- rbind(coords, data.frame(
          x = obj$Evol[[j]][i, 1],
          y = obj$Evol[[j]][i, 2],
          animal = i,
          step = j
        ))
      }
      tracks_df <- rbind(tracks_df, coords)
    }
    
    # Adiciona a última posição de cada animal
    end_points_df <- data.frame()
    
    for(i in 1:n){
      end_points_df <- rbind(end_points_df, data.frame(
        x = obj$Evol[[time.step-1]][i, 1],
        y = obj$Evol[[time.step-1]][i, 2],
        animal = i
      ))
    }
    
    # dicionário de classes e cores
    dic <- biomastats:::dict_build()
    raster <- r
    dd <- data.frame(table(raster::getValues(raster)))
    colnames(dd) <- c("class", "area")
    names_classes <- NULL
    cores_classes <- NULL
    for (j in 1:length(dd$class)) {
      pos <- dd$class[j] == dic$code
      names_classes[j] <- dic$class[pos]
      cores_classes[j] <- dic$color[pos]
    }
    raster <- as(raster, "SpatialPixelsDataFrame")
    dat <- as.data.frame(raster)
    names(dat) <- c("value", "x", "y")
    p <- ggplot2::ggplot(data = dat, ggplot2::aes(x = x, y = y, 
                                                  fill = factor(value))) + ggplot2::geom_tile() +
      ggplot2::scale_fill_manual(values = cores_classes, name = "Land Uses", 
                                 labels = names_classes, guide = ggplot2::guide_legend(reverse = TRUE)) +
      ggplot2::geom_point(data = end_points_df, aes(x = x, y = y), color = "red", size = 2, inherit.aes = FALSE) + 
      ggplot2::geom_path(data = tracks_df, aes(x = x, y = y, group = animal), color = "black", linewidth = 0.5,inherit.aes = FALSE) 
    p  
    
    
  } # fim else   
  
} # fim for tracks viewer