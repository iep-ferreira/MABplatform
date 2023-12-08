# source codes - R functions

land.create <- function(l.size = 100, n.patches = c(2, 2, 2), cover = c(0.25, 0.25, 0.25)){
  
  # Cria valores de qualidade / recursos 
  val <- c(1, 3, 7)
  
  # Cover = cobertura de cada tipo de paisagem (soma < 1)
  if( sum(cover) > 1 ){print("Erro. Cobertura total não pode ser maior do que 1"); break}
  
  # Total de recursos 
  t.r <- val %*% cover
  
  #### Criando a matriz de recurso ####
  
  # criando uma paisagem vazia - só com zeros
  m <- matrix(0, l.size, l.size) # the empty landscape
  
  # transformando a matriz em raster
  r <- raster(m, xmn=0, xmx=l.size, ymn=0, ymx=l.size)
  
  # cria sobre o raster r manchas disjuntas que 
  # representam fragmentos pobres (i=1), 
  # intermediários (i=2) e ricos (i=3)
  for(i in 1:length(cover)){
    r <- makeClass(r, n.patches[i],
                   round(cover[i]*l.size^2/n.patches[i]), val = val[i])
  }
  
  # retorna o que é importante
  return( list("raster"=r,"recursos"=t.r) )
  
} # fim da função land.create

update_mov <- function(M = M, time.step = time.step, time.step.adapt = time.step.adapt,
                       l = l, r = r){
  
  R <- NULL
  MAUX <- M
  R[[1]] <- M
  n <- dim(M)[1]
  
  classe<-c(1,2,3,4,5,6,7,8,9)
  traces<-NULL
  for(j in 2:time.step){ 
    
    # Atualização 
    for(i in 1:n){ 
      x<-M[i,1]; y<-M[i,2]
      
      if(j>time.step.adapt&j%%6==0){traces<-rbind(traces,c(x,y))}
      
      if(x==1|x==l|y==1|y==l){
        yaux<-y; xaux<-x
        
        if(x==1){xaux<-x+1}
        if(x==l){xaux<-x-1}
        
        if(y==1){yaux<-y+1}
        if(y==l){yaux<-y-1}
        
        MAUX[i,]<-c(xaux,yaux)
      } else{
        
        # atualização dentro da matriz
        v.aux<-c(r[x-1,y+1],r[x,y+1],r[x+1,y+1], 
                 r[x-1,y],r[x,y],r[x+1,y],
                 r[x-1,y-1],r[x,y-1],r[x+1,y-1])
        if(sum(v.aux)>0){
          p.aux<-v.aux/sum(v.aux)
          cl<-sample(classe,1,prob=p.aux)
        } else{
          cl<-sample(classe,1)
        } 
        
        switch(cl,
               MAUX[i,]<-c(x-1,y+1), 
               MAUX[i,]<-c(x,y+1),
               MAUX[i,]<-c(x+1,y+1),
               MAUX[i,]<-c(x-1,y),
               MAUX[i,]<-c(x,y),
               MAUX[i,]<-c(x+1,y),
               MAUX[i,]<-c(x-1,y-1),
               MAUX[i,]<-c(x,y-1),
               MAUX[i,]<-c(x+1,y-1),
        )
        
      } # fim else 
    } # fim i
    M<-MAUX      
    R[[j]]<-M
  } # fim j   
  
  return(list("R"= R, "traces" = traces))
} # fim update for movements

animal.mov<-function( n = 100, ra = NULL, time = 21, where = "resource"){
  
  # Time in weeks (20 adaptation + 1 experiment)
  if(time<=1){print("Error. At least two weeks are necessary!"); break}
  
  # Total de tempo gasto nas simulações (em horas)
  time.step<-round(time)*7*24
  # Tempo total de adaptação (em horas)
  time.step.adapt<-(round(time)-1)*7*24
  
  # Número de animais (n)
  # r = valores do raster (formato de matriz)
  if(is.null(ra)){print("Erro. Inserir os valores do raster no segundo argumento"); break}
  
  # Extraindo valores do raster, na forma de matriz 
  r <- values(ra,format="matrix")
  # Neste caso, r vira uma matriz auxiliar
  
  # dimensão da paisagem
  l <- dim(r)[1]
  
  # Gera coordenadas inicias x e y para cada 
  # um dos n indivíduos simulados
  
  # Aleatoriamente ou só onde tem recursos?
  if(where=="random"||where=="resource"){
    
    if(where=="random"){ # distribuição espacial sem restrições
      # posição inicial dos indíviduos (x_0, y_0)
      
      sample_coords <- sample(1:l,2*n,replace=TRUE)
      
      M <- matrix(sample_coords, ncol=2)
      
    } else{ # entra no else se for "resource"
      
      # distribuição espacial apenas aonde tem recurso
      coords<-NULL
      for(i in 2:(l-1)){
        for(j in 2:(l-1)){
          if(r[i,j]>0){coords<-rbind(coords,c(i,j))}  
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
  result <- update_mov(M, time.step, time.step.adapt, l, r)
  
  # return from function R, traces
  
  return(list("Evol"=result$R,"Traces"=result$traces,"time.step"=time.step,"time.step.adapt"=time.step.adapt))
} # fim for animal movements

tracks.viewer<-function(obj=NULL, r=NULL, adapt=FALSE){
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
    for(j in 1:time.step){
      coords<-rbind(coords,obj$Evol[[j]][i,])
    }
    tracks[[i]]<-coords
  }
  if(adapt==FALSE){
    plot(flip(t(r),direction='y'))+for(i in 1:n) lines(tracks[[i]][(time.step.adapt+1):time.step,],type='l',cex=0.05,col=1)+points(tracks[[i]][time.step,1],tracks[[i]][time.step,2],cex=0.8,col=2,
                                                                                                                                   pch=19)} else{
                                                                                                                                     plot(flip(t(r),direction='y'))+for(i in 1:n) lines(tracks[[i]][1:time.step.adapt,],type='l',cex=0.05,col=4) + lines(tracks[[i]][(time.step.adapt+1):time.step,],type='l',cex=0.05,col=1)+
                                                                                                                                       points(tracks[[i]][time.step,1],tracks[[i]][time.step,2],cex=0.8,col=2,pch=19)  
                                                                                                                                   }
} # fim for tracks viewer

traces.viewer<-function(obj=NULL, r=NULL){
  if(is.null(obj)){print("Erro. Inserir os resultados da simulação"); break}
  # r = valores do raster (formato de matriz)
  if(is.null(r)){print("Erro. Inserir os valores do raster no segundo argumento"); break}
  #flip(t(rtest),direction='y')
  plot(flip(t(r),direction='y'))+points(obj$Traces,cex=0.3,pch=19,col=1)
}

tracks.set<-function(obj = NULL, ra = NULL, l = 30, sampling = "random", loc = NULL, con = 0.01){
  # ntracks = number of transects
  # l = length of transects 
  # w = width of transects  
  # r = valores do raster (formato de matriz)
  if(is.null(obj)){print("Erro. Inserir o objeto tipo animal.mov"); break}
  
  
  if(is.null(ra)){print("Erro. Inserir os valores do raster no segundo argumento"); break}
  
  ll<-dim(ra)[1]
  
  dir <- runif(1, 0, 360)
  #if(is.null(loc)){loc<-round(runif(2,ll/9,ll*8/9))}  
  
  if(sampling=="random"||sampling=="convenience"){
    if(sampling=="random"){
      # spt = NULL -> then random locations
      m <- matrix(0, ll, ll) # the empty landscape
      r <- raster(m, xmn=0, xmx=ll, ymn=0, ymx=ll)
      rtest <- makeLine(r, size = l, direction = dir, convol = con, spt = loc, edge = FALSE, bgr=0, rast = TRUE, val = 10)
    } else{
      rtest <- makeLine(ra, size = l, direction = dir, convol = con, spt = loc, edge = FALSE, bgr=7, rast = TRUE, val = 10)  
    } # end first else
  } else{
    print("Error. Sampling design must be at random or by convenience")
  } # end second else
  
  
  area.t<-sum(values(rtest,format="matrix")==10)
  coverage<-area.t/(ll^2)
  coord<-NULL
  for(i in 1:ll){
    for(j in 1:ll){
      if(rtest[i,j]==10){coord<-rbind(coord,c(i,j))}
    }
  }
  
  traces<-obj$Traces
  counts<-0
  for(k in 1:dim(traces)[1]){
    if(rtest[traces[k,1],traces[k,2]]==10){counts<-counts+1}  
  }
  
  plot(flip(t(ra),direction='y'))+points(coord[,1],coord[,2],col="blue",pch=15,type="p",xlim=c(0,ll),ylim=c(0,ll),cex=0.5)+points(traces,pch=15,cex=0.2)
  return(list("transect"=rtest,"coverage"=coverage,"area"=area.t,
              "counts"=counts))
}





