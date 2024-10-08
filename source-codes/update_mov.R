update_mov <- function(M = M, time.step = time.step, time.step.adapt = time.step.adapt,
                       l = l, r = r, landscape_on = TRUE, M_CAM){
  
  R <- NULL
  MAUX <- M
  R[[1]] <- M
  n <- dim(M)[1]
  
  # classifica��es de uso e ocupa��o
  classe <- 1:62
  # peso - qualidade de ocupa��o
  weights_classes <- rep(0.1, 62) 
  weights_classes[3] <- 10 # floresta natural 
  weights_classes[9] <- 8 # floresta plantada
  weights_classes[11] <- 7 # �reas alagadas
  weights_classes[15] <- 2 # pastagens 
  weights_classes[21] <- 3 # mosaico de usos
  weights_classes[33] <- 1 # corpos h�dricos
  # consideramos a movimenta��o no per�odo produtivo
  weights_classes[39] <- 3 # soja
  weights_classes[41] <- 4 # outros cultivos tempor�rios
  
  traces<-NULL
  registro <- NULL
  for(j in 2:(time.step-1)){ 
    
    if(j>time.step.adapt){
      # considerando probabilidade de detec��o de 100%
      for(k in 1:dim(M_CAM)[1]){
        linha <- M_CAM[k,]
        # Comparando cada linha da matriz com o vetor
        linhas_iguais <- apply(M, 1, function(aux_linha) all(aux_linha == linha))
        # Contabilizando quantas linhas s�o iguais ao vetor
        contagem <- sum(linhas_iguais)
        registro <- rbind(registro, cbind(k, j, contagem) )
      }
    }
    
  
    # Atualiza��o 
    for(i in 1:n){ 
      x<-M[i,1]; y<-M[i,2]
      
      #if(j>time.step.adapt){
      # traces<-rbind(traces,c(x,y))
      #}
      
      if(x==1|x==l|y==1|y==l){
        yaux<-y; xaux<-x
        
        if(x==1){xaux<-x+1}
        if(x==l){xaux<-x-1}
        
        if(y==1){yaux<-y+1}
        if(y==l){yaux<-y-1}
        
        MAUX[i,]<-c(xaux,yaux)
      } else{
        
        # atualiza��o dentro da matriz
        v.aux<-c(r[x-1,y+1],r[x,y+1],r[x+1,y+1], 
                 r[x-1,y],r[x,y],r[x+1,y],
                 r[x-1,y-1],r[x,y-1],r[x+1,y-1])
      
        if(landscape_on == TRUE){
          
          v.aux[ v.aux == 0 ] <- 1 # para n�o ter zeros no v.aux
          
          v.weights <- weights_classes[v.aux]  
        } else{
          v.weights <- rep(1,9)
        } 
        
        if(sum(v.weights)>0){
          p.weights <- v.weights/sum(v.weights) # normaliza��o dos pesos 
          cl<-sample(1:9, 1, prob = p.weights)
        } else{
          cl<-sample(1:9,1)
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
               MAUX[i,]<-c(x+1,y-1)
        )
        
        
      } # fim else 
    } # fim i
    M<-MAUX      
    R[[j]]<-M
  } # fim j   
  
## names(registro) <- c("Camera_id", "Time", "Counts")  
  
  return(list("R"= R, "traces" = traces, "registros" = registro ))
} # fim update for movements