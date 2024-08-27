land_create <- function(l.size = 100, n.patches = c(2, 2, 2), cover = c(0.25, 0.25, 0.25)){
  
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