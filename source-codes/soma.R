# fun��o soma de ind�viduos que entram no pixel da c�mera com base na densidade populacional
soma <- function(n){
  if(n<=1){stop("Error.")}
  aux <- 0
  for(r in 0:(n-1)){
    aux <- aux + (2*r+1)*(1-exp(-n/(r+1)^2))
  }
  return(aux)
}
# teste
