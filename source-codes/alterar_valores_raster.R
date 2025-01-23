# Função para alterar valores em um raster
alterar_valores_raster <- function(raster_obj, valores_antigos, novos_valores) {
  # Verificar se os comprimentos de 'valores_antigos' e 'novos_valores' são iguais
  if (length(valores_antigos) != length(novos_valores)) {
    stop("Os vetores 'valores_antigos' e 'novos_valores' devem ter o mesmo comprimento.")
  }
  
  # Criar uma matriz de reclassificação
  matriz_reclass <- cbind(valores_antigos, novos_valores)
  
  # Reclassificar o raster
  raster_reclass <- reclassify(raster_obj, matriz_reclass)
  
  return(raster_reclass)
}