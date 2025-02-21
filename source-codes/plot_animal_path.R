plot_animal_path <- function(trajetory_obj, landscape = NULL, animal = c(1,3), weeks = c(1, 2), 
                             alpha_raster = 0.40, alpha_path = 0.9){
  
  #traj_obj <-traj %>% filter(id_numeric %in% c(1,2,3)) %>% filter(week %in% c(1,2,3)) 
  traj_obj <- trajetory_obj %>% filter(id_numeric %in% animal) %>% filter(week %in% weeks)  
  
  # para camadas de uso e ocupação
  
  if(!is.null(landscape)){
    # Para mapa de uso e ocupação
    mapa_delim <- landscape
    mapa_df <- as.data.frame(mapa_delim, xy = TRUE)
    colnames(mapa_df) <- c("x", "y", "value")
    dic <- biomastats::dict_build()
    valores_unicos <- unique(values(mapa_delim))
    valores_unicos <- valores_unicos[!is.na(valores_unicos)]  # Remover NAs, se houver
    dd <- data.frame(class = valores_unicos)
    dd$class <- as.numeric(as.character(dd$class))
    dd$color <- sapply(dd$class, function(x) {
      pos <- which(dic$code == x)
      if (length(pos) > 0) dic$color[pos] else NA
    })
    paleta_cores <- dd$color
    names(paleta_cores) <- dd$class
    mapa_df$color <- paleta_cores[as.character(mapa_df$value)]
    
    p <- ggplot(data = mapa_df, aes(x = x, y = y)) + 
      geom_path(data = traj_obj, aes(
                color = factor(week),  
                group = interaction(id_numeric, week)), 
                alpha= alpha_path) + # Usar geom_path para respeitar a ordem dos dados
      scale_color_manual(values = rainbow(length(unique(traj_obj$week)))) + 
      geom_raster(data = mapa_df, aes(x = x, y = y, fill = color), alpha = alpha_raster) +    scale_fill_identity() 
    labs(
      title = "Trajectory",
      x = "Coordenada X",
      y = "Coordenada Y",
      color = "Weeks"
    ) +
      theme_minimal() + # Tema limpo
      theme(
        plot.title = element_text(hjust = 0.5),
        legend.position = "right"
      )
    
    print(p)
    
  } else{
    p <- ggplot(traj_obj, aes(x = x, y = y)) + 
      geom_path(aes(color = factor(week),  group = interaction(id_numeric, week)), alpha=0.5) + # Usar geom_path para respeitar a ordem dos dados
      scale_color_manual(values = rainbow(length(unique(traj_obj$week)))) + # Definir cores para semanas
      labs(
        title = "Trajectory",
        x = "Coordenada X",
        y = "Coordenada Y",
        color = "Weeks"
      ) +
      theme_minimal() + # Tema limpo
      theme(
        plot.title = element_text(hjust = 0.5),
        legend.position = "right"
      )
    
    ggplotly(p)
  } # end-else
  
} # end-function