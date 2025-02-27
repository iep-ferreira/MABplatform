camera_attributes <- function(displacement = 100, 
                              habitat_weigth = NULL, 
                              mov_vector = NULL, 
                              mov_norm = NULL, 
                              limit_x = c(100, 1700), 
                              limit_y = c(100, 1700), 
                              orientation_angle = NULL,
                              viewing_angle = pi/2, 
                              field_depth = 15
                              ){

# Parâmetros do problema
limit_x <- c(100, 1700)
limit_y <- c(100, 1700)
displacement <- 100  # Distância fixa entre pontos

# Criar sequência de pontos regularmente espaçados
x_seq <- seq(limit_x[1], limit_x[2], by = displacement)
y_seq <- seq(limit_y[1], limit_y[2], by = displacement)

# Criar a grade combinando todas as coordenadas
grid_points <- expand.grid(x = x_seq, y = y_seq)

# Converter para data.frame
pontos_df <- as.data.frame(grid_points)

if(is.null(orientation_angle)){
pontos_df$orientation_angle <- runif(length(pontos_df$x) , 0, 2*pi)
}

## adicionando características das câmeras 
resolucao <- res(mov_vector)
xminimo <- xmin(mov_vector); ymaximo <- ymax(mov_vector)

habitat_list <- NULL
norm_list <- NULL
vector_list <- NULL
for(linhas in 1:dim(pontos_df)[1]){
x <- pontos_df$x[linhas]
y <- pontos_df$y[linhas]
coords_ij <- xy_to_ij(floor(x), floor(y), xminimo, ymaximo, resolucao)$ij

habitat_list[linhas] <- habitat_weigth[coords_ij[1], coords_ij[2]] 
norm_list[linhas] <- mov_norm[coords_ij[1], coords_ij[2]]  
vector_list[linhas] <- mov_vector[coords_ij[1], coords_ij[2]]      
} # end-for

pontos_df$viewing_angle = rep(viewing_angle, length(pontos_df$x) ) 
pontos_df$field_depth = rep(field_depth, length(pontos_df$x) )
pontos_df$habitat_weights = habitat_list
pontos_df$mov_norm = norm_list
pontos_df$mov_vector = vector_list

p <- ggplot(pontos_df, aes(x = x, y = y)) +
  geom_point(color = "blue", size = 2) +  # Pontos azuis de tamanho 2
  labs(title = "Grade de pontos igualmente espaçados", 
       x = "X", y = "Y") +
  theme_minimal() +  # Tema limpo
  coord_fixed()  # Mantém a proporção correta dos eixos


return(list(cam_attributes_table = pontos_df, grid_plot = p))
}


