calc_centroid_radius <- function(data, animal = NULL, weeks = NULL, ft = 28.8/1000) {
  # Filtrar pelos animais e semanas escolhidos (se fornecidos)
  if (!is.null(animal)) {
    data <- data %>% filter(id_numeric %in% animal)
  }
  if (!is.null(weeks)) {
    data <- data %>% filter(week %in% weeks)
  }
  
  # Aplicar fator de escala **apenas para cálculo de centróide e raio de dispersão**
  centroids <- data %>%
    mutate(
      x_scaled = x * ft,
      y_scaled = y * ft
    ) %>%
    group_by(id_numeric) %>%
    summarise(
      centroid_x = mean(x_scaled, na.rm = TRUE),
      centroid_y = mean(y_scaled, na.rm = TRUE),
      radius_dispersion = mean(sqrt((x_scaled - centroid_x)^2 + (y_scaled - centroid_y)^2), na.rm = TRUE),
      .groups = "drop"
    )
  
  # Criar pontos sf **sem aplicar o fator de escala** para os MCPs
  points_sf <- st_as_sf(data, coords = c("x", "y"), crs = NA)  # Sem definir CRS
  
  # Calcular MCP para cada animal (usando coordenadas originais)
  mcp_polygons <- points_sf %>%
    group_by(id_numeric) %>%
    summarise(geometry = st_convex_hull(st_union(geometry)), .groups = "drop") %>%
    mutate(area_units = st_area(geometry))  # Área do polígono na unidade original dos dados
  
  # **Cálculo do diâmetro máximo do polígono (MCP)**
  mcp_polygons <- mcp_polygons %>%
    rowwise() %>%
    mutate(
      max_diameter = ft*max(st_distance(st_cast(geometry, "POINT")))  # Calcula a maior distância entre dois pontos
    ) %>%
    ungroup()
  
  # **Cálculo da trilha e distância total percorrida**
  distance_per_animal <- data %>%
    arrange(id_numeric, step) %>%  # Ordenar por animal e tempo
    group_by(id_numeric) %>%
    mutate(
      step_length = sqrt((x - lag(x))^2 + (y - lag(y))^2) * ft  # Distância entre pontos consecutivos
    ) %>%
    summarise(
      total_distance = sum(step_length, na.rm = TRUE),  # Somar distâncias para cada animal
      .groups = "drop"
    )
  
  # Retornar lista com ambos os resultados
  return(list(centroids = centroids, mcp_polygons = mcp_polygons, distances = distance_per_animal))
  
}