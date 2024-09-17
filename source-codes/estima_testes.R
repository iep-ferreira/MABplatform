estima_testes <- function(sim, ra = ra, true_value = true_value, time = time, time_adapt, where = "random", landscape_on = FALSE, n_camera = n_camera){ 
  
  # em km^2
  l <- dim(ra)[1]
  (area_total <- (900*l^2)/1000000)
  # número de quadrículas por km^2
  (n_q <- 1000000/900)
  # densidade populacional
  (densi.pop <- true_value/area_total) 
  # densidade por quadrícula 
  (lambda_q <- densi.pop/l_q^2)
  # área da quadrícula em km^2
  a_q <- 900/1000000
  k <- time*7*24
  numero_magico <- 1.20
  raio <- sqrt(k)
  area_influencia <- pi*(numero_magico*raio)^2
  area_influencia_km <- area_influencia / 1000000 
  
  est_um <- NULL
  #prob_est <- NULL
  #est_dois <- NULL
  cam_positions <- NULL
  error_indica <- NULL
  est_prob <- NULL
  est_lambda <- NULL
  raio <- sqrt(time*7*24)
  area_influencia <- pi*raio^2
  
  for(i in 1:sim){
    mov <- animal_mov(n = true_value, ra = ra, time = time, time_adapt = time_adapt, where = "random", landscape_on = FALSE, n_camera = n_camera)
    tryCatch({
    # Criação do objeto unmarkedFramePCount
    umf_abund <- unmarkedFramePCount(y = mov$count_table[,-1])
    # Modelo de abundância com pcount
    abund_model <- pcount(~ 1 ~ 1, data = umf_abund)
    est_prob[i] <- plogis(coef(abund_model)[2])
    est_lambda[i] <- exp(coef(abund_model)[1])  
    
    m_counts <- exp(coef(abund_model)[1])*plogis(coef(abund_model)[2])
    
    est_um[i] <- m_counts / (a_q*k)
    
    #est_dois[i] <- m_counts / (area_influencia_km*k) 

    cam_positions[[i]] <- mov$cam_position
    error_indica[i] <- "sucesso"
    }, error = function(e) {
      # Tratamento em caso de erro
      message(paste("Erro na iteração", i, ": ", e$message))
      error_indica[i] <- "erro"
      est_prob[i] <- NA
      est_lambda[i] <- NA  
      m_counts <- NA
      est_um[i] <- NA
      #est_dois[i] <- NA 
      cam_positions[[i]] <- mov$cam_position
  
    })
    
  }
  
  return(list("simulacoes" = sim,
              "where" = where, 
              "landscape_on" = landscape_on, 
              "n_camera" = n_camera,
              "erros" = error_indica,
              "time_span" = time,
              "time_steps" = k,
              "square_size" = l,
              "real_pop_density" = densi.pop,
              "desn.quadricula" = lambda_q,
              "total_area" = area_total,
              "area_quadricula" = a_q, 
              "number_camera" = n_camera,
              "true_population" = true_value,
              "numero_magico" = numero_magico,
              "est_um" = est_um,
              #"est_dois" = est_dois, 
              "lambda_est" = est_lambda,
              "detection_prob" = est_prob, 
              "camera_position" = cam_positions 
              ) 
  )
}

