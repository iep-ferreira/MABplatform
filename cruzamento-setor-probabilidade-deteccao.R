#library(animation) 

# passos com distriobuicao de Pareto
rpareto_media <- function(n, media, alpha) {
  if (alpha <= 1) stop("O parâmetro alpha deve ser maior que 1 para a média existir.")
  x_m <- (media * (alpha - 1)) / alpha
  U <- runif(n)  
  step_size <- x_m * (1 - U)^(-1 / alpha)
  return(step_size)
}

# 2) Funcao para refletir nas paredes
refletir_parede <- function(coord, limite) {
  while (coord < 0 || coord > limite) {
    if (coord < 0) {
      coord <- -coord
    } else if (coord > limite) {
      coord <- 2 * limite - coord
    }
  }
  coord
}

# funcao - verifica se particula está dentro do setor
esta_no_setor <- function(x, y, cx, cy, R, ang_min, ang_max) {
  dx <- x - cx
  dy <- y - cy
  rr <- sqrt(dx^2 + dy^2)
  theta <- atan2(dy, dx)
  if (theta < 0) theta <- theta + 2*pi
  dentro_raio   <- (rr <= R)
  dentro_angulo <- (theta >= ang_min) && (theta <= ang_max)
  return(dentro_raio && dentro_angulo)
}


# funcao - simular particula 
particle_counts <- function( 
    n_particles = 1, exclusion_window = 0, n_steps  = n_passos, L = 250, step_mean = step_mean, R_setor = R_setor, 
    ang_min = ang_min, ang_max = ang_max, N_subdiv = N_subdiv, alpha = alpha, 
    cruzadas = FALSE, chart = FALSE  
    ){
  
  # capturando os parametros de entrada para retorno 
  params <- list(
    n_particles = n_particles,
    exclusion_window = exclusion_window,
    n_steps = n_steps,
    L = L,
    step_mean = step_mean,
    R_setor = R_setor,
    ang_min = ang_min,
    ang_max = ang_max,
    N_subdiv = N_subdiv,
    alpha = alpha,
    cruzadas = cruzadas,
    chart = chart
  )
  
  # centralizacao da camera e posicao inicial da paricula 
  cx <- L/2; cy <- L/2
 
  # inicializacao de vetores e contadores
  count_cruzadas <- 0
  count_detections <- 0
  time_stamp_detections <- NULL
  x_positions <- numeric(n_steps)
  y_positions <- numeric(n_steps)
  
  for(j in 1:n_particles){ 
 
    x_atual <- runif(1, 0, L); y_atual <- runif(1, 0, L)
     
  x_positions[1] <- x_atual
  y_positions[1] <- y_atual

  if(j == 1 & chart == TRUE){
  
  saveGIF({ 
  
  ## processamento  

  for(i in seq_len(n_steps)) {
    if(i%%1000 == 0){ print(100*i/n_steps) }
    direcao <- runif(1, 0, 2*pi)
    passo <- rpareto_media(1, step_mean, alpha)
    x_novo <- x_atual + passo * cos(direcao)
    y_novo <- y_atual + passo * sin(direcao)
    x_novo <- refletir_parede(x_novo, L)
    y_novo <- refletir_parede(y_novo, L)
    atual_no_setor   <- esta_no_setor(x_novo, y_novo, cx, cy, R_setor, ang_min, ang_max)
    if(atual_no_setor){ 
      
    count_detections <- count_detections + 1
    time_stamp_detections <- c(time_stamp_detections, i)
  
    }
    
    if(cruzadas == TRUE){

    inicial_no_setor <- esta_no_setor(x_atual, y_atual, cx, cy, R_setor, ang_min, ang_max)
    encontrou <- FALSE
    if (!inicial_no_setor && !atual_no_setor) {
        # Subdivide o segmento e verifica cada ponto intermediário
        for(k in seq_len(N_subdiv)) {
        t <- k / (N_subdiv + 1)  # t varia entre 0 e 1
        x_mid <- x_atual + t * (x_novo - x_atual)
        y_mid <- y_atual + t * (y_novo - y_atual)
              if (esta_no_setor(x_mid, y_mid, cx, cy, R_setor, ang_min, ang_max)) {
              encontrou <- TRUE
              break
              } # end-if-esta_no_setor
         
        } # end-for-k
      
      if (encontrou) {
      count_cruzadas <- count_cruzadas + 1
      } # end-if-encontrou
    
    } # end - if - !inicial_no_setor && !final_no_setor
      
    } # end-if-counts-cruzadas
    
    x_atual <- x_novo
    y_atual <- y_novo
    x_positions[i] <- x_atual
    y_positions[i] <- y_atual
    
    ## graficos 
    
    plot(NA, xlim = c(0, L), ylim = c(0, L),
    xlab = "x (m)", ylab = "y (m)", main = paste("Passo", i))
    # Desenha a caixa e o setor circular
    rect(0, 0, L, L, border = "black", lwd = 2)
    angulos <- seq(ang_min, ang_max, length.out = 100)
    polygon(c(cx, cx + R_setor * cos(angulos)),
    c(cy, cy + R_setor * sin(angulos)),
    col = rgb(0, 1, 0, 0.3), border = NA)
    # tracks
        if(i > 1) {
        start <- 1
        if(i>40){start <- i-30}  
        lines(x_positions[start:i], y_positions[start:i], col = "grey", lwd = 0.1, lty = 3)
        } # end-if-i>1
    points(x_atual, y_atual, col = "red", pch = 16, cex = 1.5)
    
    } # end-for-i-steps 
  
  }, movie.name = "random_walk.gif", interval = 0.1, ani.width = 600, ani.height = 600)
    
  } else{ # end-if-chart-processing
  
    ## somente processamento  
    
    for(i in seq_len(n_steps)) {
      if(i%%1000 == 0){ print(100*i/n_steps) }
      direcao <- runif(1, 0, 2*pi)
      passo <- rpareto_media(1, step_mean, alpha)
      x_novo <- x_atual + passo * cos(direcao)
      y_novo <- y_atual + passo * sin(direcao)
      x_novo <- refletir_parede(x_novo, L)
      y_novo <- refletir_parede(y_novo, L)
      atual_no_setor   <- esta_no_setor(x_novo, y_novo, cx, cy, R_setor, ang_min, ang_max)
      if(atual_no_setor){ 
        
        count_detections <- count_detections + 1
        time_stamp_detections <- c(time_stamp_detections, i)
        
      }
      
      if(cruzadas == TRUE){
        
        inicial_no_setor <- esta_no_setor(x_atual, y_atual, cx, cy, R_setor, ang_min, ang_max)
        encontrou <- FALSE
        if (!inicial_no_setor && !atual_no_setor) {
          # Subdivide o segmento e verifica cada ponto intermediário
          for(k in seq_len(N_subdiv)) {
            t <- k / (N_subdiv + 1)  # t varia entre 0 e 1
            x_mid <- x_atual + t * (x_novo - x_atual)
            y_mid <- y_atual + t * (y_novo - y_atual)
            if (esta_no_setor(x_mid, y_mid, cx, cy, R_setor, ang_min, ang_max)) {
              encontrou <- TRUE
              break
            } # end-if-esta_no_setor
            
          } # end-for-k
          
          if (encontrou) {
            count_cruzadas <- count_cruzadas + 1
          } # end-if-encontrou
          
        } # end - if - !inicial_no_setor && !final_no_setor
        
      } # end-if-counts-cruzadas
      
      x_atual <- x_novo
      y_atual <- y_novo
      x_positions[i] <- x_atual
      y_positions[i] <- y_atual
    } # end-for-i-steps 
  
    count_cruzadas <- NULL

    } # end-else-chart-processing
    
  } # end-for-j-n_particles  
  
  REM_est  <- pi*count_detections/ ((step_mean/1000)*n_steps*(R_setor/1000)*(ang_max+2))  
  n_particles_est <- REM_est*(L^2/10^6) 
  true_density <- n_particles/(L^2/10^6) 
  exp_detections <- true_density*((step_mean/1000)*n_steps*(R_setor/1000)*(ang_max+2))/pi
  
  # implementar janela de exclusão usando o tim_stamp_counts!!!
  time_stamp_detections <- sort(time_stamp_detections)
  counts_w_exclusion <- c()
  latency_time <- 0
  last_detection <- -Inf  
  detection_table <- data.frame("time_stamp" = NULL, "detected" = NULL)
  for (time_stamp in time_stamp_detections) {
    aux <- "Registered"
    if (time_stamp - last_detection >= exclusion_window) {
      counts_w_exclusion <- c(counts_w_exclusion, time_stamp)
      last_detection <- time_stamp
      latency_time <- latency_time + exclusion_window
      aux <- "Excluded"
    }
    detection_table <- rbind(detection_table, c(time_stamp, aux))
  }
  colnames(detection_table) <- c("detection_time_stamp", "status")
    
  # valores atualizados pela janela de exclusao
  detections_w_eclusion <- length(counts_w_exclusion)
  REM_with_exclusion  <- pi*detections_w_eclusion/((step_mean/1000)*n_steps*(R_setor/1000)*(ang_max+2))  
  n_particles_rem_w_exclusion <- REM_with_exclusion*(L^2/10^6) 
  exclusion_percentage <- 100*(count_detections - detections_w_eclusion) / count_detections 
  # e possivel corrigir o REM pela janela de exclusao? 
  REM_ex_wind_correction  <- pi*detections_w_eclusion/((step_mean/1000)*(n_steps-latency_time)*(R_setor/1000)*(ang_max+2))
  REM_particles_corrected_by_exclusion = REM_ex_wind_correction*(L^2/10^6)
  
  return(
  list(total_counts = count_detections,
       total_counts_w_exclusion = detections_w_eclusion,  
       true_density = true_density, 
       exp_detections = exp_detections, 
       REM_withou_exclusion_window = REM_est, 
       REM_with_exclusion = REM_with_exclusion,
       REM_ex_wind_correction = REM_ex_wind_correction,
       detections_time_stamps = time_stamp_detections,
       counts_w_exclusion = counts_w_exclusion, 
       REM_particles_without_exclusion = n_particles_est, 
       REM_particles_with_exclusion = n_particles_rem_w_exclusion,
       REM_particles_corrected_by_exclusion = REM_particles_corrected_by_exclusion, 
       exclusion_percentage = exclusion_percentage,
       detection_table = detection_table,
       input_params = params
       )
  )  
    
} # end-function 


# ---------------------------
# 1) Parâmetros da simulação
# ---------------------------
n_passos  <- 7*3*12*60       # número de passos
L         <- 250         # tamanho do lado da caixa
step_mean <- 20.82        # passo médio
R_setor   <- 30          # raio do setor
ang_min   <- 0           # ângulo mínimo do setor (0 rad)
ang_max   <- pi/2        # ângulo máximo do setor (pi/2 rad)
N_subdiv <- 500
alpha <- 5

# funcao - simular particula 
teste <- particle_counts( 
    n_particles = 45, exclusion_window = 60, n_steps  = 10*n_passos, L = 2000, 
    step_mean = step_mean, R_setor = R_setor, 
    ang_min = ang_min, ang_max = ang_max, N_subdiv = N_subdiv, alpha = alpha, 
    cruzadas = FALSE, chart = FALSE  
)

# Verificando as contagens com e sem exclusao
teste$total_counts
teste$exp_detections
teste$total_counts_w_exclusion
teste$exclusion_percentage
#sum(teste$detection_table$X0L == 0) # conferido

# Estimativas pelo REM
teste$true_density
teste$REM_withou_exclusion_window
teste$REM_with_exclusion
teste$REM_ex_wind_correction

# Numero de particulas na rede
teste$REM_particles_without_exclusion
teste$REM_particles_with_exclusion

# tabela de deteccao
head(teste$detection_table,12)

### Estudo 1 - Efeito da janela de exclusao em diferentes densidades
set.seed(22041500) # semente - descobrimento do Brasil
library(openxlsx)

# Vetores com as condições a variar
particles_vec <- c(15, 45, 80)
exclusion_vec <- c(0, 5, 15, 30, 60)
n_reps <- 50

#arquivo_excel <- "pre-processamento/estudo_janela_exclusao.xlsx"
#wb <- createWorkbook()
#addWorksheet(wb, "Resultados")

cabecalhos <- data.frame(
  repeticao = NA,
  n_particles = NA,
  exclusion_window = NA,
  total_counts = NA,
  exp_detections = NA,
  total_counts_w_exclusion = NA,
  exclusion_percentage = NA,
  true_density = NA,
  REM_without_exclusion_window = NA,
  REM_with_exclusion = NA,
  REM_ex_wind_correction = NA,
  REM_particles_without_exclusion = NA,
  REM_particles_with_exclusion = NA,
  n_steps  = NA,
  L = NA,
  step_mean = NA,
  R_setor = NA
)
#writeData(wb, sheet = "Resultados", x = cabecalhos, startRow = 1, colNames = TRUE)
current_row <- nrow(cabecalhos) + 1


for (p in particles_vec) {
  for (w in exclusion_vec) {
    for (i in 1:n_reps) {
      teste <- particle_counts(
        n_particles = p, 
        exclusion_window = w, 
        n_steps  = n_passos, 
        L = 2000, 
        step_mean = step_mean, 
        R_setor = R_setor, 
        ang_min = ang_min, 
        ang_max = ang_max, 
        N_subdiv = N_subdiv, 
        alpha = alpha, 
        cruzadas = FALSE, 
        chart = FALSE  
      )
      res <- data.frame(
        repeticao = i,
        n_particles = p,
        exclusion_window = w,
        total_counts = teste$total_counts,
        exp_detections = teste$exp_detections,
        total_counts_w_exclusion = teste$total_counts_w_exclusion,
        exclusion_percentage = teste$exclusion_percentage,
        true_density = teste$true_density,
        REM_without_exclusion_window = teste$REM_withou_exclusion_window,
        REM_with_exclusion = teste$REM_with_exclusion,
        REM_ex_wind_correction = teste$REM_ex_wind_correction,
        REM_particles_without_exclusion = teste$REM_particles_without_exclusion,
        REM_particles_with_exclusion = teste$REM_particles_with_exclusion, 
        n_steps  =  n_passos, 
        L = 2000, 
        step_mean = step_mean, 
        R_setor = R_setor
      )
  
      #writeData(wb, sheet = "Resultados", x = res, startRow = current_row, colNames = FALSE)
      #current_row <- current_row + 1
      #saveWorkbook(wb, file = arquivo_excel, overwrite = TRUE)
    }
  }

}




