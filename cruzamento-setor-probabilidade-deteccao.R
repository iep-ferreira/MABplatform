
## library(animation) 

rpareto_media <- function(n, media, alpha) {
  if (alpha <= 1) stop("O parâmetro alpha deve ser maior que 1 para a média existir.")
  
  # Calcular o parâmetro x_m a partir da média desejada
  x_m <- (media * (alpha - 1)) / alpha
  
  # Gerar números da distribuição de Pareto
  U <- runif(n)  # Números uniformes entre 0 e 1
  step_size <- x_m * (1 - U)^(-1 / alpha)
  
  return(step_size)
}

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

# Centro do setor (por exemplo, o centro da caixa)
cx <- L/2
cy <- L/2

# ---------------------------
# 2) Função para refletir nas paredes
# ---------------------------
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

# ---------------------------
# 3) Função: está_no_setor?
# ---------------------------
esta_no_setor <- function(x, y, cx, cy, R, ang_min, ang_max) {
  dx <- x - cx
  dy <- y - cy
  rr <- sqrt(dx^2 + dy^2)
  
  # Cálculo do ângulo (ajustado para [0, 2*pi))
  theta <- atan2(dy, dx)
  if (theta < 0) theta <- theta + 2*pi
  
  dentro_raio   <- (rr <= R)
  dentro_angulo <- (theta >= ang_min) && (theta <= ang_max)
  
  return(dentro_raio && dentro_angulo)
}

# ---------------------------
# 4) Preparação para a animação
# ---------------------------
# Posição inicial
x_atual <- L/5
y_atual <- L/5

# Variável para contabilizar cruzamentos
count_cruzadas <- 0
count_detections <- 0
# Vetores para armazenar as posições (úteis para desenhar a trajetória)
x_positions <- numeric(n_passos)
y_positions <- numeric(n_passos)
x_positions[1] <- x_atual
y_positions[1] <- y_atual

# Definir quais passos serão plotados (aqui, a cada 100 passos)
plot_steps <- seq(1, n_passos, by = 1)

# ---------------------------
# 5) Animação com saveGIF
# ---------------------------
##saveGIF({ 
  
  for(i in seq_len(n_passos)) {
    if(i%%1000 == 0){ print(100*i/n_passos) }
    # 5.1) Sorteia direção e define o passo
    direcao <- runif(1, 0, 2*pi)
    passo <- rpareto_media(1, step_mean, alpha)
    #step_mean  # ou use uma distribuição, ex: rnorm(1, mean = step_mean, sd = 5)
    
    # 5.2) Calcula nova posição (antes da reflexão)
    x_novo <- x_atual + passo * cos(direcao)
    y_novo <- y_atual + passo * sin(direcao)
    
    # 5.3) Aplica reflexões se ultrapassar as bordas
    x_novo <- refletir_parede(x_novo, L)
    y_novo <- refletir_parede(y_novo, L)
    
    # 5.4) Verifica se o segmento atravessa o setor
    inicial_no_setor <- esta_no_setor(x_atual, y_atual, cx, cy, R_setor, ang_min, ang_max)
    final_no_setor   <- esta_no_setor(x_novo, y_novo, cx, cy, R_setor, ang_min, ang_max)
    
    if(final_no_setor){ count_detections <- count_detections + 1}
    
    encontrou <- FALSE
    if (!inicial_no_setor && !final_no_setor) {
      # Subdivide o segmento e verifica cada ponto intermediário
      for(k in seq_len(N_subdiv)) {
        t <- k / (N_subdiv + 1)  # t varia entre 0 e 1
        x_mid <- x_atual + t * (x_novo - x_atual)
        y_mid <- y_atual + t * (y_novo - y_atual)
        if (esta_no_setor(x_mid, y_mid, cx, cy, R_setor, ang_min, ang_max)) {
          encontrou <- TRUE
          break
        }
      }
      if (encontrou) {
        count_cruzadas <- count_cruzadas + 1
      }
    }
    
    # 5.5) Atualiza posição e armazena para a trajetória
    x_atual <- x_novo
    y_atual <- y_novo
    x_positions[i] <- x_atual
    y_positions[i] <- y_atual
    
    # 5.6) Plotar apenas alguns frames para a animação
  #  if (i %in% plot_steps) {
  #    plot(NA, xlim = c(0, L), ylim = c(0, L),
  #         xlab = "x (m)", ylab = "y (m)", main = paste("Passo", i))
  #    # Desenha a caixa
  #    rect(0, 0, L, L, border = "black", lwd = 2)
      
  #    # Desenha o setor circular (setor de ângulo [ang_min, ang_max])
  #    angulos <- seq(ang_min, ang_max, length.out = 100)
  #    polygon(c(cx, cx + R_setor * cos(angulos)),
  #            c(cy, cy + R_setor * sin(angulos)),
  #            col = rgb(0, 1, 0, 0.3), border = NA)
      
      # Desenha a trajetória percorrida
  #    if(i > 1) {
  #    start <- 1
  #    if(i>40){start <- i-30}  
  #    lines(x_positions[start:i], y_positions[start:i], col = "grey", lwd = 0.1, lty = 3)
  #    }
      
      # Marca a posição atual da partícula
   #   points(x_atual, y_atual, col = "red", pch = 16, cex = 1.5)
      
      # Exibe a proporção de cruzamentos até o momento
     # proporcao <- count_detections / (count_cruzadas + count_detections) 
    #  texto <- paste("Passo:", i, "\nProporção:", round(100*proporcao, 1))
    #  text(10, L - 10, labels = texto, adj = 0)
#    }
  }

##  }, movie.name = "random_walk.gif", interval = 0.1, ani.width = 600, ani.height = 600)

  #passo <- rpareto_media(1, step_mean, alpha)
count_detections
count_cruzadas

# proporcao de detecçao efetiva
(proporcao <- count_detections / (count_cruzadas + count_detections) )

# no REM, o que importa é ponto dentro do setor naquele frame, ou seja, descondideramos cruzadas indetectadas
(REM_est  <- pi*count_detections/ (step_mean*n_passos*R_setor*(ang_max+2)) ) 

# observe que REM_est e a densidade são iguais
(densidade <- 1/(L^2)) 
# 1.6x10^-5

# número esperado de detecções
11.27*((step_mean/1000)*n_passos*(R_setor/1000)*(ang_max+2))/pi

