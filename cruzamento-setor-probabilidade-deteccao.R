set.seed(123)  # para reprodutibilidade

# ---------------------------
# 1) Parâmetros da simulação
# ---------------------------
n_passos  <- 20000       # número de passos do random walk
L         <- 400         # tamanho do lado da caixa
step_mean <- 20.8        # passo médio
R_setor   <- 30          # raio do setor
ang_min   <- 0           # setor de ângulo de pi/2
ang_max   <- pi/2
N_subdiv <- 1000
alpha <- 5

# Ponto central do setor
cx <- 100
cy <- 100

# ---------------------------
# 2) Função para refletir ao ultrapassar paredes
# ---------------------------
refletir_parede <- function(x, limite) {
  # Se x < 0, x passa para |x|
  # Se x > limite, x passa para 2*limite - x
  # 
  # Observação: se um passo "exceder muito" o limite,
  # pode ser preciso checar várias reflexões sucessivas
  # caso o passo seja maior que 2*limite. Aqui faremos
  # enquanto x estiver fora dos limites.
  
  while (x < 0 || x > limite) {
    if (x < 0) {
      x <- abs(x)       # espelha para dentro
    } else if (x > limite) {
      x <- 2*limite - x
    }
  }
  return(x)
}

# ---------------------------
# 3) Função para verificar se
#    um ponto (x,y) está no setor
# ---------------------------
esta_no_setor <- function(x, y, cx, cy, R, ang_min, ang_max) {
  dx <- x - cx
  dy <- y - cy
  r  <- sqrt(dx^2 + dy^2)
  
  # Ângulo no plano [0, 2*pi)
  # atan2(dy, dx) retorna algo em [-pi, pi]
  theta <- atan2(dy, dx)
  if (theta < 0) theta <- theta + 2*pi
  
  # Está dentro do raio e dentro do setor angular?
  cond_raio  <- (r <= R)
  cond_angulo <- (theta >= ang_min) && (theta <= ang_max)
  
  return(cond_raio && cond_angulo)
}

# ---------------------------
# 4) Simulação do Random Walk
# ---------------------------

# 4.1) Posição inicial no centro (poderia ser outro lugar, se quiser)
x_atual <- L/2
y_atual <- L/2

# Contador para "número de cruzadas sem repouso"
count_cruzada <- 0
count_detec <- 0

for(i in seq_len(n_passos)) {
  direcao <- runif(1, 0, 2*pi)
  passo <- rpareto_media(1, step_mean, alpha)
  x_novo <- x_atual + passo*cos(direcao)
  y_novo <- y_atual + passo*sin(direcao)
  x_novo <- refletir_parede(x_novo, L)
  y_novo <- refletir_parede(y_novo, L)
  inicial_no_setor <- esta_no_setor(x_atual, y_atual, cx, cy, R_setor, ang_min, ang_max)
  final_no_setor   <- esta_no_setor(x_novo,  y_novo,  cx, cy, R_setor, ang_min, ang_max)
  xm <- (x_atual + x_novo) / 2
  ym <- (y_atual + y_novo) / 2
  meio_no_setor <- esta_no_setor(xm, ym, cx, cy, R_setor, ang_min, ang_max)
  
  if(!inicial_no_setor && !final_no_setor ) {
    
    if( meio_no_setor ){
    count_detec <- count_detec + 1
    } else{
      encontrou <- FALSE
      for(k in seq_len(N_subdiv)) {
        t <- k / (N_subdiv + 1)  # t varia de 1/(N+1), 2/(N+1), ..., N/(N+1)
        x_mid <- x_atual + t*(x_novo - x_atual)
        y_mid <- y_atual + t*(y_novo - y_atual)
        
        # Se este subponto está no setor, marcamos e paramos
        if(esta_no_setor(x_mid, y_mid, cx, cy, R_setor, ang_min, ang_max)) {
          encontrou <- TRUE
          count_cruzada <- count_cruzada + 1
          break
        } # fim-if
      } # fim-for
      
    } # fim-else
      
 } # fim-if
  
  # Atualiza posição
  x_atual <- x_novo
  y_atual <- y_novo
} # fim-function

# ---------------------------
# 5) Proporção de cruzamentos
# ---------------------------
(proporcao <- count_detec / (count_cruzada + count_detec) )

