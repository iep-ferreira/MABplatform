# Instalar e carregar o pacote necessário
#if(!require(actuar)) install.packages("actuar", dependencies = TRUE)
library(actuar)

# Definir os parâmetros
num_bolinhas <- 30000
num_potes <- 2661
num_amostras <- 289
num_simulacoes <- 10000  # Número de repetições para obter a distribuição da média

# Distribuir as bolinhas de forma desigual (usando uma distribuição de Pareto)
shape_param <- 1.2  # Parâmetro de forma da distribuição de Pareto
bolinhas_por_pote <- rpareto(num_potes, shape = shape_param, scale = 1)
bolinhas_por_pote <- (bolinhas_por_pote / sum(bolinhas_por_pote)) * num_bolinhas

# Simular a seleção de potes e calcular a média
medias_amostrais <- numeric(num_simulacoes)
for (i in 1:num_simulacoes) {
  amostra <- sample(bolinhas_por_pote, num_amostras, replace = FALSE)
  medias_amostrais[i] <- mean(amostra)
}

# Plotar a distribuição da média
hist(medias_amostrais, breaks = 50, probability = TRUE, col = "blue", 
     main = "Distribuição da Média Amostral", xlab = "Média das bolinhas por pote")

# Ajustar uma curva normal para comparar
(mu <- mean(medias_amostrais))
sigma <- sd(medias_amostrais)
x <- seq(min(medias_amostrais), max(medias_amostrais), length.out = 100)
y <- dnorm(x, mean = mu, sd = sigma)
lines(x, y, col = "red", lwd = 2)
legend("topright", legend = sprintf("N(%.2f, %.2f)", mu, sigma), col = "red", lwd = 2)
