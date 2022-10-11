################################################################################
# Modelo para desarroladores
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# Oct 8, 2022
#
# Description
#
################################################################################

## SET UP ######################################################################

# Definir parámetros -----------------------------------------------------------
r <- 0.1 # tasa de crecimiento
n_steps <- 20 #número de rondas

# Crear vectors para definir espacio en memoria.
# Cada elemento corresponde a una ronda

# Vector de población
N <- numeric(n_steps)
N[1] <-100 # Población inicial

# Vector de variabilidad ambiental en el reclutemiento
m <- 1
s <- 0.1
gamma <- rlnorm(n = n_steps,
                meanlog=log(m^2 / sqrt(s^2 + m^2)), 
                sdlog=sqrt(log(1 + (s^2 / m^2))))


# Vector de eventos de mortalidad
mu <- rbinom(n = n_steps, size = 1, prob = 0.1) * 0.5#
mu[1:10] <- 0 # las primeras 10 rondas no hay mortalidad

# Crear una matriz de capturas. Cada fila es una ronda
h <- matrix(nrow = n_steps, ncol = 5)




# Iterar en el tiempo ----------------------------------------------------------
# En el caso de la app no sería un ciclo for, si no un while en lo que el pescador juega
for (t in 1:n_steps) {
  
  # Definir capturas de esta ronda
  h[t, 1] <- 4 # Este es el valor que debe ser ingresado por el usuario
  h[t, 2:5] <- pmin(rpois(n = 4, lambda = 2), 5) # En este ejemplo los "bots" son numeros aleatorios de una distribución poisson. Pero podemos formalizarlo con una equación. Lo limitamos a un máximo de 5
  H <- sum(h[t, ]) # Suma de todas las capturas
  
  # Calcular tamaño de población después de capturas
  E <- max(N[t] - H, 0) # Población restante, no puede ser negativa
  
  # Calcular el tamaño de población para la siguiente ronda
  N[t + 1] <- (1 + r) * gamma[t] * E * (1 - mu[t])
}


# Crear una tabla con 9 columna:
# - ronda
# - población de la ronda
# - valor de gamma
# - valor dee mu
# - esfuerzos históricos de cada "pescador"
data.frame(ronda = 1:n_steps,
           N = N[1:20],
           gamma, 
           mu,
           h)



