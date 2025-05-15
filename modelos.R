## -- Código para correr modelos de CoDi

library(dplyr)
library(R2jags)

wdir<-"~/Documents/ITAM/ModelosLineales/proyecto_mlg/FinalLineales/data"
setwd(wdir)
# Leer datos
df <- read.csv("clean_data.csv")



# Lista para JAGS (5 bancos: BBVA, BanCoppel, Azteca, Otros, STP)
data_jags <- list(
  N = nrow(df),
  B = 5,
  # Montos en millones (evitar overflow)
  monto = df %>% select(monto_bbva, monto_bancoppel, monto_azteca, monto_otros, monto_stp) %>% 
    mutate_all(~ ./1e6) %>% as.matrix(),
  # Predictores estandarizados
  cuentas = df %>% select(cuentas_bbva, cuentas_bancoppel, cuentas_azteca, cuentas_otros, cuentas_stp) %>% 
    scale() %>% as.matrix(),
  operaciones = df %>% select(operaciones_bbva, operaciones_bancoppel, operaciones_azteca, operaciones_otros, operaciones_stp) %>% 
    scale() %>% as.matrix()
)


inits <- function() {
  list(
    alpha = runif(5, 1, 2),  # Interceptos positivos
    mu_alpha = runif(1, 1, 2),
    sigma_alpha = runif(1, 0.5, 1.5),
    beta1 = rnorm(1, 0, 0.1),
    beta2 = rnorm(1, 0, 0.1),
    sigma = runif(1, 0.5, 2)
  )
}

parameters <- c("alpha", "beta1", "beta2", "mu_alpha", "sigma_alpha", "sigma")

fit <- jags(
  data = data_jags,
  inits = inits,
  parameters.to.save = parameters,
  model.file = "modelo_jerarquico.txt",
  n.chains = 3,
  n.iter = 15000,
  n.burnin = 5000,
  n.thin = 2
)


# Resumen
print(fit)
fit
# Diagnósticos
traceplot(fit, varname = c("alpha", "beta1", "sigma"))

# Gráfico de efectos por banco
library(ggplot2)
alpha_samples <- as.data.frame(fit$BUGSoutput$sims.list$alpha)
colnames(alpha_samples) <- c("BBVA", "BanCoppel", "Azteca", "Otros", "STP")
boxplot(alpha_samples, main = "Interceptos por Banco")




--------
library(R2jags)
  

# Datos para JAGS (solo BBVA)
data_jags <- list(
  N = nrow(df),
  y = df$monto_bbva / 1e6,  # Escalar a millones
  cuentas = scale(df$cuentas_bbva)[, 1],  # Estandarizado
  operaciones = scale(df$operaciones_bbva)[, 1]
)

# Modelo Gamma con link log (archivo "modelo_gamma.txt")
model_string <- "
model {
  # Likelihood
  for (i in 1:N) {
    y[i] ~ dgamma(shape[i], rate[i])
    shape[i] <- mu[i]^2 / sigma^2
    rate[i] <- mu[i] / sigma^2
    log(mu[i]) <- alpha + beta1 * cuentas[i] + beta2 * operaciones[i]
  }
  
  # Priors
  alpha ~ dnorm(0, 0.001)
  beta1 ~ dnorm(0, 0.001)
  beta2 ~ dnorm(0, 0.001)
  sigma ~ dunif(0, 10)
}
"
writeLines(model_string, "modelo_gamma.txt")

# Inicialización
inits <- function() {
  list(
    alpha = rnorm(1, mean = 1, sd = 1),
    beta1 = rnorm(1, mean = 0, sd = 0.5),
    beta2 = rnorm(1, mean = 0, sd = 0.5),
    sigma = runif(1, 0.5, 2)
  )
}

# Parámetros a monitorear
parameters <- c("alpha", "beta1", "beta2", "sigma")

# Ejecutar modelo
fit <- jags(
  data = data_jags,
  inits = inits,
  parameters.to.save = parameters,
  model.file = "modelo_gamma.txt",
  n.chains = 3,
  n.iter = 10000,
  n.burnin = 3000,
  n.thin = 2
)

# Resultados
print(fit)


data_jags <- list(
  N = nrow(df),
  B = 5,
  y = as.matrix(df %>% select(monto_bbva, monto_bancoppel, monto_azteca, monto_otros, monto_stp) / 1e6),
  x = as.matrix(df %>% select(operaciones_bbva, operaciones_bancoppel, operaciones_azteca, operaciones_otros, operaciones_stp))
)



model_string <- "
model {
  for (i in 1:N) {
    for (j in 1:B) {
      y[i,j] ~ dgamma(shape[i,j], rate[i,j])
      shape[i,j] <- mu[i,j]^2 / sigma^2
      rate[i,j] <- mu[i,j] / sigma^2
      log(mu[i,j]) <- alpha[j] + beta * x[i,j]
    }
  }
  for (j in 1:B) {
    alpha[j] ~ dnorm(mu_alpha, tau_alpha)
  }
  mu_alpha ~ dnorm(0, 0.001)
  tau_alpha <- 1/pow(sigma_alpha, 2)
  sigma_alpha ~ dunif(0, 10)
  beta ~ dnorm(0, 0.001)
  sigma ~ dunif(0, 10)
}
"




-------
  
library(R2jags)

library(R2jags)


# Datos para JAGS (solo BBVA)
data_jags <- list(
  N = nrow(df),  # Número de observaciones (meses)
  B = 5,         # Número de bancos (BBVA, BanCoppel, Azteca, Otros, STP)
  monto = as.matrix(df[, c("monto_bbva", "monto_bancoppel", "monto_azteca", "monto_otros", "monto_stp")] / 1e6),  # Escalado
  operaciones = as.matrix(scale(df[, c("operaciones_bbva", "operaciones_bancoppel", "operaciones_azteca", "operaciones_otros", "operaciones_stp")]))
)
model_gamma<- "
model {
  # Likelihood
  for (i in 1:N) {
    for (j in 1:B) {
      monto[i,j] ~ dgamma(shape[i,j], rate[i,j])
      shape[i,j] <- mu[i,j]^2 / sigma2
      rate[i,j] <- mu[i,j] / sigma2
      log(mu[i,j]) <- alpha[j] + beta * operaciones[i,j]  # Usa 'operaciones' (no 'x')
    }
  }
  
  # Efectos aleatorios por banco
  for (j in 1:B) {
    alpha[j] ~ dnorm(mu_alpha, tau_alpha)
  }
  
  # Hiperparámetros
  mu_alpha ~ dnorm(0, 0.001)
  tau_alpha <- 1 / pow(sigma_alpha, 2)
  sigma_alpha ~ dunif(0, 10)
  
  # Efecto fijo global
  beta ~ dnorm(0, 0.001)
  
  # Dispersión
  sigma2 <- pow(sigma, 2)
  sigma ~ dunif(0, 10)
}
"

writeLines(model_string, "modelo_gamma.txt")
inits <- function() {
  list(
    alpha = runif(5, 1, 2),          # Interceptos positivos por banco
    mu_alpha = runif(1, 1, 2),       # Media global de interceptos
    sigma_alpha = runif(1, 0.5, 1.5), # Variabilidad entre bancos
    beta = rnorm(1, 0, 0.5),         # Efecto de operaciones
    sigma = runif(1, 0.5, 2)         # Dispersión
  )
}

parameters <- c("alpha", "beta", "mu_alpha", "sigma_alpha", "sigma")


fit <- jags(
  data = data_jags,
  inits = inits,
  parameters.to.save = parameters,
  model.file = "modelo_gamma.txt",
  n.chains = 3,
  n.iter = 15000,
  n.burnin = 5000,
  n.thin = 2
)
