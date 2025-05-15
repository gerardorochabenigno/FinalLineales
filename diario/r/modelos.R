rm(list = rm())
library(tidyverse)
library(R2jags)


# Cargamos los datos
PATH = "../../data/raw"
df <- read_csv(file.path(PATH, "ops.csv"))
colnames(df) <- c('fecha', 'ops')
df <- df |> filter(fecha >= "2024-01-01")
head(df)

################################################################################
# JAGS. Modelo 1: Modelo de tendencia local

# Datos
y = df$ops
n = length(y)
k=20

y_train <- y[1:(n-k)]
y_test <- y[(n-k+1):n]

data <- list("y" = y_train,
             "y_test" = y_test,
             "n_train" = length(y_train), 
             'n_test' = k)

# Inits
inits <- function() {
  list(
    "tau.y" = 1,
    "tau.mu" = 1
  )
}

# Param para monitoreo
params <- c("mu", "yf1", "yf2", "tau.y", "tau.mu", "rmse_in", "rmse_out", "mae_in", "mae_out")

# Correr modelo
modelo <- jags(data = data,
               inits = inits,
               parameters.to.save = params,
               model.file = "./jags/nivel.txt",
               n.chains = 2,
               n.iter = 50000,
               n.burnin = 5000,
               n.thin = 2
               )

# Convergencia mu[t]
rhat_mu <- modelo$BUGSoutput$summary[grep("^mu\\[", rownames(modelo$BUGSoutput$summary)), "Rhat"]
plot(df$fecha, rhat_mu, type = "l", 
     main = expression(hat(R)~" para mu[t]"), 
     ylab = expression(hat(R)), xlab = "t",
     ylim = c(1, 1.02))

# Comvergencia de yf1[t]
rhat_yf1 <- modelo$BUGSoutput$summary[grep("^yf1\\[", rownames(modelo$BUGSoutput$summary)), "Rhat"]
plot(df$fecha[1:(n-k)] ,rhat_yf1, type = "l", 
     main = expression(hat(R)~" para yf1[t]"), 
     ylab = expression(hat(R)), xlab = "t",
     ylim = c(1, 1.02))

# Pseudo R²
yf1_mean <- apply(modelo$BUGSoutput$sims.list$yf1, 2, mean)
r2_in <- 1 - var(y_train - yf1_mean) / var(y_train)
cat("Pseudo R² dentro de la muestra:", r2_in, "\n")

# DIC
dic <- modelo$BUGSoutput$DIC

# Medidas dentro de la muestra
# Raiz del error cuadrático medio
rmse_in_mean <- mean(modelo$BUGSoutput$sims.list$rmse_in)
# Error absoluto medio
mae_in_mean <- mean(modelo$BUGSoutput$sims.list$mae_in)

# Medidas fuera de la muestra
# Raiz del error cuadrático medio
rmse_out_mean <- mean(modelo$BUGSoutput$sims.list$rmse_out)
# Error absoluto medio
mae_out_mean <- mean(modelo$BUGSoutput$sims.list$mae_out)


# Gráfice de serie vs yf1
yf1_samples <- modelo$BUGSoutput$sims.list$yf1
yf1_mean <- apply(yf1_samples, 2, mean)
yf1_lower <- apply(yf1_samples, 2, quantile, probs = 0.025)
yf1_upper <- apply(yf1_samples, 2, quantile, probs = 0.975)

plot(df$fecha[1:(n-k)], y_train, type = "l", 
     ylab = "Número de operaciones", xlab='',
     ylim = c(0, 25000))
lines(df$fecha[1:(n-k)], yf1_mean, col = "red")
polygon(c(df$fecha[1:(n-k)], rev(df$fecha[1:(n-k)])), 
        c(yf1_lower, rev(yf1_upper)), 
        col = rgb(0.6, 0.6, 1, 0.4), border = NA)


# Gráfica de yf2
yf2_samples <- modelo$BUGSoutput$sims.list$yf2
yf2_mean <- apply(yf2_samples, 2, mean)
yf2_lower <- apply(yf2_samples, 2, quantile, probs = 0.025)
yf2_upper <- apply(yf2_samples, 2, quantile, probs = 0.975)
plot(df$fecha[(n-k+1):n], y_test, type = "l", 
     ylab = "Número de operaciones", xlab='',
     ylim = c(0, 25000))
lines(df$fecha[(n-k+1):n], yf2_mean, col = "red")
polygon(c(df$fecha[(n-k+1):n], rev(df$fecha[(n-k+1):n])), 
        c(yf2_lower, rev(yf2_upper)), 
        col = rgb(0.6, 0.6, 1, 0.4), border = NA)


# Gráfica conjunta
yf_mean <- c(yf1_mean, yf2_mean)
yf_lower <- c(yf1_lower, yf2_lower)
yf_upper <- c(yf1_upper, yf2_upper)
plot(df$fecha, y, type = "l", 
     ylab = "Número de operaciones", xlab='',
     ylim = c(0, 25000))
lines(df$fecha, yf_mean, col = "red")
polygon(c(df$fecha, rev(df$fecha)), 
        c(yf_lower, rev(yf_upper)), 
        col = rgb(0.6, 0.6, 1, 0.4), border = NA)


################################################################################
# JAGS. Modelo 2: Modelo de tendencia local + crecimiento
# Datos
y = df$ops
n = length(y)
k=20

y_train <- y[1:(n-k)]
y_test <- y[(n-k+1):n]

data <- list("y" = y_train,
             "y_test" = y_test,
             "n_train" = length(y_train), 
             'n_test' = k)

# Inits
inits <- function() {
  list(
    "tau.y" = 1,
    "tau.mu" = 1,
    "tau.beta" = 1
  )
}

# Param para monitoreo
params <- c("mu", "yf1", "yf2", "tau.y", "tau.mu", "rmse_in", "rmse_out", "mae_in", "mae_out", "beta")

# Correr modelo
modelo <- jags(data = data,
               inits = inits,
               parameters.to.save = params,
               model.file = "./jags/nivel_pendiente.txt",
               n.chains = 2,
               n.iter = 100000,
               n.burnin = 10000,
               n.thin = 3
)

# Convergencia mu[t]
rhat_mu <- modelo$BUGSoutput$summary[grep("^mu\\[", rownames(modelo$BUGSoutput$summary)), "Rhat"]
plot(df$fecha, rhat_mu, type = "l", 
     main = expression(hat(R)~" para mu[t]"), 
     ylab = expression(hat(R)), xlab = "t",
     ylim = c(1, 1.02))

# Comvergencia beta[t]
rhat_beta <- modelo$BUGSoutput$summary[grep("^beta\\[", rownames(modelo$BUGSoutput$summary)), "Rhat"]
plot(df$fecha, rhat_beta, type = "l", 
     main = expression(hat(R)~" para beta[t]"), 
     ylab = expression(hat(R)), xlab = "t",
     #ylim = c(1, 1.02)
     )


# Comvergencia de yf1[t]
rhat_yf1 <- modelo$BUGSoutput$summary[grep("^yf1\\[", rownames(modelo$BUGSoutput$summary)), "Rhat"]
plot(df$fecha[1:(n-k)] ,rhat_yf1, type = "l", 
     main = expression(hat(R)~" para yf1[t]"), 
     ylab = expression(hat(R)), xlab = "t",
     ylim = c(1, 1.02))

# Pseudo R²
yf1_mean <- apply(modelo$BUGSoutput$sims.list$yf1, 2, mean)
r2_in <- 1 - var(y_train - yf1_mean) / var(y_train)
cat("Pseudo R² dentro de la muestra:", r2_in, "\n")

# DIC
dic <- modelo$BUGSoutput$DIC

# Medidas dentro de la muestra
# Raiz del error cuadrático medio
rmse_in_mean <- mean(modelo$BUGSoutput$sims.list$rmse_in)
# Error absoluto medio
mae_in_mean <- mean(modelo$BUGSoutput$sims.list$mae_in)

# Medidas fuera de la muestra
# Raiz del error cuadrático medio
rmse_out_mean <- mean(modelo$BUGSoutput$sims.list$rmse_out)
# Error absoluto medio
mae_out_mean <- mean(modelo$BUGSoutput$sims.list$mae_out)


# Gráfice de serie vs yf1
yf1_samples <- modelo$BUGSoutput$sims.list$yf1
yf1_mean <- apply(yf1_samples, 2, mean)
yf1_lower <- apply(yf1_samples, 2, quantile, probs = 0.025)
yf1_upper <- apply(yf1_samples, 2, quantile, probs = 0.975)

plot(df$fecha[1:(n-k)], y_train, type = "l", 
     ylab = "Número de operaciones", xlab='',
     ylim = c(0, 25000))
lines(df$fecha[1:(n-k)], yf1_mean, col = "red")
polygon(c(df$fecha[1:(n-k)], rev(df$fecha[1:(n-k)])), 
        c(yf1_lower, rev(yf1_upper)), 
        col = rgb(0.6, 0.6, 1, 0.4), border = NA)


# Gráfica de yf2
yf2_samples <- modelo$BUGSoutput$sims.list$yf2
yf2_mean <- apply(yf2_samples, 2, mean)
yf2_lower <- apply(yf2_samples, 2, quantile, probs = 0.025)
yf2_upper <- apply(yf2_samples, 2, quantile, probs = 0.975)
plot(df$fecha[(n-k+1):n], y_test, type = "l", 
     ylab = "Número de operaciones", xlab='',
     ylim = c(0, 25000))
lines(df$fecha[(n-k+1):n], yf2_mean, col = "red")
polygon(c(df$fecha[(n-k+1):n], rev(df$fecha[(n-k+1):n])), 
        c(yf2_lower, rev(yf2_upper)), 
        col = rgb(0.6, 0.6, 1, 0.4), border = NA)


# Gráfica conjunta
yf_mean <- c(yf1_mean, yf2_mean)
yf_lower <- c(yf1_lower, yf2_lower)
yf_upper <- c(yf1_upper, yf2_upper)
plot(df$fecha, y, type = "l", 
     ylab = "Número de operaciones", xlab='',
     ylim = c(0, 25000))
lines(df$fecha, yf_mean, col = "red")
polygon(c(df$fecha, rev(df$fecha)), 
        c(yf_lower, rev(yf_upper)), 
        col = rgb(0.6, 0.6, 1, 0.4), border = NA)

################################################################################
# JAGS. Modelo 3: Modelo de tendencia local + crecimiento + dummies de día 
# Datos
y = df$ops
n = length(y)
k=20

# Día de la semana
dow <- as.numeric(format(df$fecha, "%u"))  # 1 = lunes, ..., 7 = domingo
dow_factor <- factor(dow, levels = 1:7)
X <- model.matrix(~ dow_factor - 1)
colnames(X) <- c("Lun", "Mar", "Mie", "Jue", "Vie", "Sab", "Dom")
head(X)

y_train <- y[1:(n-k)]
y_test <- y[(n-k+1):n]

p <- ncol(X)

data <- list("y" = y_train,
             "y_test" = y_test,
             "n_train" = length(y_train), 
             'n_test' = k,
             "X" = X,
             "p" = p)

# Inits
inits <- function() {
  list(
    "tau.y" = 1,
    "tau.mu" = 1,
    "tau.beta" = 1
  )
}

# Param para monitoreo
params <- c("mu", "yf1", "yf2", "tau.y", "tau.mu", "rmse_in", "rmse_out", "mae_in", "mae_out", "beta", "alpha")

# Correr modelo
modelo <- jags(data = data,
               inits = inits,
               parameters.to.save = params,
               model.file = "./jags/poly_season_dia.txt",
               n.chains = 2,
               n.iter = 10000,
               n.burnin = 1000,
               n.thin = 2
)


# Convergencia mu[t]
rhat_mu <- modelo$BUGSoutput$summary[grep("^mu\\[", rownames(modelo$BUGSoutput$summary)), "Rhat"]
plot(df$fecha, rhat_mu, type = "l", 
     main = expression(hat(R)~" para mu[t]"), 
     ylab = expression(hat(R)), xlab = "t",
     ylim = c(1, 1.02))

# Comvergencia beta[t]
rhat_beta <- modelo$BUGSoutput$summary[grep("^beta\\[", rownames(modelo$BUGSoutput$summary)), "Rhat"]
plot(df$fecha, rhat_beta, type = "l", 
     main = expression(hat(R)~" para beta[t]"), 
     ylab = expression(hat(R)), xlab = "t",
     #ylim = c(1, 1.02)
)


# Comvergencia de yf1[t]
rhat_yf1 <- modelo$BUGSoutput$summary[grep("^yf1\\[", rownames(modelo$BUGSoutput$summary)), "Rhat"]
plot(df$fecha[1:(n-k)] ,rhat_yf1, type = "l", 
     main = expression(hat(R)~" para yf1[t]"), 
     ylab = expression(hat(R)), xlab = "t",
     ylim = c(1, 1.02))

# Pseudo R²
yf1_mean <- apply(modelo$BUGSoutput$sims.list$yf1, 2, mean)
r2_in <- 1 - var(y_train - yf1_mean) / var(y_train)
cat("Pseudo R² dentro de la muestra:", r2_in, "\n")

# DIC
dic <- modelo$BUGSoutput$DIC

# Medidas dentro de la muestra
# Raiz del error cuadrático medio
rmse_in_mean <- mean(modelo$BUGSoutput$sims.list$rmse_in)
# Error absoluto medio
mae_in_mean <- mean(modelo$BUGSoutput$sims.list$mae_in)

# Medidas fuera de la muestra
# Raiz del error cuadrático medio
rmse_out_mean <- mean(modelo$BUGSoutput$sims.list$rmse_out)
# Error absoluto medio
mae_out_mean <- mean(modelo$BUGSoutput$sims.list$mae_out)


# Gráfice de serie vs yf1
yf1_samples <- modelo$BUGSoutput$sims.list$yf1
yf1_mean <- apply(yf1_samples, 2, mean)
yf1_lower <- apply(yf1_samples, 2, quantile, probs = 0.025)
yf1_upper <- apply(yf1_samples, 2, quantile, probs = 0.975)

plot(df$fecha[1:(n-k)], y_train, type = "l", 
     ylab = "Número de operaciones", xlab='',
     ylim = c(0, 25000))
lines(df$fecha[1:(n-k)], yf1_mean, col = "red", lty=2)
polygon(c(df$fecha[1:(n-k)], rev(df$fecha[1:(n-k)])), 
        c(yf1_lower, rev(yf1_upper)), 
        col = rgb(0.6, 0.6, 1, 0.4), border = NA)


# Gráfica de yf2
yf2_samples <- modelo$BUGSoutput$sims.list$yf2
yf2_mean <- apply(yf2_samples, 2, mean)
yf2_lower <- apply(yf2_samples, 2, quantile, probs = 0.025)
yf2_upper <- apply(yf2_samples, 2, quantile, probs = 0.975)
plot(df$fecha[(n-k+1):n], y_test, type = "l", 
     ylab = "Número de operaciones", xlab='',
     ylim = c(0, 25000))
lines(df$fecha[(n-k+1):n], yf2_mean, col = "red", lty=2)
polygon(c(df$fecha[(n-k+1):n], rev(df$fecha[(n-k+1):n])), 
        c(yf2_lower, rev(yf2_upper)), 
        col = rgb(0.6, 0.6, 1, 0.4), border = NA)


# Gráfica conjunta
yf_mean <- c(yf1_mean, yf2_mean)
yf_lower <- c(yf1_lower, yf2_lower)
yf_upper <- c(yf1_upper, yf2_upper)
plot(df$fecha, y, type = "l", 
     ylab = "Número de operaciones", xlab='',
     ylim = c(0, 25000))
lines(df$fecha, yf_mean, col = "red")
polygon(c(df$fecha, rev(df$fecha)), 
        c(yf_lower, rev(yf_upper)), 
        col = rgb(0.6, 0.6, 1, 0.4), border = NA)

################################################################################
# JAGS. Modelo 3: Modelo de tendencia local + crecimiento + dummies de día + dummy de feriado
# Datos
y = df$ops
n = length(y)
k=20

# Día de la semana
dow <- as.numeric(format(df$fecha, "%u"))  # 1 = lunes, ..., 7 = domingo
dow_factor <- factor(dow, levels = 1:7)


# Día feriado
festivos <- as.Date(c(
  # 2019
  "2019-01-01", "2019-02-05", "2019-03-18", "2019-05-01", "2019-09-16", "2019-11-18", "2019-12-25",
  # 2020
  "2020-01-01", "2020-02-03", "2020-03-16", "2020-05-01", "2020-09-16", "2020-11-16", "2020-12-25",
  # 2021
  "2021-01-01", "2021-02-01", "2021-03-15", "2021-05-01", "2021-09-16", "2021-11-15", "2021-12-25",
  # 2022
  "2022-01-01", "2022-02-07", "2022-03-21", "2022-05-01", "2022-09-16", "2022-11-21", "2022-12-25",
  # 2023
  "2023-01-01", "2023-02-06", "2023-03-20", "2023-05-01", "2023-09-16", "2023-11-20", "2023-12-25",
  # 2024
  "2024-01-01", "2024-02-05", "2024-03-18", "2024-05-01", "2024-06-02", "2024-09-16", "2024-10-01", "2024-11-18", "2024-12-25",
  # 2025
  "2025-01-01", "2025-02-03", "2025-03-17", "2025-05-01", "2025-09-16", "2025-11-17", "2025-12-25"
))

festivo <- as.numeric(df$fecha %in% festivos)

X <- cbind(model.matrix(~ dow_factor - 1), festivo)

colnames(X) <- c("Lun", "Mar", "Mie", "Jue", "Vie", "Sab", "Dom", "Fest")
head(X)

y_train <- y[1:(n-k)]
y_test <- y[(n-k+1):n]
p <- ncol(X)

data <- list("y" = y_train,
             "y_test" = y_test,
             "n_train" = length(y_train), 
             'n_test' = k,
             "X" = X,
             "p" = p)
# Inits
inits <- function() {
  list(
    "tau.y" = 1,
    "tau.mu" = 1,
    "tau.beta" = 1
  )
}

# Param para monitoreo
params <- c("mu", "yf1", "yf2", "tau.y", "tau.mu", "rmse_in", "rmse_out", "mae_in", "mae_out", "beta", "alpha")

# Correr modelo
modelo <- jags(data = data,
               inits = inits,
               parameters.to.save = params,
               model.file = "./jags/poly_season_dia.txt",
               n.chains = 2,
               n.iter = 10000,
               n.burnin = 1000,
               n.thin = 2
)


# Convergencia mu[t]
rhat_mu <- modelo$BUGSoutput$summary[grep("^mu\\[", rownames(modelo$BUGSoutput$summary)), "Rhat"]
plot(df$fecha, rhat_mu, type = "l", 
     main = expression(hat(R)~" para mu[t]"), 
     ylab = expression(hat(R)), xlab = "t",
     ylim = c(1, 1.02))

# Comvergencia beta[t]
rhat_beta <- modelo$BUGSoutput$summary[grep("^beta\\[", rownames(modelo$BUGSoutput$summary)), "Rhat"]
plot(df$fecha, rhat_beta, type = "l", 
     main = expression(hat(R)~" para beta[t]"), 
     ylab = expression(hat(R)), xlab = "t",
     #ylim = c(1, 1.02)
)


# Comvergencia de yf1[t]
rhat_yf1 <- modelo$BUGSoutput$summary[grep("^yf1\\[", rownames(modelo$BUGSoutput$summary)), "Rhat"]
plot(df$fecha[1:(n-k)] ,rhat_yf1, type = "l", 
     main = expression(hat(R)~" para yf1[t]"), 
     ylab = expression(hat(R)), xlab = "t",
     ylim = c(1, 1.02))

# Pseudo R²
yf1_mean <- apply(modelo$BUGSoutput$sims.list$yf1, 2, mean)
r2_in <- 1 - var(y_train - yf1_mean) / var(y_train)
cat("Pseudo R² dentro de la muestra:", r2_in, "\n")

# DIC
dic <- modelo$BUGSoutput$DIC

# Medidas dentro de la muestra
# Raiz del error cuadrático medio
rmse_in_mean <- mean(modelo$BUGSoutput$sims.list$rmse_in)
# Error absoluto medio
mae_in_mean <- mean(modelo$BUGSoutput$sims.list$mae_in)

# Medidas fuera de la muestra
# Raiz del error cuadrático medio
rmse_out_mean <- mean(modelo$BUGSoutput$sims.list$rmse_out)
# Error absoluto medio
mae_out_mean <- mean(modelo$BUGSoutput$sims.list$mae_out)


# Gráfice de serie vs yf1
yf1_samples <- modelo$BUGSoutput$sims.list$yf1
yf1_mean <- apply(yf1_samples, 2, mean)
yf1_lower <- apply(yf1_samples, 2, quantile, probs = 0.025)
yf1_upper <- apply(yf1_samples, 2, quantile, probs = 0.975)

plot(df$fecha[1:(n-k)], y_train, type = "l", 
     ylab = "Número de operaciones", xlab='',
     ylim = c(0, 25000))
lines(df$fecha[1:(n-k)], yf1_mean, col = "red", lty=2)
polygon(c(df$fecha[1:(n-k)], rev(df$fecha[1:(n-k)])), 
        c(yf1_lower, rev(yf1_upper)), 
        col = rgb(0.6, 0.6, 1, 0.4), border = NA)


# Gráfica de yf2
yf2_samples <- modelo$BUGSoutput$sims.list$yf2
yf2_mean <- apply(yf2_samples, 2, mean)
yf2_lower <- apply(yf2_samples, 2, quantile, probs = 0.025)
yf2_upper <- apply(yf2_samples, 2, quantile, probs = 0.975)
plot(df$fecha[(n-k+1):n], y_test, type = "l", 
     ylab = "Número de operaciones", xlab='',
     ylim = c(0, 25000))
lines(df$fecha[(n-k+1):n], yf2_mean, col = "red", lty=2)
polygon(c(df$fecha[(n-k+1):n], rev(df$fecha[(n-k+1):n])), 
        c(yf2_lower, rev(yf2_upper)), 
        col = rgb(0.6, 0.6, 1, 0.4), border = NA)


# Gráfica conjunta
yf_mean <- c(yf1_mean, yf2_mean)
yf_lower <- c(yf1_lower, yf2_lower)
yf_upper <- c(yf1_upper, yf2_upper)
plot(df$fecha, y, type = "l", 
     ylab = "Número de operaciones", xlab='',
     ylim = c(0, 25000))
lines(df$fecha, yf_mean, col = "red")
polygon(c(df$fecha, rev(df$fecha)), 
        c(yf_lower, rev(yf_upper)), 
        col = rgb(0.6, 0.6, 1, 0.4), border = NA)



