rm(list=ls())
library(R2jags)
library(dlm)
library(forecast)
library(ggplot2)
library(tidyverse)
library(timeDate)

# Modelo DLM usando dlms
df <- read_csv('./python/data_total_ops.csv')
df <- df |>
  mutate(dow = wday(fecha, label = TRUE))

df <- df |> filter(fecha >= '2024-01-01')

festivos <- c(
  as.Date(c(
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
)

df$festivo <- df$fecha %in% festivos
df$quiebre1 <- df$fecha >= '2024-06-15' & df$fecha < '2024-12-31'

# Usaremos un modelo de tendencia y estacionalidad usando DLM
y <- df$total_ops

build_dlm <- function(pars, xreg) {
  mod_trend <- dlmModPoly(order = 2, dV = exp(pars[1]), dW = exp(pars[2:3]))
  mod_seas  <- dlmModSeas(frequency = 7, dV = 0, dW = rep(exp(pars[4]), 6))
  mod_reg   <- dlmModReg(X = xreg, dV = 0, addInt = FALSE, dW = exp(pars[5]))
  
  return(mod_trend + mod_seas + mod_reg)
}

# Calculamos valores de MLE
xreg <- as.numeric(df$festivo)
init_pars <- log(c(var(y), 0.01, 0.01, 0.1, 0.1))  # dV, dW_nivel, dW_pendiente, dW_seas, dW_reg

build_wrap <- function(pars) build_dlm(pars, xreg)
fit <- dlmMLE(y, parm = init_pars, build = build_wrap)
pars_mle <- fit$par

# Estiamos el modelo
mod_dlm <- build_dlm(pars_mle, xreg)
filtro <- dlmFilter(y, mod_dlm)
suave <- dlmSmooth(filtro)

# Extraemos componentes
nivel <- dropFirst(suave$s[,1])
pendiente <- dropFirst(suave$s[,2])
estacionalidad <- rowSums(dropFirst(suave$s[,3:8]))
festividad <- dropFirst(suave$s[,9])
var_list <- dlmSvd2var(suave$U.S, suave$D.S) 
nivel_sd <- sqrt(sapply(var_list[-1], function(V) V[1, 1]))
z <- qnorm(0.975)
nivel_upper <- nivel + z * nivel_sd
nivel_lower <- nivel - z * nivel_sd

# Gráficamos intervalos de confianza
df_dlm <- tibble(
  fecha = df$fecha,
  total_ops = df$total_ops,
  nivel = nivel,
  pendiente = pendiente,
  estacionalidad = estacionalidad,
  nivel_lower = nivel_lower,
  nivel_upper = nivel_upper,
  y_hat = nivel + estacionalidad,
  serie_deses = y - (estacionalidad + festividad),
  resid = y - (nivel + estacionalidad)
)

ggplot(df_dlm, aes(x = fecha)) +
  geom_line(aes(y = total_ops), color = 'lightgrey') +
  geom_line(aes(y = nivel), color = 'red', alpha=0.4) +
  geom_ribbon(aes(ymin = nivel_lower, ymax = nivel_upper), alpha = 0.4) +
  labs(title = 'DLM: Total de operaciones por día',
       x = 'Fecha', y = 'Total de operaciones') +
  theme_minimal()



# Pruebas de válidez
# 1. Normalidad de los residuos
residuos <- residuals(filtro, type = "raw")
qqnorm(residuos$res)
qqline(residuos$res, col = "red")
hist(residuos$res, breaks = 50, main = "Histograma de residuos", xlab = "Residuos")


# 2. Autocorrelación de los residuos
acf(residuos$res, main = "ACF de los residuos")


