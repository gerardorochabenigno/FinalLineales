rm(list = rm())
library(tidyverse)
library(R2jags)


# Load the data
PATH = "../data/raw"
df <- read_csv(file.path(PATH, "ops.csv"))
colnames(df) <- c('fecha', 'ops')
df <- df |> filter(fecha >= "2024-01-01")
head(df)


# JAGS. Modelo 1: Modelo de tendencia local
y = df$ops
n = length(y)
k=3

data <- list(
  "y" = y,
  "n" = n
)


inits <- function() {
  list(
    "tau.y" = 1,
    "tau.mu" = 1
  )
}

params <- c("mu", "y", "tau.y", "tau.mu")


modelo <- jags(data = data,
               inits = inits,
               parameters.to.save = params,
               model.file = "./jags/nivel.txt",
               n.chains = 2,
               n.iter = 50000,
               n.burnin = 5000,
               n.thin = 2
               )



