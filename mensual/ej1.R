# Ejemplo de crear un DLM en R
library(dlm)
rw <- dlm(m0=0, C0=100, FF=1, V=1.4, GG=1, W=0.2)
unlist(rw)
lg <- dlm(m0=rep(0,2), 
    C0=10*diag(2), FF=matrix(c(1,0)), V=1.4, GG=diag(2), W=1
)