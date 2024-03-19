#Tutorial tomado de la doccumentación del paqute deSolve

library(deSolve)
library(ReacTran)

Nx <- Ny <- 100
xgrid <- setup.grid.1D(-7, 7, N = Nx); x <- xgrid$x.mid
ygrid <- setup.grid.1D(-7, 7, N = Ny); y <- ygrid$x.mid

#Función de las derivadas

senoGordon2D <- function(t, C, parms) {
  u <- matrix(nrow = Nx, ncol = Ny, data = C[1 : (Nx*Ny)])
  v <- matrix(nrow = Nx, ncol = Ny, data = C[(Nx*Ny+1) : (2*Nx*Ny)])
  dv <- tran.2D(C = u, C.x.up = 0, C.x.down = 0, C.y.up = 0, C.y.down = 0,
                 D.x = 1, D.y = 1, dx = xgrid, dy = ygrid)$dC -
    sin(u)
  list(c(v, dv))
}

# Condiciones iniciales
peak <- function (x, y, x0, y0){return(exp(-( (x-x0)^2 + (y-y0)^2)))}
uini <- outer(x, y, FUN = function(x, y) peak(x, y, 2,2) + peak(x, y,-2,-2)
              + peak(x, y,-2,2) + peak(x, y, 2,-2))
vini <- rep(0, Nx*Ny)

out <- ode.2D (y = c(uini,vini), times = 0:3, parms = 0, func = senoGordon2D,
           names = c("u", "v"), dimens = c(Nx, Ny), method = "ode45")

image(out, which = "u", grid = list(x, y), mfrow = c(2,2), ask = FALSE)

### Generación de animación ...
out2 <- ode.2D (y = c(uini, vini), times = seq(0, 3, by = 0.1),
           parms = NULL, func = senoGordon2D,
           names=c("u", "v"), dimens = c(Nx, Ny),
           method = "ode45")
image(out2, which = "u", grid = list(x = x, y = y),
      method = "persp", border = NA, col = "lightblue",
      shade = 0.5, theta = 30, phi = 60, box = FALSE, ask = FALSE)
