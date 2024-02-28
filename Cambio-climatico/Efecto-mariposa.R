#Tutorial tomado de la documentación del paquete deSolve

library(deSolve)

chaos <- function(t, y, params) {
  with(as.list(c(y)), {
  dx  <- -8/3 * x + y * z
  dy  <- -10 * (y - z)
  dz  <- -x * y + 28 * y - z
    list(c(dx, dy, dz))
  })
}
yini <- c(x = 1, y = 1, z = 1)
yini2 <- yini + c(1e-6, 0, 0)
yini3 <- yini + c(1e-6, 1e-6, 0)
times <- seq(0, 100, 0.01)
out <- ode(y = yini, times = times, func = chaos, parms = 0)
out2 <- ode(y = yini2, times = times, func = chaos, parms = 0)
out3 <- ode(y = yini3, times = times, func = chaos, parms = 0)


plot(out, out2, out3, xlim = c(0, 30), lwd = 2, lty = 1)

par(mfrow = c(1,1))
plot(out[,"x"], out[,"y"], pch = ".", main = "La mariposa de Lorenz",
     xlab = "x", ylab = "y")

plot(out2[,"x"], out2[,"y"], pch = ".", main = "La mariposa de Lorenz",
     xlab = "x", ylab = "y")

library(rgl)
library(rglwidget)

plot3d(x = out[, "x"], y = out[, "y"], z = out[, "z"], col = "lightblue", pch = 20,
       xlab = "x", ylab = "y", zlab = "z")
rglwidget()

plot3d(x = out2[, "x"], y = out2[, "y"], z = out2[, "z"], col = "lightblue", pch = 20,
       xlab = "x", ylab = "y", zlab = "z")
rglwidget()

plot3d(x = out3[, "x"], y = out3[, "y"], z = out3[, "z"], col = "lightblue", pch = 20,
       xlab = "x", ylab = "y", zlab = "z")
rglwidget()

#Todas juntas
plot3d(x = out[, "x"], y = out[, "y"], z = out[, "z"], col = "lightblue", pch = 20,
       xlab = "x", ylab = "y", zlab = "z")
points3d(x = out2[, "x"], y = out2[, "y"], z = out2[, "z"], col = "pink")
points3d(x = out3[, "x"], y = out3[, "y"], z = out3[, "z"], col = "orange")
rglwidget()
