library(terra)
library(spatstat)

ssp1 <- rast("Cambio-climatico/Datos/ACCESS-126.tif")

source("Nicho/Funciones-spatstat/imFromStack.R")

ssp1.im <- imFromStack(ssp1)
names(ssp1.im) <- paste0("bio", 1:19)

w <- as.owin(ssp1.im[[1]])

modelo <- readRDS("Cambio-climatico/Datos/Modelo-ppm.rds")

pred.modelo <- predict(modelo)
pred126 <- predict(modelo, covariates = ssp1.im, window = w)

pred.r <- rast(pred.modelo)
pred126.r <- rast(pred126)

## Binarización

brad.f <- paste(system.file(package="dismo"), "/ex/bradypus.csv", sep="")
brad.p <- read.csv(brad.f)

fav.pres <- extract(pred.r, brad.p[, c("lon", "lat")])

umbral <- quantile(fav.pres$lyr.1, 0.05)

pred.r.pa <- pred.r > umbral
pred126.r.pa <- pred126.r > umbral
plot(pred.r.pa)
plot(pred126.r.pa)

writeRaster(pred126.r, "Cambio-climatico/Datos/Pred126.tif")
