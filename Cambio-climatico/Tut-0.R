library(dismo); library(terra)

############# Preparación de datos

# Puntos de presencia

brad.f <- paste(system.file(package="dismo"), "/ex/bradypus.csv", sep="")
brad.p <- read.csv(brad.f)

brad.sp <- vect(brad.p[, c("lon", "lat")])

#Capas raster

capas.f <- list.files(path=paste(system.file(package="dismo"), '/ex',
                               sep=''), pattern='grd', full.names=TRUE)
capas.r <- rast(capas.f)

par(mar = c(1,1,1,1))
plot(capas.r[[1]]); points(brad.sp)

buf <- buffer(brad.sp, width = 10)

par(mar = c(1,1,1,1))
plot(capas.r[[1]]); points(brad.p$lon, brad.p$lat)
plot(buf, add = T)

capas.rm <- mask(capas.r, buf)

plot(capas.rm[[1]])
points(brad.sp)

########### Análisis exploratorio

png("Tutorial-SDM/Pares.png", width = 3000, height = 3000)
pairs(capas.rm)
dev.off()

## Regressión Poisson

pres.r <- rasterize(brad.sp, capas.rm[[1]], fun = "count")
pres.r[[1]][is.na(pres.r[[1]][])] <- 0
pres.r <- mask(pres.r[[1]], capas.rm[[1]])
plot(pres.r)

capas.rm <- c(capas.rm, pres.r)

## Datos completos en formato de tabla

capas.df <- as.data.frame(capas.rm, xy = T)

wt <- rep(1.0E-6, nrow(capas.df))

wt[capas.df$count == 0] = length(wt)/sum(capas.df$count == 0)

## Regresión Poisson

modelo <- glm(count/wt ~ bio1 + bio12 + bio7 +
                I(bio1^2) + I(bio12^2) + I(bio7^2), 
                   data = capas.df, weights = wt,
                   family = poisson())

modelo.1 <- glm(count/wt ~ bio1 + bio16 + bio17 + bio7 +
                I(bio1^2) + I(bio16^2)+ I(bio17^2) + I(bio7^2), 
              data = capas.df, weights = wt,
              family = poisson())

mod.step <- step(modelo.1)

summary(mod.step)

pred.modelo <- exp(predict(mod.step))

pred.r <- rast(data.frame(capas.df[, c("x", "y")], pred.modelo))
plot(pred.r)

points(brad.sp, pch = 20, cex = 0.1)

## Mapas binarios

fav.pres <- extract(pred.r, brad.sp)
umbral <- quantile(fav.pres, 0.05)

plot(pred.r > umbral); points(brad.sp)
