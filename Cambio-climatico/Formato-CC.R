f <- list.files("~/Descargas/", "ACCESS", full.names = T)

r <- lapply(f, rast)

r.crop <- lapply(r, function(x){mask(x, buf)})

r.res <- lapply(r.crop, function(x){resample(x, capas.rm)})

writeRaster(r.res[[1]], "Cambio-climatico/Datos/ACCESS-126.tif")
writeRaster(r.res[[2]], "Cambio-climatico/Datos/ACCESS-245.tif")
writeRaster(r.res[[3]], "Cambio-climatico/Datos/ACCESS-370.tif")
writeRaster(r.res[[4]], "Cambio-climatico/Datos/ACCESS-585.tif")
