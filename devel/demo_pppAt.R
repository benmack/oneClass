require(devtools)
require(raster)
require(LPCM)
require(jpeg)

load_all('.')

data(bananas)

model <- trainOcc(x=bananas$tr[, -1], y=bananas$tr[, 1], method="ocsvm")

pnp <- pnpAt(model)

plot(pnp[, 'pnpLw'], pnp[, 'pnpMin'])

finalMin <- which.max(pnp[, 'pnpMin'])
finalLw <- which.max(pnp[, 'pnpLw'])

modelMin <- update(model, modRow=finalMin)
modelLw <- update(model, modRow=finalLw)

hist(modelMin)
hist(modelLw)

featurespace(modelMin, th=pnp[finalMin, 'thMin'])
featurespace(modelLw, th=pnp[finalMin, 'thLw'])

s <- bananas$y*0
s[] <- predict(modelLw, bananas$x[])
plot(s)

sstack <- stack(s, s, s)
fname <- 'C:/Users/ben/Desktop/SLIC/sstack.tif'
writeRaster(sstack, fname, format='GTiff', overwrite=TRUE)

fname <- 'C:/Users/ben/Desktop/SLIC/sstack2.jpg'
arr <- array(NA, dim=c(400, 400, 3))
sbyte <- round(approx(range(s[]), c(0, 255), s[])$y, 0)
sbyteb <- round(approx(range(s[]), c(255, 0), s[])$y, 0)
smat <- matrix(sbyte, 400, 400, byrow=T)
smatb <- matrix(sbyteb, 400, 400, byrow=T)

arr[,,1] <- smat
arr[,,2] <- smatb
arr[,,3] <- smat
writeJPEG(arr, fname)



