
# load("data/banana.rda")

load("data/bananas.rda")

bananas$x@extent <- bananas$y@extent <- extent(0.0,1.0,0.0,1.0)
bananas$x@crs <- bananas$y@crs <- CRS("+proj=longlat")

save(bananas, file="data/bananas.rda")
