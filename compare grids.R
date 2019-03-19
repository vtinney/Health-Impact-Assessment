library(raster)
library(rgdal)
library(methods)

d <- raster(nrow=2367, ncol=2909, ext=extent(c(-123.6325, -121.2083, 36.8925, 38.865)))
n <- raster(nrow=2365, ncol=2909, ext=extent(c(-123.6325, -121.2083, 36.89417, 38.865)))  
m <- raster(ncol=1269, nrow=1404, ext=extent(c(-122.6511, -121.594, 37.10124, 38.27151)))

proj4string(m) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
proj4string(d) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
proj4string(n) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

res(n) <- 1/1200
res(d) <- 1/1200
res(m) <- 1/1200

m[] <- 1:ncell(m)
n[] <- 1:ncell(n)
d[] <- 1:ncell(d)

mg <- as(m, "SpatialPixelsDataFrame")
ng <- as(n, "SpatialPixelsDataFrame")
dg <- as(d, "SpatialPixelsDataFrame")

plot(geometry(ng), col='green')
plot(geometry(dg), add=TRUE, col="red")
plot(geometry(mg), add=TRUE, col="blue")

plot(extent(d), col='green', lwd=2)
plot(extent(n), add=TRUE, col="red")
plot(extent(m), add=TRUE, col="blue")

origin(n)
origin(d)
origin(m)

