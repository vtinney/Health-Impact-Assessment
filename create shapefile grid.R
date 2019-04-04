
library(rgdal)
library(raster)
library(rgeos)
library(dismo)

setwd('/home/vtinney/bayarea_rasters/')
bay <- readOGR(dsn=getwd(), "BA_9_1984") 
.
grid <- raster(extent(-123.6325, -121.2083, 36.8925, 38.865))
res(grid) <- 0.0008333333
proj4string(grid)<-proj4string(bay)

gridpolygon <- rasterToPolygons(grid)
dry.grid <- intersect(dryland, gridpolygon)
writeOGR(dry.grid, dsn=getwd(), layer="inters_shape", driver="ESRI Shapefile", overwrite_layer=T)