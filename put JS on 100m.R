library(rgdal)
library(maptools)
library(raster)
library(foreign)# for loading DBF
library(reshape)


#load rasters
setwd('/home/vtinney/run/conc/')
js13 <- raster('2013_js.tif')
js14 <- raster('2014_js.tif')
js15 <- raster('2015_js.tif')
js16 <- raster('2016_js.tif')
mean15.16.js <- raster('mean_bay_15_16_js.tif')
mean13.16.js <- raster('mean_bay_13_16_js.tif')

#load shapefile with grid
setwd('/home/vtinney/pollutants/')
grid <- readOGR(dsn=getwd(), layer='BA_zip')

#extract raster values by 100m grid cells in the shapefile
ex2016 <- extract(x=js16, y=grid, fun=mean, df=TRUE)  
ex2015 <- extract(x=js15, y=grid, fun=mean, df=TRUE)
ex2014 <- extract(x=js14, y=grid, fun=mean, df=TRUE)
ex2015 <- extract(x=js15, y=grid, fun=mean, df=TRUE)
ex.mean15.16.js <- extract(x=mean15.16.js, y=grid, fun=mean, df=TRUE) #Mean raster for 2015-2016
ex.mean13.16.js <- extract(x=mean13.16.js, y=grid, fun=mean, df=TRUE) #Mean raster for 2013-2016

#Import empty grid to attach values to
empty.grid <- raster('empty_grid.tif')
ratify(empty.grid)

#Duplicate empty grid
grid.16 <- empty.grid
grid.15 <- empty.grid
grid.14 <- empty.grid
grid.13 <- empty.grid
grid.13.16 <- empty.grid
grid.15.16 < - empty.grid

levels(grid.16) <- ex2016
levels(grid.15) <- ex2015
levels(grid.14) <- ex2014
levels(grid.13) <- ex2013
levels(grid.13.16) <- ex.mean13.16.js
levels(grid.15.16) <- ex.mean15.16.js


writeRaster(grid.16, filename="grid_16.grd")
writeRaster(grid.15, filename="grid_15.grd")
writeRaster(grid.14, filename="grid_14.grd")
writeRaster(grid.13, filename="grid_13.grd")
writeRaster(grid.13.16, filename="grid_13_16.grd")
writeRaster(grid.15.16, filename="grid_15_16.grd")


