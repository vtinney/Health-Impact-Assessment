library(raster)
library(rgdal)
library(mapplots)
library(shapefiles)
library(foreign)
library(grid)
install.packages('methods')
library(methods)
library(ggplot2)
extrafont::loadfonts(device="win")

setwd("C:/Users/vtinney/GoogleDrive/EDF_shared/Population/")

day <- raster('DAY_BA.tif')
night <- raster('NIGHT_BA.tif')
day_clip <-raster('DAY_BA_ClipMGRS.tif')
night_clip <-raster('NIGHT_BA_ClipMGRS.tif')

day_grid <- as(day, 'SpatialGridDataFrame')
night_grid <- as(night, 'SpatialGridDataFrame')
night.clip.grid <-as(night_clip, 'SpatialGridDataFrame')
day.clip.grid <-as(day_clip, 'SpatialGridDataFrame')

coordinates(x)= c("x", "y")
proj4string(x) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
gridded(x) <- TRUE


spts = rasterToPoints(day, spatial = TRUE)
llprj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
llpts <- spTransform(spts, CRS(llprj))
x <- as.data.frame(llpts)
dt <- fortify(x, region="DAY_BA")

p <- ggplot() +
  geom_polygon(data = x, aes(fill = DAY_BA, 
                                    x = x, 
                                    y = y 
                                     )) +
  geom_path(data = x, aes(x = x, y = y), 
            color = "white", size = 0.0000001) +
  coord_equal()
