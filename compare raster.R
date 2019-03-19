#Crop and convert vonDonkelaar data
#2019-3-19

library(raster)
library(rgdal)
library(ncdf4)
library(rasterVis)
library(gridExtra)
library(RColorBrewer)
library(viridis)

setwd('C:/Users/vtinney/Veronica Southerland Dropbox/Veronica Tinney/Dissertation/Data inputs/Exposure files')

sst <- raster(path.expand('GWRwSPEC_PM25_NA_201601_201612-RH35.nc'))

d <- extent(c(-123.6325, -121.2082, 36.89298, 38.86424))
pm25 <- crop(sst, d)

setwd("C:/Users/vtinney/GoogleDrive/EDF_shared/Concentrations/vonDonkelaar/")
writeRaster(pm25, filename="PM25.tif", format="GTiff",overwrite=TRUE)

setwd("C:/Users/vtinney/GoogleDrive/EDF_shared/Concentrations/JS/")
b16 <- raster('2016.tif')
b15 <- raster('2015.tif')
b14 <- raster('2014.tif')
b13 <- raster('2013.tif')
bay <- stack(b16, b15, b13, b14)
plot(bay)

eo <- raster()
eo <- extent(c(-122.25, -122.12, 37.69, 37.79))
wo <- raster()
wo <- extent(c(-122.35, -122.22, 37.77, 37.84))

jwo <- crop(x=bay, y=wo)
jeo <- crop(x=bay, y=eo)
writeRaster(jeo, filename='js_eo.tif', bylayer=TRUE, format="GTiff", overwrite=TRUE)
writeRaster(jwo, filename='js_wo.tif', bylayer=TRUE, format="GTiff", overwrite=TRUE)

setwd("C:/Users/vtinney/GoogleDrive/EDF_Texas/")
t16 <- raster('2016.tif')
t15 <- raster('2015.tif')
t14 <- raster('2014.tif')
t13 <- raster('2013.tif')
houston <- stack(t16, t15, t14, t13)
plot(houston)

plot(houston)

houston_shp <- readOGR(dsn=getwd(), layer='houston_co')

levelplot(houston, 
          margin=FALSE,                       
          colorkey=list(
            space='bottom',                   
            labels=list(at=-5:5, font=4),
            axis.line=list(col='black'),
            width=0.75
          ),    
          par.settings=list(
            strip.border=list(col='transparent'),
            strip.background=list(col='transparent'),
            axis.line=list(col='transparent')
          ),
          scales=list(draw=FALSE),            
          col.regions=viridis,                   
          at=seq(-5, 5, len=101),
          names.attr=rep('', nlayers(houston))) +           
  layer(sp.polygons(houston_shp, lwd=3))