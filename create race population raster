# VTinney 2019-2-19
# Step 1 - import rasters
# Step 2 - stack rasters and divide by populatio total
# Step 3 - multiply population fraction from above to LandScan for LS resolution age group population counts
# write all rasters

# Race rasters were created using the rasterize polygon command in ArcMap, clipping to bay area extent
# value for the raster file indicates the CBP number for total count race in 546 CBG in the bay area
# Race data counts from the ACS, 2016

setwd("C:/Users/vtinney/Dropbox/EDF_2019/")

library(raster)
library(rgdal)
library(maptools)
library(rgeos)
library(foreign)


# Read in race/ethnicity rasters
totalrace <- raster('total2.tif')
white <- raster('white.tif')
black <- raster('black.tif')
ai <- raster('ai.tif')
nativehaw <- raster('nativehawaii1.tif')
asian <- raster('asian1.tif')
other <- raster('other.tif')
hispanic <- raster('hisp.tif')

#Stack all race rasters
race_stack <- stack('white.tif', 'black.tif', 'ai.tif', 'nativehawaii1.tif', 'asian1.tif','hisp.tif', 'other.tif')
names(race_stack) <- c('white', 'black', 'american_indian', 'native_hawaii', 'asian', 'hispanic', 'other')
race_frac <- race_stack / total
names(race_frac) <- c('white', 'black', 'american_indian', 'native_hawaii', 'asian', 'hispanic', 'other')

#Resample such that LandScan and Population fraction are on the same resolution
resample_race_frac <- resample(race_frac, conus, method='bilinear')

#Multiple population fraction by the original population counts in LandScan
conus_race_frac <- resample_race_frac * conus

#Rename layers
conus_race_frac
names(conus_race_frac) <- c('white', 'black', 'american_indian', 'native_hawaii', 'asian', 'hispanic', 'other')

#write all tifs
writeRaster(race_stack, filename="all_race_count.tif", format="GTiff", overwrite=TRUE)
writeRaster(race_frac, filename="race_frac.tif", format="GTiff", overwrite=TRUE)
writeRaster(conus_race_frac, filename="conus_race.tif", format="GTiff", overwrite=TRUE)

