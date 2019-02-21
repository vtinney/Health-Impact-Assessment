## Health Impact Assessment

This site provides a step by step to performing a health impact assessment of the health burden attributable to air pollution, using R.

### HIA Steps

This page will review formatting data and importing it into R. I also often use ArcMap for easy visualization that my programs have worked.

HIA relies on the following inputs: population; rate of the health endpoint of interest; a concentration response function; and concentration data for the air pollutant of interest. Depending on if you want to do a sub-analysis of population, you will need to create population fraction rasters (say for age or race), and then multiply this by the original population dataset. 

A HIA is run for each health endpoint with the corresponding rate, and population of interest. The final raster inputs must be all on the same resolution, extent, and geographic projection.

In addition to codes to complete the above, this repository also has helpful codes for common functions such as: adding an attribute table to a raster; merging multiple data attribute files; pearson correlation between rasters; and other conversions, such as raster to polygon and polygon to raster.

#### Step 1 - Population fractions for Age and Race

For this HIA example, I will format my rasters so that they can be used in the HIA calculation. My population dataset is already on a 100m resolution, but I want to create additional rasters for age, and race.

For this I am effectively going to multiply a population fraction by my original population dataset. My 
[GPW](http://sedac.ciesin.columbia.edu/data/collection/gpw-v4) - Age datasets
ACS 2016 - race datasets

At the end I will have:
1. A population count raster for each subpopulation.
2. A population fraction raster for each subpopulation.
3. A new population dataset for the subpopulation at the original population resolution.

In the first step, creating the race subpopulation rasters, I created the original rasters in ArcMap. The ACS 2016 dataset was already in a Shapefile, so I quickly converted polygon to raster for each race/ethnicity. I find it much easier to preserve the values using ArcMap and specifying the desired variable in the conversion tool.

```markdown
library(raster)
library(rgdal)
library(maptools)
library(rgeos)
library(foreign)

setwd("C:/Users/vtinney/GoogleDrive/EDF_shared/Population/Race/individ")

# Read in race/ethnicity rasters

totalrace <- raster('total2.tif')
totalrace <- raster('total.tif')
white <- raster('white.tif')
black <- raster('black.tif')
ai <- raster('american_indian.tif')
nativehaw <- raster('nativehawaii.tif')
asian <- raster('asian.tif')
other <- raster('other.tif')
hispanic <- raster('hispanic.tif')

# change projection
crs(totalrace) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
crs(white) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
crs(black) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
crs(ai) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
crs(nativehaw) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
crs(asian) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
crs(other) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
crs(hispanic) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Write raster with new projection
writeRaster(totalrace, filename="total2.tif", format="GTiff", overwrite=TRUE)
writeRaster(ai, filename="american_indian2.tif", format="GTiff", overwrite=TRUE)
writeRaster(white, filename="white2.tif", format="GTiff", overwrite=TRUE)
writeRaster(black, filename="black2.tif", format="GTiff", overwrite=TRUE)
writeRaster(other, filename="other2.tif", format="GTiff", overwrite=TRUE)
writeRaster(asian, filename="asian2.tif", format="GTiff", overwrite=TRUE)
writeRaster(nativehaw, filename="nativehawaii2.tif", format="GTiff", overwrite=TRUE)
writeRaster(hispanic, filename="hispanic2.tif", format="GTiff", overwrite=TRUE)

# Stack all race rasters
race_stack <- stack('white2.tif', 'black2.tif', 'american_indian2.tif', 'nativehawaii2.tif', 'asian2.tif','hispanic2.tif', 'other2.tif')
names(race_stack) <- c('white', 'black', 'american_indian', 'native_hawaii', 'asian', 'hispanic', 'other')
race_frac <- race_stack / totalrace
names(race_frac) <- c('white', 'black', 'american_indian', 'native_hawaii', 'asian', 'hispanic', 'other')

# import original population dataset
setwd("C:/Users/vtinney/Dropbox/EDF_2019/Population/")
conus <- raster('CONUS2017.tif')

# Resample such that LandScan and Population fraction are on the same resolution
resample_race_frac <- resample(race_frac, conus, method='bilinear')

# Multiple population fraction by the original population counts in the original population dataset
conus_race_frac <- resample_race_frac * conus

# Rename layers
conus_race_frac
names(conus_race_frac) <- c('white', 'black', 'american_indian', 'native_hawaii', 'asian', 'hispanic', 'other')

setwd("C:/Users/vtinney/GoogleDrive/EDF_shared/Population/Race")

# write all tifs
# Be sure to use the bylayer=TRUE to make sure that all race raster layers are exported into their own raster file.
writeRaster(race_stack, filename="all_race_count3.tif", bylayer=TRUE, format="GTiff", overwrite=TRUE)
writeRaster(race_frac, filename="race_frac3.tif", bylayer=TRUE, format="GTiff", overwrite=TRUE)
writeRaster(conus_race_frac, filename="conus_race3.tif", bylayer=TRUE, format="GTiff", overwrite=TRUE)

```

