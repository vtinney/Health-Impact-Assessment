library(raster)
library(rgdal)

setwd('/home/vtinney/run/conc/')
js13 <- raster('2013_js.tif')
js14 <- raster('2014_js.tif')
js15 <- raster('2015_js.tif')
js16 <- raster('2016_js.tif')
bc.edf <- raster('edf_bc.tif')
no2.edf <- raster('edf_no2.tif')
mean15.16.js <- raster('mean_bay_15_16_js.tif')
mean13.16.js <- raster('mean_bay_13_16_js.tif')

setwd('/home/vtinney/run/pop/')
day.0.18 <- raster('day_age.groups0_18.tif')
day <- raster('day_age.groupsAll_ages.tif')
night <- raster('night_age.groupsAllages.tif')
day.25.64 <- raster('day_age.groups25_64.tif')
day.65.99 <- raster('day_age.groups65_99.tif')
night.25.64 <- raster('night_age.groups25_64.tif')
night.65.99 <- raster('night_age.groups65_99.tif')

setwd('/home/vtinney/run/rates/')
asthma.a.ed <- raster('asthma_ed18_99.tif')
asthma.c.ed <- raster('asthma_ed0_17.tif')
asthma.all.ed <- raster('asthma_edAll_ages.tif')
cvd.mort <- raster('cvd_mortAll_ages.tif')
cvd.er <- raster('cvd_er65_99.tif')


extent(js13) == extent(day)
extent(js14) == extent(day)
extent(js15) == extent(day)
extent(js16) == extent(day)
extent(bc.edf) == extent(day)
extent(no2.edf) == extent(day)
extent(mean15.16.js) == extent(day)
extent(mean13.16.js) == extent(day)
extent(larkin) == extent(day)
extent(night) == extent(day)
extent(night.25.64) == extent(day)
extent(day.65.99) == extent(day)
extent(night.65.99) == extent(day)
extent(asthma.a.ed) == extent(day)
extent(asthma.c.ed) == extent(day)
extent(asthma.all.ed) == extent(day)
extent(cvd.mort) == extent(day)
extent(cvd.er) == extent(day)


bc.edf <- crop(bc.edf, y=day)
no2.edf <- crop(x=no2.edf, y=day)
asthma.a.ed <- crop(x=asthma.a.ed, y=day)
asthma.c.ed <- crop(x=asthma.c.ed, y=day)
asthma.all.ed <- crop(x=asthma.all.ed, y=day)
cvd.mort <- crop(x=cvd.mort, y=day)
cvd.er <- crop(x=cvd.er, y=day)


js13 #
js14 #
js15 #
js16 #
mean15.16.js #
mean13.16.js #


night #### landscan night population
night <- crop(x=night, y=day)
writeRaster(night, filename='night', format="GTiff", overwrite=TRUE)

e <- extent(-123.6325, -121.2083, 36.8925, 38.865)


#match
#asthma.a.ed
#asthma.c.ed
#asthma.all.ed
#cvd.mort
#cvd.er
#bc.edf 
#no2.edf
#day.0.18
#day
#night
#day.25.64
#day.65.99
#night.25.64
#night.65.99


setwd('/home/vtinney/CIESEN/individ/')

a <- raster("25_29.tif")
b <- raster("30_34.tif")
c <- raster("35_39.tif")
d <- raster("40_44.tif")
f <- raster("45_49.tif")
g <- raster("50_54.tif")
h <- raster("55_59.tif")
i <- raster("60_64.tif")
j <- raster("65_69.tif")
k <- raster("70_74.tif")
l <- raster("75_79.tif")
m <- raster("80_84.tif")

setwd('/home/vtinney/CIESEN/')
ba_pop_tot <- raster('ba_pop_tot.tif')

a25_64 <- merge(a, b, c, d, f, g, h, i)
a65_99 <- merge(j, k, l, m)

pop_frac_25_64 <- a25_64 / ba_pop_tot
pop_frac_65_99 <- a65_99 / ba_pop_tot

resample_pop_frac_25_64_day <- resample(pop_frac_25_64, day, method='ngb')
resample_pop_frac_25_64_night <- resample(pop_frac_25_64, night, method='ngb')
resample_pop_frac_65_99_day <- resample(pop_frac_25_64, day, method='ngb')
resample_pop_frac_65_99_night <- resample(pop_frac_25_64, night, method='ngb')

day_pop_frac_25_64 <- resample_pop_frac_25_64_day * day
night_pop_frac_25_64 <- resample_pop_frac_25_64_night * night
day_pop_frac_65_99 <- resample_pop_frac_65_99_day * day
night_pop_frac_65_99 <- resample_pop_frac_65_99_night * night

setwd('/home/vtinney/run/pop/')
writeRaster(day_pop_frac_25_64, filename='day_pop_frac_25_64', format="GTiff", overwrite=TRUE)
writeRaster(night_pop_frac_25_64, filename='night_pop_frac_25_64', format="GTiff", overwrite=TRUE)

writeRaster(day_pop_frac_65_99, filename='day_pop_frac_65_99', format="GTiff", overwrite=TRUE)
writeRaster(night_pop_frac_65_99, filename='night_pop_frac_65_99', format="GTiff", overwrite=TRUE)



setwd('/home/vtinney/pop/age_frac/BA/age_count/')

#Load rasters
age0_4 <- raster('allages_pop_total_0_4.tif')
age5_9 <- raster('allages_pop_total_5_9.tif')
age10_14 <- raster('allages_pop_total_10_14.tif')
age15_19 <- raster('allages_pop_total_15_19.tif')

#Create a raster with one fifth the age of 15_19
sub15_19 <- age15_19/5

#Subtract that from the overall
sub0_18 <- age15_19-sub15_19

#Stack and sum the raster values
age0_18 <- stack(age0_4, age5_9, age10_14, sub0_18)
sst <- sum(age0_18)

#Create pop fraction
pop_frac <- sst / ba_pop_tot

#Resample such that LandScan and Population fraction are on the same resolution
resample_pop_frac <- resample(pop_frac, day, method='ngb')

#Multiple population fraction by the original population counts in LandScan
day_pop_frac <- resample_pop_frac * day

setwd('/home/vtinney/run/pop/')
writeRaster(day_pop_frac, filename="day_age_pop_frac_0_18.tif", format="GTiff", overwrite=TRUE)
