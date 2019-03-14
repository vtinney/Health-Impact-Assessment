# Adapted from https://eurekastatistics.com/calculating-a-distance-matrix-for-geographic-points-using-r/
# Calculate the distance between multiple extents to determine if the grid cells line up


#day_clip
#class       : RasterLayer
#dimensions  : 1405, 1269, 1782945  (nrow, ncol, ncell)
#resolution  : 0.0008333333, 0.0008333333  (x, y)
#extent      : -122.6517, -121.5942, 37.10083, 38.27167  (xmin, xmax, ymin, ymax)
#coord. ref. : +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0
#data source : /home/vtinney/pop/LandScan/DAY_BA_ClipMGRS.tif
#names       : DAY_BA_ClipMGRS
#values      : 0, 14917  (min, max)

#night_clip
#class       : RasterLayer
#dimensions  : 1405, 1269, 1782945  (nrow, ncol, ncell)
#resolution  : 0.0008333333, 0.0008333333  (x, y)
#extent      : -122.6517, -121.5942, 37.10083, 38.27167  (xmin, xmax, ymin, ymax)
#coord. ref. : +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0
#data source : /home/vtinney/pop/LandScan/NIGHT_BA_ClipMGRS.tif
#names       : NIGHT_BA_ClipMGRS
#values      : 0, 1744  (min, max)


#day
#class       : RasterLayer
#dimensions  : 2367, 2909, 6885603  (nrow, ncol, ncell)
#resolution  : 0.0008333333, 0.0008333333  (x, y)
#extent      : -123.6325, -121.2083, 36.8925, 38.865  (xmin, xmax, ymin, ymax)
#coord. ref. : +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0
#data source : /home/vtinney/pop/LandScan/DAY_BA.tif
#names       : DAY_BA
#values      : 0, 14917  (min, max)

#night
#class       : RasterLayer
#dimensions  : 2365, 2909, 6879785  (nrow, ncol, ncell)
#resolution  : 0.0008333333, 0.0008333333  (x, y)
#extent      : -123.6325, -121.2083, 36.89417, 38.865  (xmin, xmax, ymin, ymax)
#coord. ref. : +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0
#data source : /home/vtinney/pop/LandScan/NIGHT_BA.tif
#names       : NIGHT_BA
#values      : 0, 1744  (min, max)


#mgrs
#class       : SpatialPolygonsDataFrame
#features    : 1186800
#extent      : -122.6511, -121.594, 37.10124, 38.27151  (xmin, xmax, ymin, ymax)
#coord. ref. : +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0
#variables   : 12

install.packages('sgeostat')
library(geosphere)
library(raster)
library(rgdal)
library(sgeostat)
library(Imap)

day.xlim <- c(-123.6325,-121.2083)
day.ylim <- c(36.8925,38.865)

day.min<- c(-123.6325,36.8925) 
day.max<- c(-121.2083,38.865)

night.xlim <- c(-123.6325,-121.2083)
night.ylim <- c(36.89417,38.865)

night.min <- c(-123.6325,36.89417)
night.max <- c(-121.2083,38.865)

ms.xlim <- c(-122.6511,-121.594)
ms.ylim <- c(37.10124,38.27151)

ms.min <-c(-122.6511,37.10124)
ms.max <- c(-121.594,38.27151)


df.lims <- data.frame(name = c("DayMin", "DayMax", "NightMin", "NightMax", "MSMin", "MSMax", "DayClipMin", "DayClipMax", "NightClipMin", "NightClipMax"),
                          lat  = c(36.8925,  38.865, 36.89417,  38.865, 37.10124, 38.27151, 37.10083, 38.27167, 37.10083, 38.27167),
                          lon  = c(-123.6325, -121.2083, -123.632, -121.2083, -122.6511, -121.594, -122.6517, -121.5942, -122.6517, -121.5942))


round(GeoDistanceInMetresMatrix(df.lims) / 1000)

ReplaceLowerOrUpperTriangle <- function(m, triangle.to.replace){
  # If triangle.to.replace="lower", replaces the lower triangle of a square matrix with its upper triangle.
  # If triangle.to.replace="upper", replaces the upper triangle of a square matrix with its lower triangle.
  
  if (nrow(m) != ncol(m)) stop("Supplied matrix must be square.")
  if      (tolower(triangle.to.replace) == "lower") tri <- lower.tri(m)
  else if (tolower(triangle.to.replace) == "upper") tri <- upper.tri(m)
  else stop("triangle.to.replace must be set to 'lower' or 'upper'.")
  m[tri] <- t(m)[tri]
  return(m)
}

GeoDistanceInMetresMatrix <- function(df.geopoints){
  # Returns a matrix (M) of distances between geographic points.
  # M[i,j] = M[j,i] = Distance between (df.geopoints$lat[i], df.geopoints$lon[i]) and
  # (df.geopoints$lat[j], df.geopoints$lon[j]).
  # The row and column names are given by df.geopoints$name.
  
  GeoDistanceInMetres <- function(g1, g2){
    # Returns a vector of distances. (But if g1$index > g2$index, returns zero.)
    # The 1st value in the returned vector is the distance between g1[[1]] and g2[[1]].
    # The 2nd value in the returned vector is the distance between g1[[2]] and g2[[2]]. Etc.
    # Each g1[[x]] or g2[[x]] must be a list with named elements "index", "lat" and "lon".
    # E.g. g1 <- list(list("index"=1, "lat"=12.1, "lon"=10.1), list("index"=3, "lat"=12.1, "lon"=13.2))
    DistM <- function(g1, g2){
      require("Imap")
      return(ifelse(g1$index > g2$index, 0, gdist(lat.1=g1$lat, lon.1=g1$lon, lat.2=g2$lat, lon.2=g2$lon, units="m")))
    }
    return(mapply(DistM, g1, g2))
  }
  
  n.geopoints <- nrow(df.geopoints)
  
  # The index column is used to ensure we only do calculations for the upper triangle of points
  df.geopoints$index <- 1:n.geopoints
  
  # Create a list of lists
  list.geopoints <- by(df.geopoints[,c("index", "lat", "lon")], 1:n.geopoints, function(x){return(list(x))})
  
  # Get a matrix of distances (in metres)
  mat.distances <- ReplaceLowerOrUpperTriangle(outer(list.geopoints, list.geopoints, GeoDistanceInMetres), "lower")
  
  # Set the row and column names
  rownames(mat.distances) <- df.geopoints$name
  colnames(mat.distances) <- df.geopoints$name
  
  return(mat.distances)
}

distance.100m <- GeoDistanceInMetresMatrix(df.lims)/100

                             