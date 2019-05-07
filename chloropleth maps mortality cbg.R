library(ggplot2)
library(maptools)
library(rgeos)
library(Cairo)
library(rgdal)
library(ggmap)
library(scales)
library(RColorBrewer)
set.seed(8000)
library(geojsonio)
library(broom)
install.packages('reshape')
library(reshape)
library(cowplot)

knitr::opts_chunk$set(
  out.width = "100%",
  dpi = 300,
  fig.width = 8,
  fig.height = 6,
  fig.path = 'https://timogrossenbacher.ch/wp-content/uploads/2016/12/tm-',
  strip.white = T,
  dev = "png",
  dev.args = list(png = list(bg = "transparent"))
)

remove(list = ls(all.names = TRUE))

detachAllPackages <- function() {
  basic.packages.blank <-  c("stats", 
                             "graphics", 
                             "grDevices", 
                             "utils", 
                             "datasets", 
                             "methods", 
                             "base")
  basic.packages <- paste("package:", basic.packages.blank, sep = "")
  
  package.list <- search()[ifelse(unlist(gregexpr("package:", search())) == 1, 
                                  TRUE, 
                                  FALSE)]
  
  package.list <- setdiff(package.list, basic.packages)
  
  if (length(package.list) > 0)  for (package in package.list) {
    detach(package, character.only = TRUE)
    print(paste("package ", package, " detached", sep = ""))
  }
}

detachAllPackages()


if (!require(rgeos)) {
  install.packages("rgeos", repos = "http://cran.us.r-project.org")
  require(rgeos)
}
if (!require(rgdal)) {
  install.packages("rgdal", repos = "http://cran.us.r-project.org")
  require(rgdal)
}
if (!require(raster)) {
  install.packages("raster", repos = "http://cran.us.r-project.org")
  require(raster)
}
if(!require(ggplot2)) {
  install.packages("ggplot2", repos="http://cloud.r-project.org")
  require(ggplot2)
}
if(!require(viridis)) {
  install.packages("viridis", repos="http://cloud.r-project.org")
  require(viridis)
}
if(!require(dplyr)) {
  install.packages("dplyr", repos = "https://cloud.r-project.org/")
  require(dplyr)
}
if(!require(gtable)) {
  install.packages("gtable", repos = "https://cloud.r-project.org/")
  require(gtable)
}
if(!require(grid)) {
  install.packages("grid", repos = "https://cloud.r-project.org/")
  require(grid)
}
if(!require(readxl)) {
  install.packages("readxl", repos = "https://cloud.r-project.org/")
  require(readxl)
}
if(!require(magrittr)) {
  install.packages("magrittr", repos = "https://cloud.r-project.org/")
  require(magrittr)
}

setwd("C:/Users/vtinney/GoogleDrive/EDF_shared/Rates/final_shape/")

mort <- readOGR(dsn=getwd(), layer='mortality_alameda_cbg')

alameda_short <- read_excel("C:/Users/vtinney/GoogleDrive/EDF_shared/Rates/Excel/Mortality/alameda_short.xlsx")

ala <- alameda_short

#fortify data for ggplot
map_data_fortified <- fortify(mort, region = "geo_1")  %>% 
  mutate(id = as.numeric(id))

#link spatial data
map_data <- map_data_fortified %>% left_join(ala, by = c("id" = "geo"))

x <- 28.82:330.5
y <- 8.57:125.55
z <- 42.24:600.15
p <- 121.96:1529.64
rng = range(c((x), (y), (z), (p)))

a <- ggplot() + 
  geom_polygon(data=map_data, 
               aes(x=long, y=lat, group=group, fill=rate_25_10k)) +
  geom_path(data = map_data, aes(x = long, y = lat, group = group), 
            color = "white", size = 0.01, alpha=.05) +                 
  scale_fill_distiller(name="Rate per 10,000", palette = "RdYlGn", na.value="grey50",
                       limits=c(min(map_data$cvd_25_10k), max(map_data$cvd_65_10k)))+
  labs(title="All-cause mortality, Ages 25-99 years")+
  coord_equal() +
  theme_void()

b <- ggplot() + 
  geom_polygon(data=map_data, 
               aes(x=long, y=lat, group=group, fill=rate_65_10k)) +
  geom_path(data = map_data, aes(x = long, y = lat, group = group), 
            color = "white", size = 0.01, alpha=.05) +                 
  scale_fill_distiller(name="Rate per 10,000", palette = "RdYlGn", na.value="grey50",
                       limits=c(min(map_data$cvd_25_10k), max(map_data$cvd_65_10k)))+
  labs(title="All-cause mortality, Ages 65-99 years")+
  coord_equal() +
  theme_void()

c <- ggplot() + 
  geom_polygon(data=map_data, 
               aes(x=long, y=lat, group=group, fill=cvd_25_10k)) +
  geom_path(data = map_data, aes(x = long, y = lat, group = group), 
            color = "white", size = 0.01, alpha=.05) +                 
  scale_fill_distiller(name="Rate per 10,000", palette = "RdYlGn", na.value="grey50",
                       limits=c(min(map_data$cvd_25_10k), max(map_data$cvd_65_10k)))+
  labs(title="CVD mortality, Ages 25-99 years")+
  coord_equal() +
  theme_void()

d <- ggplot() + 
  geom_polygon(data=map_data, 
               aes(x=long, y=lat, group=group, fill=cvd_65_10k)) +
  geom_path(data = map_data, aes(x = long, y = lat, group = group), 
            color = "white", size = 0.01, alpha=.05) +                 
  scale_fill_distiller(name="Rate per 10,000", palette = "RdYlGn", na.value="grey50",
                       limits=c(min(map_data$cvd_25_10k), max(map_data$cvd_65_10k)))+
  labs(title="CVD mortality, Ages 65-99 years")+
  coord_equal() +
  theme_void()

P <- plot_grid(a, b, c, d, labels='AUTO')

save_plot("alameda_mortality.png", P,
          ncol = 2, # we're saving a grid plot of 2 columns
          nrow = 2, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 1.3
)