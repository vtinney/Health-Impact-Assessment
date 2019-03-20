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


# Map rasters
# adapted from https://stackoverflow.com/questions/33227182/how-to-set-use-ggplot2-to-map-a-raster
# 2019-3-19
library(extrafont)
library(grid)
extrafont::loadfonts(device="win")
library(raster)
library(rgdal)
library(ncdf4)
library(rasterVis)
library(gridExtra)
library(RColorBrewer)
library(viridis)
library(ggplot2)
library(raster)
library(rasterVis)
library(rgdal)
library(grid)
library(scales)
library(viridis)
library(ggthemes)
library(highcharter)
library(cowplot)
library(dplyr)



setwd("C:/Users/vtinney/GoogleDrive/EDF_shared/Concentrations/JS/")
b16 <- raster('2016.tif')
b15 <- raster('2015.tif')
b14 <- raster('2014.tif')
b13 <- raster('2013.tif')
bay <- stack(b16, b15, b13, b14)

hist(bay,
     main = "Distribution of Bay Area values",
     xlab = "PM2.5", ylab = "Frequency",
     col = "springgreen")

test <- as(bay, "SpatialPixelsDataFrame")
bay_t <- as.data.frame(test)
colnames(bay_t) <- c("2016", "2015", "2013", "2014", "x", "y")

setwd("C:/Users/vtinney/GoogleDrive/EDF_shared/Population/BA_Shape/bay/")
bay_shp <- readOGR(dsn=getwd(), layer='BA_zip')


theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Calibri", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.border = element_blank(),
      ...
    )
}

no_classes <- 6
labels <- c()

quantiles <- quantile(bay_t$'2016', 
                      probs = seq(0, 1, length.out = no_classes + 1))
quantiles

a <- ggplot() +  
  geom_polygon(data=bay_shp, aes(x=long, y=lat, group=group), 
               fill=NA, color="grey50", size=0.25) +
  geom_tile(data=bay_t, aes(x=x, y=y, fill=bay_t$'2016'), alpha=0.8) + 
  coord_equal() +
  theme_map() +
  theme(
    legend.text.align = 0.5,
    legend.background = element_rect(fill = alpha('white', 0.0)),
    legend.text = element_text(size = 11, hjust = 0, color = "#4e4d47", family="Calibri"),
    plot.title = element_text(hjust = 0.5, color = "#4e4d47", family="Calibri", face="bold"),
    plot.subtitle = element_text(hjust = 0.5, color = "#4e4d47", family="Calibri",
                                 margin = margin(b = -0.1, 
                                                 t = -0.1, 
                                                 l = 2, 
                                                 unit = "cm"), 
                                 debug = F),
    legend.title = element_text(size = 14),
    plot.margin = unit(c(.5,.5,.2,.5), "cm"),
    panel.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
    panel.border = element_blank(),
    plot.caption = element_text(size = 6, 
                                hjust = 0.92, 
                                margin = margin(t = 0.2, 
                                                b = 0, 
                                                unit = "cm"), 
                                color = "#939184")
  ) +
  theme(legend.key.width=unit(2, "cm")) +
  labs(x = NULL, 
       y = NULL, 
       title = "Bay Area, 2016",
       tag = "Figure 1",
       subtitle = "Extent: -123.6325, -121.2082, 36.89298, 38.86424")+
  # this is the main part
  theme(legend.position = 'bottom') +
  scale_fill_gradientn(colours = c("#66bd63", "#a6d96a", "#d9ef8b", "#fee08b",
                                   "#fdae61", "#f46d43", "#d73027"),
                       breaks=c(0.2,1.4,3.1,4.6,5.7,7,22.3),
                       labels= c("Min: 0.22","","","","","","Max: 22.29"),
                       limits = c(0,22.5),
                       name = "PM2.5",
                       # here we use guide_colourbar because it is still a continuous scale
                       guide = guide_colorbar(
                         direction = "horizontal",
                         barheight = unit(2, units = "mm"),
                         barwidth = unit(50, units = "mm"),
                         draw.ulim = F,
                         title.position = 'top',
                         # some shifting around
                         title.hjust = 0.5,
                         label.hjust = 0.5
                       ))

no_classes <- 6
labels <- c()

quantiles <- quantile(bay_t$'2015', 
                      probs = seq(0, 1, length.out = no_classes + 1))
quantiles

b <- ggplot() +  
  geom_polygon(data=bay_shp, aes(x=long, y=lat, group=group), 
               fill=NA, color="grey50", size=0.25) +
  geom_tile(data=bay_t, aes(x=x, y=y, fill=bay_t$'2015'), alpha=0.8) + 
  coord_equal() +
  theme_map() +
  theme(
    legend.text.align = 0.5,
    legend.background = element_rect(fill = alpha('white', 0.0)),
    legend.text = element_text(size = 11, hjust = 0, color = "#4e4d47", family="Calibri"),
    plot.title = element_text(hjust = 0.5, color = "#4e4d47", family="Calibri", face="bold"),
    plot.subtitle = element_text(hjust = 0.5, color = "#4e4d47", family="Calibri",
                                 margin = margin(b = -0.1, 
                                                 t = -0.1, 
                                                 l = 2, 
                                                 unit = "cm"), 
                                 debug = F),
    legend.title = element_text(size = 14),
    plot.margin = unit(c(.5,.5,.2,.5), "cm"),
    panel.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
    panel.border = element_blank(),
    plot.caption = element_text(size = 6, 
                                hjust = 0.92, 
                                margin = margin(t = 0.2, 
                                                b = 0, 
                                                unit = "cm"), 
                                color = "#939184")
  ) +
  theme(legend.key.width=unit(2, "cm")) +
  labs(x = NULL, 
       y = NULL, 
       title = "Bay Area, 2015",
       tag = "Figure 2",
       subtitle = "Extent: -123.6325, -121.2082, 36.89298, 38.86424")+
  # this is the main part
  theme(legend.position = 'bottom') +
  scale_fill_gradientn(colours = c("#66bd63", "#a6d96a", "#d9ef8b", "#fee08b",
                                   "#fdae61", "#f46d43", "#d73027"),
                       breaks=c(0.09,0.3,2.6,4.7,6.6,8.8,18.5),
                       labels= c("Min: 0.09","","","","","","Max: 18.5"),
                       limits = c(0,20),
                       name = "PM2.5",
                       # here we use guide_colourbar because it is still a continuous scale
                       guide = guide_colorbar(
                         direction = "horizontal",
                         barheight = unit(2, units = "mm"),
                         barwidth = unit(50, units = "mm"),
                         draw.ulim = F,
                         title.position = 'top',
                         # some shifting around
                         title.hjust = 0.5,
                         label.hjust = 0.5
                   ))

no_classes <- 6
labels <- c()

quantiles <- quantile(bay_t$'2014', 
                      probs = seq(0, 1, length.out = no_classes + 1))
quantiles

c <- ggplot() +  
  geom_polygon(data=bay_shp, aes(x=long, y=lat, group=group), 
               fill=NA, color="grey50", size=0.25) +
  geom_tile(data=bay_t, aes(x=x, y=y, fill=bay_t$'2014'), alpha=0.8) + 
  coord_equal() +
  theme_map() +
  theme(
    legend.text.align = 0.5,
    legend.background = element_rect(fill = alpha('white', 0.0)),
    legend.text = element_text(size = 11, hjust = 0, color = "#4e4d47", family="Calibri"),
    plot.title = element_text(hjust = 0.5, color = "#4e4d47", family="Calibri", face="bold"),
    plot.subtitle = element_text(hjust = 0.5, color = "#4e4d47", family="Calibri",
                                 margin = margin(b = -0.1, 
                                                 t = -0.1, 
                                                 l = 2, 
                                                 unit = "cm"), 
                                 debug = F),
    legend.title = element_text(size = 14),
    plot.margin = unit(c(.5,.5,.2,.5), "cm"),
    panel.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
    panel.border = element_blank(),
    plot.caption = element_text(size = 6, 
                                hjust = 0.92, 
                                margin = margin(t = 0.2, 
                                                b = 0, 
                                                unit = "cm"), 
                                color = "#939184")
  ) +
  theme(legend.key.width=unit(2, "cm")) +
  labs(x = NULL, 
       y = NULL, 
       title = "Bay Area, 2014",
       tag = "Figure 3",
       subtitle = "Extent: -123.6325, -121.2082, 36.89298, 38.86424") +
  # this is the main part
  theme(legend.position = 'bottom') +
  scale_fill_gradientn(colours = c("#66bd63", "#a6d96a", "#d9ef8b", "#fee08b",
                                   "#fdae61", "#f46d43", "#d73027"),
                       breaks=c(0.56,0.5,3.4,4.7,6.3,8.8,15.6),
                       labels= c("Min: 0.56","","","","","","Max: 15.52"),
                       limits = c(0,16),
                       name = "PM2.5",
                       # here we use guide_colourbar because it is still a continuous scale
                       guide = guide_colorbar(
                         direction = "horizontal",
                         barheight = unit(2, units = "mm"),
                         barwidth = unit(50, units = "mm"),
                         draw.ulim = F,
                         title.position = 'top',
                         # some shifting around
                         title.hjust = 0.5,
                         label.hjust = 0.5
                       ))

no_classes <- 6
labels <- c()

quantiles <- quantile(bay_t$'2013', 
                      probs = seq(0, 1, length.out = no_classes + 1))
quantiles

d <- ggplot() +  
  geom_polygon(data=bay_shp, aes(x=long, y=lat, group=group), 
               fill=NA, color="grey50", size=0.25) +
  geom_tile(data=bay_t, aes(x=x, y=y, fill=bay_t$'2013'), alpha=0.8) + 
  coord_equal() +
  theme_map() +
  theme(
    legend.text.align = 0.5,
    legend.background = element_rect(fill = alpha('white', 0.0)),
    legend.text = element_text(size = 11, hjust = 0, color = "#4e4d47", family="Calibri"),
    plot.title = element_text(hjust = 0.5, color = "#4e4d47", family="Calibri", face="bold"),
    plot.subtitle = element_text(hjust = 0.5, color = "#4e4d47", family="Calibri",
                                 margin = margin(b = -0.1, 
                                                 t = -0.1, 
                                                 l = 2, 
                                                 unit = "cm"), 
                                 debug = F),
    legend.title = element_text(size = 14),
    plot.margin = unit(c(.5,.5,.2,.5), "cm"),
    panel.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
    panel.border = element_blank(),
    plot.caption = element_text(size = 6, 
                                hjust = 0.92, 
                                margin = margin(t = 0.2, 
                                                b = 0, 
                                                unit = "cm"), 
                                color = "#939184")
  ) +
  theme(legend.key.width=unit(2, "cm")) +
  labs(x = NULL, 
       y = NULL, 
       title = "Bay Area, 2013",
       tag = "Figure 4",
       subtitle = "Extent: -123.6325, -121.2082, 36.89298, 38.86424")+
  # this is the main part
  theme(legend.position = 'bottom') +
  scale_fill_gradientn(colours = c("#66bd63", "#a6d96a", "#d9ef8b", "#fee08b",
                                   "#fdae61", "#f46d43", "#d73027"),
                       breaks=c(0.15,0.7,2.7,4.9,6.6,8.4,18.58),
                       labels= c("Min: 0.15","","","","","","Max: 18.58"),
                       limits = c(0,20),
                       name = "PM2.5",
                      # here we use guide_colourbar because it is still a continuous scale
                      guide = guide_colorbar(
                        direction = "horizontal",
                        barheight = unit(2, units = "mm"),
                        barwidth = unit(50, units = "mm"),
                        draw.ulim = F,
                        title.position = 'top',
                        # some shifting around
                        title.hjust = 0.5,
                        label.hjust = 0.5
                      ))

P <- plot_grid(a, b, c, d)

save_plot("plot2by2.png", P,
          ncol = 2, # we're saving a grid plot of 2 columns
          nrow = 2, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 1.3
)

