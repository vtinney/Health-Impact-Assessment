# Map rasters
# adapted from https://stackoverflow.com/questions/33227182/how-to-set-use-ggplot2-to-map-a-raster
# 2019-3-19
library(extrafont)

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



setwd("C:/Users/vtinney/GoogleDrive/EDF_Texas/")
t16 <- raster('2016.tif')
t15 <- raster('2015.tif')
t14 <- raster('2014.tif')
t13 <- raster('2013.tif')
hous <- stack(t16, t15, t13, t14)

hist(hous,
     main = "Distribution of Houston values",
     xlab = "PM2.5", ylab = "Frequency",
     col = "springgreen")

test <- as(hous, "SpatialPixelsDataFrame")
hous_t <- as.data.frame(test)
colnames(hous_t) <- c("2016", "2015", "2013", "2014", "x", "y")

hous_shp <- readOGR(dsn=getwd(), layer='houston_co')

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

quantiles <- quantile(hous_t$'2016', 
                      probs = seq(0, 1, length.out = no_classes + 1))
quantiles

a <- ggplot() +  
  geom_polygon(data=hous_shp, aes(x=long, y=lat, group=group), 
               fill=NA, color="grey50", size=0.25) +
  geom_tile(data=hous_t, aes(x=x, y=y, fill=hous_t$'2016'), alpha=0.8) + 
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
       title = "Houston, 2016",
       tag = "Figure 1",
       subtitle = "Extent: -96.62198, -94.35338, 28.82557, 30.63028")+
  # this is the main part
  theme(legend.position = 'bottom') +
  scale_fill_gradientn(colours = c("#66bd63", "#a6d96a", "#d9ef8b", "#fee08b",
                                   "#fdae61", "#f46d43", "#d73027"),
                       breaks=c(4.081852,7.511791,8.397463,8.819841,9.167414,9.533811,13.567024),
                       labels= c("Min: 4.08","","","","","","Max: 13.57"),
                       limits = c(4,15),
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

quantiles <- quantile(hous_t$'2015', 
                      probs = seq(0, 1, length.out = no_classes + 1))
quantiles

b <- ggplot() +  
  geom_polygon(data=hous_shp, aes(x=long, y=lat, group=group), 
               fill=NA, color="grey50", size=0.25) +
  geom_tile(data=hous_t, aes(x=x, y=y, fill=hous_t$'2015'), alpha=0.8) + 
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
       title = "Houston, 2015",
       tag = "Figure 2",
       subtitle = "Extent: -96.62198, -94.35338, 28.82557, 30.63028")+
  # this is the main part
  theme(legend.position = 'bottom') +
  scale_fill_gradientn(colours = c("#66bd63", "#a6d96a", "#d9ef8b", "#fee08b",
                                   "#fdae61", "#f46d43", "#d73027"),
                       breaks=c(1.062089,7.399622,8.732720,9.562129,10.417957,11.470640,14.888144),
                       labels= c("Max: 1.06","","","","","","Max: 14.88"),
                       limits = c(0,15),
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

quantiles <- quantile(hous_t$'2014', 
                      probs = seq(0, 1, length.out = no_classes + 1))
quantiles

c <- ggplot() +  
  geom_polygon(data=hous_shp, aes(x=long, y=lat, group=group), 
               fill=NA, color="grey50", size=0.25) +
  geom_tile(data=hous_t, aes(x=x, y=y, fill=hous_t$'2014'), alpha=0.8) + 
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
       title = "Houston, 2014",
       tag = "Figure 3",
       subtitle = "Extent: -96.62198, -94.35338, 28.82557, 30.63028")+
  # this is the main part
  theme(legend.position = 'bottom') +
  scale_fill_gradientn(colours = c("#66bd63", "#a6d96a", "#d9ef8b", "#fee08b",
                                   "#fdae61", "#f46d43", "#d73027"),
                       breaks=c(3.846659,8.113025,9.080116,9.650870,10.168208,10.916080,16.935059),
                       labels= c("Min: 3.85","","","","","","Max: 16.94"),
                       limits = c(3,17),
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

quantiles <- quantile(hous_t$'2013', 
                      probs = seq(0, 1, length.out = no_classes + 1))
quantiles

d <- ggplot() +  
  geom_polygon(data=hous_shp, aes(x=long, y=lat, group=group), 
               fill=NA, color="grey50", size=0.25) +
  geom_tile(data=hous_t, aes(x=x, y=y, fill=hous_t$'2014'), alpha=0.8) + 
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
       title = "Houston, 2013",
       tag = "Figure 4",
       subtitle = "Extent: -96.62198, -94.35338, 28.82557, 30.63028")+
  # this is the main part
  theme(legend.position = 'bottom') +
  scale_fill_gradientn(colours = c("#66bd63", "#a6d96a", "#d9ef8b", "#fee08b",
                                   "#fdae61", "#f46d43", "#d73027"),
                       breaks=c(3.083230,7.815055,8.568681,9.177513,9.749736,10.416106,13.961426),
                       labels= c("Min: 3.08","","","","","","Max: 13.96"),
                       limits = c(3,16),
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

save_plot("plot2by2_hous.png", P,
          ncol = 2, # we're saving a grid plot of 2 columns
          nrow = 2, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 1.3
)


