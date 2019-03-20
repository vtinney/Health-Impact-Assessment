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



setwd("C:/Users/vtinney/GoogleDrive/EDF_shared/Concentrations/JS/")
o16 <- raster('o16.tif')
o15 <- raster('o15.tif')
o14 <- raster('o14.tif')
o13 <- raster('o13.tif')
ow <- stack(o16, o15, o13, o14)

hist(ow,
     main = "Distribution of Bay Area values",
     xlab = "PM2.5", ylab = "Frequency",
     col = "springgreen")

test <- as(ow, "SpatialPixelsDataFrame")
ow_t <- as.data.frame(test)
colnames(ow_t) <- c("2016", "2015", "2013", "2014", "x", "y")

setwd("C:/Users/vtinney/GoogleDrive/EDF_shared/Population/BA_Shape/bay/")
eo <- readOGR(dsn=getwd(), layer='EO_CB')
wo <- readOGR(dsn=getwd(), layer='WO_CB')

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

quantiles <- quantile(ow_t$'2016', 
                      probs = seq(0, 1, length.out = no_classes + 1))
quantiles

a <- ggplot() +  
  geom_polygon(data=eo, aes(x=long, y=lat, group=group), 
               fill=NA, color="grey25", size=0.25) +
  geom_polygon(data=wo, aes(x=long, y=lat, group=group), 
               fill=NA, color="grey25", size=0.25) +
  geom_tile(data=ow_t, aes(x=x, y=y, fill=ow_t$'2016'), alpha=0.8) + 
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
       title = "Oakland, 2016",
       tag = "Figure 1",
       subtitle = "Extent: -122.3577, -122.096, 37.69776, 37.85148")+
  # this is the main part
  theme(legend.position = 'bottom') +
  scale_fill_gradientn(colours = c("#66bd63", "#a6d96a", "#d9ef8b", "#fee08b",
                                   "#fdae61", "#f46d43", "#d73027"),
                       breaks=c(3.032013,5.048239,6.418995,6.937654,7.292847,7.838758,9.397053),
                       labels= c("Min: 3.03","","","","","","Max: 9.40"),
                       limits = c(3,10),
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

quantiles <- quantile(ow_t$'2015', 
                      probs = seq(0, 1, length.out = no_classes + 1))
quantiles

b <- ggplot() +  
  geom_polygon(data=eo, aes(x=long, y=lat, group=group), 
               fill=NA, color="grey25", size=0.25) +
  geom_polygon(data=wo, aes(x=long, y=lat, group=group), 
               fill=NA, color="grey25", size=0.25) +
  geom_tile(data=ow_t, aes(x=x, y=y, fill=ow_t$'2015'), alpha=0.8) + 
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
       title = "Oakland, 2015",
       tag = "Figure 2",
       subtitle = "Extent: -122.3577, -122.096, 37.69776, 37.85148")+
  # this is the main part
  theme(legend.position = 'bottom') +
  scale_fill_gradientn(colours = c("#66bd63", "#a6d96a", "#d9ef8b", "#fee08b",
                                   "#fdae61", "#f46d43", "#d73027"),
                       breaks=c(3.770102,6.169963,7.084611,7.767289,8.558228,9.283746,11.084679 ),
                       labels= c("Min: 3.77","","","","","","Max: 11.08"),
                       limits = c(3,12),
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

quantiles <- quantile(ow_t$'2014', 
                      probs = seq(0, 1, length.out = no_classes + 1))
quantiles

c <- ggplot() +  
  geom_polygon(data=eo, aes(x=long, y=lat, group=group), 
               fill=NA, color="grey25", size=0.25) +
  geom_polygon(data=wo, aes(x=long, y=lat, group=group), 
               fill=NA, color="grey25", size=0.25) +
  geom_tile(data=ow_t, aes(x=x, y=y, fill=ow_t$'2014'), alpha=0.8) + 
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
       title = "Oakland, 2014",
       tag = "Figure 3",
       subtitle = "Extent: -122.3577, -122.096, 37.69776, 37.85148")+
  # this is the main part
  theme(legend.position = 'bottom') +
  scale_fill_gradientn(colours = c("#66bd63", "#a6d96a", "#d9ef8b", "#fee08b",
                                   "#fdae61", "#f46d43", "#d73027"),
                       breaks=c(4.042692,7.452572 ,8.738605 ,9.782466,10.666507,11.891909,14.105399),
                       labels= c("Min: 4.04","","","","","","Max: 14.11"),
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

quantiles <- quantile(ow_t$'2013', 
                      probs = seq(0, 1, length.out = no_classes + 1))
quantiles

d <- ggplot() +  
  geom_polygon(data=eo, aes(x=long, y=lat, group=group), 
               fill=NA, color="grey25", size=0.25) +
  geom_polygon(data=wo, aes(x=long, y=lat, group=group), 
               fill=NA, color="grey25", size=0.25) +
  geom_tile(data=ow_t, aes(x=x, y=y, fill=ow_t$'2013'), alpha=0.8) + 
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
       title = "Oakland, 2013",
       tag = "Figure 4",
       subtitle = "Extent: -122.3577, -122.096, 37.69776, 37.85148")+
  # this is the main part
  theme(legend.position = 'bottom') +
  scale_fill_gradientn(colours = c("#66bd63", "#a6d96a", "#d9ef8b", "#fee08b",
                                   "#fdae61", "#f46d43", "#d73027"),
                       breaks=c(4.042692,7.452572,8.738605,9.782466,10.666507,11.891909,14.105399),
                       labels= c("Min: 4.04","","","","","","Max: 14.10"),
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

P <- plot_grid(a, b, c, d)

save_plot("plot2by2_oak.png", P,
          ncol = 2, # we're saving a grid plot of 2 columns
          nrow = 2, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 1.3
)


