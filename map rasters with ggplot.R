#Crop and convert vonDonkelaar data
#2019-3-19

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
library(viridis)  # better colors for everyone
library(ggthemes) # theme_map()


setwd("C:/Users/vtinney/GoogleDrive/EDF_shared/Concentrations/JS/")
b16 <- raster('2016.tif')
b15 <- raster('2015.tif')
b14 <- raster('2014.tif')
b13 <- raster('2013.tif')
bay <- stack(b16, b15, b13, b14)
plot(bay)

test <- as(bay, "SpatialPixelsDataFrame")
bay_t <- as.data.frame(test)
colnames(bay_t) <- c("2016", "2015", "2014", "2013", "x", "y")

setwd("C:/Users/vtinney/GoogleDrive/EDF_shared/Population/BA_Shape/bay/")
bay_shp <- readOGR(dsn=getwd(), layer='BA_zip')



a <- ggplot() +  
  geom_tile(data=bay_t, aes(x=x, y=y, fill=bay_t$'2016'), alpha=0.8) + 
  geom_polygon(data=bay_shp, aes(x=long, y=lat, group=group), 
               fill=NA, color="grey50", size=0.25) +
  coord_equal() +
  theme_map() +
  theme(
    legend.position = c(0.5, 0.03),
    legend.text.align = 0,
    legend.background = element_rect(fill = alpha('white', 0.0)),
    legend.text = element_text(size = 7, hjust = 0, color = "#4e4d47", family="Cambria"),
    plot.title = element_text(hjust = 0.5, color = "#4e4d47", family="Cambria"),
    plot.subtitle = element_text(hjust = 0.5, color = "#4e4d47", family="Cambria",
                                 margin = margin(b = -0.1, 
                                                 t = -0.1, 
                                                 l = 2, 
                                                 unit = "cm"), 
                                 debug = F),
    legend.title = element_text(size = 8),
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
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm")) +
  labs(x = "Latitude", 
       y = "Longitude", 
       title = "2016")

a<- a +
  # this is the main part
  theme(legend.position = "bottom") +
  scale_fill_viridis(
    option = "viridis", 
    direction = -1,
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

b <- ggplot() +  
  geom_tile(data=bay_t, aes(x=x, y=y, fill=bay_t$'2015'), alpha=0.8) + 
  geom_polygon(data=bay_shp, aes(x=long, y=lat, group=group), 
               fill=NA, color="grey50", size=0.25) +
  coord_equal() +
  theme_map() +
  theme(
    legend.position = c(0.5, 0.03),
    legend.text.align = 0,
    legend.background = element_rect(fill = alpha('white', 0.0)),
    legend.text = element_text(size = 7, hjust = 0, color = "#4e4d47", family="Cambria"),
    plot.title = element_text(hjust = 0.5, color = "#4e4d47", family="Cambria"),
    plot.subtitle = element_text(hjust = 0.5, color = "#4e4d47", family="Cambria",
                                 margin = margin(b = -0.1, 
                                                 t = -0.1, 
                                                 l = 2, 
                                                 unit = "cm"), 
                                 debug = F),
    legend.title = element_text(size = 8),
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
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm")) +
  labs(x = "Latitude", 
       y = "Longitude", 
       title = "2015")

b <- b +
  # this is the main part
  theme(legend.position = "bottom") +
  scale_fill_viridis(
    option = "viridis", 
    direction = -1,
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


c <- ggplot() +  
  geom_tile(data=bay_t, aes(x=x, y=y, fill=bay_t$'2014'), alpha=0.8) + 
  geom_polygon(data=bay_shp, aes(x=long, y=lat, group=group), 
               fill=NA, color="grey50", size=0.25) +
  coord_equal() +
  theme_map() +
  theme(
    legend.position = c(0.5, 0.03),
    legend.text.align = 0,
    legend.background = element_rect(fill = alpha('white', 0.0)),
    legend.text = element_text(size = 7, hjust = 0, color = "#4e4d47", family="Cambria"),
    plot.title = element_text(hjust = 0.5, color = "#4e4d47", family="Cambria"),
    plot.subtitle = element_text(hjust = 0.5, color = "#4e4d47", family="Cambria",
                                 margin = margin(b = -0.1, 
                                                 t = -0.1, 
                                                 l = 2, 
                                                 unit = "cm"), 
                                 debug = F),
    legend.title = element_text(size = 8),
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
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm")) +
  labs(x = "Latitude", 
       y = "Longitude", 
       title = "2014")

c <- c +
  # this is the main part
  theme(legend.position = "bottom") +
  scale_fill_viridis(
    option = "viridis", 
    direction = -1,
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

d <- ggplot() +  
  geom_tile(data=bay_t, aes(x=x, y=y, fill=bay_t$'2013'), alpha=0.8) + 
  geom_polygon(data=bay_shp, aes(x=long, y=lat, group=group), 
               fill=NA, color="grey50", size=0.25) +
  coord_equal() +
  theme_map() +
  theme(
    legend.position = c(0.5, 0.03),
    legend.text.align = 0,
    legend.background = element_rect(fill = alpha('white', 0.0)),
    legend.text = element_text(size = 7, hjust = 0, color = "#4e4d47", family="Cambria"),
    plot.title = element_text(hjust = 0.5, color = "#4e4d47", family="Cambria"),
    plot.subtitle = element_text(hjust = 0.5, color = "#4e4d47", family="Cambria",
                                 margin = margin(b = -0.1, 
                                                 t = -0.1, 
                                                 l = 2, 
                                                 unit = "cm"), 
                                 debug = F),
    legend.title = element_text(size = 8),
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
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm")) +
  labs(x = "Latitude", 
       y = "Longitude", 
       title = "2013")

d <- d +
  # this is the main part
  theme(legend.position = "bottom") +
  scale_fill_viridis(
    option = "viridis", 
    direction = -1,
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

houston_shp <- readOGR(dsn=getwd(), layer='houston_co')

setwd("C:/Users/vtinney/GoogleDrive/EDF_Texas/")
t16 <- raster('2016.tif')
t15 <- raster('2015.tif')
t14 <- raster('2014.tif')
t13 <- raster('2013.tif')
houston <- stack(t16, t15, t14, t13)
plot(houston)

houston_shp <- readOGR(dsn=getwd(), layer='houston_co')


s <- ggplot() +  
  geom_tile(data=test_df, aes(x=x, y=y, fill=test_df$'2013'), alpha=0.8) + 
  geom_polygon(data=houston_shp, aes(x=long, y=lat, group=group), 
               fill=NA, color="grey50", size=0.25) +
  coord_equal() +
  theme_map() +
  theme(
    legend.position = c(0.5, 0.03),
    legend.text.align = 0,
    legend.background = element_rect(fill = alpha('white', 0.0)),
    legend.text = element_text(size = 7, hjust = 0, color = "#4e4d47", family="Cambria"),
    plot.title = element_text(hjust = 0.5, color = "#4e4d47", family="Cambria"),
    plot.subtitle = element_text(hjust = 0.5, color = "#4e4d47", family="Cambria",
                                 margin = margin(b = -0.1, 
                                                 t = -0.1, 
                                                 l = 2, 
                                                 unit = "cm"), 
                                 debug = F),
    legend.title = element_text(size = 8),
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
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm")) +
  labs(x = "Latitude", 
       y = "Longitude", 
       title = "2013")

s <- s +
  # this is the main part
  theme(legend.position = "bottom") +
  scale_fill_viridis(
    option = "viridis", 
    direction = -1,
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


p <- ggplot() +  
  geom_tile(data=test_df, aes(x=x, y=y, fill=test_df$'2014'), alpha=0.8) + 
  geom_polygon(data=houston_shp, aes(x=long, y=lat, group=group), 
               fill=NA, color="grey50", size=0.25) +
  coord_equal() +
  theme_map() +
  theme(
    legend.position = c(0.5, 0.03),
    legend.text.align = 0,
    legend.background = element_rect(fill = alpha('white', 0.0)),
    legend.text = element_text(size = 7, hjust = 0, color = "#4e4d47", family="Cambria"),
    plot.title = element_text(hjust = 0.5, color = "#4e4d47", family="Cambria"),
    plot.subtitle = element_text(hjust = 0.5, color = "#4e4d47", family="Cambria",
                                 margin = margin(b = -0.1, 
                                                 t = -0.1, 
                                                 l = 2, 
                                                 unit = "cm"), 
                                 debug = F),
    legend.title = element_text(size = 8),
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
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm")) +
  labs(x = "Latitude", 
       y = "Longitude", 
       title = "2014")

p <- p +
  # this is the main part
  theme(legend.position = "bottom") +
  scale_fill_viridis(
    option = "viridis", 
    direction = -1,
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

q <- ggplot() +  
  geom_tile(data=test_df, aes(x=x, y=y, fill=test_df$'2015'), alpha=0.8) + 
  geom_polygon(data=houston_shp, aes(x=long, y=lat, group=group), 
               fill=NA, color="grey50", size=0.25) +
  coord_equal() +
  theme_map() +
  theme(
    legend.position = c(0.5, 0.03),
    legend.text.align = 0,
    legend.background = element_rect(fill = alpha('white', 0.0)),
    legend.text = element_text(size = 7, hjust = 0, color = "#4e4d47", family="Cambria"),
    plot.title = element_text(hjust = 0.5, color = "#4e4d47", family="Cambria"),
    plot.subtitle = element_text(hjust = 0.5, color = "#4e4d47", family="Cambria",
                                 margin = margin(b = -0.1, 
                                                 t = -0.1, 
                                                 l = 2, 
                                                 unit = "cm"), 
                                 debug = F),
    legend.title = element_text(size = 8),
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
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm")) +
  labs(x = "Latitude", 
       y = "Longitude", 
       title = "2015")

q <- q +
  # this is the main part
  theme(legend.position = "bottom") +
  scale_fill_viridis(
    option = "viridis", 
    direction = -1,
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

t <- ggplot() +  
  geom_tile(data=test_df, aes(x=x, y=y, fill=test_df$'2016'), alpha=0.8) + 
  geom_polygon(data=houston_shp, aes(x=long, y=lat, group=group), 
               fill=NA, color="grey50", size=0.25) +
  coord_equal() +
  theme_map() +
  theme(
    legend.position = c(0.5, 0.03),
    legend.text.align = 0,
    legend.background = element_rect(fill = alpha('white', 0.0)),
    legend.text = element_text(size = 7, hjust = 0, color = "#4e4d47", family="Cambria"),
    plot.title = element_text(hjust = 0.5, color = "#4e4d47", family="Cambria"),
    plot.subtitle = element_text(hjust = 0.5, color = "#4e4d47", family="Cambria",
                                 margin = margin(b = -0.1, 
                                                 t = -0.1, 
                                                 l = 2, 
                                                 unit = "cm"), 
                                 debug = F),
    legend.title = element_text(size = 8),
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
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm")) +
  labs(x = "Latitude", 
       y = "Longitude", 
       title = "2016")

t <- t +
  # this is the main part
  theme(legend.position = "bottom") +
  scale_fill_viridis(
    option = "viridis", 
    direction = -1,
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


