library(raster)
library(rgdal)
library(raster)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(ggmap)
library(scales)
library(extrafont)
loadfonts()
library(dplyr)
library(sf)
library(OpenStreetMap)
library(rJava)
#=========================================================================
# Input shapefiles
setwd('/home/vtinney/clip/')
oak <- readOGR(dsn=getwd(), layer='oak')
oak.f <- fortify(oak) %>% 
  mutate(id = as.numeric(id))


#oak_bb <- c(left = -122.3275, bottom = 37.71583, right = -122.1475, top = 37.8325)

map <- openmap(c(37.71583,-122.3275), c(37.8325,-122.1475),
               type = "esri-topo",
               mergeTiles = TRUE)
map.latlon <- openproj(map, projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")


# OAK
setwd("/home/vtinney/AF/no2/oak/")
list.files()

titles <- c("Oakland, CVD mortality and nitrogen dioxide, ages 25-99 years.",
"Oakland, CVD mortality and nitrogen dioxide, ages 25-99 years.",
"Oakland, all-cause mortality and nitrogen dioxide, ages 25-99 years.",
"Oakland, all-cause mortality and nitrogen dioxide, ages 25-99 years.",
"Oakland, CVD mortality and nitrogen dioxide, ages 65-99 years.",
"Oakland, CVD mortality and nitrogen dioxide, ages 65-99 years.",
"Oakland, CVD mortality and nitrogen dioxide, ages 65-99 years.",  
"Oakland, all-cause mortality and nitrogen dioxide, ages 65-99 years.",
"Oakland, all-cause mortality and nitrogen dioxide, ages 65-99 years.", 
"Oakland, asthma incidence and nitrogen dioxide, ages 0-17 years.",
"Oakland, asthma incidence and nitrogen dioxide, ages 0-17 years.",
"Oakland, all-cause mortality and nitrogen dioxide, ages 65-99 years.",
"Oakland, all-cause mortality and nitrogen dioxide, ages 65-99 years.",
"Oakland, asthma ER visits and nitrogen dioxide, ages 0-17 years.",
"Oakland, asthma ER visits and nitrogen dioxide, ages 0-17 years.",
"Oakland, asthma ER visits and nitrogen dioxide, ages 25-99 years.",
"Oakland, asthma ER visits and nitrogen dioxide, ages 25-99 years.",
"Oakland, asthma ER visits and nitrogen dioxide, ages 0-17 years.",
"Oakland, asthma ER visits and nitrogen dioxide, ages 0-17 years.",
"Oakland, asthma ER visits and nitrogen dioxide, ages 25-99 years.",
"Oakland, asthma ER visits and nitrogen dioxide, ages 25-99 years.")

title <- as.data.frame(titles)

captions <- c("Concentrations: Larkin et al. 2017. CRF: Atkinson et al., 2018.",
"Concentrations: GSV. CRF: Atkinson et al., 2018.",
"Concentrations: Larkin et al. 2017. CRF: Atkinson & Butland, 2018.",
"Concentrations: GSV. CRF: Atkinson & Butland, 2018.",
"Concentrations: Larkin et al. 2017. CRF: Eum et al. 2019, adjusted for PM2.5.",
"Concentrations: Larkin et al. 2017. CRF: Eum et al. 2019.",
"Concentrations: GSV. CRF: Eum et al. 2019.",  
"Concentrations: Larkin et al. 2017. CRF: Eum et al. 2019.",
"Concentrations: GSV. CRF: Eum et al. 2019.", 
"Concentrations: Larkin et al. 201. CRF: Khreis et al. 2017.",
"Concentrations: GSV. CRF: Khreis et al. 2017.",
"Concentrations: Larkin et al. 2017. CRF: Eum et al. 2019, adjusted for PM2.5.",
"Concentrations: GSV. CRF: Eum et al. 2019, adjusted for PM2.5.",
"Concentrations: Larkin et al. 2017. CRF: Orellano et al. 2017.",
"Concentrations: GSV. CRF: Orellano et al. 2017.",
"Concentrations: Larkin et al. 2017. CRF: Orellano et al. 2017.",
"Concentrations: GSV. CRF: Orellano et al. 2017.",
"Concentrations: Larkin et al. 2017. CRF: Zheng et al. 2015.",
"Concentrations: GSV. CRF: Zheng et al. 2015.",
"Concentrations: Larkin et al. 2017. CRF: Zheng et al. 2015.",
"Concentrations: GSV. CRF: Zheng et al. 2015.")

caption <- as.data.frame(captions)

files <- list.files(pattern = "\\.tif*", full.names=TRUE)
for (k in 1:length(files)){
  print(files[k])
  af <- raster(paste(files[k]))
  af <- crop(af, oak)
  af <- mask(af, oak)
  af[af == 0] <- NA
  
  af <- af*100
  min.af.label <- round(minValue(af),2)
  max.af.label <- round(maxValue(af),2)
  
  min.af <- minValue(af)
  max.af <- maxValue(af)
  mean.af <- (min.af+max.af)/2
  
  er <- ((af-min.af)/min.af)*100
  er.min <- minValue(er)
  er.max <- maxValue(er)
  er.mean <- (er.max+er.min)/2
  
  af.log <- log(af)
  z.af <- scale(af.log)
  
  af.df <- rasterToPoints(af)
  af.df <- data.frame(af.df)
  colnames(af.df) <- c('lon','lat','val')
  z.af.df <- rasterToPoints(z.af)
  z.af.df <- data.frame(z.af.df)
  colnames(z.af.df) <- c('lon','lat','val')
  z.af.df$val <- rescale(z.af.df$val, to=c(-3,3))
  er.df <- rasterToPoints(er)
  er.df <- data.frame(er.df)
  colnames(er.df) <- c('lon','lat','val') 
  
  autoplot(map.latlon)  +
    geom_polygon(data = oak.f, aes(x = long, y = lat, group = group), 
                 fill="grey50",alpha=0.5)+
    geom_tile(data=af.df,aes(lon, lat, fill = val),alpha=0.8) +
    scale_fill_gradient2("Attributable Fraction (%)",
                         low = "blue", #scales::muted()
                         high = "red",
                         midpoint = mean.af,
                         limits=c(min.af, max.af),
                         na.value = 'grey50',
                         guide = guide_colourbar(
                           direction = "horizontal",
                           label=TRUE,
                           keyheight = unit(2, units = "mm"),
                           title.position = 'top',
                           title.hjust = 0.5,
                           label.hjust = 0.5,
                           barwidth = 15,
                           nrow = 1,
                           byrow = T,
                           label.position = "bottom"))+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          plot.title=element_text(hjust = 0, size=11,family="DejaVu Sans Light"),
          plot.subtitle=element_text(hjust=0, size=10,family="DejaVu Sans Light"),
          plot.caption = element_text(hjust=0, size=9,family="DejaVu Sans Light"),
          legend.title=element_text(size=11, family="DejaVu Sans Light"),
          legend.text=element_text(size=11, family="DejaVu Sans Light"),
          axis.title=element_blank(),
          legend.position = 'bottom',
          legend.justification='center',
          legend.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          rect = element_blank())+
    geom_path(data = oak.f, aes(x = long, y = lat, group = group), 
              color = "grey60", size = 0.5)+
    labs(title=paste0(title$titles[k],sep=''),
         caption=paste0(caption$captions[k],sep=''),
         subtitle=paste0('Range: ',min.af.label,' to ',max.af.label,' %. Extent: -122.328, -122.148, 37.716, 37.832.'),sep='')
  ggsave(paste0(title$titles[k],caption$captions[k],'.af.png',sep=''),dpi=300)
  
  
  autoplot(map.latlon)  +
    geom_polygon(data = oak.f, aes(x = long, y = lat, group = group), 
                 fill="grey50",alpha=0.5)+
    geom_tile(data=z.af.df,aes(lon, lat, fill = val),alpha=0.8) +
    scale_fill_gradient2("Attributable Fraction 
log z-score",
                         low = "blue", #scales::muted()
                         high = "red",
                         midpoint = 0,
                         na.value = 'grey50',
                         guide = guide_colourbar(
                           direction = "horizontal",
                           label=TRUE,
                           keyheight = unit(2, units = "mm"),
                           title.position = 'top',
                           title.hjust = 0.5,
                           label.hjust = 0.5,
                           barwidth = 15,
                           nrow = 1,
                           byrow = T,
                           label.position = "bottom"))+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          plot.title=element_text(hjust = 0, size=11,family="DejaVu Sans Light"),
          plot.subtitle=element_text(hjust=0, size=10,family="DejaVu Sans Light"),
          plot.caption = element_text(hjust=0, size=9,family="DejaVu Sans Light"),
          legend.title=element_text(size=11, family="DejaVu Sans Light"),
          legend.text=element_text(size=11, family="DejaVu Sans Light"),
          axis.title=element_blank(),
          legend.position = 'bottom',
          legend.justification='center',
          legend.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          rect = element_blank())+
    geom_path(data = oak.f, aes(x = long, y = lat, group = group), 
              color = "grey60", size = 0.5)+
    labs(title=paste0(title$titles[k],sep=''),
         subtitle='Extent: -122.328, -122.148, 37.716, 37.832.',
         caption=paste0(caption$captions[k],sep=''))
  ggsave(paste0(title$titles[k],caption$captions[k],'.z.png',sep=''),dpi=300)
  
  autoplot(map.latlon)  +
    geom_polygon(data = oak.f, aes(x = long, y = lat, group = group), 
                 fill="grey50",alpha=0.5)+
    geom_tile(data=er.df,aes(lon, lat, fill = val),alpha=0.8) +
    scale_fill_gradient2("Attributable Fraction:
% difference from the lowest AF",
                         low = "blue", #scales::muted()
                         high = "red",
                         midpoint = er.mean,
                         limits = c(er.min, er.max),
                         na.value = 'grey50',
                         guide = guide_colourbar(
                           direction = "horizontal",
                           label=TRUE,
                           keyheight = unit(2, units = "mm"),
                           title.position = 'top',
                           title.hjust = 0.5,
                           label.hjust = 0.5,
                           barwidth = 15,
                           nrow = 1,
                           byrow = T,
                           label.position = "bottom"))+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          plot.title=element_text(hjust = 0, size=11,family="DejaVu Sans Light"),
          plot.subtitle=element_text(hjust=0, size=10,family="DejaVu Sans Light"),
          plot.caption = element_text(hjust=0, size=9,family="DejaVu Sans Light"),
          legend.title=element_text(size=11, family="DejaVu Sans Light"),
          legend.text=element_text(size=11, family="DejaVu Sans Light"),
          axis.title=element_blank(),
          legend.position = 'bottom',
          legend.justification='center',
          legend.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          rect = element_blank())+
    geom_path(data = oak.f, aes(x = long, y = lat, group = group), 
              color = "grey60", size = 0.5)+
    labs(title=paste0(title$titles[k],sep=''),
         subtitle='Extent: -122.328, -122.148, 37.716, 37.832.',
         caption=paste0(caption$captions[k],sep=''))
  ggsave(paste0(title$titles[k],caption$captions[k],'.er.png',sep=''),dpi=300)
}


setwd("/home/vtinney/AF/pm/oak/")
list.files()
files <- list.files(pattern = "\\.tif*", full.names=TRUE)

titles <- c("Oakland, all-cause mortality and fine particulate matter, ages 25-99 years",
"Oakland, CVD mortality and fine particulate matter, ages 65-99 years",
"Oakland, all-cause mortality and fine particulate matter, ages 25-99 years",
"Oakland, all-cause mortality and fine particulate matter, ages 65-99 years",     
"Oakland, all-cause mortality and fine particulate matter, ages 65-99 years",  
"Oakland, asthma incidence and fine particulate matter, ages 0-17 years",
"Oakland, all-cause mortality and fine particulate matter, ages 25-99 years",
"Oakland, asthma ER visits and fine particulate matter, ages 0-17 years",
"Oakland, CVD hospitalizations and fine particulate matter, ages 65-99 years",
"Oakland, all-cause mortality and fine particulate matter, ages 25-99 years")

captions <- c("Concentrations: Harvard dataset. CRF: Vodonos et al. 2018 parametric.",
"Concentrations: Harvard dataset. CRF: Thurston et al. 2016.",
"Concentrations: Harvard dataset. CRF: Lepuele et al. 2012.",
"Concentrations: Harvard dataset. CRF: Vodonos et al. 2018 continuous.",     
"Concentrations: Harvard dataset. CRF: Vodonos et al. 2018 parametric.",  
"Concentrations: Harvard dataset. CRF: Khreis et al. 2017.",
"Concentrations: Harvard dataset. CRF: Krewski et al. 2009.",
"Concentrations: Harvard dataset. CRF: Lim et al. 2016.",
"Concentrations: Harvard dataset. CRF: Bravo et al 2017.",
"Concentrations: Harvard dataset. CRF: Vodonos et al. 2018 continuous.")

title <- as.data.frame(titles)
caption <- as.data.frame(captions)

for (k in 1:length(files)){
  print(files[k])
  af <- raster(paste(files[k]))
  af <- crop(af, oak)
  af <- mask(af, oak)
  af[af == 0] <- NA
  
  af <- af*100
  min.af.label <- round(minValue(af),2)
  max.af.label <- round(maxValue(af),2)
  
  min.af <- minValue(af)
  max.af <- maxValue(af)
  mean.af <- (min.af+max.af)/2
  
  er <- ((af-min.af)/min.af)*100
  er.min <- minValue(er)
  er.max <- maxValue(er)
  er.mean <- (er.max+er.min)/2
  
  af.log <- log(af)
  z.af <- scale(af.log)
  
  af.df <- rasterToPoints(af)
  af.df <- data.frame(af.df)
  colnames(af.df) <- c('lon','lat','val')
  z.af.df <- rasterToPoints(z.af)
  z.af.df <- data.frame(z.af.df)
  colnames(z.af.df) <- c('lon','lat','val')
  z.af.df$val <- rescale(z.af.df$val, to=c(-3,3))
  er.df <- rasterToPoints(er)
  er.df <- data.frame(er.df)
  colnames(er.df) <- c('lon','lat','val') 
  
  autoplot(map.latlon)  +
    geom_polygon(data = oak.f, aes(x = long, y = lat, group = group), 
                 fill="grey50",alpha=0.5)+
    geom_tile(data=af.df,aes(lon, lat, fill = val),alpha=0.8) +
    scale_fill_gradient2("Attributable Fraction (%)",
                         low = "blue", #scales::muted()
                         high = "red",
                         midpoint = mean.af,
                         limits=c(min.af, max.af),
                         na.value = 'grey50',
                         guide = guide_colourbar(
                           direction = "horizontal",
                           label=TRUE,
                           keyheight = unit(2, units = "mm"),
                           title.position = 'top',
                           title.hjust = 0.5,
                           label.hjust = 0.5,
                           barwidth = 15,
                           nrow = 1,
                           byrow = T,
                           label.position = "bottom"))+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          plot.title=element_text(hjust = 0, size=11,family="DejaVu Sans Light"),
          plot.subtitle=element_text(hjust=0, size=10,family="DejaVu Sans Light"),
          plot.caption = element_text(hjust=0, size=9,family="DejaVu Sans Light"),
          legend.title=element_text(size=11, family="DejaVu Sans Light"),
          legend.text=element_text(size=11, family="DejaVu Sans Light"),
          axis.title=element_blank(),
          legend.position = 'bottom',
          legend.justification='center',
          legend.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          rect = element_blank())+
    geom_path(data = oak.f, aes(x = long, y = lat, group = group), 
              color = "grey60", size = 0.5)+
    labs(title=paste0(title$titles[k],sep=''),
         caption=paste0(caption$captions[k],sep=''),
         subtitle=paste0('Range: ',min.af.label,' to ',max.af.label,' %. Extent: -122.328, -122.148, 37.716, 37.832.'),sep='')
  ggsave(paste0(title$titles[k],caption$captions[k],'.af.png',sep=''),dpi=300)
  
  
  autoplot(map.latlon)  +
    geom_polygon(data = oak.f, aes(x = long, y = lat, group = group), 
                 fill="grey50",alpha=0.5)+
    geom_tile(data=z.af.df,aes(lon, lat, fill = val),alpha=0.8) +
    scale_fill_gradient2("Attributable Fraction 
log z-score",
                         low = "blue", #scales::muted()
                         high = "red",
                         midpoint = 0,
                         na.value = 'grey50',
                         guide = guide_colourbar(
                           direction = "horizontal",
                           label=TRUE,
                           keyheight = unit(2, units = "mm"),
                           title.position = 'top',
                           title.hjust = 0.5,
                           label.hjust = 0.5,
                           barwidth = 15,
                           nrow = 1,
                           byrow = T,
                           label.position = "bottom"))+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          plot.title=element_text(hjust = 0, size=11,family="DejaVu Sans Light"),
          plot.subtitle=element_text(hjust=0, size=10,family="DejaVu Sans Light"),
          plot.caption = element_text(hjust=0, size=9,family="DejaVu Sans Light"),
          legend.title=element_text(size=11, family="DejaVu Sans Light"),
          legend.text=element_text(size=11, family="DejaVu Sans Light"),
          axis.title=element_blank(),
          legend.position = 'bottom',
          legend.justification='center',
          legend.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          rect = element_blank())+
    geom_path(data = oak.f, aes(x = long, y = lat, group = group), 
              color = "grey60", size = 0.5)+
    labs(title=paste0(title$titles[k],sep=''),
         subtitle='Extent: -122.328, -122.148, 37.716, 37.832.',
         caption=paste0(caption$captions[k],sep=''))
  ggsave(paste0(title$titles[k],caption$captions[k],'.z.png',sep=''),dpi=300)
  
  autoplot(map.latlon)  +
    geom_polygon(data = oak.f, aes(x = long, y = lat, group = group), 
                 fill="grey50",alpha=0.5)+
    geom_tile(data=er.df,aes(lon, lat, fill = val),alpha=0.8) +
    scale_fill_gradient2("Attributable Fraction:
% difference from the lowest AF",
                         low = "blue", #scales::muted()
                         high = "red",
                         midpoint = er.mean,
                         limits = c(er.min, er.max),
                         na.value = 'grey50',
                         guide = guide_colourbar(
                           direction = "horizontal",
                           label=TRUE,
                           keyheight = unit(2, units = "mm"),
                           title.position = 'top',
                           title.hjust = 0.5,
                           label.hjust = 0.5,
                           barwidth = 15,
                           nrow = 1,
                           byrow = T,
                           label.position = "bottom"))+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          plot.title=element_text(hjust = 0, size=11,family="DejaVu Sans Light"),
          plot.subtitle=element_text(hjust=0, size=10,family="DejaVu Sans Light"),
          plot.caption = element_text(hjust=0, size=9,family="DejaVu Sans Light"),
          legend.title=element_text(size=11, family="DejaVu Sans Light"),
          legend.text=element_text(size=11, family="DejaVu Sans Light"),
          axis.title=element_blank(),
          legend.position = 'bottom',
          legend.justification='center',
          legend.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          rect = element_blank())+
    geom_path(data = oak.f, aes(x = long, y = lat, group = group), 
              color = "grey60", size = 0.5)+
    labs(title=paste0(title$titles[k],sep=''),
         subtitle='Extent: -122.328, -122.148, 37.716, 37.832.',
         caption=paste0(caption$captions[k],sep=''))
  ggsave(paste0(title$titles[k],caption$captions[k],'.er.png',sep=''),dpi=300)
}

setwd("/home/vtinney/AF/bc/")
list.files()
files <- list.files(pattern = "\\.tif*", full.names=TRUE)
titles <- c("Oakland, CVD mortality and black carbon, ages 25-99 years",
"Oakland, all-cause mortality and black carbon, ages 25-99 years",
"Oakland, CVD hospitalizations and black carbon, ages 65-99 years")

captions <- c("Concentrations: GSV. CRF: Janessen et al. 2011",
"Concentrations: GSV. CRF: Janssen et al. 2011",
"Concentrations: GSV. CRF: Peng et al. 2009")

caption <- as.data.frame(captions)
title <- as.data.frame(titles)

for (k in 1:length(files)){
  print(files[k])
  af <- raster(paste(files[k]))
  af <- crop(af, oak)
  af <- mask(af, oak)
  af[af == 0] <- NA
  af[af < 0.0005] <- NA
  
  af <- af*100
  min.af.label <- round(minValue(af),2)
  max.af.label <- round(maxValue(af),2)
  
  min.af <- minValue(af)
  max.af <- maxValue(af)
  mean.af <- (min.af+max.af)/2
  
  er <- ((af-min.af)/min.af)*100
  er.min <- minValue(er)
  er.max <- maxValue(er)
  er.mean <- (er.max+er.min)/2
  
  af.log <- log(af)
  z.af <- scale(af.log)
  
  af.df <- rasterToPoints(af)
  af.df <- data.frame(af.df)
  colnames(af.df) <- c('lon','lat','val')
  z.af.df <- rasterToPoints(z.af)
  z.af.df <- data.frame(z.af.df)
  colnames(z.af.df) <- c('lon','lat','val')
  z.af.df$val <- rescale(z.af.df$val, to=c(-3,3))
  er.df <- rasterToPoints(er)
  er.df <- data.frame(er.df)
  colnames(er.df) <- c('lon','lat','val') 
  
  autoplot(map.latlon)  +
    geom_polygon(data = oak.f, aes(x = long, y = lat, group = group), 
                 fill="grey50",alpha=0.5)+
    geom_tile(data=af.df,aes(lon, lat, fill = val),alpha=0.8) +
    scale_fill_gradient2("Attributable Fraction (%)",
                         low = "blue", #scales::muted()
                         high = "red",
                         midpoint = mean.af,
                         limits=c(min.af, max.af),
                         na.value = 'grey50',
                         guide = guide_colourbar(
                           direction = "horizontal",
                           label=TRUE,
                           keyheight = unit(2, units = "mm"),
                           title.position = 'top',
                           title.hjust = 0.5,
                           label.hjust = 0.5,
                           barwidth = 15,
                           nrow = 1,
                           byrow = T,
                           label.position = "bottom"))+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          plot.title=element_text(hjust = 0, size=11,family="DejaVu Sans Light"),
          plot.subtitle=element_text(hjust=0, size=10,family="DejaVu Sans Light"),
          plot.caption = element_text(hjust=0, size=9,family="DejaVu Sans Light"),
          legend.title=element_text(size=11, family="DejaVu Sans Light"),
          legend.text=element_text(size=11, family="DejaVu Sans Light"),
          axis.title=element_blank(),
          legend.position = 'bottom',
          legend.justification='center',
          legend.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          rect = element_blank())+
    geom_path(data = oak.f, aes(x = long, y = lat, group = group), 
              color = "grey60", size = 0.5)+
    labs(title=paste0(title$titles[k],sep=''),
         caption=paste0(caption$captions[k],sep=''),
         subtitle=paste0('Range: ',min.af.label,' to ',max.af.label,' %. Extent: -122.328, -122.148, 37.716, 37.832.'),sep='')
  ggsave(paste0(title$titles[k],caption$captions[k],'.af.png',sep=''),dpi=300)
  
  
  autoplot(map.latlon)  +
    geom_polygon(data = oak.f, aes(x = long, y = lat, group = group), 
                 fill="grey50",alpha=0.5)+
    geom_tile(data=z.af.df,aes(lon, lat, fill = val),alpha=0.8) +
    scale_fill_gradient2("Attributable Fraction 
log z-score",
                         low = "blue", #scales::muted()
                         high = "red",
                         midpoint = 0,
                         na.value = 'grey50',
                         guide = guide_colourbar(
                           direction = "horizontal",
                           label=TRUE,
                           keyheight = unit(2, units = "mm"),
                           title.position = 'top',
                           title.hjust = 0.5,
                           label.hjust = 0.5,
                           barwidth = 15,
                           nrow = 1,
                           byrow = T,
                           label.position = "bottom"))+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          plot.title=element_text(hjust = 0, size=11,family="DejaVu Sans Light"),
          plot.subtitle=element_text(hjust=0, size=10,family="DejaVu Sans Light"),
          plot.caption = element_text(hjust=0, size=9,family="DejaVu Sans Light"),
          legend.title=element_text(size=11, family="DejaVu Sans Light"),
          legend.text=element_text(size=11, family="DejaVu Sans Light"),
          axis.title=element_blank(),
          legend.position = 'bottom',
          legend.justification='center',
          legend.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          rect = element_blank())+
    geom_path(data = oak.f, aes(x = long, y = lat, group = group), 
              color = "grey60", size = 0.5)+
    labs(title=paste0(title$titles[k],sep=''),
         subtitle='Extent: -122.328, -122.148, 37.716, 37.832.',
         caption=paste0(caption$captions[k],sep=''))
  ggsave(paste0(title$titles[k],caption$captions[k],'.z.png',sep=''),dpi=300)
  
  autoplot(map.latlon)  +
    geom_polygon(data = oak.f, aes(x = long, y = lat, group = group), 
                 fill="grey50",alpha=0.5)+
    geom_tile(data=er.df,aes(lon, lat, fill = val),alpha=0.8) +
    scale_fill_gradient2("Attributable Fraction:
% difference from the lowest AF",
                         low = "blue", #scales::muted()
                         high = "red",
                         midpoint = er.mean,
                         limits = c(er.min, er.max),
                         na.value = 'grey50',
                         guide = guide_colourbar(
                           direction = "horizontal",
                           label=TRUE,
                           keyheight = unit(2, units = "mm"),
                           title.position = 'top',
                           title.hjust = 0.5,
                           label.hjust = 0.5,
                           barwidth = 15,
                           nrow = 1,
                           byrow = T,
                           label.position = "bottom"))+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          plot.title=element_text(hjust = 0, size=11,family="DejaVu Sans Light"),
          plot.subtitle=element_text(hjust=0, size=10,family="DejaVu Sans Light"),
          plot.caption = element_text(hjust=0, size=9,family="DejaVu Sans Light"),
          legend.title=element_text(size=11, family="DejaVu Sans Light"),
          legend.text=element_text(size=11, family="DejaVu Sans Light"),
          axis.title=element_blank(),
          legend.position = 'bottom',
          legend.justification='center',
          legend.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          rect = element_blank())+
    geom_path(data = oak.f, aes(x = long, y = lat, group = group), 
              color = "grey60", size = 0.5)+
    labs(title=paste0(title$titles[k],sep=''),
         subtitle='Extent: -122.328, -122.148, 37.716, 37.832.',
         caption=paste0(caption$captions[k],sep=''))
  ggsave(paste0(title$titles[k],caption$captions[k],'.er.png',sep=''),dpi=300)
}

#==============================================================================================================
#==============================================================================================================
#==============================================================================================================
#==============================================================================================================
#==============================================================================================================
setwd('/home/vtinney/clip/')
eo <- readOGR(dsn=getwd(), layer='EO')
eo.f <- fortify(eo) %>% 
  mutate(id = as.numeric(id))

map <- openmap(c(37.791334,-122.327816), c(37.832312,-122.253477),
               type = "esri-topo",
               mergeTiles = TRUE)
map.latlon <- openproj(map, projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

setwd("/home/vtinney/AF/no2/oak/")

titles <- c("East Oakland, CVD mortality and nitrogen dioxide, ages 25-99 years.",
"East Oakland, CVD mortality and nitrogen dioxide, ages 25-99 years.",
"East Oakland, all-cause mortality and nitrogen dioxide, ages 25-99 years.",
"East Oakland, all-cause mortality and nitrogen dioxide, ages 25-99 years.",
"East Oakland, CVD mortality and nitrogen dioxide, ages 65-99 years.",
"East Oakland, CVD mortality and nitrogen dioxide, ages 65-99 years.",
"East Oakland, CVD mortality and nitrogen dioxide, ages 65-99 years.",  
"East Oakland, all-cause mortality and nitrogen dioxide, ages 65-99 years.",
"East Oakland, all-cause mortality and nitrogen dioxide, ages 65-99 years.", 
"East Oakland, asthma incidence and nitrogen dioxide, ages 0-17 years.",
"East Oakland, asthma incidence and nitrogen dioxide, ages 0-17 years.",
"East Oakland, all-cause mortality and nitrogen dioxide, ages 65-99 years.",
"East Oakland, all-cause mortality and nitrogen dioxide, ages 65-99 years.",
"East Oakland, asthma ER visits and nitrogen dioxide, ages 0-17 years.",
"East Oakland, asthma ER visits and nitrogen dioxide, ages 0-17 years.",
"East Oakland, asthma ER visits and nitrogen dioxide, ages 25-99 years.",
"East Oakland, asthma ER visits and nitrogen dioxide, ages 25-99 years.",
"East Oakland, asthma ER visits and nitrogen dioxide, ages 0-17 years.",
"East Oakland, asthma ER visits and nitrogen dioxide, ages 0-17 years.",
"East Oakland, asthma ER visits and nitrogen dioxide, ages 25-99 years.",
"East Oakland, asthma ER visits and nitrogen dioxide, ages 25-99 years.")

title <- as.data.frame(titles)

captions <- c("Concentrations: Larkin et al. 2017. CRF: Atkinson et al., 2018.",
"Concentrations: GSV. CRF: Atkinson et al., 2018.",
"Concentrations: Larkin et al. 2017. CRF: Atkinson & Butland, 2018.",
"Concentrations: GSV. CRF: Atkinson & Butland, 2018.",
"Concentrations: Larkin et al. 2017. CRF: Eum et al. 2019, adjusted for PM2.5.",
"Concentrations: Larkin et al. 2017. CRF: Eum et al. 2019.",
"Concentrations: GSV. CRF: Eum et al. 2019.",  
"Concentrations: Larkin et al. 2017. CRF: Eum et al. 2019.",
"Concentrations: GSV. CRF: Eum et al. 2019.", 
"Concentrations: Larkin et al. 201. CRF: Khreis et al. 2017.",
"Concentrations: GSV. CRF: Khreis et al. 2017.",
"Concentrations: Larkin et al. 2017. CRF: Eum et al. 2019, adjusted for PM2.5.",
"Concentrations: GSV. CRF: Eum et al. 2019, adjusted for PM2.5.",
"Concentrations: Larkin et al. 2017. CRF: Orellano et al. 2017.",
"Concentrations: GSV. CRF: Orellano et al. 2017.",
"Concentrations: Larkin et al. 2017. CRF: Orellano et al. 2017.",
"Concentrations: GSV. CRF: Orellano et al. 2017.",
"Concentrations: Larkin et al. 2017. CRF: Zheng et al. 2015.",
"Concentrations: GSV. CRF: Zheng et al. 2015.",
"Concentrations: Larkin et al. 2017. CRF: Zheng et al. 2015.",
"Concentrations: GSV. CRF: Zheng et al. 2015.")

caption <- as.data.frame(captions)

files <- list.files(pattern = "\\.tif*", full.names=TRUE)
for (k in 1:length(files)){
  print(files[k])
  af <- raster(paste(files[k]))
  af <- crop(af, eo)
  af <- mask(af, eo)
  af[af == 0] <- NA
  
  af <- af*100
  min.af.label <- round(minValue(af),2)
  max.af.label <- round(maxValue(af),2)
  
  min.af <- minValue(af)
  max.af <- maxValue(af)
  mean.af <- (min.af+max.af)/2
  
  er <- ((af-min.af)/min.af)*100
  er.min <- minValue(er)
  er.max <- maxValue(er)
  er.mean <- (er.max+er.min)/2
  
  af.log <- log(af)
  z.af <- scale(af.log)
  
  af.df <- rasterToPoints(af)
  af.df <- data.frame(af.df)
  colnames(af.df) <- c('lon','lat','val')
  z.af.df <- rasterToPoints(z.af)
  z.af.df <- data.frame(z.af.df)
  colnames(z.af.df) <- c('lon','lat','val')
  z.af.df$val <- rescale(z.af.df$val, to=c(-3,3))
  er.df <- rasterToPoints(er)
  er.df <- data.frame(er.df)
  colnames(er.df) <- c('lon','lat','val') 
  
  autoplot(map.latlon)  +
    geom_polygon(data = eo.f, aes(x = long, y = lat, group = group), 
                 fill="grey50",alpha=0.5)+
    geom_tile(data=af.df,aes(lon, lat, fill = val),alpha=0.8) +
    scale_fill_gradient2("Attributable Fraction (%)",
                         low = "blue", #scales::muted()
                         high = "red",
                         midpoint = mean.af,
                         limits=c(min.af, max.af),
                         na.value = 'grey50',
                         guide = guide_colourbar(
                           direction = "horizontal",
                           label=TRUE,
                           keyheight = unit(2, units = "mm"),
                           title.position = 'top',
                           title.hjust = 0.5,
                           label.hjust = 0.5,
                           barwidth = 15,
                           nrow = 1,
                           byrow = T,
                           label.position = "bottom"))+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          plot.title=element_text(hjust = 0, size=11,family="DejaVu Sans Light"),
          plot.subtitle=element_text(hjust=0, size=10,family="DejaVu Sans Light"),
          plot.caption = element_text(hjust=0, size=9,family="DejaVu Sans Light"),
          legend.title=element_text(size=11, family="DejaVu Sans Light"),
          legend.text=element_text(size=11, family="DejaVu Sans Light"),
          axis.title=element_blank(),
          legend.position = 'bottom',
          legend.justification='center',
          legend.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          rect = element_blank())+
    geom_path(data = eo.f, aes(x = long, y = lat, group = group), 
              color = "grey60", size = 0.5)+
    labs(title=paste0(title$titles[k],sep=''),
         caption=paste0(caption$captions[k],sep=''),
         subtitle=paste0('Range: ',min.af.label,' to ',max.af.label,' %. Extent: -122.328, -122.148, 37.716, 37.832.'),sep='')
  ggsave(paste0(title$titles[k],caption$captions[k],'.af.png',sep=''),dpi=300)
  
  
  autoplot(map.latlon)  +
    geom_polygon(data = eo.f, aes(x = long, y = lat, group = group), 
                 fill="grey50",alpha=0.5)+
    geom_tile(data=z.af.df,aes(lon, lat, fill = val),alpha=0.8) +
    scale_fill_gradient2("Attributable Fraction 
log z-score",
                         low = "blue", #scales::muted()
                         high = "red",
                         midpoint = 0,
                         na.value = 'grey50',
                         guide = guide_colourbar(
                           direction = "horizontal",
                           label=TRUE,
                           keyheight = unit(2, units = "mm"),
                           title.position = 'top',
                           title.hjust = 0.5,
                           label.hjust = 0.5,
                           barwidth = 15,
                           nrow = 1,
                           byrow = T,
                           label.position = "bottom"))+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          plot.title=element_text(hjust = 0, size=11,family="DejaVu Sans Light"),
          plot.subtitle=element_text(hjust=0, size=10,family="DejaVu Sans Light"),
          plot.caption = element_text(hjust=0, size=9,family="DejaVu Sans Light"),
          legend.title=element_text(size=11, family="DejaVu Sans Light"),
          legend.text=element_text(size=11, family="DejaVu Sans Light"),
          axis.title=element_blank(),
          legend.position = 'bottom',
          legend.justification='center',
          legend.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          rect = element_blank())+
    geom_path(data = eo.f, aes(x = long, y = lat, group = group), 
              color = "grey60", size = 0.5)+
    labs(title=paste0(title$titles[k],sep=''),
         subtitle='Extent: -122.328, -122.148, 37.716, 37.832.',
         caption=paste0(caption$captions[k],sep=''))
  ggsave(paste0(title$titles[k],caption$captions[k],'.z.png',sep=''),dpi=300)
  
  autoplot(map.latlon)  +
    geom_polygon(data = eo.f, aes(x = long, y = lat, group = group), 
                 fill="grey50",alpha=0.5)+
    geom_tile(data=er.df,aes(lon, lat, fill = val),alpha=0.8) +
    scale_fill_gradient2("Attributable Fraction:
% difference from the lowest AF",
                         low = "blue", #scales::muted()
                         high = "red",
                         midpoint = er.mean,
                         limits = c(er.min, er.max),
                         na.value = 'grey50',
                         guide = guide_colourbar(
                           direction = "horizontal",
                           label=TRUE,
                           keyheight = unit(2, units = "mm"),
                           title.position = 'top',
                           title.hjust = 0.5,
                           label.hjust = 0.5,
                           barwidth = 15,
                           nrow = 1,
                           byrow = T,
                           label.position = "bottom"))+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          plot.title=element_text(hjust = 0, size=11,family="DejaVu Sans Light"),
          plot.subtitle=element_text(hjust=0, size=10,family="DejaVu Sans Light"),
          plot.caption = element_text(hjust=0, size=9,family="DejaVu Sans Light"),
          legend.title=element_text(size=11, family="DejaVu Sans Light"),
          legend.text=element_text(size=11, family="DejaVu Sans Light"),
          axis.title=element_blank(),
          legend.position = 'bottom',
          legend.justification='center',
          legend.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          rect = element_blank())+
    geom_path(data = eo.f, aes(x = long, y = lat, group = group), 
              color = "grey60", size = 0.5)+
    labs(title=paste0(title$titles[k],sep=''),
         subtitle='Extent: -122.328, -122.148, 37.716, 37.832.',
         caption=paste0(caption$captions[k],sep=''))
  ggsave(paste0(title$titles[k],caption$captions[k],'.er.png',sep=''),dpi=300)
}


setwd("/home/vtinney/AF/pm/oak/")
list.files()
files <- list.files(pattern = "\\.tif*", full.names=TRUE)

titles <- c("East Oakland, all-cause mortality and fine particulate matter, ages 25-99 years",
"East Oakland, CVD mortality and fine particulate matter, ages 65-99 years",
"East Oakland, all-cause mortality and fine particulate matter, ages 25-99 years",
"East Oakland, all-cause mortality and fine particulate matter, ages 65-99 years",     
"East Oakland, all-cause mortality and fine particulate matter, ages 65-99 years",  
"East Oakland, asthma incidence and fine particulate matter, ages 0-17 years",
"East Oakland, all-cause mortality and fine particulate matter, ages 25-99 years",
"East Oakland, asthma ER visits and fine particulate matter, ages 0-17 years",
"East Oakland, CVD hospitalizations and fine particulate matter, ages 65-99 years",
"East Oakland, all-cause mortality and fine particulate matter, ages 25-99 years")

captions <- c("Concentrations: Harvard dataset. CRF: Vodonos et al. 2018 parametric.",
"Concentrations: Harvard dataset. CRF: Thurston et al. 2016.",
"Concentrations: Harvard dataset. CRF: Lepuele et al. 2012.",
"Concentrations: Harvard dataset. CRF: Vodonos et al. 2018 continuous.",     
"Concentrations: Harvard dataset. CRF: Vodonos et al. 2018 parametric.",  
"Concentrations: Harvard dataset. CRF: Khreis et al. 2017.",
"Concentrations: Harvard dataset. CRF: Krewski et al. 2009.",
"Concentrations: Harvard dataset. CRF: Lim et al. 2016.",
"Concentrations: Harvard dataset. CRF: Bravo et al 2017.",
"Concentrations: Harvard dataset. CRF: Vodonos et al. 2018 continuous.")

title <- as.data.frame(titles)
caption <- as.data.frame(captions)

for (k in 1:length(files)){
  print(files[k])
  af <- raster(paste(files[k]))
  af <- crop(af, eo)
  af <- mask(af, eo)
  af[af == 0] <- NA
  
  af <- af*100
  min.af.label <- round(minValue(af),2)
  max.af.label <- round(maxValue(af),2)
  
  min.af <- minValue(af)
  max.af <- maxValue(af)
  mean.af <- (min.af+max.af)/2
  
  er <- ((af-min.af)/min.af)*100
  er.min <- minValue(er)
  er.max <- maxValue(er)
  er.mean <- (er.max+er.min)/2
  
  af.log <- log(af)
  z.af <- scale(af.log)
  
  af.df <- rasterToPoints(af)
  af.df <- data.frame(af.df)
  colnames(af.df) <- c('lon','lat','val')
  z.af.df <- rasterToPoints(z.af)
  z.af.df <- data.frame(z.af.df)
  colnames(z.af.df) <- c('lon','lat','val')
  z.af.df$val <- rescale(z.af.df$val, to=c(-3,3))
  er.df <- rasterToPoints(er)
  er.df <- data.frame(er.df)
  colnames(er.df) <- c('lon','lat','val') 
  
  autoplot(map.latlon)  +
    geom_polygon(data = eo.f, aes(x = long, y = lat, group = group), 
                 fill="grey50",alpha=0.5)+
    geom_tile(data=af.df,aes(lon, lat, fill = val),alpha=0.8) +
    scale_fill_gradient2("Attributable Fraction (%)",
                         low = "blue", #scales::muted()
                         high = "red",
                         midpoint = mean.af,
                         limits=c(min.af, max.af),
                         na.value = 'grey50',
                         guide = guide_colourbar(
                           direction = "horizontal",
                           label=TRUE,
                           keyheight = unit(2, units = "mm"),
                           title.position = 'top',
                           title.hjust = 0.5,
                           label.hjust = 0.5,
                           barwidth = 15,
                           nrow = 1,
                           byrow = T,
                           label.position = "bottom"))+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          plot.title=element_text(hjust = 0, size=11,family="DejaVu Sans Light"),
          plot.subtitle=element_text(hjust=0, size=10,family="DejaVu Sans Light"),
          plot.caption = element_text(hjust=0, size=9,family="DejaVu Sans Light"),
          legend.title=element_text(size=11, family="DejaVu Sans Light"),
          legend.text=element_text(size=11, family="DejaVu Sans Light"),
          axis.title=element_blank(),
          legend.position = 'bottom',
          legend.justification='center',
          legend.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          rect = element_blank())+
    geom_path(data = eo.f, aes(x = long, y = lat, group = group), 
              color = "grey60", size = 0.5)+
    labs(title=paste0(title$titles[k],sep=''),
         caption=paste0(caption$captions[k],sep=''),
         subtitle=paste0('Range: ',min.af.label,' to ',max.af.label,' %. Extent: -122.328, -122.148, 37.716, 37.832.'),sep='')
  ggsave(paste0(title$titles[k],caption$captions[k],'.af.png',sep=''),dpi=300)
  
  
  autoplot(map.latlon)  +
    geom_polygon(data = eo.f, aes(x = long, y = lat, group = group), 
                 fill="grey50",alpha=0.5)+
    geom_tile(data=z.af.df,aes(lon, lat, fill = val),alpha=0.8) +
    scale_fill_gradient2("Attributable Fraction 
log z-score",
                         low = "blue", #scales::muted()
                         high = "red",
                         midpoint = 0,
                         na.value = 'grey50',
                         guide = guide_colourbar(
                           direction = "horizontal",
                           label=TRUE,
                           keyheight = unit(2, units = "mm"),
                           title.position = 'top',
                           title.hjust = 0.5,
                           label.hjust = 0.5,
                           barwidth = 15,
                           nrow = 1,
                           byrow = T,
                           label.position = "bottom"))+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          plot.title=element_text(hjust = 0, size=11,family="DejaVu Sans Light"),
          plot.subtitle=element_text(hjust=0, size=10,family="DejaVu Sans Light"),
          plot.caption = element_text(hjust=0, size=9,family="DejaVu Sans Light"),
          legend.title=element_text(size=11, family="DejaVu Sans Light"),
          legend.text=element_text(size=11, family="DejaVu Sans Light"),
          axis.title=element_blank(),
          legend.position = 'bottom',
          legend.justification='center',
          legend.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          rect = element_blank())+
    geom_path(data =eo.f, aes(x = long, y = lat, group = group), 
              color = "grey60", size = 0.5)+
    labs(title=paste0(title$titles[k],sep=''),
         subtitle='Extent: -122.328, -122.148, 37.716, 37.832.',
         caption=paste0(caption$captions[k],sep=''))
  ggsave(paste0(title$titles[k],caption$captions[k],'.z.png',sep=''),dpi=300)
  
  autoplot(map.latlon)  +
    geom_polygon(data = eo.f, aes(x = long, y = lat, group = group), 
                 fill="grey50",alpha=0.5)+
    geom_tile(data=er.df,aes(lon, lat, fill = val),alpha=0.8) +
    scale_fill_gradient2("Attributable Fraction:
% difference from the lowest AF",
                         low = "blue", #scales::muted()
                         high = "red",
                         midpoint = er.mean,
                         limits = c(er.min, er.max),
                         na.value = 'grey50',
                         guide = guide_colourbar(
                           direction = "horizontal",
                           label=TRUE,
                           keyheight = unit(2, units = "mm"),
                           title.position = 'top',
                           title.hjust = 0.5,
                           label.hjust = 0.5,
                           barwidth = 15,
                           nrow = 1,
                           byrow = T,
                           label.position = "bottom"))+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          plot.title=element_text(hjust = 0, size=11,family="DejaVu Sans Light"),
          plot.subtitle=element_text(hjust=0, size=10,family="DejaVu Sans Light"),
          plot.caption = element_text(hjust=0, size=9,family="DejaVu Sans Light"),
          legend.title=element_text(size=11, family="DejaVu Sans Light"),
          legend.text=element_text(size=11, family="DejaVu Sans Light"),
          axis.title=element_blank(),
          legend.position = 'bottom',
          legend.justification='center',
          legend.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          rect = element_blank())+
    geom_path(data = eo.f, aes(x = long, y = lat, group = group), 
              color = "grey60", size = 0.5)+
    labs(title=paste0(title$titles[k],sep=''),
         subtitle='Extent: -122.328, -122.148, 37.716, 37.832.',
         caption=paste0(caption$captions[k],sep=''))
  ggsave(paste0(title$titles[k],caption$captions[k],'.er.png',sep=''),dpi=300)
}

setwd("/home/vtinney/AF/bc/")
list.files()
files <- list.files(pattern = "\\.tif*", full.names=TRUE)
titles <- c("East Oakland, CVD mortality and black carbon, ages 25-99 years",
"East Oakland, all-cause mortality and black carbon, ages 25-99 years",
"East Oakland, CVD hospitalizations and black carbon, ages 65-99 years")

captions <- c("Concentrations: GSV. CRF: Janessen et al. 2011",
"Concentrations: GSV. CRF: Janssen et al. 2011",
"Concentrations: GSV. CRF: Peng et al. 2009")

caption <- as.data.frame(captions)
title <- as.data.frame(titles)

for (k in 1:length(files)){
  print(files[k])
  af <- raster(paste(files[k]))
  af <- crop(af, eo)
  af <- mask(af, eo)
  af[af == 0] <- NA
  af[af < 0.0005] <- NA
  
  af <- af*100
  min.af.label <- round(minValue(af),2)
  max.af.label <- round(maxValue(af),2)
  
  min.af <- minValue(af)
  max.af <- maxValue(af)
  mean.af <- (min.af+max.af)/2
  
  er <- ((af-min.af)/min.af)*100
  er.min <- minValue(er)
  er.max <- maxValue(er)
  er.mean <- (er.max+er.min)/2
  
  af.log <- log(af)
  z.af <- scale(af.log)
  
  af.df <- rasterToPoints(af)
  af.df <- data.frame(af.df)
  colnames(af.df) <- c('lon','lat','val')
  z.af.df <- rasterToPoints(z.af)
  z.af.df <- data.frame(z.af.df)
  colnames(z.af.df) <- c('lon','lat','val')
  z.af.df$val <- rescale(z.af.df$val, to=c(-3,3))
  er.df <- rasterToPoints(er)
  er.df <- data.frame(er.df)
  colnames(er.df) <- c('lon','lat','val') 
  
  autoplot(map.latlon)  +
    geom_polygon(data = eo.f, aes(x = long, y = lat, group = group), 
                 fill="grey50",alpha=0.5)+
    geom_tile(data=af.df,aes(lon, lat, fill = val),alpha=0.8) +
    scale_fill_gradient2("Attributable Fraction (%)",
                         low = "blue", #scales::muted()
                         high = "red",
                         midpoint = mean.af,
                         limits=c(min.af, max.af),
                         na.value = 'grey50',
                         guide = guide_colourbar(
                           direction = "horizontal",
                           label=TRUE,
                           keyheight = unit(2, units = "mm"),
                           title.position = 'top',
                           title.hjust = 0.5,
                           label.hjust = 0.5,
                           barwidth = 15,
                           nrow = 1,
                           byrow = T,
                           label.position = "bottom"))+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          plot.title=element_text(hjust = 0, size=11,family="DejaVu Sans Light"),
          plot.subtitle=element_text(hjust=0, size=10,family="DejaVu Sans Light"),
          plot.caption = element_text(hjust=0, size=9,family="DejaVu Sans Light"),
          legend.title=element_text(size=11, family="DejaVu Sans Light"),
          legend.text=element_text(size=11, family="DejaVu Sans Light"),
          axis.title=element_blank(),
          legend.position = 'bottom',
          legend.justification='center',
          legend.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          rect = element_blank())+
    geom_path(data = eo.f, aes(x = long, y = lat, group = group), 
              color = "grey60", size = 0.5)+
    labs(title=paste0(title$titles[k],sep=''),
         caption=paste0(caption$captions[k],sep=''),
         subtitle=paste0('Range: ',min.af.label,' to ',max.af.label,' %. Extent: -122.328, -122.148, 37.716, 37.832.'),sep='')
  ggsave(paste0(title$titles[k],caption$captions[k],'.af.png',sep=''),dpi=300)
  
  
  autoplot(map.latlon)  +
    geom_polygon(data = eo.f, aes(x = long, y = lat, group = group), 
                 fill="grey50",alpha=0.5)+
    geom_tile(data=z.af.df,aes(lon, lat, fill = val),alpha=0.8) +
    scale_fill_gradient2("Attributable Fraction 
log z-score",
                         low = "blue", #scales::muted()
                         high = "red",
                         midpoint = 0,
                         na.value = 'grey50',
                         guide = guide_colourbar(
                           direction = "horizontal",
                           label=TRUE,
                           keyheight = unit(2, units = "mm"),
                           title.position = 'top',
                           title.hjust = 0.5,
                           label.hjust = 0.5,
                           barwidth = 15,
                           nrow = 1,
                           byrow = T,
                           label.position = "bottom"))+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          plot.title=element_text(hjust = 0, size=11,family="DejaVu Sans Light"),
          plot.subtitle=element_text(hjust=0, size=10,family="DejaVu Sans Light"),
          plot.caption = element_text(hjust=0, size=9,family="DejaVu Sans Light"),
          legend.title=element_text(size=11, family="DejaVu Sans Light"),
          legend.text=element_text(size=11, family="DejaVu Sans Light"),
          axis.title=element_blank(),
          legend.position = 'bottom',
          legend.justification='center',
          legend.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          rect = element_blank())+
    geom_path(data = eo.f, aes(x = long, y = lat, group = group), 
              color = "grey60", size = 0.5)+
    labs(title=paste0(title$titles[k],sep=''),
         subtitle='Extent: -122.328, -122.148, 37.716, 37.832.',
         caption=paste0(caption$captions[k],sep=''))
  ggsave(paste0(title$titles[k],caption$captions[k],'.z.png',sep=''),dpi=300)
  
  autoplot(map.latlon)  +
    geom_polygon(data = eo.f, aes(x = long, y = lat, group = group), 
                 fill="grey50",alpha=0.5)+
    geom_tile(data=er.df,aes(lon, lat, fill = val),alpha=0.8) +
    scale_fill_gradient2("Attributable Fraction:
% difference from the lowest AF",
                         low = "blue", #scales::muted()
                         high = "red",
                         midpoint = er.mean,
                         limits = c(er.min, er.max),
                         na.value = 'grey50',
                         guide = guide_colourbar(
                           direction = "horizontal",
                           label=TRUE,
                           keyheight = unit(2, units = "mm"),
                           title.position = 'top',
                           title.hjust = 0.5,
                           label.hjust = 0.5,
                           barwidth = 15,
                           nrow = 1,
                           byrow = T,
                           label.position = "bottom"))+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          plot.title=element_text(hjust = 0, size=11,family="DejaVu Sans Light"),
          plot.subtitle=element_text(hjust=0, size=10,family="DejaVu Sans Light"),
          plot.caption = element_text(hjust=0, size=9,family="DejaVu Sans Light"),
          legend.title=element_text(size=11, family="DejaVu Sans Light"),
          legend.text=element_text(size=11, family="DejaVu Sans Light"),
          axis.title=element_blank(),
          legend.position = 'bottom',
          legend.justification='center',
          legend.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          rect = element_blank())+
    geom_path(data = eo.f, aes(x = long, y = lat, group = group), 
              color = "grey60", size = 0.5)+
    labs(title=paste0(title$titles[k],sep=''),
         subtitle='Extent: -122.328, -122.148, 37.716, 37.832.',
         caption=paste0(caption$captions[k],sep=''))
  ggsave(paste0(title$titles[k],caption$captions[k],'.er.png',sep=''),dpi=300)
}