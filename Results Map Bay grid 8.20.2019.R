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
bay <- readOGR(dsn=getwd(), layer='bay')

bay.f <- fortify(bay) %>% 
  mutate(id = as.numeric(id))

#bay_bb <- c(left=-123.5335, bottom=36.97019, right=-121.5194, top=38.81954)

map <- openmap(c(36.89333,-123.5333), c(38.86417,-121.2083),
               type = "esri-topo",
               mergeTiles = TRUE)
map.latlon <- openproj(map, projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

setwd("/home/vtinney/results/main/bay/maps/")

list.files()
titles <- c("Bay Area, CVD mortality and nitrogen dioxide, ages 25-99 years.",
"Bay Area, all-cause mortality and nitrogen dioxide, ages 25-99 years.",
"Bay Area, CVD mortality and fine particulate matter, ages 25-99 years.",
"Bay Area, CVD mortality and nitrogen dioxide, ages 65-99 years.",
"Bay Area, CVD mortality and nitrogen dioxide, ages 65-99 years.",
"Bay Area, asthma incidence and nitrogen dioxide, ages 0-17 years.",
"Bay Area, asthma incidence and nitrogen dioxide, ages 0-17 years.",
"Bay Area, asthma incidence and fine particulate matter, ages 0-17 years.",
"Bay Area, all-cause mortality and fine particulate matter, ages 25-99 years.",
"Bay Area, all-cause mortality and fine particulate matter, ages 25-99 years.",
"Bay Area, asthma ER visits and nitrogen dioxide, ages 0-17 years.",
"Bay Area, asthma ER visits and nitrogen dioxide, all ages.",
"Bay Area, all-cause mortality and fine particulate matter, ages 65-99 years.",
"Bay Area, CVD mortality and fine particulate matter, ages 25-99 years.",
"Bay Area, asthma ER visits and fine particulate matter, ages 0-17 years.",
"Bay Area, CVD hospitalizations and fine particulate matter, ages 65-99 years.",
"Bay Area, all-cause mortality and fine particulate matter, ages 25-99 years.",
"Bay Area, CVD mortality and fine particulate matter, ages 25-99 years.",
"Bay Area, all-cause mortality and fine particulate matter, ages 25-99 years.",
"Bay Area, asthma ER visits and nitrogen dioxide, ages 0-17 years.",
"Bay Area, asthma ER visits and nitrogen dioxide, all ages.")


captions <- c("Concentrations: Larkin et al. 2017. CRF: Atkinson et al. 2018. Disease rates: County.",
"Concentrations: Larkin et al. 2017. CRF: Atkinson & Butland, 2018. Disease rates: County.",
"Concentrations: Harvard dataset. CRF: Vodonos et al. 2018, parametric. Disease rates: County.",
"Concentrations: Larkin et al. 2017. CRF: Eum et al. 2019, adjusted for fine particulate matter. Disease rates: County.",
"Concentrations: Larkin et al. 2017. CRF: Eum et al. 2019. Disease rates: County.",
"Concentrations: Larkin et al. 2017. CRF: Khreis et al. 2017. Disease rate: CA State.",
"Concentrations: GSV. CRF: Khreis et al. 2017. Disease rate: CA State.",
"Concentrations: Harvard dataset. CRF: Khreis et al. 2017. Disease rate: CA State.",
"Concentrations: Harvard dataset. CRF: Krewski et al. 2019. Disease rates: County.",
"Concentrations: Harvard dataset. CRF: Lepuele et al. 2012. Disease rates: County.",
"Concentrations: Larkin et al. 2017. CRF: Orellano et al. 2017. Disease rates: Zip-code.",
"Concentrations: Larkin et al. 2017. CRF: Orellano et al. 2017. Disease rates: Zip-code.",
"Concentrations: Harvard dataset. CRF: Di et al. 2017. Disease rates: County.",
"Concentrations: Harvard dataset. CRF: Thurston et al. 2016. Disease rates: County.",
"Concentrations: Harvard dataset. CRF: Lim et al. 2016. Disease rates: Zip-code.",
"Concentrations: Harvard dataset. CRF: Bravo et al. 2017. Disease rates: County.",
"Concentrations: Harvard dataset. CRF: Vodonos et al. 2018, continuous. Disease rates: County.",
"Concentrations: Harvard dataset. CRF: Vodonos et al. 2018, continuous. Disease rates: County.",
"Concentrations: Harvard dataset. CRF: Vodonos et al. 2018, parametric. Disease rates: County.",
"Concentrations: Larkin et al. 2017. CRF: Zheng et al. 2015. Disease rates: Zip-code.",
"Concentrations: Larkin et al. 2017. CRF: Zheng et al. 2015. Disease rates: Zip-code.")

#==================================================================================================

caption <- as.data.frame(captions)
title <- as.data.frame(titles)

files <- list.files(pattern = "\\.tif*", full.names=TRUE)

for (k in 1:length(files)){
  print(files[k])
  r <- raster(paste(files[k]))
  r <- crop(r, bay)
  r <- mask(r, bay)
  
  # Excess per grid cell
  r[r == 0] <- NA
  r.min <- minValue(r)
  r.max <- maxValue(r)
  min.r.label <- round(minValue(r),3)
  max.r.label <- round(maxValue(r),3)
  df <- rasterToPoints(r)
  df <- as.data.frame(df)
  colnames(df) <- c('lon','lat','val')
  df$val[df$val > r.max*0.4] <- r.max*0.4
  r.mean <- (min(df$val)+max(df$val))/2
  
  #Elevated risk
  er <- ((r-r.min)/r.min)*100
  er[er == 0] <- NA
  er.min <- minValue(er)
  er.max <- maxValue(er)
  min.er.label <- round(minValue(er),3)
  max.er.label <- round(maxValue(er),3)
  er.df <- rasterToPoints(er)
  er.df <- data.frame(er.df)
  colnames(er.df) <- c('lon','lat','val') 
  er.df$val[er.df$val > er.max*0.4] <- er.max*0.4
  er.mean <- (min(er.df$val)+max(er.df$val))/2
  
  
  # Log z-score
  r.log <- log(r)
  r.log[r.log == 0] <- NA
  z.log <- scale(r.log)
  z.df <- rasterToPoints(z.log)
  z.df <- data.frame(z.df)
  colnames(z.df) <- c('lon','lat','val')
  z.df$val <- rescale(z.df$val, to=c(-3,3))
  
  # Map of excess per grid cell
  autoplot(map.latlon)  +
    geom_polygon(data = bay.f, aes(x = long, y = lat, group = group), 
                 fill="grey50",alpha=0.5)+
    geom_tile(data=df,aes(lon, lat, fill = val),alpha=0.8) +
    scale_fill_gradient2("Count (n) per grid cell",
                         low = "#3ec267", 
                         mid = "#fff429",  #ff7e29
                         high = "#fc0339", ##ff1f40
                         midpoint = r.mean,
                         na.value='grey50',
                         limits=c(min(df$val), max(df$val)),
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
    geom_path(data = bay.f, aes(x = long, y = lat, group = group), 
              color = "grey60", size = 0.5)+
    labs(title=paste0(title$titles[k],sep=''),
         caption=paste0(caption$captions[k],sep=''),
         subtitle=paste0('Range: ',min.r.label,' to ',max.r.label,' per grid cell. Extent: -123.534, -121.208, 36.893, 38.864.'),sep='')
  ggsave(paste0(title$titles[k],caption$captions[k],'.count.grid.png',sep=''),dpi=300)
  
  autoplot(map.latlon)  +
    geom_polygon(data = bay.f, aes(x = long, y = lat, group = group), 
                 fill="grey50",alpha=0.5)+
    geom_tile(data=z.df,aes(lon, lat, fill = val),alpha=0.8) +
    scale_fill_gradient2("Excess cases per grid cell 
log z-score",
                         low = "blue", #scales::muted()
                         high = "red",
                         midpoint = 0,
                         na.value='grey50',
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
    geom_path(data = bay.f, aes(x = long, y = lat, group = group), 
              color = "grey60", size = 0.5)+
    labs(title=paste0(title$titles[k],sep=''),
         subtitle='Extent: -123.534, -121.208, 36.893, 38.864.',
         caption=paste0(caption$captions[k],sep=''))
  ggsave(paste0(title$titles[k],caption$captions[k],'.z.grid.png',sep=''),dpi=300)
  
  autoplot(map.latlon)  +
    geom_polygon(data = bay.f, aes(x = long, y = lat, group = group), 
                 fill="grey50",alpha=0.5)+
    geom_tile(data=er.df,aes(lon, lat, fill = val),alpha=0.8) +
    scale_fill_gradient2("Elevated risk of excess deaths
number of times the lowest risk.",
                         low = "#3ec267", 
                         mid = "#fff429",  #ff7e29
                         high = "#fc0339", ##ff1f40
                         midpoint = er.mean,
                         limits=c(min(er.df$val), max(er.df$val)),
                         na.value='grey50',
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
    geom_path(data = bay.f, aes(x = long, y = lat, group = group), 
              color = "grey60", size = 0.5)+
    labs(title=paste0(title$titles[k],sep=''),
         caption=paste0(caption$captions[k],sep=''),
         subtitle=paste0('Range: ',min.er.label,' to ',max.er.label,' per grid cell. Extent: -123.534, -121.208, 36.893, 38.864.'),sep='')
  ggsave(paste0(title$titles[k],caption$captions[k],'.er.grid.png',sep=''),dpi=300)
}