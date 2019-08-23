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
bay <- readOGR(dsn=getwd(), layer='bayco')
bay.f <- fortify(bay) %>% 
  mutate(id = as.numeric(id))

bay.cities <- readOGR(dsn=getwd(), layer='baycities')
bay.cities.f <- fortify(bay.cities) %>% 
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

#/////////////////////////////////////////////////////////////////////////////////////////////
# County aggregation

for (k in 1:length(files)){
  print(files[k])
  r <- raster(paste(files[k]))
  r <- crop(r, bay)
  r <- mask(r, bay)
  
  # Excess per County
  r[r == 0] <- NA
  zone.in <- bay
  raster.in <- r
  
  shp <- ZonalPipe(zone.in, raster.in, stat="sum")
  shp@data <- shp@data %>% mutate(id = row.names(.))
  shp_df <- fortify(shp, region = "id")
  shp_df <- shp_df %>% left_join(shp@data, by = c("id"="id"))
  shp_df <- as.data.frame(shp_df)
  shp_df[,ncol(shp_df)][shp_df[,ncol(shp_df)] == 0] <- NA
  r.min <- min(shp_df[,ncol(shp_df)],na.rm=TRUE)
  r.max <- max(shp_df[,ncol(shp_df)],na.rm=TRUE)
  
  r.mean <- (r.min+r.max)/2
  min.r.label <- round(r.min,3)
  max.r.label <- round(r.max,3)
  
  # Map of excess per county
  autoplot(map.latlon)  +
    geom_polygon(data = shp_df, aes(x = long, y = lat, group = group, fill = shp_df[,ncol(shp_df)]),alpha=0.7)+
    scale_fill_gradient2("Count (n) excess deaths
per County",
                         low = "#3ec267", 
                         mid = "#fff429",  #ff7e29
                         high = "#fc0339", ##ff1f40
                         midpoint = r.mean,
                         na.value='grey50',
                         limits=c(r.min, r.max),
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
              color = "grey60", size = 0.1)+
    labs(title=paste0(title$titles[k],sep=''),
         caption=paste0(caption$captions[k],sep=''),
         subtitle=paste0('Range: ',min.r.label,' to ',max.r.label,'. Extent: -123.534, -121.208, 36.893, 38.864.',sep=''))
  ggsave(paste0(title$titles[k],caption$captions[k],'.count.county.png',sep=''),dpi=300)
}


#/////////////////////////////////////////////////////////////////////////////////////////////
# City aggregation
for (k in 1:length(files)){
  print(files[k])
  r <- raster(paste(files[k]))
  r <- crop(r, bay.cities)
  r <- mask(r, bay.cities)
  
  # Excess per city
  r[r == 0] <- NA
  zone.in <- bay.cities
  raster.in <- r
  
  shp <- ZonalPipe(zone.in, raster.in, stat="sum")
  shp@data <- shp@data %>% mutate(id = row.names(.))
  shp_df <- fortify(shp, region = "id")
  shp_df <- shp_df %>% left_join(shp@data, by = c("id"="id"))
  shp_df <- as.data.frame(shp_df)
  shp_df[,ncol(shp_df)][shp_df[,ncol(shp_df)] == 0] <- NA
  r.min <- min(shp_df[,ncol(shp_df)],na.rm=TRUE)
  r.max <- max(shp_df[,ncol(shp_df)],na.rm=TRUE)
  
  r.mean <- (r.min+r.max)/2
  min.r.label <- round(r.min,3)
  max.r.label <- round(r.max,3)
  
  #Elevated risk
  er2_df <- shp_df
  er2_df$er <- shp_df[,ncol(shp_df)]
  er2_df$er[er2_df$er == 0] <- NA
  er2_df$er <- er2_df$er/r.min
  er2.min <- min(er2_df[,ncol(er2_df)],na.rm=TRUE)
  er2.max <- max(er2_df[,ncol(er2_df)],na.rm=TRUE)
  er2.mean <- (er2.max+er2.min)/2
  min.er2.label <- round(er2.min,3)
  max.er2.label <- round(er2.max,3)
  
  # Log z-score
  # Log z-score
  z_df <- shp_df
  z_df$log.z <- shp_df[,ncol(shp_df)]
  z_df$log.z[z_df$log.z == 0] <- NA
  z_df$log.z <- log(z_df$log.z)
  z_df$log.z <- scale(z_df$log.z)
  z_df$log.z <- rescale(z_df$log.z, to=c(-3,3))
  
  # Map of excess per grid cell
  autoplot(map.latlon)  +
    geom_polygon(data = bay.f, aes(x = long, y = lat, group = group), 
                 fill="grey50",alpha=0.5)+
    geom_polygon(data = shp_df, aes(x = long, y = lat, group = group, fill = shp_df[,ncol(shp_df)]),alpha=0.7)+
    scale_fill_gradient2("Count (n) excess deaths
per city in the Bay Area",
                         low = "#3ec267", 
                         mid = "#fff429",  #ff7e29
                         high = "#fc0339", ##ff1f40
                         midpoint = r.mean,
                         na.value='grey50',
                         limits=c(r.min, r.max),
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
    geom_path(data = bay.cities.f, aes(x = long, y = lat, group = group), 
              color = "white", size = 0.2)+
    labs(title=paste0(title$titles[k],sep=''),
         caption=paste0(caption$captions[k],sep=''),
         subtitle=paste0('Range: ',min.r.label,' to ',max.r.label,'. Extent: -123.534, -121.208, 36.893, 38.864.',sep=''))
  ggsave(paste0(title$titles[k],caption$captions[k],'.count.city.png',sep=''),dpi=300)
  
  autoplot(map.latlon)  +
    geom_polygon(data = bay.f, aes(x = long, y = lat, group = group), 
                 fill="grey50",alpha=0.5)+
    geom_polygon(data = er2_df, aes(x = long, y = lat, group = group, fill = er2_df[,ncol(er2_df)]),alpha=0.7)+
    scale_fill_gradient2("Elevated risk of excess deaths
number of times the lowest risk.",
                         low = "#3ec267", 
                         mid = "#fff429",  #ff7e29
                         high = "#fc0339", ##ff1f40
                         midpoint = er2.mean,
                         na.value='grey50',
                         limits=c(er2.min, er2.max),
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
    geom_path(data = bay.cities.f, aes(x = long, y = lat, group = group), 
              color = "white", size = 0.2)+
    labs(title=paste0(title$titles[k],sep=''),
         caption=paste0(caption$captions[k],sep=''),
         subtitle=paste0('Range: ',min.er2.label,' to ',max.er2.label,'. Extent: -123.534, -121.208, 36.893, 38.864.',sep=''))
  ggsave(paste0(title$titles[k],caption$captions[k],'.er2.cities.png',sep=''),dpi=300)
  
  autoplot(map.latlon)  +
    geom_polygon(data = ala.f, aes(x = long, y = lat, group = group), 
                 fill="grey50",alpha=0.5)+
    geom_polygon(data = z_df, aes(x = long, y = lat, group = group, fill = z_df[,ncol(z_df)]),alpha=0.7)+
    scale_fill_gradient2("Excess cases per city 
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
    geom_path(data = bay.cities.f, aes(x = long, y = lat, group = group), 
              color = "white", size = 0.2)+
    geom_path(data = bay.f, aes(x = long, y = lat, group = group), 
              color = "grey60", size = 0.5)+
    labs(title=paste0(title$titles[k],sep=''),
         subtitle='Extent: -122.342, -121.469, 37.454, 37.906.',
         caption=paste0(caption$captions[k],sep=''))
  ggsave(paste0(title$titles[k],caption$captions[k],'.z.cities.png',sep=''),dpi=300)
}