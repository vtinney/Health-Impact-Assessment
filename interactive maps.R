# packages ----------------------------------------------------------------
library(dplyr)
library(rmapshaper)
library(maptools)
library(highcharter)
library(geojsonio)
library(readr)
library(viridis)
library(purrr)
library(rgdal)
library(raster)
library(highcharter)
library(geojson)

# Pediatric Asthma --------------------------------------------------------------------

#import csv data file
setwd("C:/Users/vtinney/GoogleDrive/EDF_shared/Rates/Excel/CHIS/")
data <- read.csv('asthmac.csv')
data <- rename(data, value = Comb_Est)
data <- rename(data, zip = ZCTA5CE10)


#import shape file
setwd("C:/Users/vtinney/GoogleDrive/EDF_shared/Population/BA_Shape/bay/")
zip <- readOGR(dsn=getwd(), layer = "BA_zip")
map <- geojson_list(zip)

# colors
no_classes <- 6

colors <- viridis(no_classes + 2) %>% 
  rev() %>% 
  head(-1) %>% 
  tail(-1) %>% 
  gsub("FF$", "", .)

# brks <- quantile(data$value, probs = seq(0, 1, length.out = no_classes + 1))
brks <- c(min(data$value), c(0.114,0.135,0.154,0.164,0.183), max(data$value))

highchart(type = "map") %>% 
  # data part
  hc_add_series(mapData = map, data = data, type = "map",
                joinBy = c("ZCTA5CE10", "zip"), value = "value",
                borderWidth = 0) %>% 
  hc_colorAxis(dataClasses = color_classes(brks, colors)) %>% 
  # functionality
  hc_tooltip(headerFormat = "",
             pointFormat = "{point.zip}: {point.value}",
             valueDecimals = 2) %>% 
  hc_legend(align = "right", verticalAlign = "bottom", layout = "vertical",
            floating = TRUE) %>%
  hc_mapNavigation(enabled = FALSE) %>% # if TRUE to zoom the relief image dont zoom.
  # info
  hc_title(text = "Pediatric Asthma Self-Report") %>% 
  hc_subtitle(text = "Rate per 100") %>% 
  hc_credits(enabled = TRUE,
             text = "CHIS, 2014") %>% 
  # style
  hc_chart(backgroundColor = "transparent",
           events = list(
             load = JS("function(){ $(\"image\")[0].setAttribute('preserveAspectRatio', 'xMidYMid') }")
           ))

# Adult Asthma --------------------------------------------------------------------
#import csv data file
setwd("C:/Users/vtinney/GoogleDrive/EDF_shared/Rates/Excel/CHIS/")
data <- read.csv('asthmaa.csv')
data <- rename(data, value = Comb_Est)
data <- rename(data, zip = ZCTA5CE10)


#import shape file
setwd("C:/Users/vtinney/GoogleDrive/EDF_shared/Population/BA_Shape/bay/")
zip <- readOGR(dsn=getwd(), layer = "BA_zip")
map <- geojson_list(zip)

# colors
no_classes <- 6

colors <- viridis(no_classes + 2) %>% 
  rev() %>% 
  head(-1) %>% 
  tail(-1) %>% 
  gsub("FF$", "", .)

# brks <- quantile(data$value, probs = seq(0, 1, length.out = no_classes + 1))
brks <- c(min(data$value), c(0.114,0.135,0.154,0.164,0.183), max(data$value))

highchart(type = "map") %>% 
  # data part
  hc_add_series(mapData = map, data = data, type = "map",
                joinBy = c("ZCTA5CE10", "zip"), value = "value",
                borderWidth = 0) %>% 
  hc_colorAxis(dataClasses = color_classes(brks, colors)) %>% 
  # functionality
  hc_tooltip(headerFormat = "",
             pointFormat = "{point.zip}: {point.value}",
             valueDecimals = 2) %>% 
  hc_legend(align = "right", verticalAlign = "bottom", layout = "vertical",
            floating = TRUE) %>%
  hc_mapNavigation(enabled = FALSE) %>% # if TRUE to zoom the relief image dont zoom.
  # info
  hc_title(text = "Adult Asthma Self-Report") %>% 
  hc_subtitle(text = "Rate per 100") %>% 
  hc_credits(enabled = TRUE,
             text = "CHIS, 2014") %>% 
  # style
  hc_chart(backgroundColor = "transparent",
           events = list(
             load = JS("function(){ $(\"image\")[0].setAttribute('preserveAspectRatio', 'xMidYMid') }")
           ))

# Heart Disease --------------------------------------------------------------------
#import csv data file
setwd("C:/Users/vtinney/GoogleDrive/EDF_shared/Rates/Excel/CHIS/")
data <- read.csv('heartd.csv')
data <- rename(data, value = Comb_Est)
data <- rename(data, zip = ZCTA5CE10)


#import shape file
setwd("C:/Users/vtinney/GoogleDrive/EDF_shared/Population/BA_Shape/bay/")
zip <- readOGR(dsn=getwd(), layer = "BA_zip")
map <- geojson_list(zip)

# colors
no_classes <- 6

colors <- viridis(no_classes + 2) %>% 
  rev() %>% 
  head(-1) %>% 
  tail(-1) %>% 
  gsub("FF$", "", .)

# brks <- quantile(data$value, probs = seq(0, 1, length.out = no_classes + 1))
brks <- c(min(data$value), c(0.055,0.06,0.068,0.072,0.081), max(data$value))

highchart(type = "map") %>% 
  # data part
  hc_add_series(mapData = map, data = data, type = "map",
                joinBy = c("ZCTA5CE10", "zip"), value = "value",
                borderWidth = 0) %>% 
  hc_colorAxis(dataClasses = color_classes(brks, colors)) %>% 
  # functionality
  hc_tooltip(headerFormat = "",
             pointFormat = "{point.zip}: {point.value}",
             valueDecimals = 2) %>% 
  hc_legend(align = "right", verticalAlign = "bottom", layout = "vertical",
            floating = TRUE) %>%
  hc_mapNavigation(enabled = FALSE) %>% # if TRUE to zoom the relief image dont zoom.
  # info
  hc_title(text = "Heart Disease Self-Report") %>% 
  hc_subtitle(text = "Rate per 100") %>% 
  hc_credits(enabled = TRUE,
             text = "CHIS, 2014") %>% 
  # style
  hc_chart(backgroundColor = "transparent",
           events = list(
             load = JS("function(){ $(\"image\")[0].setAttribute('preserveAspectRatio', 'xMidYMid') }")
           ))

# Pediatric Asthma ER visits --------------------------------------------------------------------
#import csv data file
setwd("C:/Users/vtinney/GoogleDrive/EDF_shared/Rates/Excel/Ed visits asthma/")
data <- read.csv('asthmac_final.csv')

data <- rename(data, value = asthmac_f)
data <- rename(data, zip = ZCTA5CE10)


#import shape file
setwd("C:/Users/vtinney/GoogleDrive/EDF_shared/Population/BA_Shape/bay/")
zip <- readOGR(dsn=getwd(), layer = "BA_zip")
map <- geojson_list(zip)

# colors
no_classes <- 6

colors <- viridis(no_classes + 2) %>% 
  rev() %>% 
  head(-1) %>% 
  tail(-1) %>% 
  gsub("FF$", "", .)

# brks <- quantile(data$value, probs = seq(0, 1, length.out = no_classes + 1))
brks <- c(min(data$value), c(40,100,160,220,280), max(data$value))

highchart(type = "map") %>% 
  # data part
  hc_add_series(mapData = map, data = data, type = "map",
                joinBy = c("ZCTA5CE10", "zip"), value = "value",
                borderWidth = 0) %>% 
  hc_colorAxis(dataClasses = color_classes(brks, colors)) %>% 
  # functionality
  hc_tooltip(headerFormat = "",
             pointFormat = "{point.zip}: {point.value}",
             valueDecimals = 2) %>% 
  hc_legend(align = "right", verticalAlign = "bottom", layout = "vertical",
            floating = TRUE) %>%
  hc_mapNavigation(enabled = FALSE) %>% # if TRUE to zoom the relief image dont zoom.
  # info
  hc_title(text = "Pediatric Asthma ER visits") %>% 
  hc_subtitle(text = "Rate per 10,000") %>% 
  hc_credits(enabled = TRUE,
             text = "OSHPD, 2016") %>% 
  # style
  hc_chart(backgroundColor = "transparent",
           events = list(
             load = JS("function(){ $(\"image\")[0].setAttribute('preserveAspectRatio', 'xMidYMid') }")
           ))

# Adult Asthma ER visits --------------------------------------------------------------------
#import csv data file
setwd("C:/Users/vtinney/GoogleDrive/EDF_shared/Rates/Excel/Ed visits asthma/")
data <- read.csv('asthmaa_final.csv')

data <- rename(data, value = rate_all)
data <- rename(data, zip = ZCTA5CE10)


#import shape file
setwd("C:/Users/vtinney/GoogleDrive/EDF_shared/Population/BA_Shape/bay/")
zip <- readOGR(dsn=getwd(), layer = "BA_zip")
map <- geojson_list(zip)

# colors
no_classes <- 6

colors <- viridis(no_classes + 2) %>% 
  rev() %>% 
  head(-1) %>% 
  tail(-1) %>% 
  gsub("FF$", "", .)

# brks <- quantile(data$value, probs = seq(0, 1, length.out = no_classes + 1))
brks <- c(min(data$value), c(40,100,160,220,280), max(data$value))

highchart(type = "map") %>% 
  # data part
  hc_add_series(mapData = map, data = data, type = "map",
                joinBy = c("ZCTA5CE10", "zip"), value = "value",
                borderWidth = 0) %>% 
  hc_colorAxis(dataClasses = color_classes(brks, colors)) %>% 
  # functionality
  hc_tooltip(headerFormat = "",
             pointFormat = "{point.zip}: {point.value}",
             valueDecimals = 2) %>% 
  hc_legend(align = "right", verticalAlign = "bottom", layout = "vertical",
            floating = TRUE) %>%
  hc_mapNavigation(enabled = FALSE) %>% # if TRUE to zoom the relief image dont zoom.
  # info
  hc_title(text = "Adult Asthma ER visits") %>% 
  hc_subtitle(text = "Rate per 10,000") %>% 
  hc_credits(enabled = TRUE,
             text = "OSHPD, 2016") %>% 
  # style
  hc_chart(backgroundColor = "transparent",
           events = list(
             load = JS("function(){ $(\"image\")[0].setAttribute('preserveAspectRatio', 'xMidYMid') }")
           ))

# All Asthma ER visits --------------------------------------------------------------------
#import csv data file
setwd("C:/Users/vtinney/GoogleDrive/EDF_shared/Rates/Excel/Ed visits asthma/")
data <- read.csv('asthma_all_final.csv')

data <- rename(data, value = rate_all)
data <- rename(data, zip = ZCTA5CE10)


#import shape file
setwd("C:/Users/vtinney/GoogleDrive/EDF_shared/Population/BA_Shape/bay/")
zip <- readOGR(dsn=getwd(), layer = "BA_zip")
map <- geojson_list(zip)

# colors
no_classes <- 6

colors <- viridis(no_classes + 2) %>% 
  rev() %>% 
  head(-1) %>% 
  tail(-1) %>% 
  gsub("FF$", "", .)

# brks <- quantile(data$value, probs = seq(0, 1, length.out = no_classes + 1))
brks <- c(min(data$value), c(40,100,160,220,280), max(data$value))

highchart(type = "map") %>% 
  # data part
  hc_add_series(mapData = map, data = data, type = "map",
                joinBy = c("ZCTA5CE10", "zip"), value = "value",
                borderWidth = 0) %>% 
  hc_colorAxis(dataClasses = color_classes(brks, colors)) %>% 
  # functionality
  hc_tooltip(headerFormat = "",
             pointFormat = "{point.zip}: {point.value}",
             valueDecimals = 2) %>% 
  hc_legend(align = "right", verticalAlign = "bottom", layout = "vertical",
            floating = TRUE) %>%
  hc_mapNavigation(enabled = FALSE) %>% # if TRUE to zoom the relief image dont zoom.
  # info
  hc_title(text = "All Asthma ER visits") %>% 
  hc_subtitle(text = "Rate per 10,000") %>% 
  hc_credits(enabled = TRUE,
             text = "OSHPD, 2016") %>% 
  # style
  hc_chart(backgroundColor = "transparent",
           events = list(
             load = JS("function(){ $(\"image\")[0].setAttribute('preserveAspectRatio', 'xMidYMid') }")
           ))
