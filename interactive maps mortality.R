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

#Mortality All--------------------------------------------------------------------

#import csv data file
setwd("C:/Users/vtinney/GoogleDrive/EDF_shared/Rates/Excel/Mortality/")
data <- read.csv('mortality_all_final.csv')
data <- rename(data, value = all_final)
data <- rename(data, zip = geo_num)


#import shape file
setwd("C:/Users/vtinney/GoogleDrive/EDF_shared/Population/BA_Shape/bay/")
zip <- readOGR(dsn=getwd(), layer = "BA_zip")
map <- geojson_list(zip)

# quantiles
no_classes <- 6

quantiles <- quantile(data$value, 
                      probs = seq(0, 1, length.out = no_classes + 1))

quantiles # use this to make the breaks

colors <- viridis(no_classes + 2) %>% 
  rev() %>% 
  head(-1) %>% 
  tail(-1) %>% 
  gsub("FF$", "", .)

# brks <- quantile(data$value, probs = seq(0, 1, length.out = no_classes + 1))
brks <- c(min(data$value), c(0.4584025,0.5806000,0.6530612,0.7508062,0.9180894), max(data$value))

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
  hc_title(text = "All-cause mortality") %>% 
  hc_subtitle(text = "Rate, 2016") %>% 
  hc_credits(enabled = TRUE,
             text = "OSHPD & CDC, 2016") %>% 
  # style
  hc_chart(backgroundColor = "transparent",
           events = list(
             load = JS("function(){ $(\"image\")[0].setAttribute('preserveAspectRatio', 'xMidYMid') }")
           ))

# CVD Mortality --------------------------------------------------------------------
#import csv data file
#import csv data file
setwd("C:/Users/vtinney/GoogleDrive/EDF_shared/Rates/Excel/Mortality/")
data <- read.csv('mortality_all_final.csv')
data <- rename(data, value = cvd_final)
data <- rename(data, zip = geo_num)


#import shape file
setwd("C:/Users/vtinney/GoogleDrive/EDF_shared/Population/BA_Shape/bay/")
zip <- readOGR(dsn=getwd(), layer = "BA_zip")
map <- geojson_list(zip)

# colors
no_classes <- 6
quantiles <- quantile(data$value, 
                      probs = seq(0, 1, length.out = no_classes + 1))

quantiles

colors <- viridis(no_classes + 2) %>% 
  rev() %>% 
  head(-1) %>% 
  tail(-1) %>% 
  gsub("FF$", "", .)

# brks <- quantile(data$value, probs = seq(0, 1, length.out = no_classes + 1))
brks <- c(min(data$value), c(0.09155750,0.12137889,0.14194465,0.16539247,0.19918919), max(data$value))

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
  hc_title(text = "CVD Mortality") %>% 
  hc_subtitle(text = "Rate per 100, 2016") %>% 
  hc_credits(enabled = TRUE,
             text = "OSHPD & CDC, 2016") %>% 
  # style
  hc_chart(backgroundColor = "transparent",
           events = list(
             load = JS("function(){ $(\"image\")[0].setAttribute('preserveAspectRatio', 'xMidYMid') }")
           ))

# Stroke Mortality --------------------------------------------------------------------
#import csv data file
setwd("C:/Users/vtinney/GoogleDrive/EDF_shared/Rates/Excel/Mortality/")
data <- read.csv('mortality_all_final.csv')
data <- rename(data, value = stk_final)
data <- rename(data, zip = geo_num)


#import shape file
setwd("C:/Users/vtinney/GoogleDrive/EDF_shared/Population/BA_Shape/bay/")
zip <- readOGR(dsn=getwd(), layer = "BA_zip")
map <- geojson_list(zip)

# colors
no_classes <- 6

quantiles <- quantile(data$value, 
                      probs = seq(0, 1, length.out = no_classes + 1))

quantiles

colors <- viridis(no_classes + 2) %>% 
  rev() %>% 
  head(-1) %>% 
  tail(-1) %>% 
  gsub("FF$", "", .)

# brks <- quantile(data$value, probs = seq(0, 1, length.out = no_classes + 1))
brks <- c(min(data$value), c(0.02414484,0.031333408,0.035361640,0.043656310,0.058037293), max(data$value))

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
  hc_title(text = "Stroke Mortality") %>% 
  hc_subtitle(text = "Rate per 100") %>% 
  hc_credits(enabled = TRUE,
             text = "OSHPD & CDC, 2016") %>% 
  # style
  hc_chart(backgroundColor = "transparent",
           events = list(
             load = JS("function(){ $(\"image\")[0].setAttribute('preserveAspectRatio', 'xMidYMid') }")
           ))

