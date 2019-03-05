extrafont::loadfonts(device="win")
library(ggplot2)

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

#set theme
theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Cambria", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.border = element_blank(),
      ...
    )
}

#import csv data file
setwd("C:/Users/vtinney/GoogleDrive/EDF_shared/Rates/Excel/Ed visits asthma/")
data <- read.csv('asthmac_final.csv')

#import shape file
setwd("C:/Users/vtinney/GoogleDrive/EDF_shared/Population/BA_Shape/bay/")
zip <- readOGR(dsn=getwd(), layer = "BA_zip")

#fortify data for ggplot
map_data_fortified <- fortify(zip, region = "ZCTA5CE10") %>% 
  mutate(id = as.numeric(id))

#link spatial data
map_data <- map_data_fortified %>% left_join(data, by = c("id" = "ZCTA5CE10"))

#import shaded relief
relief <- raster("input/geodata/02-relief-georef-clipped-resampled.tif")
relief_spdf <- as(relief, "SpatialPixelsDataFrame")
relief <- as.data.frame(relief_spdf) %>% 
  rename(value = `X02.relief.georef.clipped.resampled`)

# remove unnecessary variables
rm(relief_spdf)
rm(map_data_fortified)

########################################################################################
# Main plot
p <- ggplot() +
  # municipality polygons
  geom_polygon(data = map_data, aes(fill = asthmac_f, 
                                    x = long, 
                                    y = lat, 
                                    group = group)) +
  # municipality outline
  geom_path(data = map_data, aes(x = long, 
                                 y = lat, 
                                 group = group), 
            color = "white", size = 0.1) +
  coord_equal() +
  # add the previously defined basic theme
  theme_map() +
  labs(x = NULL, 
       y = NULL, 
       title = "Pediatric asthma Emergency Department Visits", 
       subtitle = "OSHPD, 2016", 
       caption = "Rate per 10,000")
p

#Better color scale

q <- p + scale_fill_viridis(option = "magma", direction = -1)
q

###############################################################################################
#horizontal legend

q <- p +
  # this is the main part
  theme(legend.position = "bottom") +
  scale_fill_viridis(
    option = "magma", 
    direction = -1,
    name = "ED visits, rate per 10,000",
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
q

#######################################################################################
# discrete classes

no_classes <- 4
labels <- c()

quantiles <- quantile(map_data$asthmac_f, 
                      probs = seq(0, 1, length.out = no_classes + 1))

# here I define custom labels (the default ones would be ugly)
labels <- c()
for(idx in 1:length(quantiles)){
  labels <- c(labels, paste0(round(quantiles[idx], 2), 
                             " – ", 
                             round(quantiles[idx + 1], 2)))
}
# I need to remove the last label 
# because that would be something like "66.62 - NA"
labels <- labels[1:length(labels)-1]

# here I actually create a new 
# variable on the dataset with the quantiles
map_data$asthmac_f_quantiles <- cut(map_data$asthmac_f, 
                                     breaks = quantiles, 
                                     labels = labels, 
                                     include.lowest = T)

p <- ggplot() +
  # municipality polygons (watch how I 
  # use the new variable for the fill aesthetic)
  geom_polygon(data = map_data, aes(fill = asthmac_f_quantiles, 
                                    x = long, 
                                    y = lat, 
                                    group = group)) +
  # municipality outline
  geom_path(data = map_data, aes(x = long, 
                                 y = lat, 
                                 group = group), 
            color = "white", size = 0.1) +
  coord_equal() +
  theme_map() +
  labs(x = NULL, 
       y = NULL, 
       title = "Pediatric Asthma Emergency Department Visits", 
       subtitle = "Average ER visits pediatric asthma, 2016", 
       caption = "rate per 10,000") +
  # now the discrete-option is used, 
  # and we use guide_legend instead of guide_colourbar
  scale_fill_viridis(
    option = "magma",
    name = "Average rate ED visit",
    discrete = T,
    direction = -1,
    guide = guide_legend(
      keyheight = unit(5, units = "mm"),
      title.position = 'top',
      reverse = T
    ))
p

##################################################################################################
# Discrete breaks
pretty_breaks <- c(60,80,100,160,200)
# find the extremes
minVal <- min(map_data$asthmac_f, na.rm = T)
maxVal <- max(map_data$asthmac_f, na.rm = T)
# compute labels
labels <- c()
brks <- c(minVal, pretty_breaks, maxVal)
# round the labels (actually, only the extremes)
for(idx in 1:length(brks)){
  labels <- c(labels,round(brks[idx + 1], 2))
}

labels <- labels[1:length(labels)-1]

map_data$brks <- cut(map_data$asthmac_f, 
                     breaks = brks, 
                     include.lowest = TRUE, 
                     labels = labels)
brks_scale <- levels(map_data$brks)
labels_scale <- rev(brks_scale)

p <- ggplot() +
  # municipality polygons
  geom_polygon(data = map_data, aes(fill = brks, 
                                    x = long, 
                                    y = lat, 
                                    group = group)) +
  # municipality outline
  geom_path(data = map_data, aes(x = long, 
                                 y = lat, 
                                 group = group), 
            color = "white", size = 0.1) +
  coord_equal() +
  theme_map() +
  theme(legend.position = "bottom") +
  labs(x = NULL, 
       y = NULL, 
       title = "Pediatric Asthma ER visits", 
       subtitle = "2016", 
       caption = "Rate per 10,000")

q <- p +
  # now we have to use a manual scale, 
  # because only ever one number should be shown per label
  scale_fill_manual(
    # in manual scales, one has to define colors, well, manually
    # I can directly access them using viridis' magma-function
    values = rev(magma(6)),
    breaks = rev(brks_scale),
    name = "Average ER visit rate",
    drop = FALSE,
    labels = labels_scale,
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(2, units = "mm"),
      keywidth = unit(70 / length(labels), units = "mm"),
      title.position = 'top',
      # I shift the labels around, the should be placed 
      # exactly at the right end of each legend key
      title.hjust = 0.5,
      label.hjust = 1,
      nrow = 1,
      byrow = T,
      # also the guide needs to be reversed
      reverse = T,
      label.position = "bottom"
    )
  )

################################################################################################
p <- ggplot() +
  # municipality polygons
  scale_alpha(name = "", range = c(0.6, 0), guide = F)  + 
  geom_polygon(data = map_data, aes(fill = brks, 
                                    x = long, 
                                    y = lat, 
                                    group = group)) +
  # municipality outline
  geom_path(data = map_data, aes(x = long, 
                                 y = lat, 
                                 group = group), 
            color = "white", size = 0.1) +
  coord_equal() +
  theme_map() +
  theme(
    legend.position = c(0.5, 0.03),
    legend.text.align = 0,
    legend.background = element_rect(fill = alpha('white', 0.0)),
    legend.text = element_text(size = 7, hjust = 0, color = "#4e4d47"),
    plot.title = element_text(hjust = 0.5, color = "#4e4d47"),
    plot.subtitle = element_text(hjust = 0.5, color = "#4e4d47", 
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
  labs(x = NULL, 
       y = NULL, 
       title = "Pediatric Asthma Emergency Department Visits", 
       subtitle = "Average ER Rate, 2016", 
       caption = "OSHPD, 2016") + 
  scale_fill_manual(
    values = rev(magma(8, alpha = 0.8)[2:7]),
    breaks = rev(brks_scale),
    name = "Average age",
    drop = FALSE,
    labels = labels_scale,
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(2, units = "mm"),
      keywidth = unit(70/length(labels), units = "mm"),
      title.position = 'top',
      title.hjust = 0.5,
      label.hjust = 1,
      nrow = 1,
      byrow = T,
      reverse = T,
      label.position = "bottom"
    )
  )
extendLegendWithExtremes(p)


##################################################################################################
#Histogram
ggplot(data = data, aes(x = asthmac_f)) + 
  geom_histogram(binwidth = 0.5) +
  theme_minimal() +
  xlab("Average ER visit rate") +
  ylab("Count")

##################################################################################################
# LIGHTER COLORS
# Discrete breaks
pretty_breaks <- c(56,113,170,226,283)
# find the extremes
minVal <- min(map_data$asthmac_f, na.rm = T)
maxVal <- max(map_data$asthmac_f, na.rm = T)
# compute labels
labels <- c()
brks <- c(minVal, pretty_breaks, maxVal)
# round the labels (actually, only the extremes)
for(idx in 1:length(brks)){
  labels <- c(labels,round(brks[idx + 1], 2))
}

labels <- labels[1:length(labels)-1]
# define a new variable on the data set just as above
map_data$brks <- cut(map_data$asthmac_f, 
                     breaks = brks, 
                     include.lowest = TRUE, 
                     labels = labels)

brks_scale <- levels(map_data$brks)
labels_scale <- rev(brks_scale)

p <- ggplot() +
  # municipality polygons
  geom_polygon(data = map_data, aes(fill = brks, 
                                    x = long, 
                                    y = lat, 
                                    group = group)) +
  # municipality outline
  geom_path(data = map_data, aes(x = long, 
                                 y = lat, 
                                 group = group), 
            color = "white", size = 0.1) +
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
  labs(x = NULL, 
       y = NULL, 
       title = "Pediatric Asthma ER visits", 
       subtitle = "Rate per 10,000", 
       caption = "OSHPD, 2016") + 
  scale_fill_manual(
    values = rev(magma(8, alpha = 0.8)[2:7]),
    breaks = rev(brks_scale),
    name = "Average ER visit rate",
    drop = FALSE,
    labels = labels_scale,
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(2, units = "mm"),
      keywidth = unit(70/length(labels), units = "mm"),
      title.position = 'top',
      title.hjust = 0.5,
      label.hjust = 1,
      nrow = 1,
      byrow = T,
      reverse = T,
      label.position = "bottom"
    )
  )

ggsave(p, filename = "asthmac_er.png", width = 8, height = 3, dpi=320)
