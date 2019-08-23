library(readxl)
library(sjPlot)
library(ggplot2)
library(ggthemes)


setwd("C:/Users/Veronica Tinney/Google Drive/EDF_shared/Results/")
r <- read_excel("all.aggregated.results.7.31.2019.xlsx")

r <- as.data.frame(r)

r$Extent <- factor(r$Extent,
                         levels = c("ala","bay","oak","alameda"),
                         labels = c("Alameda County", "Bay Area", "Oakland","Alameda County"))

r$Output <- factor(r$Output,
                         levels = c('paf','wrate'),
                         labels = c('Excess health outcome','Rate per 100,000'))

r$'Population Fraction' <- factor(r$'Population Fraction',
                                        levels = c('ls'),
                                        labels = c('GPW'))

r$'Population Dataset' <- factor(r$'Population Dataset',
                                       levels = c('night'),
                                       labels = c('LS Night'))

r$Concentration <- factor(r$Concentration,
                                levels = c('pm','larkin','NO2','BC'),
                                labels = c('Harvard dataset - PM2.5',
                                  'Larkin et al. 2017 - NO2', 'GSV - NO2','GSV - BC'))

r$Beta <- factor(r$Beta, 
                       levels = c("spline","thurston","cont","Lim","Bravo","krew","lepuele","di","khr","atkin", 
                                  "eum","ore","zheng","janssen","peng"),
                   labels = c('Vodonos et al. 2018
parametric','Thurston et al. 2016','Vodonos et al. 2018
continuous',
                              'Lim et al. 2016','Bravo et al. 2017','Krewski et al. 2009','Lepuele et al. 2012',
                              'Di et al. 2017','Khreis et al. 2017','Atkinson et al. 2018','Eum et al. 2019',
                              'Orellano et al. 2017','Zheng et al. 2015','Janssen et al. 2011',
                              'Peng et al. 2009'))

r$'Age group' <- factor(r$'Age group',
                              levels = c('25','65','all', '17'),
                              labels = c('25-99 years', '65-99 years', 'All ages', '0-17 years'))

r$Estimates <- factor(r$Estimates, 
                            levels = c('lower', 'point', 'upper'),
                            labels = c('Lower', "Point", "Upper"))

r$Rate <- factor(r$Rate,
                       levels <- c('co', 'cbg', 'ct', 'zip','state'),
                       labels <- c('County', 'Census Block Group', 'Census tract', 'Zip-code','State'))

r$Outcome <- factor(r$Outcome,
                      levels <- c("cvd","er","ha","all.cause","inc","cvd.ha","cvd.mort" ),
                      labels <- c('CVD mortality','Asthma ER visits','CVD hospitalizations',
                                  'All-cause mortality','Asthma incidence','CVD hospitalizations',
                                  'CVD mortality'))

r$Analysis <- factor(r$Analysis,
                       levels <- c('single','adj'),
                       labels <- c('Single pollutant','Adjusted for PM2.5'))

r$Concentration <- as.character(r$Concentration)
r$Extent <- as.character(r$Extent)
r$Output <- as.character(r$Output)
r$Outcome <- as.character(r$Outcome)
r$Beta <- as.character(r$Beta)
r$Analysis <- as.character(r$Analysis)
r$Estimates <- as.character(r$Estimates)
r$Rate <- as.character(r$Rate)
r$`Age group` <- as.character(r$`Age group`)
r$`Population Dataset` <- as.character(r$`Population Dataset`)
r$`Population Fraction` <- as.character(r$`Population Fraction`)

r.ex <- subset(r, Output == 'Excess health outcome')
r.rate <- subset(r, Output == 'Rate per 100,000')

r.rate$Estimate <- r.rate$Estimate*100000

r.ex$Estimate <- round(r.ex$Estimate,3)
r.rate$Estimate <- round(r.rate$Estimate,3)

r.ex.pm <- subset(r.ex, Concentration == 'Harvard dataset - PM2.5')
r.rate.pm <- subset(r.rate, Concentration == 'Harvard dataset - PM2.5')

r.ex.no2 <- subset(r.ex, Concentration == 'Larkin et al. 2017 - NO2')
r.rate.no2 <- subset(r.rate, Concentration == 'Larkin et al. 2017 - NO2')

r.ex.no2.gsv <- subset(r.ex, Concentration == 'GSV - NO2')
r.rate.no2.gsv <- subset(r.rate, Concentration == 'GSV - NO2')

r.ex.bc <- subset(r.ex, Concentration == 'GSV - BC')
r.rate.bc <- subset(r.rate, Concentration == 'GSV - BC')


#====================================================================================================
setwd("C:/Users/Veronica Tinney/Google Drive/EDF_shared/Results/tables/")

# PM2.5

extents <- c('Bay Area', 'Alameda County', 'Oakland')
outcomes <- c("CVD mortality","Asthma ER visits","CVD hospitalizations","All-cause mortality","Asthma incidence" )

# Loop for excess outcome (sum)

for (i in 1:length(extents)){
    print(extents[i])
    for (k in 1:length(outcomes)){
      print(outcomes[k])
      x <- subset(r.ex.pm, Extent == extents[i])
      x <- subset(x, Outcome == outcomes[k])
      x <- subset(x[,(c(4:8,12))])
      names(x) <- c("Beta","Analysis","Estimates","Rate","Age group","Estimate")
      x <- subset(x, Estimates == 'Point')
      x <- subset(x[,(c(1:2,4:6))])
      filename=paste0(extents[i],outcomes[k],'pm.ex.doc',sep='')
      sink(filename)
      p <- tab_df(x, 
             title=paste0(extents[i],' excess ',outcomes[k],' for fine particulate matter.',sep=' '),
             alternate.rows = TRUE,
             show.rownames = FALSE,
             sort.column=-5,
             show.footnote = TRUE,
             footnote='Population: LandScan USA night time population (2017); Population Fraction: Gridded Population of 
the World v4 (2010). Concentration dataset = Harvard dataset, mean concentrations 2015-2016.',
             file=filename)
      print(p)
      sink()
    }
    }

for (i in 1:length(extents)){
  print(extents[i])
  for (k in 1:length(outcomes)){
    print(outcomes[k])
    x <- subset(r.rate.pm, Extent == extents[i])
    x <- subset(x, Outcome == outcomes[k])
    x <- subset(x[,(c(4:8,12))])
    names(x) <- c("Beta","Analysis","Estimates","Rate","Age group","Estimate")
    x <- subset(x, Estimates == 'Point')
    x <- subset(x[,(c(1:2,4:6))])
    filename=paste0(extents[i],outcomes[k],'pm.rate.doc',sep='')
    sink(filename)
    p <- tab_df(x, 
                title=paste0(extents[i],' excess ',outcomes[k],' per 100,000 for fine particulate matter.',sep=' '),
                alternate.rows = TRUE,
                show.rownames = FALSE,
                sort.column=-5,
                show.footnote = TRUE,
                footnote='Population: LandScan USA night time population (2017); Population Fraction: Gridded Population of 
                the World v4 (2010). Concentration dataset = Harvard dataset, mean concentrations 2015-2016.',
                file=filename)
    print(p)
    sink()
  }
}

#=============================================================================================
# NO2 Larkin

extents <- c('Bay Area', 'Alameda County', 'Oakland')
outcomes <- c("CVD mortality","Asthma ER visits","All-cause mortality","Asthma incidence")

# Loop for excess outcome (sum)

for (i in 1:length(extents)){
  print(extents[i])
  for (k in 1:length(outcomes)){
    print(outcomes[k])
    x <- subset(r.ex.no2, Extent == extents[i])
    x <- subset(x, Outcome == outcomes[k])
    x <- subset(x[,(c(4:8,12))])
    names(x) <- c("Beta","Analysis","Estimates","Rate","Age group","Estimate")
    x <- subset(x, Estimates == 'Point')
    x <- subset(x[,(c(1:2,4:6))])
    filename=paste0(extents[i],outcomes[k],'no2.ex.doc',sep='')
    sink(filename)
    p <- tab_df(x, 
                title=paste0(extents[i],' excess ',outcomes[k],' for nitrogen dioxide.',sep=' '),
                alternate.rows = TRUE,
                show.rownames = FALSE,
                sort.column=-5,
                show.footnote = TRUE,
                footnote='Population: LandScan USA night time population (2017); Population Fraction: Gridded Population of 
                the World v4 (2010). Concentration dataset = Larkin et al. 2017.',
                file=filename)
    print(p)
    sink()
  }
}

for (i in 1:length(extents)){
  print(extents[i])
  for (k in 1:length(outcomes)){
    print(outcomes[k])
    x <- subset(r.rate.no2, Extent == extents[i])
    x <- subset(x, Outcome == outcomes[k])
    x <- subset(x[,(c(4:8,12))])
    names(x) <- c("Beta","Analysis","Estimates","Rate","Age group","Estimate")
    x <- subset(x, Estimates == 'Point')
    x <- subset(x[,(c(1:2,4:6))])
    filename=paste0(extents[i],outcomes[k],'no2.rate.doc',sep='')
    sink(filename)
    p <- tab_df(x, 
                title=paste0(extents[i],' excess ',outcomes[k],' per 100,000 for nitrogen dioxide.',sep=' '),
                alternate.rows = TRUE,
                show.rownames = FALSE,
                sort.column=-5,
                show.footnote = TRUE,
                footnote='Population: LandScan USA night time population (2017); Population Fraction: Gridded Population of 
                the World v4 (2010). Concentration dataset = Larkin et al. 2017.',
                file=filename)
    print(p)
    sink()
  }
}


#=============================================================================================
# NO2 GSV


extents <- c('Oakland')
outcomes <- c("CVD mortality","Asthma ER visits","All-cause mortality","Asthma incidence" )

# Loop for excess outcome (sum)

for (i in 1:length(extents)){
  print(extents[i])
  for (k in 1:length(outcomes)){
    print(outcomes[k])
    x <- subset(r.ex.no2.gsv, Extent == extents[i])
    x <- subset(x, Outcome == outcomes[k])
    x <- subset(x[,(c(4:8,12))])
    names(x) <- c("Beta","Analysis","Estimates","Rate","Age group","Estimate")
    x <- subset(x, Estimates == 'Point')
    x <- subset(x[,(c(1:2,4:6))])
    filename=paste0(extents[i],outcomes[k],'no2.gsv.ex.doc',sep='')
    sink(filename)
    p <- tab_df(x, 
                title=paste0(extents[i],' excess ',outcomes[k],' for nitrogen dioxide.',sep=' '),
                alternate.rows = TRUE,
                show.rownames = FALSE,
                sort.column=-5,
                show.footnote = TRUE,
                footnote='Population: LandScan USA night time population (2017); Population Fraction: Gridded Population of 
                the World v4 (2010). Concentration dataset = GSV.',
                file=filename)
    print(p)
    sink()
  }
}

for (i in 1:length(extents)){
  print(extents[i])
  for (k in 1:length(outcomes)){
    print(outcomes[k])
    x <- subset(r.rate.no2, Extent == extents[i])
    x <- subset(x, Outcome == outcomes[k])
    x <- subset(x[,(c(4:8,12))])
    names(x) <- c("Beta","Analysis","Estimates","Rate","Age group","Estimate")
    x <- subset(x, Estimates == 'Point')
    x <- subset(x[,(c(1:2,4:6))])
    filename=paste0(extents[i],outcomes[k],'no2..gsv.rate.doc',sep='')
    sink(filename)
    p <- tab_df(x, 
                title=paste0(extents[i],' excess ',outcomes[k],' per 100,000 for nitrogen dioxide.',sep=' '),
                alternate.rows = TRUE,
                show.rownames = FALSE,
                sort.column=-5,
                show.footnote = TRUE,
                footnote='Population: LandScan USA night time population (2017); Population Fraction: Gridded Population of 
                the World v4 (2010). Concentration dataset = GSV.',
                file=filename)
    print(p)
    sink()
  }
}

#=============================================================================================
# BC GSV


extents <- c('Oakland')
outcomes <- c("CVD mortality","All-cause mortality","Asthma incidence" )

# Loop for excess outcome (sum)

for (i in 1:length(extents)){
  print(extents[i])
  for (k in 1:length(outcomes)){
    print(outcomes[k])
    x <- subset(r.ex.bc, Extent == extents[i])
    x <- subset(x, Outcome == outcomes[k])
    x <- subset(x[,(c(4:8,12))])
    names(x) <- c("Beta","Analysis","Estimates","Rate","Age group","Estimate")
    x <- subset(x, Estimates == 'Point')
    x <- subset(x[,(c(1:2,4:6))])
    filename=paste0(extents[i],outcomes[k],'bc.ex.doc',sep='')
    sink(filename)
    p <- tab_df(x, 
                title=paste0(extents[i],' excess ',outcomes[k],' for black carbon.',sep=' '),
                alternate.rows = TRUE,
                show.rownames = FALSE,
                sort.column=-5,
                show.footnote = TRUE,
                footnote='Population: LandScan USA night time population (2017); Population Fraction: Gridded Population of 
                the World v4 (2010). Concentration dataset = GSV.',
                file=filename)
    print(p)
    sink()
  }
}

for (i in 1:length(extents)){
  print(extents[i])
  for (k in 1:length(outcomes)){
    print(outcomes[k])
    x <- subset(r.rate.bc, Extent == extents[i])
    x <- subset(x, Outcome == outcomes[k])
    x <- subset(x[,(c(4:8,12))])
    names(x) <- c("Beta","Analysis","Estimates","Rate","Age group","Estimate")
    x <- subset(x, Estimates == 'Point')
    x <- subset(x[,(c(1:2,4:6))])
    filename=paste0(extents[i],outcomes[k],'bc.rate.doc',sep='')
    sink(filename)
    p <- tab_df(x, 
                title=paste0(extents[i],' excess ',outcomes[k],' per 100,000 for black carbon.',sep=' '),
                alternate.rows = TRUE,
                show.rownames = FALSE,
                sort.column=-5,
                show.footnote = TRUE,
                footnote='Population: LandScan USA night time population (2017); Population Fraction: Gridded Population of 
                the World v4 (2010). Concentration dataset = GSV.',
                file=filename)
    print(p)
    sink()
  }
}

#=============================================================================================
# Boxplots

no2.plot <- subset(r.ex.no2, Estimates == 'Point')
no2.plot.2 <- subset(r.ex.no2.gsv, Estimates == 'Point')
no2.plot <- rbind(no2.plot, no2.plot.2)
no2.plot <- as.data.frame(no2.plot)

names(no2.plot)[8] <- 'Age'
names(no2.plot)[11] <- 'Concentration'

ages <- c('25-99 years', '65-99 years')
outcomes <- c("All-cause mortality")
extents <- c('Bay Area', 'Alameda County')

for (j in 1:length(extents)){
  print(extents[j])
  for (k in 1:length(outcomes)){
    print(outcomes[k])
    for (i in 1:length(ages)){
      print(ages[i])
      r <- subset(no2.plot, Outcome == outcomes[k])
      m <- subset(r, Extent == extents[j])
      z <- subset(m, Age == ages[i])
      p <- ggplot(z, x=Beta, group=Rate)+
        geom_boxplot(aes(x=Beta, ymin = Minimum, lower = q25, 
                         middle = Median, 
                         upper = q75, ymax = q75*1.5,
                         fill=Rate),
                     stat='identity')+
        ylab('Excess per grid cell')+
        xlab('Study Beta')+
        scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
        theme_fivethirtyeight()+
        theme(axis.text.x = element_text(size=9),
              axis.text.y = element_text(size=9),
              plot.title=element_text(size=10),
              plot.caption =element_text(size=8),
              axis.title=element_blank(),
              rect = element_blank())+
        labs(title=paste0('Nitrogen dioxide results comparison for ',extents[j],' 
',outcomes[k],', ',ages[i],sep=''),
             caption='Value per grid cell. Lower whisker=minimum; Upper whisker=1.5*IQR; Lower box= 25th percentile; 
             Upper box=75th percentile; Middle=median; Concentration dataset = Larkin et al. 2017;
             Population dataset= LandScan USA night time population (2017).')
      ggsave(paste("C:/Users/Veronica Tinney/Google Drive/EDF_shared/Results/boxplots/",ages[i],extents[j],outcomes[k],".png",sep=""), dpi=300, width=6, height=5)
    }}}

ages <- c('25-99 years', '65-99 years')
outcomes <- c("CVD mortality")
extents <- c('Bay Area', 'Alameda County')

for (j in 1:length(extents)){
  print(extents[j])
  for (k in 1:length(outcomes)){
    print(outcomes[k])
    for (i in 1:length(ages)){
      print(ages[i])
      r <- subset(no2.plot, Outcome == outcomes[k])
      m <- subset(r, Extent == extents[j])
      z <- subset(m, Age == ages[i])
      p <- ggplot(z, x=Beta, group=Rate)+
        geom_boxplot(aes(x=Beta, ymin = Minimum, lower = q25, 
                         middle = Median, 
                         upper = q75, ymax = q75*1.5,
                         fill=Rate),
                     stat='identity')+
        ylab('Excess per grid cell')+
        xlab('Study Beta')+
        scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
        theme_fivethirtyeight()+
        facet_grid(cols=vars(Analysis))+
        theme(axis.text.x = element_text(size=9),
              axis.text.y = element_text(size=9),
              plot.title=element_text(size=10),
              plot.caption =element_text(size=8),
              axis.title=element_blank(),
              rect = element_blank())+
        labs(title=paste0('Nitrogen dioxide results comparison for ',extents[j],' 
',outcomes[k],', ',ages[i],sep=''),
             caption='Value per grid cell. Lower whisker=minimum; Upper whisker=1.5*IQR; Lower box= 25th percentile; 
             Upper box=75th percentile; Middle=median; Concentration dataset = Larkin et al. 2017;
             Population dataset= LandScan USA night time population (2017).')
      ggsave(paste("C:/Users/Veronica Tinney/Google Drive/EDF_shared/Results/boxplots/",ages[i],extents[j],outcomes[k],".png",sep=""), dpi=300, width=6, height=5)
    }}}

ages <- c('25-99 years', '65-99 years')
outcomes <- c("CVD mortality")
extents <- c('Oakland')

for (j in 1:length(extents)){
  print(extents[j])
  for (k in 1:length(outcomes)){
    print(outcomes[k])
    for (i in 1:length(ages)){
      print(ages[i])
      r <- subset(no2.plot, Outcome == outcomes[k])
      m <- subset(r, Extent == extents[j])
      z <- subset(m, Age == ages[i])
      p <- ggplot(z, x=Beta, group=Rate)+
        geom_boxplot(aes(x=Beta, ymin = Minimum, lower = q25, 
                         middle = Median, 
                         upper = q75, ymax = q75*1.5,
                         fill=Rate),
                     stat='identity')+
        ylab('Excess per grid cell')+
        xlab('Study Beta')+
        scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
        theme_fivethirtyeight()+
        facet_grid(cols=vars(Concentration), rows=vars(Analysis))+
        theme(axis.text.x = element_text(size=9),
              axis.text.y = element_text(size=9),
              plot.title=element_text(size=10),
              plot.caption =element_text(size=8),
              axis.title=element_blank(),
              rect = element_blank())+
        labs(title=paste0('Nitrogen dioxide results comparison for ',extents[j],' 
',outcomes[k],', ',ages[i],sep=''),
             caption='Value per grid cell. Lower whisker=minimum; Upper whisker=1.5*IQR; Lower box= 25th percentile; 
             Upper box=75th percentile; Middle=median; Population dataset= LandScan USA night time population (2017).')
      ggsave(paste("C:/Users/Veronica Tinney/Google Drive/EDF_shared/Results/boxplots/",ages[i],extents[j],outcomes[k],".png",sep=""), dpi=300, width=6, height=5)
    }}}


ages <- c('25-99 years', '65-99 years')
outcomes <- c("All-cause mortality")
extents <- c('Oakland')

for (j in 1:length(extents)){
  print(extents[j])
  for (k in 1:length(outcomes)){
    print(outcomes[k])
    for (i in 1:length(ages)){
      print(ages[i])
      r <- subset(no2.plot, Outcome == outcomes[k])
      m <- subset(r, Extent == extents[j])
      z <- subset(m, Age == ages[i])
      p <- ggplot(z, x=Beta, group=Rate)+
        geom_boxplot(aes(x=Beta, ymin = Minimum, lower = q25, 
                         middle = Median, 
                         upper = q75, ymax = q75*1.5,
                         fill=Rate),
                     stat='identity')+
        ylab('Excess per grid cell')+
        xlab('Study Beta')+
        scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
        theme_fivethirtyeight()+
        facet_grid(cols=vars(Concentration), rows=vars(Analysis))+
        theme(axis.text.x = element_text(size=9),
              axis.text.y = element_text(size=9),
              plot.title=element_text(size=10),
              plot.caption =element_text(size=8),
              axis.title=element_blank(),
              rect = element_blank())+
        labs(title=paste0('Nitrogen dioxide results comparison for ',extents[j],' 
',outcomes[k],', ',ages[i],sep=''),
             caption='Value per grid cell. Lower whisker=minimum; Upper whisker=1.5*IQR; Lower box= 25th percentile; 
             Upper box=75th percentile; Middle=median; Population dataset= LandScan USA night time population (2017).')
      ggsave(paste("C:/Users/Veronica Tinney/Google Drive/EDF_shared/Results/boxplots/",ages[i],extents[j],outcomes[k],".png",sep=""), dpi=300, width=6, height=5)
    }}}


ages <- c('0-17 years', 'All ages')
outcomes <- c("Asthma ER visits")
extents <- c('Oakland')

for (j in 1:length(extents)){
  print(extents[j])
  for (k in 1:length(outcomes)){
    print(outcomes[k])
    for (i in 1:length(ages)){
      print(ages[i])
      r <- subset(no2.plot, Outcome == outcomes[k])
      m <- subset(r, Extent == extents[j])
      z <- subset(m, Age == ages[i])
      p <- ggplot(z, x=Beta)+
        geom_boxplot(aes(x=Beta, ymin = Minimum, lower = q25, 
                         middle = Median, 
                         upper = q75, ymax = q75*1.5,
                         fill=Beta),
                     stat='identity')+
        ylab('Excess per grid cell')+
        xlab('Study Beta')+
        scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
        theme_fivethirtyeight()+
        facet_grid(cols=vars(Concentration))+
        theme(axis.text.x = element_text(size=9),
              axis.text.y = element_text(size=9),
              plot.title=element_text(size=10),
              plot.caption =element_text(size=8),
              axis.title=element_blank(),
              rect = element_blank())+
        labs(title=paste0('Nitrogen dioxide results comparison for ',extents[j],' 
',outcomes[k],', ',ages[i],sep=''),
             caption='Value per grid cell. Lower whisker=minimum; Upper whisker=1.5*IQR; Lower box= 25th percentile; 
             Upper box=75th percentile; Middle=median; Population dataset= LandScan USA night time population (2017).')
      ggsave(paste("C:/Users/Veronica Tinney/Google Drive/EDF_shared/Results/boxplots/",ages[i],extents[j],outcomes[k],".png",sep=""), dpi=300, width=6, height=5)
    }}}

ages <- c('0-17 years', 'All ages')
outcomes <- c("Asthma ER visits")
extents <- c('Bay Area','Alameda County')

for (j in 1:length(extents)){
  print(extents[j])
  for (k in 1:length(outcomes)){
    print(outcomes[k])
    for (i in 1:length(ages)){
      print(ages[i])
      r <- subset(no2.plot, Outcome == outcomes[k])
      m <- subset(r, Extent == extents[j])
      z <- subset(m, Age == ages[i])
      p <- ggplot(z, x=Beta)+
        geom_boxplot(aes(x=Beta, ymin = Minimum, lower = q25, 
                         middle = Median, 
                         upper = q75, ymax = q75*1.5,
                         fill=Beta),
                     stat='identity')+
        ylab('Excess per grid cell')+
        xlab('Study Beta')+
        scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
        theme_fivethirtyeight()+
        #facet_grid(cols=vars(Concentration))+
        theme(axis.text.x = element_text(size=9),
              axis.text.y = element_text(size=9),
              plot.title=element_text(size=10),
              plot.caption =element_text(size=8),
              axis.title=element_blank(),
              rect = element_blank())+
        labs(title=paste0('Nitrogen dioxide results comparison for ',extents[j],' 
',outcomes[k],', ',ages[i],sep=''),
             caption='Value per grid cell. Lower whisker=minimum; Upper whisker=1.5*IQR; Lower box= 25th percentile; 
             Upper box=75th percentile; Middle=median; Population dataset= LandScan USA night time population (2017);
             Concentration dataset = Larkin et al. 2017.')
      ggsave(paste("C:/Users/Veronica Tinney/Google Drive/EDF_shared/Results/boxplots/",ages[i],extents[j],outcomes[k],".png",sep=""), dpi=300, width=6, height=5)
    }}}

ages <- c('0-17 years')
outcomes <- c("Asthma incidence")
extents <- c('Oakland')

for (j in 1:length(extents)){
  print(extents[j])
  for (k in 1:length(outcomes)){
    print(outcomes[k])
    for (i in 1:length(ages)){
      print(ages[i])
      r <- subset(no2.plot, Outcome == outcomes[k])
      m <- subset(r, Extent == extents[j])
      z <- subset(m, Age == ages[i])
      p <- ggplot(z, x=Beta)+
        geom_boxplot(aes(x=Beta, ymin = Minimum, lower = q25, 
                         middle = Median, 
                         upper = q75, ymax = q75*1.5,
                         fill=Beta),
                     stat='identity')+
        ylab('Excess per grid cell')+
        xlab('Study Beta')+
        scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
        theme_fivethirtyeight()+
        facet_grid(cols=vars(Concentration))+
        theme(axis.text.x = element_text(size=9),
              axis.text.y = element_text(size=9),
              plot.title=element_text(size=10),
              plot.caption =element_text(size=8),
              axis.title=element_blank(),
              rect = element_blank())+
        labs(title=paste0('Nitrogen dioxide results comparison for ',extents[j],' 
',outcomes[k],', ',ages[i],sep=''),
             caption='Value per grid cell. Lower whisker=minimum; Upper whisker=1.5*IQR; Lower box= 25th percentile; 
             Upper box=75th percentile; Middle=median; Population dataset= LandScan USA night time population (2017).')
      ggsave(paste("C:/Users/Veronica Tinney/Google Drive/EDF_shared/Results/boxplots/",ages[i],extents[j],outcomes[k],".png",sep=""), dpi=300, width=6, height=5)
    }}}

ages <- c('0-17 years')
outcomes <- c("Asthma incidence")
extents <- c('Bay Area','Alameda County')

for (j in 1:length(extents)){
  print(extents[j])
  for (k in 1:length(outcomes)){
    print(outcomes[k])
    for (i in 1:length(ages)){
      print(ages[i])
      r <- subset(no2.plot, Outcome == outcomes[k])
      m <- subset(r, Extent == extents[j])
      z <- subset(m, Age == ages[i])
      p <- ggplot(z, x=Beta)+
        geom_boxplot(aes(x=Beta, ymin = Minimum, lower = q25, 
                         middle = Median, 
                         upper = q75, ymax = q75*1.5,
                         fill=Beta),
                     stat='identity')+
        ylab('Excess per grid cell')+
        xlab('Study Beta')+
        scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
        theme_fivethirtyeight()+
        #facet_grid(cols=vars(Concentration))+
        theme(axis.text.x = element_text(size=9),
              axis.text.y = element_text(size=9),
              plot.title=element_text(size=10),
              plot.caption =element_text(size=8),
              axis.title=element_blank(),
              rect = element_blank())+
        labs(title=paste0('Nitrogen dioxide results comparison for ',extents[j],' 
',outcomes[k],', ',ages[i],sep=''),
             caption='Value per grid cell. Lower whisker=minimum; Upper whisker=1.5*IQR; Lower box= 25th percentile; 
             Upper box=75th percentile; Middle=median; Population dataset= LandScan USA night time population (2017);
             Concentration dataset = Larkin et al. 2017')
      ggsave(paste("C:/Users/Veronica Tinney/Google Drive/EDF_shared/Results/boxplots/",ages[i],extents[j],outcomes[k],".png",sep=""), dpi=300, width=6, height=5)
    }}}
#==============================================================================================================
#==============================================================================================================
#PM2.5

pm.plot <- subset(r.ex.pm, Estimates == 'Point')
names(pm.plot)[8] <- 'Age'

ages <- c('0-17 years')
outcomes <- c("Asthma ER visits")
extents <- c('Bay Area','Alameda County','Oakland')

for (j in 1:length(extents)){
  print(extents[j])
  for (k in 1:length(outcomes)){
    print(outcomes[k])
    for (i in 1:length(ages)){
      print(ages[i])
      r <- subset(pm.plot, Outcome == outcomes[k])
      m <- subset(r, Extent == extents[j])
      z <- subset(m, Age == ages[i])
      p <- ggplot(z, x=Beta)+
        geom_boxplot(aes(x=Beta, ymin = Minimum, lower = q25, 
                         middle = Median, 
                         upper = q75, ymax = q75*1.5,
                         fill=Beta),
                     stat='identity')+
        ylab('Excess per grid cell')+
        xlab('Study Beta')+
        scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
        theme_fivethirtyeight()+
        #facet_grid(cols=vars(Concentration))+
        theme(axis.text.x = element_text(size=9),
              axis.text.y = element_text(size=9),
              plot.title=element_text(size=10),
              plot.caption =element_text(size=8),
              axis.title=element_blank(),
              rect = element_blank())+
        labs(title=paste0('Fine particulate matter results comparison for ',extents[j],' 
',outcomes[k],', ',ages[i],sep=''),
             caption='Value per grid cell. Lower whisker=minimum; Upper whisker=1.5*IQR; Lower box= 25th percentile; 
             Upper box=75th percentile; Middle=median; Population dataset= LandScan USA night time population (2017);
             Concentration dataset = Harvard dataset, mean concentrations 2015-2016.')
      ggsave(paste("C:/Users/Veronica Tinney/Google Drive/EDF_shared/Results/boxplots/",extents[j],outcomes[k],ages[i],".pm.png",sep=""), dpi=300, width=6, height=5)
    }}}

ages <- c('25-99 years','65-99 years')
outcomes <- c("CVD mortality")
extents <- c('Bay Area')

for (j in 1:length(extents)){
  print(extents[j])
  for (k in 1:length(outcomes)){
    print(outcomes[k])
    for (i in 1:length(ages)){
      print(ages[i])
      r <- subset(pm.plot, Outcome == outcomes[k])
      m <- subset(r, Extent == extents[j])
      z <- subset(m, Age == ages[i])
      p <- ggplot(z, x=Beta, group=Rate)+
        geom_boxplot(aes(x=Beta, ymin = Minimum, lower = q25, 
                         middle = Median, 
                         upper = q75, ymax = q75*1.5,
                         fill=Beta),
                     stat='identity')+
        ylab('Excess per grid cell')+
        xlab('Study Beta')+
        scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
        theme_fivethirtyeight()+
        #facet_grid(cols=vars(Concentration))+
        theme(axis.text.x = element_text(size=7),
              axis.text.y = element_text(size=9),
              plot.title=element_text(size=10),
              plot.caption =element_text(size=8),
              axis.title=element_blank(),
              rect = element_blank())+
        labs(title=paste0('Fine particulate matter results comparison for ',extents[j],' 
',outcomes[k],', ',ages[i],sep=''),
             caption='Value per grid cell. Lower whisker=minimum; Upper whisker=1.5*IQR; Lower box= 25th percentile; 
             Upper box=75th percentile; Middle=median; Population dataset= LandScan USA night time population (2017);
             Concentration dataset = Harvard dataset, mean concentrations 2015-2016.')
      ggsave(paste("C:/Users/Veronica Tinney/Google Drive/EDF_shared/Results/boxplots/",extents[j],outcomes[k],ages[i],".pm.png",sep=""), dpi=300, width=6, height=5)
    }}}

ages <- c('25-99 years','65-99 years')
outcomes <- c("CVD mortality")
extents <- c('Alameda County','Oakland')

for (j in 1:length(extents)){
  print(extents[j])
  for (k in 1:length(outcomes)){
    print(outcomes[k])
    for (i in 1:length(ages)){
      print(ages[i])
      r <- subset(pm.plot, Outcome == outcomes[k])
      m <- subset(r, Extent == extents[j])
      z <- subset(m, Age == ages[i])
      p <- ggplot(z, x=Beta, group=Rate)+
        geom_boxplot(aes(x=Beta, ymin = Minimum, lower = q25, 
                         middle = Median, 
                         upper = q75, ymax = q75*1.5,
                         fill=Rate),
                     stat='identity')+
        ylab('Excess per grid cell')+
        xlab('Study Beta')+
        scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
        theme_fivethirtyeight()+
        #facet_grid(cols=vars(Concentration))+
        theme(axis.text.x = element_text(size=7),
              axis.text.y = element_text(size=9),
              plot.title=element_text(size=10),
              plot.caption =element_text(size=8),
              axis.title=element_blank(),
              rect = element_blank())+
        labs(title=paste0('Fine particulate matter results comparison for ',extents[j],' 
',outcomes[k],', ',ages[i],sep=''),
             caption='Value per grid cell. Lower whisker=minimum; Upper whisker=1.5*IQR; Lower box= 25th percentile; 
             Upper box=75th percentile; Middle=median; Population dataset= LandScan USA night time population (2017);
             Concentration dataset = Harvard dataset, mean concentrations 2015-2016.')
      ggsave(paste("C:/Users/Veronica Tinney/Google Drive/EDF_shared/Results/boxplots/",extents[j],outcomes[k],ages[i],".pm.png",sep=""), dpi=300, width=6, height=5)
    }}}

ages <- c('25-99 years','65-99 years')
outcomes <- c("All-cause mortality")
extents <- c('Bay Area')

for (j in 1:length(extents)){
  print(extents[j])
  for (k in 1:length(outcomes)){
    print(outcomes[k])
    for (i in 1:length(ages)){
      print(ages[i])
      r <- subset(pm.plot, Outcome == outcomes[k])
      m <- subset(r, Extent == extents[j])
      z <- subset(m, Age == ages[i])
      p <- ggplot(z, x=Beta, group=Rate)+
        geom_boxplot(aes(x=Beta, ymin = Minimum, lower = q25, 
                         middle = Median, 
                         upper = q75, ymax = q75*1.5,
                         fill=Beta),
                     stat='identity')+
        ylab('Excess per grid cell')+
        xlab('Study Beta')+
        #scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
        theme_fivethirtyeight()+
        #facet_grid(cols=vars(Concentration))+
        theme(axis.text.x = element_text(size=7),
              axis.text.y = element_text(size=9),
              plot.title=element_text(size=10),
              plot.caption =element_text(size=8),
              axis.title=element_blank(),
              rect = element_blank())+
        labs(title=paste0('Fine particulate matter results comparison for ',extents[j],' 
',outcomes[k],', ',ages[i],sep=''),
             caption='Value per grid cell. Lower whisker=minimum; Upper whisker=1.5*IQR; Lower box= 25th percentile; 
             Upper box=75th percentile; Middle=median; Population dataset= LandScan USA night time population (2017);
             Concentration dataset = Harvard dataset, mean concentrations 2015-2016.')
      ggsave(paste("C:/Users/Veronica Tinney/Google Drive/EDF_shared/Results/boxplots/",extents[j],outcomes[k],ages[i],".pm.png",sep=""), dpi=300, width=6, height=5)
    }}}

ages <- c('25-99 years','65-99 years')
outcomes <- c("All-cause mortality")
extents <- c('Alameda County','Oakland')

for (j in 1:length(extents)){
  print(extents[j])
  for (k in 1:length(outcomes)){
    print(outcomes[k])
    for (i in 1:length(ages)){
      print(ages[i])
      r <- subset(pm.plot, Outcome == outcomes[k])
      m <- subset(r, Extent == extents[j])
      z <- subset(m, Age == ages[i])
      p <- ggplot(z, x=Beta, group=Rate)+
        geom_boxplot(aes(x=Beta, ymin = Minimum, lower = q25, 
                         middle = Median, 
                         upper = q75, ymax = q75*1.5,
                         fill=Rate),
                     stat='identity')+
        ylab('Excess per grid cell')+
        xlab('Study Beta')+
        scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
        theme_fivethirtyeight()+
        #facet_grid(cols=vars(Concentration))+
        theme(axis.text.x = element_text(size=7),
              axis.text.y = element_text(size=9),
              plot.title=element_text(size=10),
              plot.caption =element_text(size=8),
              axis.title=element_blank(),
              rect = element_blank())+
        labs(title=paste0('Fine particulate matter results comparison for ',extents[j],' 
',outcomes[k],', ',ages[i],sep=''),
             caption='Value per grid cell. Lower whisker=minimum; Upper whisker=1.5*IQR; Lower box= 25th percentile; 
             Upper box=75th percentile; Middle=median; Population dataset= LandScan USA night time population (2017);
             Concentration dataset = Harvard dataset, mean concentrations 2015-2016.')
      ggsave(paste("C:/Users/Veronica Tinney/Google Drive/EDF_shared/Results/boxplots/",extents[j],outcomes[k],ages[i],".pm.png",sep=""), dpi=300, width=6, height=5)
    }}}

ages <- c('0-17 years')
outcomes <- c("Asthma incidence")
extents <- c('Bay Area','Alameda County','Oakland')

for (j in 1:length(extents)){
  print(extents[j])
  for (k in 1:length(outcomes)){
    print(outcomes[k])
    for (i in 1:length(ages)){
      print(ages[i])
      r <- subset(pm.plot, Outcome == outcomes[k])
      m <- subset(r, Extent == extents[j])
      z <- subset(m, Age == ages[i])
      p <- ggplot(z, x=Beta)+
        geom_boxplot(aes(x=Beta, ymin = Minimum, lower = q25, 
                         middle = Median, 
                         upper = q75, ymax = q75*1.5,
                         fill=Beta),
                     stat='identity')+
        ylab('Excess per grid cell')+
        xlab('Study Beta')+
        scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
        theme_fivethirtyeight()+
        #facet_grid(cols=vars(Concentration))+
        theme(axis.text.x = element_text(size=9),
              axis.text.y = element_text(size=9),
              plot.title=element_text(size=10),
              plot.caption =element_text(size=8),
              axis.title=element_blank(),
              rect = element_blank())+
        labs(title=paste0('Fine particulate matter results comparison for ',extents[j],' 
',outcomes[k],', ',ages[i],sep=''),
             caption='Value per grid cell. Lower whisker=minimum; Upper whisker=1.5*IQR; Lower box= 25th percentile; 
             Upper box=75th percentile; Middle=median; Population dataset= LandScan USA night time population (2017);
             Concentration dataset = Harvard dataset, mean concentrations 2015-2016.')
      ggsave(paste("C:/Users/Veronica Tinney/Google Drive/EDF_shared/Results/boxplots/",extents[j],outcomes[k],ages[i],".pm.png",sep=""), dpi=300, width=6, height=5)
    }}}

ages <- c('65-99 years')
outcomes <- c("CVD hospitalizations")
extents <- c('Bay Area','Alameda County','Oakland')

for (j in 1:length(extents)){
  print(extents[j])
  for (k in 1:length(outcomes)){
    print(outcomes[k])
    for (i in 1:length(ages)){
      print(ages[i])
      r <- subset(pm.plot, Outcome == outcomes[k])
      m <- subset(r, Extent == extents[j])
      z <- subset(m, Age == ages[i])
      p <- ggplot(z, x=Beta)+
        geom_boxplot(aes(x=Beta, ymin = Minimum, lower = q25, 
                         middle = Median, 
                         upper = q75, ymax = q75*1.5,
                         fill=Beta),
                     stat='identity')+
        ylab('Excess per grid cell')+
        xlab('Study Beta')+
        scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
        theme_fivethirtyeight()+
        #facet_grid(cols=vars(Concentration))+
        theme(axis.text.x = element_text(size=9),
              axis.text.y = element_text(size=9),
              plot.title=element_text(size=10),
              plot.caption =element_text(size=8),
              axis.title=element_blank(),
              rect = element_blank())+
        labs(title=paste0('Fine particulate matter results comparison for ',extents[j],' 
',outcomes[k],', ',ages[i],sep=''),
             caption='Value per grid cell. Lower whisker=minimum; Upper whisker=1.5*IQR; Lower box= 25th percentile; 
             Upper box=75th percentile; Middle=median; Population dataset= LandScan USA night time population (2017);
             Concentration dataset = Harvard dataset, mean concentrations 2015-2016.')
      ggsave(paste("C:/Users/Veronica Tinney/Google Drive/EDF_shared/Results/boxplots/",extents[j],outcomes[k],ages[i],".pm.png",sep=""), dpi=300, width=6, height=5)
    }}}

#==============================================================================================

bc.plot <- subset(r.ex.bc, Estimates == 'Point')
names(bc.plot)[8] <- 'Age'

ages <- c('65-99 years')
outcomes <- c("CVD hospitalizations")
extents <- c('Oakland')

for (j in 1:length(extents)){
  print(extents[j])
  for (k in 1:length(outcomes)){
    print(outcomes[k])
    for (i in 1:length(ages)){
      print(ages[i])
      r <- subset(bc.plot, Outcome == outcomes[k])
      m <- subset(r, Extent == extents[j])
      z <- subset(m, Age == ages[i])
      p <- ggplot(z, x=Beta)+
        geom_boxplot(aes(x=Beta, ymin = Minimum, lower = q25, 
                         middle = Median, 
                         upper = q75, ymax = q75*1.5,
                         fill=Beta),
                     stat='identity')+
        ylab('Excess per grid cell')+
        xlab('Study Beta')+
        scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
        theme_fivethirtyeight()+
        #facet_grid(cols=vars(Concentration))+
        theme(axis.text.x = element_text(size=9),
              axis.text.y = element_text(size=9),
              plot.title=element_text(size=10),
              plot.caption =element_text(size=8),
              axis.title=element_blank(),
              rect = element_blank())+
        labs(title=paste0('Black Carbon results comparison for ',extents[j],' 
',outcomes[k],', ',ages[i],sep=''),
             caption='Value per grid cell. Lower whisker=minimum; Upper whisker=1.5*IQR; Lower box= 25th percentile; 
             Upper box=75th percentile; Middle=median; Population dataset= LandScan USA night time population (2017);
             Concentration dataset = Harvard dataset, mean concentrations 2015-2016.')
      ggsave(paste("C:/Users/Veronica Tinney/Google Drive/EDF_shared/Results/boxplots/",extents[j],outcomes[k],ages[i],".bc.png",sep=""), dpi=300, width=6, height=5)
    }}}

ages <- c('25-99 years')
outcomes <- c("All-cause mortality")
extents <- c('Oakland')

for (j in 1:length(extents)){
  print(extents[j])
  for (k in 1:length(outcomes)){
    print(outcomes[k])
    for (i in 1:length(ages)){
      print(ages[i])
      r <- subset(bc.plot, Outcome == outcomes[k])
      m <- subset(r, Extent == extents[j])
      z <- subset(m, Age == ages[i])
      p <- ggplot(z, x=Beta, group=Rate)+
        geom_boxplot(aes(x=Beta, ymin = Minimum, lower = q25, 
                         middle = Median, 
                         upper = q75, ymax = q75*1.5,
                         fill=Rate),
                     stat='identity')+
        ylab('Excess per grid cell')+
        xlab('Study Beta')+
        scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
        theme_fivethirtyeight()+
        #facet_grid(cols=vars(Concentration))+
        theme(axis.text.x = element_text(size=7),
              axis.text.y = element_text(size=9),
              plot.title=element_text(size=10),
              plot.caption =element_text(size=8),
              axis.title=element_blank(),
              rect = element_blank())+
        labs(title=paste0('Black carbon results comparison for ',extents[j],' 
',outcomes[k],', ',ages[i],sep=''),
             caption='Value per grid cell. Lower whisker=minimum; Upper whisker=1.5*IQR; Lower box= 25th percentile; 
             Upper box=75th percentile; Middle=median; Population dataset= LandScan USA night time population (2017);
             Concentration dataset = GSV.')
      ggsave(paste("C:/Users/Veronica Tinney/Google Drive/EDF_shared/Results/boxplots/",extents[j],outcomes[k],ages[i],".bc.png",sep=""), dpi=300, width=6, height=5)
    }}}

ages <- c('25-99 years')
outcomes <- c("CVD mortality")
extents <- c('Oakland')

for (j in 1:length(extents)){
  print(extents[j])
  for (k in 1:length(outcomes)){
    print(outcomes[k])
    for (i in 1:length(ages)){
      print(ages[i])
      r <- subset(bc.plot, Outcome == outcomes[k])
      m <- subset(r, Extent == extents[j])
      z <- subset(m, Age == ages[i])
      p <- ggplot(z, x=Beta, group=Rate)+
        geom_boxplot(aes(x=Beta, ymin = Minimum, lower = q25, 
                         middle = Median, 
                         upper = q75, ymax = q75*1.5,
                         fill=Rate),
                     stat='identity')+
        ylab('Excess per grid cell')+
        xlab('Study Beta')+
        scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
        theme_fivethirtyeight()+
        #facet_grid(cols=vars(Concentration))+
        theme(axis.text.x = element_text(size=7),
              axis.text.y = element_text(size=9),
              plot.title=element_text(size=10),
              plot.caption =element_text(size=8),
              axis.title=element_blank(),
              rect = element_blank())+
        labs(title=paste0('Black carbon results comparison for ',extents[j],' 
',outcomes[k],', ',ages[i],sep=''),
             caption='Value per grid cell. Lower whisker=minimum; Upper whisker=1.5*IQR; Lower box= 25th percentile; 
             Upper box=75th percentile; Middle=median; Population dataset= LandScan USA night time population (2017);
             Concentration dataset = GSV.')
      ggsave(paste("C:/Users/Veronica Tinney/Google Drive/EDF_shared/Results/boxplots/",extents[j],outcomes[k],ages[i],".bc.png",sep=""), dpi=300, width=6, height=5)
    }}}