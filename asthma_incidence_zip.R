library(dplyr)
library(xlsx)
library(readxl)

setwd("C:/Users/vtinney/Dropbox/EDF_2019/inc rate")

#Import datasets
#ACS 5 year population estimates for 2012 and 2014
#This will be used as the denominator
Census_zip_code_age_numbers_2010 <- read_excel("Census zip code age numbers 2010.xlsx")

#Prevalence estimates for 2012 and 2014 from CHIS Ask Neighborhood CHIS
asthmac_14 <- read_excel("asthmac_14.xlsx")
asthma_inc_county <- read_excel("asthma.inc.county.xlsx")
zip_county <- read_excel("zip.county.xlsx")
#############################################################################################
#Frequently used codes
#Sample split dataset by 2012 and 2014
#y12 <- mort_12_14[mort_12_14$Year == 2012, ]
#Sample rename
#names(mort_12_14)[3]<-"cod"
#Filter
#adult2 <- filter(adult, zip > 1)

#Sample Sum all deaths by zip code for all-cause mortality rates
#y12_all <- y12 %>% group_by(zip) %>%
#  summarize(Sum_Mort = sum(Count))

#Sample merge the dataframes
#test12 <- merge(x=pop12, y=y12_all, by="zip", all=TRUE)
# convert chr to num
#all.d$p.c.12 <- as.numeric(as.character(all.d$p.c.12))

####################################################################################
# Change character to numeric
asthmac_14$population <- as.numeric(as.character(asthmac_14$population))
asthmac_14$estimate <- as.numeric(as.character(asthmac_14$estimate))
asthmac_14$count <- as.numeric(as.character(asthmac_14$count))
asthmac_14$SE <- as.numeric(as.character(asthmac_14$SE))
asthmac_14$lower <- as.numeric(as.character(asthmac_14$lower))
asthmac_14$upper <- as.numeric(as.character(asthmac_14$upper))

#rename
popfrac <- Census_zip_code_age_numbers_2010
names(popfrac)[1]<-"zip"

#merge
inc <- merge(x=asthmac_14, y=popfrac, by="zip", all=TRUE)
asthma <- filter(inc, zip > 1)

#Create percent each age year among general population to apply to asthmatics
asthma$per.0 <- asthma$zero/asthma$Total
asthma$per.1 <- asthma$one/asthma$Total
asthma$per.2 <- asthma$two/asthma$Total
asthma$per.3 <- asthma$three/asthma$Total
asthma$per.4 <- asthma$four/asthma$Total
asthma$per.5 <- asthma$five/asthma$Total
asthma$per.6 <- asthma$six/asthma$Total
asthma$per.7 <- asthma$seven/asthma$Total
asthma$per.8 <- asthma$eight/asthma$Total
asthma$per.9 <- asthma$nine/asthma$Total
asthma$per.10 <- asthma$ten/asthma$Total
asthma$per.11 <- asthma$eleven/asthma$Total
asthma$per.12 <- asthma$twelve/asthma$Total
asthma$per.13 <- asthma$thirt/asthma$Total
asthma$per.14 <- asthma$fourt/asthma$Total
asthma$per.15 <- asthma$fifteen/asthma$Total
asthma$per.16 <- asthma$sixteen/asthma$Total
asthma$per.17 <- asthma$seventeen/asthma$Total

#create # of asthmatics per year with percent age year from above
asthma$asthma.0 <- asthma$count*asthma$per.0
asthma$asthma.1 <- asthma$count*asthma$per.1
asthma$asthma.2 <- asthma$count*asthma$per.2
asthma$asthma.3 <- asthma$count*asthma$per.3
asthma$asthma.4 <- asthma$count*asthma$per.4
asthma$asthma.5 <- asthma$count*asthma$per.5
asthma$asthma.6 <- asthma$count*asthma$per.6
asthma$asthma.7 <- asthma$count*asthma$per.7
asthma$asthma.8 <- asthma$count*asthma$per.8
asthma$asthma.9 <- asthma$count*asthma$per.9
asthma$asthma.10 <- asthma$count*asthma$per.10
asthma$asthma.11 <- asthma$count*asthma$per.11
asthma$asthma.12 <- asthma$count*asthma$per.12
asthma$asthma.13 <- asthma$count*asthma$per.13
asthma$asthma.14 <- asthma$count*asthma$per.14
asthma$asthma.15 <- asthma$count*asthma$per.15
asthma$asthma.16 <- asthma$count*asthma$per.16
asthma$asthma.17 <- asthma$count*asthma$per.17

#average duration per locality
asthma$zip.ped.asthma <- asthma$asthma.0 + asthma$asthma.1 + asthma$asthma.2 + asthma$asthma.3 +
  asthma$asthma.4 + asthma$asthma.5 + asthma$asthma.6 + asthma$asthma.7 + asthma$asthma.8 +
  asthma$asthma.9 + asthma$asthma.10 + asthma$asthma.11 + asthma$asthma.12 + asthma$asthma.13 +
  asthma$asthma.14 + asthma$asthma.15 + asthma$asthma.16 + asthma$asthma.17

#Sum of total years of asthma
asthma$sum.years <- ((asthma$asthma.1*1) + (asthma$asthma.2*2) + (asthma$asthma.3*3) +
                       +   (asthma$asthma.4*4) + (asthma$asthma.5*5) + (asthma$asthma.6*6) + (asthma$asthma.7*7) +
                       +   (asthma$asthma.8*8) + (asthma$asthma.9*9) + (asthma$asthma.10*10) + (asthma$asthma.11*11)
                       +  (asthma$asthma.12*12) + (asthma$asthma.13*13) + (asthma$asthma.14*14) + (asthma$asthma.15*15) +
                       +   (asthma$asthma.16*16) + (asthma$asthma.17*17))

#Sum of duration years - age less the average duration of 7 years (Mirabelli et a. 2013)
asthma$sum.dur <- (asthma$asthma.8*1) +
 (asthma$asthma.9*2) +
 (asthma$asthma.10*3) +
 (asthma$asthma.11*4) +
 (asthma$asthma.12*5) +
 (asthma$asthma.13*6) +
 (asthma$asthma.14*7) +
 (asthma$asthma.15*8) +
 (asthma$asthma.16*9) +
 (asthma$asthma.17*10)

#Average duration = sum of the total asthma years / average disease duration
asthma$avg.dur <- asthma$sum.years/asthma$sum.dur

#create odds of point prevalence with (point prevalence / 1-prevalence)

asthma$pointprev <- asthma$estimate/(1-asthma$estimate)

#Put back into P=I*D
asthma$inc.rate <- asthma$pointprev/asthma$avg.dur
asthma$inc.rate.10k <- asthma$inc.rate*10000

write.csv(asthma, file='asthma.inc.csv')


