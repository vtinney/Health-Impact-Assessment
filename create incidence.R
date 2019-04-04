library(dplyr)
install.packages('xlsx')
library(xlsx)

setwd("C:/Users/vtinney/Veronica Southerland Dropbox/Veronica Tinney/EDF_2019/scape")

#Import datasets
#Mortality for 2012 and 2014 from CHIS https://data.chhs.ca.gov/dataset/leading-causes-of-death-by-zip-code
mort_12_14 <- read_excel("mort_12_14.xlsx")

#ACS 5 year population estimates for 2012 and 2014
#This will be used as the denominator
ACS_12_5YR_B01003_with_ann <- read_excel("ACS_12_5YR_B01003_with_ann.xlsx")
ACS_14_5YR_B01003_with_ann <- read_excel("ACS_14_5YR_B01003_with_ann.xlsx")
pop12 <- ACS_12_5YR_B01003_with_ann
pop14 <- ACS_14_5YR_B01003_with_ann

#Prevalence estimates for 2012 and 2014 from CHIS Ask Neighborhood CHIS
asthmaa_12 <- read_excel("asthmaa_12.xlsx")
asthmaa_14 <- read_excel("asthmaa_14.xlsx")
asthmac_12 <- read_excel("asthmac_12.xlsx")
asthmac_14 <- read_excel("asthmac_14.xlsx")

#############################################################################################
#Part 1 - create mortality rates for all-cause mortality and CLD specific mortality

#Change name no spaces / match
names(mort_12_14)[3]<-"cod"
names(mort_12_14)[3]<-"cod"
names(y12)[2]<-"zip"
names(y14)[2]<-"zip"
names(pop12)[2]<-"zip"
names(pop14)[2]<-"zip"
names(pop12)[4]<-"count_pop"
names(pop14)[4]<-"count_pop"
names(y12_cld)[1]<-"year"
names(y14_cld)[1]<-"year"
names(y12_cld)[2]<-"zip"
names(y14_cld)[2]<-"zip"
names(y12_cld)[4]<-"count_cld"
names(y14_cld)[4]<-"count_cld"

#Split dataset by 2012 and 2014
y12 <- mort_12_14[mort_12_14$Year == 2012, ]
y14 <- mort_12_14[mort_12_14$Year == 2014, ]

#Split dataset by CLD cause of death
y12_cld <- mort_12_14[mort_12_14$cod == "CLD", ]
y14_cld <- mort_12_14[mort_12_14$cod == "CLD", ]

#Sum all deaths by zip code for all-cause mortality rates
y12_all <- y12 %>% group_by(zip) %>%
  summarize(Sum_Mort = sum(Count))

y14_all <- y14 %>% group_by(zip) %>%
  summarize(Sum_Mort = sum(Count))

#Merge the dataframes
#2012
test12 <- merge(x=pop12, y=y12_all, by="zip", all=TRUE)
y12_total <- merge(x=test12, y=y12_cld, by="zip", all=TRUE)
#2014
test14 <- merge(x=pop14, y=y14_all, by="zip", all=TRUE)
y14_total <- merge(x=test14, y=y14_cld, by="zip", all=TRUE)

#Create CLD rate
y12_total$cld.rate <- y12_total$count_cld / y12_total$count_pop
y14_total$cld.rate <- y14_total$count_cld / y14_total$count_pop

#Create overall mortality rate
y12_total$all.rate <- y12_total$Sum_Mort / y12_total$count_pop
y14_total$all.rate <- y14_total$Sum_Mort / y14_total$count_pop

#Rename for merging
names(y12_total) <- c('zip', 'ID', 'geography', 'count_pop_12', 'me_12', 'Sum_Mort_12',
                      'year1', 'cod_12', 'count_cld_12', 'cld.rate.12', 'all.rate.12', 'rate.neg.12')

names(y14_total) <- c('zip', 'ID', 'geography', 'count_pop_14', 'me_14', 'Sum_Mort_14',
                      'year1', 'cod_14', 'count_cld_14', 'cld.rate.14', 'all.rate.14', 'rate.neg.14')

#merge datasets
merge <- merge(x=y12_total, y=y14_total, by="zip", all=TRUE)

#rename for merging 
names(asthmaa_12)[2]<-"zip"
names(asthmaa_14)[2]<-"zip"
names(asthmac_12)[2]<-"zip"
names(asthmac_14)[2]<-"zip"
names(asthmaa_12)[6]<-"p.a.12"
names(asthmaa_14)[6]<-"p.a.14"
names(asthmac_12)[6]<-"p.c.12"
names(asthmac_14)[6]<-"p.c.14"

adult <- merge(x=asthmaa_12, y=asthmaa_14, by="zip", all=TRUE)
adult2 <- filter(adult, zip > 1)
ped <- merge(x=asthmac_12, y=asthmac_14, by="zip", all=TRUE)
ped2 <- filter(ped, zip > 1)
asthma <- merge(x=adult2, y=ped2, by="zip", all=TRUE)

all.d <- merge(x=merge, y=asthma, by="zip", all=TRUE)
####################################################################################
#Part 2 -  rate inputs for calculation

#Calc 1 - Create mortality negative at 2014 (SN)
all.d$sn <- 1-(all.d$all.rate.14 - all.d$cld.rate.14)

#Calc 2- Create survival proporation at 2014 (SP)
all.d$sp <- 1-all.d$cld.rate.14

# convert chr to num
all.d$p.c.12 <- as.numeric(as.character(all.d$p.c.12))
all.d$p.a.12 <- as.numeric(as.character(all.d$p.a.12))
all.d$p.c.14 <- as.numeric(as.character(all.d$p.c.14))
all.d$p.a.14 <- as.numeric(as.character(all.d$p.a.14))

#prevalence pediatric (c) and adult (a) at 2012
#all.d$prev.c.12
#all.d$prev.a.12

#prevalence pediatric (c) and adult (a) at 2014
#all.d$prev.c.14
#all.d$prev.a.14

#Calc 3 - Calculate F = (1-((1-SP)*p0))-((1-SN)*(1-p0))
#F pediatric
all.d$fc <- (1-((1-all.d$sp)*(all.d$p.c.12))-((1-all.d$sp)*(1-all.d$p.c.12)))
#F adult
all.d$fa <- (1-((1-all.d$sp)*(all.d$p.a.12))-((1-all.d$sp)*(1-all.d$p.a.12)))

#Calc 3 - Calculate incidence
# (2((F*pt)-(SP*p0)) / T((1-p0)+(F(1-pT)))
#pediatric
all.d$c.inc <- (2*(all.d$fc*all.d$p.c.14)-(all.d$sp*all.d$p.c.12))/(2*(1-all.d$p.c.12)+(all.d$fc*(1-all.d$p.c.14)))

#adult
all.d$a.inc <- (2*(all.d$fa*all.d$p.a.14)-(all.d$sp*all.d$p.a.12))/(2*(1-all.d$p.a.12)+(all.d$fa*(1-all.d$p.a.14)))



write.csv(all.d, file = "incidence.csv")
