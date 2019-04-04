library(dplyr)
library(xlsx)
library(readxl)

setwd("C:/Users/vtinney/GoogleDrive/EDF_shared/Rates/Excel/Mortality/")

#############################################################################################
#Import datasets
CA_B_1 <- read_excel("CA_B_1.XLSX")
CA_B_2 <- read_excel("CA_B_2.XLSX")

a <- CA_B_2
b <- CA_B_1

all <- rbind(a, b)

#############################################################################################
#Part 1 - all mortality

#Change names
names(all)[4]<-"ct"
names(all)[5]<-"age"
names(all)[7]<-"survive"
names(all)[8]<-"die"

#Subset dataset for only bay area counties

bay <- subset(all, CNTY2KX=='001' | CNTY2KX=='013' | CNTY2KX=='041' | CNTY2KX=='055'
                   | CNTY2KX=='075' | CNTY2KX=='081' | CNTY2KX=='085' | CNTY2KX=='095'
                   | CNTY2KX=='097')

all <- bay

#sum population survive and population die for total population
all$tot_pop <- all$survive + all$die

all.merge <- all %>% group_by(ct) %>%
  summarize(Sum_pop_ct = sum(tot_pop))

all.die <- ages25.64 %>% group_by(ct) %>%
  summarize(Sum_die_ct = sum(die))

all.calc <- merge(x=all, y=all.merge, by="ct", all=TRUE)
all.final <- merge(x=all.calc, y=all.die, by="ct", all=TRUE)

all.final$weight_rate <- all.final$Sum_die_ct / all.final$Sum_pop_ct


#In Arcmap I then merged this with the Census tract shapefile for the bay area

####################################################################################
#Part 2 - age group mortality

#subset age groups
ages25.64 <- subset(all, age=="25-34" | age=="35-44" | age=="45-54" | age=="55-64")
ages65.99 <- subset(all, age=="65-74" | age=="75-84" | age=="85\nand\nolder")

#rename variable census tract "ct"
names(ages25.64)[4]<-"ct"
names(ages65.99)[4]<-"ct"

#sum ages by census tract
ages25.64.merge <- ages25.64 %>% group_by(ct) %>%
  summarize(Sum_pop_ct = sum(tot_pop))
ages65.99.merge <- ages65.99 %>% group_by(ct) %>%
  summarize(Sum_pop_ct = sum(tot_pop))

#sum ages by census tract
ages25.64.die <- ages25.64 %>% group_by(ct) %>%
  summarize(Sum_die_ct = sum(die))
ages65.99.die <- ages65.99 %>% group_by(ct) %>%
  summarize(Sum_die_ct = sum(die))

#merge them back together so there is a column with ct total pop
ages25.64.calc <- merge(x=ages25.64, y=ages25.64.merge, by="ct", all=TRUE)
ages65.99.calc <- merge(x=ages65.99, y=ages65.99.merge, by="ct", all=TRUE)

ages25.64.final <- merge(x=ages25.64.calc, y=ages25.64.die, by="ct", all=TRUE)
ages65.99.final <- merge(x=ages65.99.calc, y=ages65.99.die, by="ct", all=TRUE)

ages25.64.final$age_rate <- ages25.64.final$Sum_die_ct / ages25.64.final$Sum_pop_ct
ages65.99.final$age_rate <- ages65.99.final$Sum_die_ct / ages65.99.final$Sum_pop_ct

####################################################################################
#Write out CSV results
write.csv(all.final, file = "all_mort.csv")
write.csv(ages25.64.final, file = 'ages_25_64.csv')
write.csv(ages65.99.final, file = 'ages_65_99.csv')

#In Arcmap I then merged this with the Census tract shapefile for the bay area
