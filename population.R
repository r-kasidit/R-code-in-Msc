rm(list=ls())

library(tidyverse)
library(plyr)
library(scales)
library(reshape2)

#import data set, namely Sheet_1_TAble_1.csv 
total.df = read_csv("Sheet 1-Table 1.csv")

order.month = c("January 2015","February 2015","March 2015","April 2015", "May 2015"
               ,"June 2015","July 2015", "August 2015", "September 2015"
               ,"October 2015", "November 2015", "December 2015","January 2016"
               ,"February 2016", "March 2016", "April 2016")

total.df$location = factor(total.df$location, levels = c("Kham", "Changkleua"))



total.df$date = paste(total.df$day,total.df$month,total.df$year)
total.df$date = as.Date(total.df$date,format="%d %B %Y")
subset.colsiz = total.df[,c("location","colony.size","date")]

#import capture data, then trannform into date data

Sheet_1_str_kham = read_csv("Sheet 1-str.kham.csv")
Sheet_1_str_kham$date1 = paste(Sheet_1_str_kham$date,Sheet_1_str_kham$month,Sheet_1_str_kham$year)
Sheet_1_str_kham$date1 = as.Date(Sheet_1_str_kham$date1,format="%d %B %Y")

#add total adult
Sheet_1_str_kham$total.adult = Sheet_1_str_kham$total.adult.female+Sheet_1_str_kham$adult.male

# next, subset and melt only interested data, the select it

sub.cap.k = Sheet_1_str_kham[,c("date1","total.adult","Juvenile","late.juvenile")]
sub.cap.k = melt(sub.cap.k[1:16,],id=c("date1"))


#plot weekly colony size, raw capture vs date
###############for kham
subset.colsiz = total.df[,c("location","colony.size","date")]
colsiz.k = subset.colsiz[which(subset.colsiz$location=="Kham"),]#extract only Kham

ggplot(colsiz.k,aes(x=date))  + 
  geom_bar(data=sub.cap.k,
           aes(x=date1,y=value*200/30,group=variable,fill=variable), 
           stat="identity")+
  geom_point(data = colsiz.k,aes(y=colony.size))+
  geom_line(data = colsiz.k,aes(y=colony.size))+
  scale_y_continuous(name = "capture number"
    ,sec.axis = sec_axis(~(.+5)/10))+
  theme_bw()+
  theme(axis.text.x = element_text(angle=90,hjust = 1,vjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs(x="Study period", y="Size of colonies",
       colour="Location of the colonies")

################################for Chang############################

subset.colsiz = total.df[,c("location","colony.size","date")]
colsiz.c = subset.colsiz[which(subset.colsiz$location!="Kham"),]#extract only Chang

Sheet_1_str_chang = read_csv("Sheet 1-str.chang.csv")
Sheet_1_str_chang$date1 = paste(Sheet_1_str_chang$date,Sheet_1_str_chang$month,Sheet_1_str_chang$year)
Sheet_1_str_chang$date1 = as.Date(Sheet_1_str_chang$date1,format="%d %B %Y")

Sheet_1_str_chang$total.adult = Sheet_1_str_chang$`total adult female`+Sheet_1_str_chang$`adult male`

sub.cap.c = Sheet_1_str_chang[,c("date1","total.adult","Juvenile","late juvenile")]
sub.cap.c = melt(sub.cap.c[1:16,],id=c("date1"))

ggplot(colsiz.c,aes(x=date))  + 
  geom_bar(data=sub.cap.c,
           aes(x=date1,y=value*200/30,group=variable,fill=variable), 
           stat="identity")+
  geom_point(data = colsiz.c,aes(y=colony.size))+
  geom_line(data = colsiz.c,aes(y=colony.size))+
  scale_y_continuous(name = "capture number"
                     ,sec.axis = sec_axis(~(.+5)/10))+
  theme_bw()+
  theme(axis.text.x = element_text(angle=90,hjust = 1,vjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs(x="Study period", y="Size of colonies",
       colour="Location of the colonies")
#################################################################################
################  Corr between colony size and various factors  #############3###
#################################################################################
# Overview [spearman rank correlation b/c i had to use avg monthly colony size]
# Colony size vs proportion of adult male 
# Colony size vs proportion of late juvenile
# Colony size vs proportion of reproductive male
# Colony size vs temperature
# Colony size vs moisture
# Colony size vs season

#plot monthly mean colony size
#preparing data.frame
total.df$monthly = paste(total.df$month,total.df$year)
total.df$press = total.df$pressure/1000
total.df$monthly = factor(total.df$monthly, levels = order.month)
monthcolo.mean = ddply(total.df,c("location","monthly"), summarize, mean.colony=mean(colony.size))
monthcolo.sd = ddply(total.df,c("location","monthly"), summarize, sd=sd(colony.size))
temp.mean = ddply(total.df,c("location","monthly"), summarize, mean.temp=mean(`air temperature`))
moist.mean = ddply(total.df,c("location","monthly"), summarize, mean.moist=mean(humidity))
monthcolo = cbind(monthcolo.mean,monthcolo.sd$sd,temp.mean$mean.temp,moist.mean$mean.moist)
monthcolo$colony = rep("colony",32)
monthcolo$locations = paste(monthcolo$location,monthcolo$colony)
monthcolo$monthly = factor(monthcolo$monthly)
colnames(monthcolo) = c("location", "month", "colsize", "colsd", "temp", "moist",
                        "col", "locations")
#spearman corr test of temp and moisture
cor.test(monthcolo$temp,monthcolo$colsize, method = c("spearman"))
cor.test(monthcolo$moist,monthcolo$colsize, method = c("spearman"))

ggplot(data=monthcolo, aes(x= temp, y=colsize))+
  geom_point()
ggplot(data=monthcolo, aes(x= moist, y=colsize))+
  geom_point()

#spearman corr test of ratio
Sheet_2_Table_3 = read.csv("Sheet 2-Table 3.csv")
Sheet_2_Table_3 = Sheet_2_Table_3[1:16,]
total.ratio = cbind( Sheet_2_Table_3)[, c(1,7,9)]

melt.col = melt(total.ratio, id= "Month")

cor.test(melt.col$value,monthcolo$colsize, method = c("spearman"))

ggplot(data=monthcolo, aes(x= melt.col$value, y=colsize))+
  geom_point()



###############################################################################
############  for the map ###################

library(maptools)
library(raster)
alt <- getData('alt', country='THA')
adm <- getData('GADM', country='THA', leve=1)
mar<-(adm[adm$NAME_1=="Chon Buri",])
maralt<-crop(alt,mar)
persp(maralt, exp=0.2,phi=35, xlab="Longitude", ylab="Latitude", zlab="Elevation")

