rm(list=ls())

setwd("/Users/kasidit/Desktop/R")

#import data set, namely Sheet_1_TAble_1.csv 

library(tidyverse)
library(plyr)
library(scales)
library(reshape2)

total.df<-`Sheet_1_Table_1`

order.month<-c("January 2015","February 2015","March 2015","April 2015", "May 2015"
               ,"June 2015","July 2015", "August 2015", "September 2015"
               ,"October 2015", "November 2015", "December 2015","January 2016"
               ,"February 2016", "March 2016", "April 2016")

total.df$location<-factor(total.df$location, levels = c("Kham", "Changkleua"))



total.df$date<-paste(total.df$day,total.df$month,total.df$year)
total.df$date<-as.Date(total.df$date,format="%d %B %Y")
subset.colsiz<-total.df[,c("location","colony.size","date")]

#import capture data, then trannform into date data
Sheet_1_str_kham$date1<-paste(Sheet_1_str_kham$date,Sheet_1_str_kham$month,Sheet_1_str_kham$year)
Sheet_1_str_kham$date1<-as.Date(Sheet_1_str_kham$date1,format="%d %B %Y")

#add total adult
Sheet_1_str_kham$total.adult<-Sheet_1_str_kham$total.adult.female+Sheet_1_str_kham$adult.male

# next, subset and melt only interested data, the select it

sub.cap.k<-Sheet_1_str_kham[,c("date1","total.adult","Juvenile","late.juvenile")]
sub.cap.k<-melt(sub.cap.k[1:16,],id=c("date1"))


#plot weekly colony size, raw capture vs date
###############for kham
subset.colsiz<-total.df[,c("location","colony.size","date")]
colsiz.k<-subset.colsiz[which(subset.colsiz$location=="Kham"),]#extract only Kham

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

#################for Chang
subset.colsiz<-total.df[,c("location","colony.size","date")]
colsiz.c<-subset.colsiz[which(subset.colsiz$location!="Kham"),]#extract only Chang

Sheet_1_str_chang$date1<-paste(Sheet_1_str_chang$date,Sheet_1_str_chang$month,Sheet_1_str_chang$year)
Sheet_1_str_chang$date1<-as.Date(Sheet_1_str_chang$date1,format="%d %B %Y")

Sheet_1_str_chang$total.adult<-Sheet_1_str_chang$`total adult female`+Sheet_1_str_chang$`adult male`

sub.cap.c<-Sheet_1_str_chang[,c("date1","total.adult","Juvenile","late juvenile")]
sub.cap.c<-melt(sub.cap.c[1:16,],id=c("date1"))

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

###### plot precipitation vs female reprouctive period
# import & select




