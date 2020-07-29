library(ggplot2)
ev1<-c(0.055,0.107,.156,.214,.248,.234)
ev2<-c("s1","s2","s3","s4","s5","s6")
ev3<-c(500,250,125,62.5,31.25,15.62)
log10(ev3)->ev3
ev<-cbind(ev2,ev1,ev3)
as.matrix(ev)

write.csv(ev,file="eve.csv")
file.location<-getwd()
eve<-read.csv(file.path(file.location,"eve.csv"))

ggplot(eve,aes(x=ev3,y=ev1))+
  geom_point()+
 #stat_smooth(method = lm,se=F,size=0.5)+
  scale_x_log10()+
  theme_bw()

png("eve.png",width=, height =4, res=200, units ="in") 
  dev.off()
  #BM
  rnorm2 <- function(n,mean,sd) { mean+sd*scale(rnorm(n)) }
  rj <- rnorm2(73,20.55,2.19);range(rj)
  write.csv(rj,file="rj.csv")
  rm<-rnorm2(238,24.1,1.4);range(rm)
  write.csv(rm,file="rm.csv")
  rf<-rnorm2(254,25,1.6);range(rf)
  write.csv(rf,file="rf.csv")
  rnr<-rnorm2(51,26.3,1.8);range(rnr)
  write.csv(rnr,file="rnr.csv")
  rl<-rnorm2(18,28.4,1.9);range(rl)
  write.csv(rl,file="rl.csv")
  rpr<-rnorm2(19,31.4,2.1);range(rpr)
  write.csv(rpr,file="rpr.csv")
  #FA
  fm <- rnorm2(238,64.7,1.5);range(fm)
  write.csv(fm,file="fm.csv")
  fnrpr<-rnorm2(69,64.9,1.4);range(fnrpr)
  write.csv(fnrpr,file="fnrpr.csv")
  ff<-rnorm2(254,64.5,1.7);range(ff)
  write.csv(ff,file="ff.csv")
  fl<-rnorm2(18,64.45,1.8);range(fl)
  write.csv(fl,file="fl.csv")
  fj<-rnorm2(73,61.8,2.5);range(fj)
  write.csv(fj,file="fj.csv")
  
  fj<-rnorm2(49,23.2,0.75);range(fj)
  write.csv(fj,file="fj.csv")
  
  a1<-rnorm2(30,15,2);a1<- order(a1, decreasing = F)
  a2<-rnorm2(30,20,2);a2<- order(a2, decreasing = T)
  a4<-
  a3<-cbind(a1,a2)
ggplot(a3,aes(a1,a2))+geom_point()
  
library(foreign)
mydata <- read.dta("http://dss.princeton.edu/training/Panel101.dta")
m1 <- polr(opinion ~ x1 + x2 + x3, data=mydata, Hess=TRUE)

#start new

library(dplyr)
inter.em <- c(0,0.05,0.10,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,.6,.65,.7,.75,.8,.85,.9,.95,1)

in_5$interval <- cut(in_5$percent, inter.em, 
                     labels=c("0.05","0.10","0.15","0.2","0.25",
                              "0.3","0.35","0.4","0.45","0.5","0.55",
                              "0.6","0.65","0.7","0.75","0.8","0.85",
                              "0.9","0.95","1"))
Data.in = 
  select(in_5,
         phase, 
         status,
         sex,
         age,
         BM,
         FA,
         interval)
Data.in$BM= as.numeric(Data.in$BM)
Data.in$FA= as.numeric(Data.in$FA)

library(FSA)
headtail(Data.in)

model.null = glm(interval ~ 1, 
                 data=Data.in,
                 family = binomial(link="logit"))

model.full = glm(interval ~ phase+sex+age+BM+FA+
                   BM:FA,
                 data=Data.in,
                 family = binomial(link="logit"))
step(model.null,
     scope = list(upper=model.full),
     direction="both",
     test="Chisq",
     data=Data.in)

model.final = glm(interval ~ phase+status+BM+FA,
                  data=Data.in,
                  family = binomial(link="logit"))
summary(model.final)
mo3<-glm(interval~status+BM+status*BM,data=sub.ph2,family = binomial(link="logit"))

model.1 = glm(interval ~ 1,
                  data=in_5,
                  family = binomial(link="logit"))
model.2 = glm(interval ~ phase,
              data=in_5,
              family = binomial(link="logit"))
model.3 = glm(interval ~ phase+sex,
              data=in_5,
              family = binomial(link="logit"))
model.4 = glm(interval ~ phase+sex+age,
              data=in_5,
              family = binomial(link="logit"))
model.5 = glm(interval ~ phase+sex+age+sex*age,
              data=in_5,
              family = binomial(link="logit"))
model.6 = glm(interval ~ phase+sex+age+sex*age+BM,
              data=in_5,
              family = binomial(link="logit"))
model.7 = glm(interval ~ phase+sex+age+sex*age+BM+FA,
              data=in_5,
              family = binomial(link="logit"))
model.8 = glm(interval ~ phase+sex+age+sex*age+BM+FA+BM*FA,
              data=in_5,
              family = binomial(link="logit"))
library(rcompanion)

compareGLM(model.1, model.2, model.3, model.4, model.5, model.6, model.7, model.8)

anova(model.1, model.2, model.3,model.4, model.5, model.6, model.7, model.8,
      test="Chisq")

ggplot(Data.in,aes(x=sex,y=interval,group=sex))+
  geom_boxplot()

library(car)
Anova(model.6, type="II", test="Wald")

library(rcompanion)
nagelkerke(model.7)


mo.1 = glm(interval ~ phase,
              data=in_5,
              family = binomial(link="logit"))
mo.2 = glm(interval ~ status+FA+BM,
              data=sub.ph1,
              family = binomial(link="logit"))
mo.3 = glm(interval ~ status,
              data=sub.ph2,
              family = binomial(link="logit"))
mo.4 = glm(interval ~ status+FA,
              data=sub.ph3,family = binomial(link="logit"))
mo.5 = glm(interval ~ BM*age,
              data=in_5,
              family = binomial(link="logit"))
mo.6 = glm(interval ~ FA,
              data=in_5,
              family = binomial(link="logit"))
mo.7 = glm(interval ~ BM*FA,
              data=in_5,
              family = binomial(link="logit"))
mo.8 = glm(interval ~ status,
              data=in_5,
              family = binomial(link="logit"))
mo.9 = glm(interval ~ phase+age+sex+age+BM+FA,
           data=in_5,
           family = binomial(link="logit"))


#create new data frame

inter.em.1 <- c(0.0105,0.104,0.198,0.292,0.385,0.479,0.573,.666,.76,.854,.947,1)
inter.em<-c(0,0.0122,0.231,0.341,0.451,0.561,0.671,0.78,0.89)

table(cut_interval(in_5$percent,9))


in_em[is.na(in_em)] <- 0
in_em$date<-paste(in_em$month,in_em$year)

out=NULL
for(i in 1:84){ 
  oui=NULL
  
  for(j in 1:19){
    a<-rep(in_em[i,32],in_em[i,j+6])
    b<-rep(in_em[i,3],in_em[i,j+6])
    c<-rep(in_em[i,6],in_em[i,j+6])
    d<-rep(colnames(in_em)[j+6],in_em[i,j+6])
    e<-rep(in_em[i,30],in_em[i,j+6])
    f<-rep(paste(colnames(in_em)[j+6],in_em[i,30],in_em[i,6],in_em[i,5],sep = "/"),in_em[i,j+6])
    ab<-cbind(a,b,c,d,e,f)
    oui=rbind(oui,ab)
  }
  out=rbind(out,oui)
}
write.csv(out,file="in.5.csv")
###################################################
in_5$interval <- cut(in_5$percent, inter.em.1, 
                     labels=c("0","0.1","0.2",
                              "0.3","0.4","0.5",
                              "0.6","0.7","0.8",
                              "0.9","1"))

sub.ph1.1<-in_5[1:106,];sub.ph1.2<-in_5[569:654,]
sub.ph1<-rbind(sub.ph1.1,sub.ph1.2)
sub.ph2<-in_5[107:218,]
sub.ph3<-in_5[219:568,]


mo.1 = glm(interval ~ age.1+sex.1*phase,
           data=in_5,
           family = binomial(link="logit"))
mo.2 = glm(interval ~ status+FA+BM,
           data=sub.ph1,
           family = binomial(link="logit"))
mo.3 = glm(interval ~ status*BM,
           data=sub.ph2,
           family = binomial(link="logit"))
mo.4 = glm(interval ~ age*FA,
           data=sub.ph3,family = binomial(link="logit"))

summary(mo.1) 
summary(mo.2)
summary(mo.3)
summary(mo.4)

ggplot(sub.ph2,aes(x=status,y=interval,group=status))+
  geom_boxplot(position = position_dodge(0.2))
geom_text(aes(label=a6$No),vjust=-1)


sub.ph2[which(sub.ph2$status=="NR"&sub.ph2$month=="June 2015"),]->a5
sub.ph2[which(sub.ph2$status=="L"),]->a5
table(a5$interval)
sub.ph2[which(sub.ph2$sex=="F"),]->a5
ggplot(a5,aes(y=interval,x=BM,colour=status))+
  geom_point()+
  geom_text(aes(label=a5$No),vjust=-1)

sub.ph3[which(sub.ph3$status=="F"|sub.ph3$status=="M"),]->a6  
sub.ph3[which(sub.ph3$status=="J"),]->a6  
table(a6$interval)
ggplot(sub.ph2,aes(x=BM,y=interval,colour=status))+
  geom_point(position = position_dodge(0.2))+
  geom_text(aes(label=sub.ph2$No),vjust=-1)



ggplot(sub.ph3,aes(x=age,y=interval,group=age))+
  geom_boxplot(position = position_dodge(0.2))
  geom_text(aes(label=a6$No),vjust=-1)
  


sub.ph2[which(sub.ph2$sex=="F"),]->a5

mo.nr<-glm(interval~BM,data=a5,family = binomial(link="logit"))
summary(mo.nr)
plot(fitted(mo.nr), 
     rstandard(mo.nr))

plot(interval.1+0.1 ~ BM, 
     data = a5,
     xlab="BM", 
     ylab="Emergence", 
     pch=19) 
curve(predict(mo.nr,data.frame(BM=x),type="response"), 
      lty=1, lwd=2, col="black",                            
      add=TRUE)
ggplot(a5,aes(x=BM,y=interval))+
  geom_point()

#plot juvenile and fa
sub.ph3[which(sub.ph3$age=="J"),]->a6

mo.j<-glm(interval~BM,data=a6,family = binomial(link="logit"))
summary(mo.j)
plot(fitted(mo.j), 
     rstandard(mo.j))
library(FSA)
plot(interval.1+0.1~ BM, 
     data = a6,
     xlab="FA", 
     ylab="percentage of emergence elapesd", 
     pch=19) 
curve(predict(mo.j,data.frame(BM=x),type="response"), 
      lty=1, lwd=2, col="black",                            
      add=T)
png("faj.png",width = 4,height = 4,units = "in",res=300)

  sub.ph2$status<-factor(sub.ph2$status, levels =  c("M", "NR", "P", "L"))  
  ggplot(sub.ph2,aes(x=status,y=interval,group=status))+
    geom_boxplot(size=0.5)+
   # coord_cartesian(ylim = c(0,10))+
   #scale_y_discrete(breaks=NULL)+
    labs(y="relative proportion of emergence", 
         x="reproductive status")
  png("status.png",width = 4, height = 4, units = "in", res=300)
  
  sub.ph3$status<-factor(sub.ph3$status, levels =  c("M", "F", "P", "J"))  
  ggplot(sub.ph3,aes(x=status,y=interval,group=status))+
    geom_boxplot(size=0.5)+
    # coord_cartesian(ylim = c(0,10))+
    #scale_y_discrete(breaks=NULL)+
    labs(y="relative proportion of emergence", 
         x="development status")
  png("dev.png",width = 4, height = 4, units = "in", res=300)
  
  in_5$sex<-factor(in_5$sex, levels = c("M","F"))
  in_5$phase<-factor(in_5$phase, levels = c("one","two","three"))
  in_5$status<-factor(in_5$status, levels = c("M","F","NR","P","L","J"))
  in_5$age<-factor(in_5$age, levels = c("J","A"))

  ggplot(in_5,aes(x=status,y=percent,group=status))+
  geom_boxplot(size=0.5, outlier.shape = 8,outlier.size = 1)+
  facet_grid(.~phase,scales = "free", space = "free")+
    theme_bw()+
    theme(panel.grid = element_blank())+
  labs(y="Normalized emergence time", 
       x="Reproductive/developmental status")
png("allfacet.png",width = 6, height = 3, units = "in", res=300)

 #find median of mean em time
  summary(sub.ph1[which(sub.ph1$status=="M"),])
  summary(sub.ph1[which(sub.ph1$status=="F"),])
  summary(sub.ph2[which(sub.ph2$status=="M"),])
  summary(sub.ph2[which(sub.ph2$status=="NR"),])
  summary(sub.ph2[which(sub.ph2$status=="P"),])
  summary(sub.ph2[which(sub.ph2$status=="L"),])
  summary(sub.ph3[which(sub.ph3$status=="M"),])
  summary(sub.ph3[which(sub.ph3$status=="F"),])
  summary(sub.ph3[which(sub.ph3$status=="J"),])
 
  sub.ph1[which(sub.ph1$status=="M"),]%>%count(interval)
  sub.ph1[which(sub.ph1$status=="F"),]%>%count(interval)
  sub.ph2[which(sub.ph2$status=="M"),]%>%count(interval)
  sub.ph2[which(sub.ph2$status=="NR"),]%>%count(interval)
  sub.ph2[which(sub.ph2$status=="P"),]%>%count(interval)
  sub.ph2[which(sub.ph2$status=="L"),]%>%count(interval)
  sub.ph3[which(sub.ph3$status=="M"),]%>%count(interval)
  sub.ph3[which(sub.ph3$status=="F"),]%>%count(interval)
  
  
  install.packages("effects")
  library(effects)
  
  sub.ph3$age<-factor(sub.ph3$age, levels = c("J","A"))
  sub.ph3$status<-factor(sub.ph3$status, levels = c("F","J","M"))
  mo.4 = glm(interval ~ status*FA,
             data=sub.ph3,family = binomial(link="logit"))
  summary(mo.4)
  allEffects(mo.4)
  plot(allEffects(mo.4))
  
  sub.ph2$sex<-factor(sub.ph2$sex, levels = c("F","M"))
  sub.ph2$status<-factor(sub.ph2$status, levels = c("M","NR","P","L"))
  mo.3 = glm(interval ~ sex+BM,
             data=sub.ph2,family = binomial(link="logit"))
  summary(mo.3)
  allEffects(mo.3)
  plot(allEffects(mo.3))
  
  in_5$sex<-factor(in_5$sex, levels = c("F","M"))
  in_5$phase<-factor(in_5$phase, levels = c("one","two","three"))
  in_5$status<-factor(in_5$status, levels = c("F","M","NR","P","L","J"))
  in_5$age<-factor(in_5$age, levels = c("J","A"))
  mo.1 = glm(interval ~ status*phase,
             data=in_5,family = binomial(link="logit"))
  summary(mo.1)
  allEffects(mo.1)
  plot(allEffects(mo.1))
  
  sub.ph3[which(
    sub.ph3$status=="J"),]->a5
table(a5$interval)
chi$sum<-chi$f1+chi$m1
ctest<-rbind(chi$f1,chi$sum)
chisq.test(ctest)

#start newly####
in_em[is.na(in_em)] <- 0
in_em$date<-paste(in_em$month,in_em$year)

out=NULL
for(i in 1:84){ 
  oui=NULL
  
  for(j in 1:19){
    a<-rep(in_em[i,32],in_em[i,j+6])
    b<-rep(in_em[i,3],in_em[i,j+6])
    c<-rep(in_em[i,6],in_em[i,j+6])
    d<-rep(colnames(in_em)[j+6],in_em[i,j+6])
    e<-rep(in_em[i,30],in_em[i,j+6])
    f<-rep(paste(colnames(in_em)[j+6],in_em[i,30],in_em[i,6],in_em[i,5],sep = "/"),in_em[i,j+6])
    ab<-cbind(a,b,c,d,e,f)
    oui=rbind(oui,ab)
  }
  out=rbind(out,oui)
}
write.csv(out,file="in.5.csv")
#put FA and BM with#
mean.BM<-ddply(in_5,c("f"), summarize, mean.BM=mean(BM))
mean.FA<-ddply(in_5,c("f"), summarize, mean.FA=mean(FA))
c.data<-cbind(mean.BM,mean.FA$mean.FA)
library(stringr)
str_split_fixed(c.data$f, "/", 4)
c.data<-cbind(str_split_fixed(c.data$f, "/", 4),mean.BM$mean.BM,mean.FA$mean.FA)
c.data<-cbind(c.data,rep("1",length(mean.BM$f)))
write.csv(c.data,file="c.data.csv")

in_em[is.na(in_em)] <- 0
in_em$date<-paste(in_em$month,in_em$year)

out=NULL
for(i in 1:84){ 
  oui=NULL
  
  for(j in 1:19){
    if(in_em[i,j+6]<1){
    d<-rep(colnames(in_em)[j+6],1)
    e<-rep(in_em[i,30],1)
    f<-rep(paste(colnames(in_em)[j+6],in_em[i,30],in_em[i,6],in_em[i,5],sep = "/"),1)
    ab<-cbind(d,e,f)
    }
    
    oui=rbind(oui,ab)
  }
  out=rbind(out,oui)
}
write.csv(out,file="wt0.csv")#then delete which duration lower than em#
wt0<-cbind(str_split_fixed(wt0$f, "/", 4),wt0$p)
write.csv(wt0,file="wt0.csv")
##start logistic
c_data[is.na(c_data)] <- 0
c_data$time<-c_data$em/c_data$du
cdata.ph1<-c_data[1:209,]
cdata.ph2<-c_data[729:1250,]
cdata.ph3<-c_data[210:728,]
mo.1 = glm(prop ~ time*BM,
           data=cdata.ph2[which(cdata.ph2$status=="P"),],
           family = binomial(link="logit"))
summary(mo.1)

ggplot(sub.ph3[which(sub.ph3$age=="J"),],aes(x=percent,y=FA))+
  geom_point()+stat_smooth(method = lm, se=F, colour="black", size=0.5)+
  theme_bw()+
  theme(panel.grid = element_blank())+
labs(y="Forearm length(mm)", 
     x="Normalized emergence time")
png("jf.png",width = 5, height = 4,unit="in",res=300)

ggplot(sub.ph3[which(sub.ph3$age=="J"),],aes(x=percent,y=BM))+
  geom_point()+stat_smooth(method = lm, se=F, colour="black", size=0.5)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  labs(y="Body mass(g)", 
       x="Normalized emergence time")
png("jb.png",width = 5, height = 4,unit="in",res=300)

ggplot(sub.ph2[which(sub.ph2$status=="NR"),],aes(x=percent,y=BM))+
  geom_point()+stat_smooth(method = lm, se=F, colour="black", size=0.5)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  labs(y="Body mass(g)", 
       x="Normalized emergence time")
png("nr.png",width = 5, height = 4,unit="in",res=300)

ggplot(sub.ph2[which(sub.ph2$status=="P"),],aes(x=percent,y=BM))+
  geom_point()+stat_smooth(method = lm, se=F, colour="black", size=0.5)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  labs(y="Body mass(g)", 
       x="Normalized emergence time")
png("preg.png",width = 5, height = 4,unit="in",res=300)


#fortestFA
ggplot(in_5,aes(x=percent,y=FA, colour=phase))+
  geom_point()+stat_smooth(method = lm, se=F, size=0.1, colour="black")
  geom_text(aes(label=in_5$No),vjust=-1)

ggplot(in_5, aes(x=phase, y=FA))+
  geom_boxplot()
#fortestBM
ggplot(in_5,aes(x=percent,y=BM, colour=phase))+
  geom_point()+stat_smooth(method = lm, se=F, size=0.1, colour="black")
geom_text(aes(label=in_5$No),vjust=-1)

ggplot(in_5, aes(x=phase, y=BM))+
  geom_boxplot()


ggplot(sub.ph3[which(sub.ph3$age=="J"),],aes(x=percent,y=FA))+
  geom_point()+stat_smooth(method = lm, se=F, colour="black", size=0.5)

a0<-lm(FA~percent,data = in_5);summary(a0)
a00<-lm(FA~phase,data = in_5);summary(a00)
a000<-lm(FA~percent:phase,data = in_5);summary(a000)

a0.1<-lm(FA~percent,data = sub.ph1);summary(a0.1)
a0.2<-lm(FA~percent,data = sub.ph2);summary(a0.2)
a0.3<-lm(FA~percent,data = sub.ph3);summary(a0.3)

a1.1<-lm(FA~percent,data = sub.ph3[which(sub.ph3$status=="J"),]);summary(a1.1)
a1.2<-lm(FA~percent,data = sub.ph3[which(sub.ph3$status=="F"),]);summary(a1.2)
a1.3<-lm(FA~percent,data = sub.ph3[which(sub.ph3$status=="M"),]);summary(a1.3)

b0<-lm(BM~percent,data = in_5);summary(b0)
b00<-lm(BM~phase,data = in_5);summary(b00)
b000<-lm(BM~percent:phase,data = in_5);summary(b000)

b0.1<-lm(BM~percent,data = sub.ph1);summary(b0.1)
b0.2<-lm(BM~percent,data = sub.ph2);summary(b0.2)
b0.3<-lm(BM~percent,data = sub.ph3);summary(b0.3)

a2.1<-lm(BM~percent,data = sub.ph3[which(sub.ph3$status=="J"),]);summary(a2.1)
a2.4<-lm(BM~percent,data = sub.ph3[which(sub.ph3$status=="M"),]);summary(a2.4)
a2.7<-lm(BM~percent,data = sub.ph3[which(sub.ph3$status=="F"),]);summary(a2.7)

a2.2<-lm(BM~percent,data = sub.ph2[which(sub.ph2$status=="NR"),]);summary(a2.2)
a2.3<-lm(BM~percent,data = sub.ph2[which(sub.ph2$status=="P"),]);summary(a2.3)
a2.5<-lm(BM~percent,data = sub.ph2[which(sub.ph2$status=="M"),]);summary(a2.5)
a2.6<-lm(BM~percent,data = sub.ph2[which(sub.ph2$status=="L"),]);summary(a2.6)

#plot#facets
in_5$status<-factor(in_5$status, levels =  c("M","F","NR","P","L","J"))
in_5$phase<-factor(in_5$phase,levels =  c("one", "two", "three"))

famean<-ddply(in_5, c("phase","status"),summarize,m=mean(FA))
fasd<-ddply(in_5, c("phase","status"),summarize,sd=sd(FA))
bmmean<-ddply(in_5, c("phase","status"),summarize,m=mean(BM))
bmsd<-ddply(in_5, c("phase","status"),summarize,sd=sd(BM))
fa<-cbind(famean,fasd$sd,rep("FA",length(famean$m)))
bm<-cbind(bmmean,bmsd$sd,rep("BM",length(bmmean$m)))
colnames(fa)<-c("phase","status","mean","sd","char")
colnames(bm)<-c("phase","status","mean","sd","char")
fabm<-rbind(fa,bm)
#forpoint
ggplot(fabm,aes(x=status,y=mean))+
  geom_point(shape=1,size=2)+
  geom_errorbar(aes(ymin= mean-sd,
                    ymax=mean+sd), 
                width=0.15,size=0.3)+
  facet_grid(char~phase,scales = "free")+
  theme_bw()+
labs(y="average", 
     x="reproductive/developmental status")
#for bar
ggplot(fabm,aes(x=status,y=mean))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin= mean,ymax=mean+sd), 
                width=0.15,size=0.3)+
  facet_grid(char~phase,scales = "free")+
  theme_bw()+
  labs(y="average", 
       x="reproductive/developmental status")
##plot with boxplot
in_5$status<-factor(in_5$status, levels =  c("M","F","NR","P","L","J"))
in_5$phase<-factor(in_5$phase,levels =  c("Phase I", "Phase II", "Phase III"))
bm1<-in_5[,c("phase","status","age","sex.1","BM","percent")]
fa1<-in_5[,c("phase","status","age","sex.1","FA","percent")]
bm1$char<-rep("BM",length(in_5$BM))
fa1$char<-rep("FA",length(in_5$FA))
colnames(fa1)<-c("phase","status","age","sex","value","percent","char")
colnames(bm1)<-c("phase","status","age","sex","value","percent","char")
bmfa1<-rbind(fa1,bm1)

ggplot(bmfa1,aes(x=status,y=value))+
  geom_boxplot(size=0.5,outlier.shape=8,
               outlier.size=1)+
  facet_grid(char~phase,scales = "free")+
  theme_bw()+
  theme(panel.grid = element_blank())+
  labs(y="average", 
       x="reproductive/developmental status")
png("facet.png",width = 6, height = 4,unit="in",res=300)
