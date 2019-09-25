cumu10<-read.csv(file.choose(),header=TRUE,stringsAsFactors = TRUE)#前十天呼吸-不分树种.csv
cbs<-subset(cumu10,Site=="CBS")
hsd<-subset(cumu10,Site=="HSD")
library(agricolae)
modelcbs<-aov(avgCumu~Treatment,data=cbs)
outcbs<-LSD.test(modelcbs,"Treatment",p.adj="none")
plot(outcbs)#全a,方差分析T、F影响均不显著

summary(aov(avgCumu~T*F,data=hsd))#T p=0.03;F p<0.01;T:F p=0.04
library(agricolae)
modelhsd<-aov(avgCumu~Treatment,data=hsd)
outhsd<-LSD.test(modelhsd,"Treatment",p.adj="none")
plot(outhsd)#a,b,b,b
hsd$Treatment<-as.factor(hsd$Treatment)
pairwise.t.test(hsd$avgCumu,hsd$Treatment,p.adjust.method="none")
plot(hsd$avgCumu~hsd$Treatment)

###画呼吸柱状图
library(plyr)
Rh.dat<-ddply(cumu10, .(Site,Treatment), summarise,
               avg = mean(avgCumu),
               se = sd(avgCumu)/sqrt(length(avgCumu)))
Rh.dat
library(RColorBrewer)
col<-brewer.pal(4,"Set1")
library(ggplot2)#分别对比两样地各处理OTU总量的差异
Rh<-ggplot(Rh.dat, aes(x=Site, y=avg, fill=Treatment)) +
  geom_bar(stat="identity",position="dodge")+
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se),
                position=position_dodge(0.9), width=.2)+
  scale_fill_manual(values=col)+
  theme_bw()
Rh.2<-Rh+ylab("Rh(μg C/ kg soil/d)")+theme(legend.position=c(0.96,0.95), legend.justification=c(0.96,0.95))+
  theme(axis.text.x=element_text(size=14),axis.text.y=element_text(size=14),
        axis.title.x = element_text(size=14),axis.title.y = element_text(size=14),
        legend.title=element_text(size=13) , 
        legend.text=element_text(size=12))
cbs.Rh<-subset(Rh.dat,Site=="CBS")
hsd.Rh<-subset(cumu10,Site=="HSD")
library(agricolae)
modelhsd.otu<-aov(R~Treatment,data=hsd)
outhsd.otu<-LSD.test(modelhsd.otu,"Treatment",p.adj="none")
plot(outhsd.otu)
###CBS
##Rh Cumu
treat.levels<-as.vector(unique(cbs$Treatment))
for(tre in 1:length(treat.levels)){#对于treat.levels中的每一个treatment,"tr"可随意命名
  tmp <- subset(cbs, Treatment==treat.levels[tre])#先赋值一个新变量，选取子集为每一个tr，
  ##注意要用"=="表示判定！若只有一个"="则表示赋值
  Cumu2 <- c()
  for(j in 1:NROW(tmp)){#对于每个tmpD的第j个元素，执行以下运算
    Cumu2[j] <- sum(tmp$avgCumu[1:j])
  }
  tmp$avgCumu.2 <- Cumu2
  if(tre ==1){
    cbs.2 = tmp
  }else{cbs.2 = rbind(cbs.2, tmp)}
  #AA.3 <- rbind(AA,tmp)
}
##se Cumu
treat.levels<-as.vector(unique(cbs$Treatment))
for(tre in 1:length(treat.levels)){#对于treat.levels中的每一个treatment,"tr"可随意命名
  tmp <- subset(cbs, Treatment==treat.levels[tre])#先赋值一个新变量，选取子集为每一个tr，
  ##注意要用"=="表示判定！若只有一个"="则表示赋值
  Cumu2 <- c()
  for(j in 1:NROW(tmp)){#对于每个tmpD的第j个元素，执行以下运算
    Cumu2[j] <- sum(tmp$seCumu[1:j])
  }
  tmp$seCumu.2 <- Cumu2
  if(tre ==1){
    cbs.3 = tmp
  }else{cbs.3 = rbind(cbs.3, tmp)}
  #AA.3 <- rbind(AA,tmp)
}
seCumu.2<-cbs.3[,7]
cbs.4<-cbind(cbs.2,seCumu.2)

###HSD
##Rh Cumu
treat.levels<-as.vector(unique(hsd$Treatment))
for(tre in 1:length(treat.levels)){#对于treat.levels中的每一个treatment,"tr"可随意命名
  tmp <- subset(hsd, Treatment==treat.levels[tre])#先赋值一个新变量，选取子集为每一个tr，
  ##注意要用"=="表示判定！若只有一个"="则表示赋值
  Cumu2 <- c()
  for(j in 1:NROW(tmp)){#对于每个tmpD的第j个元素，执行以下运算
    Cumu2[j] <- sum(tmp$avgCumu[1:j])
  }
  tmp$avgCumu.2 <- Cumu2
  if(tre ==1){
    hsd.2 = tmp
  }else{hsd.2 = rbind(hsd.2, tmp)}
  #AA.3 <- rbind(AA,tmp)
}
##se Cumu
treat.levels<-as.vector(unique(hsd$Treatment))
for(tre in 1:length(treat.levels)){#对于treat.levels中的每一个treatment,"tr"可随意命名
  tmp <- subset(hsd, Treatment==treat.levels[tre])#先赋值一个新变量，选取子集为每一个tr，
  ##注意要用"=="表示判定！若只有一个"="则表示赋值
  Cumu2 <- c()
  for(j in 1:NROW(tmp)){#对于每个tmpD的第j个元素，执行以下运算
    Cumu2[j] <- sum(tmp$seCumu[1:j])
  }
  tmp$seCumu.2 <- Cumu2
  if(tre ==1){
    hsd.3 = tmp
  }else{hsd.3 = rbind(hsd.3, tmp)}
  #AA.3 <- rbind(AA,tmp)
}
seCumu.2<-hsd.3[,7]
hsd.4<-cbind(hsd.2,seCumu.2)
###两样地合并
cumuSites<-rbind(cbs.4,hsd.4)

###作图
library(ggplot2)
a1<-ggplot(cbs.4, aes(x=Time, y=avgCumu.2,col=Treatment))+
  geom_line()+
  geom_point()+
  geom_errorbar(aes(ymin=avgCumu.2-seCumu.2, ymax=avgCumu.2+seCumu.2),
  width=.2)+
  theme(axis.text = element_text(angle = 45,hjust=1))+theme_bw()+
  ggtitle("CBS")+theme(plot.title = element_text(hjust = 0.5))
b1<-a1+xlab("Day")+ylab("Cumulative respiration(μg C/Kg soil/d)")
c1<-b1+theme(legend.position=c(1,0), legend.justification=c(1,0))+
  theme(axis.text.x=element_text(size=14),axis.text.y=element_text(size=14),
        axis.title.x = element_text(size=14),axis.title.y = element_text(size=14),
        legend.title=element_text(size=13) , 
        legend.text=element_text(size=12))

a2<-ggplot(hsd.4, aes(x=Time, y=avgCumu.2,col=Treatment))+
  geom_line()+
  geom_point()+
  geom_errorbar(aes(ymin=avgCumu.2-seCumu.2, ymax=avgCumu.2+seCumu.2),
                width=.2)+
  theme(axis.text = element_text(angle = 45,hjust=1))+theme_bw()+
  ggtitle("HSD")+theme(plot.title = element_text(hjust = 0.5))
b2<-a2+xlab("Day")+ylab("Cumulative respiration(μg C/Kg soil/d)")
c2<-b2+theme(legend.position=c(1,0), legend.justification=c(1,0))+
  theme(axis.text.x=element_text(size=14),axis.text.y=element_text(size=14),
        axis.title.x = element_text(size=14),axis.title.y = element_text(size=14),
        legend.title=element_text(size=13) , 
        legend.text=element_text(size=12))

##合并
library(ggpubr) 
d<-ggarrange(c1,c2,ncol=2,nrow=1,labels=c("A)","B)"))

  