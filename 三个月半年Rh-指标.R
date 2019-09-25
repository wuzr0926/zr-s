aa<-read.csv(file.choose(),header=TRUE,stringsAsFactors = FALSE)#15天三个月半年Rh-指标.csv
head(aa)
aa<-na.omit(aa)
names(aa)[1]<-"Name"
names(aa)[3]<-"Treatment"
f<-strsplit(aa$Treatment,split="")
Groups<-c()
for(i in 1:length(f)){
  Groups[i]<-substr(aa$Treatment[i],start=1,stop=length(f[[i]])-2)
}
aa$Groups<-as.factor(Groups)
head(aa)
aa.2<-aa[,-1]
aa.2$Day<-as.factor(aa.2$Day)
cbs<-subset(aa.2,Site=="CBS")
hsd<-subset(aa.2,Site=="HSD")

##天数与指标
#CBS
aov1<-aov(DOC~Day,data=cbs)
aov2<-aov(DON~Day,data=cbs)
aov3<-aov(MBC~Day,data=cbs)
aov4<-aov(MBN~Day,data=cbs)
aov5<-aov(Rh~Day,data=cbs)
summary(aov1)#Day* p=0.04
summary(aov2)#Day*** p<0.01
summary(aov3)
summary(aov4)#Day*** p<0.01
summary(aov5)#Day*** p<0.01
pairwise.t.test(cbs$DOC,cbs$Day,p.adjust.method = "none")
plot(cbs$DOC~cbs$Day)
pairwise.t.test(cbs$DON,cbs$Day,p.adjust.method = "none")
plot(cbs$DON~cbs$Day)
pairwise.t.test(cbs$MBC,cbs$Day,p.adjust.method = "none")
plot(cbs$MBC~cbs$Day)
pairwise.t.test(cbs$MBN,cbs$Day,p.adjust.method = "none")
plot(cbs$MBN~cbs$Day)
pairwise.t.test(cbs$Rh,cbs$Day,p.adjust.method = "none")
plot(cbs$Rh~cbs$Day)

##HSD
aov1<-aov(DOC~Day,data=hsd)
aov2<-aov(DON~Day,data=hsd)
aov3<-aov(MBC~Day,data=hsd)
aov4<-aov(MBN~Day,data=hsd)
aov5<-aov(Rh~Day,data=hsd)
summary(aov1)#Day*** p<0.01
summary(aov2)#Day*** p<0.01
summary(aov3)#Day*** p<0.01
summary(aov4)
summary(aov5)#Day*** p<0.01
pairwise.t.test(hsd$DOC,hsd$Day,p.adjust.method = "none")#均值的多重比较
plot(hsd$DOC~hsd$Day)#看影响的方向
pairwise.t.test(hsd$DON,hsd$Day,p.adjust.method = "none")
plot(hsd$DON~hsd$Day)
pairwise.t.test(hsd$MBC,hsd$Day,p.adjust.method = "none")
plot(hsd$MBC~hsd$Day)
pairwise.t.test(hsd$MBN,hsd$Day,p.adjust.method = "none")
plot(hsd$MBN~hsd$Day)
pairwise.t.test(hsd$Rh,hsd$Day,p.adjust.method = "none")
plot(hsd$Rh~hsd$Day)



#分天
cbs.d90<-subset(cbs,Day=="C")
hsd.d90<-subset(hsd,Day=="C")
cbs.d180<-subset(cbs,Day=="D")
hsd.d180<-subset(hsd,Day=="D")

###线性回归
##90天
##CBS
##DOC lm
fit1cbs.d90<-lm(Rh~DOC,data=cbs.d90)
summary(fit1cbs.d90)#R2=0.097 p=0.17



##DON lm
fit2cbs.d90<-lm(Rh~DON,data=cbs.d90)
summary(fit2cbs.d90)#R2=0.267 p=0.02

## MBC lm
fit3cbs.d90<-lm(Rh~MBC,data=cbs.d90)
summary(fit3cbs.d90)#R2=0.377 p=0.03


##MBN lm
fit4cbs.d90<-lm(Rh~MBN,data=cbs.d90)
summary(fit4cbs.d90)#R2=0.116 p=0.13

##HSD
##DOC lm
fit1hsd.d90<-lm(Rh~DOC,data=hsd.d90)
summary(fit1hsd.d90)#R2=0.04 p=0.79



##DON lm
fit2hsd.d90<-lm(Rh~DON,data=hsd.d90)
summary(fit2hsd.d90)#R2=0.090 p=0.21

## MBC lm
fit3hsd.d90<-lm(Rh~MBC,data=hsd.d90)
summary(fit3hsd.d90)#R2=0.008 p=0.72


##MBN lm
fit4hsd.d90<-lm(Rh~MBN,data=hsd.d90)
summary(fit4hsd.d90)#R2=0.035 p=0.44

##180天
##CBS
##DOC lm
fit1cbs.d180<-lm(Rh~DOC,data=cbs.d180)
summary(fit1cbs.d180)#R2=0.105 p=0.16



##DON lm
fit2cbs.d180<-lm(Rh~DON,data=cbs.d180)
summary(fit2cbs.d180)#R2=0.013 p=0.64

## MBC lm
fit3cbs.d180<-lm(Rh~MBC,data=cbs.d180)
summary(fit3cbs.d180)#R2=0.121 p=0.13


##MBN lm
fit4cbs.d180<-lm(Rh~MBN,data=cbs.d180)
summary(fit4cbs.d180)#R2=0.095 p=0.18

##HSD
##DOC lm
fit1hsd.d180<-lm(Rh~DOC,data=hsd.d180)
summary(fit1hsd.d180)#R2=0.01 p=0.66



##DON lm
fit2hsd.d180<-lm(Rh~DON,data=hsd.d180)
summary(fit2hsd.d180)#R2=0.006 p=0.74

## MBC lm
fit3hsd.d180<-lm(Rh~MBC,data=hsd.d180)
summary(fit3hsd.d180)#R2=0.009 p=0.68


##MBN lm
fit4hsd.d180<-lm(Rh~MBN,data=hsd.d180)
summary(fit4hsd.d180)#R2<0.001 p=0.89

###画回归图
d90<-subset(aa,Day=="90d")
d180<-subset(aa,Day=="180d")
library(ggplot2)
##90天
##DOC
a1<-ggplot(d90,aes(x=DOC,y=Rh,colour=Site))+
  geom_point()+
  geom_smooth(method=lm)+
  annotate("text", x=5, y=12, parse=TRUE,label="R^2==0.097",hjust=0) +
  annotate("text", x=5, y=11, parse=TRUE,label="p==0.17",hjust=0)+
  annotate("text", x=7.5, y=8, parse=TRUE,label="R^2==0.04",hjust=0) +
  annotate("text", x=7.5, y=7, parse=TRUE,label="p==0.79",hjust=0)
b1<-a1+ylab("Rh(μg C/ kg soil/d)" )+theme_bw()##theme_bw() 去掉背景色

##DON
a2<-ggplot(d90,aes(x=DON,y=Rh,colour=Site))+
  geom_point()+
  geom_smooth(method=lm)+
  annotate("text", x=2.1, y=8, parse=TRUE,label="R^2==0.090",hjust=0) +
  annotate("text", x=2.1, y=7, parse=TRUE,label="p==0.21",hjust=0)+
  annotate("text", x=9, y=6, parse=TRUE,label="R^2==0.267",hjust=0) +
  annotate("text", x=9, y=5, parse=TRUE,label="p==0.02",hjust=0)
b2<-a2+ylab("Rh(μg C/ kg soil/d)" )+theme_bw()

##MBC
a3<-ggplot(d90,aes(x=MBC,y=Rh,colour=Site))+
  geom_point()+
  geom_smooth(method=lm)+
  annotate("text", x=0, y=8, parse=TRUE,label="R^2==0.008",hjust=0) +
  annotate("text", x=0, y=7, parse=TRUE,label="p==0.72",hjust=0)+
  annotate("text", x=900, y=6, parse=TRUE,label="R^2==0.377",hjust=0) +
  annotate("text", x=900, y=5, parse=TRUE,label="p==0.03",hjust=0)
b3<-a3+ylab("Rh(μg C/ kg soil/d)" )+theme_bw()

##MBN
a4<-ggplot(d90,aes(x=MBN,y=Rh,colour=Site))+
  geom_point()+
  geom_smooth(method=lm)+
  annotate("text", x=150, y=12, parse=TRUE,label="R^2==0.116",hjust=0) +
  annotate("text", x=150, y=11, parse=TRUE,label="p==0.13",hjust=0)+
  annotate("text", x=150, y=4, parse=TRUE,label="R^2==0.035",hjust=0) +
  annotate("text", x=150, y=3, parse=TRUE,label="p==0.44",hjust=0)
b4<-a4+ylab("Rh(μg C/ kg soil/d)" )+theme_bw()
##合并
library(ggpubr) 
d<-ggarrange(b1,b2,b3,b4,ncol=2,nrow=2,labels=c("A)","B)","C)","D)"))

##180天
##DOC
a1<-ggplot(d180,aes(x=DOC,y=Rh,colour=Site))+
  geom_point()+
  geom_smooth(method=lm)+
  annotate("text", x=9, y=13, parse=TRUE,label="R^2==0.105",hjust=0) +
  annotate("text", x=9, y=12, parse=TRUE,label="p==0.16",hjust=0)+
  annotate("text", x=10, y=7, parse=TRUE,label="R^2==0.01",hjust=0) +
  annotate("text", x=10, y=6, parse=TRUE,label="p==0.66",hjust=0)
b1<-a1+ylab("Rh(μg C/ kg soil/d)" )+theme_bw()##theme_bw() 去掉背景色

##DON
a2<-ggplot(d180,aes(x=DON,y=Rh,colour=Site))+
  geom_point()+
  geom_smooth(method=lm)+
  annotate("text", x=3, y=10, parse=TRUE,label="R^2==0.013",hjust=0) +
  annotate("text", x=3, y=9, parse=TRUE,label="p==0.64",hjust=0)+
  annotate("text", x=4, y=6.5, parse=TRUE,label="R^2==0.06",hjust=0) +
  annotate("text", x=4, y=5.5, parse=TRUE,label="p==0.74",hjust=0)
b2<-a2+ylab("Rh(μg C/ kg soil/d)" )+theme_bw()

##MBC
a3<-ggplot(d180,aes(x=MBC,y=Rh,colour=Site))+
  geom_point()+
  geom_smooth(method=lm)+
  annotate("text", x=1000, y=11.5, parse=TRUE,label="R^2==0.121",hjust=0) +
  annotate("text", x=1000, y=10.5, parse=TRUE,label="p==0.13",hjust=0)+
  annotate("text", x=0, y=2, parse=TRUE,label="R^2==0.009",hjust=0) +
  annotate("text", x=0, y=1, parse=TRUE,label="p==0.68",hjust=0)
b3<-a3+ylab("Rh(μg C/ kg soil/d)" )+theme_bw()

##MBN
a4<-ggplot(d180,aes(x=MBN,y=Rh,colour=Site))+
  geom_point()+
  geom_smooth(method=lm)+
  annotate("text", x=0, y=1.25, parse=TRUE,label="R^2<0.001",hjust=0) +
  annotate("text", x=0, y=0.25, parse=TRUE,label="p==0.89",hjust=0)+
  annotate("text", x=750, y=12.5, parse=TRUE,label="R^2==0.095",hjust=0) +
  annotate("text", x=750, y=11.5, parse=TRUE,label="p==0.18",hjust=0)
b4<-a4+ylab("Rh(μg C/ kg soil/d)" )+theme_bw()
##合并
library(ggpubr) 
d<-ggarrange(b1,b2,b3,b4,ncol=2,nrow=2,labels=c("A)","B)","C)","D)"))

###15天90天半年各指标柱状图
aa<-na.omit(aa)
cbs<-subset(aa,Site=="CBS")
hsd<-subset(aa,Site=="HSD")
d90<-subset(aa,Day=="90d")
d180<-subset(aa,Day=="180d")
d15<-subset(aa,Day=="15d")
head(aa)
names(aa)[c(1,3)]<-c("Name","Treatment")
###分样地按天数比较画图效果不理想
###分天按样地比较
##90d
#DOC
library(plyr)
d90.doc.dat<-
  ddply(d90, .(Site,Treatment), summarise,
        avg = mean(DOC),
        se = sd(DOC)/sqrt(length(DOC)))

library(RColorBrewer)
col<-brewer.pal(4,"Set1")
library(ggplot2)
a1<-ggplot(d90.doc.dat,aes(x=Site,y=avg,fill=Treatment))+
  geom_bar(stat="identity",position="dodge")+
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se),
                position=position_dodge(0.9), width=.2)+
  theme(axis.title.y= element_text(size=18, vjust=0.5, hjust=0.5))+
  theme(axis.text.y= element_text(size=18, vjust=0.5, hjust=0.5))+
  ggtitle("Day90")+theme_bw()+theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values=col)
b1<-a1+ylab("DOC(mg/Kg dry soil)")

#DON
library(plyr)
d90.don.dat<-
  ddply(d90, .(Site,Treatment), summarise,
        avg = mean(DON),
        se = sd(DON)/sqrt(length(DON)))

library(RColorBrewer)
col<-brewer.pal(4,"Set1")
library(ggplot2)
a2<-ggplot(d90.don.dat,aes(x=Site,y=avg,fill=Treatment))+
  geom_bar(stat="identity",position="dodge")+
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se),
                position=position_dodge(0.9), width=.2)+
  theme(axis.title.y= element_text(size=18, vjust=0.5, hjust=0.5))+
  theme(axis.text.y= element_text(size=18, vjust=0.5, hjust=0.5))+
  ggtitle("Day90")+theme_bw()+theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values=col)
b2<-a2+ylab("DON(mg/Kg dry soil)")

#MBC
library(plyr)
d90.mbc.dat<-
  ddply(d90, .(Site,Treatment), summarise,
        avg = mean(MBC),
        se = sd(MBC)/sqrt(length(MBC)))

library(RColorBrewer)
col<-brewer.pal(4,"Set1")
library(ggplot2)
a3<-ggplot(d90.mbc.dat,aes(x=Site,y=avg,fill=Treatment))+
  geom_bar(stat="identity",position="dodge")+
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se),
                position=position_dodge(0.9), width=.2)+
  theme(axis.title.y= element_text(size=18, vjust=0.5, hjust=0.5))+
  theme(axis.text.y= element_text(size=18, vjust=0.5, hjust=0.5))+
  ggtitle("Day90")+theme_bw()+theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values=col)
b3<-a3+ylab("MBC(mg/Kg dry soil)")

#MBN
library(plyr)
d90.mbn.dat<-
  ddply(d90, .(Site,Treatment), summarise,
        avg = mean(MBN),
        se = sd(MBN)/sqrt(length(MBN)))

library(RColorBrewer)
col<-brewer.pal(4,"Set1")
library(ggplot2)
a4<-ggplot(d90.mbn.dat,aes(x=Site,y=avg,fill=Treatment))+
  geom_bar(stat="identity",position="dodge")+
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se),
                position=position_dodge(0.9), width=.2)+
  theme(axis.title.y= element_text(size=18, vjust=0.5, hjust=0.5))+
  theme(axis.text.y= element_text(size=18, vjust=0.5, hjust=0.5))+
  ggtitle("Day90")+theme_bw()+theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values=col)
b4<-a4+ylab("MBN(mg/Kg dry soil)")
library(ggpubr) 
d1<-ggarrange(b1,b2,b3,b4,ncol=2,nrow=2,labels=c("A)","B)","C)","D)"))
##15d
#DOC
library(plyr)
d15.doc.dat<-
  ddply(d15, .(Site,Treatment), summarise,
        avg = mean(DOC),
        se = sd(DOC)/sqrt(length(DOC)))

library(RColorBrewer)
col<-brewer.pal(4,"Set1")
library(ggplot2)
a1<-ggplot(d15.doc.dat,aes(x=Site,y=avg,fill=Treatment))+
  geom_bar(stat="identity",position="dodge")+
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se),
                position=position_dodge(0.9), width=.2)+
  theme(axis.title.y= element_text(size=18, vjust=0.5, hjust=0.5))+
  theme(axis.text.y= element_text(size=18, vjust=0.5, hjust=0.5))+
  ggtitle("Day15")+theme_bw()+theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values=col)
b1<-a1+ylab("DOC(mg/Kg dry soil)")

#DON
library(plyr)
d15.don.dat<-
  ddply(d15, .(Site,Treatment), summarise,
        avg = mean(DON),
        se = sd(DON)/sqrt(length(DON)))

library(RColorBrewer)
col<-brewer.pal(4,"Set1")
library(ggplot2)
a2<-ggplot(d15.don.dat,aes(x=Site,y=avg,fill=Treatment))+
  geom_bar(stat="identity",position="dodge")+
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se),
                position=position_dodge(0.9), width=.2)+
  theme(axis.title.y= element_text(size=18, vjust=0.5, hjust=0.5))+
  theme(axis.text.y= element_text(size=18, vjust=0.5, hjust=0.5))+
  ggtitle("Day15")+theme_bw()+theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values=col)
b2<-a2+ylab("DON(mg/Kg dry soil)")

#MBC
library(plyr)
d15.mbc.dat<-
  ddply(d15, .(Site,Treatment), summarise,
        avg = mean(MBC),
        se = sd(MBC)/sqrt(length(MBC)))

library(RColorBrewer)
col<-brewer.pal(4,"Set1")
library(ggplot2)
a3<-ggplot(d15.mbc.dat,aes(x=Site,y=avg,fill=Treatment))+
  geom_bar(stat="identity",position="dodge")+
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se),
                position=position_dodge(0.9), width=.2)+
  theme(axis.title.y= element_text(size=18, vjust=0.5, hjust=0.5))+
  theme(axis.text.y= element_text(size=18, vjust=0.5, hjust=0.5))+
  ggtitle("Day15")+theme_bw()+theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values=col)
b3<-a3+ylab("MBC(mg/Kg dry soil)")

#MBN
library(plyr)
d15.mbn.dat<-
  ddply(d15, .(Site,Treatment), summarise,
        avg = mean(MBN),
        se = sd(MBN)/sqrt(length(MBN)))

library(RColorBrewer)
col<-brewer.pal(4,"Set1")
library(ggplot2)
a4<-ggplot(d15.mbn.dat,aes(x=Site,y=avg,fill=Treatment))+
  geom_bar(stat="identity",position="dodge")+
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se),
                position=position_dodge(0.9), width=.2)+
  theme(axis.title.y= element_text(size=18, vjust=0.5, hjust=0.5))+
  theme(axis.text.y= element_text(size=18, vjust=0.5, hjust=0.5))+
  ggtitle("Day15")+theme_bw()+theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values=col)
b4<-a4+ylab("MBN(mg/Kg dry soil)")
library(ggpubr) 
d2<-ggarrange(b1,b2,b3,b4,ncol=2,nrow=2,labels=c("A)","B)","C)","D)"))
##180d
#DOC
library(plyr)
d180.doc.dat<-
  ddply(d180, .(Site,Treatment), summarise,
        avg = mean(DOC),
        se = sd(DOC)/sqrt(length(DOC)))

library(RColorBrewer)
col<-brewer.pal(4,"Set1")
library(ggplot2)
a1<-ggplot(d180.doc.dat,aes(x=Site,y=avg,fill=Treatment))+
  geom_bar(stat="identity",position="dodge")+
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se),
                position=position_dodge(0.9), width=.2)+
  theme(axis.title.y= element_text(size=18, vjust=0.5, hjust=0.5))+
  theme(axis.text.y= element_text(size=18, vjust=0.5, hjust=0.5))+
  ggtitle("Day180")+theme_bw()+theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values=col)
b1<-a1+ylab("DOC(mg/Kg dry soil)")

#DON
library(plyr)
d180.don.dat<-
  ddply(d180, .(Site,Treatment), summarise,
        avg = mean(DON),
        se = sd(DON)/sqrt(length(DON)))

library(RColorBrewer)
col<-brewer.pal(4,"Set1")
library(ggplot2)
a2<-ggplot(d180.don.dat,aes(x=Site,y=avg,fill=Treatment))+
  geom_bar(stat="identity",position="dodge")+
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se),
                position=position_dodge(0.9), width=.2)+
  theme(axis.title.y= element_text(size=18, vjust=0.5, hjust=0.5))+
  theme(axis.text.y= element_text(size=18, vjust=0.5, hjust=0.5))+
  ggtitle("Day180")+theme_bw()+theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values=col)
b2<-a2+ylab("DON(mg/Kg dry soil)")

#MBC
library(plyr)
d180.mbc.dat<-
  ddply(d180, .(Site,Treatment), summarise,
        avg = mean(MBC),
        se = sd(MBC)/sqrt(length(MBC)))

library(RColorBrewer)
col<-brewer.pal(4,"Set1")
library(ggplot2)
a3<-ggplot(d180.mbc.dat,aes(x=Site,y=avg,fill=Treatment))+
  geom_bar(stat="identity",position="dodge")+
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se),
                position=position_dodge(0.9), width=.2)+
  theme(axis.title.y= element_text(size=18, vjust=0.5, hjust=0.5))+
  theme(axis.text.y= element_text(size=18, vjust=0.5, hjust=0.5))+
  ggtitle("Day180")+theme_bw()+theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values=col)
b3<-a3+ylab("MBC(mg/Kg dry soil)")

#MBN
library(plyr)
d180.mbn.dat<-
  ddply(d180, .(Site,Treatment), summarise,
        avg = mean(MBN),
        se = sd(MBN)/sqrt(length(MBN)))

library(RColorBrewer)
col<-brewer.pal(4,"Set1")
library(ggplot2)
a4<-ggplot(d180.mbn.dat,aes(x=Site,y=avg,fill=Treatment))+
  geom_bar(stat="identity",position="dodge")+
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se),
                position=position_dodge(0.9), width=.2)+
  theme(axis.title.y= element_text(size=18, vjust=0.5, hjust=0.5))+
  theme(axis.text.y= element_text(size=18, vjust=0.5, hjust=0.5))+
  ggtitle("Day180")+theme_bw()+theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values=col)
b4<-a4+ylab("MBN(mg/Kg dry soil)")
library(ggpubr) 
d3<-ggarrange(b1,b2,b3,b4,ncol=2,nrow=2,labels=c("A)","B)","C)","D)"))