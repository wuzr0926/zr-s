cbsotu<-subset(otusum2,Site=="CBS")
ggplot(cbsotu,aes(x=Groups,y=avg))+
  geom_bar(stat="identity",fill="white",colour="black")+
  geom_errorbar(aes(ymin=avg-se,ymax=avg+se),width=.2)+
  ggtitle("OTU sum in CBS")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text = element_text(angle = 45,hjust=1))

hsdotu<-subset(otusum2,Site=="HSD")
ggplot(hsdotu,aes(x=Groups,y=avg))+
  geom_bar(stat="identity",fill="white",colour="black")+
  geom_errorbar(aes(ymin=avg-se,ymax=avg+se),width=.2)+
  ggtitle("OTU sum in HSD")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text = element_text(angle = 45,hjust=1))

###190907 
OTU<-read.csv(file.choose(),header=TRUE,stringsAsFactors = FALSE)#两样地OTU_data总及真细.csv
head(OTU)
library(plyr)
OTU.dat<-ddply(OTU, .(Site,Name,Treatment), summarise,
               avg = mean(OTUs.sum),
               se = sd(OTUs.sum)/sqrt(length(OTUs.sum)))
OTU.dat
library(RColorBrewer)
col<-brewer.pal(4,"Set1")
library(ggplot2)#分别对比两样地各处理OTU总量的差异
otu<-ggplot(OTU.dat, aes(x=Site, y=avg, fill=Treatment)) +
  geom_bar(stat="identity",position="dodge")+
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se),
                position=position_dodge(0.9), width=.2)+
  scale_fill_manual(values=col)+
  theme_bw()+
 theme(axis.title.x =element_text(size=14), axis.title.y=element_text(size=14),
       axis.text.x= element_text(size=15),axis.text.y= element_text(size=15))
otu.2<-otu+ylab("OTU sum")+theme(legend.position=c(0.99,0.99), legend.justification=c(0.99,0.99))
pairwise.t.test()
##分开样地画
cbs<-subset(OTU,Site=="CBS")
hsd<-subset(OTU,Site=="HSD")
#CBS
library(plyr)
cbs.dat<-ddply(cbs, .(Name,Treatment), summarise,
               avg = mean(OTUs.sum),
               se = sd(OTUs.sum)/sqrt(length(OTUs.sum)))
cbs.dat
library(RColorBrewer)
col<-brewer.pal(4,"Set1")
library(ggplot2)
cbs.otu<-ggplot(cbs.dat, aes(x=Treatment, y=avg, fill=Treatment)) +
  geom_bar(stat="identity",position="dodge")+
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se),
                position=position_dodge(0.9), width=.2)+ 
  scale_fill_manual(values=col)+
  theme_bw()+
  ggtitle("OTU Sum in CBS")+theme(plot.title = element_text(hjust = 0.5))#ggtitle要放在theme_bw()后面，不然无法居中标题
cbs.otu.2<-cbs.otu+ylab("OTU sum")
cbs.otu.2
#多重比较
library(agricolae)
modelcbs.otu<-aov(OTUs.sum~Treatment,data=cbs)
outcbs.otu<-LSD.test(modelcbs.otu,"Treatment",p.adj="none")
plot(outcbs.otu)

#HSD
library(plyr)
hsd.dat<-ddply(hsd, .(Name,Treatment), summarise,
               avg = mean(OTUs.sum),
               se = sd(OTUs.sum)/sqrt(length(OTUs.sum)))
hsd.dat
library(RColorBrewer)
col<-brewer.pal(4,"Set1")
library(ggplot2)
hsd.otu<-ggplot(hsd.dat, aes(x=Treatment, y=avg, fill=Treatment)) +
  geom_bar(stat="identity",position="dodge")+
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se),
                position=position_dodge(0.9), width=.2)+ 
  scale_fill_manual(values=col)+
  theme_bw()+
  ggtitle("OTU Sum in HSD")+theme(plot.title = element_text(hjust = 0.5))#ggtitle要放在theme_bw()后面，不然无法居中标题
hsd.otu.2<-hsd.otu+ylab("OTU sum")
hsd.otu.2
#多重比较
library(agricolae)
modelhsd.otu<-aov(OTUs.sum~Treatment,data=hsd)
outhsd.otu<-LSD.test(modelhsd.otu,"Treatment",p.adj="none")
plot(outhsd.otu)

##合并
library(ggpubr) 
d<-ggarrange(cbs.otu.2,hsd.otu.2,ncol=2,nrow=1,labels=c("A)","B)"))