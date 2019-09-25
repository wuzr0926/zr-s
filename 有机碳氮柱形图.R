#有机碳氮柱形图 
cn<-read.csv(file.choose(),header=TRUE,stringsAsFactors = FALSE)
dim(cn)
cn<-na.omit(cn)
f<-strsplit(cn$Treatment,split="")
Groups<-c()
for(i in 1:length(f)){
   Groups[i]<-substr(cn$Treatment[i],start=1,stop=length(f[[i]])-2)
   }
 cn$Groups<-as.factor(Groups)
 
library(plyr)
cn2<-
  ddply(cn,.(Site,Groups),summarise,avgN=mean(N...),avgC=mean(C...),
        avgCN=mean(C.N),seN=sd(N...)/sqrt(length(N...)),seC=sd(C...)/sqrt(length(C...)),seCN=sd(C.N)/sqrt(length(C.N)))

##CBS
#N%
cbsCN<-subset(cn3,Site=="CBS")
 ggplot(cbsCN,aes(x=Groups,y=avgN))+
   geom_bar(stat="identity",fill="plum",colour="plum")+
   geom_errorbar(aes(ymin=avgN-seN,ymax=avgN+seN),width=.2)+
   ggtitle("N% in CBS")+
   theme(plot.title = element_text(hjust = 0.5))
 #C%
 cbsCN<-subset(cn3,Site=="CBS")
 ggplot(cbsCN,aes(x=Groups,y=avgC))+
   geom_bar(stat="identity",fill="plum",colour="plum")+
   geom_errorbar(aes(ymin=avgC-seC,ymax=avgC+seC),width=.2)+
   ggtitle("C% in CBS")+
   theme(plot.title = element_text(hjust = 0.5))
 #C：N
 cbsCN<-subset(cn3,Site=="CBS")
 ggplot(cbsCN,aes(x=Groups,y=avgCN))+
   geom_bar(stat="identity",fill="plum",colour="plum")+
   geom_errorbar(aes(ymin=avgCN-seCN,ymax=avgCN+seCN),width=.2)+
   ggtitle("C:N in CBS")+
   theme(plot.title = element_text(hjust = 0.5))
 #OMCN
 #各处理间多重比较
 library(agricolae)
 #DOC
 modeldoc<-aov(DDOC~Groups,data=cbsOM)
 outmodeldoc<-LSD.test(modeldoc,"Groups",p.adj="none")
 plot(outmodeldoc)
 
 #DON
 modeldon<-aov(DDON~Groups,data=cbsOM)
 outmodeldon<-LSD.test(modeldon,"Groups",p.adj="none")
 plot(outmodeldon)
 
 #MBC
 modelmbc<-aov(DMBC~Groups,data=cbsOM)
 outmodelmbc<-LSD.test(modelmbc,"Groups",p.adj="none")
 plot(outmodelmbc)
 
 #MBN
 modelmbn<-aov(DMBN~Groups,data=cbsOM)
 outmodelmbn<-LSD.test(modelmbn,"Groups",p.adj="none")
 plot(outmodelmbn)
 
##先求errorbar范围，再做柱形图 
 cbs.OM<-
    ddply(cbsOM,.(Species,Groups),summarise,avgDOC=mean(DDOC),
          avgDON=mean(DDON),avgMBC=mean(DMBC),avgMBN=mean(DMBN),
          seDOC=sd(DDOC)/sqrt(length(DDOC)),seDON=sd(DDON)/sqrt(length(DDON)),
          seMBC=sd(DMBC)/sqrt(length(DMBC)),seMBN=sd(DMBN)/sqrt(length(DMBN)))
 #DOC
 ggplot(cbs.OM,aes(x=Groups,y=avgDOC))+
   geom_bar(stat="identity",fill="plum",colour="plum")+
   geom_errorbar(aes(ymin=avgDOC-seDOC,ymax=avgDOC+seDOC),width=.2)+
   ggtitle("DOC in CBS")+
   theme(plot.title = element_text(hjust = 0.5))
 
 #DON
 ggplot(cbs.OM,aes(x=Groups,y=avgDON))+
   geom_bar(stat="identity",fill="plum",colour="plum")+
   geom_errorbar(aes(ymin=avgDON-seDON,ymax=avgDON+seDON),width=.2)+
   ggtitle("DON in CBS")+
   theme(plot.title = element_text(hjust = 0.5))
 
 #MBC
 ggplot(cbs.OM,aes(x=Groups,y=avgMBC))+
   geom_bar(stat="identity",fill="plum",colour="plum")+
   geom_errorbar(aes(ymin=avgMBC-seMBC,ymax=avgMBC+seMBC),width=.2)+
   ggtitle("MBC in CBS")+
   theme(plot.title = element_text(hjust = 0.5))
 
 #MBN
 ggplot(cbs.OM,aes(x=Groups,y=avgMBN))+
   geom_bar(stat="identity",fill="plum",colour="plum")+
   geom_errorbar(aes(ymin=avgMBN-seMBN,ymax=avgMBN+seMBN),width=.2)+
   ggtitle("MBN in CBS")+
   theme(plot.title = element_text(hjust = 0.5))
 
 ##HSD
 hsdCN<-subset(cn3,Site=="HSD")
 #N%
 ggplot(hsdCN,aes(x=Groups,y=avgN))+
   geom_bar(stat="identity",fill="plum",colour="plum")+
   geom_errorbar(aes(ymin=avgN-seN,ymax=avgN+seN),width=.2)+
   ggtitle("N% in HSD")+
   theme(plot.title = element_text(hjust = 0.5))
 
 #C%
 ggplot(hsdCN,aes(x=Groups,y=avgC))+
   geom_bar(stat="identity",fill="plum",colour="plum")+
   geom_errorbar(aes(ymin=avgC-seC,ymax=avgC+seC),width=.2)+
   ggtitle("C% in HSD")+
   theme(plot.title = element_text(hjust = 0.5))
 
 #C:N
 ggplot(hsdCN,aes(x=Groups,y=avgCN))+
   geom_bar(stat="identity",fill="plum",colour="plum")+
   geom_errorbar(aes(ymin=avgCN-seCN,ymax=avgCN+seCN),width=.2)+
   ggtitle("C:N in HSD")+
   theme(plot.title = element_text(hjust = 0.5))
 #DOC、MBC
 #先求各处理间多重比较
 library(agricolae)
 #DOC
 modeldoc<-aov(DDOC~Groups,data=hsdOM)
 outmodeldoc<-LSD.test(modeldoc,"Groups",p.adj="none")
 plot(outmodeldoc)
 
 #DON
 modeldon<-aov(DDON~Groups,data=hsdOM)
 outmodeldon<-LSD.test(modeldon,"Groups",p.adj="none")
 plot(outmodeldon)
 
 #MBC
 modelmbc<-aov(DMBC~Groups,data=hsdOM)
 outmodelmbc<-LSD.test(modelmbc,"Groups",p.adj="none")
 plot(outmodelmbc)
 
 #MBN
 modelmbn<-aov(DMBN~Groups,data=hsdOM)
 outmodelmbn<-LSD.test(modelmbn,"Groups",p.adj="none")
 plot(outmodelmbn)
 
 ##先求errorbar再做柱形图
 hsd.data<-
    ddply(hsdplot,.(Groups),summarise,avgDOC=mean(DDOC),
           avgDOC=mean(DDOC),avgMBC=mean(DMBC),seDOC=sd(DDOC)/sqrt(length(DDOC)),
          seMBC=sd(DMBC)/sqrt(length(DMBC)))
 #DOC
 ggplot(hsd.data,aes(x=Groups,y=avgDOC))+
   geom_bar(stat="identity",fill="plum",colour="plum")+
   geom_errorbar(aes(ymin=avgDOC-seDOC,ymax=avgDOC+seDOC),width=.2)+
   ggtitle("DOC in HSD")+
   theme(plot.title = element_text(hjust = 0.5))
 #MBC
 ggplot(hsd.data,aes(x=Groups,y=avgMBC))+
   geom_bar(stat="identity",fill="plum",colour="plum")+
   geom_errorbar(aes(ymin=avgMBC-seMBC,ymax=avgMBC+seMBC),width=.2)+
   ggtitle("MBC in HSD")+
   theme(plot.title = element_text(hjust = 0.5))
 