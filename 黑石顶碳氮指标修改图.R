##hsd N C C:N DDOC DDON DMBC DMBN
#N
hsdN<-read.csv(file.choose(),header=TRUE,stringsAsFactors = FALSE)
head(hsdN)
f<-strsplit(hsdN$Name,split="")
Groups<-c()
for(i in 1:length(f)){
  Groups[i]<-substr(hsdN$Name[i],start=1,stop=length(f[[i]])-2)
}
hsdN$Groups<-as.factor(Groups)
library(plyr)
hsdN.dat <- 
  ddply(hsdN, .(Groups,Species,Treatment), summarise,
        avg = mean(N...),
        se = sd(N...)/sqrt(length(N...)))
library(ggplot2)
a<-ggplot(hsdN.dat, aes(x=Species, y=avg, fill=Treatment)) +
  geom_bar(stat="identity",position="dodge")+
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se),
                position=position_dodge(0.9), width=.2)+
  ggtitle("N% in HSD")+theme(plot.title = element_text(hjust = 0.5))+
  theme( panel.background = element_rect(fill = "transparent",colour = NA),
         panel.grid.minor = element_blank(),
         panel.grid.major = element_blank(),
         plot.background = element_rect(fill = "transparent",colour = NA))
b<-a+ylab("Average" )
#多重比较
library(agricolae)
modelhsdN<-aov(N...~Groups,data=hsdN)
outhsdN<-LSD.test(modelhsdN,"Groups",p.adj="none")
plot(outhsdN)

#C
library(plyr)
hsdC.dat <- 
  ddply(hsdN, .(Groups,Species,Treatment), summarise,
        avg = mean(C...),
        se = sd(C...)/sqrt(length(C...)))
library(ggplot2)
a<-ggplot(hsdC.dat, aes(x=Species, y=avg, fill=Treatment)) +
  geom_bar(stat="identity",position="dodge")+
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se),
                position=position_dodge(0.9), width=.2)+
  ggtitle("C% in HSD")+theme(plot.title = element_text(hjust = 0.5))+
  theme( panel.background = element_rect(fill = "transparent",colour = NA),
         panel.grid.minor = element_blank(),
         panel.grid.major = element_blank(),
         plot.background = element_rect(fill = "transparent",colour = NA))
b<-a+ylab("Average" )
#多重比较
library(agricolae)
modelhsdC<-aov(C...~Groups,data=hsdN)
outhsdC<-LSD.test(modelhsdC,"Groups",p.adj="none")
plot(outhsdC)

#C:N
library(plyr)
hsdCN.dat <- 
  ddply(hsdN, .(Groups,Species,Treatment), summarise,
        avg = mean(C.N),
        se = sd(C.N)/sqrt(length(C.N)))
library(ggplot2)
a<-ggplot(hsdCN.dat, aes(x=Species, y=avg, fill=Treatment)) +
  geom_bar(stat="identity",position="dodge")+
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se),
                position=position_dodge(0.9), width=.2)+
  ggtitle("C:N in HSD")+theme(plot.title = element_text(hjust = 0.5))+
  theme( panel.background = element_rect(fill = "transparent",colour = NA),
         panel.grid.minor = element_blank(),
         panel.grid.major = element_blank(),
         plot.background = element_rect(fill = "transparent",colour = NA))
b<-a+ylab("Average" )
#多重比较
library(agricolae)
modelhsdCN<-aov(C.N~Groups,data=hsdN)
outhsdCN<-LSD.test(modelhsdCN,"Groups",p.adj="none")
plot(outhsdCN)

#DDOC
library(plyr)
hsdDDOC.dat <- 
  ddply(hsdN, .(Groups,Species,Treatment), summarise,
        avg = mean(DDOC),
        se = sd(DDOC)/sqrt(length(DDOC)))
library(ggplot2)
a<-ggplot(hsdDDOC.dat, aes(x=Species, y=avg, fill=Treatment)) +
  geom_bar(stat="identity",position="dodge")+
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se),
                position=position_dodge(0.9), width=.2)+
  ggtitle("DOC in HSD")+theme(plot.title = element_text(hjust = 0.5))+
  theme( panel.background = element_rect(fill = "transparent",colour = NA),
         panel.grid.minor = element_blank(),
         panel.grid.major = element_blank(),
         plot.background = element_rect(fill = "transparent",colour = NA))
b<-a+ylab("mg/kg dry soil" )
#多重比较
library(agricolae)
modelhsdDDOC<-aov(DDOC~Groups,data=hsdN)
outhsdDDOC<-LSD.test(modelhsdDDOC,"Groups",p.adj="none")
plot(outhsdDDOC)

#DDON
library(plyr)
hsdDDON.dat <- 
  ddply(hsdN, .(Groups,Species,Treatment), summarise,
        avg = mean(DDON),
        se = sd(DDON)/sqrt(length(DDON)))
library(ggplot2)
a<-ggplot(hsdDDON.dat, aes(x=Species, y=avg, fill=Treatment)) +
  geom_bar(stat="identity",position="dodge")+
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se),
                position=position_dodge(0.9), width=.2)+
  ggtitle("DON in HSD")+theme(plot.title = element_text(hjust = 0.5))+
  theme( panel.background = element_rect(fill = "transparent",colour = NA),
         panel.grid.minor = element_blank(),
         panel.grid.major = element_blank(),
         plot.background = element_rect(fill = "transparent",colour = NA))
b<-a+ylab("mg/kg dry soil" )
#多重比较
library(agricolae)
modelhsdDDON<-aov(DDON~Groups,data=hsdN)
outhsdDDON<-LSD.test(modelhsdDDON,"Groups",p.adj="none")
plot(outhsdDDON)

#DMBC
hsdDMBC.dat <- 
  ddply(hsdN, .(Groups,Species,Treatment), summarise,
        avg = mean(DMBC),
        se = sd(DMBC)/sqrt(length(DMBC)))
library(ggplot2)
a<-ggplot(hsdDMBC.dat, aes(x=Species, y=avg, fill=Treatment)) +
  geom_bar(stat="identity",position="dodge")+
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se),
                position=position_dodge(0.9), width=.2)+
  ggtitle("MBC in HSD")+theme(plot.title = element_text(hjust = 0.5))+
  theme( panel.background = element_rect(fill = "transparent",colour = NA),
         panel.grid.minor = element_blank(),
         panel.grid.major = element_blank(),
         plot.background = element_rect(fill = "transparent",colour = NA))
b<-a+ylab("mg/kg dry soil" )
#多重比较
library(agricolae)
modelhsdDMBC<-aov(DMBC~Groups,data=hsdN)
outhsdDMBC<-LSD.test(modelhsdDMBC,"Groups",p.adj="none")
plot(outhsdDMBC)

#DMBN
hsdDMBN.dat <- 
  ddply(hsdN, .(Groups,Species,Treatment), summarise,
        avg = mean(DMBN),
        se = sd(DMBN)/sqrt(length(DMBN)))
library(ggplot2)
a<-ggplot(hsdDMBN.dat, aes(x=Species, y=avg, fill=Treatment)) +
  geom_bar(stat="identity",position="dodge")+
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se),
                position=position_dodge(0.9), width=.2)+
  ggtitle("MBN in HSD")+theme(plot.title = element_text(hjust = 0.5))+
  theme( panel.background = element_rect(fill = "transparent",colour = NA),
         panel.grid.minor = element_blank(),
         panel.grid.major = element_blank(),
         plot.background = element_rect(fill = "transparent",colour = NA))
b<-a+ylab("mg/kg dry soil" )
#多重比较
library(agricolae)
modelhsdDMBN<-aov(DMBN~Groups,data=hsdN)
outhsdDMBN<-LSD.test(modelhsdDMBN,"Groups",p.adj="none")
plot(outhsdDMBN)
