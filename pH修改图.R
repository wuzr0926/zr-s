##CBS
pH<-read.csv(file.choose(),header=TRUE,stringsAsFactors = FALSE)
head(pH)
pH2<-na.omit(pH)
cbspH<-subset(pH2,Site=="CBS")
f<-strsplit(cbspH$Name,split="")
Groups<-c()
for(i in 1:length(f)){
  Groups[i]<-substr(cbspH$Name[i],start=1,stop=length(f[[i]])-2)
}
cbspH$Groups<-as.factor(Groups)
cbspH2<-cbspH[,2:5]
library(plyr)
cbspH.dat <- 
  ddply(cbspH2, .(Groups,Species,Treatment), summarise,
        avg = mean(pH),
        se = sd(pH)/sqrt(length(pH)))
library(ggplot2)
a<-ggplot(cbspH.dat, aes(x=Species, y=avg, fill=Treatment)) +
  geom_bar(stat="identity",position="dodge")+
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se),
                position=position_dodge(0.9), width=.2)+
  ggtitle("pH in CBS")+theme(plot.title = element_text(hjust = 0.5))+
  theme( panel.background = element_rect(fill = "transparent",colour = NA),
         panel.grid.minor = element_blank(),
         panel.grid.major = element_blank(),
         plot.background = element_rect(fill = "transparent",colour = NA))
b<-a+ylab(" " )
#多重比较
library(agricolae)
modelcbspH<-aov(pH~Groups,data=cbspH2)
outcbspH<-LSD.test(modelcbspH,"Groups",p.adj="none")
plot(outcbspH)

##HSD
hsdpH<-subset(pH2,Site=="HSD")
f<-strsplit(hsdpH$Name,split="")
Groups<-c()
for(i in 1:length(f)){
  Groups[i]<-substr(hsdpH$Name[i],start=1,stop=length(f[[i]])-2)
}
hsdpH$Groups<-as.factor(Groups)
hsdpH2<-hsdpH[,2:5]
library(plyr)
hsdpH.dat <- 
  ddply(hsdpH2, .(Groups,Species,Treatment), summarise,
        avg = mean(pH),
        se = sd(pH)/sqrt(length(pH)))
library(ggplot2)
a<-ggplot(hsdpH.dat, aes(x=Species, y=avg, fill=Treatment)) +
  geom_bar(stat="identity",position="dodge")+
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se),
                position=position_dodge(0.9), width=.2)+
  ggtitle("pH in HSD")+theme(plot.title = element_text(hjust = 0.5))+
  theme( panel.background = element_rect(fill = "transparent",colour = NA),
         panel.grid.minor = element_blank(),
         panel.grid.major = element_blank(),
         plot.background = element_rect(fill = "transparent",colour = NA))
b<-a+ylab(" " )
#多重比较
library(agricolae)
modelhsdpH<-aov(pH~Groups,data=hsdpH2)
outhsdpH<-LSD.test(modelhsdpH,"Groups",p.adj="none")
plot(outhsdpH)