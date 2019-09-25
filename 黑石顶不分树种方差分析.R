hsdRs<-read.csv(file.choose(),header=TRUE,stringsAsFactors = FALSE)#黑石顶第一次-34批呼吸汇总new-不分树种.csv
head(hsdRs)
hsdRs2<-hsdRs
f<-strsplit(hsdRs2$Name,split="")
Groups<-c()
for(i in 1:length(f)){
  Groups[i]<-substr(hsdRs2$Name[i],start=1,stop=length(f[[i]])-2)
}
hsdRs2$Groups<-as.factor(Groups)
head(hsdRs2)
# Rs
hsdRsaov<-aov(Daily.average2~T*F,dat=hsdRs2)
summary(hsdRsaov)

#N
hsdNaov<-aov(N...~T*F,dat=hsdRs2)
summary(hsdNaov)


#C
hsdCaov<-aov(C...~T*F,dat=hsdRs2)
summary(hsdCaov)


#C:N
hsdCNaov<-aov(C.N~T*F,dat=hsdRs2)
summary(hsdCNaov)

#DDOC
hsdDOCaov<-aov(DDOC~T*F,dat=hsdRs2)
summary(hsdDOCaov)


#DDON
hsdDONaov<-aov(DDON~T*F,dat=hsdRs2)
summary(hsdDONaov)


#DMBC
hsdMBCaov<-aov(DMBC~T*F,dat=hsdRs2)
summary(hsdMBCaov)


#DMBN
hsdMBNaov<-aov(DMBN~T*F,dat=hsdRs2)
summary(hsdMBNaov)


library(plyr)
hsd.dat <- 
  ddply(hsdRs2, .(Groups,Treatment), summarise,
        avg = mean(Daily.average2),
        se = sd(Daily.average2)/sqrt(length(Daily.average2)))
library(ggplot2)
a<-ggplot(hsd.dat, aes(x=Groups, y=avg, fill=Treatment)) +
  geom_bar(stat="identity",position="dodge")+
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se),
                position=position_dodge(0.9), width=.2)+
  ggtitle("Rh in HSD")+theme(plot.title = element_text(hjust = 0.5,size=20))+##调整大小
  theme( legend.title=element_text(size=18) , legend.text=element_text(size=16),
         axis.text=element_text(size=16),axis.title.x =element_text(size=16), axis.title.y=element_text(size=16),
         panel.background = element_rect(fill = "transparent",colour = NA),
         panel.grid.minor = element_blank(),
         panel.grid.major = element_blank(),
         plot.background = element_rect(fill = "transparent",colour = NA))
b<-a+ylab("μg C/ kg-1 soil" )
##多重比较
library(agricolae)
modelhsd<-aov(Daily.average2~Groups,data=hsdRs2)
outhsd<-LSD.test(modelhsd,"Groups",p.adj="none")
plot(outhsd)