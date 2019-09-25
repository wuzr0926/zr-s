data <- read.csv(file.choose(),header=TRUE,stringsAsFactors = FALSE)
Treat[22] <- "H-G-CK-2"
a <-  strsplit(data$Treatment,split = "")#�ܹ���48��Treatment~48��Treat
groups <- c()
for (i in 1:length(a)){#ÿһ��Treat���ظ��������
  groups[i] <- substr(Treat[i],start = 1,stop=length(a[[i]])-2)#groups�еĵ�i��Ԫ�ؼ���i��Treat
  #�ĵ�1��������3���ַ�
}
groups[which(groups=="b")] <- "blk"
data$groups <- as.factor(groups)


library(plyr)
plot.dat <- 
ddply(data, .(Time,Species,groups), summarise,
      avg = mean(Cumulative.CO2.efflux),
      se = sd(Cumulative.CO2.efflux)/sqrt(length(Cumulative.CO2.efflux)))
library(ggplot2)
ggplot(plot.dat,aes(x=groups,y=avg))+
  geom_bar(stat="identity",fill="white",colour="black")+
  geom_errorbar(aes(ymin=avg-se,ymax=avg+se),width=.2)+
  geom_text(aes(label=c("a","a","ab","ab","ab","ab","ab","b")), vjust=1.5, colour="black")

##���Ӽ���ȥ��ĳЩ����
cbspH<-subset(plot.dat,Species!="FJQG" & Species != "GYHD")##һ��һ���г�����������c(),
##����ȥ������ȫ
cbs234.2 <- list()
treat.levels <- as.character(unique(cbs234$Treatment))#�ȶ���һ���±���
#�����ת����character������factor������ʱ���ܾ���123����ԭ����ֵ
## unique��ȡ���ֶ�ε����أ����ؽ��ÿ������ֻ����һ��,һ��Ҫ����unique()!
for(tr in treat.levels){#����treat.levels�е�ÿһ��treatment,"tr"����������
  tmpD <- subset(cbs234, Treatment==tr)#�ȸ�ֵһ���±�����ѡȡ�Ӽ�Ϊÿһ��tr��
  ##ע��Ҫ��"=="��ʾ�ж�����ֻ��һ��"="���ʾ��ֵ
  Cumu2 <- c()
  for(j in 1:NROW(tmpD)){#����ÿ��tmpD�ĵ�j��Ԫ�أ�ִ����������
    Cumu2[j] <- sum(tmpD$Cumulative.CO2.efflux[1:j])
  }
  tmpD$Cumulative.CO2.efflux2 <- Cumu2
  cbs234.2 <- rbind(cbs234.2,tmpD)
}
#��������ÿ�������ľ�ֵ�ͱ�׼��
cbs234.3<-
  ddply(cbs234.2,.(Time,species,Groups),summarise,
        CumulativeCO2respirtaion=mean(Cumulative.CO2.efflux2),
        se=sd(Cumulative.CO2.efflux2)/sqrt(length(Cumulative.CO2.efflux2)))
#ѡ�����Ϊ��ͼ��׼��
cl<-subset(cbs234.3,species=="Prunus padus L.")
cl$Time<-as.Date(cl$Time)#�Ȱ�Timeת��Ϊ���ڸ�ʽ����
ggplot(cl,aes(x=Time,y=CumulativeCO2respirtaion,colour=Groups))+
   geom_point()+
   geom_errorbar(aes(ymin=CumulativeCO2respirtaion-se,ymax=CumulativeCO2respirtaion+se),width=.2)+
  scale_x_date(date_labels ="%m-%d",date_breaks ="1 day",labels=date_format("%m-%d"),limits=as.Date(c("2018-09-29","2018-10-8")))+
  ggtitle("Prunus padus L.")+
  theme(plot.title = element_text(hjust = 0.5))