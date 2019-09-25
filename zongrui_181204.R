data <- read.csv(file.choose(),header=TRUE,stringsAsFactors = FALSE)
Treat[22] <- "H-G-CK-2"
a <-  strsplit(data$Treatment,split = "")#总共有48个Treatment~48个Treat
groups <- c()
for (i in 1:length(a)){#每一个Treat都重复以下语句
  groups[i] <- substr(Treat[i],start = 1,stop=length(a[[i]])-2)#groups中的第i个元素即第i个Treat
  #的第1到倒数第3个字符
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

##在子集中去除某些数据
cbspH<-subset(plot.dat,Species!="FJQG" & Species != "GYHD")##一个一个列出来，不能用c(),
##否则去除不完全
cbs234.2 <- list()
treat.levels <- as.character(unique(cbs234$Treatment))#先定义一个新变量
#如果不转换成character而保留factor，返回时可能就是123而非原本的值
## unique提取出现多次的因素，返回结果每个因素只出现一次,一定要加上unique()!
for(tr in treat.levels){#对于treat.levels中的每一个treatment,"tr"可随意命名
  tmpD <- subset(cbs234, Treatment==tr)#先赋值一个新变量，选取子集为每一个tr，
  ##注意要用"=="表示判定！若只有一个"="则表示赋值
  Cumu2 <- c()
  for(j in 1:NROW(tmpD)){#对于每个tmpD的第j个元素，执行以下运算
    Cumu2[j] <- sum(tmpD$Cumulative.CO2.efflux[1:j])
  }
  tmpD$Cumulative.CO2.efflux2 <- Cumu2
  cbs234.2 <- rbind(cbs234.2,tmpD)
}
#按日期求每个处理的均值和标准误
cbs234.3<-
  ddply(cbs234.2,.(Time,species,Groups),summarise,
        CumulativeCO2respirtaion=mean(Cumulative.CO2.efflux2),
        se=sd(Cumulative.CO2.efflux2)/sqrt(length(Cumulative.CO2.efflux2)))
#选出稠李，为画图做准备
cl<-subset(cbs234.3,species=="Prunus padus L.")
cl$Time<-as.Date(cl$Time)#先把Time转换为日期格式备用
ggplot(cl,aes(x=Time,y=CumulativeCO2respirtaion,colour=Groups))+
   geom_point()+
   geom_errorbar(aes(ymin=CumulativeCO2respirtaion-se,ymax=CumulativeCO2respirtaion+se),width=.2)+
  scale_x_date(date_labels ="%m-%d",date_breaks ="1 day",labels=date_format("%m-%d"),limits=as.Date(c("2018-09-29","2018-10-8")))+
  ggtitle("Prunus padus L.")+
  theme(plot.title = element_text(hjust = 0.5))
