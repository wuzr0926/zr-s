library(ggplot2)
ggplot(data, aes(x=Date, y=Daily average CO2 efflux, colour=, group=))+
  geom_point()+
  geom_line()
data1=read.csv(file = "data.csv")
Date<-as.Date(data1$Date)
data=subset(data1,Species!="Ormosia glaberrima")

ggplot(data, aes(x=Date, y=Average.treatment.efflux, group=Treatment,col=Treatment))+
  geom_line()+
  geom_point()+
  theme(axis.text = element_text(angle = 45,hjust=1))+
  theme(plot.title = element_text(hjust = 0.5))+
  ##显示所有日期
  scale_x_date(date_labels ="%m-%d",date_breaks ="1 day",labels=date_format("%m-%d"),limits=as.Date(c("2018-09-29","2018-10-16")))+
  ggtitle("Cyclobalanopsis chungii")

data=subset(data1,Species!="Cyclobalanopsis chungii")

ggplot(data, aes(x=Date, y=Average.treatment.efflux2, group=Treatment,col=Treatment))+
  geom_line()+
  geom_point()+
  ggtitle("Ormosia glaberrima")

#看两个样点pH有无显著差异
x<-c(3.85,4.02,4.06,3.93,3.99,3.96,4.10,4.04,4.00,3.93,4.01,3.90,4.13,4.13,4.24,4.07,4.10,4.13,4.23,4.25,4.17,4.23,3.99,4.08,4.91,5.17,5.07,4.71,5.06,NA,4.90,5.19,5.10,4.92,5.11,5.07,4.95,5.06,5.16,4.90,5.16,5.14,5.04,5.15,5.24,5.04,4.91,4.98)
a<-factor(c(rep(1,24),rep(2,24)))
bartlett.test(x~a)
#p>0.05-方差齐性
attach(pH)
t.test(pH1,pH2,paired=FALSE)#结果p<0.05,二者均值差异显著