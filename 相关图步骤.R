library(corrplot)
hsdt2cor<-cor(hsdt2)
res1<-cor.mtest(hsdt2,conf.level=.95)
corrplot(hsdt2cor,p.mat=res1$p,sig.level=.05,type="upper")

fjqg2<-subset(hsdnew,Species=="Cyclobalanopsis chungii")
fjqg3<-fjqg2[,c(6,8,21:27)]
fjqg3<-na.omit(fjqg3)
fjqgcor<-cor(fjqg3)
res2<-cor.mtest(fjqg3,conf.level=.95)
corrplot(fjqgcor,p.mat=res2$p,sig.level=.05,type="upper")

gyhd2<-subset(hsdnew,Species=="Ormosia glaberrima")
gyhd3<-gyhd2[,c(6,8,21:27)]
gyhd3<-na.omit(gyhd3)
gyhdcor<-cor(gyhd3)
res3<-cor.mtest(gyhd3,conf.level=.95)
corrplot(gyhdcor,p.mat=res3$p,sig.level=.05,type="upper")