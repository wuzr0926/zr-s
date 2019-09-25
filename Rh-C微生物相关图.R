###Rh-C微生物相关图
##长白山
cbsRs<-read.csv(file.choose(),header=TRUE,stringsAsFactors = FALSE)#Book234new-cbs不分树种.csv
head(cbsRs)
#细菌
cbs.b<-cbsRs[,c(5,14:20)]
library(corrplot)
cbs.bcor<-cor(cbs.b)
res1<-cor.mtest(cbs.b,conf.level=.95)
corrplot(cbs.bcor,p.mat=res1$p,sig.level=.05,type="upper")
#真菌
cbs.f<-cbsRs[,c(5,21:24)]
head(cbs.f)
cbs.fcor<-cor(cbs.f)
res1<-cor.mtest(cbs.f,conf.level=.95)
corrplot(cbs.fcor,p.mat=res1$p,sig.level=.05,type="upper")

##黑石顶
hsdRs<-read.csv(file.choose(),header=TRUE,stringsAsFactors = FALSE)#黑石顶第一次-34批呼吸汇总new-不分树种.csv
head(hsdRs)
#细菌
hsd.b<-hsdRs[,c(5,14:20)]
head(hsd.b)
library(corrplot)
hsd.bcor<-cor(hsd.b)
res1<-cor.mtest(hsd.b,conf.level=.95)
corrplot(hsd.bcor,p.mat=res1$p,sig.level=.05,type="upper")
#真菌
hsd.f<-hsdRs[,c(5,21:24)]
head(hsd.f)
hsd.fcor<-cor(hsd.f)
res1<-cor.mtest(hsd.f,conf.level=.95)
corrplot(hsd.fcor,p.mat=res1$p,sig.level=.05,type="upper")