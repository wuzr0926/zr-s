lm1<-read.csv(file.choose(),header=TRUE,stringsAsFactors = FALSE)#Book234new-两个样地
head(lm1)
cbs<-subset(lm1,Site=="CBS")
hsd<-subset(lm1,Site=="HSD")
#多元线性回归
Fitcbs<-lm(Rh~pH+N...+C...+C.N+DDON+DDOC+DMBC+DMBN,data=cbs)
summary(Fitcbs)
library(MASS)
Fit2cbs<-stepAIC(Fitcbs)
Fit2cbs<-lm(Rh ~ pH + N... + C... + C.N + DDOC + DMBN,data=cbs)
summary(Fit2cbs)#N%,C%,C:N均p<0.01
#检测多重共线性
library(car)
vif(Fit2cbs)
sqrt(vif(Fit2cbs))>2
#选出没有多重共线性的最佳模型
Fit3cbs<-lm(Rh ~ pH  + N... + DDOC + DMBN,data=cbs)
Fit4cbs<-lm(Rh ~ pH  + C... + DDOC + DMBN,data=cbs)
Fit5cbs<-lm(Rh ~ pH  + C.N + DDOC + DMBN,data=cbs)
summary(Fit3cbs)
summary(Fit4cbs)#最佳,R2=0.6133,p<0.01(C%**)
summary(Fit5cbs)
#相对重要性
relweights <- function(fit,...){  
  R <- cor(fit$model)  
  nvar <- ncol(R)  
  rxx <- R[2:nvar, 2:nvar]  
  rxy <- R[2:nvar, 1]  
  svd <- eigen(rxx)  
  evec <- svd$vectors  
  ev <- svd$values  
  delta <- diag(sqrt(ev))  
  lambda <- evec %*% delta %*% t(evec)  
  lambdasq <- lambda ^ 2  
  beta <- solve(lambda) %*% rxy  
  rsquare <- colSums(beta ^ 2)  
  rawwgt <- lambdasq %*% beta ^ 2  
  import <- (rawwgt / rsquare) * 100  
  lbls <- names(fit$model[2:nvar])  
  rownames(import) <- lbls  
  colnames(import) <- "Weights"  
  barplot(t(import),names.arg=lbls,  
          ylab="% of R-Square",  xlab="Predictor Variables",  
          main="Relative Importance of Predictor Variables", 
          sub=paste("R-Square=", 
          round(rsquare, digits=3)),  ...)  
  return(import)  
}
relweights(Fit4cbs)

#黑石顶
Fithsd<-lm(Rh~pH+N...+C...+C.N+DDON+DDOC+DMBC+DMBN,data=hsd)
summary(Fithsd)
library(MASS)
Fit2hsd<-stepAIC(Fithsd)
Fit2hsd<-lm(Rh ~ DDON,data=hsd)
summary(Fit2hsd)#结果不显著




##pH lm
fit1cbs<-lm(Rh~pH,data=cbs)
fit1hsd<-lm(Rh~pH,data=hsd)
summary(fit1cbs)#R2=0.018 p=0.55
summary(fit1hsd)#R2=0.009 p=0.66

##N% lm
fit2cbs<-lm(Rh~N...,data=cbs)
fit2hsd<-lm(Rh~N...,data=hsd)
summary(fit2cbs)#R2=0.577 p<0.01
summary(fit2hsd)#R2=0.020 p=0.50

## C% lm
fit3cbs<-lm(Rh~C...,data=cbs)
fit3hsd<-lm(Rh~C...,data=hsd)
summary(fit3cbs)#R2=0.567 p<0.01
summary(fit3hsd)#R2=0.002 p=0.84

##C:N lm
fit4cbs<-lm(Rh~C.N,data=cbs)
fit4hsd<-lm(Rh~C.N,data=hsd)
summary(fit4cbs)#R2=0.003 p=0.78
summary(fit4hsd)#R2=0.009 p=0.66


##DOC lm
fit5cbs<-lm(Rh~DDOC,data=cbs)
fit5hsd<-lm(Rh~DDOC,data=hsd)
summary(fit5cbs)#R2=0.024 p=0.48
summary(fit5hsd)#R2=0.019 p=0.52

##DON lm
fit6cbs<-lm(Rh~DDON,data=cbs)
fit6hsd<-lm(Rh~DDON,data=hsd)
summary(fit6cbs)#R2=0.005 p=0.74
summary(fit6hsd)#R2=0.092 p=0.15

##MBC lm
fit7cbs<-lm(Rh~DMBC,data=cbs)
fit7hsd<-lm(Rh~DMBC,data=hsd)
summary(fit7cbs)#R2=0.072 p=0.20
summary(fit7hsd)#R2=0.009 p=0.65

##MBN lm
fit8cbs<-lm(Rh~DMBN,data=cbs)
fit8hsd<-lm(Rh~DMBN,data=hsd)
summary(fit8cbs)#R2=0.121 p=0.09
summary(fit8hsd)#R2=0.077 p=0.19

##pH
a1<-ggplot(lm1,aes(x=pH,y=Rh,colour=Site))+
   geom_point()+
   geom_smooth(method=lm)+
  annotate("text", x=4.3, y=35, parse=TRUE,label="R^2==0.018",hjust=0) +
  annotate("text", x=4.3, y=32, parse=TRUE,label="p==0.55",hjust=0)+
  annotate("text", x=4.3, y=9, parse=TRUE,label="R^2==0.009",hjust=0) +
  annotate("text", x=4.3, y=6, parse=TRUE,label="p==0.66",hjust=0)
b1<-a1+ylab("Rh(μg C/ kg-1 soil/d)" )+theme_bw()##theme_bw() 去掉背景色

##N%
a2<-ggplot(lm1,aes(x=N...,y=Rh,colour=Site))+
  geom_point()+
  geom_smooth(method=lm)+
  annotate("text", x=0.5, y=40, parse=TRUE,label="R^2==0.577",hjust=0) +
  annotate("text", x=0.5, y=37, parse=TRUE,label="p<0.01",hjust=0)+
  annotate("text", x=0.25, y=9, parse=TRUE,label="R^2==0.020",hjust=0) +
  annotate("text", x=0.25, y=6, parse=TRUE,label="p==0.50",hjust=0)
b2<-a2+xlab("N%")+ylab("Rh(μg C/ kg-1 soil/d)" )+theme_bw()

##C%
a3<-ggplot(lm1,aes(x=C...,y=Rh,colour=Site))+
  geom_point()+
  geom_smooth(method=lm)+
  annotate("text", x=7.5, y=42, parse=TRUE,label="R^2==0.567",hjust=0) +
  annotate("text", x=7.5, y=39, parse=TRUE,label="p<0.01",hjust=0)+
  annotate("text", x=4.5, y=10, parse=TRUE,label="R^2==0.020",hjust=0) +
  annotate("text", x=4.5, y=7, parse=TRUE,label="p==0.50",hjust=0)
b3<-a3+xlab("C%")+ylab("Rh(μg C/ kg-1 soil/d)" )+theme_bw()

##C:N
a4<-ggplot(lm1,aes(x=C.N,y=Rh,colour=Site))+
  geom_point()+
  geom_smooth(method=lm)+
  annotate("text", x=13, y=35, parse=TRUE,label="R^2==0.003",hjust=0) +
  annotate("text", x=13, y=32, parse=TRUE,label="p==0.78",hjust=0)+
  annotate("text", x=11, y=9, parse=TRUE,label="R^2==0.009",hjust=0) +
  annotate("text", x=11, y=6, parse=TRUE,label="p==0.66",hjust=0)
b4<-a4+xlab("C:N")+ylab("Rh(μg C/ kg-1 soil/d)" )+theme_bw()

##DOC
a5<-ggplot(lm1,aes(x=DDOC,y=Rh,colour=Site))+
  geom_point()+
  geom_smooth(method=lm)+
  annotate("text", x=145, y=38, parse=TRUE,label="R^2==0.024",hjust=0) +
  annotate("text", x=145, y=35, parse=TRUE,label="p==0.48",hjust=0)+
  annotate("text", x=132, y=8, parse=TRUE,label="R^2==0.019",hjust=0) +
  annotate("text", x=132, y=5, parse=TRUE,label="p==0.52",hjust=0)
b5<-a5+xlab("DOC(mg/kg dry soil)")+ylab("Rh(μg C/ kg-1 soil/d)" )+theme_bw()

##DON
a6<-ggplot(lm1,aes(x=DDON,y=Rh,colour=Site))+
  geom_point()+
  geom_smooth(method=lm)+
  annotate("text", x=75, y=38, parse=TRUE,label="R^2==0.005",hjust=0) +
  annotate("text", x=75, y=35, parse=TRUE,label="p==0.74",hjust=0)+
  annotate("text", x=152, y=8, parse=TRUE,label="R^2==0.092",hjust=0) +
  annotate("text", x=152, y=5, parse=TRUE,label="p==0.15",hjust=0)
b6<-a6+xlab("DON(mg/kg dry soil)")+ylab("Rh(μg C/ kg-1 soil/d)" )+theme_bw()

##MBC
a7<-ggplot(lm1,aes(x=DMBC,y=Rh,colour=Site))+
  geom_point()+
  geom_smooth(method=lm)+
  annotate("text", x=300, y=43, parse=TRUE,label="R^2==0.072",hjust=0) +
  annotate("text", x=300, y=40, parse=TRUE,label="p==0.20",hjust=0)+
  annotate("text", x=400, y=8, parse=TRUE,label="R^2==0.009",hjust=0) +
  annotate("text", x=400, y=5, parse=TRUE,label="p==0.65",hjust=0)
b7<-a7+xlab("MBC(mg/kg dry soil)")+ylab("Rh(μg C/ kg-1 soil/d)" )+theme_bw()

##MBN
a8<-ggplot(lm1,aes(x=DMBN,y=Rh,colour=Site))+
  geom_point()+
  geom_smooth(method=lm)+
  annotate("text", x=50, y=38, parse=TRUE,label="R^2==0.121",hjust=0) +
  annotate("text", x=50, y=35, parse=TRUE,label="p==0.09",hjust=0)+
  annotate("text", x=125, y=9, parse=TRUE,label="R^2==0.077",hjust=0) +
  annotate("text", x=125, y=6, parse=TRUE,label="p==0.19",hjust=0)
b8<-a8+xlab("MBN(mg/kg dry soil)")+ylab("Rh(μg C/ kg-1 soil/d)" )+theme_bw()

##合并
library(ggpubr) 
d<-ggarrange(b1,b2,b3,b4,b5,b6,b7,b8,ncol=3,nrow=3,labels=c("A)","B)","C)","D)","E)","F)","G)","H)"))

##微生物
Rh.micro<-read.csv(file.choose(),header=TRUE,stringsAsFactors = FALSE)#Book234new-两个样地.csv
head(Rh.micro)
cbs.mi<-subset(Rh.micro,Site=="CBS")
hsd.mi<-subset(Rh.micro,Site=="HSD")
##细菌
##Acidobacteria lm
fit1cbs<-lm(Rh~Acidobacteria,data=cbs.mi)
fit1hsd<-lm(Rh~Acidobacteria,data=hsd.mi)
summary(fit1cbs)#R2=0.002 p=0.85
summary(fit1hsd)#R2=0.36 p<0.01

##Actinobacteria lm
fit2cbs<-lm(Rh~Actinobacteria,data=cbs.mi)
fit2hsd<-lm(Rh~Actinobacteria,data=hsd.mi)
summary(fit2cbs)#R2=0.025 p=0.46
summary(fit2hsd)#R2=0.14 p=0.07

##Bacteroidetes lm
fit3cbs<-lm(Rh~Bacteroidetes,data=cbs.mi)
fit3hsd<-lm(Rh~Bacteroidetes,data=hsd.mi)
summary(fit3cbs)#R2=0.037 p=0.37
summary(fit3hsd)#R2=0.015 p=0.56

##Planctomycetes lm
fit4cbs<-lm(Rh~Planctomycetes,data=cbs.mi)
fit4hsd<-lm(Rh~Planctomycetes,data=hsd.mi)
summary(fit4cbs)#R2=0.007 p=0.80
summary(fit4hsd)#R2=0.0004 p=0.95


##Proteobacteria lm
fit5cbs<-lm(Rh~Proteobacteria,data=cbs.mi)
fit5hsd<-lm(Rh~Proteobacteria,data=hsd.mi)
summary(fit5cbs)#R2=0.0005 p=0.92
summary(fit5hsd)#R2=0.008 p=0.68

##Verrucomicrobia lm
fit6cbs<-lm(Rh~Verrucomicrobia,data=cbs.mi)
fit6hsd<-lm(Rh~Verrucomicrobia,data=hsd.mi)
summary(fit6cbs)#R2<0.0001 p=0.98
summary(fit6hsd)#R2=0.087 p=0.16

##真菌


##Ascomycota lm
fit7cbs<-lm(Rh~Ascomycota,data=cbs.mi)
fit7hsd<-lm(Rh~Ascomycota,data=hsd.mi)
summary(fit7cbs)#R2=0.058 p=0.26
summary(fit7hsd)#R2=0.001 p=0.87

##Basidiomycota lm
fit8cbs<-lm(Rh~Basidiomycota,data=cbs.mi)
fit8hsd<-lm(Rh~Basidiomycota,data=hsd.mi)
summary(fit8cbs)#R2=0.007 p=0.70
summary(fit8hsd)#R2=0.053 p=0.28

##Mucoromycota lm
fit9cbs<-lm(Rh~Mucoromycota,data=cbs.mi)
fit9hsd<-lm(Rh~Mucoromycota,data=hsd.mi)
summary(fit9cbs)#R2=0.020 p=0.51
summary(fit9hsd)#R2=0.035 p=0.38

###线性回归图
##细菌
##Acidobacteria
library(ggplot2)
a1<-ggplot(Rh.micro,aes(x=Acidobacteria,y=Rh,colour=Site))+
  geom_point()+
  geom_smooth(method=lm)+
  annotate("text", x=0.125, y=33, parse=TRUE,label="R^2==0.002",hjust=0) +
  annotate("text", x=0.125, y=30, parse=TRUE,label="p==0.85",hjust=0)+
  annotate("text", x=0.075, y=14, parse=TRUE,label="R^2==0.360",hjust=0) +
  annotate("text", x=0.075, y=11, parse=TRUE,label="p<0.01",hjust=0)
b1<-a1+ylab("Rh(μg C/ kg-1 soil/d)" )+theme_bw()##theme_bw() 去掉背景色

##Actinobacteria
a2<-ggplot(Rh.micro,aes(x=Actinobacteria,y=Rh,colour=Site))+
  geom_point()+
  geom_smooth(method=lm)+
  annotate("text", x=0.35, y=38, parse=TRUE,label="R^2==0.025",hjust=0) +
  annotate("text", x=0.35, y=35, parse=TRUE,label="p==0.46",hjust=0)+
  annotate("text", x=0.3, y=14, parse=TRUE,label="R^2==0.14",hjust=0) +
  annotate("text", x=0.3, y=11, parse=TRUE,label="p==0.07",hjust=0)
b2<-a2+ylab("Rh(μg C/ kg-1 soil/d)" )+theme_bw()

##Bacteroidetes
a3<-ggplot(Rh.micro,aes(x=Bacteroidetes,y=Rh,colour=Site))+
  geom_point()+
  geom_smooth(method=lm)+
  annotate("text", x=0.017, y=33, parse=TRUE,label="R^2==0.037",hjust=0) +
  annotate("text", x=0.017, y=30, parse=TRUE,label="p==0.37",hjust=0)+
  annotate("text", x=0.02, y=13, parse=TRUE,label="R^2==0.015",hjust=0) +
  annotate("text", x=0.02, y=10, parse=TRUE,label="p==0.56",hjust=0)
b3<-a3+ylab("Rh(μg C/ kg-1 soil/d)" )+theme_bw()

##Planctomycetes
a4<-ggplot(Rh.micro,aes(x=Planctomycetes,y=Rh,colour=Site))+
  geom_point()+
  geom_smooth(method=lm)+
  annotate("text", x=0.025, y=30, parse=TRUE,label="R^2==0.007",hjust=0) +
  annotate("text", x=0.025, y=27, parse=TRUE,label="p==0.80",hjust=0)+
  annotate("text", x=0.04, y=13, parse=TRUE,label="R^2<0.001",hjust=0) +
  annotate("text", x=0.04, y=10, parse=TRUE,label="p==0.95",hjust=0)
b4<-a4+ylab("Rh(μg C/ kg-1 soil/d)" )+theme_bw()

##Proteobacteria
a5<-ggplot(Rh.micro,aes(x=Proteobacteria,y=Rh,colour=Site))+
  geom_point()+
  geom_smooth(method=lm)+
  annotate("text", x=0.15, y=38, parse=TRUE,label="R^2<0.001",hjust=0) +
  annotate("text", x=0.15, y=35, parse=TRUE,label="p==0.92",hjust=0)+
  annotate("text", x=0.45, y=13, parse=TRUE,label="R^2==0.008",hjust=0) +
  annotate("text", x=0.45, y=10, parse=TRUE,label="p==0.68",hjust=0)
b5<-a5+ylab("Rh(μg C/ kg-1 soil/d)" )+theme_bw()

##Verrucomicrobia
a6<-ggplot(Rh.micro,aes(x=Verrucomicrobia,y=Rh,colour=Site))+
  geom_point()+
  geom_smooth(method=lm)+
  annotate("text", x=0.1, y=40, parse=TRUE,label="R^2<0.001",hjust=0) +
  annotate("text", x=0.1, y=37, parse=TRUE,label="p==0.98",hjust=0)+
  annotate("text", x=0.05, y=8, parse=TRUE,label="R^2==0.087",hjust=0) +
  annotate("text", x=0.05, y=5, parse=TRUE,label="p==0.16",hjust=0)
b6<-a6+ylab("Rh(μg C/ kg-1 soil/d)" )+theme_bw()

##真菌
##Ascomycota
a7<-ggplot(Rh.micro,aes(x=Ascomycota,y=Rh,colour=Site))+
  geom_point()+
  geom_smooth(method=lm)+
  annotate("text", x=0.2, y=39, parse=TRUE,label="R^2==0.058",hjust=0) +
  annotate("text", x=0.2, y=36, parse=TRUE,label="p==0.26",hjust=0)+
  annotate("text", x=0.2, y=13, parse=TRUE,label="R^2==0.001",hjust=0) +
  annotate("text", x=0.2, y=10, parse=TRUE,label="p==0.87",hjust=0)
b7<-a7+ylab("Rh(μg C/ kg-1 soil/d)" )+theme_bw()

##Basidiomycota
a8<-ggplot(Rh.micro,aes(x=Basidiomycota,y=Rh,colour=Site))+
  geom_point()+
  geom_smooth(method=lm)+
  annotate("text", x=0.02, y=40, parse=TRUE,label="R^2==0.007",hjust=0) +
  annotate("text", x=0.02, y=37, parse=TRUE,label="p==0.70",hjust=0)+
  annotate("text", x=0.35, y=10, parse=TRUE,label="R^2==0.053",hjust=0) +
  annotate("text", x=0.35, y=7, parse=TRUE,label="p==0.28",hjust=0)
b8<-a8+ylab("Rh(μg C/ kg-1 soil/d)" )+theme_bw()

##Mucoromycota
a9<-ggplot(Rh.micro,aes(x=Mucoromycota,y=Rh,colour=Site))+
  geom_point()+
  geom_smooth(method=lm)+
  annotate("text", x=0.5, y=38, parse=TRUE,label="R^2==0.020",hjust=0) +
  annotate("text", x=0.5, y=35, parse=TRUE,label="p==0.51",hjust=0)+
  annotate("text", x=0.4, y=13, parse=TRUE,label="R^2==0.035",hjust=0) +
  annotate("text", x=0.4, y=10, parse=TRUE,label="p==0.38",hjust=0)
b9<-a9+ylab("Rh(μg C/ kg-1 soil/d)" )+theme_bw()
##合并
library(ggpubr) 
d<-ggarrange(b1,b2,b3,b4,b5,b6,b7,b8,b9,ncol=3,nrow=3,labels=c("A)","B)","C)","D)","E)","F)","G)","H)","I)"))


