fb.aov<-read.csv(file.choose(),header=TRUE,stringsAsFactors = FALSE)#Rh-理化性质-微生物.csv
head(fb.aov)
cbs<-subset(fb.aov,Site=="CBS")
hsd<-subset(fb.aov,Site=="HSD")
##CBS
#细菌
aov1<-aov(Acidobacteria~T*F,data=cbs)
summary(aov1)
aov2<-aov(Actinobacteria~T*F,data=cbs)
summary(aov2)
aov3<-aov(Bacteroidetes~T*F,data=cbs)
summary(aov3)
aov4<-aov(Planctomycetes~T*F,data=cbs)
summary(aov4)#F-*,p=0.04
aov5<-aov(Proteobacteria~T*F,data=cbs)
summary(aov5)
aov6<-aov(Verrucomicrobia~T*F,data=cbs)
summary(aov6)#F-*,p=0.02
#真菌
aov7<-aov(Ascomycota~T*F,data=cbs)
summary(aov7)
aov8<-aov(Basidiomycota~T*F,data=cbs)
summary(aov8)
aov9<-aov(Mucoromycota~T*F,data=cbs)
summary(aov9)

##HSD
#细菌
aov1<-aov(Acidobacteria~T*F,data=hsd)
summary(aov1)#T+ p=0.08,F- p=0.09,
aov2<-aov(Actinobacteria~T*F,data=hsd)
summary(aov2)
aov3<-aov(Bacteroidetes~T*F,data=hsd)
summary(aov3)
aov4<-aov(Planctomycetes~T*F,data=hsd)
summary(aov4)
aov5<-aov(Proteobacteria~T*F,data=hsd)
summary(aov5)#T** p<0.01,F* p=0.02
aov6<-aov(Verrucomicrobia~T*F,data=hsd)
summary(aov6)
#真菌
aov7<-aov(Ascomycota~T*F,data=hsd)
summary(aov7)
aov8<-aov(Basidiomycota~T*F,data=hsd)
summary(aov8)#F- p<0.01
aov9<-aov(Mucoromycota~T*F,data=hsd)
summary(aov9)

