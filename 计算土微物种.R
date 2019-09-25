fotu<-read.csv(file.choose(),header=TRUE,stringsAsFactors = FALSE)#真菌otu.csv
head(fotu)
#筛选出48个样品列
fotu.2<-fotu[,8:55]
head(fotu.2)
##计算总共有多少
#otu
F<-c()
for(i in 1:ncol(fotu.2)){
  F[i]<-length(which(fotu.2[,i]>0))
}#没有考虑species重复出现的情况,仅otu

#Species
FF<-c()
for(i in 1:ncol(fotu[,8:55])){
  FF[i]<-length(unique(fotu$Species[which(fotu[,i+7]>0)]))
}
##从最里面的括号开始往外解释
#i=1时实际选取的是第8列，依此类推
#which()选出非0的species
#unique()剔除掉重复出现的species，每种只记出现一次
#length()看每个样品下有多少种species

#两列结果合并，导出
FFF<-cbind(F,FF)
mode(FFF)#"numeric"
FFF<-as.data.frame(FFF)
names(FFF)<-c("OTU","Species")#改列名用names(),改行名用row.names()
write.csv(FFF,file.choose())#真菌种otu-计算后.csv

botu<-read.csv(file.choose(),header=TRUE,stringsAsFactors = FALSE)#细菌种otu.csv
head(botu)
botu.2<-botu[,3:50]
head(botu.2)
##计算总共有多少
#otu
B<-c()
for(i in 1:ncol(botu.2)){
  B[i]<-length(which(botu.2[,i]>0))
}#没有考虑species重复出现的情况,仅otu

#Species
BB<-c()
for(i in 1:ncol(botu[,3:50])){
  BB[i]<-length(unique(botu$Species[which(botu[,i+2]>0)]))
}
#两列结果合并，导出
BBB<-cbind(B,BB)
mode(BBB)#"numeric"
BBB<-as.data.frame(BBB)
names(BBB)<-c("OTU","Species")
write.csv(BBB,file.choose())#细菌种otu-计算后.csv

##碳分解相关
fotu.c<-read.csv(file.choose(),header=TRUE,stringsAsFactors = FALSE)#碳分解有关真菌otu.csv
head(fotu.c)
#筛选出48个样品列
fotu.c2<-fotu.c[,8:55]
head(fotu.c2)
##计算总共有多少
#otu
Fc<-c()
for(i in 1:ncol(fotu.c2)){
  Fc[i]<-length(which(fotu.c2[,i]>0))
}#没有考虑species重复出现的情况,仅otu

#Species
FFc<-c()
for(i in 1:ncol(fotu.c[,8:55])){
  FFc[i]<-length(unique(fotu.c$Species[which(fotu.c[,i+7]>0)]))
}

FFF.2<-cbind(Fc,FFc)
FFF.2<-as.data.frame(FFF.2)
names(FFF.2)<-c("OTU","Species")
write.csv(FFF.2,file.choose())#碳分解有关真菌种otu-计算后.csv

botu.c<-read.csv(file.choose(),header=TRUE,stringsAsFactors = FALSE)#碳分解有关细菌otu.csv
head(botu.c)
#筛选出48个样品列
botu.c2<-botu.c[,10:57]
head(botu.c2)
##计算总共有多少
#otu
Bc<-c()
for(i in 1:ncol(botu.c2)){
  Bc[i]<-length(which(botu.c2[,i]>0))
}#没有考虑species重复出现的情况,仅otu

#Species
BBc<-c()
for(i in 1:ncol(botu.c[,10:57])){
  BBc[i]<-length(unique(botu.c$Species[which(botu.c[,i+9]>0)]))
}

BBB.2<-cbind(Bc,BBc)
BBB.2<-as.data.frame(BBB.2)
names(BBB.2)<-c("OTU","Species")
write.csv(BBB.2,file.choose())#碳分解有关细菌种otu-计算后.csv