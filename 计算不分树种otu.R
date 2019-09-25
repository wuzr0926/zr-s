botu<-read.csv(file.choose(),header=TRUE,stringsAsFactors = FALSE)#碳分解有关细菌otu.csv

##CBS bacteria species
CA<-botu$X1+botu$X2+botu$X3+botu$X13+botu$X14+botu$X15
CB<-botu$X4+botu$X5+botu$X6+botu$X16+botu$X17+botu$X18
CC<-botu$X7+botu$X8+botu$X9+botu$X19+botu$X20+botu$X21
CD<-botu$X10+botu$X11+botu$X12+botu$X22+botu$X23+botu$X24

##HSD bacteria species
HA<-botu$X25+botu$X26+botu$X27+botu$X37+botu$X38+botu$X39
HB<-botu$X28+botu$X29+botu$X30+botu$X40+botu$X41+botu$X42
HC<-botu$X31+botu$X32+botu$X33+botu$X43+botu$X44+botu$X45
HD<-botu$X34+botu$X35+botu$X36+botu$X46+botu$X47+botu$X48

Merge<-data.frame(CA,CB,CC,CD,HA,HB,HC,HD)
botu2<-cbind(botu,Merge)
write.csv(botu2,file.choose())#碳分解有关细菌otu-不分树种.csv
botu2<-read.csv(file.choose(),header=TRUE,stringsAsFactors = FALSE)#碳分解有关细菌otu-不分树种.csv
ca<-botu2[which((botu2$CA>0)),]
caus<-length(unique(ca$Species))
cauo<-length(unique(ca$OTU))

cb<-botu2[which((botu2$CB>0)),]
cbus<-length(unique(cb$Species))
cbuo<-length(unique(cb$OTU))

cc<-botu2[which((botu2$CC>0)),]
ccus<-length(unique(cc$Species))
ccuo<-length(unique(cc$OTU))
ccus
ccuo

cd<-botu2[which((botu2$CD>0)),]
cdus<-length(unique(cd$Species))
cduo<-length(unique(cd$OTU))
cdus
cduo

ha<-botu2[which((botu2$HA>0)),]
haus<-length(unique(ha$Species))
hauo<-length(unique(ha$OTU))
haus
hauo

hb<-botu2[which((botu2$HB>0)),]
hbus<-length(unique(hb$Species))
hbuo<-length(unique(hb$OTU))
hbus
hbuo

hc<-botu2[which((botu2$HC>0)),]
hcus<-length(unique(hc$Species))
hcuo<-length(unique(hc$OTU))
hcus
hcuo

hd<-botu2[which((botu2$HD>0)),]
hdus<-length(unique(hd$Species))
hduo<-length(unique(hd$OTU))
hdus
hduo

fotu<-read.csv(file.choose(),header=TRUE,stringsAsFactors = FALSE)#碳分解有关真菌otu.csv
fotu.2<-fotu[,8:55]
fotu.2c<-fotu.2[,1:24]
fotu.2h<-fotu.2[,25:48]
##写循环计算不分树种的OTU
#TRUE
M<-list()
for( i in 1:ncol(fotu.2c[,1:12])){
  M[[i]]<-fotu.2c[,i]+fotu.2c[,i+12]
}#list()用来创建一个包含字符串，数字，向量和逻辑值的列表，list中每个元素都是用双括号指定的
#M[1]是M的第一个元素，但是M1是一个单独的变量名,所以应该用M[[1]]
merged.fotu.2c<-c(fotu.2c,M)#将列表合并，用c()
##简单版
M<-fotu.2c[,1:12]+fotu.2c[,13:24]
colnames(M) <- paste("M",1:12)
merged.fotu.2c <- cbind(fotu.2c, M)
write.csv()

merged.fotu.2c <- cbind(fotu.2c, M)

#FALSE
##M<-c()#c()是用来创建向量，每个M指的是向量里的每个元素
#for( i in 1:ncol(fotu.2c)){
 # M[i]<-fotu.2[,i]+fotu.2[,i+12]
#}

