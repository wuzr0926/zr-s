botu.phy<-read.csv(file.choose(),header=TRUE,stringsAsFactors = FALSE)
library(tidyr)
phylum2<-gather(botu.phy,key="Treatment",value="Relative.Abundance",-Phylum)
library(ggplot2)
ggplot(phylum2,aes(x=Treatment,y=Relative.Abundance,fill=Phylum))+
  geom_bar(stat="identity",position="stack")+
  scale_fill_manual(values=col)
fotu.phy<-read.csv(file.choose(),header=TRUE,stringsAsFactors = FALSE)#真菌-otu_taxon_Phylum.percent.full-不分树种.csv
phylum3<-gather(fotu.phy,key="Treatment",value="Relative.Abundance",-Phylum)
library(ggplot2)
ggplot(phylum3,aes(x=Treatment,y=Relative.Abundance,fill=Phylum))+
  geom_bar(stat="identity",position="stack")

##test3个重复画出来的丰度图是否有差别
aa<-read.csv(file.choose(),header=TRUE,stringsAsFactors = FALSE)#hsd不分树种-细菌门相对丰度.csv
head(aa)
##细菌
library(tidyr)
aa.2<-gather(aa,key="Phylum",value="Relative.Abundance",-Groups)
library(ggplot2)
library(RColorBrewer)
col<-c(brewer.pal(7,"Set1"))
ggplot(aa.2,aes(x=Groups,y=Relative.Abundance,fill=Phylum))+
  geom_bar(stat="identity",position="stack")+
  scale_fill_manual(values=col)
###!!!不行，纵轴变成了3（即3*100%）