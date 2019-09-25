AOV<-read.csv(file.choose(),header=TRUE,stringsAsFactors = TRUE)#Book234new-两个样地.csv
head(AOV)
cbs<-subset(AOV,Site=="CBS")
cbs<-na.omit(cbs)
hsd<-subset(AOV,Site=="HSD")
hsd<-na.omit(hsd)
###HSD
library(agricolae)
modelcbs<-aov(pH~Treatment,data=cbs)
outcbs<-LSD.test(modelcbs,"Treatment",p.adj="none")
outcbs
##加载gglert里面所需字体
install.packages("showtext")
library(showtext)
font_files()#查看可用字体列表
showtext_auto(enable = TRUE)
font_add("Arial", "arial.TTF")
##PH
gglert <-function(data,
                  X,
                  Y,
                  main = NULL,
                  xLab = NULL,
                  yLab = NULL,
                  bcol = "bisque",
                  p.adj = "none",
                  cexy = 1,
                  varwidth = TRUE,
                  las = 1,
                  paired = FALSE){
  library(ggplot2)
  #names(mydata)=c("Group","count","color")
  Y = factor(Y,levels =  unique(Y))
  #color = unique(as.vector(mydata$color))
  #color=(palette(rainbow(100)))[1:length(unique(Y))]
  color=heat.colors(length(unique(Y)))
  
  p<-ggplot(data=data,aes(Y,X,aes(Y,X,fill=unique(Y))))+
    geom_boxplot(fill=color)+theme(axis.text.x=element_text(angle=45,hjust=1))+labs(x=xLab,y=yLab)+#
    theme_light()#+
  #stat_summary(geom = 'text', label = labdata$groups, fun.y = max , vjust = -0.5
  require(agricolae)
  aa <- levels(factor(Y, levels=unique(Y)))#levels(as.factor(Y))
  #an <- as.character(c(1:length(aa)))
  
  tt1 <- matrix(nrow = length(aa), ncol = 7)
  for (i in 1:length(aa)) {
    temp <- X[Y == aa[i]]
    tt1[i, 1] <- mean(temp, na.rm = TRUE)
    tt1[i, 2] <- sd(temp, na.rm = TRUE)/sqrt(length(temp))
    tt1[i, 3] <- sd(temp, na.rm = TRUE)
    tt1[i, 4] <- min(temp, na.rm = TRUE)
    tt1[i, 5] <- max(temp, na.rm = TRUE)
    tt1[i, 6] <- median(temp, na.rm = TRUE)
    tt1[i, 7] <- length(temp)
  }
  tt1 <- as.data.frame(tt1)
  row.names(tt1) <- aa
  colnames(tt1) <- c("mean", "se", "sd", "min", "max", "median", "n")
  Yn <- factor(Y)
  model <- aov(X ~ Yn)
  if (paired == TRUE & length(aa) == 2)
  {
    coms <- t.test(X ~ Yn, paired = TRUE)
    pp <- coms$p.value
  }    else
  {
    pp <- anova(model)$Pr[1]
  }    
  
  if (pp != 0) {#原函数在p值不显著时，不标显著性字母，所以要改
    comp <- LSD.test(model,
                     "Yn",
                     alpha = 0.05,
                     p.adj = p.adj,
                     group = TRUE)
    labdata<- comp$groups
    labdata$name<-row.names(labdata)
    labdata<-labdata[match(unique(Y),labdata$name),]
    gror <- comp$groups[order(rownames(comp$groups)), ]
    tt1$group <- gror$groups
    
    p=p+stat_summary(geom = 'text', label = labdata$groups, fun.y = max , vjust = -0.5)
  }
  p=p+annotate("text",x=-Inf,y=4.5,hjust=-0.5,vjust=max(X)*0.05,label= paste("p=",sprintf("%.3f",pp),sep = ""),fontface = "italic",family = "Arial",size = 4)
  #调显示p值的横纵坐标
  list(comparison = tt1, p.value = pp,plot=p)
  
}


shapiro.test(resid(aov(hsd$pH~hsd$Treatment)))
b1<-gglert(hsd,
           hsd$pH,
           hsd$Treatment,
       yLab="pH",
       xLab="Treatment",
       bcol="bisque",
       p.adj="holm",
       las=1)

##N%
library(showtext)
showtext_auto(enable = TRUE)
font_add("Arial", "arial.TTF")
gglert <-function(data,
                  X,
                  Y,
                  main = NULL,
                  xLab = NULL,
                  yLab = NULL,
                  bcol = "bisque",
                  p.adj = "none",
                  cexy = 1,
                  varwidth = TRUE,
                  las = 1,
                  paired = FALSE){
  library(ggplot2)
  #names(mydata)=c("Group","count","color")
  Y = factor(Y,levels =  unique(Y))
  #color = unique(as.vector(mydata$color))
  #color=(palette(rainbow(100)))[1:length(unique(Y))]
  color=heat.colors(length(unique(Y)))
  
  p<-ggplot(data=data,aes(Y,X,aes(Y,X,fill=unique(Y))))+
    geom_boxplot(fill=color)+theme(axis.text.x=element_text(angle=45,hjust=1))+labs(x=xLab,y=yLab)+#
    theme_light()#+
  #stat_summary(geom = 'text', label = labdata$groups, fun.y = max , vjust = -0.5
  require(agricolae)
  aa <- levels(factor(Y, levels=unique(Y)))#levels(as.factor(Y))
  #an <- as.character(c(1:length(aa)))
  
  tt1 <- matrix(nrow = length(aa), ncol = 7)
  for (i in 1:length(aa)) {
    temp <- X[Y == aa[i]]
    tt1[i, 1] <- mean(temp, na.rm = TRUE)
    tt1[i, 2] <- sd(temp, na.rm = TRUE)/sqrt(length(temp))
    tt1[i, 3] <- sd(temp, na.rm = TRUE)
    tt1[i, 4] <- min(temp, na.rm = TRUE)
    tt1[i, 5] <- max(temp, na.rm = TRUE)
    tt1[i, 6] <- median(temp, na.rm = TRUE)
    tt1[i, 7] <- length(temp)
  }
  tt1 <- as.data.frame(tt1)
  row.names(tt1) <- aa
  colnames(tt1) <- c("mean", "se", "sd", "min", "max", "median", "n")
  Yn <- factor(Y)
  model <- aov(X ~ Yn)
  if (paired == TRUE & length(aa) == 2)
  {
    coms <- t.test(X ~ Yn, paired = TRUE)
    pp <- coms$p.value
  }    else
  {
    pp <- anova(model)$Pr[1]
  }    
  
  if (pp != 0) {#原函数在p值不显著时，不标显著性字母，所以要改
    comp <- LSD.test(model,
                     "Yn",
                     alpha = 0.05,
                     p.adj = p.adj,
                     group = TRUE)
    labdata<- comp$groups
    labdata$name<-row.names(labdata)
    labdata<-labdata[match(unique(Y),labdata$name),]
    gror <- comp$groups[order(rownames(comp$groups)), ]
    tt1$group <- gror$groups
    
    p=p+stat_summary(geom = 'text', label = labdata$groups, fun.y = max , vjust = -0.5)
  }
  p=p+annotate("text",x=-Inf,y=0.25,hjust=-0.5,vjust=max(X)*0.05,label= paste("p=",sprintf("%.3f",pp),sep = ""),fontface = "italic",family = "Arial",size = 4)
  #调显示p值的横纵坐标
  list(comparison = tt1, p.value = pp,plot=p)
  
}
shapiro.test(resid(aov(hsd$N...~hsd$Treatment)))
b2<-gglert(hsd,
           hsd$N...,
           hsd$Treatment,
       yLab="N%",
       xLab="Treatment",
       bcol="bisque",
       p.adj="holm",
       las=1)

##C%
library(showtext)
font_add("Arial", "arial.TTF")
showtext_auto(enable = TRUE)
gglert <-function(data,
                  X,
                  Y,
                  main = NULL,
                  xLab = NULL,
                  yLab = NULL,
                  bcol = "bisque",
                  p.adj = "none",
                  cexy = 1,
                  varwidth = TRUE,
                  las = 1,
                  paired = FALSE){
  library(ggplot2)
  #names(mydata)=c("Group","count","color")
  Y = factor(Y,levels =  unique(Y))
  #color = unique(as.vector(mydata$color))
  #color=(palette(rainbow(100)))[1:length(unique(Y))]
  color=heat.colors(length(unique(Y)))
  
  p<-ggplot(data=data,aes(Y,X,aes(Y,X,fill=unique(Y))))+
    geom_boxplot(fill=color)+theme(axis.text.x=element_text(angle=45,hjust=1))+labs(x=xLab,y=yLab)+#
    theme_light()#+
  #stat_summary(geom = 'text', label = labdata$groups, fun.y = max , vjust = -0.5
  require(agricolae)
  aa <- levels(factor(Y, levels=unique(Y)))#levels(as.factor(Y))
  #an <- as.character(c(1:length(aa)))
  
  tt1 <- matrix(nrow = length(aa), ncol = 7)
  for (i in 1:length(aa)) {
    temp <- X[Y == aa[i]]
    tt1[i, 1] <- mean(temp, na.rm = TRUE)
    tt1[i, 2] <- sd(temp, na.rm = TRUE)/sqrt(length(temp))
    tt1[i, 3] <- sd(temp, na.rm = TRUE)
    tt1[i, 4] <- min(temp, na.rm = TRUE)
    tt1[i, 5] <- max(temp, na.rm = TRUE)
    tt1[i, 6] <- median(temp, na.rm = TRUE)
    tt1[i, 7] <- length(temp)
  }
  tt1 <- as.data.frame(tt1)
  row.names(tt1) <- aa
  colnames(tt1) <- c("mean", "se", "sd", "min", "max", "median", "n")
  Yn <- factor(Y)
  model <- aov(X ~ Yn)
  if (paired == TRUE & length(aa) == 2)
  {
    coms <- t.test(X ~ Yn, paired = TRUE)
    pp <- coms$p.value
  }    else
  {
    pp <- anova(model)$Pr[1]
  }    
  
  if (pp != 0) {#原函数在p值不显著时，不标显著性字母，所以要改
    comp <- LSD.test(model,
                     "Yn",
                     alpha = 0.05,
                     p.adj = p.adj,
                     group = TRUE)
    labdata<- comp$groups
    labdata$name<-row.names(labdata)
    labdata<-labdata[match(unique(Y),labdata$name),]
    gror <- comp$groups[order(rownames(comp$groups)), ]
    tt1$group <- gror$groups
    
    p=p+stat_summary(geom = 'text', label = labdata$groups, fun.y = max , vjust = -0.5)
  }
  p=p+annotate("text",x=-Inf,y=5,hjust=-0.5,vjust=max(X)*0.05,label= paste("p=",sprintf("%.3f",pp),sep = ""),fontface = "italic",family = "Arial",size = 4)
  #调显示p值的横纵坐标
  list(comparison = tt1, p.value = pp,plot=p)
  
}
shapiro.test(resid(aov(hsd$C...~hsd$Treatment)))
b3<-gglert(hsd,
           hsd$C...,
           hsd$Treatment,
       yLab="C%",
       xLab="Treatment",
       bcol="bisque",
       p.adj="holm",
       las=1)

##C:N
library(showtext)
showtext_auto(enable = TRUE)
font_add("Arial", "arial.TTF")
gglert <-function(data,
                  X,
                  Y,
                  main = NULL,
                  xLab = NULL,
                  yLab = NULL,
                  bcol = "bisque",
                  p.adj = "none",
                  cexy = 1,
                  varwidth = TRUE,
                  las = 1,
                  paired = FALSE){
  library(ggplot2)
  #names(mydata)=c("Group","count","color")
  Y = factor(Y,levels =  unique(Y))
  #color = unique(as.vector(mydata$color))
  #color=(palette(rainbow(100)))[1:length(unique(Y))]
  color=heat.colors(length(unique(Y)))
  
  p<-ggplot(data=data,aes(Y,X,aes(Y,X,fill=unique(Y))))+
    geom_boxplot(fill=color)+theme(axis.text.x=element_text(angle=45,hjust=1))+labs(x=xLab,y=yLab)+#
    theme_light()#+
  #stat_summary(geom = 'text', label = labdata$groups, fun.y = max , vjust = -0.5
  require(agricolae)
  aa <- levels(factor(Y, levels=unique(Y)))#levels(as.factor(Y))
  #an <- as.character(c(1:length(aa)))
  
  tt1 <- matrix(nrow = length(aa), ncol = 7)
  for (i in 1:length(aa)) {
    temp <- X[Y == aa[i]]
    tt1[i, 1] <- mean(temp, na.rm = TRUE)
    tt1[i, 2] <- sd(temp, na.rm = TRUE)/sqrt(length(temp))
    tt1[i, 3] <- sd(temp, na.rm = TRUE)
    tt1[i, 4] <- min(temp, na.rm = TRUE)
    tt1[i, 5] <- max(temp, na.rm = TRUE)
    tt1[i, 6] <- median(temp, na.rm = TRUE)
    tt1[i, 7] <- length(temp)
  }
  tt1 <- as.data.frame(tt1)
  row.names(tt1) <- aa
  colnames(tt1) <- c("mean", "se", "sd", "min", "max", "median", "n")
  Yn <- factor(Y)
  model <- aov(X ~ Yn)
  if (paired == TRUE & length(aa) == 2)
  {
    coms <- t.test(X ~ Yn, paired = TRUE)
    pp <- coms$p.value
  }    else
  {
    pp <- anova(model)$Pr[1]
  }    
  
  if (pp != 0) {#原函数在p值不显著时，不标显著性字母，所以要改
    comp <- LSD.test(model,
                     "Yn",
                     alpha = 0.05,
                     p.adj = p.adj,
                     group = TRUE)
    labdata<- comp$groups
    labdata$name<-row.names(labdata)
    labdata<-labdata[match(unique(Y),labdata$name),]
    gror <- comp$groups[order(rownames(comp$groups)), ]
    tt1$group <- gror$groups
    
    p=p+stat_summary(geom = 'text', label = labdata$groups, fun.y = max , vjust = -0.5)
  }
  p=p+annotate("text",x=-Inf,y=20,hjust=-0.5,vjust=max(X)*0.05,label= paste("p=",sprintf("%.3f",pp),sep = ""),fontface = "italic",family = "Arial",size = 4)
  #调显示p值的横纵坐标
  list(comparison = tt1, p.value = pp,plot=p)
  
}
shapiro.test(resid(aov(hsd$C.N~hsd$Treatment)))
b4<-gglert(hsd,
           hsd$C.N,
           hsd$Treatment,
       yLab="C:N",
       xLab="Treatment",
       bcol="bisque",
       p.adj="holm",
       las=1)

##DDOC
library(showtext)
showtext_auto(enable = TRUE)
font_add("Arial", "arial.TTF")
gglert <-function(data,
                  X,
                  Y,
                  main = NULL,
                  xLab = NULL,
                  yLab = NULL,
                  bcol = "bisque",
                  p.adj = "none",
                  cexy = 1,
                  varwidth = TRUE,
                  las = 1,
                  paired = FALSE){
  library(ggplot2)
  #names(mydata)=c("Group","count","color")
  Y = factor(Y,levels =  unique(Y))
  #color = unique(as.vector(mydata$color))
  #color=(palette(rainbow(100)))[1:length(unique(Y))]
  color=heat.colors(length(unique(Y)))
  
  p<-ggplot(data=data,aes(Y,X,aes(Y,X,fill=unique(Y))))+
    geom_boxplot(fill=color)+theme(axis.text.x=element_text(angle=45,hjust=1))+labs(x=xLab,y=yLab)+#
    theme_light()#+
  #stat_summary(geom = 'text', label = labdata$groups, fun.y = max , vjust = -0.5
  require(agricolae)
  aa <- levels(factor(Y, levels=unique(Y)))#levels(as.factor(Y))
  #an <- as.character(c(1:length(aa)))
  
  tt1 <- matrix(nrow = length(aa), ncol = 7)
  for (i in 1:length(aa)) {
    temp <- X[Y == aa[i]]
    tt1[i, 1] <- mean(temp, na.rm = TRUE)
    tt1[i, 2] <- sd(temp, na.rm = TRUE)/sqrt(length(temp))
    tt1[i, 3] <- sd(temp, na.rm = TRUE)
    tt1[i, 4] <- min(temp, na.rm = TRUE)
    tt1[i, 5] <- max(temp, na.rm = TRUE)
    tt1[i, 6] <- median(temp, na.rm = TRUE)
    tt1[i, 7] <- length(temp)
  }
  tt1 <- as.data.frame(tt1)
  row.names(tt1) <- aa
  colnames(tt1) <- c("mean", "se", "sd", "min", "max", "median", "n")
  Yn <- factor(Y)
  model <- aov(X ~ Yn)
  if (paired == TRUE & length(aa) == 2)
  {
    coms <- t.test(X ~ Yn, paired = TRUE)
    pp <- coms$p.value
  }    else
  {
    pp <- anova(model)$Pr[1]
  }    
  
  if (pp != 0) {#原函数在p值不显著时，不标显著性字母，所以要改
    comp <- LSD.test(model,
                     "Yn",
                     alpha = 0.05,
                     p.adj = p.adj,
                     group = TRUE)
    labdata<- comp$groups
    labdata$name<-row.names(labdata)
    labdata<-labdata[match(unique(Y),labdata$name),]
    gror <- comp$groups[order(rownames(comp$groups)), ]
    tt1$group <- gror$groups
    
    p=p+stat_summary(geom = 'text', label = labdata$groups, fun.y = max , vjust = -0.5)
  }
  p=p+annotate("text",x=-Inf,y=150,hjust=-0.5,label= paste("p=",sprintf("%.3f",pp),sep = ""),fontface = "italic",family = "Arial",size = 4)
  #调显示p值的横纵坐标
  list(comparison = tt1, p.value = pp,plot=p)
  
}
shapiro.test(resid(aov(hsd$DDOC~hsd$Treatment)))
b5<-gglert(hsd,
           hsd$DDOC,
           hsd$Treatment,
       yLab="DOC(mg/Kg dry soil)",
       xLab="Treatment",
       bcol="bisque",
       p.adj="holm",
       las=1)

##DDON
library(showtext)
showtext_auto(enable = TRUE)
font_add("Arial", "arial.TTF")
gglert <-function(data,
                  X,
                  Y,
                  main = NULL,
                  xLab = NULL,
                  yLab = NULL,
                  bcol = "bisque",
                  p.adj = "none",
                  cexy = 1,
                  varwidth = TRUE,
                  las = 1,
                  paired = FALSE){
  library(ggplot2)
  #names(mydata)=c("Group","count","color")
  Y = factor(Y,levels =  unique(Y))
  #color = unique(as.vector(mydata$color))
  #color=(palette(rainbow(100)))[1:length(unique(Y))]
  color=heat.colors(length(unique(Y)))
  
  p<-ggplot(data=data,aes(Y,X,aes(Y,X,fill=unique(Y))))+
    geom_boxplot(fill=color)+theme(axis.text.x=element_text(angle=45,hjust=1))+labs(x=xLab,y=yLab)+#
    theme_light()#+
  #stat_summary(geom = 'text', label = labdata$groups, fun.y = max , vjust = -0.5
  require(agricolae)
  aa <- levels(factor(Y, levels=unique(Y)))#levels(as.factor(Y))
  #an <- as.character(c(1:length(aa)))
  
  tt1 <- matrix(nrow = length(aa), ncol = 7)
  for (i in 1:length(aa)) {
    temp <- X[Y == aa[i]]
    tt1[i, 1] <- mean(temp, na.rm = TRUE)
    tt1[i, 2] <- sd(temp, na.rm = TRUE)/sqrt(length(temp))
    tt1[i, 3] <- sd(temp, na.rm = TRUE)
    tt1[i, 4] <- min(temp, na.rm = TRUE)
    tt1[i, 5] <- max(temp, na.rm = TRUE)
    tt1[i, 6] <- median(temp, na.rm = TRUE)
    tt1[i, 7] <- length(temp)
  }
  tt1 <- as.data.frame(tt1)
  row.names(tt1) <- aa
  colnames(tt1) <- c("mean", "se", "sd", "min", "max", "median", "n")
  Yn <- factor(Y)
  model <- aov(X ~ Yn)
  if (paired == TRUE & length(aa) == 2)
  {
    coms <- t.test(X ~ Yn, paired = TRUE)
    pp <- coms$p.value
  }    else
  {
    pp <- anova(model)$Pr[1]
  }    
  
  if (pp != 0) {#原函数在p值不显著时，不标显著性字母，所以要改
    comp <- LSD.test(model,
                     "Yn",
                     alpha = 0.05,
                     p.adj = p.adj,
                     group = TRUE)
    labdata<- comp$groups
    labdata$name<-row.names(labdata)
    labdata<-labdata[match(unique(Y),labdata$name),]
    gror <- comp$groups[order(rownames(comp$groups)), ]
    tt1$group <- gror$groups
    
    p=p+stat_summary(geom = 'text', label = labdata$groups, fun.y = max , vjust = -0.5)
  }
  p=p+annotate("text",x=-Inf,y=175,hjust=-0.5,label= paste("p=",sprintf("%.3f",pp),sep = ""),fontface = "italic",family = "Arial",size = 4)
  #调显示p值的横纵坐标
  list(comparison = tt1, p.value = pp,plot=p)
  
}
shapiro.test(resid(aov(hsd$DDON~hsd$Treatment)))
b6<-gglert(hsd,
           hsd$DDON,
           hsd$Treatment,
       yLab="DON(mg/Kg dry soil)",
       xLab="Treatment",
       bcol="bisque",
       p.adj="holm",
       las=1)

##DMBC
library(showtext)
showtext_auto(enable = TRUE)
font_add("Arial", "arial.TTF")
gglert <-function(data,
                  X,
                  Y,
                  main = NULL,
                  xLab = NULL,
                  yLab = NULL,
                  bcol = "bisque",
                  p.adj = "none",
                  cexy = 1,
                  varwidth = TRUE,
                  las = 1,
                  paired = FALSE){
  library(ggplot2)
  #names(mydata)=c("Group","count","color")
  Y = factor(Y,levels =  unique(Y))
  #color = unique(as.vector(mydata$color))
  #color=(palette(rainbow(100)))[1:length(unique(Y))]
  color=heat.colors(length(unique(Y)))
  
  p<-ggplot(data=data,aes(Y,X,aes(Y,X,fill=unique(Y))))+
    geom_boxplot(fill=color)+theme(axis.text.x=element_text(angle=45,hjust=1))+labs(x=xLab,y=yLab)+#
    theme_light()#+
  #stat_summary(geom = 'text', label = labdata$groups, fun.y = max , vjust = -0.5
  require(agricolae)
  aa <- levels(factor(Y, levels=unique(Y)))#levels(as.factor(Y))
  #an <- as.character(c(1:length(aa)))
  
  tt1 <- matrix(nrow = length(aa), ncol = 7)
  for (i in 1:length(aa)) {
    temp <- X[Y == aa[i]]
    tt1[i, 1] <- mean(temp, na.rm = TRUE)
    tt1[i, 2] <- sd(temp, na.rm = TRUE)/sqrt(length(temp))
    tt1[i, 3] <- sd(temp, na.rm = TRUE)
    tt1[i, 4] <- min(temp, na.rm = TRUE)
    tt1[i, 5] <- max(temp, na.rm = TRUE)
    tt1[i, 6] <- median(temp, na.rm = TRUE)
    tt1[i, 7] <- length(temp)
  }
  tt1 <- as.data.frame(tt1)
  row.names(tt1) <- aa
  colnames(tt1) <- c("mean", "se", "sd", "min", "max", "median", "n")
  Yn <- factor(Y)
  model <- aov(X ~ Yn)
  if (paired == TRUE & length(aa) == 2)
  {
    coms <- t.test(X ~ Yn, paired = TRUE)
    pp <- coms$p.value
  }    else
  {
    pp <- anova(model)$Pr[1]
  }    
  
  if (pp != 0) {#原函数在p值不显著时，不标显著性字母，所以要改
    comp <- LSD.test(model,
                     "Yn",
                     alpha = 0.05,
                     p.adj = p.adj,
                     group = TRUE)
    labdata<- comp$groups
    labdata$name<-row.names(labdata)
    labdata<-labdata[match(unique(Y),labdata$name),]
    gror <- comp$groups[order(rownames(comp$groups)), ]
    tt1$group <- gror$groups
    
    p=p+stat_summary(geom = 'text', label = labdata$groups, fun.y = max , vjust = -0.5)
  }
  p=p+annotate("text",x=-Inf,y=400,hjust=-0.5,label= paste("p=",sprintf("%.3f",pp),sep = ""),fontface = "italic",family = "Arial",size = 4)
  #调显示p值的横纵坐标
  list(comparison = tt1, p.value = pp,plot=p)
  
}
shapiro.test(resid(aov(hsd$DMBC~hsd$Treatment)))
b7<-gglert(hsd,
           hsd$DMBC,
           hsd$Treatment,
       yLab="MBC(mg/Kg dry soil)",
       xLab="Treatment",
       bcol="bisque",
       p.adj="holm",
       las=1)

##DMBN
library(showtext)
showtext_auto(enable = TRUE)
font_add("Arial", "arial.TTF")
gglert <-function(data,
                  X,
                  Y,
                  main = NULL,
                  xLab = NULL,
                  yLab = NULL,
                  bcol = "bisque",
                  p.adj = "none",
                  cexy = 1,
                  varwidth = TRUE,
                  las = 1,
                  paired = FALSE){
  library(ggplot2)
  #names(mydata)=c("Group","count","color")
  Y = factor(Y,levels =  unique(Y))
  #color = unique(as.vector(mydata$color))
  #color=(palette(rainbow(100)))[1:length(unique(Y))]
  color=heat.colors(length(unique(Y)))
  
  
  p<-ggplot(data=data,aes(Y,X,aes(Y,X,fill=unique(Y))))+
    geom_boxplot(fill=color)+theme(axis.text.x=element_text(angle=45,hjust=1))+labs(x=xLab,y=yLab)+#
    theme_light()#+
  #stat_summary(geom = 'text', label = labdata$groups, fun.y = max , vjust = -0.5
  require(agricolae)
  aa <- levels(factor(Y, levels=unique(Y)))#levels(as.factor(Y))
  #an <- as.character(c(1:length(aa)))
  
  tt1 <- matrix(nrow = length(aa), ncol = 7)
  for (i in 1:length(aa)) {
    temp <- X[Y == aa[i]]
    tt1[i, 1] <- mean(temp, na.rm = TRUE)
    tt1[i, 2] <- sd(temp, na.rm = TRUE)/sqrt(length(temp))
    tt1[i, 3] <- sd(temp, na.rm = TRUE)
    tt1[i, 4] <- min(temp, na.rm = TRUE)
    tt1[i, 5] <- max(temp, na.rm = TRUE)
    tt1[i, 6] <- median(temp, na.rm = TRUE)
    tt1[i, 7] <- length(temp)
  }
  tt1 <- as.data.frame(tt1)
  row.names(tt1) <- aa
  colnames(tt1) <- c("mean", "se", "sd", "min", "max", "median", "n")
  Yn <- factor(Y)
  model <- aov(X ~ Yn)
  if (paired == TRUE & length(aa) == 2)
  {
    coms <- t.test(X ~ Yn, paired = TRUE)
    pp <- coms$p.value
  }    else
  {
    pp <- anova(model)$Pr[1]
  }    
  
  if (pp != 0) {#原函数在p值不显著时，不标显著性字母，所以要改
    comp <- LSD.test(model,
                     "Yn",
                     alpha = 0.05,
                     p.adj = p.adj,
                     group = TRUE)
    labdata<- comp$groups
    labdata$name<-row.names(labdata)
    labdata<-labdata[match(unique(Y),labdata$name),]
    gror <- comp$groups[order(rownames(comp$groups)), ]
    tt1$group <- gror$groups
    
    p=p+stat_summary(geom = 'text', label = labdata$groups, fun.y = max , vjust = -0.5)
  }
  p=p+annotate("text",x=-Inf,y=150,hjust=-0.5,label= paste("p=",sprintf("%.3f",pp),sep = ""),fontface = "italic",family = "Arial",size = 4)
  #调显示p值的横纵坐标
  list(comparison = tt1, p.value = pp,plot=p)
  
}
shapiro.test(resid(aov(hsd$DMBN~hsd$Treatment)))
b8<-gglert(hsd,
           hsd$DMBN,
           hsd$Treatment,
       yLab="MBN(mg/Kg dry soil)",
       xLab="Treatment",
       bcol="bisque",
       p.adj="holm",
       las=1)

##合并
library(ggpubr) 
d<-ggarrange(b1$plot,b2$plot,b3$plot,b4$plot,b5$plot,b6$plot,b7$plot,b8$plot,ncol=3,nrow=3,labels=c("A)","B)","C)","D)","E)","F)","G)","H)"))

###HSD


