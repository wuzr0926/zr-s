
for(tre in 1:length(treat.levels)){#对于treat.levels中的每一个treatment,"tr"可随意命名
  tmp <- subset(AA, Treatment==treat.levels[tre])#先赋值一个新变量，选取子集为每一个tr，
  ##注意要用"=="表示判定！若只有一个"="则表示赋值
  Cumu2 <- c()
  for(j in 1:NROW(tmp)){#对于每个tmpD的第j个元素，执行以下运算
    Cumu2[j] <- sum(tmp$Cumulative.CO2[1:j])
  }
  tmp$Cumulative.CO2.2 <- Cumu2
  if(tre ==1){
    AA.3 = tmp
  }else{AA.3 = rbind(AA.3, tmp)}
  #AA.3 <- rbind(AA,tmp)
}
