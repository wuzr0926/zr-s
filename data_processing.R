rm(list = ls())
setwd("/Users/xiaojunli/ZongruiWu/")
dat <- read.csv(file = "data_20190909.csv", header = TRUE)
dat <- dat[-1,]
colnames(dat) <- c("Date","Time","interval","CO2","H2O","H20C",
                   "Cell_Temperature","Cell_Pressure","CO2_Absorption","H2O_Absorption")
for(i in 4:dim(dat)[2]){
  dat[,i] <- as.numeric(as.character(dat[,i]))
}
#需要用striptime()函数将文本转换为POSIXct格式
dat$Time2 <- paste(as.character(dat$Date),as.character(dat$Time),sep=" ")
dat$Time2 <- strptime(dat$Time2, format = "%Y/%m/%d %H:%M:%S", tz="")
dat <- dat[,-c(1:3)] # remove "Date","Time","interval"
# calculate interval
time.interval <- difftime(dat$Time2[2:(NROW(dat))],dat$Time2[1:(NROW(dat)-1)],tz = "",units = "secs")
time.interval <- as.numeric(time.interval)
dat$interval2 <- c(time.interval,1)

# 处理2,0相邻的数据
dat[which(dat$interval2==0),"Time2"] <- dat[which(dat$interval2==0),"Time2"]-1
time.interval <- difftime(dat$Time2[2:(NROW(dat))],dat$Time2[1:(NROW(dat)-1)],tz = "",units = "secs")
time.interval <- as.numeric(time.interval)
dat$interval2 <- c(time.interval,1)

# 处理只有2的数据 #共5146个点
location = which(dat$interval2==2)
avgD <- matrix(NA, ncol = NCOL(dat), nrow = 1)
avgD <- as.data.frame(avgD)
colnames(avgD) <- colnames(dat)
for(k in location){
  print(which(location==k))
  tmpD = dat[k:(k+1),]
  avgD[1,1:7] = colMeans((tmpD[,1:7]))
  avgD$Time2 = tmpD[1,"Time2"]+1
  avgD$interval2 = 1
  dat = rbind(dat, avgD)
}
dat <- dat[order(dat$Time2),]
time.interval <- difftime(dat$Time2[2:(NROW(dat))],dat$Time2[1:(NROW(dat)-1)],tz = "",units = "secs")
time.interval <- as.numeric(time.interval)
dat$interval2 <- c(time.interval,1)

# 处理interval为3，或4，或5的数据
time.series <- seq(min(dat$Time2),max(dat$Time2),by=1)
time.series <- time.series[-which(as.character(time.series) %in% as.character(dat$Time2))]
compD <- matrix(NA, ncol = NCOL(dat), nrow = length(time.series))
compD <- as.data.frame(compD)
colnames(compD) <- colnames(dat)
compD$Time2 <- time.series
dat = rbind(dat, compD)
dat <- dat[order(dat$Time2),]
time.interval <- difftime(dat$Time2[2:(NROW(dat))],dat$Time2[1:(NROW(dat)-1)],tz = "",units = "secs")
time.interval <- as.numeric(time.interval)
dat$interval2 <- c(time.interval,1)

# 改回原来的数据格式
dat$Date <- substring(as.character(dat$Time2), first = 1, last = 10)
dat$Time <- substring(as.character(dat$Time2), first = 12, last = 19)
dat$interval <- "00:00:01"
dat <- dat[,c("Date","Time","interval","CO2","H2O","H20C",
              "Cell_Temperature","Cell_Pressure","CO2_Absorption","H2O_Absorption")]

write.table(dat, "processed_data.csv", row.names = FALSE, sep=",")

