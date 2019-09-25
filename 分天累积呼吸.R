cbs180.dat <- 
  ddply(cbs180.2, .(Site), summarise,
        avg = mean(Cumulative.CO2),
        se = sd(Cumulative.CO2)/sqrt(length(Cumulative.CO2)))

hsd180.dat.2 <- 
  ddply(hsd180.2, .(Site), summarise,
        avg = mean(Cumu.CO2),
        se = sd(Cumu.CO2)/sqrt(length(Cumu.CO2)))

b<-ggplot(all, aes(x=Day, y=average2,group=Site,col=Site)) + geom_point()+ geom_line() +geom_errorbar(aes(ymin=average2-se2,ymax=average2+se2),width=.2)