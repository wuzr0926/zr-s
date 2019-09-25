# Read in Data file
Df = read.table("/Users/xiaojunli/ZongruiWu/data_20190409.csv", header = TRUE, sep = ",")
# Dealing with: Treatment ""H-G-T+F--3", Day 1 is NA (Cumulative.CO2)
Df$Cumulative.CO2[which(Df$Treatment=="H-G-T+F--3" & Df$Day ==1)] =
  Df$Cumulative.CO2[which(Df$Treatment=="H-G-T+F--3" & Df$Day ==2)]*2-Df$Cumulative.CO2[which(Df$Treatment=="H-G-T+F--3" & Df$Day ==3)]
# Remove NA (Cumulative.CO2)
Df = Df[-which(is.na(Df$Cumulative.CO2)),]
# Recalculating Interval
Df$Interval[2:NROW(Df)] = Df$Day[2:NROW(Df)] - Df$Day[1:(NROW(Df)-1)]
# insert locations
locations = which(Df$Interval >1)
for(i in locations){
  print(i)
  Cumulative.CO2.seq = seq(from = Df[i-1,"Cumulative.CO2"],
                       to = Df[i,"Cumulative.CO2"],
                       length.out = Df[i,"Interval"]+1)
  tmpD = data.frame(
    Treatment = rep(Df[i, "Treatment"], Df[i,"Interval"]-1),
    Day = seq(from = Df[i-1,"Day"]+1,
              to = Df[i,"Day"]-1,
              by=1),
    Interval = rep(1,Df[i,"Interval"]-1),
    Cumulative.CO2 = Cumulative.CO2.seq[2:(length(Cumulative.CO2.seq)-1)]
    )
  Df = rbind(Df, tmpD)
}
