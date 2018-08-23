############  Check directionality simulations  ######################


library(dplyr)
library(ggplot2)
library(move)


###############  Mean Proportion of Eggs Laid  ########################

run138 = read.csv("Monarchs.2016.May.13.13_16_40.txt") #lsa dir 0.8-0.9
run139 = read.csv("Monarchs.2016.May.13.13_32_43.txt") #lsa dir 0.1-0.9
run140 = read.csv("Monarchs.2016.May.13.14_02_37.txt") #lsa dir 0.1-0.2
run141 = read.csv("Monarchs.2016.May.13.15_46_51.txt") #lsa dir 0.5-0.75
run142 = read.csv("Monarchs.2016.May.16.15_48_16.txt") #lsa opp dir 0.9-0.8
run143 = read.csv("Monarchs.2016.May.16.16_06_47.txt") #lsa opp dir 0.9-0.1
run144 = read.csv("Monarchs.2016.May.16.17_07_40.txt") #lsa opp dir 0.2-0.1
run145 = read.csv("Monarchs.2016.May.16.17_33_27.txt") #lsa opp dir 0.75-0.5
run92 = read.csv("Monarchs.2016.Mar.31.11_37_30.txt") #orig lhs 2
run146 = read.csv("Monarchs.2016.May.17.10_39_05.txt") #opp lhs 2
run148 = read.csv("Monarchs.2016.May.17.14_37_48.txt") #same lhs 2
run75 = read.csv("Monarchs.2016.Mar.15.00_49_48.txt") #orig lhs 3
run76 = read.csv("Monarchs.2016.Mar.15.14_43_47.txt") #orig lhs 3
run77 = read.csv("Monarchs.2016.Mar.15.17_42_20.txt") #orig lhs 3
run78 = read.csv("Monarchs.2016.Mar.15.20_49_28.txt") #orig lhs 3
run75.78 = rbind(run75,run76,run77,run78)
run147 = read.csv("Monarchs.2016.May.17.11_50_23.txt") #opp lhs 3
run98 = read.csv("Monarchs.2016.Apr.04.17_54_36.txt") #orig lhs 6
run149 = read.csv("Monarchs.2016.May.17.16_18_21.txt") #opp lhs 6
run150 = read.csv("Monarchs.2016.May.17.16_50_47.txt") #same lhs 6

#change to current data to summarize
d = run150
nrow(d)/10  #sample size = # of simulated butterflies, normally should usually be 10,000

#### add in percent of eggs laid ######

#get EggsLaid data from separate ticks
st1 = filter(d, tick == 1)
results[1,2] = mean(st1$EggsLaid)
results[1,3] = pop.sd(st1$EggsLaid)
results[1,4] = mean(st1$EggsToLay)
results[1,5] = pop.sd(st1$EggsToLay)
st2 = filter(d, tick == 2)
results[2,2] = mean(st2$EggsLaid)
results[2,3] = pop.sd(st2$EggsLaid)
results[2,4] = mean(st2$EggsToLay)
results[2,5] = pop.sd(st2$EggsToLay)
st3 = filter(d, tick == 3)
results[3,2] = mean(st3$EggsLaid)
results[3,3] = pop.sd(st3$EggsLaid)
results[3,4] = mean(st3$EggsToLay)
results[3,5] = pop.sd(st3$EggsToLay)
st4 = filter(d, tick == 4)
results[4,2] = mean(st4$EggsLaid)
results[4,3] = pop.sd(st4$EggsLaid)
results[4,4] = mean(st4$EggsToLay)
results[4,5] = pop.sd(st4$EggsToLay)
st5 = filter(d, tick == 5)
results[5,2] = mean(st5$EggsLaid)
results[5,3] = pop.sd(st5$EggsLaid)
results[5,4] = mean(st5$EggsToLay)
results[5,5] = pop.sd(st5$EggsToLay)
st6 = filter(d, tick == 6)
results[6,2] = mean(st6$EggsLaid)
results[6,3] = pop.sd(st6$EggsLaid)
results[6,4] = mean(st6$EggsToLay)
results[6,5] = pop.sd(st6$EggsToLay)
st7 = filter(d, tick == 7)
results[7,2] = mean(st7$EggsLaid)
results[7,3] = pop.sd(st7$EggsLaid)
results[7,4] = mean(st7$EggsToLay)
results[7,5] = pop.sd(st7$EggsToLay)
st8 = filter(d, tick == 8)
results[8,2] = mean(st8$EggsLaid)
results[8,3] = pop.sd(st8$EggsLaid)
results[8,4] = mean(st8$EggsToLay)
results[8,5] = pop.sd(st8$EggsToLay)
st9 = filter(d, tick == 9)
results[9,2] = mean(st9$EggsLaid)
results[9,3] = pop.sd(st9$EggsLaid)
results[9,4] = mean(st9$EggsToLay)
results[9,5] = pop.sd(st9$EggsToLay)
st10 = filter(d, tick == 10)
results[10,2] = mean(st10$EggsLaid)
results[10,3] = pop.sd(st10$EggsLaid)
results[10,4] = mean(st10$EggsToLay)
results[10,5] = pop.sd(st10$EggsToLay)

hist(st5$EggsLaid)


###### directionality local sensitivity check previous runs  #################
dirSA.ch = data.frame(matrix(nrow=4000, ncol=3))
colnames(dirSA.ch) = c("Directionality","EggsLaid","PropEggsLaid")
dirSA.ch$Directionality = factor(dirSA.ch$Directionality)
levels(dirSA.ch$Directionality) = c("0.1-0.2","0.1-0.9","0.5-0.75","0.8-0.9","0.9-0.8","0.9-0.1","0.2-0.1","0.75-0.50")

#run 140 - directionality = 0.1-0.2
dirSA.ch[1:1000,1] = "0.1-0.2"
dirSA.ch[1:1000,2] = st5$EggsLaid 
dirSA.ch[1:1000,3] = st5$EggsLaid[1:1000]/42 #42 possible eggs to lay on Day 5
hist(dirSA.ch[1:1000,3]); mean(dirSA.ch[1:1000,3]); median(dirSA.ch[1:1000,3]); t.test(dirSA.ch[1:1000,3])$conf.int
#run 139 - directionality = 0.1-0.9
dirSA.ch[1001:2000,1] = "0.1-0.9"
dirSA.ch[1001:2000,2] = st5$EggsLaid 
dirSA.ch[1001:2000,3] = st5$EggsLaid[1:1000]/42 #42 possible eggs to lay on Day 5
#run 141 - directionality = 0.5-0.75
dirSA.ch[2001:3000,1] = "0.5-0.75"
dirSA.ch[2001:3000,2] = st5$EggsLaid 
dirSA.ch[2001:3000,3] = st5$EggsLaid[1:1000]/42 #42 possible eggs to lay on Day 5
#run 138 - directionality = 0.8-0.9
dirSA.ch[3001:4000,1] = "0.8-0.9"
dirSA.ch[3001:4000,2] = st5$EggsLaid 
dirSA.ch[3001:4000,3] = st5$EggsLaid[1:1000]/42 #42 possible eggs to lay on Day 5
#run 142 - dir = 0.9-0.8
dirSA.ch[4001:5000,1] = "0.9-0.8"
dirSA.ch[4001:5000,2] = st5$EggsLaid 
dirSA.ch[4001:5000,3] = st5$EggsLaid[1:1000]/42 #42 possible eggs to lay on Day 5
#run 143 - dir = 0.9-0.1
dirSA.ch[5001:6000,1] = "0.9-0.1"
dirSA.ch[5001:6000,2] = st5$EggsLaid 
dirSA.ch[5001:6000,3] = st5$EggsLaid[1:1000]/42 #42 possible eggs to lay on Day 5
#run 144 - dir = 0.2-0.1
dirSA.ch[6001:7000,1] = "0.2-0.1"
dirSA.ch[6001:7000,2] = st5$EggsLaid 
dirSA.ch[6001:7000,3] = st5$EggsLaid[1:1000]/42 #42 possible eggs to lay on Day 5
#run 145 - dir = 0.75-0.50
dirSA.ch[7001:8000,1] = "0.75-0.50"
dirSA.ch[7001:8000,2] = st5$EggsLaid 
dirSA.ch[7001:8000,3] = st5$EggsLaid[1:1000]/42 #42 possible eggs to lay on Day 5


pch = ggplot(dirSA.ch, aes(x=Directionality, y=PropEggsLaid)) + geom_boxplot() + ggtitle("Proportion of Eggs Laid - Day 5")+
  xlab("Directionality") + ylab("Proportion of Eggs Laid") +
  stat_summary(fun.y="mean", geom="point", shape=3, size=3)
pch + theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
          axis.title.x = element_text(size = 26, margin=margin(10,0,10,0)),
          axis.title.y = element_text(size = 26, margin=margin(0,10,0,5)),
          axis.text = element_text(size = 20))


########## check dir on lhs combos  #######################

dirSA.chlhs = data.frame(matrix(nrow=4000, ncol=3))
colnames(dirSA.chlhs) = c("Directionality","EggsLaid","PropEggsLaid")
dirSA.chlhs$Directionality = factor(dirSA.chlhs$Directionality)
levels(dirSA.chlhs$Directionality) = c("0.1-0.2-lhs2-org","0.2-0.1-lhs2","0.1-0.2-lhs2",
                                       "0.50-0.75-lhs3-org","0.75-0.50-lhs3",
                                       "0.1-0.9-lhs6-org","0.9-0.1-lhs6","0.1-0.9-lhs6")

#run 92 - directionality = 0.1-0.2 - #### should be 10,000 rows not 1,000 #########
dirSA.chlhs[1:1000,1] = "0.1-0.2-lhs2-org"
dirSA.chlhs[1:1000,2] = st5$EggsLaid 
dirSA.chlhs[1:1000,3] = st5$EggsLaid[1:1000]/42 #42 possible eggs to lay on Day 5
hist(dirSA.chlhs[1:1000,3]); mean(dirSA.chlhs[1:1000,3]); median(dirSA.chlhs[1:1000,3]); t.test(dirSA.chlhs[1:1000,3])$conf.int
#run 146 - directionality = 0.2-0.1
dirSA.chlhs[1001:2000,1] = "0.2-0.1-lhs2"
dirSA.chlhs[1001:2000,2] = st5$EggsLaid 
dirSA.chlhs[1001:2000,3] = st5$EggsLaid[1:1000]/42 #42 possible eggs to lay on Day 5
hist(dirSA.chlhs[1001:2000,3]);
#run 148 - directionality = 0.1-0.2
dirSA.chlhs[2001:3000,1] = "0.1-0.2-lhs2"
dirSA.chlhs[2001:3000,2] = st5$EggsLaid 
dirSA.chlhs[2001:3000,3] = st5$EggsLaid[1:1000]/42 #42 possible eggs to lay on Day 5
hist(dirSA.chlhs[2001:3000,3]);
#run 75.78 - orig LHS 3
dirSA.chlhs[3001:12000,1] = "0.50-0.75-lhs3-org"
dirSA.chlhs[3001:12000,2] = st5$EggsLaid 
dirSA.chlhs[3001:12000,3] = st5$EggsLaid[1:1000]/42 #42 possible eggs to lay on Day 5
hist(dirSA.chlhs[3001:12000,3]);
#run 147 - opp LHS 3
dirSA.chlhs[12001:13000,1] = "0.75-0.50-lhs3"
dirSA.chlhs[12001:13000,2] = st5$EggsLaid 
dirSA.chlhs[12001:13000,3] = st5$EggsLaid[1:1000]/42 #42 possible eggs to lay on Day 5
hist(dirSA.chlhs[12001:13000,3]);
#run 98 - orig LHS 6
dirSA.chlhs[13001:23000,1] = "0.1-0.9-lhs6-org"
dirSA.chlhs[13001:23000,2] = st5$EggsLaid 
dirSA.chlhs[13001:23000,3] = st5$EggsLaid[1:1000]/42 #42 possible eggs to lay on Day 5
hist(dirSA.chlhs[13001:23000,3]);
#run 149 - opp LHS 6
dirSA.chlhs[23001:24000,1] = "0.9-0.1-lhs6"
dirSA.chlhs[23001:24000,2] = st5$EggsLaid 
dirSA.chlhs[23001:24000,3] = st5$EggsLaid[1:1000]/42 #42 possible eggs to lay on Day 5
hist(dirSA.chlhs[23001:24000,3]);
#run 150 - same LHS 6
dirSA.chlhs[24001:25000,1] = "0.1-0.9-lhs6"
dirSA.chlhs[24001:25000,2] = st5$EggsLaid 
dirSA.chlhs[24001:25000,3] = st5$EggsLaid[1:1000]/42 #42 possible eggs to lay on Day 5
hist(dirSA.chlhs[24001:25000,3]);


pchlhs = ggplot(dirSA.chlhs, aes(x=Directionality, y=PropEggsLaid)) + geom_boxplot() + ggtitle("Proportion of Eggs Laid - Day 5")+
  xlab("Directionality") + ylab("Proportion of Eggs Laid") +
  stat_summary(fun.y="mean", geom="point", shape=3, size=3)
pchlhs + theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
            axis.title.x = element_text(size = 26, margin=margin(10,0,10,0)),
            axis.title.y = element_text(size = 26, margin=margin(0,10,0,5)),
            axis.text = element_text(size = 20))












############### Egg Density ##########################


run138EZ = read.csv("CumEggsPerZone.2016.May.13.13_16_40.txt") 
run139EZ = read.csv("CumEggsPerZone.2016.May.13.13_32_43.txt") 
run140EZ = read.csv("CumEggsPerZone.2016.May.13.14_02_37.txt")
run141EZ = read.csv("CumEggsPerZone.2016.May.13.15_46_51.txt")

dens = run141EZ
nrow(dens) #should be 37165*20=743300

Denresults = Denresults138
Denresults = Denresults139
Denresults = Denresults140
Denresults = Denresults141

system.time(
  for(i in 1:37165)
  {
    densi = filter(dens, ID == i) #combine all 20 instances for each polygon
    Denresults[i,1] = densi[1,3] #polygon ID number
    Denresults[i,2] = as.character(densi[1,4]) #habitat type
    Denresults[i,3] = sum(densi$Eggs) #sum across the 20 instances to get total eggs for per polygon
    Denresults[i,4] = densi[1,6] #polygon area - sometimes lat/long, depends on shapefile
    Denresults[i,5] = StPolyArea[i,5] #polygon area in m^2
    Denresults[i,6] = (Denresults[i,3]/Denresults[i,5])*10000 #calc density per ha
    Denresults[i,7] = log(Denresults[i,6]) #ln of density
    Denresults[i,8] = log10(Denresults[i,6]) #log base 10 of density
    Denresults[i,9] = densi[1,5] #probEggs
  }
)


DensR6 = filter(Denresults, HabType == "MWROW60-100")
hist(DensR6$EggDensityperHA) 
hist(DensR6[,7])#ln egg density
hist(DensR6[,8])#log10 egg density
DensperHab[13,1]=mean(DensR6$EggDensityperHA)
DensperHab[13,2]=pop.sd(DensR6$EggDensityperHA)
DensperHab[13,3]=median(DensR6$EggDensityperHA)
DensperHab[13,4]=sum(DensR6$CumEggs)
mean(DensR6[,5])/10000 #average area in HA of GP polys

DensR2 = filter(Denresults, HabType == "MWROW20-60")
hist(DensR2$EggDensityperHA)
DensperHab[11,1]=mean(DensR2$EggDensityperHA)
DensperHab[11,2]=pop.sd(DensR2$EggDensityperHA)
DensperHab[11,3]=median(DensR2$EggDensityperHA)
DensperHab[11,4]=sum(DensR2$CumEggs)

DensR5 = filter(Denresults, HabType == "MWROW5-20")
hist(DensR5$EggDensityperHA)
mean(DensR5$EggDensityperHA); pop.sd(DensR5$EggDensityperHA); median(DensR5$EggDensityperHA)
DensperHab[12,1]=mean(DensR5$EggDensityperHA)
DensperHab[12,2]=pop.sd(DensR5$EggDensityperHA)
DensperHab[12,3]=median(DensR5$EggDensityperHA)
DensperHab[12,4]=sum(DensR5$CumEggs)

DensR1 = filter(Denresults, HabType == "MWROW1-5")
hist(DensR1$EggDensityperHA)
DensperHab[10,1]=mean(DensR1$EggDensityperHA)
DensperHab[10,2]=pop.sd(DensR1$EggDensityperHA)
DensperHab[10,3]=median(DensR1$EggDensityperHA)
DensperHab[10,4]=sum(DensR1$CumEggs)

DensGP = filter(Denresults, HabType == "Grass/Pasture") #1920 gp polygons
hist(DensGP$EggDensityperHA)
DensperHab[8,1]=mean(DensGP$EggDensityperHA)
DensperHab[8,2]=pop.sd(DensGP$EggDensityperHA)
DensperHab[8,3]=median(DensGP$EggDensityperHA)
DensperHab[8,4]=sum(DensGP$CumEggs) #total eggs laid in GP
#(sum(DensGP[,5]))/10000 #area in HA of all GP polys = 10763.25
#DensperHab[8,4]/((sum(DensGP[,5]))/10000) #mean eggs per HA = 70.5891
#(mean(DensGP[,3])*1920)/(sum(DensperHab[,4])) #mean eggs per polygon*1920/total eggs
#(mean(DensGP[,3])*1920)/(sum(DensGP[,5])) / ((sum(DensperHab[,4]))/sum(Denresults[,5])) #((mean eggs per polygon*1920)/meangppolyarea) / (total eggs/total polygon area)

DensCN = filter(Denresults, HabType == "Corn_nonGMO")
hist(DensCN$EggDensityperHA)
DensperHab[4,1]=mean(DensCN$EggDensityperHA)
DensperHab[4,2]=pop.sd(DensCN$EggDensityperHA)
DensperHab[4,3]=median(DensCN$EggDensityperHA)
DensperHab[4,4]=sum(DensCN$CumEggs)

DensCo = filter(Denresults, HabType == "Corn")
hist(DensCo$EggDensityperHA)
DensperHab[3,1]=mean(DensCo$EggDensityperHA)
DensperHab[3,2]=pop.sd(DensCo$EggDensityperHA)
DensperHab[3,3]=median(DensCo$EggDensityperHA)
DensperHab[3,4]=sum(DensCo$CumEggs)

DensRR = filter(Denresults, HabType == "RailroadROW")
hist(DensRR$EggDensityperHA)
DensperHab[14,1]=mean(DensRR$EggDensityperHA)
DensperHab[14,2]=pop.sd(DensRR$EggDensityperHA)
DensperHab[14,3]=median(DensRR$EggDensityperHA)
DensperHab[14,4]=sum(DensRR$CumEggs)

DensSN = filter(Denresults, HabType == "Soybeans_nonGMO")
hist(DensSN$EggDensityperHA)
DensperHab[16,1]=mean(DensSN$EggDensityperHA)
DensperHab[16,2]=pop.sd(DensSN$EggDensityperHA)
DensperHab[16,3]=median(DensSN$EggDensityperHA)
DensperHab[16,4]=sum(DensSN$CumEggs)

DensSo = filter(Denresults, HabType == "Soybeans")
hist(DensSo$EggDensityperHA)
DensperHab[15,1]=mean(DensSo$EggDensityperHA)
DensperHab[15,2]=pop.sd(DensSo$EggDensityperHA)
DensperHab[15,3]=median(DensSo$EggDensityperHA)
DensperHab[15,4]=sum(DensSo$CumEggs)

DensM0 = filter(Denresults, HabType == "MWROW0")
hist(DensM0$EggDensityperHA)
DensperHab[9,1]=mean(DensM0$EggDensityperHA)
DensperHab[9,2]=pop.sd(DensM0$EggDensityperHA)
DensperHab[9,3]=median(DensM0$EggDensityperHA)
DensperHab[9,4]=sum(DensM0$CumEggs)

DensWe = filter(Denresults, HabType == "Wetlands/Herbaceous Wetlands")
hist(DensWe$EggDensityperHA)
DensperHab[17,1]=mean(DensWe$EggDensityperHA)
DensperHab[17,2]=pop.sd(DensWe$EggDensityperHA)
DensperHab[17,3]=median(DensWe$EggDensityperHA)
DensperHab[17,4]=sum(DensWe$CumEggs)

DensFo = filter(Denresults, HabType == "Forest/Deciduous Forest/Evergreen Forest/Mixed Forest/Woody Wetlands")
hist(DensFo$EggDensityperHA)
DensperHab[7,1]=mean(DensFo$EggDensityperHA)
DensperHab[7,2]=pop.sd(DensFo$EggDensityperHA)
DensperHab[7,3]=median(DensFo$EggDensityperHA)
DensperHab[7,4]=sum(DensFo$CumEggs)

DensLO = filter(Denresults, HabType == "Developed/Open Space/Developed/Low Intensity")
hist(DensLO$EggDensityperHA)
DensperHab[6,1]=mean(DensLO$EggDensityperHA)
DensperHab[6,2]=pop.sd(DensLO$EggDensityperHA)
DensperHab[6,3]=median(DensLO$EggDensityperHA)
DensperHab[6,4]=sum(DensLO$CumEggs)

DensMH = filter(Denresults, HabType == "Blank_/Developed/Med Intensity/Developed/High Intensity")
hist(DensMH$EggDensityperHA)
DensperHab[2,1]=mean(DensMH$EggDensityperHA)
DensperHab[2,2]=pop.sd(DensMH$EggDensityperHA)
DensperHab[2,3]=median(DensMH$EggDensityperHA)
DensperHab[2,4]=sum(DensMH$CumEggs)

DensWa = filter(Denresults, HabType == "Barren/Clouds/No Data/Developed/Water/Nonag/Undefined/Aquaculture/Open Water/Perennial Ice/Snow")
hist(DensWa$EggDensityperHA)
DensperHab[1,1]=mean(DensWa$EggDensityperHA)
DensperHab[1,2]=pop.sd(DensWa$EggDensityperHA)
DensperHab[1,3]=median(DensWa$EggDensityperHA)
DensperHab[1,4]=sum(DensWa$CumEggs)

DensCR = filter(Denresults, HabType == "Cotton/Rice/Sorghum/Sunflower/Peanuts/Tobacco/Sweet Corn/Pop or Orn Corn/Mint/Barley/Durum Wheat/Spring Wheat/Winter Wheat/Other")
hist(DensCR$EggDensityperHA)
DensperHab[5,1]=mean(DensCR$EggDensityperHA)
DensperHab[5,2]=pop.sd(DensCR$EggDensityperHA)
DensperHab[5,3]=median(DensCR$EggDensityperHA)
DensperHab[5,4]=sum(DensCR$CumEggs)

#loop through habs to calc proportion of eggs laid in each hab type
hablist = c("Barren/Clouds/No Data/Developed/Water/Nonag/Undefined/Aquaculture/Open Water/Perennial Ice/Snow",
            "Blank_/Developed/Med Intensity/Developed/High Intensity","Corn","Corn_nonGMO",
            "Cotton/Rice/Sorghum/Sunflower/Peanuts/Tobacco/Sweet Corn/Pop or Orn Corn/Mint/Barley/Durum Wheat/Spring Wheat/Winter Wheat/Other",
            "Developed/Open Space/Developed/Low Intensity","Forest/Deciduous Forest/Evergreen Forest/Mixed Forest/Woody Wetlands",
            "Grass/Pasture","MWROW0","MWROW1-5","MWROW20-60","MWROW5-20","MWROW60-100","RailroadROW","Soybeans",
            "Soybeans_nonGMO","Wetlands/Herbaceous Wetlands")
for (i in hablist){
  DensperHab[i,5] = DensperHab[i,4]/(sum(DensperHab[,4]))
}


#######################  Directionality EZ SA graphs ###########################

eggDdirSA.ch = data.frame(matrix(nrow=68, ncol=4))
colnames(eggDdirSA.ch) = c("Directionality","HabType","PropEggs","Median")
eggDdirSA.ch$Directionality = factor(eggDdirSA.ch$Directionality)
levels(eggDdirSA.ch$Directionality) = c("0.1-0.2","0.1-0.9","0.5-0.75","0.8-0.9")
#run 140 - directionality 0.1-0.2
eggDdirSA.ch[1:17,1] = "0.1-0.2" #directionality
eggDdirSA.ch[1:17,2] = habtypes #hab types
eggDdirSA.ch[1:17,3] = DensperHab[1:17,5] #proportion of eggs laid per habitat type
eggDdirSA.ch[1:17,4] = DensperHab[1:17,3] #median egg density
#run 139 - directionality 0.1-0.9
eggDdirSA.ch[18:34,1] = "0.1-0.9"  #directionality
eggDdirSA.ch[18:34,2] = habtypes #hab types
eggDdirSA.ch[18:34,3] = DensperHab[1:17,5] #proportion of eggs laid per habitat type
eggDdirSA.ch[18:34,4] = DensperHab[1:17,3] #median egg density
#run 141 - directionality 0.5-0.75
eggDdirSA.ch[35:51,1] = "0.5-0.75" #directionality
eggDdirSA.ch[35:51,2] = habtypes #hab types
eggDdirSA.ch[35:51,3] = DensperHab[1:17,5] #proportion of eggs laid per habitat type
eggDdirSA.ch[35:51,4] = DensperHab[1:17,3] #median egg density
#run 138 - directionality 0.8-0.9
eggDdirSA.ch[52:68,1] = "0.8-0.9" #directionality
eggDdirSA.ch[52:68,2] = habtypes #hab types
eggDdirSA.ch[52:68,3] = DensperHab[1:17,5] #proportion of eggs laid per habitat type
eggDdirSA.ch[52:68,4] = DensperHab[1:17,3] #median egg density


#median egg density
plrch = ggplot(eggDdirSA.ch, aes(x=reorder(HabType,Median), y=Median, linetype=Directionality, group=Directionality)) + geom_line() +
  ggtitle("Median Egg Density per Habitat Type") + xlab("Habitat Type") + ylab("Median Egg Density")
plrch + theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)),
            axis.title.x = element_text(size = 26, margin=margin(10,0,10,0)),
            axis.title.y = element_text(size = 26, margin=margin(0,10,0,5)),
            axis.text.x = element_text(angle=60, hjust=1, size=16),
            axis.text = element_text(size = 20),
            legend.title = element_text(size = 26),
            legend.text = element_text(size = 26))


############ boxplots per habitat type for DIRECTIONALITY  ######################

#### Grass/Pasture ###### 1920 polygons - not sure if column 2, propeggs, is right.  Col 3 is right, though.  
EggDensGP.dirch = data.frame(matrix(nrow=5000, ncol=4))
colnames(EggDensGP.dirch) = c("Directionality","PropEggs","PropEggsMean","EggDensity")
EggDensGP.dirch$Directionality = factor(EggDensGP.dirch$Directionality)
levels(EggDensGP.dirch$Directionality) = c("0.1-0.2","0.1-0.9","0.5-0.75","0.8-0.9")

#run 140 - step length 30, directionality = 0.1-0.2
EggDensGP.dirch[1:1920,1] = "0.1-0.2" #directionality
EggDensGP.dirch[1:1920,2] = DensGP[,3]*1920/sum(DensperHab[,4]) #3050372 #proportion of eggs in gp if all polys had this number of eggs
EggDensGP.dirch[1:1920,3] = (DensGP[,6]*(sum(DensGP[,5]))/10000)/sum(DensperHab[,4]) #prop of eggs in gp if all polys this density
EggDensGP.dirch[1:1920,4] = DensGP[,6] #median egg density, i.e., distribution of egg density
mean(EggDensGP.dirch[1:1920,4])
#run 139 - directionality = 0.1-0.9
EggDensGP.dirch[1921:3840,1] = "0.1-0.9" #directionality
EggDensGP.dirch[1921:3840,2] = DensGP[,3]*1920/sum(DensperHab[,4]) #proportion of eggs in gp if all polys had this number of eggs
EggDensGP.dirch[1921:3840,3] = (DensGP[,6]*(sum(DensGP[,5]))/10000)/sum(DensperHab[,4]) #prop of eggs in gp if all polys this density
EggDensGP.dirch[1921:3840,4] = DensGP[,6] #median egg density, i.e., distribution of egg density
#run 141 - directionality = 0.5-0.75
EggDensGP.dirch[3841:5760,1] = "0.5-0.75" #directionality
EggDensGP.dirch[3841:5760,2] = DensGP[,3]*1920/sum(DensperHab[,4]) #2177392 #proportion of eggs in gp if all polys had this number of eggs
EggDensGP.dirch[3841:5760,3] = (DensGP[,6]*(sum(DensGP[,5]))/10000)/sum(DensperHab[,4]) #prop of eggs in gp if all polys this density
EggDensGP.dirch[3841:5760,4] = DensGP[,6] #median egg density, i.e., distribution of egg density
#run 138 - directionality = 0.8-0.9
EggDensGP.dirch[5761:7680,1] = "0.8-0.9" #directionality
EggDensGP.dirch[5761:7680,2] = DensGP[,3]*1920/sum(DensperHab[,4]) # #proportion of eggs in gp if all polys had this number of eggs
EggDensGP.dirch[5761:7680,3] = (DensGP[,6]*(sum(DensGP[,5]))/10000)/sum(DensperHab[,4]) #prop of eggs in gp if all polys this density
EggDensGP.dirch[5761:7680,4] = DensGP[,6] #median egg density, i.e., distribution of egg density
max(EggDensGP.dirch[5761:7680,4])

#distribution of egg density per GP plot
pgch = ggplot(EggDensGP.dirch, aes(x=Directionality, y=EggDensity)) + geom_boxplot(outlier.shape = NA) + ggtitle("Egg Density in Grass/Pasture Polygons") +
  xlab("Range in Directionality") + ylab("Eggs per HA") +
  ylim(0,40) +
  stat_summary(fun.y="mean", geom="point", shape=3, size=3) 
pgch + theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
           axis.title.x = element_text(size = 26, margin=margin(10,0,10,0)),
           axis.title.y = element_text(size = 26, margin=margin(0,10,0,5)),
           axis.text = element_text(size = 20))


############### Road ROW 60-100 ###### 8976 polygons ########  PERCEPTION ##########
EggDensR6.dirch = data.frame(matrix(nrow=10, ncol=4))
colnames(EggDensR6.dirch) = c("Directionality","PropEggs","PropEggsMean","EggDensity")
EggDensR6.dirch$Directionality = factor(EggDensR6.dir$Directionality)
levels(EggDensR6.dirch$Directionality) = c("0.1-0.2","0.1-0.9","0.5-0.75","0.8-0.9")

#run 140 - step length 30, directionality = 0.1-0.2
EggDensR6.dirch[1:8976,1] = "0.1-0.2" #directionality
EggDensR6.dirch[1:8976,2] = DensR6[,3]*1920/sum(DensperHab[,4]) #3050372 #proportion of eggs in gp if all polys had this number of eggs
EggDensR6.dirch[1:8976,3] = (DensR6[,6]*(sum(DensR6[,5]))/10000)/sum(DensperHab[,4]) #prop of eggs in gp if all polys this density
EggDensR6.dirch[1:8976,4] = DensR6[,6] #median egg density, i.e., distribution of egg density
mean(EggDensR6.dirch[1:8976,4])
#run 139 - directionality = 0.1-0.9
EggDensR6.dirch[8977:17952,1] = "0.1-0.9" #directionality
EggDensR6.dirch[8977:17952,2] = DensR6[,3]*1920/sum(DensperHab[,4]) #proportion of eggs in gp if all polys had this number of eggs
EggDensR6.dirch[8977:17952,3] = (DensR6[,6]*(sum(DensR6[,5]))/10000)/sum(DensperHab[,4]) #prop of eggs in gp if all polys this density
EggDensR6.dirch[8977:17952,4] = DensR6[,6] #median egg density, i.e., distribution of egg density
mean(EggDensR6.dirch[8977:17952,4])
#run 141 - directionality = 0.5-0.75
EggDensR6.dirch[17953:26928,1] = "0.5-0.75" #directionality
EggDensR6.dirch[17953:26928,2] = DensR6[,3]*1920/sum(DensperHab[,4]) #2177392 #proportion of eggs in gp if all polys had this number of eggs
EggDensR6.dirch[17953:26928,3] = (DensR6[,6]*(sum(DensR6[,5]))/10000)/sum(DensperHab[,4]) #prop of eggs in gp if all polys this density
EggDensR6.dirch[17953:26928,4] = DensR6[,6] #median egg density, i.e., distribution of egg density
#run 138 - directionality = 0.8-0.9
EggDensR6.dirch[26929:35904,1] = "0.8-0.9" #directionality
EggDensR6.dirch[26929:35904,2] = DensR6[,3]*1920/sum(DensperHab[,4]) # #proportion of eggs in gp if all polys had this number of eggs
EggDensR6.dirch[26929:35904,3] = (DensR6[,6]*(sum(DensR6[,5]))/10000)/sum(DensperHab[,4]) #prop of eggs in gp if all polys this density
EggDensR6.dirch[26929:35904,4] = DensR6[,6] #median egg density, i.e., distribution of egg density
hist(EggDensR6.dirch[26929:35904,4]); max(EggDensR6.dirch[26929:35904,4])

#distribution of egg density per ROW MW=60-100m2 plot
prch= ggplot(EggDensR6.dirch, aes(x=Directionality, y=EggDensity)) + geom_boxplot(outlier.shape = NA) + 
  ggtitle("Egg Density in Road Right-of-Way Polygons") +
  #ggtitle(expression(paste("Egg Density in Road Right-of-Way Polygons\n with 60-100+ m2 of Milkweed"))) +
  xlab("Range in Directionality") + ylab("Eggs per HA") +
  ylim(0,70) +
  stat_summary(fun.y="mean", geom="point", shape=3, size=3)
prch + theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
           axis.title.x = element_text(size = 26, margin=margin(10,0,10,0)),
           axis.title.y = element_text(size = 26, margin=margin(0,10,0,5)),
           axis.text = element_text(size = 20))



