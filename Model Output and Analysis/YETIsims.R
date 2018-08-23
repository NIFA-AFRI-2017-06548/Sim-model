
# YETI simulations, and speedy2 additional sims

#Global Sensitivity Analysis

######## TOC #############

# 1.  Mean proportion of eggs laid per monarch and graphs
# 2.  Egg Density and graphs
# 3.  Utilization distribution and graphs
# everywhere - split probeggs code is interspersed throughout

##########################

setwd("~/Repast/Monarchs/output")

library(dplyr)
library(ggplot2)
library(scales)
library(move)
pop.sd=function(x)(sqrt(var(x)*(length(x)-1)/length(x)))


##############################  Mean Proportion of Eggs Laid by Monarch Agents #######################################

#Create dataframe of results to output to csv
results = data.frame(matrix(nrow=10, ncol=5))
colnames(results) = c("Tick", "MeanEggsLaid", "SDEggsLaid","MeanEggsNotLaid","SDEggsNotLaid")
rownames(results) = 1:10
results$Tick = 1:10

#output files from simulation runs
run153 = read.csv("Monarchs.2016.Aug.10.12_52_36.txt") #test and LHS2
run156 = read.csv("Monarchs.2016.Sep.06.17_30_16.txt") #LHS1.1 (LHS1 = 156-162,164-166)
run157 = read.csv("Monarchs.2016.Sep.07.12_12_56.txt") #LHS1.2
run158 = read.csv("Monarchs.2016.Sep.07.14_40_25.txt") #LHS1.3   
run159 = read.csv("Monarchs.2016.Sep.07.16_23_42.txt") #LHS1.4
run160 = read.csv("Monarchs.2016.Sep.08.12_13_03.txt") #LHS1.5
run161 = read.csv("Monarchs.2016.Sep.08.16_43_19.txt") #LHS1.6
run162 = read.csv("Monarchs.2016.Sep.08.18_32_00.txt") #LHS1.7
run164 = read.csv("Monarchs.2016.Sep.09.12_16_21.txt") #LHS1.8
run165 = read.csv("Monarchs.2016.Sep.09.15_05_48.txt") #LHS1.9
run166 = read.csv("Monarchs.2016.Sep.09.17_00_36.txt") #LHS1.10
runlhs1 = rbind(run156,run157,run158,run159,run160,run161,run162,run164,run165,run166)
########################### new LHS sims without 10m below this line #####################
run175 = read.csv("Monarchs.2016.Oct.11.08_26_15.txt") #LHS1 (new, as are all the following)
run191 = read.csv("Monarchs.2016.Nov.03.08_36_28.txt") #LHS2
run177 = read.csv("Monarchs.2016.Oct.11.10_13_35.txt") #LHS3
run179 = read.csv("Monarchs.2016.Oct.11.14_47_07.txt") #LHS4
run181 = read.csv("Monarchs.2016.Oct.14.13_03_17.txt") #LHS5
run183 = read.csv("Monarchs.2016.Oct.13.15_18_17.txt") #LHS6
run185 = read.csv("Monarchs.2016.Oct.14.09_42_59.txt") #LHS7
run187 = read.csv("Monarchs.2016.Oct.14.13_06_42.txt") #LHS8
run189 = read.csv("Monarchs.2016.Oct.17.12_21_28.txt") #LHS9
run192 = read.csv("Monarchs.2016.Nov.02.12_43_19.txt") #LHS10a - 1,000
run194 = read.csv("Monarchs.2016.Nov.03.20_53_56.txt") #LHS10b - 4,000
run196 = read.csv("Monarchs.2016.Nov.04.21_49_59.txt") #LHS10c - 5,000
runlhs10 = rbind(run192,run194,run196) #LHS10 combined
run195 = read.csv("Monarchs.2016.Nov.04.09_54_36.txt") #LHS11
run197 = read.csv("Monarchs.2016.Nov.07.09_27_59.txt") #LHS12a
run200 = read.csv("Monarchs.2016.Nov.09.11_34_18.txt") #LHS12b
run217 = read.csv("Monarchs.2016.Nov.16.09_38_58.txt") #LHS12c
LHS12 = rbind(run197,run200,run217)
run199 = read.csv("Monarchs.2016.Nov.07.17_47_03.txt") #LHS13
run202 = read.csv("Monarchs.2016.Nov.08.16_46_26.txt") #LHS14
run204 = read.csv("Monarchs.2016.Nov.08.23_01_49.txt") #LHS15
run206 = read.csv("Monarchs.2016.Nov.09.13_02_52.txt") #LHS16
run208 = read.csv("Monarchs.2016.Nov.09.17_01_58.txt") #LHS17a
run209 = read.csv("Monarchs.2016.Nov.11.03_16_23.txt") #LHS17b
run210 = read.csv("Monarchs.2016.Nov.12.02_50_44.txt") #LHS17c
LHS17 = rbind(run208, run209, run210)
run212 = read.csv("Monarchs.2016.Nov.14.09_27_27.txt") #LHS18 - sample size 98,716 instead of 100,000
run214 = read.csv("Monarchs.2016.Nov.15.08_42_32.txt") #LHS19
run216 = read.csv("Monarchs.2016.Nov.15.17_55_12.txt") #LHS20
run219 = read.csv("Monarchs.2016.Nov.21.11_20_10.txt") #LHS21
run221 = read.csv("Monarchs.2016.Nov.29.02_04_24.txt") #LHS22
run223 = read.csv("Monarchs.2016.Nov.30.17_09_12.txt") #LHS23
run225 = read.csv("Monarchs.2016.Dec.01.18_52_55.txt") #LHS24
run227 = read.csv("Monarchs.2016.Dec.01.13_30_05.txt") #LHS25a
run228 = read.csv("Monarchs.2016.Dec.02.14_30_31.txt") #LHS25b
LHS25 = rbind(run227,run228)
run230 = read.csv("Monarchs.2017.Jan.27.08_20_53.txt") #simulation to test split probEggs
run233 = read.csv("Monarchs.2017.Jan.31.14_06_34.txt") #test case 2
run235 = read.csv("Monarchs.2017.Feb.02.14_53_52.txt") #test case 2 with lastAngle returned to before
run236 = read.csv("Monarchs.2017.Feb.02.18_23_26.txt") #rerun of 235 to see how similar

d = run236
nrow(d)/10  #sample size = # of simulated butterflies, normally should be 10,000

#I used mean prop eggs laid for day 5 only for sensitivity analysis
st5 = filter(d, tick == 5)
results[5,2] = mean(st5$EggsLaid)
results[5,3] = pop.sd(st5$EggsLaid)
results[5,4] = mean(st5$EggsToLay)
results[5,5] = pop.sd(st5$EggsToLay)

hist(st5$EggsLaid)
hist(st5$EggsLaid, breaks=20)
plot(density(st5$EggsLaid))


#plot day 5 from individual runs
eggsGS = data.frame(matrix(nrow=10000, ncol=3))
colnames(eggsGS) = c("Run","EggsLaid","PropEggsLaid")
eggsGS$Run = factor(eggsGS$Run)
levels(eggsGS$Run) = c("LHS1","LHS2","LHS3","LHS4","LHS5","LHS6","LHS7","LHS8","LHS9","LHS10","LHS11","LHS12","LHS13",
                       "LHS14","LHS15","LHS16","LHS17","LHS18","LHS19","LHS20","LHS21","LHS22","LHS23","LHS24","LHS25",
                       "run230","run233","run235","run236")

#run 175 - LHS1
eggsGS[1:10000,1] = "LHS1"
eggsGS[1:10000,2] = st5$EggsLaid 
eggsGS[1:10000,3] = st5$EggsLaid[1:10000]/42 #42 possible eggs to lay on Day 5
hist(eggsGS[1:10000,3]); mean(eggsGS[1:1000,3]); median(eggsGS[1:10000,3]); t.test(eggsGS[1:10000,3])$conf.int
# - LHS2
eggsGS[10001:20000,1] = "LHS2"
eggsGS[10001:20000,2] = st5$EggsLaid 
eggsGS[10001:20000,3] = st5$EggsLaid[1:10000]/42 #42 possible eggs to lay on Day 5
mean(eggsGS[10001:20000,3]); median(eggsGS[10001:20000,3])
#run 177 - LHS3
eggsGS[20001:30000,1] = "LHS3"
eggsGS[20001:30000,2] = st5$EggsLaid 
eggsGS[20001:30000,3] = st5$EggsLaid[1:10000]/42 #42 possible eggs to lay on Day 5
mean(eggsGS[20001:30000,3]); median(eggsGS[20001:30000,3])
#run 179 - LHS4
eggsGS[30001:40000,1] = "LHS4"
eggsGS[30001:40000,2] = st5$EggsLaid 
eggsGS[30001:40000,3] = st5$EggsLaid[1:10000]/42 #42 possible eggs to lay on Day 5
mean(eggsGS[30001:40000,3]); median(eggsGS[30001:40000,3])
#run 181 - LHS5
eggsGS[40001:50000,1] = "LHS5"
eggsGS[40001:50000,2] = st5$EggsLaid 
eggsGS[40001:50000,3] = st5$EggsLaid[1:10000]/42 #42 possible eggs to lay on Day 5
mean(eggsGS[40001:50000,3]); median(eggsGS[40001:50000,3])
#run 183 - LHS6
eggsGS[50001:60000,1] = "LHS6"
eggsGS[50001:60000,2] = st5$EggsLaid 
eggsGS[50001:60000,3] = st5$EggsLaid[1:10000]/42 #42 possible eggs to lay on Day 5
mean(eggsGS[50001:60000,3]); median(eggsGS[50001:60000,3])
#run 185 - LHS7
eggsGS[60001:70000,1] = "LHS7"
eggsGS[60001:70000,2] = st5$EggsLaid 
eggsGS[60001:70000,3] = st5$EggsLaid[1:10000]/42 #42 possible eggs to lay on Day 5
mean(eggsGS[60001:70000,3]); median(eggsGS[60001:70000,3])
#run 187 - LHS8
eggsGS[70001:80000,1] = "LHS8"
eggsGS[70001:80000,2] = st5$EggsLaid 
eggsGS[70001:80000,3] = st5$EggsLaid[1:10000]/42 #42 possible eggs to lay on Day 5
mean(eggsGS[70001:80000,3]); median(eggsGS[70001:80000,3])
#run 189 - LHS9
eggsGS[80001:90000,1] = "LHS9"
eggsGS[80001:90000,2] = st5$EggsLaid 
eggsGS[80001:90000,3] = st5$EggsLaid[1:10000]/42 #42 possible eggs to lay on Day 5
mean(eggsGS[80001:90000,3]); median(eggsGS[80001:90000,3])
#run195 - LHS11
eggsGS[90001:100000,1] = "LHS11"
eggsGS[90001:100000,2] = st5$EggsLaid 
eggsGS[90001:100000,3] = st5$EggsLaid[1:10000]/42 #42 possible eggs to lay on Day 5
mean(eggsGS[90001:100000,3]); median(eggsGS[90001:100000,3])
#run192,194,196 - LHS10
eggsGS[100001:110000,1] = "LHS10"
eggsGS[100001:110000,2] = st5$EggsLaid 
eggsGS[100001:110000,3] = st5$EggsLaid[1:10000]/42 #42 possible eggs to lay on Day 5
mean(eggsGS[100001:110000,3]); median(eggsGS[100001:110000,3])
#run199 - LHS13
eggsGS[110001:120000,1] = "LHS13"
eggsGS[110001:120000,2] = st5$EggsLaid 
eggsGS[110001:120000,3] = st5$EggsLaid[1:10000]/42 #42 possible eggs to lay on Day 5
mean(eggsGS[110001:120000,3]); median(eggsGS[110001:120000,3])
#run202 - LHS14
eggsGS[120001:130000,1] = "LHS14"
eggsGS[120001:130000,2] = st5$EggsLaid 
eggsGS[120001:130000,3] = st5$EggsLaid[1:10000]/42 #42 possible eggs to lay on Day 5
mean(eggsGS[120001:130000,3]); median(eggsGS[120001:130000,3])
#run204 - LHS15
eggsGS[130001:140000,1] = "LHS15"
eggsGS[130001:140000,2] = st5$EggsLaid 
eggsGS[130001:140000,3] = st5$EggsLaid[1:10000]/42 #42 possible eggs to lay on Day 5
mean(eggsGS[130001:140000,3]); median(eggsGS[130001:140000,3])
#run206 - LHS16
eggsGS[140001:150000,1] = "LHS16"
eggsGS[140001:150000,2] = st5$EggsLaid 
eggsGS[140001:150000,3] = st5$EggsLaid[1:10000]/42 #42 possible eggs to lay on Day 5
mean(eggsGS[140001:150000,3]); median(eggsGS[140001:150000,3])
#run 197,200,217 - LHS12
eggsGS[150001:160000,1] = "LHS12"
eggsGS[150001:160000,2] = st5$EggsLaid 
eggsGS[150001:160000,3] = st5$EggsLaid[1:10000]/42 #42 possible eggs to lay on Day 5
mean(eggsGS[150001:160000,3]); median(eggsGS[150001:160000,3])
#run 208,209,210 - LHS17
eggsGS[160001:170000,1] = "LHS17"
eggsGS[160001:170000,2] = st5$EggsLaid 
eggsGS[160001:170000,3] = st5$EggsLaid[1:10000]/42 #42 possible eggs to lay on Day 5
mean(eggsGS[160001:170000,3]); median(eggsGS[160001:170000,3])
#run 212 - LHS18
eggsGS[170001:180000,1] = "LHS18"
eggsGS[170001:180000,2] = st5$EggsLaid 
eggsGS[170001:180000,3] = st5$EggsLaid[1:10000]/42 #42 possible eggs to lay on Day 5
mean(eggsGS[170001:180000,3]); median(eggsGS[170001:180000,3])
#run 214 - LHS19
eggsGS[180001:190000,1] = "LHS19"
eggsGS[180001:190000,2] = st5$EggsLaid 
eggsGS[180001:190000,3] = st5$EggsLaid[1:10000]/42 #42 possible eggs to lay on Day 5
mean(eggsGS[180001:190000,3]); median(eggsGS[180001:190000,3])
#run 216 - LHS20
eggsGS[190001:200000,1] = "LHS20"
eggsGS[190001:200000,2] = st5$EggsLaid 
eggsGS[190001:200000,3] = st5$EggsLaid[1:10000]/42 #42 possible eggs to lay on Day 5
mean(eggsGS[190001:200000,3]); median(eggsGS[190001:200000,3])
#run 219 = LHS21
eggsGS[200001:210000,1] = "LHS21"
eggsGS[200001:210000,2] = st5$EggsLaid 
eggsGS[200001:210000,3] = st5$EggsLaid[1:10000]/42 #42 possible eggs to lay on Day 5
mean(eggsGS[200001:210000,3]); median(eggsGS[200001:210000,3])
#run 221 = LHS22
eggsGS[210001:220000,1] = "LHS22"
eggsGS[210001:220000,2] = st5$EggsLaid 
eggsGS[210001:220000,3] = st5$EggsLaid[1:10000]/42 #42 possible eggs to lay on Day 5
mean(eggsGS[210001:220000,3]); median(eggsGS[210001:220000,3])
#run 223 = LHS23
eggsGS[220001:230000,1] = "LHS23"
eggsGS[220001:230000,2] = st5$EggsLaid 
eggsGS[220001:230000,3] = st5$EggsLaid[1:10000]/42 #42 possible eggs to lay on Day 5
mean(eggsGS[220001:230000,3]); median(eggsGS[220001:230000,3])
#run 225 = LHS24
eggsGS[230001:240000,1] = "LHS24"
eggsGS[230001:240000,2] = st5$EggsLaid 
eggsGS[230001:240000,3] = st5$EggsLaid[1:10000]/42 #42 possible eggs to lay on Day 5
mean(eggsGS[230001:240000,3]); median(eggsGS[230001:240000,3])
#run 227-228 = LHS25
eggsGS[240001:250000,1] = "LHS25"
eggsGS[240001:250000,2] = st5$EggsLaid 
eggsGS[240001:250000,3] = st5$EggsLaid[1:10000]/42 #42 possible eggs to lay on Day 5
mean(eggsGS[240001:250000,3]); median(eggsGS[240001:250000,3])
#run 230
eggsGS[250001:260000,1] = "run230"
eggsGS[250001:260000,2] = st5$EggsLaid 
eggsGS[250001:260000,3] = st5$EggsLaid[1:10000]/42 #42 possible eggs to lay on Day 5
mean(eggsGS[250001:260000,3]); median(eggsGS[250001:260000,3])
#run 233
eggsGS[260001:270000,1] = "run233"
eggsGS[260001:270000,2] = st5$EggsLaid 
eggsGS[260001:270000,3] = st5$EggsLaid[1:10000]/42 #42 possible eggs to lay on Day 5
#run 235
eggsGS[270001:280000,1] = "run235"
eggsGS[270001:280000,2] = st5$EggsLaid 
eggsGS[270001:280000,3] = st5$EggsLaid[1:10000]/42 #42 possible eggs to lay on Day 5
#run 236
eggsGS[280001:290000,1] = "run236"
eggsGS[280001:290000,2] = st5$EggsLaid 
eggsGS[280001:290000,3] = st5$EggsLaid[1:10000]/42 #42 possible eggs to lay on Day 5



p = ggplot(eggsGS, aes(x=Run, y=PropEggsLaid)) + geom_boxplot() + ggtitle("Proportion of Eggs Laid - Day 5") +
  xlab("Run") + ylab("Proportion of Eggs Laid") +
  stat_summary(fun.y="mean", geom="point", shape=3, size=3)
p + theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
          axis.title.x = element_text(size = 26, margin=margin(10,0,10,0)),
          axis.title.y = element_text(size = 26, margin=margin(0,10,0,5)),
          axis.text = element_text(size = 20))

##################  TEST CASEs  - compare LHS9 and run230 and run236 ###################

spliteggsGS = rbind(eggsGS[80001:90000,],eggsGS[250001:260000,],eggsGS[280001:290000,])

p = ggplot(spliteggsGS, aes(x=Run, y=PropEggsLaid)) + geom_boxplot() + ggtitle("Proportion of Eggs Laid - Day 5") +
  xlab("Run") + ylab("Proportion of Eggs Laid") +
  scale_x_discrete(labels=c("Original","Case 1","Case 2")) +
  stat_summary(fun.y="mean", geom="point", shape=3, size=3)
p + theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 26, margin=margin(0,10,0,5)),
          axis.text = element_text(size = 20))



###############################  Cumulative Eggs and Egg Densities per Zone -   Main Code   ###########################

Denresults = data.frame(matrix(nrow=37165, ncol=9))
colnames(Denresults) = c("PolygonID", "HabType", "CumEggs","PolygonAreaLL","PolygonAream2","EggDensityperHA",
                         "lnEggDensity","log10EggDensity","ProbEggs")

###  Polygon Areas
StPolyArea = read.csv("polygon_area_St_Co.csv")  #Story Co 37165 polygons

run151EZ = read.csv("CumEggsPerZone.2016.Aug.01.08_30_05.txt")
run153EZ = read.csv("CumEggsPerZone.2016.Aug.10.12_52_36.txt")
run156EZ = read.csv("CumEggsPerZone.2016.Sep.06.17_30_16.txt") #LHS1.1 (LHS1 = 156-162,164-166)
run157EZ = read.csv("CumEggsPerZone.2016.Sep.07.12_12_56.txt") #LHS1.2
run158EZ = read.csv("CumEggsPerZone.2016.Sep.07.14_40_25.txt") #LHS1.3
run159EZ = read.csv("CumEggsPerZone.2016.Sep.07.16_23_42.txt") #LHS1.4
run160EZ = read.csv("CumEggsPerZone.2016.Sep.08.12_13_03.txt") #LHS1.5
run161EZ = read.csv("CumEggsPerZone.2016.Sep.08.16_43_19.txt") #LHS1.6
run162EZ = read.csv("CumEggsPerZone.2016.Sep.08.18_32_00.txt") #LHS1.7
run164EZ = read.csv("CumEggsPerZone.2016.Sep.09.12_16_21.txt") #LHS1.8
run165EZ = read.csv("CumEggsPerZone.2016.Sep.09.15_05_48.txt") #LHS1.9
run166EZ = read.csv("CumEggsPerZone.2016.Sep.09.17_00_36.txt") #LHS1.10
runlhs1EZ = rbind(run156EZ,run157EZ,run158EZ,run159EZ,run160EZ,run161EZ,run162EZ,run164EZ,run165EZ,run166EZ)
########################### new LHS sims without 10m below this line #####################
run175EZ = read.csv("CumEggsPerZone.2016.Oct.11.08_26_15.txt") #LHS1 (new, as are all the following)
run191EZ = read.csv("CumEggsPerZone.2016.Nov.03.08_36_28.txt") #LHS2 was run on YETI
run177EZ = read.csv("CumEggsPerZone.2016.Oct.11.10_13_35.txt") #LHS3
run179EZ = read.csv("CumEggsPerZone.2016.Oct.11.14_47_07.txt") #LHS4
run181EZ = read.csv("CumEggsPerZone.2016.Oct.14.13_03_17.txt") #LHS5 - need to compile with outputcombiner.sh
run183EZ = read.csv("CumEggsPerZone.2016.Oct.13.15_18_17.txt") #LHS6
run185EZ = read.csv("CumEggsPerZone.2016.Oct.14.09_42_59.txt") #LHS7
run187EZ = read.csv("CumEggsPerZone.2016.Oct.14.13_06_42.txt") #LHS8
run189EZ = read.csv("CumEggsPerZone.2016.Oct.17.12_21_28.txt") #LHS9
run192EZ = read.csv("CumEggsPerZone.2016.Nov.02.12_43_19.txt") #LHS10 - 1,000
run194EZ = read.csv("CumEggsPerZone.2016.Nov.03.20_53_56.txt") #LHS10 - 4,000
run196EZ = read.csv("CumEggsPerZone.2016.Nov.04.21_49_59.txt") #LHS10 - 5,000
runlhs10EZ = rbind(run192EZ,run194EZ,run196EZ) #LHS10 combined
run195EZ = read.csv("CumEggsPerZone.2016.Nov.04.09_54_36.txt") #LHS11
run197EZ = read.csv("CumEggsPerZone.2016.Nov.07.09_27_59.txt") #LHS12a
run200EZ = read.csv("CumEggsPerZone.2016.Nov.09.11_34_18.txt") #LHS12b
run217EZ = read.csv("CumEggsPerZone.2016.Nov.16.09_38_58.txt") #LHS12c
LHS12EZ = rbind(run197EZ,run200EZ,run217EZ)
run199EZ = read.csv("CumEggsPerZone.2016.Nov.07.17_47_03.txt") #LHS13
run202EZ = read.csv("CumEggsPerZone.2016.Nov.08.16_46_26.txt") #LHS14
run204EZ = read.csv("CumEggsPerZone.2016.Nov.08.23_01_49.txt") #LHS15
run206EZ = read.csv("CumEggsPerZone.2016.Nov.09.13_02_52.txt") #LHS16
run208EZ = read.csv("CumEggsPerZone.2016.Nov.09.17_01_58.txt") #LHS17a
run209EZ = read.csv("CumEggsPerZone.2016.Nov.11.03_16_23.txt") #LHS17b
run210EZ = read.csv("CumEggsPerZone.2016.Nov.12.02_50_44.txt") #LHS17c
LHS17EZ = rbind(run208EZ,run209EZ,run210EZ) #LHS17
run212EZ = read.csv("CumEggsPerZone.2016.Nov.14.09_27_27.txt") #LHS18
run214EZ = read.csv("CumEggsPerZone.2016.Nov.15.08_42_32.txt") #LHS19
run216EZ = read.csv("CumEggsPerZone.2016.Nov.15.17_55_12.txt") #LHS20
run219EZ = read.csv("CumEggsPerZone.2016.Nov.21.11_20_10.txt") #LHS21
run221EZ = read.csv("CumEggsPerZone.2016.Nov.29.02_04_24.txt") #LHS22
run223EZ = read.csv("CumEggsPerZone.2016.Nov.30.17_09_12.txt") #LHS23
run225EZ = read.csv("CumEggsPerZone.2016.Dec.01.18_52_55.txt") #LHS24
run227EZ = read.csv("CumEggsPerZone.2016.Dec.01.13_30_05.txt") #LHS25a
run228EZ = read.csv("CumEggsPerZone.2016.Dec.02.14_30_31.txt") #LHS25b
LHS25EZ = rbind(run227EZ,run228EZ)
run230EZ = read.csv("CumEggsPerZone.2017.Jan.27.08_20_53.txt") #run230
run233EZ = read.csv("CumEggsPerZone.2017.Jan.31.14_06_34.txt") #run233
run236EZ = read.csv("CumEggsPerZone.2017.Feb.02.18_23_26.txt") #run236
run235EZ = read.csv("CumEggsPerZone.2017.Feb.02.14_53_52.txt") #run235

dens = run235EZ
nrow(dens) #should be 37165*20=743300

#loop through dens to create object with densities for each polygon - 5.7 mins (run44), 5.4 mins (run48), 
#5.6 mins (run43), 8.8 mins (run50.51EZ has 2x as many rows), 5.5 mins (run40EZ), 21 mins(run10mEZ) - 360s=6min
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

Denresults = Denresults151
Denresults = Denresults153
Denresults = DenresultsLHS1
######################
Denresults = Denresults175
Denresults = Denresults191
Denresults = Denresults177
Denresults = Denresults179
Denresults = Denresults181
Denresults = Denresults183
Denresults = Denresults185
Denresults = Denresults187
Denresults = Denresults189
Denresults = DenresultsLHS10
Denresults = Denresults195
Denresults = DenresultsLHS12
Denresults = Denresults199
Denresults = Denresults202
Denresults = Denresults204
Denresults = Denresults206
Denresults = DenresultsLHS17
Denresults = Denresults212
Denresults = Denresults214
Denresults = Denresults216
Denresults = Denresults219
Denresults = Denresults221
Denresults = Denresults223
Denresults = Denresults225
Denresults = DenresultsLHS25
Denresults = Denresults230
Denresults = Denresults233
Denresults = Denresults236
Denresults = Denresults235

#write densities to csv to map in ArcMap
write.csv(Denresults,"DensityResultsLHS10.csv") #runLHS10
write.csv(Denresults,"DensityResultsLHS18.csv") #runLHS18


DensperHab = data.frame(matrix(nrow=17, ncol=5))
colnames(DensperHab) = c("MeanEggDensity", "SD", "Median","Cumulative Eggs","Proportion of Eggs")
rownames(DensperHab) = c(levels(run175EZ$Name))


#stats by habitat type - I could loop through these by habitat type like i did for time budget instead of listing them all
DensR6 = filter(Denresults, HabType == "MWROW60-100")
hist(DensR6$EggDensityperHA) 
hist(DensR6[,7])#ln egg density
hist(DensR6[,8])#log10 egg density
DensperHab[13,1]=mean(DensR6$EggDensityperHA)
DensperHab[13,2]=pop.sd(DensR6$EggDensityperHA)
DensperHab[13,3]=median(DensR6$EggDensityperHA)
DensperHab[13,4]=sum(DensR6$CumEggs)
mean(DensR6[,5])/10000 #average area in HA of GP polys

DensGP = filter(Denresults, HabType == "Grass/Pasture") #1920 gp polygons
hist(DensGP$EggDensityperHA)
DensperHab[8,1]=mean(DensGP$EggDensityperHA)
DensperHab[8,2]=pop.sd(DensGP$EggDensityperHA)
DensperHab[8,3]=median(DensGP$EggDensityperHA)
DensperHab[8,4]=sum(DensGP$CumEggs) #total eggs laid in GP


## plot egg density x area for ms figure

DensGP.LHS11 = DensGP[,5:6]
DensGP.LHS11$PolygonAreaHA = DensGP.LHS11$PolygonAream2/1000
median(DensGP.LHS11$EggDensityperHA)

ggplot(DensGP.LHS11, aes(x=DensGP.LHS11$PolygonAreaHA, y=DensGP.LHS11$EggDensityperHA)) + geom_point() +
  ggtitle("Spatial Memory = 100") + geom_hline(yintercept = 57.68, linetype = "dashed") +
  ylab("Egg Density") + xlab("Polygon Area (Ha)") + ylim(0,1500) +
  theme(plot.title = element_text(size=22, face="bold", hjust=0.5, margin=margin(10,0,20,0)), 
        axis.title.x = element_text(size = 18, margin=margin(10,0,5,0)),
        axis.title.y = element_text(size = 18, margin=margin(0,10,0,5)),
        axis.text = element_text(size = 20))


DensGP.LHS24 = DensGP[,5:6]
DensGP.LHS24$PolygonAreaHA = DensGP.LHS24$PolygonAream2/1000
median(DensGP.LHS24$EggDensityperHA)

ggplot(DensGP.LHS24, aes(x=DensGP.LHS24$PolygonAreaHA, y=DensGP.LHS24$EggDensityperHA)) + geom_point() + 
  ggtitle("Spatial Memory = 10") + geom_hline(yintercept = 93.92, linetype = "dashed") +
  ylab("Egg Density") + xlab("Polygon Area (Ha)") + #ylim(-25,150) +
  theme(plot.title = element_text(size=22, face="bold", hjust=0.5, margin=margin(10,0,20,0)), 
        axis.title.x = element_text(size = 18, margin=margin(10,0,5,0)),
        axis.title.y = element_text(size = 18, margin=margin(0,10,0,5)),
        axis.text = element_text(size = 20))



##########  Calculate Proportion of Eggs Per Habitat type for each simulation  ##################

#now calcs all habitat types, instead of just 2 above

###  Dataframe to hold prop eggs per hab type for each scenario ##########
#fill it using code below for each hab type
DistPropEggsHab = data.frame(matrix(nrow = 17, ncol = 25))
colnames(DistPropEggsHab) = c(1:27)
rownames(DistPropEggsHab) = c(levels(run175EZ$Name))
DistPropEggsHab[,1] = DensperHab$`Proportion of Eggs` #28 is run236

DistPropEggsHab = DistPropEggsHab[1:17,]

DistPropEggsHab[18,] = 0
#row.names(DistPropEggsHab[18,]) = "MWROW"


####  Dataframe to hold total eggs per habitat type for each scenario to compare to prop eggs for each hab type ####

DistTotEggsHab = data.frame(matrix(nrow = 17, ncol = 25))
colnames(DistTotEggsHab) = c(1:25)
rownames(DistTotEggsHab) = c(levels(run175EZ$Name))

DistTotEggsHab[,25] = DensperHab$`Cumulative Eggs`



############ split probEggs TEST CASE ##################

spliteggs.propeggs = data.frame(matrix(nrow = 54, ncol = 3))
spliteggs.propeggs[1:18,3] = DistPropEggsHab[,9]
spliteggs.propeggs[19:36,3] = DistPropEggsHab[,26]
spliteggs.propeggs[37:53,3] = DistPropEggsHab[,28]
spliteggs.propeggs[1:18,2] = "LHS9"
spliteggs.propeggs[19:36,2] = "run230"
spliteggs.propeggs[37:53,2] = "run236"
colnames(spliteggs.propeggs) = c("Hab","Sim","PropEggs")
spliteggs.propeggs[1:17,1] = c(levels(run175EZ$Name))
spliteggs.propeggs[19:35,1] = c(levels(run175EZ$Name))
spliteggs.propeggs[37:53,1] = c(levels(run175EZ$Name))

spliteggs.propeggs2 = spliteggs.propeggs[-c(1,2,3,5,6,7,9,10,14,15,16,17,18,19,20,21,23,24,25,27,28,32,33,34,
                                            35,36,37,38,39,41,42,43,45,46,50,51,52,53,54),]

p = ggplot(spliteggs.propeggs2, aes(x=Hab, y=PropEggs, fill=Sim)) + geom_bar(stat="identity", position="dodge") +
  xlab("Land-Cover Type") + ylab("Proportion of Eggs") + scale_y_continuous(labels = comma) + labs(fill = "Simulation") +
  scale_fill_grey(labels=c("Original","Case 1","Case 2")) +
  scale_x_discrete(limits=c("Grass/Pasture","Corn_nonGMO","MWROW60-100","MWROW20-60","MWROW5-20"),
                   labels = c("Grass/Pasture","Non-GMO Corn","MWROW60-100","MWROW20-60","MWROW5-20")) +
  guides(fill=guide_legend(title = NULL, keyheight = 2))

p + theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
          axis.title.x = element_text(size = 26, margin=margin(10,0,10,0)),
          axis.title.y = element_text(size = 26, margin=margin(0,10,0,5)),
          axis.text = element_text(size = 16), 
          legend.position=c(0.8,0.8),
          legend.title=element_text(size = 20),
          axis.text.x = element_text(angle = 0),
          legend.text = element_text(size = 26))




########################################################

### Graphs for Manuscript I COULDN'T FIND!!!  FIGURE 7

#transpose prop eggs dataframe for graphing histograms

Tdf = t(DistPropEggsHab)
#sum ROWs into 1 number
for (i in 1:26){
  Tdf[i,18] = sum(Tdf[i,9],Tdf[i,10],Tdf[i,11],Tdf[i,12],Tdf[i,13])
}

max(Tdf[,8])
min(Tdf[,8])

library(lattice)

par(mfrow=c(3,3))

hist(Tdf[,18], xlim = c(0,0.5), xlab = NULL, main = "Road Right-of-Ways",breaks = "Sturges")
hist(Tdf[,8], xlim = c(0,0.5), xlab = NULL, main = "Grassland and Pasture",breaks = "Sturges")
hist(Tdf[,4], xlim = c(0,0.5), xlab = NULL, main = "Non-GMO Corn", breaks = "Sturges")
hist(Tdf[,16], xlim = c(0,0.5), xlab = NULL, main = "Non-GMO Soybeans",breaks = "Sturges")
hist(Tdf[,14], xlim = c(0,0.5), xlab = NULL, main = "Railroads",breaks = "Sturges")
hist(Tdf[,6], xlim = c(0,0.5), xlab = NULL, main = "Low Intensity Development",breaks = "Sturges")
hist(Tdf[,3], xlim = c(0,0.5), xlab = NULL, main = "GMO Corn", breaks = "Sturges")
hist(Tdf[,15], xlim = c(0,0.5), xlab = NULL, main = "GMO Soybeans",breaks = "Sturges")
hist(Tdf[,7], xlim = c(0,0.5), xlab = NULL, main = "Forest",breaks = "Sturges")

#hist(Tdf[,5], xlim = c(0,0.5), xlab = NULL, main = "Other Crops",breaks = "Sturges")
#hist(Tdf[,17], xlim = c(0,0.5), xlab = NULL, main = "Wetlands",breaks = "Sturges")

dev.off()

################   Dist of Cumulative Eggs

Tdf2 = t(DistTotEggsHab)
ROWtot = rep(NA,25)
Tdf2 = cbind(Tdf2,ROWtot)
#sum ROWs into 1 number
for (i in 1:25){
  Tdf2[i,18] = sum(Tdf2[i,9],Tdf2[i,10],Tdf2[i,11],Tdf2[i,12],Tdf2[i,13])
}

library(lattice)

par(mfrow=c(3,3))

hist(Tdf2[,18], xlim = c(0,2000000), xlab = NULL, main = "Road Right-of-Ways",breaks = "Sturges")
hist(Tdf2[,8], xlim = c(0,2000000), xlab = NULL, main = "Grassland and Pasture",breaks = "Sturges")
hist(Tdf2[,4], xlim = c(0,2000000), xlab = NULL, main = "Non-GMO Corn", breaks = "Sturges")
hist(Tdf2[,16], xlim = c(0,2000000), xlab = NULL, main = "Non-GMO Soybeans",breaks = "Sturges")
hist(Tdf2[,14], xlim = c(0,2000000), xlab = NULL, main = "Railroads",breaks = "Sturges")
hist(Tdf2[,6], xlim = c(0,2000000), xlab = NULL, main = "Low Intensity Development",breaks = "Sturges")
hist(Tdf2[,3], xlim = c(0,2000000), xlab = NULL, main = "GMO Corn", breaks = "Sturges")
hist(Tdf2[,15], xlim = c(0,2000000), xlab = NULL, main = "GMO Soybeans",breaks = "Sturges")
hist(Tdf2[,7], xlim = c(0,2000000), xlab = NULL, main = "Forest",breaks = "Sturges")

#hist(Tdf[,5], xlim = c(0,0.5), xlab = NULL, main = "Other Crops",breaks = "Sturges")
#hist(Tdf[,17], xlim = c(0,0.5), xlab = NULL, main = "Wetlands",breaks = "Sturges")

dev.off()





## compare split probEggs to LHS9
plot(Tdf[9,],Tdf[26,])
Tdfsplit = data.frame(matrix(nrow = 18, ncol = 5))
Tdfsplit[,1] = c(levels(run175EZ$Name),"ROW")
Tdfsplit[,2:3] = cbind(Tdf[9,],Tdf[26,])

colnames(Tdfsplit) = c("LHS9","run230") 

ggplot(Tdfsplit, aes())



# per habitat type - goes into dataframe above
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
























#############  Total eggs laid in the map, per habitat type  #####################

CumEggsperHab = data.frame(matrix(nrow=25, ncol=5))
##  total eggs laid in Grass/Pasture polygons, total is all eggs laid in map, propeggs is proportion of total laid in GP, etc.  
colnames(CumEggsperHab) = c("ROW60CumEggs","ROW60PropEggs","GPCumEggs","GPPropEggs","TotalCumEggs")
rownames(CumEggsperHab) = c("LHS1","LHS2","LHS3","LHS4","LHS5","LHS6","LHS7","LHS8","LHS9","LHS10","LHS11","LHS12",
                         "LHS13","LHS14","LHS15","LHS16","LHS17","LHS18","LHS19","LHS20","LHS21","LHS22","LHS23",
                         "LHS24","LHS25","run230","run236")

CumEggsperHab[27,1] = DensperHab[13,4]
CumEggsperHab[27,2] = DensperHab[13,4]/CumEggsperHab[27,5]
CumEggsperHab[27,3] = DensperHab[8,4]
CumEggsperHab[27,4] = DensperHab[8,4]/CumEggsperHab[27,5]
## opened CumEggs.txt file in Excel and took sum of CumEggs column, across instances
CumEggsperHab[1,5] = 2056734
CumEggsperHab[2,5] = 1837184
CumEggsperHab[3,5] = 1614292
CumEggsperHab[4,5] = 3075664
CumEggsperHab[5,5] = 2621394
CumEggsperHab[6,5] = 3883814
CumEggsperHab[7,5] = 1496940
CumEggsperHab[8,5] = 2290900
CumEggsperHab[9,5] = 2522948
CumEggsperHab[10,5] = 3548008
CumEggsperHab[11,5] = 1788480
CumEggsperHab[12,5] = 2746188
CumEggsperHab[13,5] = 2969884
CumEggsperHab[14,5] = 3753840
CumEggsperHab[15,5] = 1522072
CumEggsperHab[16,5] = 1684606
CumEggsperHab[17,5] = 3511234
CumEggsperHab[18,5] = 2177216
CumEggsperHab[19,5] = 1493918
CumEggsperHab[20,5] = 1963864
CumEggsperHab[21,5] = 1025954
CumEggsperHab[22,5] = 2782340
CumEggsperHab[23,5] = 3791472
CumEggsperHab[24,5] = 3082114
CumEggsperHab[25,5] = 2223690
CumEggsperHab[26,5] = 1819454
CumEggsperHab[27,5] = 2098266

################### Graphs ############################

#compare LHS9 with run230 and run236
spliteggs.toteggs = rbind(CumEggsperHab[9,],CumEggsperHab[26,],CumEggsperHab[27,])
spliteggs.toteggs[,6] = c("LHS9","run230","run236")
colnames(spliteggs.toteggs)[6] = "Sim"

ggplot(spliteggs.toteggs, aes(x=Sim, y=TotalCumEggs)) + geom_bar(stat="identity")

spliteggs.count = data.frame(matrix(nrow=9, ncol=3))
colnames(spliteggs.count) = c("Area","Sim","CumEggs")
spliteggs.count[,1] = c("Total", "Total", "Total", "ROW60", "ROW60", "ROW60", "GP", "GP", "GP")
spliteggs.count[,2] = c("LHS9","run230","run236","LHS9","run230","run236","LHS9","run230","run236")
spliteggs.count[,3] = c(spliteggs.toteggs[1,5],spliteggs.toteggs[2,5],spliteggs.toteggs[3,5],
                        spliteggs.toteggs[1,1],spliteggs.toteggs[2,1],spliteggs.toteggs[3,1],
                        spliteggs.toteggs[1,3],spliteggs.toteggs[2,3],spliteggs.toteggs[3,3])

library(scales)

p = ggplot(spliteggs.count, aes(x=Area, y=CumEggs, fill=Sim)) + geom_bar(stat="identity", position="dodge") +
  #xlab("Land-Cover Type") + 
  ylab("Total Eggs Laid") + scale_y_continuous(labels = scientific) + labs(fill = "Simulation") +
  scale_fill_grey(labels=c("Original","Case 1","Case 2")) +
  scale_x_discrete(limits=c("Total","GP","ROW60"),
                   labels=c("Total","Grass/Pasture","Roadsides")) +
  guides(fill=guide_legend(title = NULL, keyheight = 2))

p + theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 26, margin=margin(0,10,0,5)),
          axis.text = element_text(size = 20), 
          legend.position=c(0.8,0.8),
          legend.title=element_text(size = 20),
          legend.text = element_text(size = 26))


######## MWROW60-100 egg density graph ###################

EggDensR6 = data.frame(matrix(nrow=10, ncol=2))
colnames(EggDensR6) = c("Simulation","EggDensity")
EggDensR6$Simulation = factor(EggDensR6$Simulation)
levels(EggDensR6$Simulation) = c("LHS1","LHS2","LHS3","LHS4","LHS5","LHS6","LHS7","LHS8","LHS9","LHS10","LHS11","LHS12",
                                 "LHS13","LHS14","LHS15","LHS16","LHS17","LHS18","LHS19","LHS20","LHS21","LHS22","LHS23",
                                 "LHS24","LHS25","run230","run236","run235")
#8976 MWROW60-100 polys
EggDensR6[1:8976,1] = "LHS1"
EggDensR6[1:8976,2] = DensR6[,6] #egg density per HA
mean(EggDensR6[1:8976,2]); median(EggDensR6[1:8976,2])
EggDensR6[8977:17952,1] = "LHS2"
EggDensR6[8977:17952,2] = DensR6[,6] #egg density per HA
mean(EggDensR6[8977:17952,2]); median(EggDensR6[8977:17952,2])
EggDensR6[17953:26928,1] = "LHS3"
EggDensR6[17953:26928,2] = DensR6[,6] #egg density per HA
mean(EggDensR6[17953:26928,2]); median(EggDensR6[17953:26928,2])
EggDensR6[26929:35904,1] = "LHS4"
EggDensR6[26929:35904,2] = DensR6[,6] #egg density per HA
mean(EggDensR6[26929:35904,2]); median(EggDensR6[26929:35904,2])
EggDensR6[35905:44880,1] = "LHS5"
EggDensR6[35905:44880,2] = DensR6[,6] #egg density per HA
mean(EggDensR6[35905:44880,2]); median(EggDensR6[35905:44880,2])
EggDensR6[44881:53856,1] = "LHS6"
EggDensR6[44881:53856,2] = DensR6[,6] #egg density per HA
mean(EggDensR6[44881:53856,2]); median(EggDensR6[44881:53856,2])
EggDensR6[53857:62832,1] = "LHS7"
EggDensR6[53857:62832,2] = DensR6[,6] #egg density per HA
mean(EggDensR6[53857:62832,2]); median(EggDensR6[53857:62832,2])
EggDensR6[62833:71808,1] = "LHS8"
EggDensR6[62833:71808,2] = DensR6[,6] #egg density per HA
mean(EggDensR6[62833:71808,2]); median(EggDensR6[62833:71808,2])
EggDensR6[71809:80784,1] = "LHS9"
EggDensR6[71809:80784,2] = DensR6[,6] #egg density per HA
mean(EggDensR6[71809:80784,2]); median(EggDensR6[71809:80784,2])
EggDensR6[80785:89760,1] = "LHS10"
EggDensR6[80785:89760,2] = DensR6[,6] #egg density per HA
mean(EggDensR6[80785:89760,2]); median(EggDensR6[80785:89760,2])

# Figure 
hist(EggDensR6[80785:89760,2], breaks=400, xlab="Egg Density", xlim=c(0,1000), main="Perceptual Range = 400")
abline(v=16, lty=2, lwd=3)
median(EggDensR6[80785:89760,2])

EggDensR6[89761:98736,1] = "LHS11"
EggDensR6[89761:98736,2] = DensR6[,6] #egg density per HA
mean(EggDensR6[89761:98736,2]); median(EggDensR6[89761:98736,2])
EggDensR6[98737:107712,1] = "LHS12"
EggDensR6[98737:107712,2] = DensR6[,6] #egg density per HA
mean(EggDensR6[98737:107712,2]); median(EggDensR6[98737:107712,2])
EggDensR6[107713:116688,1] = "LHS13"
EggDensR6[107713:116688,2] = DensR6[,6] #egg density per HA
mean(EggDensR6[107713:116688,2]); median(EggDensR6[107713:116688,2])
EggDensR6[116689:125664,1] = "LHS14"
EggDensR6[116689:125664,2] = DensR6[,6] #egg density per HA
mean(EggDensR6[116689:125664,2]); median(EggDensR6[116689:125664,2])
EggDensR6[125665:134640,1] = "LHS15"
EggDensR6[125665:134640,2] = DensR6[,6] #egg density per HA
mean(EggDensR6[125665:134640,2]); median(EggDensR6[125665:134640,2])
EggDensR6[134641:143616,1] = "LHS16"
EggDensR6[134641:143616,2] = DensR6[,6] #egg density per HA
mean(EggDensR6[134641:143616,2]); median(EggDensR6[134641:143616,2])
EggDensR6[143617:152592,1] = "LHS17"
EggDensR6[143617:152592,2] = DensR6[,6] #egg density per HA
mean(EggDensR6[143617:152592,2]); median(EggDensR6[143617:152592,2])
EggDensR6[152593:161568,1] = "LHS18"
EggDensR6[152593:161568,2] = DensR6[,6] #egg density per HA
mean(EggDensR6[152593:161568,2]); median(EggDensR6[152593:161568,2])

#Figure
hist(EggDensR6[152593:161568,2], breaks=40, xlab="Egg Density", xlim=c(0,1000), ylim=c(0,6000), main="Perceptual Range = 50")
abline(v=113.1, lty=2, lwd=3)
median(EggDensR6[152593:161568,2])

EggDensR6[161569:170544,1] = "LHS19"
EggDensR6[161569:170544,2] = DensR6[,6] #egg density per HA
mean(EggDensR6[161569:170544,2]); median(EggDensR6[161569:170544,2])
EggDensR6[170545:179520,1] = "LHS20"
EggDensR6[170545:179520,2] = DensR6[,6] #egg density per HA
mean(EggDensR6[170545:179520,2]); median(EggDensR6[161569:170544,2])
EggDensR6[179521:188496,1] = "LHS21"
EggDensR6[179521:188496,2] = DensR6[,6] #egg density per HA
mean(EggDensR6[179521:188496,2]); median(EggDensR6[179521:188496,2])
EggDensR6[188497:197472,1] = "LHS22"
EggDensR6[188497:197472,2] = DensR6[,6] #egg density per HA
mean(EggDensR6[188497:197472,2]); median(EggDensR6[188497:197472,2])
EggDensR6[197473:206448,1] = "LHS23"
EggDensR6[197473:206448,2] = DensR6[,6] #egg density per HA
mean(EggDensR6[197473:206448,2]); median(EggDensR6[197473:206448,2])
EggDensR6[206449:215424,1] = "LHS24"
EggDensR6[206449:215424,2] = DensR6[,6] #egg density per HA
mean(EggDensR6[206449:215424,2]); median(EggDensR6[206449:215424,2])
EggDensR6[215425:224400,1] = "LHS25"
EggDensR6[215425:224400,2] = DensR6[,6] #egg density per HA
mean(EggDensR6[215425:224400,2]); median(EggDensR6[215425:224400,2])
EggDensR6[224401:233376,1] = "run230"
EggDensR6[224401:233376,2] = DensR6[,6] #egg density per HA
mean(EggDensR6[224401:233376,2]); median(EggDensR6[224401:233376,2])
EggDensR6[233377:242352,1] = "run236"
EggDensR6[233377:242352,2] = DensR6[,6] #egg density per HA
EggDensR6[242353:251328,1] = "run235"
EggDensR6[242353:251328,2] = DensR6[,6] #egg density per HA



## plot histograms to compare perception distances
agr1 = hist(EggDensR6[80785:89760,2], breaks=400, xlab="Egg Density", xlim=c(0,1000), main="Perception Distance = 400")
range(EggDensR6[80785:89760,2])
bgr1 = hist(EggDensR6[152593:161568,2], breaks=40, xlab="Egg Density", xlim=c(0,1000), ylim=c(0,6000), main="Perception Distance = 50")
grid.arrange(agr1,bgr1, ncol=2)#, top = textGrob("Distribution of Egg Density", gp=gpar(fontsize=30)))
#try library lattice for these

#WITH OUTLIERS
p = ggplot(EggDensR6, aes(x=Simulation, y=EggDensity)) + geom_boxplot() + 
  ggtitle("Egg Density in Road Right-of-Way Polygons") +
  #ggtitle(expression(paste("Egg Density in Road Right-of-Way Polygons\n with 60-100+ m2 of Milkweed"))) +
  xlab("Simulation Run") + ylab("Eggs per Hectare") +
  #ylim(0,400) +
  stat_summary(fun.y="mean", geom="point", shape=3, size=3)
p + theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
           axis.title.x = element_text(size = 26, margin=margin(10,0,10,0)),
           axis.title.y = element_text(size = 26, margin=margin(0,10,0,5)),
           axis.text = element_text(size = 20))

#WITHOUT OUTLIERS
p = ggplot(EggDensR6, aes(x=Simulation, y=EggDensity)) + geom_boxplot(outlier.shape = NA) + 
  ggtitle("Egg Density in Road Right-of-Way Polygons") +
  #ggtitle(expression(paste("Egg Density in Road Right-of-Way Polygons\n with 60-100+ m2 of Milkweed"))) +
  xlab("Simulation Run") + ylab("Eggs per Hectare") +
  ylim(0,400) +
  stat_summary(fun.y="mean", geom="point", shape=3, size=3)
p + theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
          axis.title.x = element_text(size = 26, margin=margin(10,0,10,0)),
          axis.title.y = element_text(size = 26, margin=margin(0,10,0,5)),
          axis.text = element_text(size = 20))

############# graph run 236 and run 235 ###############
EggDensR6.2x = rbind(EggDensR6[233377:242352,],EggDensR6[242353:251328,])

#WITH OUTLIERS
p = ggplot(EggDensR6.2x, aes(x=Simulation, y=EggDensity)) + geom_boxplot() + 
  ggtitle("Egg Density in Road Right-of-Way Polygons") +
  #ggtitle(expression(paste("Egg Density in Road Right-of-Way Polygons\n with 60-100+ m2 of Milkweed"))) +
  xlab("Simulation Run") + ylab("Eggs per Hectare") +
  #ylim(0,400) +
  stat_summary(fun.y="mean", geom="point", shape=3, size=3)
p + theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
          axis.title.x = element_text(size = 26, margin=margin(10,0,10,0)),
          axis.title.y = element_text(size = 26, margin=margin(0,10,0,5)),
          axis.text = element_text(size = 20))

#WITH OUTLIERS
p = ggplot(EggDensR6.2x, aes(x=Simulation, y=EggDensity)) + geom_boxplot() + 
  ggtitle("Egg Density in Road Right-of-Way Polygons") +
  #ggtitle(expression(paste("Egg Density in Road Right-of-Way Polygons\n with 60-100+ m2 of Milkweed"))) +
  xlab("Simulation Run") + ylab("Eggs per Hectare") +
  #ylim(0,400) +
  stat_summary(fun.y="mean", geom="point", shape=3, size=3)
p + theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
          axis.title.x = element_text(size = 26, margin=margin(10,0,10,0)),
          axis.title.y = element_text(size = 26, margin=margin(0,10,0,5)),
          axis.text = element_text(size = 20))



############# split probs TEST CASE #######################
#egg density in MWROW60+

spliteggs.densR6 = rbind(EggDensR6[71809:80784,],EggDensR6[224401:233376,],EggDensR6[233377:242352,])

p = ggplot(spliteggs.densR6, aes(x=Simulation, y=EggDensity)) + geom_boxplot() + 
  ggtitle("Egg Density in Roadsides") +
  #ggtitle(expression(paste("Egg Density in Road Right-of-Way Polygons\n with 60-100+ m2 of Milkweed"))) +
  xlab("Simulation Run") + ylab("Eggs per Hectare") +
  scale_x_discrete(labels=c("Original","Case 1","Case 2")) +
  #ylim(0,400) +
  stat_summary(fun.y="mean", geom="point", shape=3, size=3)
p + theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 26, margin=margin(0,10,0,5)),
          axis.text = element_text(size = 20))




############## Grass/Pasture egg density graph ###################

EggDensGP = data.frame(matrix(nrow=10, ncol=2))
colnames(EggDensGP) = c("Simulation","EggDensity")
EggDensGP$Simulation = factor(EggDensGP$Simulation)
levels(EggDensGP$Simulation) = c("LHS1","LHS2","LHS3","LHS4","LHS5","LHS6","LHS7","LHS8","LHS9","LHS10","LHS11","LHS12",
                                 "LHS13","LHS14","LHS15","LHS16","LHS17","LHS18","LHS19","LHS20","LHS21","LHS22","LHS23",
                                 "LHS24","LHS25","run230","run236")

# 1920 polygons
EggDensGP[1:1920,1] = "LHS1"
EggDensGP[1:1920,2] = DensGP[,6]
mean(EggDensGP[1:1920,2]);median(EggDensGP[1:1920,2])
EggDensGP[1921:3840,1] = "LHS2"
EggDensGP[1921:3840,2] = DensGP[,6]
mean(DensGP[,6]);median(DensGP[,6]); hist(EggDensGP[1921:3840,2])
EggDensGP[3841:5760,1] = "LHS3"
EggDensGP[3841:5760,2] = DensGP[,6]
mean(EggDensGP[3841:5760,2]);median(EggDensGP[3841:5760,2])
EggDensGP[5761:7680,1] = "LHS4"
EggDensGP[5761:7680,2] = DensGP[,6]
mean(EggDensGP[5761:7680,2]);median(EggDensGP[5761:7680,2])
EggDensGP[7681:9600,1] = "LHS5"
EggDensGP[7681:9600,2] = DensGP[,6]
mean(EggDensGP[7681:9600,2]);median(EggDensGP[7681:9600,2])
EggDensGP[9601:11520,1] = "LHS6"
EggDensGP[9601:11520,2] = DensGP[,6]
mean(EggDensGP[9601:11520,2]);median(EggDensGP[9601:11520,2])
EggDensGP[11521:13440,1] = "LHS7"
EggDensGP[11521:13440,2] = DensGP[,6]
mean(EggDensGP[11521:13440,2]);median(EggDensGP[11521:13440,2])
EggDensGP[13441:15360,1] = "LHS8"
EggDensGP[13441:15360,2] = DensGP[,6]
mean(EggDensGP[13441:15360,2]);median(EggDensGP[13441:15360,2])
EggDensGP[15361:17280,1] = "LHS9"
EggDensGP[15361:17280,2] = DensGP[,6]
mean(EggDensGP[15361:17280,2]);median(EggDensGP[15361:17280,2])
EggDensGP[17281:19200,1] = "LHS10"
EggDensGP[17281:19200,2] = DensGP[,6]
mean(EggDensGP[17281:19200,2]);median(EggDensGP[17281:19200,2])
EggDensGP[19201:21120,1] = "LHS11"
EggDensGP[19201:21120,2] = DensGP[,6]
mean(EggDensGP[19201:21120,2]);median(EggDensGP[19201:21120,2])
EggDensGP[21121:23040,1] = "LHS12"
EggDensGP[21121:23040,2] = DensGP[,6]
mean(EggDensGP[21121:23040,2]);median(EggDensGP[21121:23040,2])
EggDensGP[23041:24960,1] = "LHS13"
EggDensGP[23041:24960,2] = DensGP[,6]
mean(EggDensGP[23041:24960,2]);median(EggDensGP[23041:24960,2])
EggDensGP[24961:26880,1] = "LHS14"
EggDensGP[24961:26880,2] = DensGP[,6]
mean(EggDensGP[24961:26880,2]);median(EggDensGP[24961:26880,2])
EggDensGP[26881:28800,1] = "LHS15"
EggDensGP[26881:28800,2] = DensGP[,6]
mean(EggDensGP[26881:28800,2]);median(EggDensGP[26881:28800,2])
EggDensGP[28801:30720,1] = "LHS16"
EggDensGP[28801:30720,2] = DensGP[,6]
mean(EggDensGP[28801:30720,2]);median(EggDensGP[28801:30720,2])
EggDensGP[30721:32640,1] = "LHS17"
EggDensGP[30721:32640,2] = DensGP[,6]
mean(EggDensGP[30721:32640,2]);median(EggDensGP[30721:32640,2])
EggDensGP[32641:34560,1] = "LHS18"
EggDensGP[32641:34560,2] = DensGP[,6]
mean(EggDensGP[32641:34560,2]);median(EggDensGP[32641:34560,2])
EggDensGP[34561:36480,1] = "LHS19"
EggDensGP[34561:36480,2] = DensGP[,6]
mean(EggDensGP[34561:36480,2]);median(EggDensGP[34561:36480,2])
EggDensGP[36481:38400,1] = "LHS20"
EggDensGP[36481:38400,2] = DensGP[,6]
mean(EggDensGP[36481:38400,2]);median(EggDensGP[36481:38400,2])
EggDensGP[38401:40320,1] = "LHS21"
EggDensGP[38401:40320,2] = DensGP[,6]
mean(EggDensGP[38401:40320,2]);median(EggDensGP[38401:40320,2])
EggDensGP[40321:42240,1] = "LHS22"
EggDensGP[40321:42240,2] = DensGP[,6]
mean(EggDensGP[40321:42240,2]);median(EggDensGP[40321:42240,2])
EggDensGP[42241:44160,1] = "LHS23"
EggDensGP[42241:44160,2] = DensGP[,6]
mean(EggDensGP[42241:44160,2]);median(EggDensGP[42241:44160,2])
EggDensGP[44161:46080,1] = "LHS24"
EggDensGP[44161:46080,2] = DensGP[,6]
mean(EggDensGP[44161:46080,2]);median(EggDensGP[44161:46080,2])
EggDensGP[46081:48000,1] = "LHS25"
EggDensGP[46081:48000,2] = DensGP[,6]
mean(EggDensGP[46081:48000,2]);median(EggDensGP[46081:48000,2])
EggDensGP[48001:49920,1] = "run230"
EggDensGP[48001:49920,2] = DensGP[,6]
mean(EggDensGP[48001:49920,2]);median(EggDensGP[48001:49920,2])
EggDensGP[49921:51840,1] = "run236"
EggDensGP[49921:51840,2] = DensGP[,6]


#WITHOUT OUTLIERS
p = ggplot(EggDensGP, aes(x=Simulation, y=EggDensity)) + geom_boxplot(outlier.shape = NA) + 
  ggtitle("Egg Density in Grass/Pasture Polygons") +
  #ggtitle(expression(paste("Egg Density in Road Right-of-Way Polygons\n with 60-100+ m2 of Milkweed"))) +
  xlab("Simulation Run") + ylab("Eggs per Hectare") +
  ylim(0,400) +
  stat_summary(fun.y="mean", geom="point", shape=3, size=3)
p + theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
          axis.title.x = element_text(size = 26, margin=margin(10,0,10,0)),
          axis.title.y = element_text(size = 26, margin=margin(0,10,0,5)),
          axis.text = element_text(size = 16))
#WITH OUTLIERS
p = ggplot(EggDensGP, aes(x=Simulation, y=EggDensity)) + geom_boxplot() + 
  ggtitle("Egg Density in Grass/Pasture Polygons") +
  #ggtitle(expression(paste("Egg Density in Road Right-of-Way Polygons\n with 60-100+ m2 of Milkweed"))) +
  xlab("Simulation Run") + ylab("Eggs per Hectare") +
  #ylim(0,400) +
  stat_summary(fun.y="mean", geom="point", shape=3, size=3)
p + theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
          axis.title.x = element_text(size = 26, margin=margin(10,0,10,0)),
          axis.title.y = element_text(size = 26, margin=margin(0,10,0,5)),
          axis.text = element_text(size = 16))



############# split probs TEST CASE #######################
#egg density in G/P

spliteggs.densGP = rbind(EggDensGP[15361:17280,],EggDensGP[48001:49920,],EggDensGP[49921:51840,])

p = ggplot(spliteggs.densGP, aes(x=Simulation, y=EggDensity)) + geom_boxplot() + 
  ggtitle("Egg Density in Grass/Pasture") +
  #ggtitle(expression(paste("Egg Density in Road Right-of-Way Polygons\n with 60-100+ m2 of Milkweed"))) +
  #xlab("Simulation Run") + 
  ylab("Eggs per Hectare") +
  scale_x_discrete(labels=c("Original","Case 1","Case 2")) +
  #ylim(0,400) +
  stat_summary(fun.y="mean", geom="point", shape=3, size=3)
p + theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 26, margin=margin(0,10,0,5)),
          axis.text = element_text(size = 20))



############################# Utilization Distribution ################################

load("area153.RData")
load("area174.RData")
load("area176.RData")
load("area178.RData")
load("area180.RData")
load("area182.RData")
load("area184.RData")
load("area186.RData")
load("area188.RData")
load("area192.RData")
load("area193.RData")
load("area197.RData")
load("area198.RData")
load("area201.RData")
load("area203.RData")
load("area205.RData")
load("area208.RData")
load("area211.RData")
load("area213.RData")
load("area215.RData")
load("area218.RData")
load("area220.RData")
load("area222.RData")
load("area224.RData")
load("area226.RData")
load("area231.RData")
load("area232.RData")
load("area234.RData")

#area 153 - LHS2
area153b = area153[!is.na(area153)] #remove the NA
mean(area153b); pop.sd(area153b); median(area153b); hist(area153b); plot(density(area153b))
sort(area153b)[1:20]; sort(area153b)[(length(area153b)-20):length(area153b)]

#area 174 - LHS1
mean(area174); pop.sd(area174); median(area174); hist(area174); plot(density(area174)); hist(area174, breaks=20);
sort(area174)[1:50]; sort(area174)[(length(area174)-20):length(area174)]
#25 on lower end didn't follow the distribution
area174b = sort(area174)[26:1000]
mean(area174b); pop.sd(area174b); median(area174b); hist(area174b); plot(density(area174b)); hist(area174b, breaks=20);
sort(area174b)[1:50]; sort(area174b)[(length(area174b)-20):length(area174b)]

#area 176
mean(area176); pop.sd(area176); median(area176); hist(area176); plot(density(area176)); hist(area176, breaks=20);
sort(area176)[1:50]; sort(area176)[(length(area176)-20):length(area176)]
#first 41 don't follow distr
area176b = sort(area176)[42:1000]
mean(area176b); pop.sd(area176b); median(area176b); hist(area176b); plot(density(area176b)); hist(area176b, breaks=20);
sort(area176b)[1:50]; sort(area176b)[(length(area176b)-20):length(area176b)]

#area 178
mean(area178); pop.sd(area178); median(area178); hist(area178); plot(density(area178)); hist(area178, breaks=20);
sort(area178)[1:20]; sort(area178)[(length(area178)-20):length(area178)]

#area 180
mean(area180); pop.sd(area180); median(area180); hist(area180); plot(density(area180))
sort(area180)[1:20]; sort(area180)[(length(area180)-20):length(area180)]

#area 182
mean(area182); pop.sd(area182); median(area182); hist(area182); plot(density(area182))
sort(area182)[1:20]; sort(area182)[(length(area182)-20):length(area182)]

#area 184
mean(area184); pop.sd(area184); median(area184); hist(area184); plot(density(area184))
sort(area184)[1:20]; sort(area184)[(length(area184)-20):length(area184)]

#area 186
mean(area186); pop.sd(area186); median(area186); hist(area186, breaks=20); plot(density(area186))
sort(area186)[1:20]; sort(area186)[(length(area186)-20):length(area186)]

#area 188
area188b = area188[!is.na(area188)] #remove the NA
mean(area188b); pop.sd(area188b); median(area188b); hist(area188b, breaks=20); plot(density(area188b))
sort(area188b)[1:20]; sort(area188b)[(length(area188b)-20):length(area188b)]
#removing 8 outliers - should have a look at them and see what happened because this is 30m step length
area188c = sort(area188b)[9:999]
mean(area188c); pop.sd(area188c); median(area188c); hist(area188c, breaks=20); plot(density(area188c))
sort(area188c)[1:20]; sort(area188c)[(length(area188c)-20):length(area188c)]

#area 192
mean(area192); pop.sd(area192); median(area192); hist(area192, breaks=20); plot(density(area192))
sort(area192)[1:20]; sort(area192)[(length(area192)-20):length(area192)]

#area 193
mean(area193); pop.sd(area193); median(area193); hist(area193, breaks=20); plot(density(area193))
sort(area193)[1:10]; sort(area193)[(length(area193)-20):length(area193)]; hist(sort(area193)[10:1000])
#removed first 9
area193b = sort(area193)[10:1000]
mean(area193b); pop.sd(area193b); median(area193b); hist(area193b, breaks=20); plot(density(area193b))

#area 197
mean(area197); pop.sd(area197); median(area197); hist(area197, breaks=20); plot(density(area197))
sort(area197)[1:10]; sort(area197)[(length(area197)-20):length(area197)]
mean(sort(area197)[5:1000])

#area 198
mean(area198); pop.sd(area198); median(area198); hist(area198, breaks=20); plot(density(area198))
sort(area198)[1:10]; sort(area198)[(length(area198)-20):length(area198)]

#area 201
mean(area201); pop.sd(area201); median(area201); hist(area201, breaks=20); plot(density(area201))
sort(area201)[1:10]; sort(area201)[(length(area201)-20):length(area201)]

#area 203
mean(area203); pop.sd(area203); median(area203); hist(area203, breaks=20); plot(density(area203))
sort(area203)[1:10]; sort(area203)[(length(area203)-20):length(area203)]

#area 205
mean(area205); pop.sd(area205); median(area205); hist(area205, breaks=20); plot(density(area205))
sort(area205)[1:20]; sort(area205)[(length(area205)-20):length(area205)]; hist(sort(area205)[0:10])
#removed first 10
area205b = sort(area205)[11:1000]
mean(area205b); pop.sd(area205b); median(area205b); hist(area205b, breaks=20); plot(density(area205b))
sort(area205b)[1:20]; sort(area205b)[(length(area205b)-20):length(area205b)]; hist(sort(area205b)[0:10])

#area 208
mean(area208); pop.sd(area208); median(area208); hist(area208, breaks=20); plot(density(area208))
sort(area208)[1:20]; sort(area208)[(length(area208)-20):length(area208)]; hist(sort(area208)[0:10])

#area 211
mean(area211); pop.sd(area211); median(area211); hist(area211, breaks=20); plot(density(area211))
sort(area211)[1:20]; sort(area211)[(length(area211)-20):length(area211)]; hist(sort(area211)[0:10])

#area 213
mean(area213); pop.sd(area213); median(area213); hist(area213, breaks=20); plot(density(area213))
sort(area213)[1:20]; sort(area213)[(length(area213)-20):length(area213)]

#area 215
mean(area215); pop.sd(area215); median(area215); hist(area215, breaks=20); plot(density(area215))
sort(area215)[1:20]; sort(area215)[(length(area215)-20):length(area215)]

#area 218
mean(area218); pop.sd(area218); median(area218); hist(area218, breaks=20); plot(density(area218))
sort(area218)[1:20]; sort(area218)[(length(area218)-20):length(area218)]

#area 218b
area218b = sort(area218)[91:1000]
mean(area218b); pop.sd(area218b); median(area218b); hist(area218b, breaks=20); plot(density(area218b))
sort(area218b)[1:20]; sort(area218b)[(length(area218b)-20):length(area218b)]

#area 220
mean(area220); pop.sd(area220); median(area220); hist(area220, breaks=20); plot(density(area220))
sort(area220)[1:20]; sort(area220)[(length(area220)-20):length(area220)]

#area 222
mean(area222); pop.sd(area222); median(area222); hist(area222, breaks=20); plot(density(area222))
sort(area222)[1:20]; sort(area222)[(length(area222)-20):length(area222)]

#area 224
mean(area224); pop.sd(area224); median(area224); hist(area224, breaks=20); plot(density(area224))
sort(area224)[1:20]; sort(area224)[(length(area224)-20):length(area224)]

#area 226
mean(area226); pop.sd(area226); median(area226); hist(area226, breaks=20); plot(density(area226))
sort(area226)[1:20]; sort(area226)[(length(area226)-20):length(area226)]

#area 231
mean(area231); pop.sd(area231); median(area231); hist(area231, breaks=20); plot(density(area231))
sort(area231)[1:20]; sort(area231)[(length(area231)-20):length(area231)]
area231b = sort(area231)[5:1000]
mean(area231b); pop.sd(area231b); median(area231b); hist(area231b, breaks=20); plot(density(area231b))

#area 232
mean(area232); pop.sd(area232); median(area232); hist(area232, breaks=20); plot(density(area232))
sort(area232)[1:20]; sort(area232)[(length(area232)-20):length(area232)]

#area 234
mean(area234); pop.sd(area234); median(area234); hist(area234, breaks=20); plot(density(area234))
sort(area234)[1:20]; sort(area234)[(length(area234)-20):length(area234)]
area234b = sort(area234)[6:1000]
mean(area234b); pop.sd(area234b); median(area234b); hist(area234b, breaks=20); plot(density(area234b))

load("dbbmms153.RData")
load("dbbmms182.RData")

#LHS 10 - use for illustrative purposes
load("dbbmms192.RData")

#Plot dbbmms

dbbmms153[1]
#plot contours from dbbmms - will only plot 100 at a time
for (i in 1:100){
  contour(dbbmms192[[i]], levels=c(0.5, 0.95), col=c(6,2), lwd=2, main=i)#, add=TRUE)
}
#plot points - usually leave allmons_t on speedy2 though
plot(allmons_t[[389]], pch=20, cex=0.5)





#plot of UDs

UDsdf = data.frame(matrix(nrow=10, ncol=2))
colnames(UDsdf) = c("Simulation","Area")
UDsdf$Simulation = factor(UDsdf$Simulation)
levels(UDsdf$Simulation) = c("LHS1","LHS2","LHS3","LHS4","LHS5","LHS6","LHS7","LHS8","LHS9","LHS10","LHS11","LHS12","LHS13",
                             "LHS14","LHS15","LHS16","LHS17","LHS18","LHS19","LHS20","run231","run232","run234","run234b")

UDsdf[1:975,1] = "LHS1"
UDsdf[1:975,2] = area174b
mean(UDsdf[1:975,2]);median(UDsdf[1:975,2])
UDsdf[976:1974,1] = "LHS2"
UDsdf[976:1974,2] = area153b
mean(UDsdf[976:1974,2]);median(UDsdf[976:1974,2])
UDsdf[1975:2933,1] = "LHS3"
UDsdf[1975:2933,2] = area176b
mean(UDsdf[1975:2933,2]);median(UDsdf[1975:2933,2])
UDsdf[2934:3933,1] = "LHS4"
UDsdf[2934:3933,2] = area178
mean(UDsdf[2934:3933,2]);median(UDsdf[2934:3933,2])
UDsdf[3934:4933,1] = "LHS5"
UDsdf[3934:4933,2] = area180
mean(UDsdf[3934:4933,2]);median(UDsdf[3934:4933,2])
UDsdf[4934:5933,1] = "LHS6"
UDsdf[4934:5933,2] = area182
mean(UDsdf[4934:5933,2]);median(UDsdf[4934:5933,2])
UDsdf[5934:6933,1] = "LHS7"
UDsdf[5934:6933,2] = area184
mean(UDsdf[5934:6933,2]);median(UDsdf[5934:6933,2])
UDsdf[6934:7933,1] = "LHS8"
UDsdf[6934:7933,2] = area186
mean(UDsdf[6934:7933,2]);median(UDsdf[6934:7933,2])
UDsdf[7934:8924,1] = "LHS9"
UDsdf[7934:8924,2] = area188c
mean(UDsdf[7934:8924,2]);median(UDsdf[7934:8924,2])
UDsdf[8925:9924,1] = "LHS10"
UDsdf[8925:9924,2] = area192
mean(UDsdf[8925:9924,2]);median(UDsdf[8925:9924,2])
UDsdf[9925:10915,1] = "LHS11"
UDsdf[9925:10915,2] = area193b
mean(UDsdf[9925:10915,2]);median(UDsdf[9925:10915,2])
UDsdf[10916:11915,1] = "LHS12"
UDsdf[10916:11915,2] = area197
mean(UDsdf[10916:11915,2]);median(UDsdf[10916:11915,2])
UDsdf[11916:12915,1] = "LHS13"
UDsdf[11916:12915,2] = area198
mean(UDsdf[11916:12915,2]);median(UDsdf[11916:12915,2])
UDsdf[12916:13911,1] = "run231"
UDsdf[12916:13911,2] = area231b
UDsdf[13912:14911,1] = "run232"
UDsdf[13912:14911,2] = area232
UDsdf[14912:15911,1] = "run234"
UDsdf[14912:15911,2] = area234
UDsdf[15912:16906,1] = "run234b"
UDsdf[15912:16906,2] = area234b
mean(UDsdf[15912:16906,2])



#geom_boxplot(outlier.shape = NA)
p = ggplot(UDsdf, aes(x=Simulation, y=Area)) + geom_boxplot() + 
  ggtitle("Area of Utilization Distribution") +
  #xlab("Simulation Run") + ylab("Area (m2)") +
  #ylim(0,400) +
  stat_summary(fun.y="mean", geom="point", shape=3, size=3)
p + theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
          axis.title.x = element_text(size = 26, margin=margin(10,0,10,0)),
          axis.title.y = element_text(size = 26, margin=margin(0,10,0,5)),
          axis.text = element_text(size = 20))



######## compare UDs when probEggs is split
spliteggs.UDs = rbind(UDsdf[7934:8924,],UDsdf[12916:13911,],UDsdf[15912:16906,])
#convert to HA
spliteggs.UDsHA = spliteggs.UDs
spliteggs.UDsHA[,2] = spliteggs.UDs[,2]*30*30/10000


p1 = ggplot(spliteggs.UDsHA, aes(x=Simulation, y=Area)) + geom_boxplot() + 
  ggtitle("Area of Utilization Distribution") +
  #xlab("Simulation Run") + 
  ylab("Area (Ha)") +
  #ylim(0,400) +
  scale_x_discrete(labels=c("Original","Case 1","Case 2")) +
  stat_summary(fun.y="mean", geom="point", shape=3, size=3)
p1 + theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 26, margin=margin(0,10,0,5)),
          axis.text = element_text(size = 20))







