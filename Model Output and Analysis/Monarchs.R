#########  Monarch Simulations for Sensitivity Analysis  #####################


library(dplyr)
library(ggplot2)
library(move)
library(ggmap)
library(mapproj)
#library(adehabitat)
library(adehabitatHR)
pop.sd=function(x)(sqrt(var(x)*(length(x)-1)/length(x)))
#options(stringsAsFactors = FALSE)
#setwd("~/Repast/Monarchs/output")


######## TOC #############

# 1.  Mean eggs laid per monarch and graphs
# 2.  Egg Density and graphs
# 3.  Utilization distribution and graphs
# 4.  Time Budget
# 5.  Latin Hypercube sampling and parameter combos to run


############# summary statistics for Monarch Agents for Mean Eggs Laid SA ###################

#Create dataframe of results to output to csv
results = data.frame(matrix(nrow=10, ncol=5))
colnames(results) = c("Tick", "MeanEggsLaid", "SDEggsLaid","MeanEggsNotLaid","SDEggsNotLaid")
rownames(results) = 1:10
results$Tick = 1:10

#code to read in .txt files from batch runs
run1 = read.csv("Monarchs.2016.Jan.07.09_44_48.txt")
run2 = read.csv("Monarchs.2016.Jan.07.10_00_45.txt")
run3 = read.csv("Monarchs.2016.Jan.07.10_11_26.txt")
run4 = read.csv("Monarchs.2016.Jan.07.10_21_19.txt")
run5 = read.csv("Monarchs.2016.Jan.07.10_30_26.txt")
run6 = read.csv("Monarchs.2016.Jan.07.13_32_22.txt")
run7 = read.csv("Monarchs.2016.Jan.07.13_50_12.txt")
run8 = read.csv("Monarchs.2016.Jan.07.14_31_06.txt")
run9 = read.csv("Monarchs.2016.Jan.08.06_32_38.txt")
run10 = read.csv("Monarchs.2016.Jan.08.12_40_19.txt")
run11 = read.csv("Monarchs.2016.Jan.08.12_50_21.txt")
run12 = read.csv("Monarchs.2016.Jan.08.14_08_23.txt")
run13 = read.csv("Monarchs.2016.Jan.11.09_12_37.txt")
run14 = read.csv("Monarchs.2016.Jan.11.09_38_27.txt")
run15 = read.csv("Monarchs.2016.Jan.11.11_12_11.txt")
run16 = read.csv("Monarchs.2016.Jan.11.11_21_00.txt")
run17 = read.csv("Monarchs.2016.Jan.11.12_35_08.txt")
run18 = read.csv("Monarchs.2016.Jan.11.12_42_15.txt")
run19 = read.csv("Monarchs.2016.Jan.11.15_31_21.txt")
run20 = read.csv("Monarchs.2016.Jan.12.10_47_59.txt")
run21 = read.csv("Monarchs.2016.Jan.12.11_26_48.txt")
run22 = read.csv("Monarchs.2016.Jan.14.16_05_46.txt")
run48 = read.csv("Monarchs.2016.Mar.01.19_38_25.txt") #30m step
run44 = read.csv("Monarchs.2016.Mar.01.12_47_57.txt") #50m step
run50 = read.csv("Monarchs.2016.Mar.02.12_34_14.txt") #20m step - 5,000
run51 = read.csv("Monarchs.2016.Mar.02.16_26_51.txt") #20m step - 5,000
run50.51 = rbind(run50,run51)
#runs 40,52-55 are 10m step length, 1000 each
run40 = read.csv("Monarchs.2016.Feb.29.13_32_28.txt")
run52 = read.csv("Monarchs.2016.Mar.02.19_57_36.txt")
run53 = read.csv("Monarchs.2016.Mar.03.01_54_53.txt")
run54 = read.csv("Monarchs.2016.Mar.03.13_41_09.txt")
run55 = read.csv("Monarchs.2016.Mar.03.17_28_41.txt")
run10m = rbind(run40,run52,run53,run54,run55)
run66 = read.csv("Monarchs.2016.Mar.08.15_11_16.txt")
run62 = read.csv("Monarchs.2016.Mar.04.23_20_00.txt")
run63 = read.csv("Monarchs.2016.Mar.08.02_58_20.txt")
run83 = read.csv("Monarchs.2016.Mar.18.01_19_35.txt")
run82 = read.csv("Monarchs.2016.Mar.17.21_08_46.txt")
run79 = read.csv("Monarchs.2016.Mar.16.02_58_53.txt")
run119 = read.csv("Monarchs.2016.Apr.20.11_44_26.txt") #rem=10
run122 = read.csv("Monarchs.2016.Apr.20.16_12_57.txt") #rem=40
run123 = read.csv("Monarchs.2016.Apr.20.19_56_18.txt") #rem=100

#change to current data to summarize
d = run123
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
hist(st10$EggsLaid)

#function to calc population standard deviation (with examples from Wikipedia)
vec=c(2,4,4,4,5,5,7,9)
pop.sd=function(x)(sqrt(var(x)*(length(x)-1)/length(x)))
pop.sd(vec) ##as compared to
sd(vec)

#code to save and load object - object that is automatically created by RS GUI
save(CUserstgrantDocumentsMonarchButterfliesModellingwithRepastSimphonyModelOutputMonarchs22015Dec15072126txt, 
     file = "2015Dec15072126.RData")
load("2015Dec15072126.RData")


#############  Publication Quality Graphs - Step Length Sensitivity  ############################

library(ggplot2)

###### directionality local sensitivity round 2 - runs  - this one used in ms. #############
dirSA.2 = data.frame(matrix(nrow=40000, ncol=3))
colnames(dirSA.2) = c("Directionality","EggsLaid","PropEggsLaid")
dirSA.2$Directionality = factor(dirSA.2$Directionality)
levels(dirSA.2$Directionality) = c("0.1-0.2","0.1-0.9","0.5-0.75","0.8-0.9")

#run 83 - directionality = 0.1-0.2
dirSA.2[1:10000,1] = "0.1-0.2"
dirSA.2[1:10000,2] = st5$EggsLaid 
dirSA.2[1:10000,3] = st5$EggsLaid[1:10000]/42 #42 possible eggs to lay on Day 5
hist(dirSA.2[1:10000,3]); mean(dirSA.2[1:10000,3]); median(dirSA.2[1:10000,3]); t.test(dirSA.2[1:10000,3])$conf.int
#run 82 - directionality = 0.1-0.9
dirSA.2[10001:20000,1] = "0.1-0.9"
dirSA.2[10001:20000,2] = st5$EggsLaid 
dirSA.2[10001:20000,3] = st5$EggsLaid[1:10000]/42 #42 possible eggs to lay on Day 5
#run 48 - directionality = 0.5-0.75
dirSA.2[20001:30000,1] = "0.5-0.75"
dirSA.2[20001:30000,2] = st5$EggsLaid 
dirSA.2[20001:30000,3] = st5$EggsLaid[1:10000]/42 #42 possible eggs to lay on Day 5
#run 79 - directionality = 0.8-0.9
dirSA.2[30001:40000,1] = "0.8-0.9"
dirSA.2[30001:40000,2] = st5$EggsLaid 
dirSA.2[30001:40000,3] = st5$EggsLaid[1:10000]/42 #42 possible eggs to lay on Day 5

p = ggplot(dirSA.2, aes(x=Directionality, y=PropEggsLaid)) + geom_boxplot() + ggtitle("Proportion of Eggs Laid - Day 5") +
  xlab("Directionality") + ylab("Proportion of Eggs Laid") +
  stat_summary(fun.y="mean", geom="point", shape=3, size=3)
p + theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
          axis.title.x = element_text(size = 26, margin=margin(10,0,10,0)),
          axis.title.y = element_text(size = 26, margin=margin(0,10,0,5)),
          axis.text = element_text(size = 20))

###### perception local sensitivity round 2 - runs  - this one used in ms. #############
perceptionSA.2 = data.frame(matrix(nrow=40000, ncol=3))
colnames(perceptionSA.2) = c("Perception","EggsLaid","PropEggsLaid")
perceptionSA.2$Perception = factor(perceptionSA.2$Perception)
levels(perceptionSA.2$Perception) = c(50,100,200,400)

#run 66 - perception = 50
perceptionSA.2[1:10000,1] = 50 
perceptionSA.2[1:10000,2] = st5$EggsLaid 
perceptionSA.2[1:10000,3] = st5$EggsLaid[1:10000]/42 #42 possible eggs to lay on Day 5
#run 48 - perception = 100
perceptionSA.2[10001:20000,1] = 100
perceptionSA.2[10001:20000,2] = st5$EggsLaid 
perceptionSA.2[10001:20000,3] = st5$EggsLaid[1:10000]/42 #42 possible eggs to lay on Day 5
#run 62 - perception = 200
perceptionSA.2[20001:30000,1] = 200
perceptionSA.2[20001:30000,2] = st5$EggsLaid 
perceptionSA.2[20001:30000,3] = st5$EggsLaid[1:10000]/42 #42 possible eggs to lay on Day 5
#run 63 - perception = 400
perceptionSA.2[30001:40000,1] = 400
perceptionSA.2[30001:40000,2] = st5$EggsLaid 
perceptionSA.2[30001:40000,3] = st5$EggsLaid[1:10000]/42 #42 possible eggs to lay on Day 5

p = ggplot(perceptionSA.2, aes(x=Perception, y=PropEggsLaid)) + geom_boxplot() + ggtitle("Proportion of Eggs Laid - Day 5") +
  xlab("Perception Distance") + ylab("Proportion of Eggs Laid") +
  stat_summary(fun.y="mean", geom="point", shape=3, size=3)
p + theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
          axis.title.x = element_text(size = 26, margin=margin(10,0,10,0)),
          axis.title.y = element_text(size = 26, margin=margin(0,10,0,5)),
          axis.text = element_text(size = 20))

###### rememebered local sensitivity round 2 - runs  - this one used in ms. #############
rememberedSA.2 = data.frame(matrix(nrow=40000, ncol=3))
colnames(rememberedSA.2) = c("Remembered","EggsLaid","PropEggsLaid")
rememberedSA.2$Remembered = factor(rememberedSA.2$Remembered)
levels(rememberedSA.2$Remembered) = c(0,10,40,100)

#run 48 - remembered = 0
rememberedSA.2[1:10000,1] = 0 
rememberedSA.2[1:10000,2] = st5$EggsLaid 
rememberedSA.2[1:10000,3] = st5$EggsLaid[1:10000]/42 #42 possible eggs to lay on Day 5
#run 119 - remembered = 10
rememberedSA.2[10001:20000,1] = 10
rememberedSA.2[10001:20000,2] = st5$EggsLaid 
rememberedSA.2[10001:20000,3] = st5$EggsLaid[1:10000]/42 #42 possible eggs to lay on Day 5
#run 122 - remembered = 40
rememberedSA.2[20001:30000,1] = 40
rememberedSA.2[20001:30000,2] = st5$EggsLaid 
rememberedSA.2[20001:30000,3] = st5$EggsLaid[1:10000]/42 #42 possible eggs to lay on Day 5
#run 123 - remembered = 100
rememberedSA.2[30001:40000,1] = 100
rememberedSA.2[30001:40000,2] = st5$EggsLaid 
rememberedSA.2[30001:40000,3] = st5$EggsLaid[1:10000]/42 #42 possible eggs to lay on Day 5

p = ggplot(rememberedSA.2, aes(x=Remembered, y=PropEggsLaid)) + geom_boxplot() + ggtitle("Proportion of Eggs Laid - Day 5") +
  xlab("Polygons Remembered") + ylab("Proportion of Eggs Laid") +
  stat_summary(fun.y="mean", geom="point", shape=3, size=3)
p + theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
          axis.title.x = element_text(size = 26, margin=margin(10,0,10,0)),
          axis.title.y = element_text(size = 26, margin=margin(0,10,0,5)),
          axis.text = element_text(size = 20))

###### step length sensitivity round 2 - runs 48, 45, 46, 56, 52 - this one used in ms. #############
steplengthSA.2 = data.frame(matrix(nrow=40000, ncol=3))
colnames(steplengthSA.2) = c("StepLength","EggsLaid","PropEggsLaid")
steplengthSA.2$StepLength = factor(steplengthSA.2$StepLength)
levels(steplengthSA.2$StepLength) = c(10,20,30,50)

#run - step length 10
steplengthSA.2[1:10000,1] = 10 
steplengthSA.2[1:10000,2] = st5$EggsLaid 
steplengthSA.2[1:10000,3] = st5$EggsLaid[1:10000]/42 #42 possible eggs to lay on Day 5
#run 50.51 - step length 20
steplengthSA.2[10001:20000,1] = 20
steplengthSA.2[10001:20000,2] = st5$EggsLaid 
steplengthSA.2[10001:20000,3] = st5$EggsLaid[1:10000]/42 #42 possible eggs to lay on Day 5
#run 48 - step length 30
steplengthSA.2[20001:30000,1] = 30
steplengthSA.2[20001:30000,2] = st5$EggsLaid 
steplengthSA.2[20001:30000,3] = st5$EggsLaid[1:10000]/42 #42 possible eggs to lay on Day 5
#run 44 - step length 50
steplengthSA.2[30001:40000,1] = 50
steplengthSA.2[30001:40000,2] = st5$EggsLaid 
steplengthSA.2[30001:40000,3] = st5$EggsLaid[1:10000]/42 #42 possible eggs to lay on Day 5

p = ggplot(steplengthSA.2, aes(x=StepLength, y=PropEggsLaid)) + geom_boxplot() + ggtitle("Proportion of Eggs Laid - Day 5") +
  xlab("Step Length") + ylab("Proportion of Eggs Laid") +
  stat_summary(fun.y="mean", geom="point", shape=3, size=3)
p + theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
          axis.title.x = element_text(size = 26, margin=margin(10,0,10,0)),
          axis.title.y = element_text(size = 26, margin=margin(0,10,0,5)),
          axis.text = element_text(size = 20))


#step length sensitivity - runs 10-14,22 - this time I'm going to used box plots for day 5, since
#percentage laid each day is similar
steplengthSA = data.frame(matrix(nrow=6000, ncol=3))
colnames(steplengthSA) = c("StepLength","EggsLaid","PropEggsLaid")
steplengthSA$StepLength = factor(steplengthSA$StepLength)
levels(steplengthSA$StepLength) = c(10,18,25,30,40,50)
#run 10 - step length 25
steplengthSA[1:1000,1] = 25 
steplengthSA[1:1000,2] = st5$EggsLaid 
steplengthSA[1:1000,3] = st5$EggsLaid[1:1000]/42 #42 possible eggs to lay on Day 5
#run 11 - step length 50
steplengthSA[1001:2000,1] = 50
steplengthSA[1001:2000,2] = st5$EggsLaid 
steplengthSA[1001:2000,3] = st5$EggsLaid[1:1000]/42 #42 possible eggs to lay on Day 5
#run 12 - step length 10
steplengthSA[2001:3000,1] = 10
steplengthSA[2001:3000,2] = st5$EggsLaid 
steplengthSA[2001:3000,3] = st5$EggsLaid[1:1000]/42 #42 possible eggs to lay on Day 5
#run 13 - step length 40
steplengthSA[3001:4000,1] = 40
steplengthSA[3001:4000,2] = st5$EggsLaid 
steplengthSA[3001:4000,3] = st5$EggsLaid[1:1000]/42 #42 possible eggs to lay on Day 5
#run 14 - step length 30
steplengthSA[4001:5000,1] = 30
steplengthSA[4001:5000,2] = st5$EggsLaid 
steplengthSA[4001:5000,3] = st5$EggsLaid[1:1000]/42 #42 possible eggs to lay on Day 5
#run 22 - step length 18
steplengthSA[5001:6000,1] = 18
steplengthSA[5001:6000,2] = st5$EggsLaid 
steplengthSA[5001:6000,3] = st5$EggsLaid[1:1000]/42 #42 possible eggs to lay on Day 5

#quick plot
qplot(steplengthSA$StepLength, steplengthSA$`Proportion of Eggs Laid`, geom="boxplot")
mean(steplengthSA$`Proportion of Eggs Laid`[1:1000])

#pro plot
p = ggplot(steplengthSA, aes(x=StepLength, y=PropEggsLaid)) + geom_boxplot() + ggtitle("Proportion of Eggs Laid") +
  xlab("Step Length") + ylab("Proportion of Eggs Laid") +
  stat_summary(fun.y="mean", geom="point", shape=3, size=3)
p + theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
          axis.title.x = element_text(size = 20, margin=margin(10,0,10,0)),
          axis.title.y = element_text(size = 20, margin=margin(0,10,0,5)),
          axis.text = element_text(size = 12))

#violin plot
ggplot(steplengthSA, aes(x=StepLength, y=PropEggsLaid)) + geom_violin()




#############  Publication Quality Graphs - "Remembered" Sensitivity - OLD  ######################

rememberedSA = data.frame(matrix(nrow=6000, ncol=3))
colnames(rememberedSA) = c("Remembered","EggsLaid","PropEggsLaid")
rememberedSA$Remembered = factor(rememberedSA$Remembered)
levels(rememberedSA$Remembered) = c(1,2,5,10,20,40)
#run 11, 15-19
#run 11 - remembered 10
rememberedSA[1:1000,1] = 10 
rememberedSA[1:1000,2] = st5$EggsLaid 
rememberedSA[1:1000,3] = st5$EggsLaid[1:1000]/42 #42 possible eggs to lay on Day 5
#run 15 - remembered 20
rememberedSA[1001:2000,1] = 20
rememberedSA[1001:2000,2] = st5$EggsLaid 
rememberedSA[1001:2000,3] = st5$EggsLaid[1:1000]/42 #42 possible eggs to lay on Day 5
#run 16 - remembered 5
rememberedSA[2001:3000,1] = 5
rememberedSA[2001:3000,2] = st5$EggsLaid 
rememberedSA[2001:3000,3] = st5$EggsLaid[1:1000]/42 #42 possible eggs to lay on Day 5
#run 17 - remembered 2
rememberedSA[3001:4000,1] = 2
rememberedSA[3001:4000,2] = st5$EggsLaid 
rememberedSA[3001:4000,3] = st5$EggsLaid[1:1000]/42 #42 possible eggs to lay on Day 5
#run 18 - remembered 40
rememberedSA[4001:5000,1] = 40
rememberedSA[4001:5000,2] = st5$EggsLaid 
rememberedSA[4001:5000,3] = st5$EggsLaid[1:1000]/42 #42 possible eggs to lay on Day 5
#run 19 - remembered 1
rememberedSA[5001:6000,1] = 1
rememberedSA[5001:6000,2] = st5$EggsLaid 
rememberedSA[5001:6000,3] = st5$EggsLaid[1:1000]/42 #42 possible eggs to lay on Day 5

p = ggplot(rememberedSA, aes(x=Remembered, y=PropEggsLaid)) + geom_boxplot() + ggtitle("Proportion of Eggs Laid") +
  xlab("Polygons Remembered") + ylab("Proportion of Eggs Laid") +
  stat_summary(fun.y="mean", geom="point", shape=3, size=3)
p + theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
          axis.title.x = element_text(size = 20, margin=margin(10,0,10,0)),
          axis.title.y = element_text(size = 20, margin=margin(0,10,0,5)),
          axis.text = element_text(size = 12))





#############  Publication Quality Graphs - Directionality Sensitivity  ######################

dirSA = data.frame(matrix(nrow=5000, ncol=3))
colnames(dirSA) = c("Directionality","EggsLaid","PropEggsLaid")
dirSA$Directionality = factor(dirSA$Directionality)
levels(dirSA$Directionality) = c(0.10,0.25,0.50,0.75,0.90)
#run 1-5
#run 1 - directionality 0.75
dirSA[1:1000,1] = 0.75 
dirSA[1:1000,2] = st5$EggsLaid 
dirSA[1:1000,3] = st5$EggsLaid[1:1000]/42 #42 possible eggs to lay on Day 5
#run 2 - directionality 0.5
dirSA[1001:2000,1] = 0.50 
dirSA[1001:2000,2] = st5$EggsLaid 
dirSA[1001:2000,3] = st5$EggsLaid[1:1000]/42 #42 possible eggs to lay on Day 5
#run 3 - directionality 0.25
dirSA[2001:3000,1] = 0.25 
dirSA[2001:3000,2] = st5$EggsLaid 
dirSA[2001:3000,3] = st5$EggsLaid[1:1000]/42 #42 possible eggs to lay on Day 5
#run 4 - directionality 0.1
dirSA[3001:4000,1] = 0.10 
dirSA[3001:4000,2] = st5$EggsLaid 
dirSA[3001:4000,3] = st5$EggsLaid[1:1000]/42 #42 possible eggs to lay on Day 5
#run 5 - directionality 0.9
dirSA[4001:5000,1] = 0.90 
dirSA[4001:5000,2] = st5$EggsLaid 
dirSA[4001:5000,3] = st5$EggsLaid[1:1000]/42 #42 possible eggs to lay on Day 5

p = ggplot(dirSA, aes(x=Directionality, y=PropEggsLaid)) + geom_boxplot() + ggtitle("Proportion of Eggs Laid") +
  xlab("Directionality") + ylab("Proportion of Eggs Laid") +
  stat_summary(fun.y="mean", geom="point", shape=3, size=3)
p + theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
          axis.title.x = element_text(size = 20, margin=margin(10,0,10,0)),
          axis.title.y = element_text(size = 20, margin=margin(0,10,0,5)),
          axis.text = element_text(size = 12))





#############  Publication Quality Graphs - Perception Distance Sensitivity  ######################

percSA = data.frame(matrix(nrow=5000, ncol=3))
colnames(percSA) = c("Perception","EggsLaid","PropEggsLaid")
percSA$Perception = factor(percSA$Perception)
levels(percSA$Perception) = c(25,50,100,200,400)
#run 1,6-9
#run 6 - perception 25
percSA[1:1000,1] = 25 
percSA[1:1000,2] = st5$EggsLaid 
percSA[1:1000,3] = st5$EggsLaid[1:1000]/42 #42 possible eggs to lay on Day 5
#run 7 - perception 200
percSA[1001:2000,1] = 200 
percSA[1001:2000,2] = st5$EggsLaid 
percSA[1001:2000,3] = st5$EggsLaid[1:1000]/42 #42 possible eggs to lay on Day 5
#run 8 - perception 400
percSA[2001:3000,1] = 400 
percSA[2001:3000,2] = st5$EggsLaid 
percSA[2001:3000,3] = st5$EggsLaid[1:1000]/42 #42 possible eggs to lay on Day 5
#run 9 - perception 50
percSA[3001:4000,1] = 50 
percSA[3001:4000,2] = st5$EggsLaid 
percSA[3001:4000,3] = st5$EggsLaid[1:1000]/42 #42 possible eggs to lay on Day 5
#run 1 - perception 100
percSA[4001:5000,1] = 100 
percSA[4001:5000,2] = st5$EggsLaid 
percSA[4001:5000,3] = st5$EggsLaid[1:1000]/42 #42 possible eggs to lay on Day 5


p = ggplot(percSA, aes(x=Perception, y=PropEggsLaid)) + geom_boxplot() + ggtitle("Proportion of Eggs Laid") +
  xlab("Perception Distance") + ylab("Proportion of Eggs Laid") +
  stat_summary(fun.y="mean", geom="point", shape=3, size=3)
p + theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
          axis.title.x = element_text(size = 20, margin=margin(10,0,10,0)),
          axis.title.y = element_text(size = 20, margin=margin(0,10,0,5)),
          axis.text = element_text(size = 12))





###############################  Cumulative Eggs and Egg Densities per Zone -   Main Code   ###########################

#Dataframe to fill with results
Denresults = data.frame(matrix(nrow=37165, ncol=9))
colnames(Denresults) = c("PolygonID", "HabType", "CumEggs","PolygonAreaLL","PolygonAream2","EggDensityperHA",
                         "lnEggDensity","log10EggDensity","ProbEggs")
###  Polygon Areas
PolyArea = read.csv("PolygonAream^2.csv")  #724 polygons from test polygon
StPolyArea = read.csv("polygon_area_St_Co.csv")  #Story Co 37165 polygons

run23EZ = read.csv("CumEggsPerZone.2016.Feb.01.11_36_22.txt")#new baseline with varying directionality
run24EZ = read.csv("CumEggsPerZone.2016.Feb.02.06_42_30.txt")
run25EZ = read.csv("CumEggsPerZone.2016.Feb.05.10_28_39.txt")#2nd baseline to see how diff from other baseline
run33EZ = read.csv("CumEggsPerZone.2016.Feb.22.10_25_04.txt") #1,000 in St Co
run35EZ = read.csv("CumEggsPerZone.2016.Feb.24.04_14_06.txt") #10,000 in St Co, 50m step length
run36EZ = read.csv("CumEggsPerZone.2016.Feb.25.13_02_28.txt")  #10,000 in St Co, 30m step length
run41EZ = read.csv("CumEggsPerZone.2016.Feb.29.14_47_11.txt") #1,000 in St Co, 50m, all probEggs = 0.1
run42EZ = read.csv("CumEggsPerZone.2016.Feb.29.17_32_15.txt") #10,000 in St Co, 30m, all probEggs = 0.1
run49EZ = read.csv("CumEggsPerZone.2016.Mar.02.00_47_59.txt") #20,000 in St Co, 30m, all probEggs = 0.1, could sum with run42
dens42.49 = rbind(run42EZ,run49EZ) #combine probeggs=0.1 
run48EZ = read.csv("CumEggsPerZone.2016.Mar.01.19_38_25.txt") #10,000 in St Co, 30m step length (baseline)
run44EZ = read.csv("CumEggsPerZone.2016.Mar.01.12_47_57.txt") #10,000 in St Co, 50m
run43EZ = read.csv("CumEggsPerZone.2016.Mar.01.10_35_59.txt") #10,000 in St Co, 40m
run50EZ = read.csv("CumEggsPerZone.2016.Mar.02.12_34_14.txt") #5,000 in St Co, 20m
run51EZ = read.csv("CumEggsPerZone.2016.Mar.02.16_26_51.txt") #5,000 in St Co, 20m
run50.51EZ = rbind(run50EZ,run51EZ)
run40EZ = read.csv("CumEggsPerZone.2016.Feb.29.13_32_28.txt") #1,000 in St Co, 10m
run52EZ = read.csv("CumEggsPerZone.2016.Mar.02.19_57_36.txt") #1,000 in St Co, 10m
run53EZ = read.csv("CumEggsPerZone.2016.Mar.03.01_54_53.txt") #1,000 in St Co, 10m
run54EZ = read.csv("CumEggsPerZone.2016.Mar.03.13_41_09.txt") #1,000 in St Co, 10m
run55EZ = read.csv("CumEggsPerZone.2016.Mar.03.17_28_41.txt") #1,000 in St Co, 10m
run10mEZ = rbind(run40EZ,run52EZ,run53EZ,run54EZ,run55EZ)
run67EZ = read.csv("CumEggsPerZone.2016.Mar.09.17_36_28.txt") #remembered = 40
run59EZ = read.csv("CumEggsPerZone.2016.Mar.04.14_15_09.txt") #remembered = 2
run66EZ = read.csv("CumEggsPerZone.2016.Mar.08.15_11_16.txt") #perception=50
run62EZ = read.csv("CumEggsPerZone.2016.Mar.04.23_20_00.txt") #perception=200
run63EZ = read.csv("CumEggsPerZone.2016.Mar.08.02_58_20.txt") #perception=400
run92EZ = read.csv("CumEggsPerZone.2016.Mar.31.11_37_30.txt") #step=50, rem=40, per=200, dir=0.1-0.2
run76EZ = read.csv("CumEggsPerZone.2016.Mar.15.14_43_47.txt") #step=10, rem=0, per=50, dir=0.5-0.75
run77EZ = read.csv("CumEggsPerZone.2016.Mar.15.17_42_20.txt") #more of run76 parms
run78EZ = read.csv("CumEggsPerZone.2016.Mar.15.20_49_28.txt") #more of run76 parms
run76.78EZ = rbind(run76EZ,run77EZ,run78EZ)
run79EZ = read.csv("CumEggsPerZone.2016.Mar.16.02_58_53.txt") #dir=0.8-0.9
run82EZ = read.csv("CumEggsPerZone.2016.Mar.17.21_08_46.txt") #dir=0.1-0.9
run83EZ = read.csv("CumEggsPerZone.2016.Mar.18.01_19_35.txt") #dir=0.1-0.2
run94EZ = read.csv("CumEggsPerZone.2016.Mar.31.16_14_23.txt") #step=30, rem=10, per=100, dir=0.1-0.2
run96EZ = read.csv("CumEggsPerZone.2016.Apr.01.22_43_27.txt") #step=30, rem=100,per=400, dir=0.8-0.9
run98EZ = read.csv("CumEggsPerZone.2016.Apr.04.17_54_36.txt") #step=20, rem=40, per=100, dir=0.1-0.9
run101EZ = read.csv("CumEggsPerZone.2016.Apr.05.17_46_34.txt") #step=50, rem=100, per=200, dir=0.8-0.9
run102EZ = read.csv("CumEggsPerZone.2016.Apr.05.19_11_30.txt") #same as 101
run101.102EZ = rbind(run101EZ,run102EZ)
run104EZ = read.csv("CumEggsPerZone.2016.Apr.07.12_48_37.txt") #step=20, rem=100, per=400, dir=0.1-0.9
run105EZ = read.csv("CumEggsPerZone.2016.Apr.08.00_45_45.txt") #same as above
run104.105EZ = rbind(run104EZ,run105EZ) #5,000 agents
run106EZ = read.csv("CumEggsPerZone.2016.Apr.09.00_55_33.txt") #same as 104 and 105
run104.106EZ = rbind(run104EZ,run105EZ,run106EZ)
run89EZ = read.csv("CumEggsPerZone.2016.Mar.29.14_41_14.txt")  #step=10, rem=10, per=100, dir=0.5-0.75
run107EZ = read.csv("CumEggsPerZone.2016.Apr.11.09_57_15.txt") #same as 89
run108EZ = read.csv("CumEggsPerZone.2016.Apr.11.14_18_55.txt") #same as 89
run109EZ = read.csv("CumEggsPerZone.2016.Apr.12.10_34_33.txt") #same as 89
run110EZ = read.csv("CumEggsPerZone.2016.Apr.12.13_29_49.txt") #same as 89
run89.107.110EZ = rbind(run89EZ,run107EZ,run108EZ,run109EZ,run110EZ)
run119EZ = read.csv("CumEggsPerZone.2016.Apr.20.11_44_26.txt") #rem=10
run122EZ = read.csv("CumEggsPerZone.2016.Apr.20.16_12_57.txt") #rem=40
run123EZ = read.csv("CumEggsPerZone.2016.Apr.20.19_56_18.txt") #rem=100
run103EZ = read.csv("CumEggsPerZone.2016.Apr.06.17_29_47.txt") #lhs 8
run111EZ = read.csv("CumEggsPerZone.2016.Apr.15.17_09_22.txt") #lhs 8
run115EZ = read.csv("CumEggsPerZone.2016.Apr.18.17_01_05.txt") #lhs 8
run116EZ = read.csv("CumEggsPerZone.2016.Apr.19.11_42_19.txt") #lhs 8
run117EZ = read.csv("CumEggsPerZone.2016.Apr.19.15_39_50.txt") #lhs 8
run118EZ = read.csv("CumEggsPerZone.2016.Apr.19.19_16_42.txt") #lhs 8
run125EZ = read.csv("CumEggsPerZone.2016.Apr.21.13_23_22.txt") #lhs 8
run126EZ = read.csv("CumEggsPerZone.2016.Apr.21.20_50_46.txt") #lhs 8
run127EZ = read.csv("CumEggsPerZone.2016.Apr.22.12_28_42.txt") #lhs 8
run128EZ = read.csv("CumEggsPerZone.2016.Apr.22.17_20_31.txt") #lhs 8
run103.128EZ = rbind(run103EZ,run111EZ,run115EZ,run116EZ,run117EZ,run118EZ,run125EZ,run126EZ,run127EZ,run128EZ)

#finally just saved Denresults for each run
Denresults = Denresults48
Denresults = Denresults42.49
Denresults = Denresults44
Denresults = Denresults43
Denresults = Denresults50.51
Denresults = Denresults40
Denresults = Denresults52
Denresults = Denresults54
Denresults = Denresults10m
Denresults = Denresults55
Denresults = Denresults67
Denresults = Denresults59
Denresults = Denresults66
Denresults = Denresults62
Denresults = Denresults63
Denresults = Denresults79
Denresults = Denresults82
Denresults = Denresults83
Denresults = Denresults92
Denresults = Denresults76.78
Denresults = Denresults94
Denresults = Denresults96
Denresults = Denresults98
Denresults = Denresults101.102
Denresults = Denresults104.105
Denresults = Denresults106
Denresults = Denresults104.106
Denresults = Denresults89.107.110
Denresults = Denresults119
Denresults = Denresults122
Denresults = Denresults123
Denresults = Denresults103.128EZ

### these 2 lines were an attempt to change a column of factors to chars, finally just changed read file options
##i = sapply(run23EZ$ID, is.factor)
##run23EZ$ID[i] = lapply(run23EZ$ID[i], as.character)

dens = dens42.49
nrow(dens) #should be 37165*20=743300
#attempt to create numerical index from ID nos., finally just changed RS output to number instead of string
##mutate(dens, ID2 = strsplit(dens$ID,"Z"))
##strsplit(dens[1,3], "Z")
#get hab type names
levels(run35EZ$Name)

#example for one polygon - loop for all polygons below
dens35 = filter(dens, ID == 35)
Denresults[35,1] = dens35[1,3]
Denresults[35,2] = as.character(dens35[1,4])
Denresults[35,3] = sum(dens35$Eggs)
Denresults[35,4] = dens35[1,6]
Denresults[35,5] = PolyArea[35,7]
Denresults[35,6] = (Denresults[35,3]/Denresults[35,5])*10000


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




###############  Mean and SD of Egg Density by Habitat Type  ##############################

DensperHab = data.frame(matrix(nrow=17, ncol=5))
colnames(DensperHab) = c("MeanEggDensity", "SD", "Median","Cumulative Eggs","Proportion of Eggs")
rownames(DensperHab) = c(levels(run103EZ$Name))

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




##################  Publication Quality Graphs - Egg Density Per Habitat Type  #######################################

######### Step Length  ###################

#line graph of all parameters and hab types
eggDstepSA = data.frame(matrix(nrow=85, ncol=4))
colnames(eggDstepSA) = c("StepLength","HabType","PropEggs","Median")
eggDstepSA$StepLength = factor(eggDstepSA$StepLength)
levels(eggDstepSA$StepLength) = c(10,20,30,40,50)
#run 48 - step length 30
eggDstepSA[1:17,1] = 30 #step length
eggDstepSA[1:17,2] = habtypes #hab types
eggDstepSA[1:17,3] = DensperHab[1:17,5] #proportion of eggs laid per habitat type
eggDstepSA[1:17,4] = DensperHab[1:17,3] #median egg density
#run 44 - step length 50
eggDstepSA[18:34,1] = 50  #step length
eggDstepSA[18:34,2] = habtypes #hab types
eggDstepSA[18:34,3] = DensperHab[1:17,5] #proportion of eggs laid per habitat type
eggDstepSA[18:34,4] = DensperHab[1:17,3] #median egg density
#run 43 - step length 40
eggDstepSA[35:51,1] = 40 
eggDstepSA[35:51,2] = habtypes #hab types
eggDstepSA[35:51,3] = DensperHab[1:17,5] #proportion of eggs laid per habitat type
eggDstepSA[35:51,4] = DensperHab[1:17,3] #median egg density
#run 50,51 - step length 20
eggDstepSA[52:68,1] = 20 
eggDstepSA[52:68,2] = habtypes #hab types
eggDstepSA[52:68,3] = DensperHab[1:17,5] #proportion of eggs laid per habitat type
eggDstepSA[52:68,4] = DensperHab[1:17,3] #median egg density
#run - step length 10
#multiply by 2 since only 5,000 mons instead of 10,000
eggDstepSA[69:85,1] = 10 
eggDstepSA[69:85,2] = habtypes #hab types
eggDstepSA[69:85,3] = DensperHab[1:17,5] #proportion of eggs laid per habitat type
eggDstepSA[69:85,4] = 2*DensperHab[1:17,3] #median egg density

hablist = c("Barren/Clouds/No Data/Developed/Water/Nonag/Undefined/Aquaculture/Open Water/Perennial Ice/Snow",
            "Blank_/Developed/Med Intensity/Developed/High Intensity","Corn","Corn_nonGMO",
            "Cotton/Rice/Sorghum/Sunflower/Peanuts/Tobacco/Sweet Corn/Pop or Orn Corn/Mint/Barley/Durum Wheat/Spring Wheat/Winter Wheat/Other",
            "Developed/Open Space/Developed/Low Intensity","Forest/Deciduous Forest/Evergreen Forest/Mixed Forest/Woody Wetlands",
            "Grass/Pasture","MWROW0","MWROW1-5","MWROW20-60","MWROW5-20","MWROW60-100","RailroadROW","Soybeans",
            "Soybeans_nonGMO","Wetlands/Herbaceous Wetlands")
habtypes = c("Water (0.3%, 0.00)","High-Med Dev (1.1%, 0.00)","Corn (37.5%, 0.002)","Corn_nonGMO (5.4%, 0.085)",
            "Other Ag (0.8%, 0.005)",
            "Low Intensity Dev (6.2%, 0.005)","Forest (4.5%, 0.002)",
            "Grass/Pasture (7.2%, 0.075)","MWROW0 (0.8%, 0.00)","MWROW1-5 (0.4%, 0.05)","MWROW20-60 (1.2%, 0.085)",
            "MWROW5-20 (1.2%, 0.075)",
            "MWROW60-100 (1.5%, 0.09)","RailroadROW (0.5%, 0.05)","Soybeans (30.2%, 0.002)",
            "Soybeans_nonGMO (1.2%, 0.085)","Wetlands (0.02%, 0.02)")


#proportion of eggs laid
pro = ggplot(eggDstepSA, aes(x=reorder(HabType,PropEggs), y=PropEggs, linetype=StepLength, group=StepLength)) +
  geom_line() + ggtitle("Proportion of Eggs Laid Per Habitat Type") +
  xlab("Habitat Type (Proportion of Available Habitat, Probability of Laying Eggs Per Step)") +
  ylab("Proportion of Eggs")
pro + theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)),
            axis.title.x = element_text(size = 20, margin=margin(10,0,10,0)),
            axis.title.y = element_text(size = 20, margin=margin(0,10,0,5)),
            axis.text.x = element_text(angle=60, hjust=1),
            axis.text = element_text(size = 12),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 16))

#remove 40m step length
eggDstepSAless40 = eggDstepSA[-c(35:51),]

#median egg density
pl = ggplot(eggDstepSAless40, aes(x=reorder(HabType,Median), y=Median, linetype=StepLength, group=StepLength)) + geom_line() +
  ggtitle("Median Egg Density per Habitat Type") + xlab("Habitat Type") + ylab("Median Egg Density")
pl + theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)),
           axis.title.x = element_text(size = 26, margin=margin(10,0,10,0)),
           axis.title.y = element_text(size = 26, margin=margin(0,10,0,5)),
           axis.text.x = element_text(angle=60, hjust=1),
           axis.text = element_text(size = 20),
           legend.title = element_text(size = 26),
           legend.text = element_text(size = 26))

#proportion of eggs in roadsides for 30m step length
sum(eggDstepSA[9:13,3])  # = 0.500019



############### BOXPLOTS of certain habitat types for STEP Length ###################

#in the boxplots below, the MEDIAN column is MISLEADING, it corresponds to the median graph above, but its the
#egg density for each polygon

#### Grass/Pasture ###### 1920 polygons - not sure if column 2, propeggs, is right.  Col 3 is right, though.  
EggDensGP = data.frame(matrix(nrow=5000, ncol=4))
colnames(EggDensGP) = c("Steplength","PropEggs","PropEggsMean","Median")
EggDensGP$Steplength = factor(EggDensGP$Steplength)
levels(EggDensGP$Steplength) = c(10,20,30,40,50)

#run 48 - step length 30
EggDensGP[1:1920,1] = 30 #step length
EggDensGP[1:1920,2] = DensGP[,3]*1920/sum(DensperHab[,4]) #3050372 #proportion of eggs in gp if all polys had this number of eggs
EggDensGP[1:1920,3] = (DensGP[,6]*(sum(DensGP[,5]))/10000)/sum(DensperHab[,4]) #prop of eggs in gp if all polys this density
EggDensGP[1:1920,4] = DensGP[,6] #median egg density, i.e., distribution of egg density
mean(EggDensGP[1:1920,2]); mean(EggDensGP[1:1920,3])
#run 44 - step length 50
EggDensGP[1921:3840,1] = 50 #step length
EggDensGP[1921:3840,2] = DensGP[,3]*1920/sum(DensperHab[,4]) #proportion of eggs in gp if all polys had this number of eggs
EggDensGP[1921:3840,3] = (DensGP[,6]*(sum(DensGP[,5]))/10000)/sum(DensperHab[,4]) #prop of eggs in gp if all polys this density
EggDensGP[1921:3840,4] = DensGP[,6] #median egg density, i.e., distribution of egg density
#run 43 - step length 40
EggDensGP[3841:5760,1] = 40 #step length
EggDensGP[3841:5760,2] = DensGP[,3]*1920/sum(DensperHab[,4]) #2177392 #proportion of eggs in gp if all polys had this number of eggs
EggDensGP[3841:5760,3] = (DensGP[,6]*(sum(DensGP[,5]))/10000)/sum(DensperHab[,4]) #prop of eggs in gp if all polys this density
EggDensGP[3841:5760,4] = DensGP[,6] #median egg density, i.e., distribution of egg density
#run 50,51 - step length 20
EggDensGP[5761:7680,1] = 20 #step length
EggDensGP[5761:7680,2] = DensGP[,3]*1920/sum(DensperHab[,4]) # #proportion of eggs in gp if all polys had this number of eggs
EggDensGP[5761:7680,3] = (DensGP[,6]*(sum(DensGP[,5]))/10000)/sum(DensperHab[,4]) #prop of eggs in gp if all polys this density
EggDensGP[5761:7680,4] = DensGP[,6] #median egg density, i.e., distribution of egg density
#run - step length 10
#multiply by 2 since only 5,000 mons instead of 10,000
EggDensGP[7681:9600,1] = 10 #step length
EggDensGP[7681:9600,2] = 2*DensGP[,3]*1920/sum(DensperHab[,4]) #2031104 #proportion of eggs in gp if all polys had this number of eggs
EggDensGP[7681:9600,3] = 2*(DensGP[,6]*(sum(DensGP[,5]))/10000)/sum(DensperHab[,4]) #prop of eggs in gp if all polys this density
EggDensGP[7681:9600,4] = 2*(DensGP[,6]) #median egg density, i.e., distribution of egg density
hist(EggDensGP[7681:9600,4])

#remove step length 40m
EggDensGPless40 = EggDensGP[-c(3841:5760),]
max(EggDensGP[,4])

#distribution of egg density per GP plot
pr = ggplot(EggDensGPless40, aes(x=Steplength, y=Median)) + geom_boxplot(outlier.shape = NA) + ggtitle("Egg Density in Grass/Pasture Polygons") +
  xlab("Step Length") + ylab("Eggs per HA") +
  ylim(0,350) +
  stat_summary(fun.y="mean", geom="point", shape=3, size=3) 
pr + theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
           axis.title.x = element_text(size = 26, margin=margin(10,0,10,0)),
           axis.title.y = element_text(size = 26, margin=margin(0,10,0,5)),
           axis.text = element_text(size = 20))

#distribution of proportion of eggs if each individual plot extrapolated to all plots and total eggs
pr = ggplot(EggDensGP, aes(x=Steplength, y=PropEggsMean)) + geom_boxplot() + ggtitle("Proportion of Total Eggs as a Function of\nPolygon-Scale Egg Density") +
  xlab("Step Length") + ylab("Proportion of Eggs Laid in Grass/Pasture Habitat") +
  stat_summary(fun.y="mean", geom="point", shape=3, size=3)
pr + theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
           axis.title.x = element_text(size = 20, margin=margin(10,0,10,0)),
           axis.title.y = element_text(size = 20, margin=margin(0,10,0,5)),
           axis.text = element_text(size = 16))

############### Railrow ROW ###### Only 2 polygons
EggDensRR = data.frame(matrix(nrow=10, ncol=4))
colnames(EggDensRR) = c("Steplength","PropEggs","PropEggsMean","Median")
EggDensRR$Steplength = factor(EggDensRR$Steplength)
levels(EggDensRR$Steplength) = c(10,20,30,40,50)

#run 48 - step length 30
EggDensRR[1:2,1] = 30 #step length
EggDensRR[1:2,2] = DensRR[,3]*1920/sum(DensperHab[,4]) #3050372 #proportion of eggs in gp if all polys had this number of eggs
EggDensRR[1:2,3] = (DensRR[,6]*(sum(DensRR[,5]))/10000)/sum(DensperHab[,4]) #prop of eggs in gp if all polys this density
EggDensRR[1:2,4] = DensRR[,6] #median egg density, i.e., distribution of egg density
mean(EggDensRR[1:1920,2]); mean(EggDensRR[1:1920,4])
#run 44 - step length 50
EggDensRR[3:4,1] = 50 #step length
EggDensRR[3:4,2] = DensRR[,3]*1920/sum(DensperHab[,4]) #proportion of eggs in gp if all polys had this number of eggs
EggDensRR[3:4,3] = (DensRR[,6]*(sum(DensRR[,5]))/10000)/sum(DensperHab[,4]) #prop of eggs in gp if all polys this density
EggDensRR[3:4,4] = DensRR[,6] #median egg density, i.e., distribution of egg density
#run 43 - step length 40
EggDensRR[5:6,1] = 40 #step length
EggDensRR[5:6,2] = DensRR[,3]*1920/sum(DensperHab[,4]) #2177392 #proportion of eggs in gp if all polys had this number of eggs
EggDensRR[5:6,3] = (DensRR[,6]*(sum(DensRR[,5]))/10000)/sum(DensperHab[,4]) #prop of eggs in gp if all polys this density
EggDensRR[5:6,4] = DensRR[,6] #median egg density, i.e., distribution of egg density
#run 50,51 - step length 20
EggDensRR[7:8,1] = 20 #step length
EggDensRR[7:8,2] = DensRR[,3]*1920/sum(DensperHab[,4]) # #proportion of eggs in gp if all polys had this number of eggs
EggDensRR[7:8,3] = (DensRR[,6]*(sum(DensRR[,5]))/10000)/sum(DensperHab[,4]) #prop of eggs in gp if all polys this density
EggDensRR[7:8,4] = DensRR[,6] #median egg density, i.e., distribution of egg density
#run - step length 10
#multiply by 2 since only 5,000 mons instead of 10,000
EggDensRR[9:10,1] = 10 #step length
EggDensRR[9:10,2] = 2*DensRR[,3]*1920/(sum(DensperHab[,4])) #2031104 #proportion of eggs in gp if all polys had this number of eggs
EggDensRR[9:10,3] = 2*(DensRR[,6]*(sum(DensRR[,5]))/10000)/sum(DensperHab[,4]) #prop of eggs in gp if all polys this density
EggDensRR[9:10,4] = 2*(DensRR[,6]) #median egg density, i.e., distribution of egg density
#hist(EggDensRR[7681:9600,4])

#distribution of egg density per RR plot
pr = ggplot(EggDensRR, aes(x=Steplength, y=Median)) + geom_boxplot() + ggtitle("Egg Density in Railroad Polygons") +
  xlab("Step Length") + ylab("Eggs per HA") +
  stat_summary(fun.y="mean", geom="point", shape=3, size=3)
pr + theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
           axis.title.x = element_text(size = 20, margin=margin(10,0,10,0)),
           axis.title.y = element_text(size = 20, margin=margin(0,10,0,5)),
           axis.text = element_text(size = 16))

############### Road ROW 60-100 ###### 8976 polygons
EggDensR6 = data.frame(matrix(nrow=10, ncol=4))
colnames(EggDensR6) = c("Steplength","PropEggs","PropEggsMean","Median")
EggDensR6$Steplength = factor(EggDensR6$Steplength)
levels(EggDensR6$Steplength) = c(10,20,30,40,50)

#run 48 - step length 30
EggDensR6[1:8976,1] = 30 #step length
EggDensR6[1:8976,2] = DensR6[,3]*1920/sum(DensperHab[,4]) #3050372 #proportion of eggs in gp if all polys had this number of eggs
EggDensR6[1:8976,3] = (DensR6[,6]*(sum(DensR6[,5]))/10000)/sum(DensperHab[,4]) #prop of eggs in gp if all polys this density
EggDensR6[1:8976,4] = DensR6[,6] #median egg density, i.e., distribution of egg density
mean(EggDensR6[1:1920,2]); mean(EggDensR6[1:1920,4])
#run 44 - step length 50
EggDensR6[8977:17952,1] = 50 #step length
EggDensR6[8977:17952,2] = DensR6[,3]*1920/sum(DensperHab[,4]) #proportion of eggs in gp if all polys had this number of eggs
EggDensR6[8977:17952,3] = (DensR6[,6]*(sum(DensR6[,5]))/10000)/sum(DensperHab[,4]) #prop of eggs in gp if all polys this density
EggDensR6[8977:17952,4] = DensR6[,6] #median egg density, i.e., distribution of egg density
#run 43 - step length 40
EggDensR6[17953:26928,1] = 40 #step length
EggDensR6[17953:26928,2] = DensR6[,3]*1920/sum(DensperHab[,4]) #2177392 #proportion of eggs in gp if all polys had this number of eggs
EggDensR6[17953:26928,3] = (DensR6[,6]*(sum(DensR6[,5]))/10000)/sum(DensperHab[,4]) #prop of eggs in gp if all polys this density
EggDensR6[17953:26928,4] = DensR6[,6] #median egg density, i.e., distribution of egg density
#run 50,51 - step length 20
EggDensR6[26929:35904,1] = 20 #step length
EggDensR6[26929:35904,2] = DensR6[,3]*1920/sum(DensperHab[,4]) # #proportion of eggs in gp if all polys had this number of eggs
EggDensR6[26929:35904,3] = (DensR6[,6]*(sum(DensR6[,5]))/10000)/sum(DensperHab[,4]) #prop of eggs in gp if all polys this density
EggDensR6[26929:35904,4] = DensR6[,6] #median egg density, i.e., distribution of egg density
hist(EggDensR6[26929:35904,4])
#run - step length 10
#multiply by 2 since only 5,000 mons instead of 10,000
EggDensR6[35905:44880,1] = 10 #step length
EggDensR6[35905:44880,2] = 2*DensR6[,3]*1920/(sum(DensperHab[,4])) #2031104 #proportion of eggs in gp if all polys had this number of eggs
EggDensR6[35905:44880,3] = 2*(DensR6[,6]*(sum(DensR6[,5]))/10000)/sum(DensperHab[,4]) #prop of eggs in gp if all polys this density
EggDensR6[35905:44880,4] = 2*(DensR6[,6]) #median egg density, i.e., distribution of egg density
hist(EggDensR6[35905:44880,4])

#remove step length 40m
EggDensR6less40 = EggDensR6[-c(17953:26928),]
#a look at some chars of the distribution
max(EggDensR6[,4]); mean(EggDensR6less40[,4])
hist(EggDensR6[35905:44880,4])
sum(EggDensR6[35905:44880,4]); sum(EggDensR6[8977:17952,4])
#despite many more eggs laid in 10m sim, median and mean egg density are much lower because of clumping

#distribution of egg density per ROW MW=60-100m2 plot
pr = ggplot(EggDensR6less40, aes(x=Steplength, y=Median)) + geom_boxplot(outlier.shape = NA) + 
  ggtitle("Egg Density in Road Right-of-Way Polygons") +
  #ggtitle(expression(paste("Egg Density in Road Right-of-Way Polygons\n with 60-100+ m2 of Milkweed"))) +
  xlab("Step Length") + ylab("Eggs per HA") +
  ylim(0,1000) +
  stat_summary(fun.y="mean", geom="point", shape=3, size=3)
pr + theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
           axis.title.x = element_text(size = 26, margin=margin(10,0,10,0)),
           axis.title.y = element_text(size = 26, margin=margin(0,10,0,5)),
           axis.text = element_text(size = 20))






################ remembered EZ SA graphs  ###########################
#replaced old with new

#line graph of all parameters and hab types
eggDremSA = data.frame(matrix(nrow=85, ncol=4))
colnames(eggDremSA) = c("Remembered","HabType","PropEggs","Median")
eggDremSA$Remembered = factor(eggDremSA$Remembered)
levels(eggDremSA$Remembered) = c(0,10,40,100)
#run 48 - remembered 0
eggDremSA[1:17,1] = 0 #remembered
eggDremSA[1:17,2] = habtypes #hab types
eggDremSA[1:17,3] = DensperHab[1:17,5] #proportion of eggs laid per habitat type
eggDremSA[1:17,4] = DensperHab[1:17,3] #median egg density
#run 119 - remembered 10
eggDremSA[18:34,1] = 10  #remembered
eggDremSA[18:34,2] = habtypes #hab types
eggDremSA[18:34,3] = DensperHab[1:17,5] #proportion of eggs laid per habitat type
eggDremSA[18:34,4] = DensperHab[1:17,3] #median egg density
#run 122 - remembered 40
eggDremSA[35:51,1] = 40 #remembered
eggDremSA[35:51,2] = habtypes #hab types
eggDremSA[35:51,3] = DensperHab[1:17,5] #proportion of eggs laid per habitat type
eggDremSA[35:51,4] = DensperHab[1:17,3] #median egg density
#run 123 - remembered 100
eggDremSA[52:68,1] = 100 #remembered
eggDremSA[52:68,2] = habtypes #hab types
eggDremSA[52:68,3] = DensperHab[1:17,5] #proportion of eggs laid per habitat type
eggDremSA[52:68,4] = DensperHab[1:17,3] #median egg density
eggDremSA = eggDremSA[-c(69:85),]

#proportion of eggs laid
pror = ggplot(eggDremSA, aes(x=reorder(HabType,PropEggs), y=PropEggs, linetype=Remembered, group=Remembered)) +
  geom_line() + ggtitle("Proportion of Eggs Laid Per Habitat Type") +
  xlab("Habitat Type (Proportion of Available Habitat, Probability of Laying Eggs Per Step)") +
  ylab("Proportion of Eggs")
pror + theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)),
            axis.title.x = element_text(size = 20, margin=margin(10,0,10,0)),
            axis.title.y = element_text(size = 20, margin=margin(0,10,0,5)),
            axis.text.x = element_text(angle=60, hjust=1, size = 16),
            axis.text = element_text(size = 12),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 16))

#median egg density
plr = ggplot(eggDremSA, aes(x=reorder(HabType,Median), y=Median, linetype=Remembered, group=Remembered)) + geom_line() +
  ggtitle("Median Egg Density per Habitat Type") + xlab("Habitat Type") + ylab("Median Egg Density")
plr + theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)),
           axis.title.x = element_text(size = 26, margin=margin(10,0,10,0)),
           axis.title.y = element_text(size = 26, margin=margin(0,10,0,5)),
           axis.text.x = element_text(angle=60, hjust=1, size=16),
           axis.text = element_text(size = 20),
           legend.title = element_text(size = 26),
           legend.text = element_text(size = 26))


############ boxplots per habitat type for REMEMBERED  ######################

#### Grass/Pasture ###### 1920 polygons - not sure if column 2, propeggs, is right.  Col 3 is right, though.  
EggDensGP.rem = data.frame(matrix(nrow=5000, ncol=4))
colnames(EggDensGP.rem) = c("Remembered","PropEggs","PropEggsMean","EggDensity")
EggDensGP.rem$Remembered = factor(EggDensGP.rem$Remembered)
levels(EggDensGP.rem$Remembered) = c(0,10,40,100)

#run 48 - step length 30, remembered = 0
EggDensGP.rem[1:1920,1] = 0 #remembered
EggDensGP.rem[1:1920,2] = DensGP[,3]*1920/sum(DensperHab[,4]) #3050372 #proportion of eggs in gp if all polys had this number of eggs
EggDensGP.rem[1:1920,3] = (DensGP[,6]*(sum(DensGP[,5]))/10000)/sum(DensperHab[,4]) #prop of eggs in gp if all polys this density
EggDensGP.rem[1:1920,4] = DensGP[,6] #median egg density, i.e., distribution of egg density
mean(EggDensGP.rem[1:1920,4])
#run 119 - remembered = 10
EggDensGP.rem[1921:3840,1] = 10 #remembered
EggDensGP.rem[1921:3840,2] = DensGP[,3]*1920/sum(DensperHab[,4]) #proportion of eggs in gp if all polys had this number of eggs
EggDensGP.rem[1921:3840,3] = (DensGP[,6]*(sum(DensGP[,5]))/10000)/sum(DensperHab[,4]) #prop of eggs in gp if all polys this density
EggDensGP.rem[1921:3840,4] = DensGP[,6] #median egg density, i.e., distribution of egg density
#run 122 - remembered = 40
EggDensGP.rem[3841:5760,1] = 40 #remembered
EggDensGP.rem[3841:5760,2] = DensGP[,3]*1920/sum(DensperHab[,4]) #2177392 #proportion of eggs in gp if all polys had this number of eggs
EggDensGP.rem[3841:5760,3] = (DensGP[,6]*(sum(DensGP[,5]))/10000)/sum(DensperHab[,4]) #prop of eggs in gp if all polys this density
EggDensGP.rem[3841:5760,4] = DensGP[,6] #median egg density, i.e., distribution of egg density
#run 123 - remembered = 100
EggDensGP.rem[5761:7680,1] = 100 #remembered
EggDensGP.rem[5761:7680,2] = DensGP[,3]*1920/sum(DensperHab[,4]) # #proportion of eggs in gp if all polys had this number of eggs
EggDensGP.rem[5761:7680,3] = (DensGP[,6]*(sum(DensGP[,5]))/10000)/sum(DensperHab[,4]) #prop of eggs in gp if all polys this density
EggDensGP.rem[5761:7680,4] = DensGP[,6] #median egg density, i.e., distribution of egg density
max(EggDensGP.rem[5761:7680,4])

#distribution of egg density per GP plot
pr = ggplot(EggDensGP.rem, aes(x=Remembered, y=EggDensity)) + geom_boxplot(outlier.shape = NA) + ggtitle("Egg Density in Grass/Pasture Polygons") +
  xlab("Remembered Polygons") + ylab("Eggs per HA") +
  ylim(0,250) +
  stat_summary(fun.y="mean", geom="point", shape=3, size=3) 
pr + theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
           axis.title.x = element_text(size = 26, margin=margin(10,0,10,0)),
           axis.title.y = element_text(size = 26, margin=margin(0,10,0,5)),
           axis.text = element_text(size = 20))


############### Road ROW 60-100 ###### 8976 polygons ########  REMEMBERED ##########
EggDensR6.rem = data.frame(matrix(nrow=10, ncol=4))
colnames(EggDensR6.rem) = c("Remembered","PropEggs","PropEggsMean","EggDensity")
EggDensR6.rem$Remembered = factor(EggDensR6.rem$Remembered)
levels(EggDensR6.rem$Remembered) = c(0,10,40,100)

#run 48 - step length 30, remembered = 0
EggDensR6.rem[1:8976,1] = 0 #remembered
EggDensR6.rem[1:8976,2] = DensR6[,3]*1920/sum(DensperHab[,4]) #3050372 #proportion of eggs in gp if all polys had this number of eggs
EggDensR6.rem[1:8976,3] = (DensR6[,6]*(sum(DensR6[,5]))/10000)/sum(DensperHab[,4]) #prop of eggs in gp if all polys this density
EggDensR6.rem[1:8976,4] = DensR6[,6] #median egg density, i.e., distribution of egg density
mean(EggDensR6.rem[1:8976,4])
#run 119 - remembered = 10
EggDensR6.rem[8977:17952,1] = 10 #remembered
EggDensR6.rem[8977:17952,2] = DensR6[,3]*1920/sum(DensperHab[,4]) #proportion of eggs in gp if all polys had this number of eggs
EggDensR6.rem[8977:17952,3] = (DensR6[,6]*(sum(DensR6[,5]))/10000)/sum(DensperHab[,4]) #prop of eggs in gp if all polys this density
EggDensR6.rem[8977:17952,4] = DensR6[,6] #median egg density, i.e., distribution of egg density
mean(EggDensR6.rem[8977:17952,4])
#run 122 - remembered = 40
EggDensR6.rem[17953:26928,1] = 40 #remembered
EggDensR6.rem[17953:26928,2] = DensR6[,3]*1920/sum(DensperHab[,4]) #2177392 #proportion of eggs in gp if all polys had this number of eggs
EggDensR6.rem[17953:26928,3] = (DensR6[,6]*(sum(DensR6[,5]))/10000)/sum(DensperHab[,4]) #prop of eggs in gp if all polys this density
EggDensR6.rem[17953:26928,4] = DensR6[,6] #median egg density, i.e., distribution of egg density
#run 123 - remembered= 100
EggDensR6.rem[26929:35904,1] = 100 #remembered
EggDensR6.rem[26929:35904,2] = DensR6[,3]*1920/sum(DensperHab[,4]) # #proportion of eggs in gp if all polys had this number of eggs
EggDensR6.rem[26929:35904,3] = (DensR6[,6]*(sum(DensR6[,5]))/10000)/sum(DensperHab[,4]) #prop of eggs in gp if all polys this density
EggDensR6.rem[26929:35904,4] = DensR6[,6] #median egg density, i.e., distribution of egg density
hist(EggDensR6.rem[26929:35904,4]); max(EggDensR6.rem[26929:35904,4])

#distribution of egg density per ROW MW=60-100m2 plot
pr = ggplot(EggDensR6.rem, aes(x=Remembered, y=EggDensity)) + geom_boxplot(outlier.shape = NA) + 
  ggtitle("Egg Density in Road Right-of-Way Polygons") +
  #ggtitle(expression(paste("Egg Density in Road Right-of-Way Polygons\n with 60-100+ m2 of Milkweed"))) +
  xlab("Remembered Polygons") + ylab("Eggs per HA") +
  ylim(0,700) +
  stat_summary(fun.y="mean", geom="point", shape=3, size=3)
pr + theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
           axis.title.x = element_text(size = 26, margin=margin(10,0,10,0)),
           axis.title.y = element_text(size = 26, margin=margin(0,10,0,5)),
           axis.text = element_text(size = 20))



#######################  Perception EZ SA graphs ###########################

eggDpercSA = data.frame(matrix(nrow=85, ncol=4))
colnames(eggDpercSA) = c("Perception","HabType","PropEggs","Median")
eggDpercSA$Perception = factor(eggDpercSA$Perception)
levels(eggDpercSA$Perception) = c(50,100,200,400)
#run 66 - perception 50
eggDpercSA[1:17,1] = 50 #remembered
eggDpercSA[1:17,2] = habtypes #hab types
eggDpercSA[1:17,3] = DensperHab[1:17,5] #proportion of eggs laid per habitat type
eggDpercSA[1:17,4] = DensperHab[1:17,3] #median egg density
#run 48 - perception 100
eggDpercSA[18:34,1] = 100  #perception
eggDpercSA[18:34,2] = habtypes #hab types
eggDpercSA[18:34,3] = DensperHab[1:17,5] #proportion of eggs laid per habitat type
eggDpercSA[18:34,4] = DensperHab[1:17,3] #median egg density
#run 62 - perception 200
eggDpercSA[35:51,1] = 200 #perception
eggDpercSA[35:51,2] = habtypes #hab types
eggDpercSA[35:51,3] = DensperHab[1:17,5] #proportion of eggs laid per habitat type
eggDpercSA[35:51,4] = DensperHab[1:17,3] #median egg density
#run 63 - perception 400
eggDpercSA[52:68,1] = 400 #perception
eggDpercSA[52:68,2] = habtypes #hab types
eggDpercSA[52:68,3] = DensperHab[1:17,5] #proportion of eggs laid per habitat type
eggDpercSA[52:68,4] = DensperHab[1:17,3] #median egg density
eggDpercSA = eggDpercSA[-c(69:85),]

#proportion of eggs laid
perp = ggplot(eggDpercSA, aes(x=reorder(HabType,PropEggs), y=PropEggs, color=Perception, group=Perception)) +
  geom_line() + ggtitle("Proportion of Eggs Laid Per Habitat Type") +
  xlab("Habitat Type (Proportion of Available Habitat, Probability of Laying Eggs Per Step)") +
  ylab("Proportion of Eggs")
perp + theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)),
             axis.title.x = element_text(size = 20, margin=margin(10,0,10,0)),
             axis.title.y = element_text(size = 20, margin=margin(0,10,0,5)),
             axis.text.x = element_text(angle=60, hjust=1, size = 16),
             axis.text = element_text(size = 12),
             legend.title = element_text(size = 16),
             legend.text = element_text(size = 16))

#median egg density
plr = ggplot(eggDpercSA, aes(x=reorder(HabType,Median), y=Median, linetype=Perception, group=Perception)) + geom_line() +
  ggtitle("Median Egg Density per Habitat Type") + xlab("Habitat Type") + ylab("Median Egg Density")
plr + theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)),
            axis.title.x = element_text(size = 20, margin=margin(10,0,10,0)),
            axis.title.y = element_text(size = 20, margin=margin(0,10,0,5)),
            axis.text.x = element_text(angle=60, hjust=1, size=16),
            axis.text = element_text(size = 12),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 16))


############ boxplots per habitat type for PERCEPTION  ######################

#### Grass/Pasture ###### 1920 polygons - not sure if column 2, propeggs, is right.  Col 3 is right, though.  
EggDensGP.perc = data.frame(matrix(nrow=5000, ncol=4))
colnames(EggDensGP.perc) = c("Perception","PropEggs","PropEggsMean","EggDensity")
EggDensGP.perc$Perception = factor(EggDensGP.perc$Perception)
levels(EggDensGP.perc$Perception) = c(50,100,200,400)

#run 48 - step length 30, perception = 100
EggDensGP.perc[1:1920,1] = 100 #perception
EggDensGP.perc[1:1920,2] = DensGP[,3]*1920/sum(DensperHab[,4]) #3050372 #proportion of eggs in gp if all polys had this number of eggs
EggDensGP.perc[1:1920,3] = (DensGP[,6]*(sum(DensGP[,5]))/10000)/sum(DensperHab[,4]) #prop of eggs in gp if all polys this density
EggDensGP.perc[1:1920,4] = DensGP[,6] #median egg density, i.e., distribution of egg density
mean(EggDensGP.perc[1:1920,4])
#run 66 - perception = 50
EggDensGP.perc[1921:3840,1] = 50 #perception
EggDensGP.perc[1921:3840,2] = DensGP[,3]*1920/sum(DensperHab[,4]) #proportion of eggs in gp if all polys had this number of eggs
EggDensGP.perc[1921:3840,3] = (DensGP[,6]*(sum(DensGP[,5]))/10000)/sum(DensperHab[,4]) #prop of eggs in gp if all polys this density
EggDensGP.perc[1921:3840,4] = DensGP[,6] #median egg density, i.e., distribution of egg density
#run 62 - perception = 200
EggDensGP.perc[3841:5760,1] = 200 #perception
EggDensGP.perc[3841:5760,2] = DensGP[,3]*1920/sum(DensperHab[,4]) #2177392 #proportion of eggs in gp if all polys had this number of eggs
EggDensGP.perc[3841:5760,3] = (DensGP[,6]*(sum(DensGP[,5]))/10000)/sum(DensperHab[,4]) #prop of eggs in gp if all polys this density
EggDensGP.perc[3841:5760,4] = DensGP[,6] #median egg density, i.e., distribution of egg density
#run 63 - perception = 400
EggDensGP.perc[5761:7680,1] = 400 #perception
EggDensGP.perc[5761:7680,2] = DensGP[,3]*1920/sum(DensperHab[,4]) # #proportion of eggs in gp if all polys had this number of eggs
EggDensGP.perc[5761:7680,3] = (DensGP[,6]*(sum(DensGP[,5]))/10000)/sum(DensperHab[,4]) #prop of eggs in gp if all polys this density
EggDensGP.perc[5761:7680,4] = DensGP[,6] #median egg density, i.e., distribution of egg density
max(EggDensGP.perc[5761:7680,4])

#distribution of egg density per GP plot
pr = ggplot(EggDensGP.perc, aes(x=Perception, y=EggDensity)) + geom_boxplot(outlier.shape = NA) + ggtitle("Egg Density in Grass/Pasture Polygons") +
  xlab("Perception Distance (m)") + ylab("Eggs per HA") +
  ylim(0,250) +
  stat_summary(fun.y="mean", geom="point", shape=3, size=3) 
pr + theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
           axis.title.x = element_text(size = 26, margin=margin(10,0,10,0)),
           axis.title.y = element_text(size = 26, margin=margin(0,10,0,5)),
           axis.text = element_text(size = 20))


############### Road ROW 60-100 ###### 8976 polygons ########  PERCEPTION ##########
EggDensR6.perc = data.frame(matrix(nrow=10, ncol=4))
colnames(EggDensR6.perc) = c("Perception","PropEggs","PropEggsMean","EggDensity")
EggDensR6.perc$Perception = factor(EggDensR6.perc$Perception)
levels(EggDensR6.perc$Perception) = c(50,100,200,400)

#run 48 - step length 30, perception = 100
EggDensR6.perc[1:8976,1] = 100 #perception
EggDensR6.perc[1:8976,2] = DensR6[,3]*1920/sum(DensperHab[,4]) #3050372 #proportion of eggs in gp if all polys had this number of eggs
EggDensR6.perc[1:8976,3] = (DensR6[,6]*(sum(DensR6[,5]))/10000)/sum(DensperHab[,4]) #prop of eggs in gp if all polys this density
EggDensR6.perc[1:8976,4] = DensR6[,6] #median egg density, i.e., distribution of egg density
mean(EggDensR6.perc[1:8976,4])
#run 66 - perception = 50
EggDensR6.perc[8977:17952,1] = 50 #perception
EggDensR6.perc[8977:17952,2] = DensR6[,3]*1920/sum(DensperHab[,4]) #proportion of eggs in gp if all polys had this number of eggs
EggDensR6.perc[8977:17952,3] = (DensR6[,6]*(sum(DensR6[,5]))/10000)/sum(DensperHab[,4]) #prop of eggs in gp if all polys this density
EggDensR6.perc[8977:17952,4] = DensR6[,6] #median egg density, i.e., distribution of egg density
mean(EggDensR6.perc[8977:17952,4])
#run 62 - perception = 200
EggDensR6.perc[17953:26928,1] = 200 #perception
EggDensR6.perc[17953:26928,2] = DensR6[,3]*1920/sum(DensperHab[,4]) #2177392 #proportion of eggs in gp if all polys had this number of eggs
EggDensR6.perc[17953:26928,3] = (DensR6[,6]*(sum(DensR6[,5]))/10000)/sum(DensperHab[,4]) #prop of eggs in gp if all polys this density
EggDensR6.perc[17953:26928,4] = DensR6[,6] #median egg density, i.e., distribution of egg density
#run 63 - perception = 400
EggDensR6.perc[26929:35904,1] = 400 #perception
EggDensR6.perc[26929:35904,2] = DensR6[,3]*1920/sum(DensperHab[,4]) # #proportion of eggs in gp if all polys had this number of eggs
EggDensR6.perc[26929:35904,3] = (DensR6[,6]*(sum(DensR6[,5]))/10000)/sum(DensperHab[,4]) #prop of eggs in gp if all polys this density
EggDensR6.perc[26929:35904,4] = DensR6[,6] #median egg density, i.e., distribution of egg density
hist(EggDensR6.perc[26929:35904,4]); max(EggDensR6.perc[26929:35904,4])

#distribution of egg density per ROW MW=60-100m2 plot
pr = ggplot(EggDensR6.perc, aes(x=Perception, y=EggDensity)) + geom_boxplot(outlier.shape = NA) + 
  ggtitle("Egg Density in Road Right-of-Way Polygons") +
  #ggtitle(expression(paste("Egg Density in Road Right-of-Way Polygons\n with 60-100+ m2 of Milkweed"))) +
  xlab("Perception Distance (m)") + ylab("Eggs per HA") +
  ylim(0,700) +
  stat_summary(fun.y="mean", geom="point", shape=3, size=3)
pr + theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
           axis.title.x = element_text(size = 26, margin=margin(10,0,10,0)),
           axis.title.y = element_text(size = 26, margin=margin(0,10,0,5)),
           axis.text = element_text(size = 20))






#######################  Directionality EZ SA graphs ###########################

eggDdirSA = data.frame(matrix(nrow=68, ncol=4))
colnames(eggDdirSA) = c("Directionality","HabType","PropEggs","Median")
eggDdirSA$Directionality = factor(eggDdirSA$Directionality)
levels(eggDdirSA$Directionality) = c("0.1-0.2","0.1-0.9","0.5-0.75","0.8-0.9")
#run 83 - directionality 0.1-0.2
eggDdirSA[1:17,1] = "0.1-0.2" #directionality
eggDdirSA[1:17,2] = habtypes #hab types
eggDdirSA[1:17,3] = DensperHab[1:17,5] #proportion of eggs laid per habitat type
eggDdirSA[1:17,4] = DensperHab[1:17,3] #median egg density
#run 82 - directionality 0.1-0.9
eggDdirSA[18:34,1] = "0.1-0.9"  #directionality
eggDdirSA[18:34,2] = habtypes #hab types
eggDdirSA[18:34,3] = DensperHab[1:17,5] #proportion of eggs laid per habitat type
eggDdirSA[18:34,4] = DensperHab[1:17,3] #median egg density
#run 48 - directionality 0.5-0.75
eggDdirSA[35:51,1] = "0.5-0.75" #directionality
eggDdirSA[35:51,2] = habtypes #hab types
eggDdirSA[35:51,3] = DensperHab[1:17,5] #proportion of eggs laid per habitat type
eggDdirSA[35:51,4] = DensperHab[1:17,3] #median egg density
#run 79 - directionality 0.8-0.9
eggDdirSA[52:68,1] = "0.8-0.9" #directionality
eggDdirSA[52:68,2] = habtypes #hab types
eggDdirSA[52:68,3] = DensperHab[1:17,5] #proportion of eggs laid per habitat type
eggDdirSA[52:68,4] = DensperHab[1:17,3] #median egg density

#proportion of eggs laid
perp = ggplot(eggDdirSA, aes(x=reorder(HabType,PropEggs), y=PropEggs, color=Directionality, group=Directionality)) +
  geom_line() + ggtitle("Proportion of Eggs Laid Per Habitat Type") +
  xlab("Habitat Type (Proportion of Available Habitat, Probability of Laying Eggs Per Step)") +
  ylab("Proportion of Eggs")
perp + theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)),
             axis.title.x = element_text(size = 20, margin=margin(10,0,10,0)),
             axis.title.y = element_text(size = 20, margin=margin(0,10,0,5)),
             axis.text.x = element_text(angle=60, hjust=1, size = 16),
             axis.text = element_text(size = 12),
             legend.title = element_text(size = 16),
             legend.text = element_text(size = 16))

#median egg density
plr = ggplot(eggDdirSA, aes(x=reorder(HabType,Median), y=Median, linetype=Directionality, group=Directionality)) + geom_line() +
  ggtitle("Median Egg Density per Habitat Type") + xlab("Habitat Type") + ylab("Median Egg Density")
plr + theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)),
            axis.title.x = element_text(size = 26, margin=margin(10,0,10,0)),
            axis.title.y = element_text(size = 26, margin=margin(0,10,0,5)),
            axis.text.x = element_text(angle=60, hjust=1, size=16),
            axis.text = element_text(size = 20),
            legend.title = element_text(size = 26),
            legend.text = element_text(size = 26))






############ boxplots per habitat type for DIRECTIONALITY  ######################

#### Grass/Pasture ###### 1920 polygons - not sure if column 2, propeggs, is right.  Col 3 is right, though.  
EggDensGP.dir = data.frame(matrix(nrow=5000, ncol=4))
colnames(EggDensGP.dir) = c("Directionality","PropEggs","PropEggsMean","EggDensity")
EggDensGP.dir$Directionality = factor(EggDensGP.dir$Directionality)
levels(EggDensGP.dir$Directionality) = c("0.1-0.2","0.1-0.9","0.5-0.75","0.8-0.9")

#run 83 - step length 30, directionality = 0.1-0.2
EggDensGP.dir[1:1920,1] = "0.1-0.2" #directionality
EggDensGP.dir[1:1920,2] = DensGP[,3]*1920/sum(DensperHab[,4]) #3050372 #proportion of eggs in gp if all polys had this number of eggs
EggDensGP.dir[1:1920,3] = (DensGP[,6]*(sum(DensGP[,5]))/10000)/sum(DensperHab[,4]) #prop of eggs in gp if all polys this density
EggDensGP.dir[1:1920,4] = DensGP[,6] #median egg density, i.e., distribution of egg density
mean(EggDensGP.dir[1:1920,4])
#run 82 - directionality = 0.1-0.9
EggDensGP.dir[1921:3840,1] = "0.1-0.9" #directionality
EggDensGP.dir[1921:3840,2] = DensGP[,3]*1920/sum(DensperHab[,4]) #proportion of eggs in gp if all polys had this number of eggs
EggDensGP.dir[1921:3840,3] = (DensGP[,6]*(sum(DensGP[,5]))/10000)/sum(DensperHab[,4]) #prop of eggs in gp if all polys this density
EggDensGP.dir[1921:3840,4] = DensGP[,6] #median egg density, i.e., distribution of egg density
#run 48 - directionality = 0.5-0.75
EggDensGP.dir[3841:5760,1] = "0.5-0.75" #directionality
EggDensGP.dir[3841:5760,2] = DensGP[,3]*1920/sum(DensperHab[,4]) #2177392 #proportion of eggs in gp if all polys had this number of eggs
EggDensGP.dir[3841:5760,3] = (DensGP[,6]*(sum(DensGP[,5]))/10000)/sum(DensperHab[,4]) #prop of eggs in gp if all polys this density
EggDensGP.dir[3841:5760,4] = DensGP[,6] #median egg density, i.e., distribution of egg density
#run 79 - directionality = 0.8-0.9
EggDensGP.dir[5761:7680,1] = "0.8-0.9" #directionality
EggDensGP.dir[5761:7680,2] = DensGP[,3]*1920/sum(DensperHab[,4]) # #proportion of eggs in gp if all polys had this number of eggs
EggDensGP.dir[5761:7680,3] = (DensGP[,6]*(sum(DensGP[,5]))/10000)/sum(DensperHab[,4]) #prop of eggs in gp if all polys this density
EggDensGP.dir[5761:7680,4] = DensGP[,6] #median egg density, i.e., distribution of egg density
max(EggDensGP.dir[5761:7680,4])

#distribution of egg density per GP plot
pr = ggplot(EggDensGP.dir, aes(x=Directionality, y=EggDensity)) + geom_boxplot(outlier.shape = NA) + ggtitle("Egg Density in Grass/Pasture Polygons") +
  xlab("Range in Directionality") + ylab("Eggs per HA") +
  ylim(0,250) +
  stat_summary(fun.y="mean", geom="point", shape=3, size=3) 
pr + theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
           axis.title.x = element_text(size = 26, margin=margin(10,0,10,0)),
           axis.title.y = element_text(size = 26, margin=margin(0,10,0,5)),
           axis.text = element_text(size = 20))


############### Road ROW 60-100 ###### 8976 polygons ########  PERCEPTION ##########
EggDensR6.dir = data.frame(matrix(nrow=10, ncol=4))
colnames(EggDensR6.dir) = c("Directionality","PropEggs","PropEggsMean","EggDensity")
EggDensR6.dir$Directionality = factor(EggDensR6.dir$Directionality)
levels(EggDensR6.dir$Directionality) = c("0.1-0.2","0.1-0.9","0.5-0.75","0.8-0.9")

#run 83 - step length 30, directionality = 0.1-0.2
EggDensR6.dir[1:8976,1] = "0.1-0.2" #directionality
EggDensR6.dir[1:8976,2] = DensR6[,3]*1920/sum(DensperHab[,4]) #3050372 #proportion of eggs in gp if all polys had this number of eggs
EggDensR6.dir[1:8976,3] = (DensR6[,6]*(sum(DensR6[,5]))/10000)/sum(DensperHab[,4]) #prop of eggs in gp if all polys this density
EggDensR6.dir[1:8976,4] = DensR6[,6] #median egg density, i.e., distribution of egg density
mean(EggDensR6.dir[1:8976,4])
#run 82 - directionality = 0.1-0.9
EggDensR6.dir[8977:17952,1] = "0.1-0.9" #directionality
EggDensR6.dir[8977:17952,2] = DensR6[,3]*1920/sum(DensperHab[,4]) #proportion of eggs in gp if all polys had this number of eggs
EggDensR6.dir[8977:17952,3] = (DensR6[,6]*(sum(DensR6[,5]))/10000)/sum(DensperHab[,4]) #prop of eggs in gp if all polys this density
EggDensR6.dir[8977:17952,4] = DensR6[,6] #median egg density, i.e., distribution of egg density
mean(EggDensR6.dir[8977:17952,4])
#run 48 - directionality = 0.5-0.75
EggDensR6.dir[17953:26928,1] = "0.5-0.75" #directionality
EggDensR6.dir[17953:26928,2] = DensR6[,3]*1920/sum(DensperHab[,4]) #2177392 #proportion of eggs in gp if all polys had this number of eggs
EggDensR6.dir[17953:26928,3] = (DensR6[,6]*(sum(DensR6[,5]))/10000)/sum(DensperHab[,4]) #prop of eggs in gp if all polys this density
EggDensR6.dir[17953:26928,4] = DensR6[,6] #median egg density, i.e., distribution of egg density
#run 79 - directionality = 0.8-0.9
EggDensR6.dir[26929:35904,1] = "0.8-0.9" #directionality
EggDensR6.dir[26929:35904,2] = DensR6[,3]*1920/sum(DensperHab[,4]) # #proportion of eggs in gp if all polys had this number of eggs
EggDensR6.dir[26929:35904,3] = (DensR6[,6]*(sum(DensR6[,5]))/10000)/sum(DensperHab[,4]) #prop of eggs in gp if all polys this density
EggDensR6.dir[26929:35904,4] = DensR6[,6] #median egg density, i.e., distribution of egg density
hist(EggDensR6.dir[26929:35904,4]); max(EggDensR6.dir[26929:35904,4])

#distribution of egg density per ROW MW=60-100m2 plot
pr = ggplot(EggDensR6.dir, aes(x=Directionality, y=EggDensity)) + geom_boxplot(outlier.shape = NA) + 
  ggtitle("Egg Density in Road Right-of-Way Polygons") +
  #ggtitle(expression(paste("Egg Density in Road Right-of-Way Polygons\n with 60-100+ m2 of Milkweed"))) +
  xlab("Range in Directionality") + ylab("Eggs per HA") +
  ylim(0,700) +
  stat_summary(fun.y="mean", geom="point", shape=3, size=3)
pr + theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
           axis.title.x = element_text(size = 26, margin=margin(10,0,10,0)),
           axis.title.y = element_text(size = 26, margin=margin(0,10,0,5)),
           axis.text = element_text(size = 20))




############# graph the different 10m runs to see how similar ##################
eggDstepSA.10 = data.frame(matrix(nrow=85, ncol=4))
colnames(eggDstepSA.10) = c("Run","HabType","PropEggs","Median")
eggDstepSA.10$Run = factor(eggDstepSA.10$Run)
levels(eggDstepSA.10$Run) = c(40,52,53,54,55)
#run 40 
eggDstepSA.10[1:17,1] = 40 #step length
eggDstepSA.10[1:17,2] = habtypes #hab types
eggDstepSA.10[1:17,3] = DensperHab[1:17,5] #proportion of eggs laid per habitat type
eggDstepSA.10[1:17,4] = DensperHab[1:17,3] #median egg density
#run 52
eggDstepSA.10[18:34,1] = 52  #step length
eggDstepSA.10[18:34,2] = habtypes #hab types
eggDstepSA.10[18:34,3] = DensperHab[1:17,5] #proportion of eggs laid per habitat type
eggDstepSA.10[18:34,4] = DensperHab[1:17,3] #median egg density
#run 53 
eggDstepSA.10[35:51,1] = 53 
eggDstepSA.10[35:51,2] = habtypes #hab types
eggDstepSA.10[35:51,3] = DensperHab[1:17,5] #proportion of eggs laid per habitat type
eggDstepSA.10[35:51,4] = DensperHab[1:17,3] #median egg density
#run 54
eggDstepSA.10[52:68,1] = 54 
eggDstepSA.10[52:68,2] = habtypes #hab types
eggDstepSA.10[52:68,3] = DensperHab[1:17,5] #proportion of eggs laid per habitat type
eggDstepSA.10[52:68,4] = DensperHab[1:17,3] #median egg density
#run 55
eggDstepSA.10[69:85,1] = 55 
eggDstepSA.10[69:85,2] = habtypes #hab types
eggDstepSA.10[69:85,3] = DensperHab[1:17,5] #proportion of eggs laid per habitat type
eggDstepSA.10[69:85,4] = DensperHab[1:17,3] #median egg density

ggplot(eggDstepSA.10, aes(x=reorder(HabType,Median), y=Median, linetype=Run, group=Run)) + geom_line() +
  ggtitle("Median Egg Density per Habitat Type") + xlab("Habitat Type") + ylab("Median Egg Density")

ggplot(eggDstepSA.10, aes(x=reorder(HabType,PropEggs), y=PropEggs, linetype=Run, group=Run)) + geom_line() +
  ggtitle("Proportion of Eggs Laid Per Habitat Type")







#a look at relationship between polygon area and egg density
plot(Denresults$PolygonAream2, Denresults$EggDensityperHA, pch = 20, cex=0.5)
#confidence limits from below
lines(Denresults$PolygonAream2, expm1(pred.log.w.clim))
#polygons with probEggs >0, i.e., only polygons that could have eggs
pzeroplus = filter(Denresults, ProbEggs > 0)
plot(pzeroplus$PolygonAream2,pzeroplus$EggDensityperHA, pch = 20, cex=0.5)
#additionally filtering out roadsides
plargepolys = filter(pzeroplus, PolygonAream2 > 3000)
plot(plargepolys$PolygonAream2,plargepolys$EggDensityperHA, pch = 20, cex=0.5)

#multiple regression of sites with probeggs >0
fit = lm(pzeroplus$EggDensityperHA ~ pzeroplus$PolygonAream2 + pzeroplus$ProbEggs, data=pzeroplus)
#regression when probEggs all equal
#couldnt' get predict work until  I made data into a and b
a = pzeroplus$EggDensityperHA; b = pzeroplus$PolygonAream2
#fit = lm(pzeroplus$EggDensityperHA ~ pzeroplus$PolygonAream2, data=pzeroplus)
fit = lm(a ~ b)
#area is barely significant p=0.0486
summary(fit); plot(fit)
#values for prediction
area = data.frame(b=seq(0,12000000,10000))
pred = predict(fit,area)
#confidence intervals on predicted values
pred.w.clim = predict(fit, area, interval="confidence")
matplot(area$b, pred.w.clim, type = "l")
#fit an exponential model = log(0) throws an error, so use a simple Box-Cox tansformation which is log1p()
fitexp = lm(log(pzeroplus$EggDensityperHA) ~ pzeroplus$PolygonAream2 + pzeroplus$ProbEggs, data=pzeroplus)
fitexp = lm(log1p(pzeroplus$EggDensityperHA) ~ pzeroplus$PolygonAream2 + pzeroplus$ProbEggs, data=pzeroplus)
fitexp = lm(log1p(a) ~ b)
summary(fitexp); plot(fitexp)
pred.log = predict(fitexp, area)
pred.log.w.clim = predict(fitexp, area, interval = "confidence")
pred.log.w.clim.all = predict(fitexp, interval = "confidence")
pred.expm1 = expm1(pred.log.w.clim)
#ylim is log1p(1000)
matplot(area$b, pred.expm1, type = "l", ylim=c(0,1000))

plot(Denresults$PolygonAream2, Denresults$EggDensityperHA, pch = 20, cex=0.5)
lines(Denresults$PolygonAream2, expm1(pred.log.w.clim.all))


#fit the model to large polygons
fitexpl = lm(log1p(plargepolys$EggDensityperHA) ~ plargepolys$PolygonAream2 + plargepolys$ProbEggs, data=plargepolys)
summary(fitexpl); plot(fitexpl)



#write densities to csv to map in ArcMap
write.csv(Denresults,"DensityResults.csv") #run24
write.csv(Denresults, "DensityResults25.csv")
write.csv(Denresults, "DensityResults33.csv")
write.csv(Denresults, "DensityResults35.csv")
write.csv(Denresults, "DensityResults35_b.csv")
write.csv(Denresults, "DensityResults41.csv")
write.csv(Denresults, "DensityResults42.csv")
write.csv(Denresults, "DensityResults49.csv")
write.csv(Denresults, "DensityResults42-49.csv")



######################  Utilization Distributions and Time Budget per Habitat  #############################

library(move)
library(ggmap)
library(mapproj)

#test adehabitatHR

#test different time intervals
adecoords = as.data.frame(xnumeric[[1]])
adecoords[,2] = as.data.frame(ynumeric[[1]])
time14 = as.POSIXct(1:length(xnumeric[[1]]), origin="1970-01-01", format = "%H")
time14 = as.POSIXct(1:length(xnumeric[[1]]), origin="1970-01-01", format = "%d")
M14 = as.ltraj(xy = adecoords, date = time14, id = "M14")
plot(M14)
#sig2 is loc error - in lat/long units, I guess
lik = liker(M14, sig2 = 0.00001, rangesig1 = c(-10,10))
bbmm = kernelbb(M14, sig1 = 0, sig2 = 0.00001, grid = 50)
image(bbmm)





############# Main Code - Utilization Distribution  ##################

CoordData33 = read.csv("Coords.2016.Feb.22.10_25_04.txt") #run33
CoordData38 = read.csv("Coords.2016.Feb.29.10_00_12.txt") #run38
CoordData45 = read.csv("Coords.2016.Mar.01.14_08_40.txt") #run 45 - 50m step length UDs
CoordData46 = read.csv("Coords.2016.Mar.01.15_25_31.txt") #run 46 - 40m step length UDs
CoordData47 = read.csv("Coords.2016.Mar.01.16_00_42.txt") #run 47 - 30m step length UDs
CoordData56 = read.csv("Coords.2016.Mar.03.08_32_20.txt") #run 56 - 20m step length UDs
CoordData52 = read.csv("Coords.2016.Mar.02.19_57_36.txt") #run 52 - 10m step length UDs
CoordData58 = read.csv("Coords.2016.Mar.04.10_32_46.txt") #run 58 - remembered = 40
CoordData60 = read.csv("Coords.2016.Mar.04.14_50_54.txt") #run 60 - remembered = 2
CoordData65 = read.csv("Coords.2016.Mar.08.13_25_05.txt") #run 65 - perception = 50
CoordData61 = read.csv("Coords.2016.Mar.04.15_57_50.txt") #run 61 - perception = 200
CoordData64 = read.csv("Coords.2016.Mar.08.10_03_50.txt") #run 64 - perception = 400
#step length for above are 30m unless otherwise specified
CoordData70 = read.csv("Coords.2016.Mar.10.23_29_58.txt") #run 71 - step=50, remembered=40
CoordData45 = read.csv("Coords.2016.Mar.01.14_08_40.txt") #run 45 - step=50, remembered=10

names = as.character(CoordData46$Name)
#split strings
xcoords = strsplit(as.character(CoordData46$MonXs), ",")
ycoords = strsplit(as.character(CoordData46$MonYs), ",")

#convert each list component from character to numeric
xnumeric = list()
ynumeric = list()
for (i in 1:1000){
  xnumeric[[i]] = as.numeric(xcoords[[i]])
}
for (i in 1:1000){
  ynumeric[[i]] = as.numeric(ycoords[[i]])
}

#create move objects for each monarch and put them into one list
allmons = list()
for (i in 1:1000){
  timeall = as.POSIXct(1:length(xnumeric[[i]]), origin="1970-01-01", format = "%M", tz="UTC")
  allmons[[i]] = move(x=xnumeric[[i]], y=ynumeric[[i]], time=timeall, 
                      animal=names[i], proj=CRS("+proj=longlat"))
}

#transform all move objects
allmons_t = list()
for (i in 1:1000){
  allmons_t[[i]] = spTransform(x=allmons[[i]], CRSobj="+proj=aeqd", center=TRUE)
}


############## calculate UDs for all monarchs ##################

dbbmms45 = list()
system.time( #this function calculates the time this function takes
  for (i in 1:1000){
    dbbmms45[[i]] = brownian.bridge.dyn(allmons_t[[i]], raster=30, location.error = 0.001)
  }
)  #end system.time

dbbmms70 = list()
system.time( #this function calculates the time this function takes
  for (i in 1:1000){
    dbbmms70[[i]] = brownian.bridge.dyn(allmons_t[[i]], raster=30, location.error = 0.001)
  }
)  #end system.time
dbbmms64 = list()
system.time( #this function calculates the time this function takes
  for (i in 1:1000){
    dbbmms64[[i]] = brownian.bridge.dyn(allmons_t[[i]], raster=30, location.error = 0.001)
  }
)  #end system.time
dbbmms61 = list()
system.time( #this function calculates the time this function takes
  for (i in 1:1000){
    dbbmms61[[i]] = brownian.bridge.dyn(allmons_t[[i]], raster=30, location.error = 0.001)
  }
)  #end system.time
dbbmms65 = list()
system.time( #this function calculates the time this function takes
  for (i in 1:500){
    dbbmms65[[i]] = brownian.bridge.dyn(allmons_t[[i]], raster=30, location.error = 0.001)
  }
)  #end system.time
dbbmms60 = list()
system.time( #this function calculates the time this function takes
  for (i in 1:500){
    dbbmms60[[i]] = brownian.bridge.dyn(allmons_t[[i]], raster=30, location.error = 0.001)
  }
)  #end system.time
dbbmms58 = list()
system.time( #this function calculates the time this function takes
  for (i in 1:1000){
    dbbmms58[[i]] = brownian.bridge.dyn(allmons_t[[i]], raster=30, location.error = 0.001)
  }
)  #end system.time
dbbmms52 = list()
system.time( #this function calculates the time this function takes
  for (i in 1:500){
    dbbmms52[[i]] = brownian.bridge.dyn(allmons_t[[i]], raster=30, location.error = 0.001)
  }
)  #end system.time
dbbmms56 = list()
system.time( #this function calculates the time this function takes
  for (i in 1:1000){
    dbbmms56[[i]] = brownian.bridge.dyn(allmons_t[[i]], raster=30, location.error = 0.001)
  }
)  #end system.time
dbbmms45 = list()
system.time( #this function calculates the time this function takes
for (i in 1:1000){
  dbbmms45[[i]] = brownian.bridge.dyn(allmons_t[[i]], raster=30, location.error = 0.001)
}
)  #end system.time
dbbmms46 = list()
system.time( #this function calculates the time this function takes
  for (i in 1:1000){
    dbbmms46[[i]] = brownian.bridge.dyn(allmons_t[[i]], raster=30, location.error = 0.001)
  }
)  #end system.time
for (i in 101:1000){
  dbbmms38[[i]] = brownian.bridge.dyn(allmons_t[[i]], raster=30, location.error = 0.001)
}


#################### calcualte area for each monarch UD #########################

#plot them all - will only plot 100 at a time
for (i in 101:200){
  contour(dbbmms46[[842]], levels=c(0.5, 0.95), col=c(6,2), lwd=2, main=i, add=TRUE)
}
#plot anything that looks strange
plot(allmons_t[[842]], pch=20, cex=0.5)


load("area80.RData")
load("area81.RData")
load("area84.RData")
load("dbbmms91.RData")
load("dbbmms93.RData")
load("dbbms89.RData")
load("area95.RData")
load("area99.RData")
load("area97.RData")
load("area103.RData")
load("area104.RData")
load("area120.RData")
load("area121.RData")
load("area124.RData")

#area103
mean(area103); pop.sd(area103); median(area103); hist(area103); plot(density(area103))
sort(area103)[1:20]; sort(area103)[(length(area103)-20):length(area103)]

#area104
mean(area104); pop.sd(area104); median(area104); hist(area104); plot(density(area104))
sort(area104)[1:20]; sort(area104)[(length(area104)-20):length(area104)]

#area97
mean(area97); pop.sd(area97); median(area97); hist(area97); plot(density(area97))
sort(area97)[1:20]; sort(area97)[(length(area97)-20):length(area97)]

#area99
mean(area99); pop.sd(area99); median(area99); hist(area99); plot(density(area99))
sort(area99)[1:20]; sort(area99)[(length(area99)-20):length(area99)]

#area95
mean(area95); pop.sd(area95); median(area95); hist(area95); plot(density(area95))
sort(area95)[1:20]; sort(area95)[(length(area95)-20):length(area95)]

# these 3 had problems running - I left index as 149:1000
# first 148 are empty
run89 = vector()
for (i in 1:1000){
  run89[i] = is.null(dbbmms89[[i]])
}
#first 148 are empty
run91 = vector()
for (i in 1:1000){
  run91[i] = is.null(dbbmms91[[i]])
}
#first 148 are empty
run93 = vector()
for (i in 1:1000){
  run93[i] = is.null(dbbmms93[[i]])
}

#loaded from speedy2
area89 = vector()
for (i in 149:1000){
  volUD = getVolumeUD(dbbmms89[[i]])
  volUD = volUD<=.95
  area89[i] = sum(values(volUD))
}
area_s89 = area89[149:1000]; area_s89b = area_s89[-c(150,633,814)]
mean(area_s89); pop.sd(area_s89); median(area_s89); hist(area_s89); plot(density(area_s89))
sort(area_s89)[1:20]; sort(area_s89)[(length(area_s89)-20):length(area_s89)]
which(area_s89 == 49)

area91 = vector()
for (i in 149:1000){
  volUD = getVolumeUD(dbbmms91[[i]])
  volUD = volUD<=.95
  area91[i] = sum(values(volUD))
}
area_s91 = area91[149:1000]; area_s91b = area_s91[-c(150,633,814)]
mean(area_s91b); pop.sd(area_s91b); median(area_s91b); hist(area_s91b); plot(density(area_s91b))
sort(area_s91b)[1:20]; sort(area_s91b)[(length(area_s91b)-20):length(area_s91b)]
which(area91 == 49)

area93 = vector()
for (i in 149:1000){
  volUD = getVolumeUD(dbbmms93[[i]])
  volUD = volUD<=.95
  area93[i] = sum(values(volUD))
}
area_s93 = area93[149:1000]; area_s93b = area_s93[-c(150,633,814)]
mean(area_s93); pop.sd(area_s93); median(area_s93); hist(area_s93); plot(density(area_s93))
sort(area_s93)[1:20]; sort(area_s93)[(length(area_s93)-20):length(area_s93)]
which(area_s93 == 49)

#loaded area75 from speedy2
mean(area75); pop.sd(area75); hist(area75); plot(density(area75))
sort(area75)[1:20]; sort(area75)[(length(area75)-20):length(area75)]

#loaded area71 from speedy2
load("area71.RData")
mean(area71); pop.sd(area71); hist(area71); plot(density(area71))
sort(area71)[1:20]; sort(area71)[(length(area71)-20):length(area71)]
which(area71 == 17)
#remove outliers from area71
area_s71 = area71[-c(389,143,566,350,562)]
mean(area_s71); pop.sd(area_s71); hist(area_s71); plot(density(area_s71))

#this was run on speedy2, compare to 45 below that was run on PC
#load("dbbmms45sp2.RData")
area45sp2 = vector()
for (i in 1:1000){
  volUD = getVolumeUD(dbbmms45[[i]])
  volUD = volUD<=.95
  area45sp2[i] = sum(values(volUD))
}
mean(area45sp2); pop.sd(area45sp2); hist(area45sp2); plot(density(area45sp2))
sort(area45sp2)[1:20]; sort(area45sp2)[(length(area45sp2)-20):length(area45sp2)]
which(area45sp2 == 201)
save(area45sp2, file="area45sp2.RData")
load("area45sp2.RData")

area45 = vector()
for (i in 1:1000){
  volUD = getVolumeUD(dbbmms45[[i]])
  volUD = volUD<=.95
  area45[i] = sum(values(volUD))
}
mean(area45); pop.sd(area45); hist(area45); plot(density(area45))
sort(area45)[1:20]; sort(area45)[(length(area45)-20):length(area45)]
which(area45 == 201)
#remove outliers from run 45
area_s45 = area45[-c(212,251,208,549,452,833,842,725,185,78,665,433)]
mean(area_s45); pop.sd(area_s45); hist(area_s45); plot(density(area_s45))

area70 = vector()
for (i in 1:1000){
  volUD = getVolumeUD(dbbmms70[[i]])
  volUD = volUD<=.95
  area70[i] = sum(values(volUD))
}
mean(area70); pop.sd(area70); hist(area70); plot(density(area70))
sort(area70)[1:20]; sort(area70)[(length(area70)-20):length(area70)]
which(area70 == 199)
#remove outliers from run 70
area_s70 = area70[-c(23,111,210,82,101,490,43,283,436,152,178,765,744,516,908,717)]
mean(area_s70); pop.sd(area_s70); hist(area_s70); plot(density(area_s70))
area64 = vector()
for (i in 1:500){
  volUD = getVolumeUD(dbbmms64[[i]])
  volUD = volUD<=.95
  area64[i] = sum(values(volUD))
}
mean(area64); pop.sd(area64); hist(area64); plot(density(area64))
sort(area64)[1:20]; sort(area64)[(length(area64)-20):length(area64)]
which(area64 == 5)
area61 = vector()
for (i in 1:500){
  volUD = getVolumeUD(dbbmms61[[i]])
  volUD = volUD<=.95
  area61[i] = sum(values(volUD))
}
mean(area61); pop.sd(area61); hist(area61); plot(density(area61))
sort(area61)[1:20]; sort(area61)[(length(area61)-20):length(area61)]
which(area61 == 5)
area65 = vector()
for (i in 1:500){
  volUD = getVolumeUD(dbbmms65[[i]])
  volUD = volUD<=.95
  area65[i] = sum(values(volUD))
}
mean(area65); pop.sd(area65); hist(area65); plot(density(area65))
sort(area65)[1:20]; sort(area65)[(length(area65)-20):length(area65)]
which(area65 == 65)
area60 = vector()
for (i in 1:500){
  volUD = getVolumeUD(dbbmms60[[i]])
  volUD = volUD<=.95
  area60[i] = sum(values(volUD))
}
mean(area60); pop.sd(area60); hist(area60); plot(density(area60))
sort(area60)[1:20]; sort(area60)[(length(area60)-20):length(area60)]
which(area60 == 1007)
area58 = vector()
for (i in 1:1000){
  volUD = getVolumeUD(dbbmms58[[i]])
  volUD = volUD<=.95
  area58[i] = sum(values(volUD))
}
mean(area58); pop.sd(area58); hist(area58); plot(density(area58))
sort(area58)[1:20]; sort(area58)[(length(area58)-20):length(area58)]
which(area58 == 1007)
area52 = vector()
for (i in 1:1000){
  volUD = getVolumeUD(dbbmms52[[i]])
  volUD = volUD<=.95
  area52[i] = sum(values(volUD))
}
mean(area52); pop.sd(area52); hist(area52); plot(density(area52))
sort(area52)[1:20]; sort(area52)[(length(area52)-20):length(area52)]
which(area52 == 5)
save(area52, file="area52.RData")
area56 = vector()
for (i in 1:1000){
  volUD = getVolumeUD(dbbmms52[[i]])
  volUD = volUD<=.95
  area56[i] = sum(values(volUD))
}
mean(area56); pop.sd(area56); hist(area56)
sort(area56)[1:20]; sort(area56)[(length(area56)-20):length(area56)]
which(area56 == 5)
save(area56, file="area56.RData")
area46 = vector()
for (i in 1:1000){
  volUD = getVolumeUD(dbbmms46[[i]])
  volUD = volUD<=.95
  area46[i] = sum(values(volUD))
}
mean(area46); pop.sd(area46); hist(area46)
sort(area46)[1:20]; sort(area46)[(length(area46)-20):length(area46)]
which(area46 == 1349)
#remove outliers from run46
area_s46 = area46[-c(408,132,806,4,811,742,248,965,455,154,675)]
mean(area_s46); pop.sd(area_s46); hist(area_s46)
area45 = vector()
for (i in 1:1000){
  volUD = getVolumeUD(dbbmms45[[i]])
  volUD = volUD<=.95
  area45[i] = sum(values(volUD))
}
mean(area45); pop.sd(area45); hist(area45)
sort(area45)[1:10]; sort(area45)[(length(area45)-10):length(area45)]
which(area45 == 32)

mean(area38); pop.sd(area38); hist(area38)
sort(area38)[1:10]; sort(area38)[(length(area38)-10):length(area38)]
which(area38 == 1048)
save(area38, file="area38.RData")
#rmove outliers from run 65
area_s65 = area65[-c(379)]
mean(area_s65); pop.sd(area_s65); hist(area_s65)
#remove outliers from run 45
area_s45 = area45[-c(212,251,867,208,549,452,833)]
mean(area_s45); pop.sd(area_s45); hist(area_s45)
#remove outliers from run 33
area_s33 = area[-c(999,994,917,906,902,866,851,850,815,789,778,757,753,746,660,653,644,638,637,634,
                 615,613,608,595,588,533,510,475,459,457,455,442,424,405,330,
                 284,271,253,232,188,171,128,71,66,190,119,26)]
mean(area_s33); pop.sd(area_s33); hist(area_s33)
sort(area_s33)[1:10]; sort(area_s)[(length(area_s)-10):length(area_s)]
which(area_s33 == 3714); which(area33 == 3714)


#run this code one line at time to get it to work - sometimes doesn't work
move1fl = as(allmons[[549]], "data.frame")
m = get_map(bbox(extent(move1fl)*1.1), source="stamen", zoom=14)
ggmap(m)+geom_path(data=move1fl, aes(x=x, y=y))


#save shapefiles
x = raster2contour(dbbmms46[[842]],level=c(.95))
writeOGR(x, dsn="C:/Users/tgrant/Documents/Monarchs GIS/Shapefiles/UDs", layer="run46.842", driver="ESRI Shapefile")




################ Publication quality graphs of UDs  #########################


######  10m step by perception graph ##########

areastep10SA = data.frame(matrix(nrow=1500, ncol=2))
colnames(areastep10SA) = c("Perception","Area")
areastep10SA$Perception = factor(areastep10SA$Perception)
levels(areastep10SA$Perception) = c(50,100)
load("area75.RData")
#run 75 - perception 50, step 10
areastep10SA[1:1000,1] = 50 
areastep10SA[1:1000,2] = area75
#run 52 - perception 100, step 10
areastep10SA[1001:1500,1] = 100 
areastep10SA[1001:1500,2] = area52

p = ggplot(areastep10SA, aes(x=Perception, y=Area)) + geom_boxplot() + ggtitle("Area of Utilization Distribution") +
  xlab("Perception Distance") + ylab("Area") +
  stat_summary(fun.y="mean", geom="point", shape=3, size=3)
p + theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
          axis.title.x = element_text(size = 20, margin=margin(10,0,10,0)),
          axis.title.y = element_text(size = 20, margin=margin(0,10,0,5)),
          axis.text = element_text(size = 12))




############## directionality SA graph ###################

dirSA = data.frame(matrix(nrow=4000, ncol=2))
colnames(dirSA) = c("Directionality","Area")
dirSA$Directionality = factor(dirSA$Directionality)
levels(dirSA$Directionality) = c("0.1-0.2","0.1-0.9","0.5-0.75","0.8-0.9")
load("area38.RData")

#run 80 - dir=0.8-0.9
dirSA[1:1000,1] = "0.8-0.9"
dirSA[1:1000,2] = area80
#run 81 - dir=0.1-0.9
dirSA[1001:2000,1] = "0.1-0.9"
dirSA[1001:2000,2] = area81
#run 84 - dir=0.1-0.2
dirSA[2001:3000,1] = "0.1-0.2"
dirSA[2001:3000,2] = area84
#run 38 - dir=0.5-0.75
dirSA[3001:4000,1] = "0.5-0.75"
dirSA[3001:4000,2] = area38

pdir = ggplot(dirSA, aes(x=Directionality, y=Area)) + geom_boxplot() + ggtitle("Area of Utilization Distribution") +
  xlab("Directionality") + ylab("Area") +
  stat_summary(fun.y="mean", geom="point", shape=3, size=3)
pdir + theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
             axis.title.x = element_text(size = 26, margin=margin(10,0,10,0)),
             axis.title.y = element_text(size = 26, margin=margin(0,10,0,5)),
             axis.text = element_text(size = 20),
             legend.title = element_text(size = 16),
             legend.text = element_text(size = 16))


##############  step=50 by remembered SA graph
areastep50SA = data.frame(matrix(nrow=2967, ncol=2))
colnames(areastep50SA) = c("Remembered","Area")
areastep50SA$Remembered = factor(areastep50SA$Remembered)
levels(areastep50SA$Remembered) = c(2,10,40)

#run 70 - step=50, rem=40
areastep50SA[1:984,1] = 40 
areastep50SA[1:984,2] = area_s70
#run 45 - step=50, rem=10
areastep50SA[985:1972,1] = 10 
areastep50SA[985:1972,2] = area_s45
#run 71 - step=50, rem=2
areastep50SA[1973:2967,1] = 2 
areastep50SA[1973:2967,2] = area_s71

p50r = ggplot(areastep50SA, aes(x=Remembered, y=Area)) + geom_boxplot() + ggtitle("Area of Utilization Distribution") +
  xlab("Remembered") + ylab("Area") +
  stat_summary(fun.y="mean", geom="point", shape=3, size=3)
p50r + theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
           axis.title.x = element_text(size = 26, margin=margin(10,0,10,0)),
           axis.title.y = element_text(size = 26, margin=margin(0,10,0,5)),
           axis.text = element_text(size = 20),
           legend.title = element_text(size = 16),
           legend.text = element_text(size = 16))


################# Perception SA graph ########################
#step length = 30m
areapercSA = data.frame(matrix(nrow=3499, ncol=2))
colnames(areapercSA) = c("Perception","Area")
areapercSA$Perception = factor(areapercSA$Perception)
levels(areapercSA$Perception) = c(50,100,200,400)
load("area38.RData")
#run 38 - perception 100
areapercSA[1:1000,1] = 100 
areapercSA[1:1000,2] = area38
#run 61 - perception 200
areapercSA[1001:2000,1] = 200 
areapercSA[1001:2000,2] = area61
#run 64 - perception 400
areapercSA[2001:3000,1] = 400 
areapercSA[2001:3000,2] = area64
#run 65 - perception 50
areapercSA[3001:3499,1] = 50 
areapercSA[3001:3499,2] = area_s65

  
pr = ggplot(areapercSA, aes(x=Perception, y=Area)) + geom_boxplot() + ggtitle("Area of Utilization Distribution") +
  xlab("Perception") + ylab("Area") +
  stat_summary(fun.y="mean", geom="point", shape=3, size=3)
pr + theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
           axis.title.x = element_text(size = 26, margin=margin(10,0,10,0)),
           axis.title.y = element_text(size = 26, margin=margin(0,10,0,5)),
           axis.text = element_text(size = 20))



##### Remembered by UD SA graph - NEW ##########

arearemSA = data.frame(matrix(nrow=4000, ncol=2))
colnames(arearemSA) = c("Remembered","Area")
arearemSA$Remembered = factor(arearemSA$Remembered)
levels(arearemSA$Remembered) = c(0,10,40,100)
load("area38.RData")
#run  - remembered 0
arearemSA[1:1000,1] = 0 
arearemSA[1:1000,2] = area38
#run  - remembered 10
arearemSA[1001:2000,1] = 10 
arearemSA[1001:2000,2] = area120
#run  - remembered 40
arearemSA[2001:3000,1] = 40 
arearemSA[2001:3000,2] = area121
#run  - remembered 100
arearemSA[3001:4000,1] = 100 
arearemSA[3001:4000,2] = area124

pr = ggplot(arearemSA, aes(x=Remembered, y=Area)) + geom_boxplot() + ggtitle("Area of Utilization Distribution") +
  xlab("Remembered") + ylab("Area") +
  stat_summary(fun.y="mean", geom="point", shape=3, size=3)
pr + theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
           axis.title.x = element_text(size = 26, margin=margin(10,0,10,0)),
           axis.title.y = element_text(size = 26, margin=margin(0,10,0,5)),
           axis.text = element_text(size = 20))


##### Remembered by UD SA graph - OLD ##########

###### this is the old remembered UD graph when remembered always = 0 because of a bug ###############

arearemSA = data.frame(matrix(nrow=5000, ncol=2))
colnames(arearemSA) = c("Remembered","Area")
arearemSA$Remembered = factor(arearemSA$Remembered)
levels(arearemSA$Remembered) = c(2,10,40)
load("area38.RData")
#run 58 - remembered 40
arearemSA[1:1000,1] = 40 
arearemSA[1:1000,2] = area58
#run 38 - remembered 10
arearemSA[1001:2000,1] = 10 
arearemSA[1001:2000,2] = area38
#run 60 - remembered 2
arearemSA[2001:3000,1] = 2 
arearemSA[2001:2500,2] = area60

pr = ggplot(arearemSA, aes(x=Remembered, y=Area)) + geom_boxplot() + ggtitle("Area of Utilization Distribution") +
  xlab("Remembered") + ylab("Area") +
  stat_summary(fun.y="mean", geom="point", shape=3, size=3)
pr + theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
          axis.title.x = element_text(size = 20, margin=margin(10,0,10,0)),
          axis.title.y = element_text(size = 20, margin=margin(0,10,0,5)),
          axis.text = element_text(size = 12))


##### Step length by UD SA graph ########

areastepSA = data.frame(matrix(nrow=6000, ncol=2))
colnames(areastepSA) = c("StepLength","Area")
areastepSA$StepLength = factor(areastepSA$StepLength)
levels(areastepSA$StepLength) = c(10,20,30,40,50,60)
load("area52.RData")
load("area56.RData")
#run 38 - step length 30
areastepSA[1:1000,1] = 30 
areastepSA[1:1000,2] = area38
#run 45 - step length 50
areastepSA[1001:2000,1] = 50 
areastepSA[1001:1993,2] = area_s45
#run 46 - step length 40
areastepSA[2001:3000,1] = 40 
areastepSA[2001:2989,2] = area_s46
#run 56 - step length 20
areastepSA[3001:4000,1] = 20
areastepSA[3001:4000,2] = area56
#run 52 - step length 10
areastepSA[4001:5000,1] = 10
areastepSA[4001:5000,2] = area52
#run 86 - step 30 and fixed remembered code
areastepSA[4001:5000,1] = 60
areastepSA[4001:5000,2] = area86


#run 86 - check if same as run 38
load("area86.RData")
mean(area86); pop.sd(area86); hist(area86)
sort(area86)[1:10]; sort(area86)[(length(area86)-10):length(area86)]

#remove 40m and 60m
areastepSAless40 = areastepSA[-c(2001:3000),]

p = ggplot(na.omit(areastepSAless40), aes(x=StepLength, y=Area)) + geom_boxplot() + ggtitle("Area of Utilization Distribution") +
  xlab("Step Length") + ylab("Area") +
  stat_summary(fun.y="mean", geom="point", shape=3, size=3)
p + theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
          axis.title.x = element_text(size = 26, margin=margin(10,0,10,0)),
          axis.title.y = element_text(size = 26, margin=margin(0,10,0,5)),
          axis.text = element_text(size = 20))





############################## TIME BUDGET per habitat type #############################

#data file for reference - imported above
CoordData33 #run33

timebudget = data.frame(matrix(nrow=17, ncol=2))
colnames(timebudget) = c("Sum", "Proportion")
rownames(timebudget) = c(levels(run35EZ$Name))
  
#split strings
habtypes = strsplit(as.character(CoordData33$ClassNames), ",")
str(habtypes)
length(habtypes) # = 1000

hablist = c("Barren/Clouds/No Data/Developed/Water/Nonag/Undefined/Aquaculture/Open Water/Perennial Ice/Snow",
            "Blank_/Developed/Med Intensity/Developed/High Intensity","Corn","Corn_nonGMO",
            "Cotton/Rice/Sorghum/Sunflower/Peanuts/Tobacco/Sweet Corn/Pop or Orn Corn/Mint/Barley/Durum Wheat/Spring Wheat/Winter Wheat/Other",
            "Developed/Open Space/Developed/Low Intensity","Forest/Deciduous Forest/Evergreen Forest/Mixed Forest/Woody Wetlands",
            "Grass/Pasture","MWROW0","MWROW1-5","MWROW20-60","MWROW5-20","MWROW60-100","RailroadROW","Soybeans",
            "Soybeans_nonGMO","Wetlands/Herbaceous Wetlands")

#gethab = function(list,hab){grep(hab,list)}
Inds = list()
for (i in hablist){
Inds[[i]] = lapply(habtypes, function(x) grep(i, x, fixed=TRUE))
}

#find number of entries
length(habtypes[[i]])
lapply(habtypes, function(x) length(x))
sum = 0
for (i in 1:1000){
  sum = sum + length(habtypes[[i]])
}
sum #=1,550,000

Inds$`MWROW60-100`
#I can't figure out how to index the habitat types in th list Inds
timebudget[,1]=0
for (i in 1:1000){
  timebudget[1,1] = timebudget[1,1] + length(Inds$`Barren/Clouds/No Data/Developed/Water/Nonag/Undefined/Aquaculture/Open Water/Perennial Ice/Snow`[[i]])
}
for (i in 1:1000){
  timebudget[2,1] = timebudget[2,1] + length(Inds$`Blank_/Developed/Med Intensity/Developed/High Intensity`[[i]])
}
for (i in 1:1000){
  timebudget[3,1] = timebudget[3,1] + length(Inds$Corn[[i]])
}
for (i in 1:1000){
  timebudget[4,1] = timebudget[4,1] + length(Inds$Corn_nonGMO[[i]])
}
for (i in 1:1000){
  timebudget[5,1] = timebudget[5,1] + length(Inds$`Cotton/Rice/Sorghum/Sunflower/Peanuts/Tobacco/Sweet Corn/Pop or Orn Corn/Mint/Barley/Durum Wheat/Spring Wheat/Winter Wheat/Other`[[i]])
}
for (i in 1:1000){
  timebudget[6,1] = timebudget[6,1] + length(Inds$`Developed/Open Space/Developed/Low Intensity`[[i]])
}
for (i in 1:1000){
  timebudget[7,1] = timebudget[7,1] + length(Inds$`Forest/Deciduous Forest/Evergreen Forest/Mixed Forest/Woody Wetlands`[[i]])
}
for (i in 1:1000){
  timebudget[8,1] = timebudget[8,1] + length(Inds$`Grass/Pasture`[[i]])
}
for (i in 1:1000){
  timebudget[9,1] = timebudget[9,1] + length(Inds$MWROW0[[i]])
}
for (i in 1:1000){
  timebudget[10,1] = timebudget[10,1] + length(Inds$`MWROW1-5`[[i]])
}
for (i in 1:1000){
  timebudget[11,1] = timebudget[11,1] + length(Inds$`MWROW20-60`[[i]])
}
for (i in 1:1000){
  timebudget[12,1] = timebudget[12,1] + length(Inds$`MWROW5-20`[[i]])
}
for (i in 1:1000){
  timebudget[13,1] = timebudget[13,1] + length(Inds$`MWROW60-100`[[i]])
}
for (i in 1:1000){
  timebudget[14,1] = timebudget[14,1] + length(Inds$RailroadROW[[i]])
}
for (i in 1:1000){
  timebudget[15,1] = timebudget[15,1] + length(Inds$Soybeans[[i]])
}
for (i in 1:1000){
  timebudget[16,1] = timebudget[16,1] + length(Inds$Soybeans_nonGMO[[i]])
}
for (i in 1:1000){
  timebudget[17,1] = timebudget[17,1] + length(Inds$`Wetlands/Herbaceous Wetlands`[[i]])
}
#correct for extra "Corn"s
timebudget[3,1] = timebudget[3,1] - timebudget[4,1] - timebudget[5,1]
timebudget[15,1] = timebudget[15,1] - timebudget[16,1]

sumlocs = sum(timebudget$Sum) # = 1,550,000 - should be 1,550,000

for (i in 1:17){
  timebudget[i,2] = timebudget[i,1]/sumlocs
}

write.csv(timebudget, "TimeBudget33.csv")








############  Latin HyperCube Sampling  ##################

library(lhs)

# randomLHS(n,k) where n is number of intervals per parameter and k is number of parameters
# so below is 4 parameters, with 3 intervals or possibilities for each

#matrix of simulations already run
sims = matrix(
  c(0.125,0.125,0.375,0.625,0.625,0.625,0.625,0.625,0.625,0.875,0.875,0.875,0.625,0.625,0.625,
    0.375,0.375,0.375,0.125,0.375,0.375,0.375,0.375,0.625,0.125,0.375,0.625,0.375,0.375,0.375,
    0.125,0.375,0.375,0.375,0.125,0.375,0.625,0.875,0.375,0.375,0.375,0.375,0.375,0.375,0.375,
    0.625,0.625,0.625,0.625,0.625,0.625,0.625,0.625,0.625,0.625,0.625,0.625,0.875,0.375,0.125),
  nrow = 15,
  ncol = 4,
  byrow = FALSE)

#number of simulations/parameter combinations
n = 30
C = randomLHS(n,4)
Final = C



### convert random LHS numbers to my sampling situation ##########
# for 4 parameter possibilities for each parameter

LHSparms = data.frame(matrix(nrow=n,ncol=4))
colnames(LHSparms) = c("StepLength", "Remembered", "Perception", "Directionality")

#step length - 10,20,30, or 50
for (i in 1:n){
  if (C[i,1] <= 0.25){
    z = 40
  }
  if ( (C[i,1] > 0.25)&(C[i,1] <= 0.5) ){
    z = 20
  }
  if ( (C[i,1] > 0.5)&(C[i,1] <= 0.75) ){
    z = 30
  }
  if (C[i,1] > 0.75) {
    z = 50
  }
  LHSparms[i,1] = z
}

#remembered - 0,10,40, or 100
for (i in 1:n){
  if (C[i,2] <= 0.25){
    z = 0
  }
  if ( (C[i,2] > 0.25)&(C[i,2] <= 0.5) ){
    z = 10
  }
  if ( (C[i,2] > 0.5)&(C[i,2] <= 0.75) ){
    z = 40
  }
  if (C[i,2] > 0.75) {
    z = 100
  }
  LHSparms[i,2] = z
}

#Perception - 50,100,200, or 400
for (i in 1:n){
  if (C[i,3] <= 0.25){
    z = 50
  }
  if ( (C[i,3] > 0.25)&(C[i,3] <= 0.5) ){
    z = 100
  }
  if ( (C[i,3] > 0.5)&(C[i,3] <= 0.75) ){
    z = 200
  }
  if (C[i,3] > 0.75) {
    z = 400
  }
  LHSparms[i,3] = z
}

#Directionality - dir
for (i in 1:n){
  if (C[i,4] <= 0.25){
    z = "0.1-0.2"
  }
  if ( (C[i,4] > 0.25)&(C[i,4] <= 0.5) ){
    z = "0.1-0.9"
  }
  if ( (C[i,4] > 0.5)&(C[i,4] <= 0.75) ){
    z = "0.5-0.75"
  }
  if (C[i,4] > 0.75) {
    z = "0.8-0.9"
  }
  LHSparms[i,4] = z
}

write.csv(LHSparms, "ParmCombos1.csv")


####### augmented (additional) simulations

D = augmentLHS(Final, m = 5)
DFinal = D

LHSaugparms = data.frame(matrix(nrow=10,ncol=4))
colnames(LHSaugparms) = c("StepLength", "Remembered", "Perception", "Directionality")

#step length - 10,20,30, or 50
for (i in 1:10){
  if (D[i,1] <= 0.25){
    z = 10
  }
  if ( (D[i,1] > 0.25)&(D[i,1] <= 0.5) ){
    z = 20
  }
  if ( (D[i,1] > 0.5)&(D[i,1] <= 0.75) ){
    z = 30
  }
  if (D[i,1] > 0.75) {
    z = 50
  }
  LHSaugparms[i,1] = z
}

#remembered - 0,10,40, or 100
for (i in 1:10){
  if (D[i,2] <= 0.25){
    z = 0
  }
  if ( (D[i,2] > 0.25)&(D[i,2] <= 0.5) ){
    z = 10
  }
  if ( (D[i,2] > 0.5)&(D[i,2] <= 0.75) ){
    z = 40
  }
  if (D[i,2] > 0.75) {
    z = 100
  }
  LHSaugparms[i,2] = z
}

#Perception - 50,100,200, or 400
for (i in 1:10){
  if (D[i,3] <= 0.25){
    z = 50
  }
  if ( (D[i,3] > 0.25)&(D[i,3] <= 0.5) ){
    z = 100
  }
  if ( (D[i,3] > 0.5)&(D[i,3] <= 0.75) ){
    z = 200
  }
  if (D[i,3] > 0.75) {
    z = 400
  }
  LHSaugparms[i,3] = z
}

#Directionality - dir
for (i in 1:10){
  if (D[i,4] <= 0.25){
    z = "0.1-0.2"
  }
  if ( (D[i,4] > 0.25)&(D[i,4] <= 0.5) ){
    z = "0.1-0.9"
  }
  if ( (D[i,4] > 0.5)&(D[i,4] <= 0.75) ){
    z = "0.5-0.75"
  }
  if (D[i,4] > 0.75) {
    z = "0.8-0.9"
  }
  LHSaugparms[i,4] = z
}

write.csv(LHSaugparms, "ParmCombosaug1.csv")

###########  More augmented combos ##############

E = augmentLHS(DFinal, m = 5)
EFinal = E


LHSaugparms2 = data.frame(matrix(nrow=15,ncol=4))
colnames(LHSaugparms2) = c("StepLength", "Remembered", "Perception", "Directionality")

#step length - 10,20,30, or 50
for (i in 1:15){
  if (E[i,1] <= 0.25){
    z = 10
  }
  if ( (E[i,1] > 0.25)&(E[i,1] <= 0.5) ){
    z = 20
  }
  if ( (E[i,1] > 0.5)&(E[i,1] <= 0.75) ){
    z = 30
  }
  if (E[i,1] > 0.75) {
    z = 50
  }
  LHSaugparms2[i,1] = z
}

#remembered - 0,10,40, or 100
for (i in 1:15){
  if (E[i,2] <= 0.25){
    z = 0
  }
  if ( (E[i,2] > 0.25)&(E[i,2] <= 0.5) ){
    z = 10
  }
  if ( (E[i,2] > 0.5)&(E[i,2] <= 0.75) ){
    z = 40
  }
  if (E[i,2] > 0.75) {
    z = 100
  }
  LHSaugparms2[i,2] = z
}

#Perception - 50,100,200, or 400
for (i in 1:15){
  if (E[i,3] <= 0.25){
    z = 50
  }
  if ( (E[i,3] > 0.25)&(E[i,3] <= 0.5) ){
    z = 100
  }
  if ( (E[i,3] > 0.5)&(E[i,3] <= 0.75) ){
    z = 200
  }
  if (E[i,3] > 0.75) {
    z = 400
  }
  LHSaugparms2[i,3] = z
}

#Directionality - dir
for (i in 1:15){
  if (E[i,4] <= 0.25){
    z = "0.1-0.2"
  }
  if ( (E[i,4] > 0.25)&(E[i,4] <= 0.5) ){
    z = "0.1-0.9"
  }
  if ( (E[i,4] > 0.5)&(E[i,4] <= 0.75) ){
    z = "0.5-0.75"
  }
  if (E[i,4] > 0.75) {
    z = "0.8-0.9"
  }
  LHSaugparms2[i,4] = z
}

write.csv(LHSaugparms2, "ParmCombosaug2.csv")


###########  More augmented combos THREE ##############

F3 = augmentLHS(EFinal, m = 10)
FFinal = F3


LHSaugparms3 = data.frame(matrix(nrow=25,ncol=4))
colnames(LHSaugparms3) = c("StepLength", "Remembered", "Perception", "Directionality")

#step length - 10,20,30, or 50
for (i in 1:25){
  if (F3[i,1] <= 0.25){
    z = 40
  }
  if ( (F3[i,1] > 0.25)&(F3[i,1] <= 0.5) ){
    z = 20
  }
  if ( (F3[i,1] > 0.5)&(F3[i,1] <= 0.75) ){
    z = 30
  }
  if (F3[i,1] > 0.75) {
    z = 50
  }
  LHSaugparms3[i,1] = z
}

#remembered - 0,10,40, or 100
for (i in 1:25){
  if (F3[i,2] <= 0.25){
    z = 0
  }
  if ( (F3[i,2] > 0.25)&(F3[i,2] <= 0.5) ){
    z = 10
  }
  if ( (F3[i,2] > 0.5)&(F3[i,2] <= 0.75) ){
    z = 40
  }
  if (F3[i,2] > 0.75) {
    z = 100
  }
  LHSaugparms3[i,2] = z
}

#Perception - 50,100,200, or 400
for (i in 1:25){
  if (F3[i,3] <= 0.25){
    z = 50
  }
  if ( (F3[i,3] > 0.25)&(F3[i,3] <= 0.5) ){
    z = 100
  }
  if ( (F3[i,3] > 0.5)&(F3[i,3] <= 0.75) ){
    z = 200
  }
  if (F3[i,3] > 0.75) {
    z = 400
  }
  LHSaugparms3[i,3] = z
}

#Directionality - dir
for (i in 1:25){
  if (F3[i,4] <= 0.25){
    z = "0.1-0.2"
  }
  if ( (F3[i,4] > 0.25)&(F3[i,4] <= 0.5) ){
    z = "0.1-0.9"
  }
  if ( (F3[i,4] > 0.5)&(F3[i,4] <= 0.75) ){
    z = "0.5-0.75"
  }
  if (F3[i,4] > 0.75) {
    z = "0.8-0.9"
  }
  LHSaugparms3[i,4] = z
}

write.csv(LHSaugparms3, "ParmCombosaug3.csv")
save(LHSaugparms3, file="LHSaugparms3.RData")

