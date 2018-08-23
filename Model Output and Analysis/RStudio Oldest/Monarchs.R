


library(dplyr)



# summary statistics from Monarch Simulations

#Create dataframe of results to output to csv
results = data.frame(matrix(nrow=10, ncol=3))
colnames(results) = c("Tick", "MeanSteps", "SDSteps")
rownames(results) = 1:10
results$Tick = 1:10

#code to save and load object
save(CUserstgrantDocumentsMonarchButterfliesModellingwithRepastSimphonyModelOutputMonarchs22015Dec15072126txt, 
     file = "2015Dec15072126.RData")
load("2015Dec15072126.RData")


#Sensitivity Analysis 12-14-15 step length = 25
stA = CUserstgrantDocumentsMonarchButterfliesModellingwithRepastSimphonyModelOutputMonarchs22015Dec14082243txt
stB = CUserstgrantDocumentsMonarchButterfliesModellingwithRepastSimphonyModelOutputMonarchs22015Dec14093648txt
st = rbind(stA, stB)

#Sensitivity Analysis 12-14-15 directionality = 0.25
load("2015Dec14101742.RData")
DirA = CUserstgrantDocumentsMonarchButterfliesModellingwithRepastSimphonyModelOutputMonarchs22015Dec14101742txt
DirB = CUserstgrantDocumentsMonarchButterfliesModellingwithRepastSimphonyModelOutputMonarchs22015Dec14110807txt
st = rbind(DirA, DirB)

#Sensitivity Analysis 12-15-15 no distance effect
load("2015Dec15071426.RData")
load("2015Dec15072126.RData")
DstA = CUserstgrantDocumentsMonarchButterfliesModellingwithRepastSimphonyModelOutputMonarchs22015Dec15071426txt
DstB = CUserstgrantDocumentsMonarchButterfliesModellingwithRepastSimphonyModelOutputMonarchs22015Dec15072126txt
st = rbind(DstA,DstB)

#sample size = # of simulated butterflies
nrow(st)/10

#get cumSteps data from separate ticks
st1 = filter(st, tick == 1)
results[1,2] = mean(st1$cumSteps)
results[1,3] = pop.sd(st1$cumSteps)
st2 = filter(st, tick == 2)
results[2,2] = mean(st2$cumSteps)
results[2,3] = pop.sd(st2$cumSteps)
st3 = filter(st, tick == 3)
results[3,2] = mean(st3$cumSteps)
results[3,3] = pop.sd(st3$cumSteps)
st4 = filter(st, tick == 4)
results[4,2] = mean(st4$cumSteps)
results[4,3] = pop.sd(st4$cumSteps)
st5 = filter(st, tick == 5)
results[5,2] = mean(st5$cumSteps)
results[5,3] = pop.sd(st5$cumSteps)
st6 = filter(st, tick == 6)
results[6,2] = mean(st6$cumSteps)
results[6,3] = pop.sd(st6$cumSteps)
st7 = filter(st, tick == 7)
results[7,2] = mean(st7$cumSteps)
results[7,3] = pop.sd(st7$cumSteps)
st8 = filter(st, tick == 8)
results[8,2] = mean(st8$cumSteps)
results[8,3] = pop.sd(st8$cumSteps)
st9 = filter(st, tick == 9)
results[9,2] = mean(st9$cumSteps)
results[9,3] = pop.sd(st9$cumSteps)
st10 = filter(st, tick == 10)
results[10,2] = mean(st10$cumSteps)
results[10,3] = pop.sd(st10$cumSteps)

#results to csv
write.csv(results,"resultsnodistanceeff.csv")

hist(st1$cumSteps)
hist(st10$cumSteps)

#function to calc population standard deviation (with examples from Wikipedia)
vec=c(2,4,4,4,5,5,7,9)
pop.sd=function(x)(sqrt(var(x)*(length(x)-1)/length(x)))
pop.sd(vec) ##as compared to
sd(vec)

