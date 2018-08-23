##########  Monarch Global Sensitivity Analysis  ###############


library(sensitivity)
library(lhs)
library(tgp)
library(boot)

#Thiele et al. 2014 example
param.sets = lhs(n=100, rect=matrix(c(0.0,0.95,0.5,1.0), 2))


#PCC example
n <- 100
X <- data.frame(X1 = runif(n, 0.5, 1.5),
                X2 = runif(n, 1.5, 4.5),
                X3 = runif(n, 4.5, 13.5))

y <- with(X, X1 + X2 + X3)

x <- pcc(X, y, nboot = 100)

plot(x)









######## parameter combos ####################

load("LHSaugparms.RData")
load("LHSaugparms2.RData")
LHSaugparms



######## model response vectors ##############

#utilization distribution
#          (area89,   area91,   area75,  area93,   area95,  area97,  area99,  area103,area65,  area104)

UDmeans = c(116.7829, 627.9093, 120.239, 373.8415, 251.048, 348.506, 902.478, 38.696, 683.558, 175.328)
UDmedians = c(103,    621,      100,     365,      238,     355.5,   905,     29,     696.5,   165    )



#Egg Density - use mean or median egg density for grass/pasture and MWROW60-100
#these numbers are from DensperHab
#                   run89.107.110, run92,      run 76-78,   run94,       run96,       run98,       run101.102,  run103.128,  run66,      run104.105)
#run104.105 had only 5,000 agents

#Grass/Pasture
EggDensGPmeans =   c(191.6313111, 68.5709739,  129.7396394, 134.5407250, 87.3299378,  170.1690227, 65.8066686,  190.6794112, 74.2421148, 109.8001940)
                                                                                                                      #53.9159442 - run104-5
                                                                                                                      #55.8842498 - run106
EggDensGPmedians = c(106.9670746, 46.52184449, 83.7801356,  90.08232210, 30.99500895, 113.1519153, 46.76035624, 88.27919384, 65.7495727, 26.90419945)
                                                                                                                      #11.9746170 - run104-5
                                                                                                                      #13.0044538 - run106
plot(EggDensGPmeans,EggDensGPmedians) 

#MWROW60-100
EggDens60means =   c(326.7479024, 65.9817896,  329.9694367, 162.6718590, 154.8937637, 249.6431185, 98.5477328,  272.2250472, 240.1389927, 211.1435705)
                                                                                                                       #106.1823183 - run104-5
                                                                                                                       #104.9612522 - run106
EggDens60medians = c(144.0000966, 40.40919255, 224.5770913, 120.9131724, 24.14722781, 199.9988972, 72.00002076, 34.66424412, 217.2061144, 23.99999138)
                                                                                                                       #8.0000108 - run104-5
                                                                                                                       #8.0216234 - run106
plot(EggDens60means,EggDens60medians)

#mean eggs laid?




##### Partial Rank Correlation Coefficients #######

UDmeanstest = c(116.7829, 627.9093, 120.239, 373.8415, 251.048, 348.506, 902.478, 683.558)
LHSaugparmstest = rbind(LHSaugparms[1:7,1:3],LHSaugparms[9,1:3])
pccUDtest = pcc(LHSaugparmstest, UDmeanstest, nboot = 100)
pccUDtestr = pcc(LHSaugparmstest, UDmeanstest, rank=TRUE, nboot = 1000)
plot(pccUDtestr)

LHSaugparmswodir = LHSaugparms[,1:3]
prccUDmeans = pcc(LHSaugparmswodir, UDmeans, rank=TRUE, nboot=1000)
plot(prccUDmeans)
prccUDmedians = pcc(LHSaugparmswodir, UDmedians, rank=TRUE, nboot=1000)
plot(prccUDmedians)
prccUD.GPmeans = pcc(LHSaugparmswodir, EggDensGPmeans, rank=TRUE, nboot=1000)
plot(prccUD.GPmeans)
prccUD.GPmedians = pcc(LHSaugparmswodir, EggDensGPmedians, rank=TRUE, nboot=1000)
plot(prccUD.GPmedians)
prccUD.60means = pcc(LHSaugparmswodir, EggDens60means, rank=TRUE, nboot=1000)
plot(prccUD.60means)
prccUD.60medians = pcc(LHSaugparmswodir, EggDens60medians, rank=TRUE, nboot=1000)
plot(prccUD.60medians)


#######  Regression  ##############

library(MASS)

Regdata = cbind(UDmeanstest,LHSaugparmstest)
fit = lm(UDmeanstest ~ StepLength + Remembered + Perception, data=Regdata)
step = stepAIC(fit, direction="both")

Regdata = cbind(UDmeans,LHSaugparms)
fit = lm(UDmeans ~ StepLength + Remembered + Perception, data=Regdata)
summary(fit)
plot(fit)

fitdir = lm(UDmeans ~ Directionality, data=Regdata)
summary(fitdir)
plot(fitdir)





#proportion of eggs per habitat type? they would be correlated, so maybe violate assumptions














