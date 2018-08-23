##########  Monarch Global Sensitivity Analysis - Simulations from Yeti ###############

#parameter combinations from Excel
load("LHSaugparms2.RData")

library(MASS)
library(sensitivity)
library(boot)
library(grid)
library(gridExtra)
library(scales)
library(ggplot2)
library(car)

######################### model response vectors ######################################


#utilization distribution
#            (area174b, area153,  area176b, area 178, area180, area182, area184, area186, area188,  area192, area193,  area197, area198, area201, area203, area205b, area208, area211, area213, area215, area218, area220, area222,  area224,  area226)
#            (LHS1,     LHS2,     LHS3,     LHS4,     LHS5,    LHS6,    LHS7,    LHS8,    LHS9,     LHS10,   LHS11,    LHS12,   LHS13,   LHS14,   LHS15,   LHS16,    LHS17,   LHS18,   LHS19,   LHS20,   LHS21,   LHS22,   LHS23,    LHS24,    LHS25)

UDmeans =   c(853.7856, 616.5916, 1097.661, 368.731,  295.737, 274.345, 963.924, 296.75,  727.4985, 95.27,   688.4945, 299.146, 456.559, 339.992, 460.899, 992.9657, 78.499,  925.021, 953.029, 342.44, 1873.131, 268.575, 109.038,  362.995,  540.274)
UDmedians = c(859,      609,      1116,     362,      284,     278,     976.5,   259.5,   738,      80,      699,      280,     453,     342.5,   439.5,   1006,     63,      943.5,   965.5,   318,    1894,     218.5,   91.5,     359.5,    546.5)
plot(UDmeans,UDmedians)
UDmeansm2 = UDmeans*30*30
UDmeansHA = UDmeansm2/10000

#Egg Density - use mean or median egg density for grass/pasture and MWROW60-100
#                    LHS1-175, LHS2-191, LHS3-177, LHS4-179, LHS5-181, LHS6-183, LHS7-185, LHS8-187, LHS9-189, LHS10,    LHS11-195, LHS12,    LHS13-199, LHS14-202, LHS15-204, LHS16-206, LHS17,    LHS18-212, LHS19-214, LHS20-216, LHS21-219, LHS22-221, LHS23-223, LHS24-225, LHS25
#Grass/Pasture
EggDensGPmeans =   c(66.33268, 67.74928, 42.55414, 134.3366, 83.30598, 187.254,  63.32271, 96.78865, 71.71821, 109.6689, 67.71837,  117.6951, 118.7038,  166.312,   51.6515,   53.12124,  109.795,  75.85822,  63.55033,  63.84719,  29.60425, 81.73949,   178.1753,  137.9664,  89.49321)
EggDensGPmedians = c(54.89951, 44.94795, 39.49057, 94.65077, 28.19716, 119.9393, 47.08455, 54.94243, 65.40996, 22.85785, 57.6783,   66.71762, 70.95414,  130.0766,  23.94073,  45.36459,  22.34044, 70.25958,  46.78361,  26.80281,  25.80246, 24.36241,   84.59087,  93.91569,  65.26926)
plot(EggDensGPmeans,EggDensGPmedians) 
#MWROW60-100
EggDens60means =   c(137.5459, 64.2098,  149.3282, 160.8802, 143.4594, 184.8739, 94.65815, 146.3005, 233.841,  186.8568, 76.58935,  240.4291, 127.5851,   162.5881,  94.41288,  71.46837, 211.4441, 122.1607,  94.50195,  130.7744, 86.5785,   139.5264,   254.9047,  162.5131,  96.92056)
EggDens60medians = c(120.4603, 40.00028, 137.3133, 113.0471, 24.00002, 128,      71.99997, 72.00003, 216,      16,       64.39136,  120.0001, 64.00057,   135.9998,  24.06224,  64.00006, 15.99993, 113.1037,  71.99997,  71.99997, 79.99999,  16.08453,   64.00001,  120,       80.00001)
plot(EggDens60means,EggDens60medians)


#Mean proportion of eggs laid
#                    LHS1-175,  LHS2-191,  LHS3-177,  LHS4-179,  LHS5-181,  LHS6-183, LHS7-185,  LHS8-187,  LHS9-189,  LHS10,     LHS11-195, LHS12,     LHS13,     LHS14,     LHS15,     LHS16,     LHS17,    LHS18,     LHS19,     LHS20,     LHS21,     LHS22,     LHS23,     LHS24,     LHS25
EggsLaidmeans =    c(0.5051905, 0.4665619, 0.3983714, 0.782319,  0.653681,  0.957581, 0.3727619, 0.5782714, 0.6240857, 0.8747286, 0.4569952, 0.6783476, 0.7467714, 0.9268571, 0.377819,  0.4237667, 0.868019, 0.5751,    0.3709524, 0.4871048, 0.2525571, 0.6941381, 0.9341095, 0.784571,  0.5628857)
EggsLaidmedians =  c(0.5238095, 0.4761905, 0.3809524, 0.8095238, 0.6666667, 1,        0.3809524, 0.5714286, 0.6190476, 1,         0.4761905, 0.6666667, 0.7619048, 1,         0.3809524, 0.4285714, 1,        0.5714286, 0.3809524, 0.4761905, 0.2380952, 0.7142857, 1,         0.8095238, 0.5714286)
plot(EggsLaidmeans,EggsLaidmedians)




############################ Partial Rank Correlation Coefficients ########################

load("LHSaugparms2.RData")
load("LHSaugparms3.RData")

#replace 10m with 40m in first column
for (i in 1:15){
  if (LHSaugparms2[i,1]==10) LHSaugparms2[i,1]=40
}

LHSaugparmswodir = LHSaugparms3[,1:3]

#test data frames
LHSaugparmswodir12 = LHSaugparmswodir[1:12,]
LHSaugparmswodir13.16 = LHSaugparmswodir[13:16,]
LHSaugparmswodir1.11.13.16 = rbind(LHSaugparmswodir11,LHSaugparmswodir13.16)
LHSaugparmswodir13 = LHSaugparmswodir[1:13,]

prccUDmeans = pcc(LHSaugparmswodir13, UDmeans, rank=TRUE, nboot=1000)
plot(prccUDmeans) #, ylim=c(-2,2))
prccUDmedians = pcc(LHSaugparmswodir9, UDmedians, rank=TRUE, nboot=1000)
plot(prccUDmedians)
plot(LHSaugparmswodir11$StepLength,UDmeans, xlab = "Step Length", ylab = "UD Area")
plot(LHSaugparmswodir11$Remembered,UDmeans)
plot(LHSaugparmswodir9$Perception,UDmeans)

prccGPmeans = pcc(LHSaugparmswodir1.11.13.16, EggDensGPmeans, rank=TRUE, nboot=1000)
plot(prccGPmeans)
prccGPmedians = pcc(LHSaugparmswodir1.11.13.16, EggDensGPmedians, rank=TRUE, nboot=1000)
plot(prccGPmedians)

prcc60means = pcc(LHSaugparmswodir1.11.13.16, EggDens60means, rank=TRUE, nboot=1000)
plot(prcc60means)
prcc60medians = pcc(LHSaugparmswodir1.11.13.16, EggDens60medians, rank=TRUE, nboot=1000)
plot(prcc60medians)

prcceggslaidmeans = pcc(LHSaugparmswodir1.11.13.16, EggsLaidmeans, rank=TRUE, nboot=1000)
plot(prcceggslaidmeans)
prcceggslaidmedians = pcc(LHSaugparmswodir1.11.13.16, EggsLaidmedians, rank=TRUE, nboot=1000)
plot(prcceggslaidmedians)




################################  CI width by sample size  ##########################

####################  UD means #############################
CIWidthUDmeans = data.frame(matrix(nrow=25, ncol=4))
colnames(CIWidthUDmeans) = c("No.of Sims","StepLength","Remembered","Perception")
CIWidthUDmeans[,1]=1:25

LHSaugparmswodir25 = LHSaugparmswodir[1:25,]
prccUDmeans.df = pcc(LHSaugparmswodir25, UDmeans, rank=TRUE, nboot=1000)

CIWidthUDmeans[25,2:4] = prccUDmeans.df$PRCC[,5]-prccUDmeans.df$PRCC[,4]

plot(CIWidthUDmeans$`No.of Sims`, CIWidthUDmeans$StepLength, type="o", ylim=c(0,2), xlim=c(7,25),
     xlab="Number of Simulation Scenarios", ylab="Confidence Interval Width", main="Area of Utilization Distribution")
lines(CIWidthUDmeans$`No.of Sims`, CIWidthUDmeans$Remembered, type="o", lty=2)
lines(CIWidthUDmeans$`No.of Sims`, CIWidthUDmeans$Perception, type="o", lty=3)

#need to put data in different format for ggplot
ggplot(CIWidthUDmeans, aes(x=CIWidthUDmeans$`No.of Sims`, y=CIWidthUDmeans$StepLength)) + geom_line() +geom_point()

####################  mean prop eggs laid ###################
CIWidthpropeggs = data.frame(matrix(nrow=20, ncol=4))
colnames(CIWidthpropeggs) = c("No.of Sims","StepLength","Remembered","Perception")
CIWidthpropeggs[,1]=1:25

prcceggslaidmeans = pcc(LHSaugparmswodir25, EggsLaidmeans, rank=TRUE, nboot=1000)

CIWidthpropeggs[25,2:4] = prcceggslaidmeans$PRCC[,5]-prcceggslaidmeans$PRCC[,4]

plot(CIWidthpropeggs$`No.of Sims`, CIWidthpropeggs$StepLength, type="o", ylim=c(0,2), xlim=c(7,25),
     xlab="Number of Simulation Scenarios", ylab="Confidence Interval Width", main="Proportion of Eggs Laid")
lines(CIWidthpropeggs$`No.of Sims`, CIWidthpropeggs$Remembered, type="o", lty=2)
lines(CIWidthpropeggs$`No.of Sims`, CIWidthpropeggs$Perception, type="o", lty=3)


#################### egg density MWROW60-100 ###############
CIWidthMW60med = data.frame(matrix(nrow=20, ncol=4))
colnames(CIWidthMW60med) = c("No.of Sims","StepLength","Remembered","Perception")
CIWidthMW60med[,1]=1:25

prcc60medians = pcc(LHSaugparmswodir25, EggDens60medians, rank=TRUE, nboot=1000)

CIWidthMW60med[25,2:4] = prcc60medians$PRCC[,5]-prcc60medians$PRCC[,4]

plot(CIWidthMW60med$`No.of Sims`, CIWidthMW60med$StepLength, type="o", ylim=c(0,2), xlim=c(7,25),
     xlab="Number of Simulation Scenarios", ylab="Confidence Interval Width", main="Median Egg Density in ROW with 60+ MW")
lines(CIWidthMW60med$`No.of Sims`, CIWidthMW60med$Remembered, type="o", lty=2)
lines(CIWidthMW60med$`No.of Sims`, CIWidthMW60med$Perception, type="o", lty=3)


#################### egg density Grass/Pasture #############
CIWidthGPmed = data.frame(matrix(nrow=20, ncol=4))
colnames(CIWidthGPmed) = c("No.of Sims","StepLength","Remembered","Perception")
CIWidthGPmed[,1]=1:25

prccGPmedians = pcc(LHSaugparmswodir25, EggDensGPmedians, rank=TRUE, nboot=1000)

CIWidthGPmed[25,2:4] = prccGPmedians$PRCC[,5]-prccGPmedians$PRCC[,4]

plot(CIWidthGPmed$`No.of Sims`, CIWidthGPmed$StepLength, type="o", ylim=c(0,2), xlim=c(7,25),
     xlab="Number of Simulation Scenarios", ylab="Confidence Interval Width", main="Median Egg Density in Grass/Pasture")
lines(CIWidthGPmed$`No.of Sims`, CIWidthGPmed$Remembered, type="o", lty=2)
lines(CIWidthGPmed$`No.of Sims`, CIWidthGPmed$Perception, type="o", lty=3)




###############  Exploratory Scatterplots  ########################

# UDmeans
plot(LHSaugparms3$StepLength, UDmeans)
plot(LHSaugparms3$Remembered, UDmeans)
plot(LHSaugparms3$Perception, UDmeans)
LHSaugparms3$Directionality = factor(LHSaugparms3$Directionality)
plot(LHSaugparms3$Directionality, UDmeans)
points(LHSaugparms3$Directionality, UDmeans)
plot.default(LHSaugparms3$Directionality, UDmeans)




##############  Standarized Regression Coefficients with R^2 and Estimate Graphs  ########################

############ sigmas to calc SRCs  ###############

sd(UDmeans)
sd(EggsLaidmeans)
sd(EggDens60medians)
sd(EggDensGPmedians)

sd(LHSaugparms3$StepLength)
sd(LHSaugparms3$Remembered)
sd(LHSaugparms3$Perception)

########### standardized variables to test against my calcs below ###############

UDmeans.std = scale(UDmeans)
EggsLaidmeans.std = scale(EggsLaidmeans)
EggDens60medians.std = scale(EggDens60medians)
EggDensGPmedians.std = scale(EggDensGPmedians)
SL.std = scale(LHSaugparms3$StepLength)
PR.std = scale(LHSaugparms3$Perception)
RM.std = scale(LHSaugparms3$Remembered)

################## UDs ##################

## Rank
srccUDmeans.df = src(LHSaugparmswodir25, UDmeans, rank=TRUE, nboot=1000)
plot(srccUDmeans.df)
plot(prccUDmeans.df)

## NOT Rank
srcUDmeans.df = src(LHSaugparmswodir25, UDmeans, rank=FALSE, nboot=1000)
#this one doesn't work
srcUDmeanswdir = src(LHSaugparms3, UDmeans, rank=FALSE, nboot=1000)
pccUDmeans.df = pcc(LHSaugparmswodir25, UDmeans, rank=FALSE, nboot=1000)
plot(srcUDmeans.df)
plot(pccUDmeans.df)

# this is how Thiele et al. 2014 did it, but its supposed to be the R^2 for the whole model, not just one at a time
UDlmSL = lm(UDmeans ~ LHSaugparms3$StepLength)
summary(UDlmSL)$r.squared
UDlmRM = lm(UDmeans ~ LHSaugparms3$Remembered)
summary(UDlmRM)$r.squared
UDlmPR = lm(UDmeans ~ LHSaugparms3$Perception)
summary(UDlmPR)$r.squared
UDlmDR = lm(UDmeans ~ LHSaugparms3$Directionality)
summary(UDlmDR)$r.squared
## Step Length R^2 = 0.4611485
## Remembered R^2 = 0.1585989
## Perception R^2 = 0.290042
## Directionality R^2 = 0.178097

### standardized coeffs without dir to compare to src() - they are the same as results from src() - CIs not the same
#because src() uses bootstrap

BSL.d = 20.4997*(sd(LHSaugparms3$StepLength)/sd(UDmeans))
BSL.d.lci = 10.9269842*(sd(LHSaugparms3$StepLength)/sd(UDmeans))
BSL.d.uci = 30.0723541*(sd(LHSaugparms3$StepLength)/sd(UDmeans))
BPR.d = -1.4836*(sd(LHSaugparms3$Perception)/sd(UDmeans))
BRM.d = 2.3771 *(sd(LHSaugparms3$Remembered)/sd(UDmeans))


######  Good Ol' Mutiple Regression

#vars for regression as vectors
SL = LHSaugparms3$StepLength
PR = LHSaugparms3$Perception
RM = LHSaugparms3$Remembered
DR = LHSaugparms3$Directionality
DR = relevel(DR, ref="0.5-0.75")

#mutiple linear reg
UDMR = lm(UDmeans ~ LHSaugparms3$StepLength + LHSaugparms3$Perception + LHSaugparms3$Remembered)
summary(UDMR)
UDMRDR = lm(UDmeans ~ LHSaugparms3$StepLength + LHSaugparms3$Perception + LHSaugparms3$Remembered + LHSaugparms3$Directionality)
summary(UDMRDR)
anova(UDMRDR)

#various results
fitted(UDMRDR)
coefficients(UDMRDR)
confint(UDMR, level=0.95)

#model selection - retained all 4 parms - but I decided no point in doing model selection
MS = stepAIC(UDMRDR)
MS$anova

### change levels on directionality so that 0.5-0.75 is reference/intercept level
LHSaugparm3dirlvl = LHSaugparms3
levels(LHSaugparm3dirlvl$Directionality)
LHSaugparm3dirlvl$Directionality = relevel(LHSaugparm3dirlvl$Directionality, ref = "0.5-0.75")

UDMRDR.lvldir = lm(UDmeans ~ LHSaugparms3$StepLength + LHSaugparms3$Perception + 
                     LHSaugparms3$Remembered + LHSaugparm3dirlvl$Directionality)
summary(UDMRDR.lvldir)
#Final
UDMRDR.lvldir = lm(UDmeans ~ SL + PR + RM + DR) #exact same result as above
#FINAL in HA
UDMRDR.HA = lm(UDmeansHA ~ SL + PR + RM + DR)
summary(UDMRDR.HA)
UDMRDR.HA.DR = lm(UDmeansHA ~ DR + SL + PR +RM)
summary(UDMRDR.HA.DR)

#### lm() wit standardized variables ############

UDmeans.std = scale(UDmeans)
SL.std = scale(LHSaugparms3$StepLength)
PR.std = scale(LHSaugparms3$Perception)
RM.std = scale(LHSaugparms3$Remembered)
UDmeans.std.ha = scale(UDmeansHA)

#contrasts(DR) = contr.sum(4)
UDMRDR.lvldir.std = lm(UDmeans.std ~ SL.std + PR.std + RM.std + DR)
summary(UDMRDR.lvldir.std)
#with HA just make sure they are the same
UDMRDR.lvldir.std.ha = lm(UDmeans.std.ha ~ SL.std + PR.std + RM.std + DR)
summary(UDMRDR.lvldir.std.ha)
anova(UDMRDR.lvldir.std.ha)
qqPlot(UDMRDR.lvldir.std.ha) 

#demonstration that order matters - non-orthogonal
UDMRDR.lvldir.std.ha.B = lm(UDmeans.std.ha ~ DR + SL.std + PR.std + RM.std)
anova(UDMRDR.lvldir.std.ha.B)



#### everything is the same except the intercept - can't figure out how to convert the intercept
#### I guess I have to go back and so them all again with standardized variables.  

#####  data frame for SRC results ################
SRC.UD = data.frame(matrix(nrow=7, ncol=6))
colnames(SRC.UD) = c("SRC","Analytical LCL","Analytical UCL","Bootstrap LCL","Bootstrap UCL","Sx=SRC^2")
rownames(SRC.UD) = c("Intercept","Step Length","Remembered","Perception","Dir 0.1-0.2","Dir 0.1-0.9","Dir 0.8-0.9")

SRC.UD[1,1] = coefficients(UDMRDR.lvldir.std)[1]
SRC.UD[2,1] = coefficients(UDMRDR.lvldir.std)[2]
SRC.UD[4,1] = coefficients(UDMRDR.lvldir.std)[3]
SRC.UD[3,1] = coefficients(UDMRDR.lvldir.std)[4]
SRC.UD[5,1] = coefficients(UDMRDR.lvldir.std)[5]
SRC.UD[6,1] = coefficients(UDMRDR.lvldir.std)[6]
SRC.UD[7,1] = coefficients(UDMRDR.lvldir.std)[7]
## squared reg coefficients
SRC.UD[1,6] = SRC.UD[1,1]^2
SRC.UD[2,6] = SRC.UD[2,1]^2
SRC.UD[3,6] = SRC.UD[3,1]^2
SRC.UD[4,6] = SRC.UD[4,1]^2
SRC.UD[5,6] = SRC.UD[5,1]^2
SRC.UD[6,6] = SRC.UD[6,1]^2
SRC.UD[7,6] = SRC.UD[7,1]^2
## Analytical LCL and UCL - just standardizing CLs like estimate
SRC.UD[1,2] = confint(UDMRDR.lvldir.std, level=0.95)[1,1]
SRC.UD[1,3] = confint(UDMRDR.lvldir.std, level=0.95)[1,2]
SRC.UD[2,2] = confint(UDMRDR.lvldir.std, level=0.95)[2,1]
SRC.UD[2,3] = confint(UDMRDR.lvldir.std, level=0.95)[2,2]
SRC.UD[4,2] = confint(UDMRDR.lvldir.std, level=0.95)[3,1]
SRC.UD[4,3] = confint(UDMRDR.lvldir.std, level=0.95)[3,2]
SRC.UD[3,2] = confint(UDMRDR.lvldir.std, level=0.95)[4,1]
SRC.UD[3,3] = confint(UDMRDR.lvldir.std, level=0.95)[4,2]
SRC.UD[5,2] = confint(UDMRDR.lvldir.std, level=0.95)[5,1]
SRC.UD[5,3] = confint(UDMRDR.lvldir.std, level=0.95)[5,2]
SRC.UD[6,2] = confint(UDMRDR.lvldir.std, level=0.95)[6,1]
SRC.UD[6,3] = confint(UDMRDR.lvldir.std, level=0.95)[6,2]
SRC.UD[7,2] = confint(UDMRDR.lvldir.std, level=0.95)[7,1]
SRC.UD[7,3] = confint(UDMRDR.lvldir.std, level=0.95)[7,2]


######  BOOTSTRAP  the CIs ##############

summary(UDMRDR.lvldir)$coefficients
coef(UDMRDR.lvldir)

coefs = function(formula, data, indices) {
  d = data[indices,]
  fit = lm(formula, data=d)
  return(coef(fit))
}

boot.UD = boot(data=UDmeans, statistic = coefs, R=100, formula=UDmeans ~ SL + PR + RM + DR)




############# ORIGINAL UNITS ##############
#########  PREDICTED graphs of relationships, others held at mean  ##############
p = predict(UDMRDR.lvldir, interval = "conf")
plot(UDmeans, p[,1])
predict(UDMRDR.lvldir, list(SL=35, PR=225, RM=50, DR="0.5-0.75"), interval="conf")

#need dataframe that feeds in correctly to predict(), the above predict() command works

####### STEP LENGTH ################
pred.sl.df = data.frame(SL = c(20,30,40,50), PR=225, RM=50, DR="0.5-0.75")
pred.sl = data.frame(predict(UDMRDR.lvldir, pred.sl.df, interval = "conf"))
pred.sl$SL = c(20,30,40,50)

p1 = ggplot(pred.sl, aes(x=SL, y=fit)) + geom_line() + geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + ylim(0,1200) +
  ggtitle("Step Length") + xlab("Step Length (m)") + ylab("Area of Utilization Distribution") +
  theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
          axis.title.x = element_text(size = 26, margin=margin(10,0,10,0)),
          axis.title.y = element_text(size = 26, margin=margin(0,10,0,5)),
          axis.text = element_text(size = 20))

######## PERCEPTION DISTANCE ###############
pred.per.df = data.frame(SL = 35, PR=c(50,100,200,400), RM=50, DR="0.5-0.75")
pred.per = data.frame(predict(UDMRDR.lvldir, pred.per.df, interval = "conf"))
pred.per$perc = c(50,100,200,400)

p2 = ggplot(pred.per, aes(x=perc, y=fit)) + geom_line() + geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + ylim(0,1200) +
  ggtitle("Perception Distance") + xlab("Perception Distance (m)") + #ylab("Area of Utilization Distribution") +
  theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
          axis.title.x = element_text(size = 26, margin=margin(10,0,10,0)),
          axis.title.y = element_blank(),
          axis.text = element_text(size = 20))

######## REMEMBERED ###############
pred.rem.df = data.frame(SL = 35, PR=225, RM=c(0,10,40,100), DR="0.5-0.75")
pred.rem = data.frame(predict(UDMRDR.lvldir, pred.rem.df, interval = "conf"))
pred.rem$rem = c(0,10,40,100)

p3 = ggplot(pred.rem, aes(x=rem, y=fit)) + geom_line() + geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + ylim(0,1200) +
  ggtitle("No. of Polygons Remembered") + xlab("No. Polygons Remembered") + ylab("Area of Utilization Distribution") +
  theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
          axis.title.x = element_text(size = 26, margin=margin(10,0,10,0)),
          axis.title.y = element_text(size = 26, margin=margin(0,10,0,5)),
          axis.text = element_text(size = 20))


######### DIRECTIONALITY #####################
pred.dir.df = data.frame(SL = 35, PR=225, RM=50, DR= c("0.5-0.75","0.1-0.2","0.1-0.9","0.8-0.9"))
pred.dir = data.frame(predict(UDMRDR.lvldir, pred.dir.df, interval = "conf"))
pred.dir$dir = c("0.5-0.75","0.1-0.2","0.1-0.9","0.8-0.9")

p4 = ggplot(pred.dir, aes(x=dir, y=fit)) + geom_bar(stat="identity", fill = "#CCCCCC") + ylim(0,1200) + 
  geom_errorbar(aes(ymin=lwr, ymax=upr), width=0.2) +
  ggtitle("Directionality") + xlab("Directionality Scenarios") + #ylab("Area of Utilization Distribution") +
  theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
          axis.title.x = element_text(size = 26, margin=margin(10,0,10,0)),
          axis.title.y = element_blank(),
          axis.text = element_text(size = 20))


grid.arrange(p1,p2,p3,p4, ncol=2)



############# *** HECTARES ***  HECTARES *** ##############

####### STEP LENGTH ################
pred.sl.df = data.frame(SL = c(20,30,40,50), PR=225, RM=50, DR="0.5-0.75")
pred.sl = data.frame(predict(UDMRDR.HA, pred.sl.df, interval = "conf"))
pred.sl$SL = c(20,30,40,50)

p1.ha = ggplot(pred.sl, aes(x=SL, y=fit)) + geom_line() + geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + ylim(0,125) +
  #ggtitle("Step Length") + 
  ylab("Utilization Distribution \n(Ha)") + #xlab("Step Length (m)") + 
  theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
        #axis.title.x = element_text(size = 16, margin=margin(5,0,5,0)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16, margin=margin(0,5,0,5)),
        axis.text = element_text(size = 18))

######## PERCEPTION DISTANCE ###############
pred.per.ud.df = data.frame(SL = 35, PR=c(50,100,200,400), RM=50, DR="0.5-0.75")
pred.per.ud = data.frame(predict(UDMRDR.HA, pred.per.ud.df, interval = "conf"))
pred.per.ud$perc = c(50,100,200,400)

p2.ha = ggplot(pred.per.ud, aes(x=perc, y=fit)) + geom_line() + geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + ylim(0,125) +
  ylab("\nUtilization Distribution (Ha)") + #xlab("Perception Distance (m)") + 
  theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16, margin=margin(0,10,0,5)),
        axis.text = element_text(size = 18))

######## REMEMBERED ###############
pred.rem.ha.df = data.frame(SL = 35, PR=225, RM=c(0,10,40,100), DR="0.5-0.75")
pred.rem.ha = data.frame(predict(UDMRDR.HA, pred.rem.ha.df, interval = "conf"))
pred.rem.ha$rem = c(0,10,40,100)

p3.ha = ggplot(pred.rem.ha, aes(x=rem, y=fit)) + geom_line() + geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + ylim(0,125) +
  ylab("\nUtilization Distribution (Ha)") + #xlab("No. Polygons Remembered") + 
  theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16, margin=margin(0,10,0,5)),
        axis.text = element_text(size = 18))


######### DIRECTIONALITY #####################
pred.dir.df = data.frame(SL = 35, PR=225, RM=50, DR= c("0.5-0.75","0.1-0.2","0.1-0.9","0.8-0.9"))
pred.dir = data.frame(predict(UDMRDR.HA, pred.dir.df, interval = "conf"))
pred.dir$dir = c("0.5-0.75","0.1-0.2","0.1-0.9","0.8-0.9")

p4.ha = ggplot(pred.dir, aes(x=dir, y=fit)) + geom_bar(stat="identity", fill = "#CCCCCC") + ylim(0,125) + 
  geom_errorbar(aes(ymin=lwr, ymax=upr), width=0.2) +
  ylab("\nUtilization Distribution (Ha)") + #xlab("Directionality Scenarios") + 
  theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16, margin=margin(0,10,0,5)),
        axis.text = element_text(size = 18))


grid.arrange(p1.ha,p2.ha,p3.ha,p4.ha, ncol=2, top = textGrob("Utilization Distribution", gp=gpar(fontsize=30)))





##############  Mean Prop Eggs  #####################

PEMR = lm(EggsLaidmeans ~ LHSaugparms3$StepLength + LHSaugparms3$Perception + LHSaugparms3$Remembered)
summary(PEMR)
PEMRDR = lm(EggsLaidmeans ~ LHSaugparms3$StepLength + LHSaugparms3$Perception + LHSaugparms3$Remembered + LHSaugparms3$Directionality)
summary(PEMRDR)
PEMRDR.lvl = lm(EggsLaidmeans ~ SL + PR + RM + DR) 
summary(PEMRDR.lvl)
PEMRDR.lvl.std = lm(EggsLaidmeans.std ~ SL.std + RM.std + PR.std + DR)
summary(PEMRDR.lvl.std)

PEMS = stepAIC(PEMRDR)
PEMS$anova

## SRCs ####
srcpropeggs = src(LHSaugparmswodir25, EggsLaidmeans, rank=FALSE, nboot=1000)


#####  data frame for SRC results ################
SRC.eggslaid = data.frame(matrix(nrow=7, ncol=6))
colnames(SRC.eggslaid) = c("SRC","Analytical LCL","Analytical UCL","Bootstrap LCL","Bootstrap UCL","Sx=SRC^2")
rownames(SRC.eggslaid) = c("Intercept","Step Length","Remembered","Perception","Dir 0.1-0.2","Dir 0.1-0.9","Dir 0.8-0.9")

SRC.eggslaid[1,1] = coefficients(PEMRDR.lvl.std)[1]
SRC.eggslaid[2,1] = coefficients(PEMRDR.lvl.std)[2]
SRC.eggslaid[3,1] = coefficients(PEMRDR.lvl.std)[3]
SRC.eggslaid[4,1] = coefficients(PEMRDR.lvl.std)[4]
SRC.eggslaid[5,1] = coefficients(PEMRDR.lvl.std)[5]
SRC.eggslaid[6,1] = coefficients(PEMRDR.lvl.std)[6]
SRC.eggslaid[7,1] = coefficients(PEMRDR.lvl.std)[7]
## squared reg coefficients
SRC.eggslaid[1,6] = SRC.eggslaid[1,1]^2
SRC.eggslaid[2,6] = SRC.eggslaid[2,1]^2
SRC.eggslaid[3,6] = SRC.eggslaid[3,1]^2
SRC.eggslaid[4,6] = SRC.eggslaid[4,1]^2
SRC.eggslaid[5,6] = SRC.eggslaid[5,1]^2
SRC.eggslaid[6,6] = SRC.eggslaid[6,1]^2
SRC.eggslaid[7,6] = SRC.eggslaid[7,1]^2
## Analytical LCL and UCL - just standardizing CLs like estimate
SRC.eggslaid[1,2] = confint(PEMRDR.lvl.std, level=0.95)[1,1]
SRC.eggslaid[1,3] = confint(PEMRDR.lvl.std, level=0.95)[1,2]
SRC.eggslaid[2,2] = confint(PEMRDR.lvl.std, level=0.95)[2,1]
SRC.eggslaid[2,3] = confint(PEMRDR.lvl.std, level=0.95)[2,2]
SRC.eggslaid[3,2] = confint(PEMRDR.lvl.std, level=0.95)[3,1]
SRC.eggslaid[3,3] = confint(PEMRDR.lvl.std, level=0.95)[3,2]
SRC.eggslaid[4,2] = confint(PEMRDR.lvl.std, level=0.95)[4,1]
SRC.eggslaid[4,3] = confint(PEMRDR.lvl.std, level=0.95)[4,2]
SRC.eggslaid[5,2] = confint(PEMRDR.lvl.std, level=0.95)[5,1]
SRC.eggslaid[5,3] = confint(PEMRDR.lvl.std, level=0.95)[5,2]
SRC.eggslaid[6,2] = confint(PEMRDR.lvl.std, level=0.95)[6,1]
SRC.eggslaid[6,3] = confint(PEMRDR.lvl.std, level=0.95)[6,2]
SRC.eggslaid[7,2] = confint(PEMRDR.lvl.std, level=0.95)[7,1]
SRC.eggslaid[7,3] = confint(PEMRDR.lvl.std, level=0.95)[7,2]

#########  PREDICTED graphs of relationships, others held at mean  ##############
predict(PEMRDR.lvl, list(SL=35, PR=225, RM=50, DR="0.5-0.75"), interval="conf")

#need dataframe that feeds in correctly to predict(), the above predict() command works

####### STEP LENGTH ################
pred.sl.prop.df = data.frame(SL = c(20,30,40,50), PR=225, RM=50, DR="0.5-0.75")
pred.sl.prop = data.frame(predict(PEMRDR.lvl, pred.sl.df, interval = "conf"))
pred.sl.prop$SL = c(20,30,40,50)

p5 = ggplot(pred.sl.prop, aes(x=SL, y=fit)) + geom_line() + geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + ylim(0,1) +
  ylab("\nProportion of Eggs Laid") + #xlab("Step Length (m)") + 
  theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16, margin=margin(0,10,0,5)),
        axis.text = element_text(size = 18))

##########  PERCEPTION DISTANCE ##############
pred.per.df = data.frame(SL = 35, PR=c(50,100,200,400), RM=50, DR="0.5-0.75")
pred.per = data.frame(predict(PEMRDR.lvl, pred.per.df, interval = "conf"))
pred.per$perc = c(50,100,200,400)

p6 = ggplot(pred.per, aes(x=perc, y=fit)) + geom_line() + geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) +
  xlab("Perception Distance (m)") + ylab("\nProportion of Eggs Laid") +
  ylim(0,1) +
  theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 16, margin=margin(0,10,0,5)),
          axis.text = element_text(size = 18))

########### REMEMBERED #######################
pred.rem.df = data.frame(SL = 35, PR=225, RM=c(0,10,40,100), DR="0.5-0.75")
pred.rem = data.frame(predict(PEMRDR.lvl, pred.rem.df, interval = "conf"))
pred.rem$rem = c(0,10,40,100)

p7 = ggplot(pred.rem, aes(x=rem, y=fit)) + geom_line() + geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + ylim(0,1) +
  xlab("Number of Polygons Remembered") + ylab("\nProportion of Eggs Laid") +
  theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16, margin=margin(0,10,0,5)),
        axis.text = element_text(size = 18))


######  DIRECTIONALITY ####################
pred.dir.prop.df = data.frame(SL = 35, PR=225, RM=50, DR= c("0.5-0.75","0.1-0.2","0.1-0.9","0.8-0.9"))
pred.dir.prop = data.frame(predict(PEMRDR.lvl, pred.dir.df, interval = "conf"))
pred.dir.prop$dir = c("0.5-0.75","0.1-0.2","0.1-0.9","0.8-0.9")

p8 = ggplot(pred.dir.prop, aes(x=dir, y=fit)) + geom_bar(stat="identity", fill = "#CCCCCC") + ylim(0,1) + 
  geom_errorbar(aes(ymin=lwr, ymax=upr), width=0.2) +
  ylab("\nProportion of Eggs Laid") + xlab("Directionality Scenarios") + 
  theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16, margin=margin(0,10,0,5)),
        axis.text = element_text(size = 18))


grid.arrange(p5,p6,p7,p8, ncol=2, top = textGrob("Mean Proportion of Eggs Laid", gp=gpar(fontsize=30)))



#############  Egg Density MWROW60+  ###################

RWMR = lm(EggDens60medians ~ LHSaugparms3$StepLength + LHSaugparms3$Perception + LHSaugparms3$Remembered)
summary(RWMR)
RWMRDR = lm(EggDens60medians ~ LHSaugparms3$StepLength + LHSaugparms3$Perception + LHSaugparms3$Remembered + LHSaugparms3$Directionality)
summary(RWMRDR)
RWMRDR.lvl = lm(EggDens60medians ~ SL + PR + RM + DR) 
summary(RWMRDR.lvl)
RWMRDR.lvl.std = lm(EggDens60medians.std ~ SL.std + RM.std + PR.std + DR) 
summary(RWMRDR.lvl.std)

RWMS = stepAIC(RWMRDR)
RWMS$anova

## SRCs ####
srcdensMW = src(LHSaugparmswodir25, EggDens60medians, rank=FALSE, nboot=1000)


#####  data frame for SRC results ################
SRC.MW60 = data.frame(matrix(nrow=7, ncol=6))
colnames(SRC.MW60) = c("SRC","Analytical LCL","Analytical UCL","Bootstrap LCL","Bootstrap UCL","Sx=SRC^2")
rownames(SRC.MW60) = c("Intercept","Step Length","Remembered","Perception","Dir 0.1-0.2","Dir 0.1-0.9","Dir 0.8-0.9")

SRC.MW60[1,1] = coefficients(RWMRDR.lvl.std)[1]
SRC.MW60[2,1] = coefficients(RWMRDR.lvl.std)[2]
SRC.MW60[3,1] = coefficients(RWMRDR.lvl.std)[3]
SRC.MW60[4,1] = coefficients(RWMRDR.lvl.std)[4]
SRC.MW60[5,1] = coefficients(RWMRDR.lvl.std)[5]
SRC.MW60[6,1] = coefficients(RWMRDR.lvl.std)[6]
SRC.MW60[7,1] = coefficients(RWMRDR.lvl.std)[7]
## squared reg coefficients
SRC.MW60[1,6] = SRC.MW60[1,1]^2
SRC.MW60[2,6] = SRC.MW60[2,1]^2
SRC.MW60[3,6] = SRC.MW60[3,1]^2
SRC.MW60[4,6] = SRC.MW60[4,1]^2
SRC.MW60[5,6] = SRC.MW60[5,1]^2
SRC.MW60[6,6] = SRC.MW60[6,1]^2
SRC.MW60[7,6] = SRC.MW60[7,1]^2
## Analytical LCL and UCL - just standardizing CLs like estimate
SRC.MW60[1,2] = confint(RWMRDR.lvl.std, level=0.95)[1,1]
SRC.MW60[1,3] = confint(RWMRDR.lvl.std, level=0.95)[1,2]
SRC.MW60[2,2] = confint(RWMRDR.lvl.std, level=0.95)[2,1]
SRC.MW60[2,3] = confint(RWMRDR.lvl.std, level=0.95)[2,2]
SRC.MW60[3,2] = confint(RWMRDR.lvl.std, level=0.95)[3,1]
SRC.MW60[3,3] = confint(RWMRDR.lvl.std, level=0.95)[3,2]
SRC.MW60[4,2] = confint(RWMRDR.lvl.std, level=0.95)[4,1]
SRC.MW60[4,3] = confint(RWMRDR.lvl.std, level=0.95)[4,2]
SRC.MW60[5,2] = confint(RWMRDR.lvl.std, level=0.95)[5,1]
SRC.MW60[5,3] = confint(RWMRDR.lvl.std, level=0.95)[5,2]
SRC.MW60[6,2] = confint(RWMRDR.lvl.std, level=0.95)[6,1]
SRC.MW60[6,3] = confint(RWMRDR.lvl.std, level=0.95)[6,2]
SRC.MW60[7,2] = confint(RWMRDR.lvl.std, level=0.95)[7,1]
SRC.MW60[7,3] = confint(RWMRDR.lvl.std, level=0.95)[7,2]


#########  PREDICTED graphs of relationships, others held at mean  ##############
predict(RWMRDR.lvl, list(SL=35, PR=225, RM=50, DR="0.5-0.75"), interval="conf")

####### STEP LENGTH ################
pred.sl.ed60.df = data.frame(SL = c(20,30,40,50), PR=225, RM=50, DR="0.5-0.75")
pred.sl.ed60 = data.frame(predict(RWMRDR.lvl, pred.sl.df, interval = "conf"))
pred.sl.ed60$SL = c(20,30,40,50)

p9 = ggplot(pred.sl.ed60, aes(x=SL, y=fit)) + geom_line() + geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + ylim(-25,150) +
  ylab("Median Egg Density\n(Roadsides)") + #xlab("Step Length (m)") + 
  theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16, margin=margin(0,10,0,5)),
        axis.text = element_text(size = 18))

##########  PERCEPTION DISTANCE ##############
pred.per.ed.df = data.frame(SL = 35, PR=c(50,100,200,400), RM=50, DR="0.5-0.75")
pred.per.ed = data.frame(predict(RWMRDR.lvl, pred.per.ed.df, interval = "conf"))
pred.per.ed$perc = c(50,100,200,400)

p10 = ggplot(pred.per.ed, aes(x=perc, y=fit)) + geom_line() + geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) +
  ylab("Median Egg Density\n(Roadsides)") + #xlab("Perception Distance (m)") + 
  ylim(-25,150) +
  theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16, margin=margin(0,10,0,5)),
        axis.text = element_text(size = 18))


########### REMEMBERED #######################
pred.rem.ed60.df = data.frame(SL = 35, PR=225, RM=c(0,10,40,100), DR="0.5-0.75")
pred.rem.ed60 = data.frame(predict(RWMRDR.lvl, pred.rem.ed60.df, interval = "conf"))
pred.rem.ed60$rem = c(0,10,40,100)

p11 = ggplot(pred.rem.ed60, aes(x=rem, y=fit)) + geom_line() + geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + ylim(-25,150) +
  xlab("Number of Polygons Remembered") + ylab("Median Egg Density\n(Roadsides)") +
  theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16, margin=margin(0,10,0,5)),
        axis.text = element_text(size = 18))


######  DIRECTIONALITY ####################
pred.dir.ed60.df = data.frame(SL = 35, PR=225, RM=50, DR= c("0.5-0.75","0.1-0.2","0.1-0.9","0.8-0.9"))
pred.dir.ed60 = data.frame(predict(RWMRDR.lvl, pred.dir.df, interval = "conf"))
pred.dir.ed60$dir = c("0.5-0.75","0.1-0.2","0.1-0.9","0.8-0.9")

p12 = ggplot(pred.dir.ed60, aes(x=dir, y=fit)) + geom_bar(stat="identity", fill = "#CCCCCC") + #ylim(-25,150) + 
  geom_errorbar(aes(ymin=lwr, ymax=upr), width=0.2) +
  ylab("Median Egg Density\n(Roadsides)") + xlab("Directionality Scenarios") + 
  theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16, margin=margin(0,10,0,5)),
        axis.text = element_text(size = 18))


grid.arrange(p9,p10,p11,p12, ncol=2, top = textGrob("Median Egg Density in Right-of-Ways", gp=gpar(fontsize=30)))






#############  Egg Density in G/P  #########################

GPMR = lm(EggDensGPmedians ~ LHSaugparms3$StepLength + LHSaugparms3$Perception + LHSaugparms3$Remembered)
summary(GPMR)
GPMRDR = lm(EggDensGPmedians ~ LHSaugparms3$StepLength + LHSaugparms3$Perception + LHSaugparms3$Remembered + LHSaugparms3$Directionality)
summary(GPMRDR)
GPMRDR.lvl = lm(EggDensGPmedians ~ SL + PR + RM + DR) 
summary(GPMRDR.lvl)
GPMRDR.lvl.std = lm(EggDensGPmedians.std ~ SL.std + RM.std + PR.std + DR) 
summary(GPMRDR.lvl.std)


GPMS = stepAIC(GPMRDR)
GPMS$anova

## SRCs ####
srcdensGP = src(LHSaugparmswodir25, EggDensGPmedians, rank=FALSE, nboot=1000)

#####  data frame for SRC results ################
SRC.GP = data.frame(matrix(nrow=7, ncol=6))
colnames(SRC.GP) = c("SRC","Analytical LCL","Analytical UCL","Bootstrap LCL","Bootstrap UCL","Sx=SRC^2")
rownames(SRC.GP) = c("Intercept","Step Length","Remembered","Perception","Dir 0.1-0.2","Dir 0.1-0.9","Dir 0.8-0.9")

SRC.GP[1,1] = coefficients(GPMRDR.lvl.std)[1]
SRC.GP[2,1] = coefficients(GPMRDR.lvl.std)[2]
SRC.GP[3,1] = coefficients(GPMRDR.lvl.std)[3]
SRC.GP[4,1] = coefficients(GPMRDR.lvl.std)[4]
SRC.GP[5,1] = coefficients(GPMRDR.lvl.std)[5]
SRC.GP[6,1] = coefficients(GPMRDR.lvl.std)[6]
SRC.GP[7,1] = coefficients(GPMRDR.lvl.std)[7]
## squared reg coefficients
SRC.GP[1,6] = SRC.GP[1,1]^2
SRC.GP[2,6] = SRC.GP[2,1]^2
SRC.GP[3,6] = SRC.GP[3,1]^2
SRC.GP[4,6] = SRC.GP[4,1]^2
SRC.GP[5,6] = SRC.GP[5,1]^2
SRC.GP[6,6] = SRC.GP[6,1]^2
SRC.GP[7,6] = SRC.GP[7,1]^2
## Analytical LCL and UCL - just standardizing CLs like estimate
SRC.GP[1,2] = confint(GPMRDR.lvl.std, level=0.95)[1,1]
SRC.GP[1,3] = confint(GPMRDR.lvl.std, level=0.95)[1,2]
SRC.GP[2,2] = confint(GPMRDR.lvl.std, level=0.95)[2,1]
SRC.GP[2,3] = confint(GPMRDR.lvl.std, level=0.95)[2,2]
SRC.GP[3,2] = confint(GPMRDR.lvl.std, level=0.95)[3,1]
SRC.GP[3,3] = confint(GPMRDR.lvl.std, level=0.95)[3,2]
SRC.GP[4,2] = confint(GPMRDR.lvl.std, level=0.95)[4,1]
SRC.GP[4,3] = confint(GPMRDR.lvl.std, level=0.95)[4,2]
SRC.GP[5,2] = confint(GPMRDR.lvl.std, level=0.95)[5,1]
SRC.GP[5,3] = confint(GPMRDR.lvl.std, level=0.95)[5,2]
SRC.GP[6,2] = confint(GPMRDR.lvl.std, level=0.95)[6,1]
SRC.GP[6,3] = confint(GPMRDR.lvl.std, level=0.95)[6,2]
SRC.GP[7,2] = confint(GPMRDR.lvl.std, level=0.95)[7,1]
SRC.GP[7,3] = confint(GPMRDR.lvl.std, level=0.95)[7,2]


#########  PREDICTED graphs of relationships, others held at mean  ##############
predict(GPMRDR.lvl, list(SL=35, PR=225, RM=50, DR="0.5-0.75"), interval="conf")

####### STEP LENGTH ################
pred.sl.edgp.df = data.frame(SL = c(20,30,40,50), PR=225, RM=50, DR="0.5-0.75")
pred.sl.edgp = data.frame(predict(GPMRDR.lvl, pred.sl.df, interval = "conf"))
pred.sl.edgp$SL = c(20,30,40,50)

p13 = ggplot(pred.sl.edgp, aes(x=SL, y=fit)) + geom_line() + geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + ylim(-25,150) +
  ylab("Median Egg Density\n(Grass/Pasture)") + #xlab("Step Length (m)") + 
  theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16, margin=margin(0,10,0,5)),
        axis.text = element_text(size = 18))

##########  PERCEPTION DISTANCE ##############
pred.per.edgp.df = data.frame(SL = 35, PR=c(50,100,200,400), RM=50, DR="0.5-0.75")
pred.per.edgp = data.frame(predict(GPMRDR.lvl, pred.per.edgp.df, interval = "conf"))
pred.per.edgp$perc = c(50,100,200,400)

p14 = ggplot(pred.per.edgp, aes(x=perc, y=fit)) + geom_line() + geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) +
  ylab("Median Egg Density\n(Grass/Pasture)") + #xlab("Perception Distance (m)") + 
  ylim(-25,150) +
  theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16, margin=margin(0,10,0,5)),
        axis.text = element_text(size = 18))

########### REMEMBERED #######################
pred.rem.edgp.df = data.frame(SL = 35, PR=225, RM=c(0,10,40,100), DR="0.5-0.75")
pred.rem.edgp = data.frame(predict(GPMRDR.lvl, pred.rem.edgp.df, interval = "conf"))
pred.rem.edgp$rem = c(0,10,40,100)

p15 = ggplot(pred.rem.edgp, aes(x=rem, y=fit)) + geom_line() + geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + ylim(-25,150) +
  ylab("Median Egg Density\n(Grass/Pasture)") + xlab("Number of Polygons Remembered") + 
  theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16, margin=margin(0,10,0,5)),
        axis.text = element_text(size = 18))


######  DIRECTIONALITY ####################
pred.dir.edgp.df = data.frame(SL = 35, PR=225, RM=50, DR= c("0.5-0.75","0.1-0.2","0.1-0.9","0.8-0.9"))
pred.dir.edgp = data.frame(predict(GPMRDR.lvl, pred.dir.df, interval = "conf"))
pred.dir.edgp$dir = c("0.5-0.75","0.1-0.2","0.1-0.9","0.8-0.9")

p16 = ggplot(pred.dir.edgp, aes(x=dir, y=fit)) + geom_bar(stat="identity", fill = "#CCCCCC") + #ylim(-25,150) + 
  geom_errorbar(aes(ymin=lwr, ymax=upr), width=0.2) +
  ylab("Median Egg Density\n(Grass/Pasture)") + xlab("Directionality Scenarios") + 
  theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16, margin=margin(0,10,0,5)),
        axis.text = element_text(size = 18))


grid.arrange(p13,p14,p15,p16, ncol=2, top = textGrob("Median Egg Density in Grassland and Pasture", gp=gpar(fontsize=30)))



#############   Cumulative Eggs per habitat and map: Total eggs in study area##############

CumEggsperHab

#Cumulative Total eggs in the whole study area of Story County
CumEggsTot.lm = lm(CumEggsperHab$TotalCumEggs ~ SL + RM + PR + DR) 
summary(CumEggsTot.lm) ###  step length highly significant, also low min directionality
#Standardized total eggs
TotalCumEggs.std = scale(CumEggsperHab$TotalCumEggs)
#regression with standardized vars
CumEggsTot.lm.std = lm(TotalCumEggs.std ~ SL.std + RM.std + PR.std + DR) 
summary(CumEggsTot.lm.std) ###  step length highly significant, also low min directionality
confint(CumEggsTot.lm.std, level = 0.95)

#########  PREDICTED graphs of relationships, others held at mean  ##############
predict(CumEggsTot.lm, list(SL=35, PR=225, RM=50, DR="0.5-0.75"), interval="conf")

####### STEP LENGTH ################
pred.tot.sl.df = data.frame(SL = c(20,30,40,50), PR=225, RM=50, DR="0.5-0.75")
pred.tot.sl = data.frame(predict(CumEggsTot.lm, pred.tot.sl.df, interval = "conf"))
pred.tot.sl$SL = c(20,30,40,50)

p17 = ggplot(pred.tot.sl, aes(x=SL, y=fit)) + geom_line() + geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + #ylim(-25,150) +
  scale_y_continuous(limits = c(0,3500000), labels = scientific) +
  xlab("Step Length (m)") + ylab("Total Eggs Laid") +
  theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
        axis.title.x = element_text(size = 18, margin=margin(10,0,10,0)),
        axis.title.y = element_text(size = 16, margin=margin(0,10,0,5)),
        axis.text = element_text(size = 14))

##########  PERCEPTION DISTANCE ##############
pred.tot.per.df = data.frame(SL = 35, PR=c(50,100,200,400), RM=50, DR="0.5-0.75")
pred.tot.per = data.frame(predict(CumEggsTot.lm, pred.per.df, interval = "conf"))
pred.tot.per$perc = c(50,100,200,400)

p18 = ggplot(pred.tot.per, aes(x=perc, y=fit)) + geom_line() + geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) +
  xlab("Perceptual Range (m)") + ylab("Total Eggs Laid\n(Study Area)") +
  #ylim(1000000,3500000) +
  scale_y_continuous(limits = c(0,3500000), labels = scientific) +
  theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
        axis.title.x = element_text(size = 18, margin=margin(10,0,10,0)),
        axis.title.y = element_text(size = 16, margin=margin(0,10,0,5)),
        axis.text = element_text(size = 18))

########### REMEMBERED #######################
pred.tot.rem.df = data.frame(SL = 35, PR=225, RM=c(0,10,40,100), DR="0.5-0.75")
pred.tot.rem = data.frame(predict(CumEggsTot.lm, pred.rem.df, interval = "conf"))
pred.tot.rem$rem = c(0,10,40,100)

p19 = ggplot(pred.tot.rem, aes(x=rem, y=fit)) + geom_line() + geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
  #ylim(1000000,3500000) +
  scale_y_continuous(limits = c(0,3500000), labels = scientific) +
  xlab("Spatial Memory Parameter") + ylab("Total Eggs Laid\n(Study Area)") +
  theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
        axis.title.x = element_text(size = 18, margin=margin(10,0,10,0)),
        axis.title.y = element_text(size = 16, margin=margin(0,10,0,5)),
        axis.text = element_text(size = 18))

######  DIRECTIONALITY ####################
pred.tot.dir.df = data.frame(SL = 35, PR=225, RM=50, DR= c("0.5-0.75","0.1-0.2","0.1-0.9","0.8-0.9"))
pred.tot.dir = data.frame(predict(CumEggsTot.lm, pred.dir.df, interval = "conf"))
pred.tot.dir$dir = c("0.5-0.75","0.1-0.2","0.1-0.9","0.8-0.9")

p20 = ggplot(pred.tot.dir, aes(x=dir, y=fit)) + geom_bar(stat="identity", fill = "#CCCCCC") + 
  #ylim(0,3500000) + 
  scale_y_continuous(limits = c(0,3500000), labels = scientific) +
  geom_errorbar(aes(ymin=lwr, ymax=upr), width=0.2) +
  xlab("Directionality Scenarios") + ylab("Total Eggs Laid\n(Study Area)") +
  theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
        axis.title.x = element_text(size = 18, margin=margin(10,0,10,0)),
        axis.title.y = element_text(size = 16, margin=margin(0,10,0,5)),
        axis.text = element_text(size = 18))


grid.arrange(p17,p18,p19,p20, ncol=2, top = textGrob("Total Eggs Laid in the Study Area", gp=gpar(fontsize=30)))


############### Cumulative Eggs in GP polygons #############################

CumEggsGP.lm = lm(CumEggsperHab$GPCumEggs ~ SL + RM + PR + DR) 
summary(CumEggsGP.lm) ###  step length highly significant, also low min directionality
GPCumEggs.std = scale(CumEggsperHab$GPCumEggs)
CumEggsGP.lm.std = lm(GPCumEggs.std ~ SL.std + RM.std + PR.std + DR) 
summary(CumEggsGP.lm.std) ###  step length highly significant, also low min directionality
confint(CumEggsGP.lm.std, level = 0.95)

#########  PREDICTED graphs of relationships, others held at mean  ##############
predict(CumEggsGP.lm, list(SL=35, PR=225, RM=50, DR="0.5-0.75"), interval="conf")

####### STEP LENGTH ################
pred.gp.sl.df = data.frame(SL = c(20,30,40,50), PR=225, RM=50, DR="0.5-0.75")
pred.gp.sl = data.frame(predict(CumEggsGP.lm, pred.tot.sl.df, interval = "conf"))
pred.gp.sl$SL = c(20,30,40,50)

p21 = ggplot(pred.gp.sl, aes(x=SL, y=fit)) + geom_line() + geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
  scale_y_continuous(limits = c(0,1250000), labels = scientific) +
  ylab("Total Eggs Laid\n(Grass/Pasture)") + xlab("Step Length (m)") + 
  theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
        axis.title.x = element_text(size = 18, margin=margin(10,0,10,0)),
        axis.title.y = element_text(size = 16, margin=margin(0,10,0,5)),
        axis.text = element_text(size = 12))

##########  PERCEPTION DISTANCE ##############
pred.gp.per.df = data.frame(SL = 35, PR=c(50,100,200,400), RM=50, DR="0.5-0.75")
pred.gp.per = data.frame(predict(CumEggsGP.lm, pred.per.df, interval = "conf"))
pred.gp.per$perc = c(50,100,200,400)

p22 = ggplot(pred.gp.per, aes(x=perc, y=fit)) + geom_line() + geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) +
  xlab("Perceptual Range (m)") + ylab("Total Eggs Laid\n(Grass/Pasture)") +
  scale_y_continuous(limits = c(0,1250000), labels = scientific) +
  theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
        axis.title.x = element_text(size = 18, margin=margin(10,0,10,0)),
        axis.title.y = element_text(size = 16, margin=margin(0,10,0,5)),
        axis.text = element_text(size = 18))

########### REMEMBERED #######################
pred.gp.rem.df = data.frame(SL = 35, PR=225, RM=c(0,10,40,100), DR="0.5-0.75")
pred.gp.rem = data.frame(predict(CumEggsGP.lm, pred.rem.df, interval = "conf"))
pred.gp.rem$rem = c(0,10,40,100)

p23 = ggplot(pred.gp.rem, aes(x=rem, y=fit)) + geom_line() + geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
  scale_y_continuous(limits = c(0,1250000), labels = scientific) +
  xlab("Spatial Memory Parameter") + ylab("Total Eggs Laid\n(Grass/Pasture)") +
  theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
        axis.title.x = element_text(size = 18, margin=margin(10,0,10,0)),
        axis.title.y = element_text(size = 16, margin=margin(0,10,0,5)),
        axis.text = element_text(size = 18))

######  DIRECTIONALITY ####################
pred.gp.dir.df = data.frame(SL = 35, PR=225, RM=50, DR= c("0.5-0.75","0.1-0.2","0.1-0.9","0.8-0.9"))
pred.gp.dir = data.frame(predict(CumEggsGP.lm, pred.dir.df, interval = "conf"))
pred.gp.dir$dir = c("0.5-0.75","0.1-0.2","0.1-0.9","0.8-0.9")

p24 = ggplot(pred.gp.dir, aes(x=dir, y=fit)) + geom_bar(stat="identity", fill = "#CCCCCC") + 
  scale_y_continuous(limits = c(0,1250000), labels = scientific) +
  geom_errorbar(aes(ymin=lwr, ymax=upr), width=0.2) +
  xlab("Directionality Scenarios") + ylab("Total Eggs Laid\n(Grass/Pasture)") +
  theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
        axis.title.x = element_text(size = 18, margin=margin(10,0,10,0)),
        axis.title.y = element_text(size = 16, margin=margin(0,10,0,5)),
        axis.text = element_text(size = 18))


grid.arrange(p21,p22,p23,p24, ncol=2, top = textGrob("Total Eggs Laid in Grassland and Pasture Polygons", gp=gpar(fontsize=30)))


########## Cumulative Eggs in MWROW60+ Polygons ###################

CumEggsRW.lm = lm(CumEggsperHab$ROW60CumEggs ~ SL + RM + PR + DR) 
summary(CumEggsRW.lm)  ###  step length highly significant, also remembered
CumEggsRW.std = scale(CumEggsperHab$ROW60CumEggs)
CumEggsRW.lm.std = lm(CumEggsRW.std ~ SL.std + RM.std + PR.std + DR)
summary(CumEggsRW.lm.std)
confint(CumEggsRW.lm.std, level = 0.95)



#########  PREDICTED graphs of relationships, others held at mean  ##############
predict(CumEggsRW.lm, list(SL=35, PR=225, RM=50, DR="0.5-0.75"), interval="conf")

####### STEP LENGTH ################
pred.rw.sl.df = data.frame(SL = c(20,30,40,50), PR=225, RM=50, DR="0.5-0.75")
pred.rw.sl = data.frame(predict(CumEggsRW.lm, pred.tot.sl.df, interval = "conf"))
pred.rw.sl$SL = c(20,30,40,50)

p25 = ggplot(pred.rw.sl, aes(x=SL, y=fit)) + geom_line() + geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
  scale_y_continuous(limits = c(0,550000), labels = scientific) +
  ylab("Total Eggs Laid\n(Roadsides)") + #xlab("Step Length (m)") + 
  theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16, margin=margin(0,10,0,5)),
        axis.text = element_text(size = 12))

##########  PERCEPTION DISTANCE ##############
pred.rw.per.df = data.frame(SL = 35, PR=c(50,100,200,400), RM=50, DR="0.5-0.75")
pred.rw.per = data.frame(predict(CumEggsRW.lm, pred.per.df, interval = "conf"))
pred.rw.per$perc = c(50,100,200,400)

p26 = ggplot(pred.rw.per, aes(x=perc, y=fit)) + geom_line() + geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) +
  xlab("Perceptual Range (m)") + ylab("Total Eggs Laid\n(Roadsides)") +
  scale_y_continuous(limits = c(0,550000), labels = scientific) +
  theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
        #axis.title.x = element_text(size = 18, margin=margin(10,0,10,0)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16, margin=margin(0,10,0,5)),
        axis.text = element_text(size = 18))

########### REMEMBERED #######################
pred.rw.rem.df = data.frame(SL = 35, PR=225, RM=c(0,10,40,100), DR="0.5-0.75")
pred.rw.rem = data.frame(predict(CumEggsRW.lm, pred.rem.df, interval = "conf"))
pred.rw.rem$rem = c(0,10,40,100)

p27 = ggplot(pred.rw.rem, aes(x=rem, y=fit)) + geom_line() + geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
  scale_y_continuous(limits = c(0,550000), labels = scientific) +
  xlab("Spatial Memory Parameter") + ylab("Total Eggs Laid\n(Roadsides)") +
  theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
        #axis.title.x = element_text(size = 18, margin=margin(10,0,10,0)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16, margin=margin(0,10,0,5)),
        axis.text = element_text(size = 18))

######  DIRECTIONALITY ####################
pred.rw.dir.df = data.frame(SL = 35, PR=225, RM=50, DR= c("0.5-0.75","0.1-0.2","0.1-0.9","0.8-0.9"))
pred.rw.dir = data.frame(predict(CumEggsRW.lm, pred.dir.df, interval = "conf"))
pred.rw.dir$dir = c("0.5-0.75","0.1-0.2","0.1-0.9","0.8-0.9")

p28 = ggplot(pred.rw.dir, aes(x=dir, y=fit)) + geom_bar(stat="identity", fill = "#CCCCCC") + 
  scale_y_continuous(limits = c(0,550000), labels = scientific) +
  geom_errorbar(aes(ymin=lwr, ymax=upr), width=0.2) +
  xlab("Directionality Scenarios") + ylab("Total Eggs Laid\n(Roadsides)") +
  theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
        #axis.title.x = element_text(size = 18, margin=margin(10,0,10,0)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16, margin=margin(0,10,0,5)),
        axis.text = element_text(size = 18))


grid.arrange(p25,p26,p27,p28, ncol=2, top = textGrob("Total Eggs Laid in High Density Milkweed Right-of-Way Polygons", gp=gpar(fontsize=30)))








##########################################  Graphs by Parameter Instead of Response Variable ##################################


### THESE GRAPHS ARE IN THE PAPER 

############ Perception Distance  #################################


#all
grid.arrange(p2.ha,p6,p10,p14,p18,p22,p26, top = textGrob("Perception Distance", gp=gpar(fontsize=30)))

#UDs, prop eggs laid, median egg density in roadsides, median egg density in GP
grid.arrange(p2.ha,p6,p10,p14, ncol=2, top = textGrob("Perception Distance", gp=gpar(fontsize=30)))

#UDs, prop eggs laid, median egg density in roadsides, median egg density in GP, total eggs in roadsides, total eggs in GP
grid.arrange(p2.ha,p6,p10,p14,p26,p22,p18, ncol=2, top = textGrob("Model Responses to Variation in Perceptual Range", gp=gpar(fontsize=26)))



############ Remembered   #######################################

#UDs, prop eggs laid, median egg density in roadsides, median egg density in GP, total eggs in roadsides, total eggs in GP
grid.arrange(p3.ha,p7,p11,p15,p27,p23,p19, ncol=2, top = textGrob("Model Responses to Variation in Spatial Memory", gp=gpar(fontsize=26)))




###########  Directionality  ####################################

grid.arrange(p4.ha,p8,p12,p16,p28,p24,p20, ncol=2, top = textGrob("Model Responses to Variation in Directionality", gp=gpar(fontsize=26)))



##########  Step Length  ###################################

grid.arrange(p1.ha,p5,p9,p13,p25,p21,p17, ncol=2, top = textGrob("Model Responses to Variation in Step Length", gp=gpar(fontsize=26)))


########  qqPlots

qqPlot(UDMRDR.lvldir.std.ha) 
qqPlot(PEMRDR.lvl)
qqPlot(RWMRDR.lvl.std)
qqPlot(GPMRDR.lvl.std)
qqPlot(CumEggsGP.lm.std)
qqPlot(CumEggsRW.lm.std)
qqPlot(CumEggsTot.lm.std)


########  Sensitivity Index as SSi/SST  ######################################################

######## Utilization Distribution  ###############

UDMRDR.lvldir.std.ha = lm(UDmeans.std.ha ~ SL.std + PR.std + RM.std + DR)

UDMRDR.lvldir.std.ha.sl = lm(UDmeans.std.ha ~ SL.std)
anova(UDMRDR.lvldir.std.ha.sl)
sum(anova(UDMRDR.lvldir.std.ha.sl)[,2])

UDMRDR.lvldir.std.ha.pr = lm(UDmeans.std.ha ~ PR.std)
anova(UDMRDR.lvldir.std.ha.pr)
sum(anova(UDMRDR.lvldir.std.ha.pr)[,2])

UDMRDR.lvldir.std.ha.rm = lm(UDmeans.std.ha ~ RM.std)
anova(UDMRDR.lvldir.std.ha.rm)

UDMRDR.lvldir.std.ha.dr = lm(UDmeans.std.ha ~ DR)
anova(UDMRDR.lvldir.std.ha.dr)

######## Proportion of Eggs Laid ##################

PEMRDR.lvl.std = lm(EggsLaidmeans.std ~ SL.std + RM.std + PR.std + DR)
anova(PEMRDR.lvl.std)
sum(anova(PEMRDR.lvl.std)[,2])

PEMRDR.lvl.std.sl = lm(EggsLaidmeans.std ~ SL.std)
anova(PEMRDR.lvl.std.sl)

PEMRDR.lvl.std.rm = lm(EggsLaidmeans.std ~ RM.std)
anova(PEMRDR.lvl.std.rm)

PEMRDR.lvl.std.pr = lm(EggsLaidmeans.std ~ PR.std)
anova(PEMRDR.lvl.std.pr)

PEMRDR.lvl.std.dr = lm(EggsLaidmeans.std ~ DR)
anova(PEMRDR.lvl.std.dr)

####### Egg Density in MWROW60+ ########################

RWMRDR.lvl.std = lm(EggDens60medians.std ~ SL.std + RM.std + PR.std + DR) 
anova(RWMRDR.lvl.std)
sum(anova(RWMRDR.lvl.std)[,2])

RWMRDR.lvl.std.sl = lm(EggDens60medians.std ~ SL.std) 
anova(RWMRDR.lvl.std.sl)

RWMRDR.lvl.std.rm = lm(EggDens60medians.std ~ RM.std) 
anova(RWMRDR.lvl.std.rm)

RWMRDR.lvl.std.pr = lm(EggDens60medians.std ~ PR.std) 
anova(RWMRDR.lvl.std.pr)

RWMRDR.lvl.std.dr = lm(EggDens60medians.std ~ DR) 
anova(RWMRDR.lvl.std.dr)

###### Egg Density in G/P  ###############################

GPMRDR.lvl.std = lm(EggDensGPmedians.std ~ SL.std + RM.std + PR.std + DR) 
anova(GPMRDR.lvl.std)
sum(anova(GPMRDR.lvl.std)[,2])

GPMRDR.lvl.std.sl = lm(EggDensGPmedians.std ~ SL.std) 
anova(GPMRDR.lvl.std.sl)

GPMRDR.lvl.std.rm = lm(EggDensGPmedians.std ~ RM.std) 
anova(GPMRDR.lvl.std.rm)

GPMRDR.lvl.std.pr = lm(EggDensGPmedians.std ~ PR.std) 
anova(GPMRDR.lvl.std.pr)

GPMRDR.lvl.std.dr = lm(EggDensGPmedians.std ~ DR) 
anova(GPMRDR.lvl.std.dr)

#######  Total Eggs in the Study Area  ###################

CumEggsTot.lm.std = lm(TotalCumEggs.std ~ SL.std + RM.std + PR.std + DR) 
anova(CumEggsTot.lm.std)
sum(anova(CumEggsTot.lm.std)[,2])

CumEggsTot.lm.std.sl = lm(TotalCumEggs.std ~ SL.std) 
anova(CumEggsTot.lm.std.sl)

CumEggsTot.lm.std.rm = lm(TotalCumEggs.std ~ RM.std) 
anova(CumEggsTot.lm.std.rm)

CumEggsTot.lm.std.pr = lm(TotalCumEggs.std ~ PR.std) 
anova(CumEggsTot.lm.std.pr)

CumEggsTot.lm.std.dr= lm(TotalCumEggs.std ~ DR) 
anova(CumEggsTot.lm.std.dr)

#######  Total Eggs in G/P ##############################

CumEggsGP.lm.std = lm(GPCumEggs.std ~ SL.std + RM.std + PR.std + DR) 
anova(CumEggsGP.lm.std)
sum(anova(CumEggsGP.lm.std)[,2])

CumEggsGP.lm.std.sl = lm(GPCumEggs.std ~ SL.std) 
anova(CumEggsGP.lm.std.sl)

CumEggsGP.lm.std.rm = lm(GPCumEggs.std ~ RM.std) 
anova(CumEggsGP.lm.std.rm)

CumEggsGP.lm.std.pr = lm(GPCumEggs.std ~ PR.std) 
anova(CumEggsGP.lm.std.pr)

CumEggsGP.lm.std.dr= lm(GPCumEggs.std ~ DR) 
anova(CumEggsGP.lm.std.dr)

#######  Total Eggs in MWROW60+  ########################

CumEggsRW.lm.std = lm(CumEggsRW.std ~ SL.std + RM.std + PR.std + DR)
anova(CumEggsRW.lm.std)
sum(anova(CumEggsRW.lm.std)[,2])

CumEggsRW.lm.std.sl = lm(CumEggsRW.std ~ SL.std)
anova(CumEggsRW.lm.std.sl)

CumEggsRW.lm.std.rm = lm(CumEggsRW.std ~ RM.std)
anova(CumEggsRW.lm.std.rm)

CumEggsRW.lm.std.pr = lm(CumEggsRW.std ~ PR.std)
anova(CumEggsRW.lm.std.pr)

CumEggsRW.lm.std.dr= lm(CumEggsRW.std ~ DR)
anova(CumEggsRW.lm.std.dr)





#################  Model Selection  ####################################

library(MASS)

############# Utilization Distribution ###############

#development

UDMRDR.HA = lm(UDmeansHA ~ SL + PR + RM + DR)
summary(UDMRDR.HA)
UDMRDR.HA.DR = lm(UDmeansHA ~ DR + SL + PR + RM)
summary(UDMRDR.HA.DR)

step = stepAIC(UDMRDR.HA)
step$anova
step.DR = stepAIC(UDMRDR.HA.DR)

AIC(UDMRDR.HA)
AIC(UDMRDR.HA.DR)

lm.UD.SL = lm(UDmeansHA ~ SL)
lm.UD.DR = lm(UDmeansHA ~ DR)

AIC(lm.UD.SL)
AIC(lm.UD.DR)


##### UD ################

UDMRDR.HA.1 = lm(UDmeansHA ~ 1)
step.1 = stepAIC(UDMRDR.HA.1, scope = list(upper = ~ SL + PR + RM + DR, lower = ~1))
step.1$anova
coefficients(step.1)
confint(step.1)

#### Mean Prop Eggs Laid #################

PEMRDR.lvl = lm(EggsLaidmeans ~ SL + PR + RM + DR) 
PEMRDR.lvl.1 = lm(EggsLaidmeans ~ 1)
step.2 = stepAIC(PEMRDR.lvl.1, scope = list(upper = ~ SL + PR + RM + DR, lower = ~1))
step.2$anova
#step AIC didn't include last 2 vars, so I added them in order of best AIC
PEMRDR.lvl.1.2 = lm(EggsLaidmeans ~ SL + DR + RM + PR)
summary(PEMRDR.lvl.1.2)
coefficients(PEMRDR.lvl.1.2)
confint(PEMRDR.lvl.1.2)

########  Egg Density in MWROW60+  ##################

RWMRDR.lvl = lm(EggDens60medians ~ SL + PR + RM + DR) 
RWMRDR.lvl.1 = lm(EggDens60medians ~ 1) 
step.3 = stepAIC(RWMRDR.lvl.1, scope = list(upper = ~ SL + PR + RM + DR, lower = ~1))
step.3$anova
summary(step.3)
coefficients(step.3)
confint(step.3)

#########  Egg Density in G/P #####################

GPMRDR.lvl = lm(EggDensGPmedians ~ SL + PR + RM + DR) 
GPMRDR.lvl.1 = lm(EggDensGPmedians ~ 1) 
step.4 = stepAIC(GPMRDR.lvl.1, scope = list(upper = ~ SL + PR + RM + DR, lower = ~1))
#step AIC didn't include last 2 vars, so I added them in order of best AIC
GPMRDR.lvl.1.2 = lm(EggDensGPmedians ~ PR + SL + DR + RM) 
summary(GPMRDR.lvl.1.2)
coefficients(GPMRDR.lvl.1.2)
confint(GPMRDR.lvl.1.2)

######### Total eggs in study area ####################

CumEggsTot.lm = lm(CumEggsperHab$TotalCumEggs[1:25] ~ SL + RM + PR + DR) 
CumEggsTot.lm.1 = lm(CumEggsperHab$TotalCumEggs[1:25] ~ 1) 
step.5 = stepAIC(CumEggsTot.lm.1, scope = list(upper = ~ SL + PR + RM + DR, lower = ~1))
step.5$anova
summary(step.5)
coefficients(step.5)
confint(step.5)

########  Total eggs in grass/pasture ####################

CumEggsGP.lm = lm(CumEggsperHab$GPCumEggs[1:25] ~ SL + RM + PR + DR) 
CumEggsGP.lm.1 = lm(CumEggsperHab$GPCumEggs[1:25] ~ 1) 
step.6 = stepAIC(CumEggsGP.lm.1, scope = list(upper = ~ SL + PR + RM + DR, lower = ~1))
#step AIC didn't include last 2 vars, so I added them in order of best AIC
CumEggsGP.lm.1.2 = lm(CumEggsperHab$GPCumEggs[1:25] ~ SL + DR + PR + RM) 
summary(CumEggsGP.lm.1.2)
coefficients(CumEggsGP.lm.1.2)
confint(CumEggsGP.lm.1.2)

######### Total eggs in MWROW60+  #######################

CumEggsRW.lm = lm(CumEggsperHab$ROW60CumEggs[1:25] ~ SL + RM + PR + DR) 
CumEggsRW.lm.1 = lm(CumEggsperHab$ROW60CumEggs[1:25] ~ 1) 
step.7 = stepAIC(CumEggsRW.lm.1, scope = list(upper = ~ SL + PR + RM + DR, lower = ~1))
#step AIC didn't include last 2 vars, so I added them in order of best AIC
CumEggsRW.lm.1.2 = lm(CumEggsperHab$ROW60CumEggs[1:25] ~ SL + RM + DR + PR) 
summary(CumEggsRW.lm.1.2)
coefficients(CumEggsRW.lm.1.2)
confint(CumEggsRW.lm.1.2)















