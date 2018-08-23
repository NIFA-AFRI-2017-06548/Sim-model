############  Check directionality simulations  ######################


library(dplyr)
library(ggplot2)
library(move)


run138 = read.csv("Monarchs.2016.May.13.13_16_40.txt") #lsa dir 0.8-0.9
run139 = read.csv("Monarchs.2016.May.13.13_32_43.txt") #lsa dir 0.1-0.9
run140 = read.csv("Monarchs.2016.May.13.14_02_37.txt") #lsa dir 0.1-0.2

#change to current data to summarize
d = run140
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
levels(dirSA.ch$Directionality) = c("0.1-0.2","0.1-0.9","0.5-0.75","0.8-0.9")


#run 140 - directionality = 0.1-0.2
dirSA.ch[1:1000,1] = "0.1-0.2"
dirSA.ch[1:1000,2] = st5$EggsLaid 
dirSA.ch[1:1000,3] = st5$EggsLaid[1:1000]/42 #42 possible eggs to lay on Day 5
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


pch = ggplot(dirSA.ch, aes(x=Directionality, y=PropEggsLaid)) + geom_boxplot() + ggtitle("Proportion of Eggs Laid - Day 5")
+ xlab("Directionality") + ylab("Proportion of Eggs Laid") +
  stat_summary(fun.y="mean", geom="point", shape=3, size=3)
pch + theme(plot.title = element_text(size=30, face="bold", margin=margin(10,0,20,0)), 
            axis.title.x = element_text(size = 26, margin=margin(10,0,10,0)),
            axis.title.y = element_text(size = 26, margin=margin(0,10,0,5)),
            axis.text = element_text(size = 20))


