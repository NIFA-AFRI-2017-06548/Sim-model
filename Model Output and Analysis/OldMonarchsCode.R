

##################  Old Test and Development Code  #################


#################   Cumulative Eggs per Zone - before changed output file  ###################

EZresults = data.frame(matrix(nrow=14, ncol=8))
colnames(EZresults) = c("MeanCumEggs", "SDCumEggs","MedianCumEggs","MeanProportionofCumEggs","SDPropCumEggs",
                        "No. Sim Polygons","No. Map Polygons","Run")
rownames(EZresults) = c("MWROW60-100","MWROW20-60","MWROW5-20","MWROW1-5","Grass/Pasure","Corn_nonGMO",
                        "Corn","Soybeans_nonGMO","Soybeans","Developed/Open Space/Developed/Low Intensity","Other",
                        "Forest","MWROW0","High Intensity")



run11EZ = read.csv("EggsPerZone.2016.Jan.08.12_50_21.txt")  #baseline/step length 50/directionality 0.75/remembered 10
run12EZ = read.csv("EggsPerZone.2016.Jan.08.14_08_23.txt")  #step length 10
run13EZ = read.csv("EggsPerZone.2016.Jan.11.09_12_37.txt")  #step length 40
run14EZ = read.csv("EggsPerZone.2016.Jan.11.09_38_27.txt")  #step length 30
run10EZ = read.csv("EggsPerZone.2016.Jan.08.12_40_19.txt")  #step length 25
run22EZ = read.csv("CumEggsPerZone.2016.Jan.14.16_05_46.txt")  #step length 18
run2EZ = read.csv("EggsPerZone.2016.Jan.07.10_00_45.txt")  #directionality 0.5
run3EZ = read.csv("EggsPerZone.2016.Jan.07.10_11_26.txt")  #directionality 0.25
run4EZ = read.csv("EggsPerZone.2016.Jan.07.10_21_19.txt")  #directionality 0.1
run5EZ = read.csv("EggsPerZone.2016.Jan.07.10_30_26.txt")  #directionality 0.9
run6EZ = read.csv("EggsPerZone.2016.Jan.07.13_32_22.txt")  #perception 25
run7EZ = read.csv("EggsPerZone.2016.Jan.07.13_50_12.txt")  #perception 200
run8EZ = read.csv("EggsPerZone.2016.Jan.07.14_31_06.txt")  #perception 400
run9EZ = read.csv("EggsPerZone.2016.Jan.08.06_32_38.txt")  #perception 50
run15EZ = read.csv("EggsPerZone.2016.Jan.11.11_12_11.txt")  #remembered 20
run16EZ = read.csv("EggsPerZone.2016.Jan.11.11_21_00.txt")  #remembered 5
run17EZ = read.csv("EggsPerZone.2016.Jan.11.12_35_08.txt")  #remembered 2
run18EZ = read.csv("EggsPerZone.2016.Jan.11.12_42_15.txt")  #remembered 40
run19EZ = read.csv("EggsPerZone.2016.Jan.11.15_31_21.txt")  #remembered 1


#current dataset for analysis
dez = run19EZ
nrow(dez)

#Final data from after 10th tick
EZ10 = filter(dez, tick == 10)
#20 instances for 724 polygons = 14,480
nrow(EZ10)
#total eggs across all zones and instances
GrandCumEggs = sum(EZ10$Eggs)
#total eggs for each instance
Int1CumEggs = sum(EZ10$Eggs[1:724])
#calc proportion of eggs in new column in EZ10 x 100,000 for easier reading
EZ10$prop = 100000*(EZ10$Eggs/GrandCumEggs)


#MWROW60-100 - this includes 20 instances - 20 maps, each with 50 Monarchs, not aggregated
EZR6 = filter(EZ10, Name == "MWROW60-100")
hist(EZR6$Eggs)
hist(EZR6$prop)
EZresults[1,1] = mean(EZR6$Eggs)
EZresults[1,2] = pop.sd(EZR6$Eggs)
EZresults[1,3] = median(EZR6$Eggs)
EZresults[1,4] = mean(EZR6$prop)
EZresults[1,5] = pop.sd(EZR6$prop)
EZresults[1,6] = nrow(EZR6)
EZresults[1,7] = nrow(EZR6)/20

EZR2 = filter(EZ10, Name == "MWROW20-60")
hist(EZR2$Eggs)
EZresults[2,1] = mean(EZR2$Eggs)
EZresults[2,2] = pop.sd(EZR2$Eggs)
EZresults[2,3] = median(EZR2$Eggs)
EZresults[2,4] = mean(EZR2$prop)
EZresults[2,5] = pop.sd(EZR2$prop)
EZresults[2,6] = nrow(EZR2)
EZresults[2,7] = nrow(EZR2)/20

EZR5 = filter(EZ10, Name == "MWROW5-20")
hist(EZR5$Eggs)
EZresults[3,1] = mean(EZR5$Eggs)
EZresults[3,2] = pop.sd(EZR5$Eggs)
EZresults[3,3] = median(EZR5$Eggs)
EZresults[3,4] = mean(EZR5$prop)
EZresults[3,5] = pop.sd(EZR5$prop)
EZresults[3,6] = nrow(EZR5)
EZresults[3,7] = nrow(EZR5)/20

EZR1 = filter(EZ10, Name == "MWROW1-5")
hist(EZR1$Eggs)
EZresults[4,1] = mean(EZR1$Eggs)
EZresults[4,2] = pop.sd(EZR1$Eggs)
EZresults[4,3] = median(EZR1$Eggs)
EZresults[4,4] = mean(EZR1$prop)
EZresults[4,5] = pop.sd(EZR1$prop)
EZresults[4,6] = nrow(EZR1)
EZresults[4,7] = nrow(EZR1)/20

EZGP = filter(EZ10, Name == "Grass/Pasture")
hist(EZGP$Eggs)
EZresults[5,1] = mean(EZGP$Eggs)
EZresults[5,2] = pop.sd(EZGP$Eggs)
EZresults[5,3] = median(EZGP$Eggs)
EZresults[5,4] = mean(EZGP$prop)
EZresults[5,5] = pop.sd(EZGP$prop)
EZresults[5,6] = nrow(EZGP)
EZresults[5,7] = nrow(EZGP)/20

EZCN = filter(EZ10, Name == "Corn_nonGMO")
hist(EZCN$Eggs)
EZresults[6,1] = mean(EZCN$Eggs)
EZresults[6,2] = pop.sd(EZCN$Eggs)
EZresults[6,3] = median(EZCN$Eggs)
EZresults[6,4] = mean(EZCN$prop)
EZresults[6,5] = pop.sd(EZCN$prop)
EZresults[6,6] = nrow(EZCN)
EZresults[6,7] = nrow(EZCN)/20

EZC = filter(EZ10, Name == "Corn")
hist(EZC$Eggs)
EZresults[7,1] = mean(EZC$Eggs)
EZresults[7,2] = pop.sd(EZC$Eggs)
EZresults[7,3] = median(EZC$Eggs)
EZresults[7,4] = mean(EZC$prop)
EZresults[7,5] = pop.sd(EZC$prop)
EZresults[7,6] = nrow(EZC)
EZresults[7,7] = nrow(EZC)/20

EZSN = filter(EZ10, Name == "Soybeans_nonGMO")
hist(EZSN$Eggs)
EZresults[8,1] = mean(EZSN$Eggs)
EZresults[8,2] = pop.sd(EZSN$Eggs)
EZresults[8,3] = median(EZSN$Eggs)
EZresults[8,4] = mean(EZSN$prop)
EZresults[8,5] = pop.sd(EZSN$prop)
EZresults[8,6] = nrow(EZSN)
EZresults[8,7] = nrow(EZSN)/20

EZS = filter(EZ10, Name == "Soybeans")
hist(EZS$Eggs)
EZresults[9,1] = mean(EZS$Eggs)
EZresults[9,2] = pop.sd(EZS$Eggs)
EZresults[9,3] = median(EZS$Eggs)
EZresults[9,4] = mean(EZSN$prop)
EZresults[9,5] = pop.sd(EZSN$prop)
EZresults[9,6] = nrow(EZS)
EZresults[9,7] = nrow(EZS)/20

EZDL = filter(EZ10, Name == "Developed/Open Space/Developed/Low Intensity")
hist(EZDL$Eggs)
EZresults[10,1] = mean(EZDL$Eggs)
EZresults[10,2] = pop.sd(EZDL$Eggs)
EZresults[10,3] = median(EZDL$Eggs)
EZresults[10,4] = mean(EZDL$prop)
EZresults[10,5] = pop.sd(EZDL$prop)
EZresults[10,6] = nrow(EZDL)
EZresults[10,7] = nrow(EZDL)/20

EZOT = subset(EZ10, grepl("Cotton", EZ10$Name))
hist(EZOT$Eggs)
EZresults[11,1] = mean(EZOT$Eggs)
EZresults[11,2] = pop.sd(EZOT$Eggs)
EZresults[11,3] = median(EZOT$Eggs)
EZresults[11,4] = mean(EZOT$prop)
EZresults[11,5] = pop.sd(EZOT$prop)
EZresults[11,6] = nrow(EZOT)
EZresults[11,7] = nrow(EZOT)/20

EZF = subset(EZ10, grepl("Forest", EZ10$Name))
hist(EZF$Eggs)
EZresults[12,1] = mean(EZF$Eggs)
EZresults[12,2] = pop.sd(EZF$Eggs)
EZresults[12,3] = median(EZF$Eggs)
EZresults[12,4] = mean(EZF$prop)
EZresults[12,5] = pop.sd(EZF$prop)
EZresults[12,6] = nrow(EZF)
EZresults[12,7] = nrow(EZF)/20

EZR0 = filter(EZ10, Name == "MWROW0")
hist(EZR0$Eggs)
EZresults[13,1] = mean(EZR0$Eggs)
EZresults[13,2] = pop.sd(EZR0$Eggs)
EZresults[13,3] = median(EZR0$Eggs)
EZresults[13,4] = mean(EZR0$prop)
EZresults[13,5] = pop.sd(EZR0$prop)
EZresults[13,6] = nrow(EZR0)
EZresults[13,7] = nrow(EZR0)/20

EZDH = filter(EZ10, Name == "Blank_/Developed/Med Intensity/Developed/High Intensity")
hist(EZDH$Eggs)
EZresults[14,1] = mean(EZDH$Eggs)
EZresults[14,2] = pop.sd(EZDH$Eggs)
EZresults[14,3] = median(EZDH$Eggs)
EZresults[14,4] = mean(EZDH$prop)
EZresults[14,5] = pop.sd(EZDH$prop)
EZresults[14,6] = nrow(EZDH)
EZresults[14,7] = nrow(EZDH)/20




EZresults$Run[1:14] = "Run19"
#total polygons - should be 724 for sensitivity analyses
sum(EZresults[7])

# save results for each run
EZresultsRun11 = EZresults
write.csv(EZresults,"EZrun11.csv")
EZresultsRun12 = EZresults
write.csv(EZresults,"EZrun12.csv")
EZresultsRun13 = EZresults
write.csv(EZresults,"EZrun13.csv")
EZresultsRun14 = EZresults
write.csv(EZresults,"EZrun14.csv")
EZresultsRun10 = EZresults
write.csv(EZresults,"EZrun10.csv")
EZresultsRun2 = EZresults
write.csv(EZresults,"EZrun2.csv")
EZresultsRun3 = EZresults
write.csv(EZresults,"EZrun3.csv")
EZresultsRun4 = EZresults
write.csv(EZresults,"EZrun4.csv")
EZresultsRun5 = EZresults
write.csv(EZresults,"EZrun5.csv")
EZresultsRun6 = EZresults
write.csv(EZresults,"EZrun6.csv")
EZresultsRun7 = EZresults
write.csv(EZresults,"EZrun7.csv")
EZresultsRun8 = EZresults
write.csv(EZresults,"EZrun8.csv")
EZresultsRun9 = EZresults
write.csv(EZresults,"EZrun9.csv")
EZresultsRun15 = EZresults
write.csv(EZresults,"EZrun15.csv")
EZresultsRun16 = EZresults
write.csv(EZresults,"EZrun16.csv")
EZresultsRun17 = EZresults
write.csv(EZresults,"EZrun17.csv")
EZresultsRun18 = EZresults
write.csv(EZresults,"EZrun18.csv")
EZresultsRun19 = EZresults
write.csv(EZresults,"EZrun19.csv")





######### Test and development code for new output format  ################

#current dataset for analysis
dez = run22EZ
nrow(dez)

#Final data from after 10th tick
EZ10 = filter(dez, tick == 10)
#20 instances for 724 polygons = 14,480
nrow(EZ10)
#total eggs across all zones and instances
GrandCumEggs = sum(EZ10$Eggs)
#total eggs for each instance
Int1CumEggs = sum(EZ10$Eggs[1:724])
#calc proportion of eggs in new column in EZ10 x 100,000 for easier reading
EZ10$prop = 100000*(EZ10$Eggs/GrandCumEggs)


#MWROW60-100 - this includes 20 instances - 20 maps, each with 50 Monarchs, not aggregated
EZR6 = filter(EZ10, Name == "MWROW60-100")
hist(EZR6$Eggs)
hist(EZR6$prop)
EZresults[1,1] = mean(EZR6$Eggs)
EZresults[1,2] = pop.sd(EZR6$Eggs)
EZresults[1,3] = median(EZR6$Eggs)
EZresults[1,4] = mean(EZR6$prop)
EZresults[1,5] = pop.sd(EZR6$prop)
EZresults[1,6] = nrow(EZR6)
EZresults[1,7] = nrow(EZR6)/20

EZR2 = filter(EZ10, Name == "MWROW20-60")
hist(EZR2$Eggs)
EZresults[2,1] = mean(EZR2$Eggs)
EZresults[2,2] = pop.sd(EZR2$Eggs)
EZresults[2,3] = median(EZR2$Eggs)
EZresults[2,4] = mean(EZR2$prop)
EZresults[2,5] = pop.sd(EZR2$prop)
EZresults[2,6] = nrow(EZR2)
EZresults[2,7] = nrow(EZR2)/20

EZR5 = filter(EZ10, Name == "MWROW5-20")
hist(EZR5$Eggs)
EZresults[3,1] = mean(EZR5$Eggs)
EZresults[3,2] = pop.sd(EZR5$Eggs)
EZresults[3,3] = median(EZR5$Eggs)
EZresults[3,4] = mean(EZR5$prop)
EZresults[3,5] = pop.sd(EZR5$prop)
EZresults[3,6] = nrow(EZR5)
EZresults[3,7] = nrow(EZR5)/20

EZR1 = filter(EZ10, Name == "MWROW1-5")
hist(EZR1$Eggs)
EZresults[4,1] = mean(EZR1$Eggs)
EZresults[4,2] = pop.sd(EZR1$Eggs)
EZresults[4,3] = median(EZR1$Eggs)
EZresults[4,4] = mean(EZR1$prop)
EZresults[4,5] = pop.sd(EZR1$prop)
EZresults[4,6] = nrow(EZR1)
EZresults[4,7] = nrow(EZR1)/20

EZGP = filter(EZ10, Name == "Grass/Pasture")
hist(EZGP$Eggs)
EZresults[5,1] = mean(EZGP$Eggs)
EZresults[5,2] = pop.sd(EZGP$Eggs)
EZresults[5,3] = median(EZGP$Eggs)
EZresults[5,4] = mean(EZGP$prop)
EZresults[5,5] = pop.sd(EZGP$prop)
EZresults[5,6] = nrow(EZGP)
EZresults[5,7] = nrow(EZGP)/20

EZCN = filter(EZ10, Name == "Corn_nonGMO")
hist(EZCN$Eggs)
EZresults[6,1] = mean(EZCN$Eggs)
EZresults[6,2] = pop.sd(EZCN$Eggs)
EZresults[6,3] = median(EZCN$Eggs)
EZresults[6,4] = mean(EZCN$prop)
EZresults[6,5] = pop.sd(EZCN$prop)
EZresults[6,6] = nrow(EZCN)
EZresults[6,7] = nrow(EZCN)/20

EZC = filter(EZ10, Name == "Corn")
hist(EZC$Eggs)
EZresults[7,1] = mean(EZC$Eggs)
EZresults[7,2] = pop.sd(EZC$Eggs)
EZresults[7,3] = median(EZC$Eggs)
EZresults[7,4] = mean(EZC$prop)
EZresults[7,5] = pop.sd(EZC$prop)
EZresults[7,6] = nrow(EZC)
EZresults[7,7] = nrow(EZC)/20

EZSN = filter(EZ10, Name == "Soybeans_nonGMO")
hist(EZSN$Eggs)
EZresults[8,1] = mean(EZSN$Eggs)
EZresults[8,2] = pop.sd(EZSN$Eggs)
EZresults[8,3] = median(EZSN$Eggs)
EZresults[8,4] = mean(EZSN$prop)
EZresults[8,5] = pop.sd(EZSN$prop)
EZresults[8,6] = nrow(EZSN)
EZresults[8,7] = nrow(EZSN)/20

EZS = filter(EZ10, Name == "Soybeans")
hist(EZS$Eggs)
EZresults[9,1] = mean(EZS$Eggs)
EZresults[9,2] = pop.sd(EZS$Eggs)
EZresults[9,3] = median(EZS$Eggs)
EZresults[9,4] = mean(EZSN$prop)
EZresults[9,5] = pop.sd(EZSN$prop)
EZresults[9,6] = nrow(EZS)
EZresults[9,7] = nrow(EZS)/20

EZDL = filter(EZ10, Name == "Developed/Open Space/Developed/Low Intensity")
hist(EZDL$Eggs)
EZresults[10,1] = mean(EZDL$Eggs)
EZresults[10,2] = pop.sd(EZDL$Eggs)
EZresults[10,3] = median(EZDL$Eggs)
EZresults[10,4] = mean(EZDL$prop)
EZresults[10,5] = pop.sd(EZDL$prop)
EZresults[10,6] = nrow(EZDL)
EZresults[10,7] = nrow(EZDL)/20

EZOT = subset(EZ10, grepl("Cotton", EZ10$Name))
hist(EZOT$Eggs)
EZresults[11,1] = mean(EZOT$Eggs)
EZresults[11,2] = pop.sd(EZOT$Eggs)
EZresults[11,3] = median(EZOT$Eggs)
EZresults[11,4] = mean(EZOT$prop)
EZresults[11,5] = pop.sd(EZOT$prop)
EZresults[11,6] = nrow(EZOT)
EZresults[11,7] = nrow(EZOT)/20

EZF = subset(EZ10, grepl("Forest", EZ10$Name))
hist(EZF$Eggs)
EZresults[12,1] = mean(EZF$Eggs)
EZresults[12,2] = pop.sd(EZF$Eggs)
EZresults[12,3] = median(EZF$Eggs)
EZresults[12,4] = mean(EZF$prop)
EZresults[12,5] = pop.sd(EZF$prop)
EZresults[12,6] = nrow(EZF)
EZresults[12,7] = nrow(EZF)/20

EZR0 = filter(EZ10, Name == "MWROW0")
hist(EZR0$Eggs)
EZresults[13,1] = mean(EZR0$Eggs)
EZresults[13,2] = pop.sd(EZR0$Eggs)
EZresults[13,3] = median(EZR0$Eggs)
EZresults[13,4] = mean(EZR0$prop)
EZresults[13,5] = pop.sd(EZR0$prop)
EZresults[13,6] = nrow(EZR0)
EZresults[13,7] = nrow(EZR0)/20

EZDH = filter(EZ10, Name == "Blank_/Developed/Med Intensity/Developed/High Intensity")
hist(EZDH$Eggs)
EZresults[14,1] = mean(EZDH$Eggs)
EZresults[14,2] = pop.sd(EZDH$Eggs)
EZresults[14,3] = median(EZDH$Eggs)
EZresults[14,4] = mean(EZDH$prop)
EZresults[14,5] = pop.sd(EZDH$prop)
EZresults[14,6] = nrow(EZDH)
EZresults[14,7] = nrow(EZDH)/20



EZresults$Run[1:14] = "Run22"
#total polygons - should be 724 for sensitivity analyses
sum(EZresults[7])

# save results for each run
EZresultsRun22 = EZresults
write.csv(EZresults,"EZrun22.csv")





#################  Test and Development Code  #######################

#first test UD
testUD = read.csv("Coords.2016.Feb.20.03_41_23.txt")
xcoords = strsplit(as.character(testUD$MonXs), ",")
ycoords = strsplit(as.character(testUD$MonYs), ",")
xnum10 = as.numeric(xcoords)
ynum10 = as.numeric(ycoords[[10]])

#time format doesn't really matter
time = as.POSIXct(1:length(ynum10), origin="1970-01-01", format = "%M", tz="UTC")

movedata = move(x=xnum10, y=ynum10, time=time, proj=CRS("+proj=longlat"))
summary(movedata)
plot(movedata)
str(movedata)
proj4string(movedata)

movedata_t = spTransform(x=movedata, CRSobj = "+proj=aeqd", center=TRUE)
plot(movedata_t, pch = 20, cex=0.5)
dbbmm = brownian.bridge.dyn(movedata_t, raster = 10, location.error = 0.1)
plot(dbbmm)
contour(dbbmm, levels=c(0.5, 0.95), col=c(6,2), lwd=2, add=TRUE)

dbbmm2 = brownian.bridge.dyn(movedata_t, raster = 30, location.error = 0.1)
contour(dbbmm2, levels=c(0.5, 0.95), col=c(6,2), lwd=2)#, add=TRUE)

# calculate area of UD within 95%
volUD = getVolumeUD(dbbmm2)
volUD = volUD<=.95
area = sum(values(volUD))







##############  More test and development code  ######################

str(xcoords) #each row is a monarch
max(sapply(xcoords, length)) #find max number of monarch locs = 1607

#one monarch - first monarch
time1 = as.POSIXct(1:length(xnumeric[[1]]), origin="1970-01-01", format = "%M", tz="UTC")
move1 = move(x=xnumeric[[1]], y=ynumeric[[1]], animal=names[1], time=time1, proj=CRS("+proj=longlat"))
summary(move1)
plot(move1, pch=20, cex=0.5)
proj4string(move1)
#plot in google maps
move1_df = as(move1, "data.frame")
m = get_map(bbox(extent(move1)*1.1), source="stamen", zoom=14)
ggmap(m)+geom_path(data=move1_df, aes(x=x, y=y))

#first 5 monarchs
stack1to5 = moveStack(list(allmons[[1]],allmons[[2]],allmons[[3]],allmons[[4]],allmons[[5]]))
equalProj(list(allmons[[1]],allmons[[2]],allmons[[3]],allmons[[4]],allmons[[5]]))

#first 5 monarchs - not in equal proj, which i'm not sure what that means - 
#can't use stack i guess, add to list one at a time
equalProj(list(allmons_t[[1]],allmons_t[[2]],allmons_t[[3]],allmons_t[[4]],allmons_t[[5]]))
stack1to5_t = moveStack(list(allmons_t[[1]],allmons_t[[2]],allmons_t[[3]],allmons_t[[4]],allmons_t[[5]]))
proj4string(allmons[[5]])
proj4string(allmons_t[[5]])

#try different values of raster size
#for allmons_t[[1]], the map is 2,000 x 1,000 units, whatever those units are

dbbmms_30 = brownian.bridge.dyn(allmons_t[[1]], raster=30, location.error = 0.1) #40s, Computational size: 2.4e+08
dbbmms_30.1 = brownian.bridge.dyn(allmons_t[[1]], raster=30, location.error = 1) #36s, Computational size: 2.4e+08
dbbmms_30.01 = brownian.bridge.dyn(allmons_t[[1]], raster=30, location.error = 0.01) #35s, Computational size: 2.4e+08
dbbmms_30.001 = brownian.bridge.dyn(allmons_t[[1]], raster=30, location.error = 0.001) #25s, Computational size: 2.4e+08
dbbmms_30.0001 = brownian.bridge.dyn(allmons_t[[1]], raster=30, location.error = 0.0001) #36s, Computational size: 2.4e+08
dbbmms_10.001 = brownian.bridge.dyn(allmons_t[[1]], raster=10, location.error = 0.001) #1m:22s, Computational size: 2.2e+09
dbbmms_20.001 = brownian.bridge.dyn(allmons_t[[1]], raster=20, location.error = 0.001) #45s, Computational size: 5.4e+08

plot(dbbmms_30)
contour(dbbmms_20.001, levels=c(0.5, 0.95), col=c(6,2), lwd=2)#, add=TRUE)


# calculate area of UD within 95%
volUD = getVolumeUD(dbbmms_30)
volUD = volUD<=.95
area = sum(values(volUD))

volUD_test = list()
area_test = list()
volUD_test[[1]] = getVolumeUD(dbbmms_30.001)
volUD_test[[1]] = volUD_test[[1]]<=0.95
area_test[[1]] = sum(values(volUD_test[[1]]))
volUD_test[[2]] = getVolumeUD(dbbmms_10.001)
volUD_test[[2]] = volUD_test[[2]]<=0.95
area_test[[2]] = sum(values(volUD_test[[2]]))
volUD_test[[3]] = getVolumeUD(dbbmms_20.001)
volUD_test[[3]] = volUD_test[[3]]<=0.95
area_test[[3]] = sum(values(volUD_test[[3]]))






