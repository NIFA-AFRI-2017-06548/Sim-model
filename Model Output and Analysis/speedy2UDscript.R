######################  Utilization Distributions and Time Budget per Habitat  #############################


"C:\Program Files (x86)\PuTTY\pscp.exe" C:\Users\tgrant\Downloads\file tgrant@speedy2:/home/tgrant/R/.


module load R

R --no-restore (doesn't load workspace)
R --vanilla (doesn't load workspace)
R --no-save (doesn't save workspace)

ll -a (hidden files on linux) 

save.image(file = ".Rdata")

############# Main Code - Utilization Distribution  ##################

library(move)

CoordData120 = read.csv("Coords.2016.Mar.08.10_03_50.txt") #run 64 - perception = 400

names = as.character(CoordData231$Name)
#split strings
xcoords = strsplit(as.character(CoordData231$MonXs), ",")
ycoords = strsplit(as.character(CoordData231$MonYs), ",")

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


#calculate UD for all monarchs

dbbmms231 = list()
system.time( #this function calculates the time this function takes
  for (i in 1:1000){
    dbbmms231[[i]] = brownian.bridge.dyn(allmons_t[[i]], raster=30, location.error = 0.001)
  }
)  #end system.time


area124= vector()
for (i in 1:1000){
  volUD = getVolumeUD(dbbmms124[[i]])
  volUD = volUD<=.95
  area124[i] = sum(values(volUD))
}


save(dbbmms124, file="dbbmms124.RData")

save(area124, file="area124.RData")

source(“speedy2UDscript.R”)

Ctrl C to stop R running


"C:\Program Files (x86)\PuTTY\pscp.exe" tgrant@speedy2:/home/tgrant/R/file C:\Users\tgrant\Downloads\. 
