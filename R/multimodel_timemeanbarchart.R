#####################################################################
# PART I: MANUAL LABOR

##### First, use exploreShiny to get variable "Data" or "subData"

library(METsteps)
METsteps::exploreShiny(path.feather='C:/Users/sleung/Desktop/USGShydro/newfeatherfiles/hucfinalfeatherfiles_mmpermonth')

#### Now fill in some user-defined options (3 of them)

# - 1.) Choose the huc numbers that you want to look at
# (should be in vector format)
hucnums <- 1:18 # Ex: c(1,4,8,18)

# - 2.) Choose either "subData" or "Data" to look at by setting
# datanow equal to either variable;
# "Data" or "subData" (subset of Data corresp to chosen season or month):
# list of length numdatasets;
# each element in list is num of dates (rows) by hucs (cols)
datanow <- subData

# - 3.) Main title you want for your barcharts
bartitle <- paste0("Springtime runoff, ",tLim[1],"-",tLim[2])
# tLim is a var that comes after you exploreShiny

#####################################################################
# PART II: AUTOMATED PLOTTING

#### Get vars ready and load packages

library(hydroTSM)
datanow <- lapply(subData, function(x) x[,hucnums])
numhucs <- length(hucnums)
numdatasets <- length(datanow)
datasetnames <- substr(names(datanow),1,nchar(names(datanow))-2)

#### Calculate time means over each huc for each model

# "meanhucsmods": list of length numdatasets;
# each element in list is vector - 1 (row) by numhucs (cols)
meanhucsmods <- lapply(datanow,annualfunction,mean,na.rm=TRUE)

# # ...Testing:
# # Print out meanhucsmods
# meanhucsmods
# # Model 1, first chosen HUC
# mean(datanow[[1]][,1])
# # Model 1, second chosen HUC
# mean(datanow[[1]][,2])
# # Model 2, fourth chosen HUC
# mean(datanow[[2]][,4])
# # ...Looks good!

#### Calculate multi-model means of time means over each huc

# "meanhucs": vector - 1 (row) by numhucs (cols)
meanhucs <- apply(simplify2array(meanhucsmods),1:2,mean,na.rm=TRUE)

# # ...Testing:
# # Print out meanhucs
# meanhucs
# # Multi-model mean, first chosen huc
# mean(unlist(lapply(meanhucsmods, function(x) x[1])))
# # Multi-model mean, second chosen huc
# mean(unlist(lapply(meanhucsmods, function(x) x[2])))
# # ...Looks good!

#### Calculate multi-model stdevs of annual means in each HUC region

# "sdhucs": vector - 1 (row) by numhucs (cols)
sdhucs <- apply(simplify2array(meanhucsmods),1:2,sd,na.rm=TRUE)

# # ...Testing:
# # Print out sdhucs
# sdhucs
# # Multi-model sd, first chosen huc
# sd(unlist(lapply(meanhucsmods, function(x) x[1])))
# # Multi-model sd, second chosen huc
# sd(unlist(lapply(meanhucsmods, function(x) x[2])))
# # ...Looks good!

#### Plot barcharts
l
# - Subplot 1: Multi-model time mean and stdev for each HUC
x11(width=8,height=4) # inches
#par(mfrow = c(2,1))
barcenters <- barplot(meanhucs, names.arg = hucnums, main=bartitle, las=1, ylim=c(0,max(meanhucs+sdhucs)))
segments(barcenters, meanhucs-sdhucs, barcenters, meanhucs+sdhucs, lwd=1)
title(xlab='HUC',ylab='multi-model mean +/- 1sd')

# - Subplot 2: Each model's time mean (with lines for multi-model mean and stdev) for each HUC
colors <- viridis(numdatasets)
meanhucsmodsmat <- matrix(unlist(meanhucsmods), ncol = numhucs, byrow=TRUE)
x11(width=8,height=4) # inches
barcenters <- barplot(meanhucsmodsmat, main=bartitle, beside=TRUE, col=colors,
                      names.arg = hucnums,las=1, ylim=c(0,max(meanhucsmodsmat)))
legend("top",fill=colors,legend=datasetnames,bty="n",ncol=3)
segments(barcenters[median(1:numdatasets),]-numdatasets/2, meanhucs, barcenters[median(1:numdatasets),]+numdatasets/2, meanhucs, lwd=2)
segments(barcenters[median(1:numdatasets),], meanhucs-sdhucs, barcenters[median(1:numdatasets),], meanhucs+sdhucs, lwd=2)
title(xlab='HUC',ylab='multi-model mean +/- 1sd')

