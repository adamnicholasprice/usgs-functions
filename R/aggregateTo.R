
aggregateTo= function(fpath,dataname,aggregateTOstep,mthd)
{
# import and get metadata
  for (i in c(2,4,6,8,10))
    {
    
    
trex <- list.files(path = fpath, pattern =glob2rx(paste0("*",dataname,"*","HUC",i,"*")))
fdata <- feather::read_feather(paste0(fpath,trex))

metadata <- METsteps::extractMetadata(trex)

# Create date seq
dateVector <- seq.Date(from = as.Date(metadata$startDate),
                       by = metadata$timeStep,
                       length.out = nrow(fdata))

dateVector[length(dateVector)] <- metadata$endDate

library(lubridate)
match.fun(aggregateTOstep)(dateVector)

# create zoo object
library(zoo)
zooobj <- as.zoo(fdata)
index(zooobj) <- dateVector


#aggregate .zoo
newzoo <- aggregate(x = zooobj,
                    by = match.fun(aggregateTOstep),
                    FUN = match.fun(mthd),
                    na.rm = TRUE
                    )

#redefine metadata
metadata2 <- as.data.frame(metadata)
newStart <- paste0(index(newzoo)[1], '-01-01')
newEnd <- paste0(index(newzoo)[nrow(newzoo)], '-01-01')
metadata2$startDate <- newStart
metadata2$endDate <- newEnd
metadata2$timeStep <- aggregateTOstep
metadata2$dataName <- paste0(metadata$dataName,'-',mthd)


#combine new metadata and newly aggregated zoo file into a list
  if (i ==2){  
        newzoo2 <- list( HUC2=newzoo ,info = t(metadata2))
          }

  if (i==4){
        newzoo2 <- list(HUC4=newzoo ,info = t(metadata2))
          }

  if (i==6){
        newzoo2 <- list(HUC6=newzoo ,info = t(metadata2))
          }

  if (i==8){
        newzoo2 <- list(HUC8=newzoo ,info = t(metadata2))
          }

  if (i==10){
        newzoo2 <- list(HUC10=newzoo ,info = t(metadata2))
  }

#write new .zoo obj to feather
METsteps::zooHUCtoFeather(zoo.obj = newzoo2,
                featherPath =  fpath)
  }
}
