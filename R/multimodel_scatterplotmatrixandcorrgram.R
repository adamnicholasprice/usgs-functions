nhucs <- 18

datanow <- Data
nmodels <- length(datanow)
alltime <- index(datanow[[1]])
modelnames <- substr(names(datanow),1,nchar(names(datanow))-2)

getSeasonNum <- function(DATES) {
  WS <- as.Date("2012-12-01", format = "%Y-%m-%d") # Winter monthly beg
  SE <- as.Date("2012-03-01",  format = "%Y-%m-%d") # Spring monthly beg
  SS <- as.Date("2012-06-01",  format = "%Y-%m-%d") # Summer monthly beg
  FE <- as.Date("2012-09-01",  format = "%Y-%m-%d") # Fall monthly beg
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  
  ifelse (d >= WS | d < SE, 1,
          ifelse (d >= SE & d < SS, 2,
                  ifelse (d >= SS & d < FE, 3, 4)))
}
allseasnum <- getSeasonNum(alltime)

romat <- array(data=NA, dim=c(length(alltime),nhucs,nmodels+1))
for (imod in 1:nmodels) {
  for (ihuc in 1:nhucs) {
    romat[,ihuc,imod] <- datanow[[imod]][,ihuc]
  }
}
for (ihuc in 1:nhucs) {
  romat[, ihuc, nmodels+1] <- allseasnum
}
dimnames(romat)[[3]] <- c(modelnames,"SEASON")
dimnames(romat)[[2]] <- 1:nhucs
dimnames(romat)[[1]] <- alltime


###### VERSION 2: Correlogram + scatterplot matrix
# See psych library for more functions:
# http://personality-project.org/r/Rfunc/pairs.panels.R
# Also:
# https://stat.ethz.ch/R-manual/R-devel/library/graphics/html/pairs.html
# library(psych)

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  
  r <- abs(cor(x, y, use="pairwise", method="pearson"))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
  
  p <- cor.test(x, y, na.action = "na.omit", method="pearson")$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.2, txt2)
}

panel.hist <- function(x, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot=FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y)
}

idline <- function(x,y,...){
  points(x,y,...)
  abline(a = 0,b = 1,col = "black",...)
}

for (ihuc in 1:nhucs) {
  jpeg(paste0('spmatrixHUC2.',ihuc,'.jpg'),width=10,height=10,units='in',res=500)
  
  rohucnow <- data.frame(romat[,ihuc,])
  pairs(rohucnow[,1:nmodels],panel=idline,diag.panel=panel.hist,upper.panel=panel.cor,
        pch=21,bg=c("blue","green","red","yellow")[rohucnow$SEASON],gap=0,main=paste0("RO-HUC2.",ihuc))
  
  dev.off()
}
