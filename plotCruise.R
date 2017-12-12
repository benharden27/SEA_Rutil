rm(list = ls())

# Load the OCE package
library(oce)
library(ocedata)
# options(java.parameters = "-Xmx8000m")
# library(xlsx)
source("~/code/Rutil/readSEAdata.R")
source("~/code/Rutil/SEAplots.R")
data(coastlineWorldFine)

# Set the master (source) and output folders for the data
cruiseID <- 'C271A'
# elgs <- 87
# CTDflag <- NULL
CTDflag <- 6:10 # S269

foldmaster <- file.path('~/data/SEA',cruiseID)
foldship <- file.path(foldmaster,'SHIPDATA')
foldCTD <- file.path(foldmaster,'CTD','Cnv')

folddoc <- file.path('~/Documents/SEA/cruiseReports',cruiseID)
if (file.exists(paste(folddoc, "/", sep = ""))) {
  cat(folddoc,"is already a directory")
} else {
  dir.create(folddoc)
}

foldplot <- file.path(folddoc,'plots')
if (file.exists(paste(foldplot, "/", sep = ""))) {
  cat(foldplot,"is already a directory")
} else {
  dir.create(foldplot)
}


#####
# CTD DATA
#####
# Read in CTD data
foldin <- foldCTD
CTDs <- readSEActd(foldin,CTDflag=CTDflag)

if(length(CTDs)>1) {
  # plot Temp and Sal data with Map
  outname <- file.path(foldplot,'CTD_section.png')
  png(filename=outname,height=9,width=7,units='in',res=300,type='cairo')
  plotmapTS(CTDs)
  dev.off()
  # plot Auxilary data (O2 and fluo)
  outname <- file.path(foldplot,'CTD_section_2.png')
  png(filename=outname,height=6,width=7,units='in',res=300,type='cairo')
  plotO2flsec(CTDs)
  dev.off()
} else {
  # just plot the only CTD
  outname <- file.path(foldplot,'CTD_section.png')
  png(filename=outname,height=9,width=7,units='in',res=300,type='cairo')
  plot(CTDs[[1]])
  dev.off()
}


#####
# FLOWTHROUGH DATA
#####
# Read in flowthrough data
elgs <- list.files(path=foldmaster, pattern="\\.elg")
filein <- file.path(foldmaster,elgs)
df <- readSEAelg(filein)
# Plot flowthrough data
outname <- file.path(foldplot,'ELG_flowthrough.png')
png(filename=outname,height=7,width=7,units='in',res=300,type='cairo') # set up the png file to print to
plotSEAelg(df)
dev.off()
# Plot cruisetrack
outname <- file.path(foldplot,'cruiseTrack.png')
png(filename=outname,height=7,width=7,units='in',res=300,type='cairo') # set up the png file to print to
plotSEAct(df)
dev.off()


#####
# HOURLY DATA
#####
# Read in hourly work
filein <- file.path(foldship,paste0(cruiseID,'_Hourlywork.xlsm'))
df <- readSEAxls(filein)
if(cruiseID=='C271A') {
  scale <- 0.05
  stp <- 1
} else {
  scale = 0.2
  stp <- 1
}
# Plot winds
outname <- file.path(foldplot,'winds.png')
png(filename=outname,height=7,width=7,units='in',res=300,type='cairo') # set up the png file to print to
plotSEAwind(df,scale=scale,stp=stp)
dev.off()


#####
# SURFACE STATIONS
#####
# Read in surface stations
filein <- file.path(foldship,paste0(cruiseID,'_surfsamp.xlsm'))
df <- readSEAxls(filein)
# Plot surface station data
outname <- file.path(foldplot,'Surface_stations.png')
png(filename=outname,height=9,width=7,units='in',res=300,type='cairo') # set up the png file to print to
plotSEAsurf(df,vars=c(1,2,3,4,5,6))
dev.off()


#####
# CURRENTS
#####
# Read currents
foldin <- file.path(foldmaster,'OceanDataView','ADCP\ Text\ Files')
X <- readSEAadcp_all(foldin)
if(cruiseID=='C271A') {
  scale <- 0.01
  stp <- 2
} else {
  scale <- 0.2
  stp <- 6
}
# plot currents
outname <- file.path(foldplot,'currents.png')
png(filename=outname,height=7,width=7,units='in',res=300,type='cairo') # set up the png file to print to
plotSEAcurr(X,scale=scale,stp=stp)
dev.off()


#####
# BIOMASS
#####
# Read biomass
filein <- file.path(foldship,paste0(cruiseID,'_Neuston.xlsm'))
df <- try(readSEAxls(filein))
if(inherits(df,'try-error')) {
  filein <- file.path(foldship,paste0(cruiseID,'_Neuston.xlsx'))
  df <- try(readSEAxls(filein,skip=1))
}
outname <- file.path(foldplot,'biomass.png')
png(filename=outname,height=7,width=7,units='in',res=300,type='cairo') # set up the png file to print to
plotSEAbio(df)
dev.off()



