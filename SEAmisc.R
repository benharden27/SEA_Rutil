########
# RETURN STANDARD FOLDERS
########
makeFolders <- function(cruiseID,isReport=F) {
  
  foldmaster <- file.path('~/data/SEA',cruiseID)
  foldship <- file.path(foldmaster,'SHIPDATA')
  foldCTD <- file.path(foldmaster,'CTD','Cnv')
  
  if(isReport) {
    folddoc <- file.path('~/Documents/SEA/',cruiseID,'cruiseReport')
  } else {
    folddoc <- file.path('~/Documents/SEA/',cruiseID,'seaComponent')
  }
  
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
  
  return(list(foldmaster=foldmaster,foldship=foldship,foldCTD=foldCTD,folddoc=folddoc,foldplot=foldplot))
  
}