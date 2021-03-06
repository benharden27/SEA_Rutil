readLatLon <- function(filein) {

r <- readLines(filein,n=30,encoding ='UTF-8')

# set possible patterns to search for
patt <- "([0-9]+[^0-9]+[0-9]+[^0-9]+[0-9]*)"
patt2 <- "([0-9]+[^0-9]+[0-9])"
patt3 <- "([0-9]+)"

# LATITUDE
# switch depending on what format the lon and lat are stored as
if(length(grep('^.*Lat.*Lon.*$',r)) > 0) { 
  case <- 1
  line <- grep('^.*Lat.*Lon.*$',r)
} else if (length(grep('Lat|Lat',r,ignore.case=T))==0) {
  case <- 2
  line <- 1
} else {
  case <- 3
  line <- grep("Lat",r,ignore.case=T)[1] # finds the word "Latitude" in r
}

# search for the patterns in order
a <- regexpr(patt,r[line])
if (a==-1) {
  a <- regexpr(patt2,r[line])
  if(a==-1) {
    a <- regexpr(patt3,r[line])
  }
}

# assign the latitude based on the search findings
lat <- substr(r[line],a,a+attr(a,"match.length")-1)
lat <- strsplit(lat,"[^0-9\\.]")[[1]]
lat <- lat[lat!=""] # removes blank sections

# Define Hemisphere
hemi <- substr(r[line],regexpr("[NS]",r[line]),regexpr("[NS]",r[line]));
if(hemi=='S'){
  fac <- -1
} else {
  fac <- 1
}

# depending on the end format of "lat" do various different things to parse the output
if(length(lat)==1) {
  if(length(strsplit(lat,"\\.")[[1]])<3) {
    lat <- fac*as.numeric(substr(lat,1,2)) + fac * as.numeric(substr(lat,3,100))/60
  } else {
    lat <- strsplit(lat,"\\.")[[1]]
    lat <- fac * as.numeric(lat[1]) + fac * (as.numeric(lat[2])+as.numeric(lat[3])/10)/60
  }
} else {
  lat <- fac * as.numeric(lat[1]) + fac * as.numeric(lat[2])/60;
}


# LONGITUDE

# again, switch by the format of the line
if (case==1) {
  rest<- substr(r[line],a+attr(a,"match.length"),100)
} else if (case==2) {
  rest <- 'xxxxx'
} else {
  rest <- r[grep("Lon",r,ignore.case = T)[1]]
}

# search for the patterns
a <- regexpr(patt,rest)
if (a==-1) {
  a <- regexpr(patt2,rest)
  if(a==-1) {
    a <- regexpr(patt3,rest)
  }
}

# assign longitude based on patterns
lon <- substr(rest,a,a+attr(a,"match.length")-1)
lon <- strsplit(lon,"[^0-9\\.]")[[1]]
lon <- lon[lon!=""] # removes blank sections

# Define Hemisphere
hemi <- substr(rest,regexpr("[EW]",rest),regexpr("[WE]",rest));
if(hemi=='W'){
  fac <- -1
} else {
  fac <- 1
}
  

# do various formating based on type of "lon" output
if(length(lon)==1) {
  if(length(strsplit(lon,"\\.")[[1]])==2) {
    lon <- fac * as.numeric(substr(lon,1,2)) + fac * as.numeric(substr(lon,3,100))/60
  } else if (nchar(lon)>3) {
    lon <- strsplit(lon,"\\.")[[1]]
    lon <- fac * as.numeric(lon[1]) +fac * (as.numeric(lon[2])+as.numeric(lon[3])/10)/60
  } else {
    lon <- fac*as.numeric(lon)
  }
} else {
  lon <- fac* as.numeric(lon[1]) + fac * as.numeric(lon[2])/60;
}

# show the lines of output for when there is no lon or no lat
if(is.na(lon)|is.na(lat)) {
  show(r)
  # a<-readline('Press enter key to continue...')
}

X <- NULL
X$lon <- lon
X$lat <- lat
X$r <- r

return(X)
}