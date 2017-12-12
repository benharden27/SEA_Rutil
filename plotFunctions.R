plotmapTS<-function(CTDs) {
  # Make the whole Section and read metadata
  sec <- as.section(CTDs)
  dist <- geodDist(sec,alongPath=T)
  s <- sectionGrid(sec)
  nstation <- length(s[['station']])
  p <- unique(s[['pressure']])
  d <- swDepth(p,mean(s@metadata$latitude,na.rm=T))
  
  # Set up T and S arrays for plotting
  np <- length(p)
  Te <- S <- array(NA, dim=c(nstation, np))
  lonctd <- latctd <- rep(NA,nstation)
  for (i in 1:nstation) {
    Te[i, ] <- s[['station']][[i]][['temperature']]
    S[i, ] <- s[['station']][[i]][['salinity']]
    lonctd[i] <- s[["station"]][[i]]@metadata$longitude
    latctd[i] <- s[["station"]][[i]]@metadata$latitude
  }
  lonctd[lonctd<0] <- lonctd[lonctd<0]+360;
  
  
  # Make appropriate axes
  nch <- 8 # min desired number of levels
  
  # Temerature
  rnd <- 0.5
  Tinc <- c(2,1,0.5) # decending order
  Tran <- c(floor(min(Te,na.rm=T)/rnd)*rnd,ceiling(max(Te,na.rm=T)/rnd)*rnd)
  Tinc <- Tinc[which(diff(Tran)/Tinc>nch)[1]]
  Tran <- seq(Tran[1], Tran[2], Tinc)
  
  # Salinity
  rnd <- 0.1
  Sinc <- c(0.5,0.2,0.1) # decending order
  Sran <- c(floor(quantile(S,0.01,na.rm=T)/rnd)*rnd,ceiling(quantile(S,0.99,na.rm=T)/rnd)*rnd)
  Sinc <- Sinc[which(diff(Sran)/Sinc>nch)[1]]
  Sran <- seq(Sran[1], Sran[2], Sinc)
  
  # Create the colormaps
  Tcm <- colormap(Te, breaks=Tran, col=oceColorsTemperature)
  Scm <- colormap(S, breaks=Sran, col=oceColorsSalinity)
  
  # Sets up the layout
  layout(matrix(c(1,2,3),3,1),heights=c(1.5,1,1))
  
  # Plot the map
  par(pty='s') # square plot
  
  # find range of lon/lat
  lonran <- diff(range(lonctd,na.rm=T))/10
  latran <- diff(range(latctd,na.rm=T))/10
  
  if(sum(lonctd>180)==0) {
    mapPlot(coastlineWorldFine,proj="+proj=merc +lon_0=180",
            latitudelim = range(latctd,na.rm=T)+c(-latran,latran), 
            longitudelim = range(lonctd,na.rm=T)+c(-lonran,lonran),
            col='gray')
  } else {
    mapPlot(coastlineWorldFine,proj="+proj=merc +lon_0=180",
            latitudelim = range(latctd,na.rm=T)+c(-latran,latran), 
            longitudelim = range(lonctd,na.rm=T)+c(-lonran,lonran),
            col='gray',grid=FALSE)
    latlabels <- seq(-90, 0, 5)
    lonlabels <- c(seq(0, 175, 5), seq(-180, 0, 5))
    mapGrid(longitude=lonlabels, latitude=latlabels)
    mapAxis(longitude=lonlabels, latitude=latlabels)
  }
    
  
  # Plot stations, path and labels
  mapLines(lonctd,latctd)
  mapPoints(lonctd,latctd,col=2)
  distmin <- tail(dist,1)/(length(dist)+1)
  cur <- 1
  for (i in 1:length(dist)) {
    if (i==1 | geodDist(lonctd[cur],latctd[cur],lonctd[i],latctd[i])>distmin) {
      mapText(lonctd[i],latctd[i],s@metadata$stationId[i],pos=4)
      cur <- i
    }
  }
  
  
  # Plots the sections
  # Change aspect ratio of plots
  par(pty='m')
  
  # Plot temperature and add labels and profile lines
  imagep(dist, p, Te, colormap=Tcm, flipy=TRUE,ylab='depth [m]',
         filledContour=TRUE,zlab='temperature [degC]',missingColor = NULL,
         drawTriangles = T, ylim = c(0,600),
         zlabPosition = 'side')
  cur <- 1
  for (i in 1:length(dist)){
    lines(dist[c(i,i)],c(0,max(CTDs[[i]][['depth']],na.rm=T)),col='gray') 
    if (i==1 | geodDist(lonctd[cur],latctd[cur],lonctd[i],latctd[i])>distmin) {
      mtext(s@metadata$stationId[i],3,0,at=dist[i])
      cur <- i
    }
  }
  
  # Plot temperature and add labels and profile lines
  imagep(dist, p, S, colormap=Scm, flipy=TRUE,xlab='distance [km]', ylab='depth [m]',
         filledContour=TRUE,zlab='salinity',missingColor = NULL,
         drawTriangles = T, ylim = c(0,600),
         zlabPosition = 'side')
  cur <- 1
  for (i in 1:length(dist)){
    lines(dist[c(i,i)],c(0,max(CTDs[[i]][['depth']],na.rm=T)),col='gray') 
    if (i==1 | geodDist(lonctd[cur],latctd[cur],lonctd[i],latctd[i])>distmin) {
      mtext(s@metadata$stationId[i],3,0,at=dist[i])
      cur <- i
    }
  }

}




plotmap3sec<-function(CTDs) {
  # Make the whole Section and read metadata
  sec <- as.section(CTDs)
  dist <- geodDist(sec,alongPath=T)
  s <- sectionGrid(sec)
  nstation <- length(s[['station']])
  p <- unique(s[['pressure']])
  d <- swDepth(p,mean(s@metadata$latitude,na.rm=T))
  
  # Set up T and S arrays for plotting
  np <- length(p)
  Te <- S <- Den <- array(NA, dim=c(nstation, np))
  lonctd <- latctd <- rep(NA,nstation)
  for (i in 1:nstation) {
    Te[i, ] <- s[['station']][[i]][['temperature']]
    S[i, ] <- s[['station']][[i]][['salinity']]
    Den[i, ] <- s[['station']][[i]][['sigmaTheta']]
    lonctd[i] <- s[["station"]][[i]]@metadata$longitude
    latctd[i] <- s[["station"]][[i]]@metadata$latitude
  }
  lonctd[lonctd<0] <- lonctd[lonctd<0]+360;
  
  
  # Make appropriate axes
  nch <- 8 # min desired number of levels
  
  # Temerature
  rnd <- 0.5
  Tinc <- c(2,1,0.5) # decending order
  Tran <- c(floor(min(Te,na.rm=T)/rnd)*rnd,ceiling(max(Te,na.rm=T)/rnd)*rnd)
  Tinc <- Tinc[which(diff(Tran)/Tinc>nch)[1]]
  Tran <- seq(Tran[1], Tran[2], Tinc)
  
  # Salinity
  rnd <- 0.1
  Sinc <- c(2,1,0.5,0.2,0.1,0.05,0.02,0.01) # decending order
  Sran <- c(floor(quantile(S,0.01,na.rm=T)/rnd)*rnd,ceiling(quantile(S,0.99,na.rm=T)/rnd)*rnd)
  Sinc <- Sinc[which(diff(Sran)/Sinc>nch)[1]]
  Sran <- seq(Sran[1], Sran[2], Sinc)
  
  # Density
  rnd <- 0.1
  Dinc <- c(0.5,0.2,0.1) # decending order
  Dran <- c(floor(quantile(Den,0.01,na.rm=T)/rnd)*rnd,ceiling(quantile(Den,0.99,na.rm=T)/rnd)*rnd)
  Dinc <- Dinc[which(diff(Dran)/Dinc>nch)[1]]
  Dran <- seq(Dran[1], Dran[2], Dinc)
  
  # Create the colormaps
  Tcm <- colormap(Te, breaks=Tran, col=oceColorsTemperature)
  Scm <- colormap(S, breaks=Sran, col=oceColorsSalinity)
  Dcm <- colormap(Den, breaks=Dran, col=oceColorsDensity)
  
  # Sets up the layout
  layout(matrix(c(1,1,1,2,3,4,2,3,4),3,3))
  
  # Plot the map
  par(pty='s') # square plot
  mapPlot(coastlineWorldFine,proj="+proj=merc +lon_0=180",
          latitudelim = range(latctd,na.rm=T)+c(-.2,.2), 
          longitudelim = range(lonctd,na.rm=T)+c(-.2,.2),
          col='gray')
  # latlabels <- seq(-90, 0, 5)
  # lonlabels <- c(seq(0, 175, 5), seq(-180, 0, 5))
  # mapGrid(longitude=lonlabels, latitude=latlabels)
  # mapAxis(longitude=lonlabels, latitude=latlabels)
  
  # Plot stations, path and labels
  mapLines(lonctd,latctd)
  mapPoints(lonctd,latctd,col=2)
  distmin <- tail(dist,1)/(length(dist)+1)
  cur <- 1
  for (i in 1:length(dist)) {
    if (i==1 | geodDist(lonctd[cur],latctd[cur],lonctd[i],latctd[i])>distmin) {
      mapText(lonctd[i],latctd[i],s@metadata$stationId[i],pos=4)
      cur <- i
    }
  }
  
  
  # Plots the sections
  # Change aspect ratio of plots
  par(pty='m')
  
  # Plot temperature and add labels and profile lines
  imagep(dist, p, Te, colormap=Tcm, flipy=TRUE,ylab='depth [m]',
         filledContour=TRUE,zlab='temperature [degC]',missingColor = NULL,
         drawTriangles = T, ylim = c(0,200),
         zlabPosition = 'side')
  cur <- 1
  for (i in 1:length(dist)){
    lines(dist[c(i,i)],c(0,max(CTDs[[i]][['depth']],na.rm=T)),col='gray') 
    if (i==1 | geodDist(lonctd[cur],latctd[cur],lonctd[i],latctd[i])>distmin) {
      mtext(s@metadata$stationId[i],3,0,at=dist[i])
      cur <- i
    }
  }
  
  # Plot temperature and add labels and profile lines
  imagep(dist, p, S, colormap=Scm, flipy=TRUE,xlab='distance [km]', ylab='depth [m]',
         filledContour=TRUE,zlab='salinity',missingColor = NULL,
         drawTriangles = T, ylim = c(0,200),
         zlabPosition = 'side')
  cur <- 1
  for (i in 1:length(dist)){
    lines(dist[c(i,i)],c(0,max(CTDs[[i]][['depth']],na.rm=T)),col='gray') 
    if (i==1 | geodDist(lonctd[cur],latctd[cur],lonctd[i],latctd[i])>distmin) {
      mtext(s@metadata$stationId[i],3,0,at=dist[i])
      cur <- i
    }
  }
  
  # Plot temperature and add labels and profile lines
  imagep(dist, p, Den, colormap=Dcm, flipy=TRUE,xlab='distance [km]', ylab='depth [m]',
         filledContour=TRUE,zlab='density',missingColor = NULL,
         drawTriangles = T, ylim = c(0,200),
         zlabPosition = 'side')
  cur <- 1
  for (i in 1:length(dist)){
    lines(dist[c(i,i)],c(0,max(CTDs[[i]][['depth']],na.rm=T)),col='gray') 
    if (i==1 | geodDist(lonctd[cur],latctd[cur],lonctd[i],latctd[i])>distmin) {
      mtext(s@metadata$stationId[i],3,0,at=dist[i])
      cur <- i
    }
  }
  
  
}


plotO2flsec<-function(CTDs) {
  # Make the whole Section and read metadata
  sec <- as.section(CTDs)
  dist <- geodDist(sec,alongPath=T)
  s <- sectionGrid(sec)
  nstation <- length(s[['station']])
  p <- unique(s[['pressure']])
  d <- swDepth(p,mean(s@metadata$latitude,na.rm=T))
  
  # Set up T and S arrays for plotting
  np <- length(p)
  Te <- S <- array(NA, dim=c(nstation, np))
  lonctd <- latctd <- rep(NA,nstation)
  Te <- S <- array(NA, dim=c(nstation, np))
  for (i in 1:nstation) {
    if(!is.null(s[['station']][[i]][['fluorescence']])) {
      Te[i, ] <- s[['station']][[i]][['fluorescence']]
    }
    if(!is.null(s[['station']][[i]][['oxygenConcentrationMole']])) {
      S[i, ] <- s[['station']][[i]][['oxygenConcentrationMole']]
    } else if (!is.null(s[['station']][[i]][['oxygen']])) {
      S[i, ] <- s[['station']][[i]][['oxygen']]
    }
    lonctd[i] <- s[["station"]][[i]]@metadata$longitude
    latctd[i] <- s[["station"]][[i]]@metadata$latitude
  }
  lonctd[lonctd<0] <- lonctd[lonctd<0]+360;
  
  nch <- 8 # min desired number of levels
  
  rnd <- 0.001
  Tinc <- c(0.5,0.2,0.1,0.05,0.02,0.01,0.005,0.002,0.001) # decending order
  Tran <- c(floor(min(Te,na.rm=T)/rnd)*rnd,ceiling(quantile(Te,0.99,na.rm=T)/rnd)*rnd)
  Tinc <- Tinc[which(diff(Tran)/Tinc>nch)[1]]
  Tran <- seq(Tran[1], Tran[2], Tinc)
  
  rnd <- 1
  Sinc <- c(5,2,1,0.5,0.2,0.1) # decending order
  Sran <- c(floor(quantile(S,0.01,na.rm=T)/rnd)*rnd,ceiling(quantile(S,0.99,na.rm=T)/rnd)*rnd)
  Sinc <- Sinc[which(diff(Sran)/Sinc>nch)[1]]
  Sran <- seq(Sran[1], Sran[2], Sinc)
  
  par(mfrow=c(2, 1))
  Tcm <- colormap(Te, breaks=Tran, col=oceColorsChlorophyll)
  Scm <- colormap(S, breaks=Sran, col=oceColorsSalinity)
  distmin <- tail(dist,1)/(length(dist)+1)
  
  imagep(dist, p, Te, colormap=Tcm, flipy=TRUE,ylab='depth [m]',
         filledContour=TRUE,zlab='chl-a fluorescence [Volts]',missingColor = NULL,
         drawTriangles = T, ylim = c(0,300),
         zlabPosition = 'side')
  cur <- 1
  for (i in 1:length(dist)){
    lines(dist[c(i,i)],c(0,max(CTDs[[i]][['depth']],na.rm=T)),col='gray') 
    if (i==1 | geodDist(lonctd[cur],latctd[cur],lonctd[i],latctd[i])>distmin) {
      mtext(s@metadata$stationId[i],3,0,at=dist[i])
      cur <- i
    }
  }
  imagep(dist, p, S, colormap=Scm, flipy=TRUE,xlab='distance [km]', ylab='depth [m]',
         filledContour=TRUE,zlab='oxygen conc. [Mol/L]',missingColor = NULL,
         drawTriangles = T, ylim = c(0,300),
         zlabPosition = 'side')
  cur <- 1
  for (i in 1:length(dist)){
    lines(dist[c(i,i)],c(0,max(CTDs[[i]][['depth']],na.rm=T)),col='gray') 
    if (i==1 | geodDist(lonctd[cur],latctd[cur],lonctd[i],latctd[i])>distmin) {
      mtext(s@metadata$stationId[i],3,0,at=dist[i])
      cur <- i
    }
  }
}

