markSpeed <- function(t) {
    
    color <- ifelse(          t$v > v.up, "green"
                     , ifelse(t$v < v.dn, "red",  "black"))
    points( t$x, t$y )
    v.rnd <- round(t$v / v.inc) * v.inc
    v.up <<- v.rnd + v.inc
    v.dn <<- v.rnd - v.inc
    
    #figure out whether to place it above or left of the point
    pos <- ifelse (abs(t$x.d) > t$v/2, 3, 2)  #place above is mostly horiz, otherwise left 
    text( t$x, t$y, round(t$v), pos=pos, col=color)
}

markTime <- function(t, val) {
    points( t$x, t$y , pch = 3)
    pos <- ifelse (abs(t$x.d) > t$v/2, 3, 2)  #place above is mostly horiz, otherwise left 
    text( t$x, t$y, paste0("t=", val), pos=pos, cex=0.5)
}


#Global Variables
v.up <- 100; v.dn <- 0
v.inc <- 10
dist <- 0

plotTrip <- function(trip, v.mark=5, t.mark=100, tmin=1, tmax=nrow(trip)) {
    tmax <- min(tmax, nrow(trip))
    v.inc <<- v.mark
    par.orig <- par(mar=c(5,4,5,2))
    plot(trip[tmin:tmax, 1:2], type="n", asp=1)
    mtext("Plot of Route", line=4)
    
    a.thresh = 1
    
    markSpeed(trip[tmin,])
    current <- numeric(4); names(current) <- c('dist', 'v', 'a', 'heading')
    
    for (i in (tmin+1):tmax) {
        if (i %% t.mark == 0 ) markTime( trip[i, ], i) 
        i.v <- trip[i,"v"]
        i.a <- trip[i,"a"]
        thk <- round( i.v / 2)
        color <- ifelse( abs(i.a) < a.thresh, "orange", 
                         ifelse(    (i.a) > 0, "green", "red") ) 
        lines( trip[c(i,i-1), 1], trip[c(i,i-1), 2], type="l", lwd=thk, col=color)
        if( i.v > v.up | i.v < v.dn ) markSpeed( trip[i, ])
        
        current['dist'] <- current['dist'] + i.v/1000 #  dist.seg = speed * time (=1sec)
        current['v'] <- i.v * 3.6
        current['a'] <- i.a
        bearing <- calcBearing( trip[i, ])
        current['heading'] <- ifelse( is.nan(bearing), bearing.last, bearing) %% 360
    }
    #print(trip[i,])
    trip.info <- sprintf("distance traveled:%5.1f km\ndirection=%5.0f deg\ncurrent speed=%5.1f km/h\nacceleration=%5.1f m/s^2"
                         ,  current['dist'], current['heading'], current['v'], current['a'] )
    mtext(trip.info, adj=0, cex=.8)
    par(par.orig)
}

bearing.smooth <<- 0
bearing.last <<- 0
calcBearing <- function( t, smooth=TRUE ) {
    #smooth causes a 2deg ccw rotation at 179 to go to 181 rather than -179
    #, but results in angles > 360  ... may be imperfect if there are large swings in bearings
    vx <- t$x.d
    vy <- t$y.d
    bearing <- atan(vy / vx) * 180/pi
    if ( vx  < 0 )  {
        if ( vy < 0 ) {   #3rd Quadrant
            bearing <- bearing - 180  
            if (bearing.last %% 360 <= 180) bearing.smooth <<- bearing.smooth + 360
        } else {          #2nd Quadrant
            bearing <- bearing + 180
            if (bearing.last %% 360 > 180) bearing.smooth <<- bearing.smooth - 360
        }
    }
    if (smooth) bearing <- bearing + bearing.smooth
    if ( ! is.nan(bearing) ) bearing.last <<- bearing  
    return(bearing)
}

plotTripSegment <- function(trip, tmin=1, tmax=tmin+100, f=.01, ...) {
    par.orig <- par(mfrow=c(1,2))
    plotTrip( trip, tmin=tmin, tmax=tmax, ...)
    plot(lowess(tmin:tmax, trip$v[tmin:tmax], f=f), type="l",xlab="seconds", ylab="speed m/s")
    par(par.orig)
}

getTrip <- function(driver, trip) {
    #data.dir is the assumed directory, sequentially numbered .csv files are assumed
    trip.file <- paste0( data.dir, '/', driver, '/', trip, ".csv")
    trip <- read.csv( trip.file )
    trip.last <- trip[-nrow(trip), ]
    trip <- trip[-1, ]
    trip$x.d <- trip$x - trip.last$x
    trip$y.d <- trip$y - trip.last$y
    trip$v <- sqrt( trip$x.d^2 + trip$y.d^2 )  # distance travelled per second
    trip.last <- trip[-nrow(trip), 3:5 ]
    trip <- trip[-1, ]
    trip$x.d2 <- trip$x.d - trip.last$x.d
    trip$y.d2 <- trip$y.d - trip.last$y.d
    trip$a <- trip$v - trip.last$v     
    bearing <- numeric()
    for(i in 1:nrow(trip)) {
        bearing[i] <- ifelse( trip[i,]$v > 5, calcBearing( trip[i,]), NA )
    }
    trip <- cbind(trip, bearing)
    #trip$cum.dist <- 
    
    return(trip)
}

overlayHeading <- function (t, mag=100) {
    scale <- mag / t$v
    hx <- c( 0, t$x.d * scale) + t$x
    hy <- c( 0, t$y.d * scale) + t$y
    lines(hx, hy)
}

overlayTripHeadings <- function (trip, mag=100, skip=10) {
    for (i in seq(1, nrow(trip), by=skip)) plotHeading( trip[i,], mag=mag)
}