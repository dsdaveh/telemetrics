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

plotTrip <- function(trip, v.mark=5, t.mark=100, tmin=1, tmax=nrow(trip), b.marks=NULL, header=TRUE) {
    tmin <- max(tmin, 1)
    tmax <- min(tmax, nrow(trip))
    v.inc <<- v.mark
    mar.top <- ifelse (header, 5, 2)
    par.orig <- par(mar=c(5,4,mar.top,2))
    plot(trip[tmin:tmax, 1:2], type="n", asp=1)
    if(header) mtext("Plot of Route", line=4)
    
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
    if(header) mtext(trip.info, adj=0, cex=.8)
    
    if (length(b.marks > 0)) overlaySegmentBorders( trip, b.marks )
    
    par(par.orig)
}

bearing.smooth <<- 0
bearing.last   <<- 0
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

plotTripSegment.speed <- function (trip, tmin=1, tmax=tmin+100, f=.01, b.marks=NULL, b.col="red", ma=5) {
    tmin <- max(tmin, 1)
    tmax <- min(tmax, nrow(trip))
    v.ma <- filter( trip[tmin:tmax, ]$v, rep(1/ma,ma), sides=2)
    plot(tmin:tmax, v.ma, type="l", main="Speed (MA)", ylab="speed m/s", xlab="")
    abline(h=17, col=b.col, lty=2)
    if (length(b.marks) > 0) {
        b.sort <- sort(b.marks)
        ltype <- 2 + 1:length(b.sort) %% 2   # alternate linestyles between 2 & 3
        abline(v=b.sort, col=b.col, lty=ltype)
    }
}

plotTripSegment <- function(trip, tmin=1, tmax=tmin+100, f=.01, b.marks=NULL, b.col="red", ...) {
    tmin <- max(tmin, 1)
    tmax <- min(tmax, nrow(trip))
    par.orig <- par(mfrow=c(1,2))

    plotTrip(trip, tmin=tmin, tmax=tmax, header=TRUE, ...)
    if (length(b.marks) > 0) overlaySegmentBorders( trip, b.marks, b.col=b.col )
    
    plotTripSegment.speed( trip, tmin=tmin, tmax=tmax, b.marks=b.marks, b.col=b.col )
    
    par(par.orig)
}

plotTripSegment6 <- function(trip, tmin=1, tmax=tmin+100, ma=5, b.marks=NULL, b.col="red", ...) {
    tmin <- max(tmin, 1)
    tmax <- min(tmax, nrow(trip))
    tt <- trip[tmin:tmax,]
    par.orig <- par(mfrow=c(3,2), mar=c(4,4,2,2))
    
    plot(tmin:tmax, cumsum(tt$v), type="l", main="Cumulative Distance", ylab="distance", xlab="")
    if (length(b.marks)) abline(v=b.marks, col=b.col, lty=2)
    
    b.ma <- filter( tt$bearing, rep(1/ma,ma), sides=2)
    plot(tmin:tmax, b.ma, type="l", main="Bearing (MA)", ylab="degrees (+X=0)", xlab="")
    if (length(b.marks)) abline(v=b.marks, col=b.col, lty=2)
    
    plotTripSegment.speed( trip, tmin, tmax, ma=ma, b.marks=b.marks)
    
    b.ima <- filter( diff(tt$bearing, lag=1), rep(1/ma,ma), sides=2)
    plot((tmin+1):tmax, b.ima, type="l", main="Bearing (IMA)", ylab="degrees (+X=0)", xlab="")
    abline( h=c( -3, 3 ), col=b.col, lty=2)
    if (length(b.marks)) abline(v=b.marks, col=b.col, lty=2)
    
    a.ma <- filter( tt$a, rep(1/ma,ma), sides=2)
    plot(tmin:tmax, a.ma, type="l", main="Accel (MA)", ylab="speed m/s^2", xlab="")
    if (length(b.marks)) abline(v=b.marks, col=b.col, lty=2)
    
    plotTrip(trip, tmin=tmin, tmax=tmax, header=FALSE, ...)
    if (length(b.marks)) overlaySegmentBorders( trip, b.marks )
    
    par(par.orig)
    
}

overlaySegmentBorders <- function (trip, t.vec, size=nrow(trip)/2, b.col="red", ...) {
    rotate.90 <- matrix( c(0, 1, -1, 0), ncol=2)
    ends <- matrix( rep(NA, 4), ncol=2)
    i <- 0
    for (t in sort(t.vec)) {    i <- i+1
        tt <- trip[t, ]
        scale <- size / tt$v
        v <- c(tt$x.d, tt$y.d) * scale / 2   # v is the heading vector
        v.90 <- rotate.90 %*% v
        ends[1, ] <- as.numeric(tt[1, 1:2] + v.90)
        ends[2, ] <- as.numeric(tt[1, 1:2] - v.90)
        ltype <- 2 + i %% 2 # alternate linestyles between 2 & 3
        lines(ends, col=b.col, lty=ltype )
    }
}

getTrip <- function(driver, trip, v.thresh=5) {
    bearing.smooth <<- 0
    bearing.last   <<- 0
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
    bearing.smooth <<- 0
    bearing.last   <<- 0
    for(i in 1:nrow(trip)) {
        bearing[i] <- ifelse( trip[i,]$v > v.thresh, calcBearing( trip[i,]), NA )
    }
    trip <- cbind(trip, bearing)
    
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

MIN_SEGMENT_LENGTH <- 30
segment.parse.bearing <- function(trip, tmin=1, tmax=nrow(trip), zone=3) {
    tmin <- max(1, tmin)
    tmax <- min(tmax, nrow(trip))
    ma <- 5  
    b.ima <- filter( diff(trip$bearing[tmin:tmax], lag=1), rep(1/ma,ma), sides=2)
    
    in.zone <- ifelse( is.na(b.ima[1]), FALSE, abs(b.ima[1]) < zone )
    t.start <- ifelse( in.zone, 2, 0)
    ss <- data.frame( t0=integer(), tlen=integer())  # straight segments
    for (i in 2:length(b.ima)) {
        t <- tmin + i
        in.zone <- ifelse( is.na(b.ima[i]), FALSE, abs(b.ima[i]) < zone )
        if ( in.zone ) { #in the zone
            if( t.start > 0) {
                t.end <- t             #still in the zone
            } else {
                t.start <- t.end <- t  #new zone
            }
        } else {                 #out of the zone
            if (t.start > 0) {
                seg.len <- t.end - t.start
                if (seg.len >= MIN_SEGMENT_LENGTH) {
                    ss <- rbind(ss, data.frame( t0=t.start, tlen=seg.len))
                }
                t.start <- 0
            }
        }
    }
    if (t.start > 0) {
        seg.len <- t.end - t.start
        if (seg.len >= MIN_SEGMENT_LENGTH) {
            ss <- rbind(ss, data.frame( t0=t.start, tlen=seg.len))
        }
    }
        
    return(ss)
}

segment.parse.accel <- function(trip, tmin=1, tmax=nrow(trip), thresh=5) {
    tmin <- max(1, tmin)
    tmax <- min(tmax, nrow(trip))
    ma <- 5  
    
    v.ma <- filter( trip[tmin:tmax, ]$v, rep(1/ma,ma), sides=2)
    t.loss <- floor( ma / 2)
    
    acc <- data.frame( t0=integer(), tn=integer(), v0=numeric(), vn=numeric(), a.mid=numeric() )  # straight segments
    t0 <- t.loss + 1
    v0 <- vn <- v.ma[t0]
    for (t in (t.loss+2):(length(v.ma)-t.loss)) {
        vt <- v.ma[t]
        if ( vt > vn ) {    #acceleration
            vn <- vt 
        } else {            #deceleration
            if ( abs(vn - v0) >= abs(thresh) & t - t0 > 5 ) {               #exceeded threshold 
                t.mid  <- round((t-1 + t0) / 2)
                acc.seg <- data.frame( t0=t0, tn=t-1, v0=v0, vn=vn, a.mid=(v.ma[t.mid+1] - v.ma[t.mid]))
                acc <- rbind( acc, acc.seg)
            }
            t0 <- t
            v0 <- vn <- vt      #start new segment
        }
    }
    return(acc)
}

segment.parse.decel <- function(trip, tmin=1, tmax=nrow(trip), thresh=5) {
    tmin <- max(1, tmin)
    tmax <- min(tmax, nrow(trip))
    ma <- 5  
    
    v.ma <- filter( trip[tmin:tmax, ]$v, rep(1/ma,ma), sides=2)
    t.loss <- floor( ma / 2)
    
    dec <- data.frame( t0=integer(), tn=integer(), v0=numeric(), vn=numeric(), a.mid=numeric() )  # straight segments
    t0 <- t.loss + 1
    v0 <- vn <- v.ma[t0]
    for (t in (t.loss+2):(length(v.ma)-t.loss)) {
        vt <- v.ma[t]
        if ( vt < vn ) {    #deceleration   #NOTE: besides names, this is the only diff for accel code (?)
            vn <- vt 
        } else {            #acceleration
            if ( abs(vn - v0) >= abs(thresh) & t - t0 > 5 ) {               #exceeded threshold 
                t.mid  <- round((t-1 + t0) / 2)
                dec.seg <- data.frame( t0=t0, tn=t-1, v0=v0, vn=vn, a.mid=(v.ma[t.mid+1] - v.ma[t.mid]))
                dec <- rbind( dec, dec.seg)
            }
            t0 <- t
            v0 <- vn <- vt      #start new segment
        }
    }
    return(dec)
}