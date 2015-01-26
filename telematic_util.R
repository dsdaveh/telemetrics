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

in.zone <- function(t, b) {
    if (length(b) <= 0) return(FALSE)
    b <- sort(b)
    if (t < b[1]) return (FALSE)
    for (i in 2:length(b)) {
        if (i %% 2 == 0) {
            if ( t <= b[i] ) return(TRUE)
        } else {
            if ( t <= b[i] ) return(FALSE)
        }
    }   
    return (FALSE)
}

plotTrip <- function(trip, v.mark=5, t.mark=100, tmin=1, tmax=nrow(trip), b.marks=NULL, header=TRUE) {
    tmin <- max(tmin, 1)
    tmax <- min(tmax, nrow(trip))
    v.inc <<- v.mark
    mar.top <- ifelse (header, 5, 2)
    par.orig <- par(mar=c(5,4,mar.top,2))
    plot(trip[tmin:tmax, 1:2], type="n", asp=1)
    if(header) mtext("Plot of Route", line=4)
    
    accel <- segment.parse.accel(trip)
    decel <- segment.parse.decel(trip)
    accel.bound <- sort( c( accel$t0, accel$tn))
    decel.bound <- sort( c( decel$t0, decel$tn))
    
    markSpeed(trip[tmin,])
    current <- numeric(4); names(current) <- c('dist', 'v', 'a', 'heading')
    
    for (i in (tmin+1):tmax) {
        if (i %% t.mark == 0 ) markTime( trip[i, ], i) 
        i.v <- trip[i,"v"]
        i.a <- trip[i,"a"]
        thk <- round( i.v / 2)
        
        color <- ifelse ( in.zone( i, accel.bound), "green",
                 ifelse ( in.zone( i, decel.bound), "red",   "orange"))
        
        lines( trip[c(i,i-1), 1], trip[c(i,i-1), 2], type="l", lwd=thk, col=color)
        if( i.v > v.up | i.v < v.dn ) markSpeed( trip[i, ])
        
        current['dist'] <- current['dist'] + i.v/1000 #  dist.seg = speed * time (=1sec)
        current['v'] <- i.v * 3.6
        current['a'] <- i.a
        bearing <- calcBearing( trip[i, ])
        current['heading'] <- ifelse( is.nan(bearing), bearing.last, bearing) %% 360
    }
    #print(trip[i,])
    trip.info <- sprintf("distance traveled:%5.2f km\ndirection=%5.0f deg\ncurrent speed=%5.1f km/h\nacceleration=%5.1f m/s^2"
                         ,  current['dist'], current['heading'], current['v'], current['a'] )
    if(header) mtext(trip.info, adj=0, cex=.8)
    
    if (length(b.marks > 0)) overlaySegmentBorders( trip, b.marks )
    
    par(par.orig)
}


crossVec <- function (x,y) x[1]*y[2]-x[2]*y[1]

calcRad.lineEq <- function( mx, pt ) {
    # given a 2D vector mx passing thru pt, return the coefficients (m,b) of the line  y=mx+b
    if (mx[1] == 0) mx[1] <- .000001    #prevent div by 0
    m = mx[2]/mx[1]
    b = pt[2] - m * pt[1]
    return( c(m, b))
}

plotTrip.r <- function( trip, tmin=1, tmax=nrow(trip)) {
    rotate.90 <- matrix( c(0, 1, -1, 0), ncol=2)
    t0 <- ifelse(tmin < 1, 1, tmin)
    tn <- ifelse(tmax > nrow(trip), nrow(trip), tmax)
    trip <- trip[t0:tn, ]
    
    origin <- matrix( rep(NA, nrow(trip) * 2), ncol=2 )
    r <- numeric( nrow(trip))
    pos.rv <- rep(1, nrow(trip)) 
    xprod <- numeric( nrow(trip))
    v.col <- rep("black", nrow(trip))
    for (t in 1:(nrow(trip)-2)) {
        if ( any( trip[(t+(1:2)), "v"] == 0 ) ) {   #indicates a speed=0 : break up segment
            origin[ t+(1:2), 1:2 ] <- NA
            r[ t+(1:2)] <- NA
        } else {
            v1 <- as.numeric(c( trip[t+1, c("x.d","y.d")])) / trip[t+1, "v"]  #heading unit vector
            v2 <- as.numeric(c( trip[t+2, c("x.d","y.d")])) / trip[t+2, "v"]
            xprod[t+1] <- crossVec( v1, v2)
            mid1 <- colMeans( trip[ t   :(t+1), c("x","y")])
            mid2 <- colMeans( trip[(t+1):(t+2), c("x","y")])
            v1.90 <- rotate.90 %*% v1
            v2.90 <- rotate.90 %*% v2
            eq1 <- calcRad.lineEq( v1.90, mid1)
            eq2 <- calcRad.lineEq( v2.90, mid2)
            origin[t+1, 1] <- (eq2[2] - eq1[2]) / (eq1[1] - eq2[1])     #  x = b2-b1 / m1-m2
            origin[t+1, 2] <- eq1[1] * origin[t+1, 1] + eq1[2]               #  y = mx + b
            r[t+1] <- sqrt(sum( (origin[t+1,] - mid1)^2) )
            #        cat("t,r,xprod", t,  r[t+1], xprod[t+1], " \n")
            bearing <- calcBearing( trip[t+1, ], smooth=FALSE )
            pos.rv[t+1] <- ifelse( abs(bearing < 45) | abs(bearing) > 135 , 1, 2)
            v.col[t+1] <- ifelse( trip[t+1, "a"] > 0, "green" , "red")
        }
    }    
    
    plotTrip(trip)
    r.switch <- c("red", ifelse( sign(xprod[-length(r)]) == sign(xprod[-1]), "black", "red") )
    points ( type="p", c(trip[, "x"]), c(trip[, "y"]), asp=1, pch=20, col=r.switch)
    text( trip[, "x"], trip[, "y"], label=round(r),pos=pos.rv)
    text( trip[, "x"], trip[, "y"], label=round(trip$v), pos=pos.rv+2, col=v.col)
}


calcAngle <- function( a, b) acos( sum(a*b) / ( sqrt(sum(a * a)) * sqrt(sum(b * b)) ) )

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

plotTripSegment <- function(trip, tmin=1, tmax=tmin+100, ma=5, b.marks=NULL, b.col="red", ...) {
    tmin <- max(tmin, 1)
    tmax <- min(tmax, nrow(trip))
    par.orig <- par(mfrow=c(1,2))

    plotTrip(trip, tmin=tmin, tmax=tmax, header=TRUE, ...)
    if (length(b.marks) > 0) overlaySegmentBorders( trip, b.marks, b.col=b.col )
    
    plotTripSegment.speed( trip, tmin=tmin, tmax=tmax, ma=ma, b.marks=b.marks, b.col=b.col )
    
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

getTripProfile <- function(trip ) {
    
    prof <- data.frame( trip.len=nrow(trip) ) 
    prof$trip.dist <- sum(trip$v)    #requires t interval = 1
    prof$speed.avg <- mean(trip$v)   # same as dist/len ?
    prof$speed.max <- max(trip$v)
    
    straights <- segment.parse.bearing(trip)
    prof$ss.n   <- nrow(straights)
    if ( nrow(straights) > 0) {
        prof$ss.len.min <- min(straights$tlen)
        prof$ss.len.max <- max(straights$tlen)
        prof$ss.len.avg <- mean(straights$tlen)
        prof$ss.len.sd  <- sd(straights$tlen)
        range <- ( straights$v.max - straights$v.min )
        prof$ss.range.avg <- mean( range) 
        prof$ss.range.sd <- sd( range)
        prof$ss.vmid.avg <- mean( straights$v.mid) 
        prof$ss.vmid.sd <- sd( straights$v.mid)
    } else {
        prof$ss.len.min <- prof$ss.len.max <- prof$ss.len.avg <- prof$ss.len.sd  <- NA
        prof$ss.range.avg <- prof$ss.range.sd <- prof$ss.vmid.avg <- prof$ss.vmid.sd <- NA
    }
    acc <- segment.parse.accel(trip)
    prof$acc.n <- nrow(acc)
    if ( nrow(acc) > 0) {
        len <- acc$tn - acc$t0
        prof$acc.len.min <- min( len ) 
        prof$acc.len.max <- max( len )
        prof$acc.len.avg <- mean( len )
        prof$acc.len.sd  <- sd( len )
        range <- ( acc$vn - acc$v0 )
        prof$acc.range.avg <- mean( range) 
        prof$acc.range.sd <- sd( range)
        prof$acc.v0.avg <- mean( acc$v0) 
        prof$acc.v0.sd <-  sd( acc$v0)
        prof$acc.vn.avg <- mean( acc$vn) 
        prof$acc.vn.sd <-  sd( acc$vn)
        prof$acc.amid.avg <- mean( acc$a.mid) 
        prof$acc.amid.sd <-  sd( acc$a.mid)
    } else {
        prof$acc.len.min <- prof$acc.len.max <- prof$acc.len.avg <- prof$acc.len.sd  <- NA
        prof$acc.range.avg <- prof$acc.range.sd <- prof$acc.v0.avg <- prof$acc.v0.sd <-  NA
        prof$acc.vn.avg <- prof$acc.vn.sd <-  prof$acc.amid.avg <- prof$acc.amid.sd <-  NA
    }
    
    dec <- segment.parse.decel(trip)
    prof$dec.n <- nrow(dec)
    if ( nrow(dec) > 0) {
        len <- dec$tn - dec$t0
        prof$dec.len.min <- min( len ) 
        prof$dec.len.max <- max( len )
        prof$dec.len.avg <- mean( len )
        prof$dec.len.sd  <- sd( len )
        range <- ( dec$vn - dec$v0 )
        prof$dec.range.avg <- mean( range) 
        prof$dec.range.sd <- sd( range)
        prof$dec.v0.avg <- mean( dec$v0) 
        prof$dec.v0.sd <-  sd( dec$v0)
        prof$dec.vn.avg <- mean( dec$vn) 
        prof$dec.vn.sd <-  sd( dec$vn)
        prof$dec.amid.avg <- mean( dec$a.mid) 
        prof$dec.amid.sd <-  sd( dec$a.mid)
    } else {
        prof$dec.len.min <- prof$dec.len.max <- prof$dec.len.avg <- prof$dec.len.sd  <- NA
        prof$dec.range.avg <- prof$dec.range.sd <- prof$dec.v0.avg <- prof$dec.v0.sd <-  NA
        prof$dec.vn.avg <- prof$dec.vn.sd <-  prof$dec.amid.avg <- prof$dec.amid.sd <-  NA
    }
    return(prof)
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
    ss <- data.frame( t0=integer(), tlen=integer(),
                      v.min=numeric(), v.max=numeric(), v.mid=numeric() )  # straight segments
    for (i in 2:length(b.ima)) {
        t <- tmin + i -1
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
                    seg.row <- data.frame( t0=t.start, tlen=seg.len)
                    seg.row$v.min <- min( trip[t.start:t.end, ]$v)
                    seg.row$v.max <- max( trip[t.start:t.end, ]$v)
                    seg.row$v.mid <- min( trip[round((t.start+t.end)/2), ]$v)
                    ss <- rbind(ss, seg.row)
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

segment.parse.accel <- function(trip, tmin=1, tmax=nrow(trip), thresh=5, ma=5) {
    tmin <- max(1, tmin)
    tmax <- min(tmax, nrow(trip))
    
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

segment.parse.decel <- function(trip, tmin=1, tmax=nrow(trip), thresh=5, ma=5) {
    tmin <- max(1, tmin)
    tmax <- min(tmax, nrow(trip))
    
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

segment.parse.stops <- function(trip, tmin=1, tmax=nrow(trip), thresh.stop=1, thresh.roll=2, ma=5) {
    tmin <- max(1, tmin)
    tmax <- min(tmax, nrow(trip))
    
    stop <- data.frame( t0=integer(), tn=integer())  # stop segments
    t0 <- 1
    thresh <- thresh.stop
    stopped <- FALSE
    for (t in tmin:tmax) {
        vt <- trip[t, "v"]
        if ( stopped ) {
            if ( vt > thresh.roll | t >= tmax) { # no longer stopped OR end of trip
                #                cat ("end stop", t, vt, t0, "\n")
                stop.seg <- data.frame( t0=t0, tn=t-1)        
                stop <- rbind( stop, stop.seg)
                stopped <- FALSE
            }
            
        } else {
            if ( vt <= thresh.stop ) {    #begin of new stop segment
                t0 <- t      
                #                cat ("start stop", t, vt, t0, "\n")
                stopped <- TRUE
            }
        }
    }
    return(stop)
}