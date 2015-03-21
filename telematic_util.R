if (! exists("segs.all")) segs.all <- data.frame()  # segment info in memory

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
    
    title <- sprintf("Plot of Trip %d (Driver %d)", trip[1,"tripnum"], trip[1,"driver"])
    xlab <- ifelse (header, "x", title)
    
    plot(trip[tmin:tmax, 1:2], type="n", asp=1, xlab=xlab)
    if(header) mtext( title, line=4)
    
    accel <- segment.parse.accel(trip)
    decel <- segment.parse.decel(trip)
    accel.bound <- sort( c( accel$t0, accel$tn))
    decel.bound <- sort( c( decel$t0, decel$tn))
    
    trip.seg <- segment.by.stops(trip)
    jumps <- trip.seg[ which(trip.seg$type == "jump"), ]   #don't plot jumps
    
    markSpeed(trip[tmin,])
    current <- numeric(4); names(current) <- c('dist', 'v', 'a', 'heading')
    
    for (i in (tmin+1):tmax) {
        if (i %in% jumps$t0) next;   # don't plot jumps
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

plotTripOverlay <- function(trip, segs, col="blue", lwd=4, tmin=1, tmax=nrow(trip)) {
    # segs is a data frame with t0,tn columns for each row/segment to be overlayed
    # col and lwd can be overridden by columns of the same name in segs
    tmin <- max(tmin, 1)
    tmax <- min(tmax, nrow(trip))
    
    for (i in 1:nrow(segs)) {
        t0 <- segs[i, "t0"] 
        tn <- segs[i, "tn"]
        if (t0 < tmin | tn > tmax) next   #whole segment must lie within tmin/max range
        
        i.col <- ifelse( "col" %in% colnames(segs), segs[i,"col" ], col)
        i.lwd <- ifelse( "lwd" %in% colnames(segs), segs[i,"lwd" ], lwd)
        
        if (t0 == tn) next  #skip special case (1 point in segment)
        for (t in t0:(tn-1)) {
            lines( trip[t:(t+1), "x"], trip[t:(t+1), "y"], col=i.col, lwd=i.lwd)
        }
    }
}

plotTrip.stopOverlay <- function(trip, stops, col="black", pch=5, cex=2, tmin=1, tmax=nrow(trip)) {
    # stops is a data frame with t0,tn columns for each row/segment to be overlayed
    # col and lwd can be overridden by columns of the same name in segs
    tmin <- max(tmin, 1)
    tmax <- min(tmax, nrow(trip))
    for (i in 1:nrow(stops)) {
        t0 <- stops[i, "t0"]; tn <- stops[i, "tn"]
        if (t0 < tmin | tn > tmax) next   #whole segment must lie within tmin/max range
        
        loc.x <- mean( trip[c(t0,tn), "x"] )
        loc.y <- mean( trip[c(t0,tn), "y"] )
        points( loc.x, loc.y, pch = 5, cex=2)
        text( loc.x, loc.y, labels=i)
    }
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
    tmin <- max(1, tmin)
    tmax <- min(tmax, nrow(trip))
    
    trip.rinfo <- calc.rinfo(trip, tmin=tmin, tmax=tmax)
    plotTrip(trip, tmin=tmin, tmax=tmax)
    with (trip.rinfo, {
        r.switch <- c("red", ifelse( sign(xprod[-length(r)]) == sign(xprod[-1]), "black", "red") )
        points ( type="p", c(trip[tmin:tmax, "x"]), c(trip[tmin:tmax, "y"]), asp=1, pch=20, col=r.switch)
        text( trip[tmin:tmax, "x"], trip[tmin:tmax, "y"], label=round(r),pos=pos.rv)
        text( trip[tmin:tmax, "x"], trip[tmin:tmax, "y"], label=round(trip[tmin:tmax,"v"]), pos=pos.rv+2, col=as.character(v.col))
    })
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



overlaySegmentBorders <- function (trip, t.vec, size=-1, b.col="red", ...) {
    #size is in meters,  -1 will use 5% of the diagonal of the plot area
    bbox <- par('usr') 
    def.size <- sqrt( (bbox[1]-bbox[2])^2 + (bbox[3]-bbox[4])^2 ) / 20                      
    size <- ifelse( size == -1, def.size, size)
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

getTrip <- function(driver.id, trip.id, v.thresh=5, data=NULL) {
    #   load from memory if available
    if (exists ("trips")) {
        trip <- trips[ trips$driver == driver.id & trips$tripnum == trip.id, ]
        if (nrow(trip) > 0) return (trip);
        #############
    }
    
    
    bearing.smooth <<- 0
    bearing.last   <<- 0
    #data.dir is the assumed directory, sequentially numbered .csv files are assumed
    trip.file <- paste0( data.dir, '/', driver.id, '/', trip.id, ".csv")
    
    if ( is.null(data)) {
        trip.xy <- read.csv( trip.file )
    } else {  
        trip.xy <- with(data, data[driver==driver.id & tripnum==trip.id, 3:4] )  
    }
    trip <- getTrip.features(trip.xy, v.thresh)
    
    #add a timepoint for   hyperspace jumps so we can catch them later
    a.thresh <- 50 #  5g's
    jumps <- which( abs(trip$a) > a.thresh)
    i.jump <- 1
    while (length(jumps) >= i.jump) {
        t.jump <- jumps[i.jump]
        #special case - if jump is at end of trip... lop it off
        if (t.jump > nrow(trip) - 2) {
            trip <- trip[1:(nrow(trip)-1), ]
            break;
        }
        
        t.orig <- as.integer(rownames(trip)[t.jump])  # index in raw xy data
        
        # calculate t.adj - the amount of time we need to add to simulate 
        # a straight/steady acceleration path to bridge the jump
        jump.s <- trip[t.jump, "v"]   # distance (=velocity for 1 sec)
        jump.v0 <- trip[t.jump-1, "v"] #velocity prior to jump
        jump.vn <- trip[t.jump+1, "v"] #velocity after jum
        jump.v_avg <- mean( c(jump.v0, jump.vn) )
        t.adj <- as.integer( jump.s / jump.v_avg )
        
        xy.n <- as.matrix( trip.xy[t.orig, ]   )
        xy.0 <- as.matrix( trip.xy[t.orig-1, ] )
        xy.adj <- (1:t.adj %*% ( xy.n - xy.0 ) / t.adj) + rep(xy.0, each=t.adj)
        
        #insert the interpolated points into the trip and recalc the entire trip.
        trip.xy <- rbind(           trip.xy[1:(t.orig-1), ] 
                          , xy.adj, trip.xy[(t.orig+1):nrow(trip.xy), ])
        trip <- getTrip.features(trip.xy, v.thresh)
        
        i.jump <- i.jump +1
        jumps <- which( abs(trip$a) > a.thresh)        
    }
    trip$driver <- driver.id
    trip$tripnum <- trip.id
    
    return(trip)
}

getTrip.features <- function(trip, v.thresh) {
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
    trip
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

#MIN_SEGMENT_LENGTH <- 30  # replaced by tlen.min
segment.parse.bearing <- function(trip, tmin=1, tmax=nrow(trip), zone=3, tlen.min=30) {
#cat("spb: ", tmin, tmax, zone, tlen.min, "\n")
    tmin <- max(1, tmin)
    tmax <- min(tmax, nrow(trip))
    ma <- 5  
    b.ima.full <- filter( diff(trip$bearing, lag=1), rep(1/ma,ma), sides=2)
    b.ima <- b.ima.full[tmin:tmax]
    
    in.zone <- ifelse( is.na(b.ima[1]), FALSE, abs(b.ima[1]) < zone )
    t.start <- t.end <- ifelse( in.zone, tmin, 0)
    ss <- data.frame( t0=integer(), tlen=integer(),
                      v.min=numeric(), v.max=numeric(), v.mid=numeric() )  # straight segments
    for (i in 2:length(b.ima)) {
        b.ima_i  <- b.ima[i]  # for debug listing
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
                if (seg.len >= tlen.min) {
                    ss <- rbind( ss, segment.parse.bearing.write ( trip, t.start, t.end))
                }
                t.start <- 0
            }
        }
    }
    if (t.start > 0) {
        seg.len <- t.end - t.start
        if (seg.len >= tlen.min) {
            ss <- rbind( ss, segment.parse.bearing.write ( trip, t.start, t.end))
        }
    }
        
    return(ss)
}

segment.parse.bearing.write <-function ( trip, t.start, t.end) {
    seg.len <- t.end - t.start
    seg.row <- data.frame( t0=t.start, tlen=seg.len)
    seg.row$v.min <- min( trip[t.start:t.end, ]$v)
    seg.row$v.max <- max( trip[t.start:t.end, ]$v)
    seg.row$v.mid <- min( trip[round((t.start+t.end)/2), ]$v)
    seg.row
}

interpolate.v <- function( x1, y1, x2, y2, x) { y <- y1 + (y2-y1) * (x-x1) / (x2-x1); y }
a.test.accel <- function(v1, v2) v1 > v2
a.test.decel <- function(v1, v2) v1 < v2
segment.parse.accel <- function(trip, tmin=1, tmax=nrow(trip), thresh=5, ma=5, a.test = a.test.accel ) {
    tmin <- max(1, tmin)
    tmax <- min(tmax, nrow(trip))
    
    trip.seg <- segment.by.stops(trip)
    jumps <- trip.seg[ which(trip.seg$type == "jump"), ]
    
    v.ma <- filter( trip[tmin:tmax, ]$v, rep(1/ma,ma), sides=2)
    t.loss <- floor( ma / 2)
    
    acc <- data.frame( t0=integer(), tn=integer(), v0=numeric(), vn=numeric()
                     , a.mid=numeric()  #acceleration at t_mid
                     , a.v5=numeric()   #acceleration when speed= 5m/s
                     , a.v10=numeric()  #acceleration when speed= 10m/s
                     , a.v15=numeric()  #acceleration when speed= 15m/s
                     , a.v20=numeric()  #acceleration when speed= 20m/s
                     , dist=numeric()  
                     ) 
    a.v5 <- a.v10 <- a.v15 <- a.v20 <- NA
    t0 <- t.loss + 1
    v0 <- vn <- v.ma[t0]
    dist <- 0
    t.lower <- t.loss+2
    t.upper <- length(v.ma)-t.loss
    an <- v.ma[t.lower+1] - v.ma[t.lower]
    for (t in t.lower:t.upper) {
            vt <- v.ma[t]
        if ( a.test(vt, vn) & t < t.upper & ! t %in% jumps$t0) {    #acceleration
            at <- vt - vn
            if ( vt >=  5 & vn <  5 ) a.v5  <- interpolate.v ( vn, an, vt, at,  5)
            if ( vt >= 10 & vn < 10 ) a.v10 <- interpolate.v ( vn, an, vt, at,  10)
            if ( vt >= 15 & vn < 15 ) a.v15 <- interpolate.v ( vn, an, vt, at,  15)
            if ( vt >= 20 & vn < 20 ) a.v20 <- interpolate.v ( vn, an, vt, at,  20)
            dist <- dist + vt  # this works because t_interval = 1 sec
            vn <- vt 
            an <- at
        } else {            #deceleration
            if ( abs(vn - v0) >= abs(thresh) & t - t0 > 5 ) {               #exceeded threshold 
                if ( a.test(vt, vn) & t == t.upper)  t <- t+1  #special case end of trip during segment
                t.mid  <- round((t-1 + t0) / 2)
                acc.seg <- data.frame( t0=t0, tn=t-1, v0=v0, vn=vn
                                     , a.mid=(v.ma[t.mid+1] - v.ma[t.mid])
                                     , a.v5, a.v10, a.v15, a.v20
                                     , dist
                                     )
                acc <- rbind( acc, acc.seg)
            }
            t0 <- t
            v0 <- vn <- vt      #start new segment
            dist <- 0
            a.v5 <- a.v10 <- a.v15 <- a.v20 <- NA
        }
    }
    return(acc)
}

#deprecate segment.parse.decel
segment.parse.decel <- function (trip, tmin=1, tmax=nrow(trip), thresh=5, ma=5) {    
    segment.parse.accel(trip, tmin=tmin, tmax=tmax, thresh=thresh, ma=ma, a.test=a.test.decel) }
xsegment.parse.decel <- function(trip, tmin=1, tmax=nrow(trip), thresh=5, ma=5) {  
    tmin <- max(1, tmin)
    tmax <- min(tmax, nrow(trip))
    
    v.ma <- filter( trip[tmin:tmax, ]$v, rep(1/ma,ma), sides=2)
    t.loss <- floor( ma / 2)
    
    dec <- data.frame( t0=integer(), tn=integer(), v0=numeric(), vn=numeric(), a.mid=numeric() )  # straight segments
    t0 <- t.loss + 1
    v0 <- vn <- v.ma[t0]
    t.lower <- t.loss+2
    t.upper <- length(v.ma)-t.loss
    for (t in t.lower:t.upper) {
        vt <- v.ma[t]
        if ( vt < vn & t < t.upper) {    #deceleration   #NOTE: besides names, this is the only diff for accel code (?)
            vn <- vt 
        } else {            #acceleration
            if ( abs(vn - v0) >= abs(thresh) & t - t0 > 5 ) {               #exceeded threshold 
                if ( vt < vn & t == t.upper)  t <- t+1  #special case end of trip during segment
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

segment.parse.stops <- function(trip, tmin=1, tmax=nrow(trip), thresh.stop=1, thresh.roll=2) {
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


validate.turn <- function ( seg, r.thresh=20, t.thresh=2 ) {
    # minimum of 2 consecutive points under the threshold radius
    under <- seg$r <= r.thresh
    maxrun <- run <- 0
    for (i in under) { 
        run <- ifelse (i , run + 1, 0)
        maxrun <- max( maxrun, run)
    }
    if (maxrun < t.thresh) return (FALSE)
    
    return(TRUE)
}

calc.rinfo <- function ( trip, tmin=1, tmax=nrow(trip)) {
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
            r[ t+(1:2)] <- 9999   #set the r to ~ inf
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
    #fix r values that are NaN with pseudo inf.
    r[ is.nan(r) ] <- 999999
    return( data.frame( origin=origin, r=r, pos.rv=pos.rv, xprod=xprod, v.col=v.col ))
}

segment.parse.curves <- function(trip, tmin=1, tmax=nrow(trip)) {
    segment.parse.curve.gen (trip, tmin, tmax, thresh=1000) 
}

segment.parse.turns <- function(trip, tmin=1, tmax=nrow(trip)) {
    segment.parse.curve.gen (trip, tmin, tmax, thresh=20) 
}

    
segment.parse.curve.gen <- function(trip, tmin=1, tmax=nrow(trip), r.thresh=20, t.thresh=2) {
    # IMPORTANT: this function assumes that parse.stop segments have been removed
    tmin <- max(1, tmin)
    tmax <- min(tmax, nrow(trip))
    
    #rules:  4 points with identical curvature (direction) under R=thresh m
    
    turn <- data.frame( t0=integer(), tn=integer())  # turn segments
    rinfo.trip <- calc.rinfo(trip)  # augmented radii info (for full trip)
    
    t0 <- tmin
    turning <- FALSE
    xprod.last <- 0   
    for (t in (tmin+1):tmax) {  #cat(t,".")
                                
        rinfo <- rinfo.trip[t, ]
        
        if (turning) {
            #dd="::"; cat("\n", xprod.last, dd,rinfo$xprod, dd, rinfo$r, dd, r.thresh, "\n")
            if ( xprod.last * rinfo$xprod < 0 | rinfo$r > r.thresh  ) { #came out of turn
                tn <- t-1
                turn.seg <- data.frame( t0=t0, tn=tn)
                turn.info <- cbind(trip[t0:tn, ], rinfo.trip[t0:tn, ])
                if (validate.turn( turn.info, r.thresh=r.thresh, t.thresh )) {
                    #cat ("good turn:\n"); print(turn.seg)
                    turn <- rbind(turn, turn.seg)
                }
                turning <- FALSE
                t0 <- t
            }
        } else {
            if ( rinfo$r <= r.thresh )  {   #start of a new turn (assume then check)
                turning <- TRUE
                t0 <- t
            }
        }
        xprod.last <- rinfo$xprod
    }
    return(turn)
}

segment.clean.points <- function( s ) {
    #s is the data frame of segments.  clean.points will set all segments of length 1 to type="point"
    for (i in 1:nrow(s))  s[i,"type"] <- ifelse( s[i,"t0"] == s[i,"tn"] & is.na(s[i,"type"]), "x.point", s[i,"type"])
    s
}

# abondoned this in lieu of the segment.parse.bearing since the local radii fluxuate too erratically
x.segment.parse.straight <- function(trip, tmin=1, tmax=nrow(trip), r.thresh=500, t.thresh=4) {
    # IMPORTANT: this function assumes that parse.stop segments have been removed
    tmin <- max(1, tmin)
    tmax <- min(tmax, nrow(trip))
    
    #rules:  >= t.thresh points with over R=thresh m
    
    straight <- data.frame( t0=integer(), tn=integer())  # turn segments
    rinfo.trip <- calc.rinfo(trip)  # augmented radii info (for full trip)
    
    t0 <- tmin
    turning <- FALSE
    for (t in (tmin+1):tmax) {  #cat(t,".")
        rinfo <- rinfo.trip[t, ]
        
        if (! turning) {
            #dd="::"; cat("\n", xprod.last, dd,rinfo$xprod, dd, rinfo$r, dd, r.thresh, "\n")
            if ( rinfo$r < r.thresh  ) { # started a turn
                tn <- t-1
                straight.seg <- data.frame( t0=t0, tn=tn)
                straight.info <- cbind(trip[t0:tn, ], rinfo.trip[t0:tn, ])
                if ( (tn - t0) >= t.thresh ) {
                    #cat ("good straight:\n"); print(straight.seg)
                    straight <- rbind(straight, straight.seg)
                }
                turning <- TRUE
                t0 <- t
            }
        } else {
            if ( rinfo$r >= r.thresh )  {   #start of a new straight 
                turning <- FALSE
                t0 <- t
            }
        }
    }
    return(straight)
}


segment.by.stops <- function (trip, thresh.stop=1, thresh.roll=2) {
    trip.seg <- data.frame( id=1, t0=1, tn=nrow(trip), type=NA, type.id=NA )
    
    trip.seg <- segment.remove.jumps(trip, trip.seg)
    
    segs.unk <- trip.seg[ is.na(trip.seg$type), ]  # using the type=NA to determine the segments that need to be parsed
    
    
    for (seg in segs.unk$id) {
        seg.data <- segs.unk[segs.unk$id==seg, ]
        t.beg <- seg.data$t0 
        t.fin <- seg.data$tn
        
        stops <- segment.parse.stops(trip, tmin=t.beg, tmax=t.fin, thresh.stop=1, thresh.roll=2)
        if (exists("trip.stops")) {
            trip.stops <- rbind( trip.stops, stops)
        } else {
            trip.stops <- stops
        }
    }
        
    if (nrow(trip.stops) > 0 ) {
        stop.id <- 1:nrow(trip.stops)
        trip.stops <- cbind( stop.id, trip.stops)
   
    
        t <- 1
        i.seg <- orig.seg <- nrow(trip.seg)   # last segment written 
        for (i in stop.id) {
            t0 <- trip.stops[i, "t0"]; tn <- trip.stops[i, "tn"]
            if (t0 > t) { 
                i.seg <- i.seg + 1 
                trip.seg <- rbind (trip.seg, data.frame( id=i.seg, t0=t, tn=t0-1, type=NA, type.id=NA ))
            }
            i.seg <- i.seg + 1
            trip.seg <- rbind (trip.seg, data.frame( id=i.seg, t0=t0, tn=tn, type="stop", type.id=i ))
            t <- tn + 1
        }
        t.fin <- nrow(trip)
        if (t <= t.fin) {   # segment after the last stop (if any)
            i.seg <- i.seg +1
            trip.seg <- rbind (trip.seg, data.frame( id=i.seg, t0=t, tn=t.fin, type=NA, type.id=NA))
        }
        
        
        trip.seg[ orig.seg, "type"] <- "x.split"
    }
    
    segment.clean.points(trip.seg)
}



segment.by.turns <- function ( trip, trip.seg, r.thresh=20 ) {
    segment.by.curve.gen( trip, trip.seg, r.thresh=r.thresh, ctype="turn")
}

segment.by.curve.gen <- function ( trip, trip.seg, r.thresh=20, t.thresh=2, zone=4, ctype="gen.curve") {
    # The string value in ctype will be used for the segment type
    # if ctype contains the string "straight" ...parse.straight is used otherwise ...parse.curve
    
    segs.unk <- trip.seg[ is.na(trip.seg$type), ]  # using the type=NA to determine the segments that need to be parsed
    
    for (seg in segs.unk$id) {
        seg.data <- segs.unk[segs.unk$id==seg, ]
        t.beg <- seg.data$t0 
        t.fin <- seg.data$tn
        if (grepl ("straight", ctype) ) {
            straights <- segment.parse.bearing(trip, tmin=t.beg, tmax=t.fin, zone=zone, tlen.min=t.thresh)
            turns <- with(straights, data.frame( t0=t0, tn=t0+tlen )) 
        } else {
            turns <- with(seg.data, segment.parse.curve.gen(trip, tmin=t.beg, tmax=t.fin, r.thresh=r.thresh, t.thresh=t.thresh))
        }
        if (nrow(turns) > 0) {
            t <- t.beg
            i.seg <- nrow(trip.seg)   # last segment written 
            for (i in 1:nrow(turns)) {  
                t0 <- turns[i, "t0"]; tn <- turns[i, "tn"]  # t0,tn are beginning and end timepoints of turn
                if (t0 > t) {                               # segment in front of the first turn (if any)
                    i.seg <- i.seg + 1 
                    trip.seg <- rbind (trip.seg, data.frame( id=i.seg, t0=t, tn=t0-1, type=NA, type.id=NA ))
                }
                i.seg <- i.seg + 1
                # the turn id will be encoded so that id/1000 = segment and id%1000 = trip
                trip.seg <- rbind (trip.seg, data.frame( id=i.seg, t0=t0, tn=tn, type=ctype, type.id=i+(1000*seg) ))
                t <- tn + 1
            }
            
            if (t <= t.fin) {   # segment after the last turn (if any)
                i.seg <- i.seg +1
                trip.seg <- rbind (trip.seg, data.frame( id=i.seg, t0=t, tn=t.fin, type=NA, type.id=NA))
            }
            
            trip.seg[ trip.seg$id == seg  , "type"] <- "x.split"  #mark this segment as split
            
            #marry the turns trip segment to a master list for the trip
            turns$id <- 1:nrow(turns) + seg * 1000
            if ( exists( "trip.turns")) { 
                trip.turns <- rbind( trip.turns, turns)
            } else {
                trip.turns <- turns
            }
            
        }
    }
    segment.clean.points(trip.seg)
}

getAccels <- function(driver.id = 2591, plot.trip=TRUE) {
    for (trip.id in 1:200) {
        cat ("trip ", trip.id, "...")
        trip <- getTrip( driver.id, trip.id, v.thresh=2 )
        
        if (plot.trip == TRUE) plotTrip(trip)
        acc <- segment.parse.accel(trip)
        if (nrow(acc) <= 0) next
        
        segs <- getTripSegments(trip)
        segs <- segs[ !grepl("^x\\.", segs$type), ]  #remove the dead elements

        curves <- segs[grepl("^curve",segs$type), ]
        straights <- segs[grepl("^straight",segs$type), ]
        turns <- segs[grepl("^turn",segs$type), ]
        stops <- segs[grepl("^stop",segs$type), ]
               
        a.seginfo <- data.frame ( snip.t0=integer(), snip.tn=integer(), n.snip=integer()
                                  , first.seg=character(), n.strt=integer()
                                  , strt.gap=integer(), strt.overrun=integer())
        for (i in 1:nrow(acc)) { cat ("in acc", i, "\n")
            seg.i <- acc[i, ]
            seg.beg <- rev(which (segs$t0 <= seg.i$t0))[1]   #this is the first seg prior to the accel
            seg.fin <- which(segs$tn >= seg.i$tn)[1]  #this segment contains the last point
            snip <- segs[seg.beg:seg.fin, ]
            n.snip <- nrow(snip)
            snip
            t1 <- segs[seg.beg, "t0"]
            t2 <- segs[seg.fin, "tn"]
            seg.straights <- which( snip$type == "straight")
            n.straights <- length(seg.straights)
            if (n.straights > 0) { 
                straight.start.gap <- ifelse( snip[1, "type"] == "straight", 0, snip[seg.straights[1], "t0"] - seg.i$t0 )
                straight.overrun <- snip[seg.straights[n.straights], "tn"] - seg.i$tn  # negative is we don't end on a straight
            } else {  # no straights (shouldn't happen I thnk)
                straight.start.gap <- -1
                straight.overrun <- -1
            } 
            a.seginfo <- rbind( a.seginfo
                                , data.frame( snip.t0=snip[1,"t0"]
                                              , snip.tn=snip[n.snip,"tn"], n.snip
                                              , first.seg=snip[1,"type"], n.strt=n.straights
                                              , strt.gap=straight.start.gap, strt.overrun=straight.overrun))
        }
        acc <- cbind(acc, a.seginfo)
        
        acc$driver <- driver.id
        acc$trip <- trip.id
        cat ("... ", nrow(acc), " acceleration segments added\n")
        print(acc)
        #acc.all <- ifelse( trip.id == 1, acc, rbind(acc.all, acc))
        if (exists("acc.all")) { 
            acc.all <- rbind(acc.all, acc)
        } else {
            acc.all <- acc
        }
    }
    acc.all
}

get.fname <- function(dir, driver.id, type) sprintf("%s/driver_%s_%s.RData", dir, driver.id, type)

getTripSegments <- function(trip, lookup=TRUE) {
    #active = true to remove 'dead segments'
    driver.id <- trip[1,]$driver
    trip.id <- trip[1,]$tripnum
    
    #first see if its in memory
    if (lookup & nrow(segs.all) > 0) {
        trip.seg <- with(segs.all, segs.all[ driver == driver.id & tripnum == trip.id, ])
        if (nrow(trip.seg) > 0 ) return (trip.seg)      #  <--- RETURN
    }
    #next see if we have it on disk
    if (lookup & exists("segs.dir") ) { 
        driver.segs <- paste0( segs.dir, "/", dir(segs.dir) )
        fname <- get.fname( segs.dir, driver.id, "segs" )
        driver.data.exists <- any( driver.segs == fname )
        
        if (driver.data.exists) {
            load(file=fname)
            trip.seg <- trip.seg[ trip.seg$tripnum==trip.id, ]
            return(trip.seg)#                            <--- RETURN
        }
    }
    
    #last resort -- calculate
    trip.seg <- segment.by.stops(trip)
    trip.seg <- segment.by.turns( trip, trip.seg)
    trip.seg <- segment.by.curve.gen( trip, trip.seg, r.thresh=300, t.thresh=4, ctype="curve")
    trip.seg <- segment.by.curve.gen( trip, trip.seg, t.thresh=3, zone=4, ctype="straight")
    trip.seg <- trip.seg[ order(trip.seg$t0), ]  #return an ordered data frame
    trip.seg$driver <- driver.id
    trip.seg$tripnum <- trip.id
    
    if (exists("segs.all") ) {
        if (nrow(segs.all) == 0) {
            segs.all <<- trip.seg
        } else {
            segs.all <<- rbind(segs.all, trip.seg)
        }
    }
    
    trip.seg
    
    
#     segs <- trip.seg[!grepl("^x", trip.seg$type), ]
#     segs <- segs[ order(segs$t0),]
#     segs
}

segment.remove.jumps <- function ( trip, trip.seg, a.thresh=50 ) {
    # remove hyperspace jumps - default is 3 standard deviations away from mean a
    
    segs.unk <- trip.seg[ is.na(trip.seg$type), ]  # using the type=NA to determine the segments that need to be parsed
    
    for (seg in segs.unk$id) {
        seg.data <- segs.unk[segs.unk$id==seg, ]
        t.beg <- seg.data$t0 
        t.fin <- seg.data$tn
        jumps <- which( abs(trip[t.beg:t.fin, "a"]) > a.thresh)
        i.jump <- 0
        seg.jumps <- data.frame( t0=integer(), tn=integer(), dist=numeric(), dtheta=numeric())
        for ( jump in jumps) {
            i.jump <- i.jump + 1
            t.jump <- t.beg + jump - 1
            
            i.seg <- nrow(trip.seg) +1 # next segment
            if (t.jump > t.beg) {
                trip.seg <- rbind (trip.seg, data.frame( id=i.seg, t0=t.beg, tn=t.jump-1, type=NA, type.id=NA ))
                i.seg <- i.seg + 1
            }
            # the jump id will be encoded so that id/1000 = segment and id%1000 = jump
            trip.seg <- rbind (trip.seg, data.frame( id=i.seg, t0=t.jump, tn=t.jump, type="jump", type.id=i.jump+(1000*seg) ))
            i.seg <- i.seg + 1
            seg.jumps <- rbind( seg.jumps, data.frame(t0=t.jump, tn=t.jump
                                                      , dist=trip[t.jump,"v"], dtheta=diff(trip[(t.jump-1):t.jump,"bearing"]) ))
            
            if (t.jump < t.fin) {
                trip.seg <- rbind (trip.seg, data.frame( id=i.seg, t0=t.jump+1, tn=t.fin, type=NA, type.id=NA ))
                i.seg <- i.seg + 1                    
            }
        }
        if (length(jumps) > 0) {        
            trip.seg[ trip.seg$id == seg  , "type"] <- "x.split"  #mark this segment as split
            
            #marry the jumps trip segment to a master list for the trip  #not doing anything with this yet
            seg.jumps$id <- 1:nrow(seg.jumps) + seg * 1000
            if ( exists( "trip.turns")) { 
                trip.jumps <- rbind( trip.turns, seg.jumps)
            } else {
                trip.jumps <- seg.jumps
            }
            
        }
    }
    segment.clean.points(trip.seg)
}
