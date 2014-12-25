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
        current['heading'] <- atan(trip[i,"y.d"] / trip[i,"x.d"]) * 180/pi
        if ( trip[i, "x.d"] < 0 )  {
            if ( trip[i, "y.d"] < 0 ) { 
                current['heading'] <- current['heading'] - 180  
            } else {
                current['heading'] <- current['heading'] + 180
            }
        }
    }
    #print(trip[i,])
    trip.info <- sprintf("distance traveled:%5.1f km\ndirection=%5.0f deg\ncurrent speed=%5.1f km/h\nacceleration=%5.1f m/s^2"
                         ,  current['dist'], current['heading'], current['v'], current['a'] )
    mtext(trip.info, adj=0, cex=.8)
    par(par.orig)
}

