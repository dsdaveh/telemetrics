library(knitr)
library(reshape)
source("telematic_util.R")

data.dir <- "c:/NO_BACKUP/kaggle/telematics/drivers"

drivers.dir <- dir(data.dir)
set.seed(2349)

driver.id <- 2591

segment.summary <- function ( segs , trip ) {
    segs$tlen <- segs$tn - segs$t0 + 1
    unk.segs <- is.na( segs$type)
    segs[unk.segs, "type"] <- "uncategorized"
    stats <- data.frame(table(segs$type))
    tlen.sum <- aggregate( segs$tlen, list(segs$type), sum )
    stats <- cbind( stats, tlen.sum$x, round(tlen.sum$x/nrow(trip)*100, 2))
    colnames(stats) <- c("Type", "count", "total_t", "pct_of_trip")
    loss <- nrow(trip) - sum( stats[,"total_t"])  
    stats <- rbind(stats, data.frame(Type="loss", count=NA, total_t= loss, pct_of_trip= round(100*loss/nrow(trip), 2 )))
    stats
}

trip.id=30

trip <- getTrip( driver.id, trip.id, v.thresh=2 )

trip.seg <- segment.by.stops(trip)
trip.seg <- segment.by.turns( trip, trip.seg)
trip.seg <- segment.by.curve.gen( trip, trip.seg, r.thresh=300, t.thresh=4, ctype="curve")
trip.seg <- segment.by.curve.gen( trip, trip.seg, t.thresh=3, zone=4, ctype="straight")

segs <- trip.seg[!grepl("^x", trip.seg$type), ]
segs <- segs[ order(segs$t0),]

stats <- segment.summary( segs, trip)
stats$driver <- driver.id
stats$trip <- trip.id

print(stats)

curves <- segs[grepl("curve",segs$type), ]
straights <- segs[grepl("straight",segs$type), ]
turns <- segs[grepl("turn",segs$type), ]
stops <- segs[grepl("stop",segs$type), ]

t1 <- 1
t2 <- nrow(trip)
plotTrip       (trip, tmin=t1, tmax=t2, header=F)
if (nrow(curves) > 0) plotTripOverlay(trip, curves, tmin=t1, tmax=t2)
if (nrow(turns) > 0) plotTripOverlay(trip, turns, tmin=t1, tmax=t2, lwd=6, col="magenta")
if (nrow(straights) > 0) plotTripOverlay(trip, straights, tmin=t1, tmax=t2, col="yellow")
if (nrow(stops) > 0) plotTrip.stopOverlay(trip, stops, tmin=t1, tmax=t2)


