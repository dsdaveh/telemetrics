ptime0 <- proc.time()
    ptime.last <- ptime0
for (trip.id in 1:200) {
    
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
    if (exists("driver_stats")) { 
        driver_stats <- rbind(driver_stats, stats)
    } else {
        driver_stats <- stats
    }
    print(stats)
    
    ptime1 <- proc.time()
    cat (ptime1-ptime.last, "\n")
    ptime.last <- ptime1
}
    cat ("Total Time: ", ptime.last-ptime0, "\n")
seg.summary <- cast( driver_stats,  trip~Type, value="count" )
save(driver_stats, file="driver_2591_driver_stats.RData")
seg.summary
