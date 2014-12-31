source("telematic_util.R")

data.dir <- "c:/NO_BACKUP/kaggle/telematics/drivers"

drivers.dir <- dir(data.dir)
set.seed(2349)

random.driver <- order(runif(1:length(drivers.dir), ))
driver.list <- drivers.dir[random.driver]  #needless complication that I'm stuck with at the moment
N.DRIVERS <- length(drivers.dir)

prof.dir <- "profiles"

chunks <- list()
chunk.size <- 200

for (i in seq(1, N.DRIVERS, 200)) {
    i.chunk <- round( i / chunk.size) + 1
    end.chunk <- min( i+chunk.size-1, N.DRIVERS )
    chunks[i.chunk] <- list(driver.list[i: end.chunk])
}

chunk <- unlist(chunks[5])  # <--------- CHANGE THIS BEFORE EXECUTING

ptime0 <- proc.time()
for (i in 1:length(chunk)) {
    driver.id <- chunk[i]
    trip.prof <- data.frame()
    ptime1 <- proc.time()
    for (i.trip in 1:200) {
        trip <- getTrip( driver.id, i.trip, v.thresh=1 )    #v.thresh = lowered for slow trips
        tp <- getTripProfile( trip )
        trip.prof <- rbind(trip.prof, tp)
        cat(i, driver.id, i.trip, ": length, dist, straights, accels, decels # ", tp$trip.len, round(tp$trip.dist/1000,1), tp$ss.n, tp$acc.n, tp$dec.n, "...\n")
    }
    rownames(trip.prof) <- 1:200
    
    ptime2 <- proc.time();  print( ptime2-ptime1)
    
    prof.fn <- sprintf("%s/driver_%s_prof.RData", prof.dir, driver.id);
    cat(prof.fn, "...\n")
    save(file=prof.fn, trip.prof)
    
}
ptime3 <- proc.time() ; print(ptime3-ptime0)