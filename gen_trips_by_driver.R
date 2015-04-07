library(dplyr);  filter <- stats::filter

source("telematic_util.R")
data.dir <- "c:/NO_BACKUP/kaggle/telematics/drivers"
trips.dir <- "c:/NO_BACKUP/kaggle/telematics/profile_trips"

drivers.dir <- dir(data.dir)
set.seed(2349)

random.driver <- order(runif(1:length(drivers.dir), ))
driver.list <- drivers.dir[random.driver]  #needless complication that I'm stuck with at the moment
N.DRIVERS <- length(drivers.dir)


#queue <- driver.list[1:6]   # <-- SET THIS MANUALLY

clearSegs()

gen_trips <- function (queue, plot.trip=FALSE) {

    ptime1 <- proc.time()
    time.log <- rbind(ptime1, ptime1)
    time.log <- time.log[-(1:2), ]   #kludge
    for (driver.id in queue) {
        
        fname <- get.fname( trips.dir, driver.id, "trips")
        cat(fname, " ... \n")
        
        trip.data <- data.frame()
        
        for (trip.id in 1:200) {
            cat(trip.id, "...");  if (trip.id %% 15 == 0) cat("\n")
            trip <- getTrip( driver.id, trip.id, v.thresh=2 ) 
            if (plot.trip) plotTrip(trip)
            trip.data <- rbind(trip.data, trip)
        }
        
        save(trip.data, file=fname)
        

        seg.all <- data.frame()
        seg.all <- rbind(seg.all, cbind(type="jump", seg.jumps))
        seg.all <- rbind(seg.all, cbind(type="stop", seg.stops))
        seg.all <- rbind(seg.all, cbind(type="straight", seg.straights))
        seg.all <- rbind(seg.all, cbind(type="turn", seg.turns))
        fname <- get.fname( trips.dir, driver.id, "segs")
        cat(fname, " ... \n")
        save(seg.all, file=fname)
        clearSegs()
        rm(seg.all)
        
        ptime2 <- proc.time();  delta <- ptime2-ptime1
        time.log <- rbind(time.log, delta)
        ptime1 <- ptime2;
        cat (delta, "\n")
    }
    print(time.log)
}
