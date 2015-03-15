source("telematic_util.R")

data.dir <- "c:/NO_BACKUP/kaggle/telematics/drivers"

drivers.dir <- dir(data.dir)
set.seed(2349)

random.driver <- order(runif(1:length(drivers.dir), ))
driver.list <- drivers.dir[random.driver]  #needless complication that I'm stuck with at the moment
N.DRIVERS <- length(drivers.dir)

segs.dir <- "profile_segs"

#queue <- driver.list[9:15]   # <-- SET THIS MANUALLY

gen_segs <- function (queue, plot.trip=TRUE) {

    ptime1 <- proc.time()
    time.log <- rbind(ptime1, ptime1)
    time.log <- time.log[-(1:2), ]   #kludge
    for (driver.id in queue) {
        
        fname <- get.fname( segs.dir, driver.id, "segs")
        cat(fname, " ... \n")
        
        for (trip.id in 1:200) {
            cat(trip.id, "...");  if (trip.id %% 15 == 0) cat("\n")
            trip <- getTrip( driver.id, trip.id, v.thresh=2 ) 
            if (plot.trip) plotTrip(trip)
            if (trip.id == 1) { 
                trip.seg <-                  getTripSegments(trip, lookup=TRUE)
            } else { 
                trip.seg <- rbind( trip.seg, getTripSegments(trip, lookup=TRUE))
            }
        }
        
        save(trip.seg, file=fname)
        
        ptime2 <- proc.time();  delta <- ptime2-ptime1
        time.log <- rbind(time.log, delta)
        ptime1 <- ptime2;
        cat (delta, "\n")
    }
    print(time.log)
}
