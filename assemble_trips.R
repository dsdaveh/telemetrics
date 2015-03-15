source("telematic_util.R")

data.dir <- "c:/NO_BACKUP/kaggle/telematics/drivers"
trip.dir <- "c:/NO_BACKUP/kaggle/telematics/trip_chunks"

drivers.dir <- dir(data.dir)
set.seed(2349)

random.driver <- order(runif(1:length(drivers.dir), ))
driver.list <- drivers.dir[random.driver]  #needless complication that I'm stuck with at the moment
N.DRIVERS <- length(drivers.dir)

csize <- 100
trips <- getTrip( drivers[1], 1) ; trips <- trips [-(1:nrow(trips)), ]

i <- 1
while (i < N.DRIVERS ) {
    i.n <- ifelse( i+csize <= N.DRIVERS, i+csize, N.DRIVERS )
    drivers <- driver.list[i:i.n]
    
    for (driver.id in drivers ) {
        cat ("processing driver ", driver.id, "...\n")
        for (trip.id in 1:200)  {
            cat(".")
            trips <- rbind (trips, getTrip(driver.id, trip.id, vthresh=2) )
        }
    }
    fn <- sprintf("%s/trips_%d-%d.RData", trip.dir, i, i.n)
    cat("\n", fn, "\n")
    trips <- trips [-(1:nrow(trips)), ]
    
    write.csv(trips, file=fn)
    i <- i.n + 1
}