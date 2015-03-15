source("telematic_util.R")

data.dir <- "c:/NO_BACKUP/kaggle/telematics/drivers"

drivers.dir <- dir(data.dir)
set.seed(2349)

random.driver <- order(runif(1:length(drivers.dir), ))
driver.list <- drivers.dir[random.driver]  #needless complication that I'm stuck with at the moment
N.DRIVERS <- length(drivers.dir)

segs.dir <- "profile_segs"

driver.id <- 2591

examine <- (1:6) + 6*12
examine <- c( 11, 10, 14, 37, 40, 43, 44, 45, 49)


par.orig <- par(mar=c(1,1,1,1), mfrow=c(2,3), ask=FALSE)
for (trip.id in examine) {
    cat( trip.id , " ")
    if ( trip.id %% 3 == 0) cat ("\n")
    if ( trip.id %% 6 == 0) cat ("\n")
    plotTrip( getTrip(driver.id, trip.id), header=FALSE)
}
par(par.orig)
