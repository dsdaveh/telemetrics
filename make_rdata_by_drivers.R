#Use this script to create an RData file for a list of drivers

data.dir <- "c:/NO_BACKUP/kaggle/telematics/drivers"
trip.dir <- "c:/NO_BACKUP/kaggle/telematics/trip_chunks"
data.fn <- "c:/NO_BACKUP/kaggle/telematics/telematics.Rdata"

drivers.dir <- dir(data.dir)
set.seed(2349)

random.driver <- order(runif(1:length(drivers.dir), ))
driver.list <- as.integer(drivers.dir[random.driver])  #needless complication that I'm stuck with at the moment
N.DRIVERS <- length(drivers.dir)
N.TRIPS <-200

# # dev values
N.DRIVERS <- 10
#N.TRIPS <- 50

data <- data.frame( driver=integer(), tripnum=integer(), x=numeric(), y=numeric)

p0 <- proc.time()
i <- 1
for (driver in driver.list[1:N.DRIVERS]) {
    cat(sprintf("\n %s (%d/%d) ", driver, i, N.DRIVERS)); i <- i+1
    for (tripnum in 1:N.TRIPS) {
        cat( ifelse( tripnum %% 10 == 0, "x", "."))
        trip.file <- paste0( data.dir, '/', driver, '/', tripnum, ".csv")
        trip <- read.csv( trip.file )
        trip <- cbind( driver, tripnum, trip)
        data <- rbind( data, trip)
    }
    
}
save( data, file=data.fn)

p1 <- proc.time()
print (p0-p1)
