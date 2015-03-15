getTrip.kv <- function( driver.id, trip.id, kv, ...) {
    keystr <- paste0( driver.id, ".", trip.id)
    ix <- which( keys(kv) == keystr ) ; if (length(ix) >1) ix <- ix[1]
    fromJSON( unlist(values(kv)[ix]) )
}

map.driver <- function(k, v) {   
    log <- file( sprintf("map_driver-%s.log", Sys.info()["nodename"]), "a")
    cat( sprintf("%s: begin map.drive with %d drivers\n", Sys.time() , length(v)), file=log)
    
    trips.json <- list()
    trip.keys <- character()
    p0 <- p1 <- proc.time()
    done <- integer()
    for (driver.id in v) {
        
        cat(driver.id,": ")
        for (trip.id in 1:N.TRIPS) {
            cat( ifelse (trip.id %% 10 == 0, "x", "."))
            trip.key <- paste0(driver.id, ".", trip.id)
            trip <- getTrip( driver.id, trip.id, v.thresh=2, data=data)
            trip.keys <- c(trip.keys, trip.key)
            trips.json <- c(trips.json, toJSON( trip ))
        }
        p2 <- proc.time()
        cat( sprintf(". %d secs", round((p2-p1)[3]) ), file=log)
        p1 <- p2
        cat("\n")
        done <- c(done, driver.id)
    }
    final.time <- round((p1-p0)[3])
    cat( "\nCompleted drivers: ", done, " in ", final.time, " secs\n", file=log)
    close(log)
    keyval( trip.keys, trips.json )
}


# set R environments
# Sys.setenv(HADOOP_CMD="/home/hadoop/bin/hadoop")
# Sys.setenv(HADOOP_STREAMING="/home/hadoop/contrib/streaming/hadoop-streaming.jar")
# Sys.setenv(JAVA_HOME="/usr/java/latest/jre")

library(rmr2);  rmr.options(backend="local")
#library(rhdfs)
#library(plyrmr)

# initiate rhdfs package
#hdfs.init()

library(jsonlite)
source("telematic_util.R")

data.dir <- "c:/NO_BACKUP/kaggle/telematics/drivers"
trip.dir <- "c:/NO_BACKUP/kaggle/telematics/trip_chunks"
data.fn <- "chunk_1.RData"
#data.fn <- "telematics-dev10-200.Rdata"
load(data.fn)
driver.list <- unique(data$driver)
N.DRIVERS <- length(driver.list)
N.TRIPS <- length( unique( data$tripnum))  #should be 200 for full data

subc.start <- 1
subc.size <- 10
subc.scale <- 2

while (subc.start <= N.DRIVERS) {
    subc.end <- ifelse( subc.start + subc.size -1 > N.DRIVERS, N.DRIVERS, subc.start + subc.size - 1 )
    
    drivers <- driver.list[ subc.start:subc.end ]
    ndrivers <- length(drivers)
    #N.TRIPS <- 2  #for development
    
    data <- data[ data$driver %in% drivers, ]
    
    p0 <- proc.time()
    out <- mapreduce(
        input = to.dfs(drivers),
        map = map.driver 
    )
    res <- from.dfs(out)
    p1 <- proc.time()
    cat ((p1-p0), "\n")
    
    # trip <- getTrip.kv( 2591, 3, res)
    # head(trip)
    # plotTrip(trip)
    
    #write to disk, but keep a hash of drivers in driver.list by filename
    if (! exists("driver.index")) driver.index <- list()
    data.out.fn <- sprintf("data_chunk_%d-%d.RData"
                       , min(data$driver), max(data$driver))
    kv <- keyval( data.out.fn, list(drivers))
    driver.index <- c(driver.index, list(kv))
    
    save(data, file=data.out.fn)
    save(driver.index, file="driver.index.RData")  #DON'T CLEAR THIS
    rm(data)
    
    #to look for  a driver
    #if (2591 %in% unlist(values(driver.index[[1]]))) keys(driver.index[[1]])
    
    load(data.fn)
    subc.start <- subc.end + 1
    subc.size <- subc.size * subc.scale  #subchunk size next time around
}
rm(data)