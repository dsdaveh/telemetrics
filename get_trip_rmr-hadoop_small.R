# This is a working example of R & Hadoop using the kaggle telemetrics data
# To Run:
#    1)  Create an AWS cluster using the instructions from:  
#        http://blogs.aws.amazon.com/bigdata/post/Tx37RSKRFDQNTSL/Statistical-Analysis-with-Open-Source-R-and-RStudio-on-Amazon-EMR
#    2)  log into RStudio on the master node
#    3)  upload telematic_util.R **
#    4)  telematics-dev5-50.Rdata **
#    5)  run this script on the master node 
#        Output: data_chunk_1154-3305.RData   (contains list of JSON objects for 15 trips)
#        plot of driver 2591, trip 3
#
# **files are available at https://github.com/dsdaveh/telemetrics


getTrip.kv <- function( driver.id, trip.id, kv, ...) {
    keystr <- paste0( driver.id, ".", trip.id)
    ix <- which( keys(kv) == keystr ) ; if (length(ix) >1) ix <- ix[1]
    fromJSON( unlist(values(kv)[ix]) )
}

map.driver <- function(k, v) {   
    log <- sprintf("%s: begin map.drive with %d drivers\n", Sys.time() , length(v))
    #     
    trips.json <- list()
    trip.keys <- character()
    p0 <- p1 <- proc.time()
    done <- integer()
    for (driver.id in v) {
        log <- paste0( log, sprintf("%d: ", driver.id))
        for (trip.id in 1:N.TRIPS) {
            trip.key <- paste0(driver.id, ".", trip.id)
            trip <- getTrip( driver.id, trip.id, v.thresh=2, data=data)
            trip.json <- toJSON( trip )
            trip.keys <- c(trip.keys, trip.key)
            trips.json <- c(trips.json, trip.json)
        }
        p2 <- proc.time()
        log <- paste0( log, sprintf(",%d", round((p2-p1)[3]) ) )
        p1 <- p2
        
        done <- c(done, driver.id)
    }
    final.time <- round((p1-p0)[3])
    log <- paste0( log
                   , sprintf("\nCompleted drivers (%d secs): ", final.time)
                   , paste0(done, collapse=","), "\n")
    keyval( c("log", trip.keys), c(log, trips.json) )
#    keyval( c("log", "json_size"), c(log, object.size(trips.json)) )
}

Sys.setenv(HADOOP_CMD="/home/hadoop/bin/hadoop")
Sys.setenv(HADOOP_STREAMING="/home/hadoop/contrib/streaming/hadoop-streaming.jar")
Sys.setenv(JAVA_HOME="/usr/java/latest/jre")

# load library
library(rmr2)

# run locally - good for debuging
# rmr.options(backend="local")

# now run it on the AWS EMR Hadoop Cluster
rmr.options(backend="hadoop")
#library(rhdfs)
#library(plyrmr)

# initiate rhdfs package
#hdfs.init()

library(jsonlite)
library(stringr)
source("telematic_util.R")

data.fn <- "telematics-dev5-50.Rdata"
load(data.fn)
driver.list <- unique(data$driver)
N.DRIVERS <- length(driver.list)
#N.TRIPS <- length( unique( data$tripnum))  #should be 200 for full data
N.TRIPS <- 3

subc.start <- 1
subc.size <- 10
subc.scale <- 1

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
    result <- from.dfs(out)
    p1 <- proc.time()
    cat ((p1-p0), "\n")
    
    trip <- getTrip.kv( 2591, 3, result)
    head(trip)
    plotTrip(trip)
    
    #write to disk, but keep a hash of drivers in driver.list by filename
    if (! exists("driver.index")) driver.index <- list()
    data.out.fn <- sprintf("data_chunk_%d-%d.RData"
                       , min(data$driver), max(data$driver))
    kv <- keyval( data.out.fn, list(drivers))
    driver.index <- c(driver.index, list(kv))
    
    save(result, file=data.out.fn)
    save(driver.index, file="driver.index.RData")  #DON'T CLEAR THIS
    rm(data)
    
    #to look for  a driver
    #if (2591 %in% unlist(values(driver.index[[1]]))) keys(driver.index[[1]])
    
    load(data.fn)
    subc.start <- subc.end + 1
    subc.size <- subc.size * subc.scale  #subchunk size next time around
}
rm(data)