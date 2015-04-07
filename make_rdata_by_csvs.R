#Use this script to take the a chunks of consolidated drivers x.y data and turn it into RData
# csv files were consolidated into chunks using mkcsv.pl (R is too slow)
library(stringr)

trip.dir <- "c:/NO_BACKUP/kaggle/telematics/trip_chunks"  #has the chunk files created by mkcsv.pl
setwd( trip.dir )

files <- dir()
files <- files[ grep("chunk_.*\\.csv", files) ]

for (f in files) {
    fid <- str_extract( f, "\\d+")
    cat("fid", fid, "\n")
    #wrif (as.integer(fid) <= 9) next #already wrote these
    f.rdata <- sprintf("chunk_%s.RData", fid)   # guess I could have just subbed csv and RData
    data <- read.csv( f )
    ndrivers <- length(unique(data$driver))
    save( data, file=f.rdata)
    rm(data)
    cat ("...wrote ", ndrivers, " drivers to ", trip.dir, "/", f.rdata, "\n")
}

