data.dir <- "c:/NO_BACKUP/kaggle/telematics/drivers"
prof.dir <- "profiles"
prof.exist <- dir( prof.dir )
chunk.dir <- "chunks"
chunks.exist <- dir(chunk.dir)

drivers.dir <- dir(data.dir)
set.seed(2349)

random.driver <- order(runif(1:length(drivers.dir), ))
driver.list <- drivers.dir[random.driver]  #needless complication that I'm stuck with at the moment
N.DRIVERS <- length(drivers.dir)

#reconstruct the chunks
chunks <- list()
chunk.size <- 200

for (i in seq(1, N.DRIVERS, 200)) {
    i.chunk <- round( i / chunk.size) + 1
    end.chunk <- min( i+chunk.size-1, N.DRIVERS )
    chunks[i.chunk] <- list(driver.list[i: end.chunk])
}

i.chunk <- 1
for (chunk in chunks) {
    
    chunk.fn <- sprintf("chunk%d.RData", i.chunk); i.chunk <- i.chunk + 1
    chunk.exists <- any(grepl( chunk.fn, chunks.exist))            
    if (chunk.exists) { cat("skipping chunk", i.chunk-1,"(already exists)\n"); next}
    chunk.fn <- paste0( chunk.dir, "/", chunk.fn)
    
    check.file <- sprintf("_%s_", tail(chunk,1))
    file.exists <- any(grepl( check.file, prof.exist ))
    if (! file.exists) { cat("skipping chunk", i.chunk-1, "(last file doesn't exist)\n"); next}
    
    chunk.df <- data.frame()
    for (i in 1:length(chunk)) {
        cat(ifelse( i %% 10 == 0, 'x', '.'))
        driver.id <- chunk[i]
        prof.fn <- sprintf("%s/driver_%s_prof.RData", prof.dir, driver.id)
        load(file=prof.fn)
        trip.prof$driver <- driver.id
        trip.prof$trip <- rownames(trip.prof)
        chunk.df <- rbind(chunk.df, trip.prof)
    }   
    cat(chunk.fn, "...\n")
    save(file=chunk.fn, chunk.df)
}
